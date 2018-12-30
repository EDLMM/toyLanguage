(ns language0.core
  (:require [instaparse.core :as insta])
  (:import (clojure.asm Opcodes Type ClassWriter)
    (clojure.asm.commons Method GeneratorAdapter))
  (:gen-class)
)

;; parser
  (def lang0-parser
    (instaparse.core/parser
    "prog = (spaces expr spaces <';'> spaces)*
      <expr> = assig | add-sub
      assig = varname spaces <'='> spaces expr
      <add-sub> = mult-div | add | sub
      add = add-sub spaces <'+'> spaces mult-div
      sub = add-sub spaces <'-'> spaces mult-div
      <mult-div> = factor | mult |div
      mult = mult-div spaces <'*'> spaces factor
      div = mult-div spaces <'/'> spaces factor
      <factor> = number | <'('> spaces expr spaces <')'> | varget |assig
      <spaces> = <#'\\s*'>
      number = #'-?[0-9]+'
      varget = varname
      varname = #'[a-zA-Z]\\w*'"))
  (def lang1-parser
    (instaparse.core/parser
    "prog = (spaces expr spaces <';'> spaces)*
        <expr> = assig | add-sub
        assig = varname spaces <'='> spaces expr
        <add-sub> = mult-div | add | sub
        add = add-sub spaces <'+'> spaces mult-div
        sub = add-sub spaces <'-'> spaces mult-div
        <mult-div> = factor | mult |div
        mult = mult-div spaces <'*'> spaces factor
        div = mult-div spaces <'/'> spaces factor
        <factor> = number | <'('> spaces expr spaces <')'> | varget |assig
        <spaces> = <#'\\s*'>
        number = #'-?[0-9]+'
        varget = varname | argument
        varname = #'[a-zA-Z]\\w*'
        argument= <'%'>#'[0-9]+'"))

;; interpreter
  (defn make-interpreting [make-instr-interpreting init-env]
    {:prog (fn [& instrs] (:_ret (reduce
                                        (fn[env instr]
                                          (instaparse.core/transform (make-instr-interpreting env) instr))
                                        init-env
                                        instrs)))})
  (defn make-lang0-instr-interpreting [env]
    { :assig (fn[{varname :_ret :as env1} {value :_ret :as env2}]
              (assoc (merge env1 env2) varname value :_ret value))
    :add (fn[{v1 :_ret :as env1} {v2 :_ret :as env2}]
            (assoc (merge env1 env2) :_ret (+ v1 v2)))
    :sub (fn[{v1 :_ret :as env1} {v2 :_ret :as env2}]
            (assoc (merge env1 env2) :_ret (- v1 v2)))
    :mult (fn[{v1 :_ret :as env1} {v2 :_ret :as env2}]
            (assoc (merge env1 env2) :_ret (* v1 v2)))
    :div (fn[{v1 :_ret :as env1} {v2 :_ret :as env2}]
            (assoc (merge env1 env2) :_ret (quot v1 v2)))
    :number #(assoc env :_ret (Integer/parseInt %))
    :varname #(assoc env :_ret (keyword %))
    :varget (fn [{varname :_ret :as env1}]
              (assoc env1 :_ret (varname env1)))})
  ;; dynamic evaluation for reusing
  (defn dynamic-eval [interpreter]
    (fn[ast]
      (fn[]
        (instaparse.core/transform interpreter ast))))

  ;;NOTE: update for language 1 here
  ;; add the arguments to AST
  (defn args-to-env[args]
    (into {} (map-indexed #(vector (keyword (str "%" %1)) %2) args)))
  (defn dynamic-eval-args [make-interpreter]
    (fn[ast]
      (fn[& args]
        (instaparse.core/transform (make-interpreting make-interpreter
                                            (assoc (args-to-env args)
                                                  :_ret 0))
                        ast))))
  ;; update the tranformation map  
  (defn make-lang1-instr-interpreting [env]
    (assoc (make-lang0-instr-interpreting env)
          :argument #(assoc env :_ret (keyword (str "%" %)))))

;; compiler
  ;; import asm package
    ;; NOTE: blackbox here
    (import '(clojure.asm Opcodes Type ClassWriter))
    (import '(clojure.asm.commons Method GeneratorAdapter))
    (defn compiled [n-args class-name bytecode-generator]
        (let [cw (ClassWriter. (+ ClassWriter/COMPUTE_FRAMES ClassWriter/COMPUTE_MAXS ))
              init (Method/getMethod "void <init>()")
              meth-name "run"
              meth-sig (str "(" (apply str (repeat n-args "I")) ")I")]
          (.visit cw Opcodes/V1_6 Opcodes/ACC_PUBLIC (.replace class-name \. \/) nil "java/lang/Object" nil)
          (doto (GeneratorAdapter. Opcodes/ACC_PUBLIC init nil nil cw)
            (.visitCode)
            (.loadThis)
            (.invokeConstructor (Type/getType Object) init)
            (.returnValue)
            (.endMethod))
          (doto (.visitMethod cw (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) meth-name meth-sig nil nil )
            (.visitCode)
            (bytecode-generator)
            (.visitMaxs 0 0 )
            (.visitEnd))
          (.visitEnd cw)
          (let [b (.toByteArray cw)
                cl (clojure.lang.DynamicClassLoader.)]
            (.defineClass cl class-name b nil))
          (fn [& args] (clojure.lang.Reflector/invokeStaticMethod class-name meth-name (into-array args))))
        )
  ;; arith compiler
    ;; ast -> vector
    ;; use to concatenate the transform rules
    (defn assoc-binary-op [m [op instr]]
      (let[binary-op-compiling (fn[op]
                                (fn[instrs-v0 instrs-v1]
            (conj (into instrs-v0 instrs-v1) [op])))]
        (assoc m op (binary-op-compiling instr))))

    (def const-compiling
      {:prog (fn[& instrs](conj (reduce into [[:loadi 0]] instrs)[:reti]))
        :number #(vector [:loadi (Long/parseLong %)])})
    (def addsub-compiling
      (reduce assoc-binary-op const-compiling [[:add :addi][:sub :subi]]))
    (def addmult-compiling
      (reduce assoc-binary-op addsub-compiling [[:mult :multi][:div :divi]]))

    ;; generate byte code
    (defmulti generate-instr (fn [mv [instr & args]] instr))
    (defmethod generate-instr :loadi [mv [instr & args]]
      (doto mv
        (.visitLdcInsn (int (first args)))))
    (defmethod generate-instr :reti [mv [instr & args]]
      (doto mv
        (.visitInsn Opcodes/IRETURN)))
    (defmethod generate-instr :addi [mv [instr & args]]
      (doto mv
        (.visitInsn Opcodes/IADD)))
    (defmethod generate-instr :subi [mv [instr & args]]
      (doto mv
        (.visitInsn Opcodes/ISUB)))
    (defmethod generate-instr :multi [mv [instr & args]]
      (doto mv
        (.visitInsn Opcodes/IMUL)))
    (defmethod generate-instr :divi [mv [instr & args]]
      (doto mv
        (.visitInsn Opcodes/IDIV)))

    ;; create a compiler that will take an ast as argument
    ;; and return the function created with compiled
    (defn dispatching-bytecode-generating-eval [n-args class-name compiling]
      (fn[ast]
        (let[instrs (instaparse.core/transform compiling ast)
            generate-prog (fn[mv] (reduce generate-instr mv instrs))]
          (compiled n-args class-name generate-prog))))

  (def lang0-compiling
    (assoc addmult-compiling
          :varget #(vector [:load %])
          :assig (fn[var instrs](conj instrs [:store var]))))
  (use 'clojure.set)
  ;; helper function that replaces all the values in map m with the given value v
  (defn replace-vals [m v]
    (into {} (map vector (keys m) (repeat v ))))
  
  ;; in this transformation, the ast after parsing would be modified.
  ;; the vector of keyword :varget will change as:
  ;; [:varget [:varname variable_name]] =>[:varget variable_number]
  ;; the variable_number to the variable_name is a bijection function
  ;; the value assignment keyword will be inserted with extra value: variable_number
  (defn to-numeric-vars[nb-args ast]
    (let[varnames ;;transform 
        (instaparse.core/transform
          (assoc (replace-vals ;; (assosc map key value) will update the value under the same keyword
                  lang0-compiling
                  (fn[& instrs] (apply clojure.set/union (filter set? instrs))))
            :varname (fn[varname]#{varname}) ;; use :varname to get all the varname values with map destructing
          )
        ast)
        name->num (into {} (map vector varnames (iterate inc nb-args)))] ;; assign a number to a variable
      (instaparse.core/transform {:varname #(get name->num %)} ast)))
  (defmethod generate-instr :load [mv [instr & args]]
    (doto mv
      (.visitVarInsn Opcodes/ILOAD (int (first args)))))
  (defmethod generate-instr :store [mv [instr & args]]
    (doto mv
      (.visitInsn Opcodes/DUP)
      (.visitVarInsn Opcodes/ISTORE (int (first args)))))
  
  ;; language 1 starts here
  ;;count and return the number of arguments
  (defn nb-args[ast]
    (inc (instaparse.core/transform (assoc (replace-vals
                            lang0-compiling (fn[& args]
                                              (apply max (conj (filter number? args)
                                                                -1))))
                            :argument #(Integer/parseInt %))
                    ast)))
  ;;  replace the :argument nodes with the value of the argument number:
  (defn args->varnum[ast]
    (instaparse.core/transform {:argument #(Integer/parseInt %)} ast))

  (defn lang1-compiler-chain[class-name ast]
    (let[n-args (nb-args ast)
        compiler (dispatching-bytecode-generating-eval n-args class-name lang0-compiling)]
      (->> ast args->varnum (to-numeric-vars n-args) compiler)))

(defn -main [& args]
  ;; language 0 test
    ; (println (lang0-parser "a=1+1*3;b=a-2; a+b;"))
    ; (def lang0-interpret (dynamic-eval (make-interpreting make-lang0-instr-interpreting {:_ret 0})))
    ; (def lang0-interpret-test (->> "a=1+1*3;b=a-2; a=a+b;c;" lang0-parser lang0-interpret))
    ; (println (lang0-interpret-test))

    ;;to-numeric-vars and ast->instruction vec
    ; (println (str (->> "a=1+1*3;b=2+a; a=a+b;c;" lang0-parser (to-numeric-vars 0))))
    ; (println (str (->> "a=1+1*3;b=2+a; a=a+b;c;" lang0-parser (to-numeric-vars 0) (instaparse.core/transform lang0-compiling))))

    ; (def lang0-compiler (dispatching-bytecode-generating-eval 0 "Lang0Compiler" lang0-compiling))
    ; (def lang0-compiler-test (->> "a=1 + 3 * (-2 - 1);b= 0 - a; b=b-a;" lang0-parser (to-numeric-vars 0) lang0-compiler))
    ; (println (lang0-compiler-test))

  (def lang1-interpret (dynamic-eval-args make-lang1-instr-interpreting))
  (def lang1-interpret-test (->> "a=%0;a + %1 *3;" lang1-parser lang1-interpret))
  (println (lang1-interpret-test 2 3))
  ; test of the nb-args function
  ; (println (->> "a=%0;a + %1 *3;" lang1-parser nb-args))
  (def lang1-compiler-test (->> "a=%0;a + %1 *3;" lang1-parser (lang1-compiler-chain "Lang1Compiler")))
  (println (lang1-compiler-test 2 5))
)
