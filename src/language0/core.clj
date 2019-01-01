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
  (def lang-if-parser;        
    (instaparse.core/parser
    "prog = (expr-space|if-flow|if-else-flow)*
        
        if-flow = spaces <'if'> condition true-case <';'> spaces
        if-else-flow = spaces <'if'> condition true-case false-case <';'> spaces
        <condition> = spaces <'('> spaces expr spaces <')'> spaces
        <true-case> = spaces <'{'> expr-space <'}'> spaces
        <false-case> = spaces <'else'> spaces <'{'> expr-space <'}'> spaces
        
        <expr-space> = spaces expr spaces <';'> spaces
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
        argument= <'%'>#'[0-9]+'"
    )
  )
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
  ;;NOTE: update for language 1 here
  ;; update the tranformation map  
  (defn make-lang1-instr-interpreting [env]
    (assoc (make-lang0-instr-interpreting env)
          :argument #(assoc env :_ret (keyword (str "%" %)))))
  ;;FIXME: how to deal with else case?
  ;; add if flow control for the transformation map
  (defn make-lang-if-instr-interpreting [env]
    (assoc (make-lang1-instr-interpreting env) ;; what if not false-case
        :if-flow (fn [{v1 :_ret :as env1} {v2 :_ret :as env2}]; {v3 :_ret :as env3}
                    (assoc (merge env1 env2); env3
                      :_ret (if (false? (zero? v1)) v2);" " v3
                    )
                  )
        :if-else-flow (fn [{v1 :_ret :as env1} {v2 :_ret :as env2} {v3 :_ret :as env3}]
                    (assoc (merge env1 env2 env3)
                      :_ret (if (zero? v1) v3 v2)
                    )
                  )
    )
  ) 
  ;; dynamic evaluation for reusing
  (defn dynamic-eval [interpreter]
    (fn[ast]
      (fn[]
        (instaparse.core/transform interpreter ast))))
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
  
  ;; language 0 compiler
    (def lang0-compiling
      (assoc addmult-compiling
            :varget #(vector [:load %])
            :assig (fn[var instrs](conj instrs [:store var]))))
    ;; NOTE:language if-else compiling
    (def lang-if-compiling
      (assoc lang0-compiling
            :if-flow (fn[var instrs](conj instrs [:if var]))
            :if-else-flow (fn [condition t-case f-case]
                            (conj condition [:t-case t-case] [:f-case f-case])
                          )
      ))

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
                    ;;FIXME: change the compiling here
                    ;;every time add new flow control
                    ;;or the varname will not be replace
                    lang-if-compiling
                    (fn[& instrs] (apply clojure.set/union (filter set? instrs))))
              :varname (fn [varname] #{varname}) ;; use :varname to get all the varname values with map destructing
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

  ; (def lang1-interpret (dynamic-eval-args make-lang1-instr-interpreting))
  ; (def lang1-interpret-test (->> "a=%0;a + %1 *3;" lang1-parser lang1-interpret))
  ; (println (lang1-interpret-test 2 3))
  ; ; test of the nb-args function
  ; (println (->> "a=%0;a + %1 *3;" lang1-parser nb-args))
  ; (def lang1-compiler-test (->> "a=%0;a + %1 *3;" lang1-parser (lang1-compiler-chain "Lang1Compiler")))
  ; (println (lang1-compiler-test 2 5))

  ;; lang-if
  ; (println (lang1-parser "a=%0;a + %1 *3;" 2 3))
  ; (insta/visualize (lang1-parser "a=%0;a + %1 *3;" 2 3) :output-file "resources/lang1parser.png" :options{:dpi 150})
  ; (insta/visualize (lang-if-parser "a=%0;a + %1 *3;if(1 +3 ) { d=100; };" 2 3) :output-file "resources/if1.png" :options{:dpi 150})
  
  ; (insta/visualize (lang-if-parser "a=%0;a + %1 *3;if( 0) { d=100; } else { 2;};" 2 3) :output-file "resources/if2.png" :options{:dpi 150})
  (println (->> "a=%0;a + %1 *3;if( 0) { b=100; } else { 2;};" lang-if-parser (to-numeric-vars 0)))
  
  (def lang-if-interpret (dynamic-eval-args make-lang-if-instr-interpreting))

  (def lang-if-interpret-test (->> "a=%0;a + %1 *3 ; if(1 +3 ) { d=100;};" lang-if-parser lang-if-interpret))
  (println (lang-if-interpret-test 2 3))
  (def lang-if-else-interpret-test (->> "a=0;a + 1 *3;b=4 ; if(c=0) { d=100/b ;}else{2;};" lang-if-parser lang-if-interpret))
  (println (lang-if-else-interpret-test))

  (println (->> "a=0;a + 1 *3;b=4 ; if(c=0) { d=100/b ;}else{2;};" lang-if-parser (to-numeric-vars 0) (instaparse.core/transform lang-if-compiling)))
  ; test of the nb-args function
  (println (->>  "a=0;a + 1 *3;b=4 ; if(c=0) { d=100/b ;}else{2;};" lang-if-parser nb-args))
)
