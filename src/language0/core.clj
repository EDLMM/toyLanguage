(ns language0.core
  (:require [instaparse.core :as insta])
  (:import (clojure.asm Opcodes Type ClassWriter)
    (clojure.asm.commons Method GeneratorAdapter))
  (:gen-class)
)
(use 'clojure.walk)
(use 'clojure.pprint) 
;; parser
  (def lang-if-parser; 
    (instaparse.core/parser
    "prog = (expr-space)*        
        flag= spaces

        if-flow = spaces <'if'> spaces condition spaces true-case spaces
        if-else-flow = spaces <'if'> spaces condition spaces true-case flag false-case spaces
        <condition> = spaces <'('> spaces expr spaces <')'> spaces
        <true-case> = spaces <'{'> (expr-space)* <'}'> spaces
        <false-case> = spaces <'else'> spaces <'{'> (expr-space)* <'}'> spaces

        while-flow=spaces <'while'> spaces condition spaces true-case spaces
        for-flow = spaces <'for'> spaces for-condition spaces true-case spaces
        <for-condition>= spaces <'('> spaces assig spaces <';'> spaces expr spaces <';'> spaces assig spaces <')'> spaces

        <expr-space> = spaces expr spaces <';'> spaces
        <expr> = assig | add-sub | if-flow | if-else-flow | while-flow | for-flow
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
    ))
;; interpreter
  (defn make-lang0-interpreter [env]
    {:assig (fn[{varname :_ret :as env1} {value :_ret :as env2}]
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
    :varname (fn [x] (assoc env :_ret (keyword x)))
    :varget (fn [{varname :_ret :as env1}]
              (assoc env1 :_ret (varname env1)))})
  ;;NOTE: update for language 1 here
  (defn make-lang1-interpreter [env]
    (assoc (make-lang0-interpreter env)
          :argument #(assoc env :_ret (keyword (str "%" %)))))
  ;; NOTE: condition evaluation: not change the :_reti
  (defn condition-eval [env sentence]
      (let [{stack-ret :_ret} env
            update-env (instaparse.core/transform (make-lang1-interpreter env) sentence)
            {cr :_ret} update-env
            update-env (assoc update-env :_ret stack-ret)]
          [(not= cr 0) update-env]
    ))
  ;; evaluate the result of expressions in subcases
  (defn subcase-eval [env sentences] 
      (let [update-env 
            (reduce 
              (fn [env instr] (instaparse.core/transform (make-lang1-interpreter env) instr))
              env
              sentences)]
        update-env))
  ;;use to split the cases in if-esle with flag
    (defn notflag? [vec]
      (if (not= vec [:flag]) true false))
    (defn lazy-to-vec [lz]
      (into [] (concat lz))
    )
    (defn in-split [cs]
       (let [ mid (split-with notflag? cs)
             [:as t-case] (first mid)
             [:as f-case] (rest (second mid))]
        [t-case f-case]
       ))
  (defn get-if-choice [env args]
    (let [cases (rest args)
          condition (first cases)
          [notjump update-env] (condition-eval env condition)
          t (rest cases)
          choice (lazy-to-vec (if notjump t [[:number (str (:_ret env))]]))]
      [update-env choice]))
  (defn get-if-else-choice [env args]
    (let [cases (rest args)
          condition (first cases)
          [notjump update-env] (condition-eval env condition)
          [t f] (in-split (rest cases))
          choice (lazy-to-vec (if notjump t f))]
      [update-env choice]))

  (defn toy-interpreter [env instr]
    (let [c (str (first instr))
          update-env (case c
            ":assig" (let [{stack-ret :_ret} env
                          {varname :_ret} (toy-interpreter env (second instr))
                          value-env (toy-interpreter env (get instr 2))
                          update-env (assoc value-env varname (:_ret value-env))]
                        update-env
                      )
            ":only-assig" (let [{stack-ret :_ret} env
                          {varname :_ret} (toy-interpreter env (second instr))
                          value-env (toy-interpreter env (get instr 2))
                          update-env (assoc value-env varname (:_ret value-env))]
                        (assoc update-env :_ret stack-ret)
                      )
            
            ":if-else-flow" (let [[ifelse-env choice] (get-if-else-choice env instr)]
                              (reduce toy-interpreter ifelse-env choice))
            ":if-flow" (let [[if-env choice] (get-if-choice env instr)]
                              (reduce toy-interpreter if-env choice))
            ":while-flow" (let [condition (get instr 1)
                                iterations (lazy-to-vec (nthrest instr 2))
                                env (toy-interpreter env [:number "0"])
                                aa (condition-eval env condition)]
                            (loop [[isRun update-env] aa]
                              (if (not isRun)
                                update-env
                                (recur (condition-eval (reduce toy-interpreter update-env iterations) condition)))                                     
                              ))
            ":for-flow" (let [init-assig (get instr 1)
                              init-env (toy-interpreter env init-assig)
                              condition (get instr 2)
                              var-update (insta/transform {:assig (fn [& args] (lazy-to-vec (concat [:only-assig] (lazy-to-vec args))))} (get instr 3))
                              iterations (lazy-to-vec (nthrest instr 4))
                              ivi (lazy-to-vec (concat iterations [var-update]))                                                                                
                              env (toy-interpreter init-env [:number "0"])
                              aa (condition-eval env condition)]
                          ; (pprint test-update)
                          ; (pprint (reduce toy-interpreter env [test-update]))
                          (loop [[isRun pre-env] aa]
                            (if (not isRun)
                              pre-env
                              (recur (condition-eval (reduce toy-interpreter (reduce toy-interpreter pre-env iterations) ivi) condition)
                            ))                   
                        ))
            (subcase-eval env [instr])) ;TODO: 最后再给while 加默认值
          ]
    update-env))
  ;; add the arguments to AST
    (defn args-to-env[args]
      (into {} (map-indexed #(vector (keyword (str "%" %1)) %2) args)))
    (defn dynamic-eval-args [make-interpreter ast & args] ;TODO: add function for % argument
      ; (pprint "get args:")
      ; (pprint args)
      ; (pprint "result of args-to env:")
      (def rate (args-to-env args))
      ; (pprint rate)
      ; (pprint "result of get sentences:")
      (def sentences (insta/transform {:prog (fn [& exprs] exprs)} ast))
      ; (pprint sentences)
      ; (pprint "--------information-------")
      ; (pprint "end result:")
      (def init-env {:_ret 999})
      (def final-env (reduce toy-interpreter init-env sentences))
      ; (pprint final-env)
      (:_ret final-env)
    )

;; compiler
  ;; import asm package and generate
    ;; NOTE: many blackbox here, an implementation of ASM package
    (import '(clojure.asm Opcodes Type Label ClassWriter));;NOTE: add Label import here
    (import '(clojure.asm.commons Method GeneratorAdapter))
    (defn compiled [n-args class-name bytecode-generator]
        (let [cw (ClassWriter. (+ ClassWriter/COMPUTE_FRAMES)); ClassWriter/COMPUTE_MAXS 
              init (Method/getMethod "void <init>()")
              meth-name "run"
              ;method descriptor here: take n interger argument, return an interger at last
              meth-sig (str "(" (apply str (repeat n-args "I")) ")I")]
          ;classWriter begin
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

          ;classWriter end
          (.visitEnd cw)
          (let [b (.toByteArray cw)
                cl (clojure.lang.DynamicClassLoader.)];define a class loader and
            (.defineClass cl class-name b nil)) ;use the class loader to load the generated class
          (fn [& args] (clojure.lang.Reflector/invokeStaticMethod class-name meth-name (into-array args))))
        )
  ;; arith compiling
    ;; ast -> vector: use to concatenate the transform rules
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
  ;; language 0 compiling
    (def lang0-compiling
      (assoc addmult-compiling
            :varget #(vector [:load %])
            :assig (fn[var instrs](conj instrs [:store var]))))
   
  ;; language if-else compiling
    ;; FIXME: only support if call once I guess. Maybe it can name the label with a global counter?
    ;; transform instrs: ([[] []] [[]]) -> [[] [] []]
    (defn combine-ins [vec] 
      (into [] (reduce concat (first vec) (rest vec))))
    (defn split-case [cs]
       (let [mid (split-with notflag? cs)
             [:as t-case] (first mid)
             [:as f-case] (rest (second mid))]
        [(combine-ins t-case) (combine-ins f-case)]
       ))
    (defn isLoad [i-vec] ;operation that will add 1 interger in stack
      (or (= i-vec [:loadi]) (= i-vec [:load]) (= i-vec [:duptop])))
    (defn isNochange [i-vec] ;operation that won't change number of intergers
      (or (= i-vec [:store]) (= i-vec [:goto]) (= i-vec [:label])))
    (defn count-i [instr] ;return the number of interger in a stack for one branch
      (let [clean-arg (walk (fn [i] [(first i)]) #(identity %) instr) ;delete argument
            n-load (count (filter #(isLoad %) clean-arg))
            n-op (count (filter #(not (or (isLoad %) (isNochange %))) clean-arg))
          ]
      (- n-load n-op)
    ))
    (def lang-if-compiling
      (assoc lang0-compiling
            :if-flow 
              (fn [condition & cases]
                  (let [t-case (combine-ins cases)
                        f-case [[:duptop]] ;NOTE: a fake false case, is it really no harm?
                        compare (- (count-i t-case) (count-i f-case)) 
                        to-f-case (new Label)
                        to-end-if-else (new Label)
                        tdup (if (>= compare 0) nil (lazy-to-vec (repeat (- compare) [:duptop])))
                        fdup (if (>= 0 compare) nil (lazy-to-vec (repeat compare [:duptop])))
                      ]
                   
                    (into [] (concat condition
                      [[:ifeq to-f-case]] ;skip true case if ==zero
                      t-case tdup [[:goto to-end-if-else]]
                      [[:label to-f-case]] f-case fdup
                      [[:label to-end-if-else]]))));skip false case if execute true case
            :if-else-flow
              (fn [condition & both]
                (let [[t-case f-case] (split-case both)
                      compare (- (count-i t-case) (count-i f-case)) 
                      to-f-case (new Label)
                      to-end-if-else (new Label) ;NOTE: make sure the number of variables same in different branches
                      tdup (if (>= compare 0) nil (lazy-to-vec (repeat (- compare) [:duptop])))
                      fdup (if (>= 0 compare) nil (lazy-to-vec (repeat compare [:duptop])))
                      ]
                  ; (pprint "t-case if-else")
                  ; (pprint t-case)
                  (into [] (concat condition
                    [[:ifeq to-f-case]] ;skip true case if ==zero
                    t-case tdup [[:goto to-end-if-else]]
                    [[:label to-f-case]] f-case fdup
                    [[:label to-end-if-else]]));skip false case if execute true case
                )
              ) 
      ))
    (def lang-while-compiling
      (assoc lang-if-compiling
      :while-flow 
        (fn [condition & cases] 
            (let [t-case (combine-ins cases)
                  f-case [[:duptop]]
                  to-f-case (new Label)
                  run-again (new Label)  
                  compare (- (+ (count-i t-case) (count-i condition)) (count-i f-case))
                  tdup (if (>= compare 0) nil (lazy-to-vec (repeat (- compare) [:duptop])))
                  fdup (if (>= 0 compare) nil (lazy-to-vec (repeat (+ compare) [:duptop])))
            ]

            ; (pprint "n-t n-f:")
            ; (pprint [ (+ (count-i t-case) (count-i condition)) (count-i f-case)])
            ; (pprint "------------")
            (into [] (concat [[:label run-again]]
              condition [[:ifeq to-f-case]]
              t-case tdup [[:goto run-again]]
              [[:label to-f-case]] f-case fdup
            ))))
    ))


  ;; generate byte code
    (defmulti generate-instr (fn [mv [instr & args]] instr))
    (defmethod generate-instr :loadi [mv [instr & args]]
      (doto mv
        ; (.visitInsn (int (first args)))))
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
    (defmethod generate-instr :load [mv [instr & args]]
      (doto mv
        (.visitVarInsn Opcodes/ILOAD (int (first args)))))
    (defmethod generate-instr :store [mv [instr & args]]
      (doto mv
        (.visitInsn Opcodes/DUP)
        (.visitVarInsn Opcodes/ISTORE (int (first args)))))
    ;;TODO: 2. update generate here
    (defmethod generate-instr :ifeq [mv [instr & args]]
      (doto mv
        (.visitJumpInsn Opcodes/IFEQ (first args))))
    (defmethod generate-instr :goto [mv [instr & args]]
      (doto mv
        (.visitJumpInsn Opcodes/GOTO (first args))))  
    (defmethod generate-instr :label [mv [instr & args]]
      (doto mv
        (.visitLabel (first args))
        ; (.visitFrame 0 0 nil 0 nil)
      ))
    (defmethod generate-instr :duptop [mv [instr & args]]
      (doto mv
        (.visitInsn Opcodes/DUP)      
    ))
    (defmethod generate-instr :ifne [mv [instr & args]]
      (doto mv
      (.visitJumpInsn Opcodes/IFNE (first args))))

    ;; create a compiler that will take an ast as argument
    ;; and return the function created with compiled
    (defn dispatching-bytecode-generating-eval [n-args class-name compiling]
      (fn[ast]
        (let[instrs (instaparse.core/transform compiling ast);call compiling to transform parsed program
            generate-prog (fn[mv] (reduce generate-instr mv instrs))]
          (compiled n-args class-name generate-prog))))

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
                    lang-while-compiling
                    (fn[& instrs] (apply clojure.set/union (filter set? instrs))))
              :varname (fn [varname] #{varname}) ;; use :varname to get all the varname values with map destructing
            )
          ast)
          name->num (into {} (map vector varnames (iterate inc nb-args)))] ;; assign a number to a variable
        (instaparse.core/transform {:varname #(get name->num %)} ast)))

  ;; compiler chain (language 1)
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
      (let[n-args (nb-args ast) ;NOTE:calculate the argumenumber from scratch 
          compiler (dispatching-bytecode-generating-eval n-args class-name lang0-compiling)]
        (->> ast args->varnum (to-numeric-vars n-args) compiler)))
    (defn lang-if-compiler-chain[class-name ast]
      (let[n-args (nb-args ast) ;NOTE:calculate the argumenumber from scratch 
          compiler (dispatching-bytecode-generating-eval n-args class-name lang-while-compiling)]
        (->> ast args->varnum (to-numeric-vars n-args) compiler)))


(defn -main [& args]
  ;; lang-if
 
  ; (insta/visualize (lang-if-parser "a=%0;a + %1 *3;if(1 +3 ) { d=100; };" 2 3) :output-file "resources/if1.png" :options{:dpi 150})
  ; (pprint "results of parser + to-numeric-vars:")

  ;; interpreter test
    (def parsed-if-else (lang-if-parser "if (0) {1; 2;}else{3+4;};"))
    (def parsed-if (lang-if-parser "3; 1; if (1-1) {1; 2;};"))
    (def parsed-while-1 (lang-if-parser "a=1; b= a+2; c=while(a-3){a=a+1;b=b+2;}; c+1;"))
    (def parsed-while-2 (lang-if-parser "a=1; b= a+2; c=while(a-1){a=a+1;b=b+2;}; c+1;"))
    (def parsed-for-1 (lang-if-parser "a=0; b=for(c=a+1; c-10; c=c+1){a=2*c;}; b+c;"))
    (def parsed-for-2 (lang-if-parser "a=0; b=for(c=a+1; c-1; c=c+1){a=2*c;}; b+c;"))

    (defn lang-if-interpret [parsed] (dynamic-eval-args toy-interpreter parsed))

    (println (lang-if-interpret parsed-if-else))
    (println (lang-if-interpret parsed-if))
    (println (lang-if-interpret parsed-while-1))
    (println (lang-if-interpret parsed-while-2))
    (println (lang-if-interpret parsed-for-1))
    (println (lang-if-interpret parsed-for-2))

  ;; compiling test case
  ;"b=1-2*9+10;a=10; c=999; if ( b ) { a=4;a+1;b;}else{1;};c;"
  ; "b=1-2*9+10;if ( b ) { 1;b+1;}; c=999;c=9;"
  ;"a=10; while (0){a-1;13;a*2;}; 10;"
  (def while-prog "b=1-2*9+10;a=10; c=999; d=if ( b ) { a=4;a+1;b;}else{1;};c+d;")
  

  ; (pprint (->> if-prog lang-if-parser))

  ; (pprint "compiling result:")
  ; (def parsed (->> while-prog lang-if-parser (to-numeric-vars 0)))
  ; (def lang-if-before-generator (#(instaparse.core/transform lang-while-compiling %) parsed))
  ; (pprint lang-if-before-generator)

  ;; final if-compier test
  (pprint "generating bytecode:")
    (def lang-if-compiler-test-0
      (->> while-prog lang-if-parser (lang-if-compiler-chain "LangIfCompiler0")))
    (println (lang-if-compiler-test-0))
)
