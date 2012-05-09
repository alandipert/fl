(ns fl.core
  (:refer-clojure :exclude (compile))
  (:use [clojure.string :only (join)]
        [clojure.walk :only (postwalk)]
        [clojure.pprint :only (pprint)]))

(def preserve
  `(fn [f#]
     (fn
       ([arg#] (when-not (nil? arg#) (f# arg#)))
       ([]))))

(def primitives
  {'def {:type :form
         :nary true}

   'clojure.core/unquote 'constant
   'constant {:type :form
              ;; don't make selector functions out of object arguments
              :form-behavior {:takes-selectors? false}
              :emit `(~preserve constantly)}

   '. 'compose
   'compose {:type :form
             :form-behavior {:takes-selectors? true}
             :nary true
             :emit `(fn [& args#]
                      (if-not (empty? args#)
                        (if-not (some nil? args#)
                          (reduce comp args#))))}

   '$ 'construct
   'construct {:type :form
               :form-behavior {:takes-selectors? true}
               :nary true,
               :emit `(fn [& args#]
                        (if-not (empty? args#)
                          (if-not (some nil? args#)
                            (apply juxt args#))))}

   '/ 'insert
   'insert {:type :form
            :form-behavior {:takes-selectors? true}
            :emit `(~preserve
                    (fn [f#]
                      #(reduce (fn [xs# y#]
                                 (f# [xs# y#])) %)))}

   'a 'apply-to-all
   'apply-to-all {:type :form
                  :form-behavior {:takes-selectors? true}
                  :emit `(fn [f#]
                           (~preserve
                            #(if (coll? %)
                               (filter (complement nil?) (map f# %)))))}

   '-> 'condition
   'condition {:type :form
               :form-behavior {:takes-selectors? true}
               :nary #{3}
               :emit `(fn
                        ;; (-> p f g):x => (if p:x f:x g:x)
                        ([p# f# g#]
                           (~preserve
                            (fn ([x#] (if (p# x#) (f# x#) (g# x#))) ([]))))
                        ;; (-> p f):x => (if p:x f:x)
                        ([p# f#]
                           (~preserve
                            (fn ([x#] (if (p# x#) (f# x#))) ([]))))
                        ;; (-> p):x => (if p:x x)
                        ([p#]
                           (~preserve
                            (fn ([x#] (if (p# x#) x#)) ([]))))
                        ([]))}

   'while {:type :form
           :form-behavior {:takes-selectors? true}
           :nary #{2}
           :emit `(fn [p# f#]
                    (fn [x#]
                      (do (while (p# x#)
                            (f# x#)))))}

   '+ {:type :function
       :emit `(~preserve (partial apply +))}
   '- {:type :function
       :emit `(~preserve (partial apply -))}
   '* {:type :function
       :emit `(~preserve (partial apply *))}
   '% {:type :function
       :emit `(~preserve (partial apply /))}
   'id {:type :function
        :emit `(~preserve identity)}
   'trans {:type :function
           :emit `(~preserve (partial apply map vector))}
   'and {:type :function
         :emit `(~preserve
                 (fn [arg#]
                   (boolean (identity (reduce #(and %1 %2) arg#)))))}
   '_ {:type :object
       :value nil}})

(def named-primitives
  (reduce (fn [xs [name spec]]
            (if (symbol? spec)
              (assoc xs name spec)      ;is an alias -> name
              (assoc xs name (assoc spec :args [], :name name))))
          {}
          primitives))

(defn get-prim [sym]
  (let [spec (named-primitives sym)]
    (if (symbol? spec)
      (named-primitives spec)
      spec)))

(defn clojure-def? [sym]
  "True if this symbol represents a Clojure definition available in this namespace."
  (and ((merge (ns-refers *ns*) (ns-publics *ns*)) sym)
       (not (:fl (meta (ns-resolve *ns* sym))))))

(defn clojure-function? [sym]
  (if (clojure-def? sym)
    (fn? @(ns-resolve *ns* sym))))

(def
  ^{:doc "True if this symbol represents an FL definition available in this namespace."}
  fl-def? (complement clojure-def?))

(defn fl-format [flform]
  (postwalk (fn [f]
              (if (list? f)
                (if (= (first f) 'clojure.core/unquote)
                  (symbol (str "~" (fl-format (second f))))
                  f)
                f))
            flform))

;;; parse

(defmulti parse (fn [_ expr] (class expr)))

(defmethod parse clojure.lang.PersistentList
  [env [op & more :as expr]]
  (cond

   ;; definition
   (= op 'def)
   (assoc (get-prim 'def)
     :env env,
     :args [{:type :object, :value (first more)},
            (parse (conj env expr) (first (rest more)))])

   ;; primitive form
   ;; numeric arguments are interpreted as selectors
   (= :form (:type (get-prim op)))
   (assoc (get-prim op),
     :env env,
     :args (map (comp
                 #(assoc %
                    :parent-type :form
                    :parent-form-behavior (:form-behavior (get-prim op)))
                 (partial parse (conj env expr))) more))

   ;; primitive function
   (= :function (:type (get-prim op)))
   (assoc (get-prim op),
     :env env,
     :args (map (comp
                 #(assoc % :parent-type :function)
                 (partial parse (conj env expr))) more))

   ;; invocation
   (symbol? op)
   (cond
    ;; defined FL function, form, or value
    (fl-def? op)
    {:type ::inline-fl-defined,
     :expr {:type :object,
            :value op,
            :env env}
     :env env
     :args (map (partial parse (conj env expr)) more)}

    ;; defined clojure value
    (clojure-def? op)
    {:type ::inline-clojure-call,
     :expr {:type :clojure-function,
            :value op,
            :env env}
     :env env
     :args (map (partial parse (conj env expr)) more)})

   ;; selector functions eg (1 [1 2 3]) -> 2
   (integer? op)
   {:type ::inline-integer-selection
    :expr {:type :inline-integer-selector,
           :value op,
           :env env}
    :env env
    :args (map (partial parse (conj env expr)) more)}

   ;; inline expression
   (list? op)
   {:type ::inline-expression,
    :expr (parse (conj env expr) op)
    :env env,
    :args (map (partial parse (conj env expr)) more)}))

(defmethod parse clojure.lang.Symbol
  [env sym]
  (cond

   ;; FL primitive value
   (get-prim sym)
   (assoc (get-prim sym) :env env)

   ;; clojure function
   (clojure-function? sym)
   {:type :clojure-function,
    :value sym,
    :env env}

   :else
   {:type :object, :value sym, :env env}))

(defmethod parse clojure.lang.PersistentVector
  [env v]
  {:env env, :type :object, :value v})

(defmethod parse :default
  [env expr]
  {:env env, :type :object, :value expr})

;;; analyze

(defmulti analyze :type)

(defmethod analyze :default
  [object] object)

;;; emit

(defmulti emit :type)

(defmethod emit :form
  [form]
  (if (= (:name form) 'def)
    `(do
       ~(list 'def
              (:value (first (:args form)))
              (emit (second (:args form))))
       (alter-meta! (var ~(:value (first (:args form))))
                    merge
                    {:fl {:source '~(fl-format (first (:env (second (:args form)))))}})
       #'~(:value (first (:args form))))
    (if (empty? (:args form))
      (:emit form)
      (list* (:emit form) (map emit (:args form))))))

(defmethod emit :function
  [function]
  (if (empty? (:args function))
    (:emit function)
    (list* (:emit function) (map emit (:args function)))))

(defmethod emit :clojure-function
  [function]
  `(~preserve
    (fn
      ([arg#]
         (if (coll? arg#)
           (apply ~(:value function) arg#)
           (~(:value function) arg#)))
      ([]))))

(defmethod emit :inline-integer-selector
  [selector]
  `(~preserve
    (fn [seq#]
      (if (coll? seq#)
        (if-not (empty? seq#)
          (get (vec seq#) ~(:value selector)))))))

(derive ::inline-integer-selection ::inline)
(derive ::inline-expression ::inline)
(derive ::inline-fl-defined ::inline)
(derive ::inline-clojure-call ::inline)

(defmethod emit ::inline
  [inline]
  (list* (emit (:expr inline)) (map emit (:args inline))))

(defmethod emit :object
  [object]
  ;; handle integers inside of forms specially - they are selector functions
  (if (and
       (integer? (:value object))
       (get-in object [:parent-form-behavior :takes-selectors?]))
    `(~preserve #(get (vec %) ~(:value object)))
    (:value object)))

(defn compile [expr]
  (emit (analyze (parse [] expr))))

(defmacro fl [& body]
  `(doseq [expr# '~body] (eval (compile expr#))))

(defn fl-source [name]
  "Look up the fl source for a var: (fl-source #'length)"
  (fl-format (get-in (meta name) [:fl :source])))

(defn repl []
  (prn "Type " :q " to quit.")
  (loop []
    (print "fl: ")
    (flush)
    (let [form (read)]
      (if (= form :q)
        :quit
        (do (try
              (->> `'~form eval compile eval println)
              (catch Exception e (println e))
              (finally (flush)))
            (recur))))))

(comment
  (fl (def x ~1) (def y 123))
  (fl
   (def x ~1)
   (def inner-product (. + (a *) trans))
   (def sum-and-prod ($ + *))
   (def incr (. + ($ id ~1)))
   (def intsto (. range ($ ~1 id) incr))
   (def length (. + (a ~1)))
   (def fact (. * intsto))

   ;; selectors
   (3 [1 2 3 4 5]) ;=> 4
   (($ 0 1) [10 11 12]) ;=> [10 11]

   ;; nil-filtering a, condition form
   ((a (-> even?)) (range 10)) ;=> (0 2 4 6 8)
   ((-> odd? inc dec) 1) ;=> 2
   ((-> integer? id) 1) ;=> 1
   ((-> integer?) 1) ;=> 1
   ((a (-> even? ~:even ~:odd)) (range 5)) ;=> (:even :odd :even :odd :even)

   (def pair? (. = ($ ~2 length)))
   ((a (-> pair? 1 0)) [[1 2] [3] 10 [4 5]]) ;=> (2 3 5)
   ((a (-> pair? 1)) [1 2 [3 4]]) ;=> (4)
   )

  (fl-source #'length)
  )