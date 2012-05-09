(ns fl.core
  (:use [clojure.string :only (join)]
        [clojure.walk :only (postwalk)]))

(def preserve-bottom [f]
  `(fn
     ([arg] (when-not (nil? arg) (f arg)))
     ([])))

(def primitives
  {'def {:type :form
         :nary true}

   'clojure.core/unquote 'constant
   'constant {:type :form
              :emit `(preserve constantly)}

   '. 'compose
   'compose {:type :form
             :nary true
             :emit `(fn [& args#]
                      (reduce comp (map ~preserve-bottom args#)))}

   '$ 'construct
   'construct {:type :form
               :nary true,
               :emit `(fn [& args#]
                        (apply juxt (map ~preserve-bottom args#)))}

   '/ 'insert
   'insert {:type :form
            :emit `(~preserve-bottom
                    (fn [f#]
                      #(reduce (fn [xs# y#]
                                 (f# [xs# y#])) %)))}

   'a 'apply-to-all
   'apply-to-all {:type :form
                  :emit `(fn [f#]
                           (~preserve-bottom (partial map f#)))}

   '-> 'condition
   'condition {:type :form
               :nary #{3}
               :emit `(~preserve-bottom
                       (fn [p# f# g#]
                         (fn [x#] (if (p# x#) (f# x#) (g# x#)))))}

   'while {:type :form
           :nary #{2}
           :emit `(fn [p# f#]
                    (fn [x#]
                      (do (while (p# x#)
                            (f# x#)))))}

   '+ {:type :function
       :emit `(~preserve-bottom (partial apply +))}
   '- {:type :function
       :emit `(~preserve-bottom (partial apply -))}
   '* {:type :function
       :emit `(~preserve-bottom (partial apply *))}
   '% {:type :function
       :emit `(~preserve-bottom (partial apply /))}
   'id {:type :function
        :emit `(~preserve-bottom identity)}
   'trans {:type :function
           :emit `(~preserve-bottom (partial apply map vector))}
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

   ;; primitive form or function
   (get-prim op)
   (assoc (get-prim op),
     :env env,
     :args (map (partial parse (conj env expr)) more))

   ;; invocation
   (symbol? op)
   (cond
    ;; defined FL function, form, or value
    (fl-def? op)
    {:type :inline,
     :expr {:type :object,
            :value op,
            :env env}
     :env env
     :args (map (partial parse (conj env expr)) more)}

    ;; defined clojure value
    (clojure-def? op)
    {:type :clojure-call,
     :expr {:type :clojure-function,
            :value op,
            :env env}
     :env env
     :args (map (partial parse (conj env expr)) more)})

   ;; inline expression
   (list? op)
   {:type :inline,
    :expr (map (partial parse (conj env expr)) op)
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

(defn flpp [flform]
  (postwalk (fn [f]
              (if (list? f)
                (if (= (first f) 'clojure.core/unquote)
                  (symbol (str "~" (flpp (second f))))
                  f)
                f))
            flform))
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
                    {:fl {:source '~(flpp (first (:env (second (:args form)))))}})
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
  `(~preserve-bottom
    (fn
      ([arg#]
         (if (coll? arg#)
           (apply ~(:value function) arg#)
           (~(:value function) arg#)))
      ([]))))

(defmethod emit :clojure-call
  [call]
  (list* (emit (:expr call)) (map emit (:args call))))

(defmethod emit :inline
  [inline]
  (if (= :object (:type (:expr inline)))
    (list* (:value (:expr inline)) (map emit (:args inline)))
    (list* (map emit (:expr inline)) (map emit (:args inline)))))

(defmethod emit :object
  [object]
  (:value object))

(defn emitn [exprs]
  (doseq [expr exprs]
    (eval (emit (analyze (parse [] expr))))))

(defmacro fl [& body]
  `(do ~@(eval `(emitn '~body))))

(defn fl-source [name]
  "Look up the fl source for a var: (fl-source #'length)"
  (flpp (first (:env (first (get-in (meta name) [:fl :args 1 :args]))))))

(defn repl []
  (prn "Type " :q " to quit.")
  (loop []
    (print "fl: ")
    (flush)
    (let [form (read)]
      (if (= form :q)
        :quit
        (do (try
              (->> `(identity '~form)
                   eval
                   (parse [])
                   analyze
                   emit
                   eval
                   flpp
                   println)
              (catch Exception e
                (println e))
              (finally (flush)))
            (recur))))))

(comment
  (fl
   (def x ~1)
   (def inner-product (. (/ +) (a *) trans))
   (def sum-and-prod ($ (/ +) (/ *)))
   (def incr (. (/ +) ($ id ~1)))
   (def intsto (. range ($ ~1 id) inc))
   (def fact (. * intsto))

   (. * )
   )

  (fl-source #'length)
  )