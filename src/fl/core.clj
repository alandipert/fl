(ns fl.core
  (:use [clojure.string :only (join)]
        [clojure.walk :only (postwalk)]))

(defn preserve [f]
  (letfn [(bottom? [x] (boolean (or (nil? x)
                                    (when (coll? x)
                                      (some nil? x)))))]
    #(when-not (bottom? %) (f %))))

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
                      (reduce comp (map preserve args#)))}

   '$ 'construct
   'construct {:type :form
               :nary true,
               :emit `(fn [& args#]
                        (reduce juxt (map preserve args#)))}

   '/ 'insert
   'insert {:type :form
            :emit `(preserve (fn [f#]
                               #(reduce (fn [xs# y#]
                                          (f# [xs# y#])) %)))}

   'a 'apply-to-all
   'apply-to-all {:type :form
                  :emit `(fn [f#]
                           (preserve (partial map f#)))}

   '-> 'condition
   'condition {:type :form
               :nary #{3}
               :emit `(fn [p# f# g#]
                        (fn [x#] (preserve (if (p# x#) (f# x#) (g# x#)))))}

   'while {:type :form
           :nary #{2}
           :emit `(fn [p# f#]
                    (fn [x#]
                      (do (while (p# x#)
                            (f# x#))
                          (preserve x#))))}

   '+ {:type :function
       :emit `(preserve (partial apply +))}
   '- {:type :function
       :emit `(preserve (partial apply -))}
   '* {:type :function
       :emit `(preserve (partial apply *))}
   '% {:type :function
       :emit `(preserve (partial apply /))}
   'id {:type :function
        :emit `(preserve identity)}
   'trans {:type :function
           :emit `(preserve (partial apply map vector))}
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

(defn flpp [flform]
  (postwalk (fn [f]
              (if (list? f)
                (if (= (first f) 'clojure.core/unquote)
                  (symbol (str "~" (flpp (second f))))
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

   ;; named form or function
   (get-prim op)
   (assoc (get-prim op),
     :env env,
     :args (map (partial parse (conj env expr)) more))

   ;; invocation
   (symbol? op)
   {:type :inline,
    :expr {:type :object, :value op, :env env}
    :env env
    :args (map (partial parse (conj env expr)) more)}

   ;; inline invocation
   (list? op)
   {:type :inline,
    :expr (map (partial parse (conj env expr)) op)
    :env env,
    :args (map (partial parse (conj env expr)) more)}))

(defmethod parse clojure.lang.Symbol
  [env sym]
  (if (get-prim sym)
    (assoc (get-prim sym) :env env)
    {:type :object, :value sym, :env env}))

(defmethod parse clojure.lang.PersistentVector
  [env v]
  {:env env, :type :object, :value v})

(defmethod parse clojure.lang.PersistentArrayMap
  [env _]
  (throw (Exception.
          (str "- Maps are not supported.  At: "
               (if (empty? env)
                 "Top level."
                 (join " → " (flpp (conj env {}))))))))

(defmethod parse :default
  [env expr]
  {:env env, :type :object, :value expr})

;;; analyze

(defmulti analyze :type)

(defmethod analyze :default
  [object]
  object)

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
                    {:fl '~form})
       #'~(:value (first (:args form))))
    (if (empty? (:args form))
      (:emit form)
      (list* (:emit form) (map emit (:args form))))))

(defmethod emit :function
  [function]
  (if (empty? (:args function))
    (:emit function)
    (list* (:emit function) (map emit (:args function)))))

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
        (do (println (flpp (eval (emit (analyze (parse [] (eval `(identity '~form))))))))
            (flush)
            (recur))))))

(comment
  (fl
   (def x ~1)
   (def inner-product (. (/ +) (a *) trans))
   (def sum-and-prod ($ (/ +) (/ *)))
   (def length (. (/ +) (a ~1)))
   )

  (fl-source #'length)
  )