(ns fl.core
  (:use [clojure.string :only (join)]
        [clojure.walk :only (postwalk-replace)]))

(defn preserve [f]
  (letfn [(bottom? [x] (boolean (or (nil? x)
                                    (when (coll? x)
                                      (some nil? x)))))]
    #(when-not (bottom? %) (f %))))

(def primitives
  {'def {:type :form
         :nary true}
   'constant {:type :form
              :emit `(preserve constantly)
              :alias 'clojure.core/unquote}

   'compose {:type :form
             :nary true
             :emit `(fn [& args#]
                      (reduce comp (map preserve args#)))
             :alias '.}

   'construct {:type :form
               :nary true,
               :emit `(fn [& args#]
                        (reduce juxt (map preserve args#)))
               :alias '$}

   'insert {:type :form
            :emit `(preserve (fn [f#]
                               #(reduce (fn [xs# y#]
                                          (f# [xs# y#])) %)))
            :alias '/}

   'apply-to-all {:type :form
                  :emit `(fn [f#]
                           (preserve (partial map f#)))
                  :alias 'a}

   'condition {:type :form
               :nary #{3}
               :emit `(fn [p# f# g#]
                        (fn [x#] (preserve (if (p# x#) (f# x#) (g# x#)))))
               :alias '->}

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

(def named-aliases
  (->> primitives
       (map (fn [[name, spec]] [(:alias spec) name]))
       (filter (comp identity first))
       (map (fn [[alias, name]] [alias, (get primitives name)]))
       (into {})))

(def named-primitives
  (reduce (fn [xs [name spec]]
            (assoc xs name (assoc spec :args [], :name name)))
          {}
          primitives))

(def named-ops (merge named-primitives named-aliases))

;;; parse

(defmulti parse (fn [_ expr] (class expr)))

(defmethod parse clojure.lang.PersistentList
  [env [op & more :as expr]]
  (if (= op 'def)
    (assoc (named-ops 'def)
      :env env,
      :args [{:type :object, :value (first more)},
             (parse (conj env expr) (first (rest more)))])
    (assoc (named-ops op),
      :env env,
      :args (map (partial parse (conj env expr)) more))))

(defmethod parse clojure.lang.Symbol
  [env sym]
  (assoc (named-ops sym) :env env))

(defmethod parse clojure.lang.PersistentVector
  [env v]
  {:env env, :type :object, :value v})

(defmethod parse clojure.lang.PersistentArrayMap
  [env _]
  (throw (Exception.
          (str "- Maps are not supported.  At: "
               (if (empty? env)
                 "Top level."
                 (join " → "
                       (postwalk-replace
                        {'clojure.core/unquote (symbol "~")} (conj env {}))))))))

(defmethod parse :default
  [env expr]
  {:env env, :type :object, :value expr})

;;; analyze

(defmulti analyze :type)

(defmethod analyze :form
  [form]
  (let [arg-count (count (:args form))]
    (if (and (> arg-count 1) (not (:nary form)))
      (throw (Exception. (format "- Form %s was expecting 1 argument but was passed %s: %s"
                                 (:name form)
                                 arg-count
                                 (apply str (join ", " (map (comp name :type) (:args form)))))))
      (do
        (map analyze (:args form))
        form))))

(defmethod analyze :function
  [function]
  (do
    (map analyze (:args function))
    function))

(defmethod analyze :object
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
                    {:fl '~form}))
    (if (empty? (:args form))
      (:emit form)
      (list* (:emit form) (map emit (:args form))))))

(defmethod emit :function
  [function]
  (if (empty? (:args function))
    (:emit function)
    (list* (:emit function) (map emit (:args function)))))

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
  (postwalk-replace
   {'clojure.core/unquote (symbol "~")}
   (first (:env (first (get-in (meta name) [:fl :args 1 :args]))))))

(comment
  (fl
   (def x ~1)
   (def inner-product (. (/ +) (a *) trans))
   (def sum-and-prod ($ (/ +) (/ *)))
   (def length (. (/ +) (a ~1)))
   )

  (fl-source #'length)
  )