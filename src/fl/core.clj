(ns fl.core
  (:refer-clojure :exclude (compile)))

(defn preserve [f]
  (letfn [(bottom? [x] (boolean (or (nil? x)
                                    (when (coll? x)
                                      (some nil? x)))))]
    #(when-not (bottom? %) (f %))))

(def primitives
  {'constant {:type :form
              :name 'constant
              :emit `(preserve constantly)
              :alias 'clojure.core/unquote
              :args [#{:function :object}]}

   'compose {:type :form
             :name 'compose
             :nary true
             :emit `(fn [& args#]
                      (reduce comp (map preserve args#)))
             :alias '.
             :args [:function+]}

   'construct {:type :form
               :name 'construct
               :nary true,
               :emit `(fn [& args#]
                        (reduce juxt (map preserve args#)))
               :alias '$
               :args [:function+]}

   'insert {:type :form
            :name 'insert
            :emit `(preserve (fn [f#]
                               #(reduce (fn [xs# y#]
                                          (f# [xs# y#])) %)))
            :alias '/
            :args [:function]}

   'apply-to-all {:type :form
                  :name 'apply-to-all
                  :emit `(fn [f#]
                           (preserve (partial map f#)))
                  :alias 'a
                  :args [#{:function :form}]}

   'condition {:type :form
               :name 'condition
               :nary #{3}
               :emit `(fn [p# f# g#]
                        (fn [x#] (preserve (if (p# x#) (f# x#) (g# x#)))))
               :alias '->
               :args [:function :function :function]}

   'while {:type :form
           :name 'while
           :nary #{2}
           :emit `(fn [p# f#]
                    (fn [x#]
                      (do (while (p# x#)
                            (f# x#))
                          (preserve x#))))
           :args [:function :function]}

   '+ {:type :function
       :name '+
       :emit `(preserve (partial apply +))
       :args [:object]}
   '- {:type :function
       :name '-
       :emit `(preserve (partial apply -))
       :args [:object]}
   '* {:type :function
       :name '*
       :emit `(preserve (partial apply *))
       :args [:object]}
   '% {:type :function
       :name '%
       :emit `(preserve (partial apply /))
       :args [:object]}
   'id {:type :function
        :name 'id
        :emit `(preserve identity)
        :args [:object]}
   'trans {:type :function
           :name 'trans
           :emit `(preserve (partial apply map vector))
           :args [:object]}})

(def named-aliases
  (->> primitives
       (map (fn [[name, spec]] [(:alias spec) name]))
       (filter (comp identity first))
       (map (fn [[alias, name]] [alias, (get primitives name)]))
       (into {})))

(def named-primitives
  (reduce (fn [xs [name spec]]
            (assoc xs name spec))
          {}
          primitives))

(def named-ops (merge named-primitives named-aliases))

(defmulti parse (fn [_ expr] (class expr)))

(defmethod parse clojure.lang.PersistentList
  [env [op & rest :as expr]]
  [(assoc (named-ops op) :env env) (mapcat (partial parse (conj env expr)) rest)])

(defmethod parse clojure.lang.Symbol
  [env sym]
  [(assoc (named-ops sym) :env env)])

(defmethod parse :default
  [env expr]
  [{:env env, :type :object, :value expr}])

(comment
  (use 'clojure.pprint)
  (def ip (compile '((compose + (insert +) (apply-to-all *) trans) [[1 2 3] [6 5 4]])))
  (pprint ip)
  (eval ip) ;=> Exception : (clojure.core/partial clojure.core/apply +) failed with argument 28

  (def ip (compile '((compose (insert +) (apply-to-all *) trans) [[1 2 3] [6 5 4]])))
  (pprint ip)
  (time (dotimes [_ 1000]
          (eval ip)))                        ;=> 28
  (eval (compile '(trans {})))

  (def inner-product (. (/ +) (a *) trans))
  (def sum-and-prod ($ (/ +) (/ *)))
  (def length (. (/ +) (a ~1)))
  (def intsto (. range ($ ~1 inc)))
  (def fact (. (/ *) intsto))
  (def hello (str' ~"Hello " id))
  )
