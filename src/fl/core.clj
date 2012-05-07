(ns fl.core
  (:refer-clojure :exclude (compile)))

(defn preserve [f]
  (letfn [(bottom? [x] (boolean (or (nil? x)
                                    (when (coll? x)
                                      (some nil? x)))))]
    #(when-not (bottom? %) (f %))))

(def primitives
  {:forms {'constant {:emit `(preserve constantly)
                      :aliases '#{clojure.core/unquote}}
           'compose {:nary true,
                     :emit `(fn [& args#]
                              (reduce comp (map preserve args#)))
                     :aliases '#{.}}
           'construct {:nary true,
                       :emit `(fn [& args#]
                                (reduce juxt (map preserve args#)))
                       :aliases '#{$}}
           'insert {:emit `(preserve (fn [f#]
                                       #(reduce (fn [xs# y#]
                                                  (f# [xs# y#])) %)))
                    :aliases '#{/}}
           'apply-to-all {:emit `(fn [f#]
                                   (preserve (partial map f#)))
                          :aliases '#{a}}
           'condition {:nary #{3}
                       :emit `(fn [p# f# g#]
                                (fn [x#] (preserve (if (p# x#) (f# x#) (g# x#)))))
                       :aliases '#{->}}
           'while {:nary #{2}
                   :emit `(fn [p# f#]
                            (fn [x#]
                              (do (while (p# x#)
                                    (f# x#))
                                  (preserve x#))))}}
   :functions {'+ {:emit `(preserve (partial apply +))}
               '- {:emit `(preserve (partial apply -))}
               '* {:emit `(preserve (partial apply *))}
               '% {:emit `(preserve (partial apply %))}
               'id {:emit `(preserve identity)}
               'trans {:emit `(preserve (partial apply map vector))}}})

(defn compile [expr]
  (cond

   (symbol? expr)
   (or (primitives expr)
       (find-var `expr)
       (throw (RuntimeException. (str "Symbol '" (name expr) "' is undefined."))))

   (list? expr)
   (map compile expr)

   ;; TODO nth primitives, e.g. 2:x
   :else expr))

(defmulti analyze (fn [_ expr] (class expr)))

(defmethod analyze clojure.lang.Symbol
  [env sym] (println sym))

(defmethod analyze clojure.lang.PersistentList
  [env [op & rest]] (println op))

(defmethod analyze :default
  [env expr] (println expr))

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
