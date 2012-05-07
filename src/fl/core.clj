(ns fl.core
  (:refer-clojure :exclude (compile)))

(defn bottom? [x]
  (boolean (or (nil? x) (when (coll? x) (some nil? x)))))

(defn preserve [f]
  (fn [arg]
    (when-not (bottom? arg) (f arg))))

(defn fform [f]
  (fn [& args]
    (reduce f (map preserve args))))

(def primitives
  {'compose `(fform comp)
   'construct `(fform juxt)
   '+ `(preserve (partial apply +))
   '- `(preserve (partial apply -))
   '* `(preserve (partial apply *))
   '% `(preserve (partial apply %))
   'id `(preserve identity)
   'constant `(preserve constantly)
   'trans `(preserve (partial apply map vector))
   'insert `(fn [f#] (preserve #(reduce (fn [xs# y#] (f# [xs# y#])) %)))
   'apply-to-all `(fn [f#] (preserve (partial map f#)))})

(defn compile [expr]
  (cond
   
   (symbol? expr)
   (or (primitives expr)
       (find-var `expr)
       (throw (RuntimeException. (str "Symbol '" (name expr) "' is undefined."))))
   
   (list? expr)
   (map compile1 expr)
   
   ;; TODO nth primitives, e.g. 2:x
   :else expr))

(comment
  (use 'clojure.pprint)
  (def ip (compile '((compose + (insert +) (apply-to-all *) trans) [[1 2 3] [6 5 4]])))
  (pprint ip)
  (eval ip) ;=> Exception : (clojure.core/partial clojure.core/apply +) failed with argument 28
  
  (def ip (compile '((compose (insert +) (apply-to-all *) trans) [[1 2 3] [6 5 4]])))
  (pprint ip)
  (eval ip) ;=> 28
  (eval (compile '(trans {})))

  (def inner-product (. (/ +) (a *) trans))
  (def sum-and-prod ($ (/ +) (/ *)))
  (def length (. (/ +) (a ~1)))
  (def intsto (. range ($ ~1 inc)))
  (def fact (. (/ *) intsto))
  (def hello (str' ~"Hello " id))
  )
