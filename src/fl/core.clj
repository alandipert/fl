(ns fl.core
  (:refer-clojure :exclude (compile))
  (:use [clojure.core.match :only (match)]))

(defn bottom? [x]
  (boolean (or (nil? x) (when (coll? x) (some nil? x)))))

(defn preserve [f]
  #(when-not (bottom? %) (f %)))

(defn fform [f]
  #(reduce f (map preserve %&)))

(def functionals
  {'compose `(fform comp)
   'construct `(fform juxt)})

(def math-primitives
  (into {} (map #(vector %1 (%2 %1))
                '[+ - * /]
                (repeat #(do `(preserve (partial apply ~%)))))))

(def primitives
  (merge math-primitives
         {'id `(preserve identity)
          'constant `(preserve constantly)
          'trans `(preserve (partial apply map vector))
          'insert `(fn [f#] (preserve #(reduce (fn [xs# y#] (f# [xs# y#])) %)))
          'apply-to-all `(fn [f#] (preserve (partial map f#)))}))

(defn compile1 [expr]
  (cond
   
   (symbol? expr)
   (or (primitives expr)
       (find-var `expr)
       (throw (RuntimeException. (str "Symbol " (name expr) " is undefined."))))

   (list? expr)
   (let [[op & args] expr]
     (if-let [functional (functionals op)]
       `(~functional ~@(map compile1 args))
       (if-let [primitive (primitives op)]
         `(~primitive ~@(map compile1 args))
         (throw (RuntimeException. (str "Op " op " is undefined."))))))

   ;; TODO nth primitives, e.g. 2:x
   :else expr))

(defn compile [exprs] (map compile1 exprs))

(comment
  (def ip (compile '((compose (insert +) (apply-to-all *) trans) [[1 2 3] [6 5 4]])))
  (pprint ip) ;=>
  ;; (((fl.core/fform clojure.core/comp)
  ;;   ((clojure.core/fn
  ;;     [f__2804__auto__]
  ;;     (fl.core/preserve
  ;;      (fn*
  ;;       [p1__2803__2805__auto__]
  ;;       (clojure.core/reduce
  ;;        (clojure.core/fn
  ;;         [xs__2806__auto__ y__2807__auto__]
  ;;         (f__2804__auto__ [xs__2806__auto__ y__2807__auto__]))
  ;;        p1__2803__2805__auto__))))
  ;;    (fl.core/preserve (clojure.core/partial clojure.core/apply +)))
  ;;   ((clojure.core/fn
  ;;     [f__2808__auto__]
  ;;     (fl.core/preserve
  ;;      (clojure.core/partial clojure.core/map f__2808__auto__)))
  ;;    (fl.core/preserve (clojure.core/partial clojure.core/apply *)))
  ;;   (fl.core/preserve
  ;;    (clojure.core/partial
  ;;     clojure.core/apply
  ;;     clojure.core/map
  ;;     clojure.core/vector)))
  ;;  [[1 2 3] [6 5 4]])
  (eval ip) 
  ;; => 28
  )