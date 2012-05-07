(ns fl.core
  (:refer-clojure :exclude (compile)))

(defn bottom? [x]
  (boolean (or (nil? x) (when (coll? x) (some nil? x)))))

;; (defn preserve [f]
;;   #(when-not (bottom? %) (try (f %)
;;                               (catch Throwable t
;;                                 (throw (RuntimeException.
;;                                         (format "Function %s was not able to receive argument %s" f %)))))))

(defmacro preserve [f]
  `(let [fsrc# '~f
         f# ~f]
     (fn [arg#]
       (when-not (bottom? arg#)
         (try (f# arg#)
              (catch Throwable t#
                (throw (Exception.
                        (format ": %s failed with argument '%s'" fsrc# arg#)))))))))

(defn fform [f]
  (fn [& args]
    (reduce f args)))

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
  (use 'clojure.pprint)
  (def ip (compile '((compose + (insert +) (apply-to-all *) trans) [[1 2 3] [6 5 4]])))
  (pprint ip)
  (eval ip) ;=> Exception : (clojure.core/partial clojure.core/apply +) failed with argument 28

  (def ip (compile '((compose (insert +) (apply-to-all *) trans) [[1 2 3] [6 5 4]])))
  (pprint ip)
  (eval ip) ;=> 28
  )