(defproject fl "1.0.0-SNAPSHOT"
  :description "Function-level language"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.antlr/antlr "3.4"]]
  :plugins [[lein-swank "1.4.4"]]
  :compile-path "classes/clj"
  :extra-classpath-dirs ["classes/java"])
