(ns uix.test-runner
  (:require [clojure.test :refer :all]
            [uix.core-test]
            [uix.aot-test]
            [uix.hooks-test]
            [uix.hooks.linter-test]))

(defn -main [& args]
  (run-tests
   'uix.core-test
   'uix.aot-test
   'uix.hooks-test
   'uix.hooks.linter-test))
