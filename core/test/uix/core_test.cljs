(ns uix.core-test
  (:require [clojure.test :refer [deftest is async testing run-tests]]
            [uix.core :as uix]
            ;[uix.core.lazy-loader :refer [require-lazy]]
            [uix.lib]
            [react :as r]
            [uix.test-utils :as t]
            [cljs-bean.core :as bean]))

(deftest test-lib
  (is (= (seq (uix.lib/re-seq* (re-pattern "foo") "foo bar foo baz foo zot"))
         (list "foo" "foo" "foo")))

  (is (= (map vec (uix.lib/re-seq* (re-pattern "f(.)o") "foo bar foo baz foo zot"))
         (list ["foo" "o"] ["foo" "o"] ["foo" "o"])))

  (is (= '("") (seq (uix.lib/re-seq* #"\s*" "")))))

#_(deftest test-memoize
    (uix.core/defui test-memoize-comp [{:keys [x]}]
      (is (= 1 x))
      ($ :h1 x))
    (let [f (uix.core/memoize test-memoize-comp)]
      (is (t/react-element-of-type? f "react.memo"))
      (is (= "<h1>1</h1>" (t/as-string ($ f {:x 1}))))))

#_(deftest test-require-lazy
    (require-lazy '[uix.core :refer [strict-mode]])
    (is (t/react-element-of-type? strict-mode "react.lazy")))

(deftest test-html
  (is (t/react-element-of-type? ($ :h1 1) "react.element")))

(deftest test-defui
  (defui h1 [{:keys [children]}]
    ($ :h1 {} children))
  (is (= (t/as-string ($ h1 {} 1))) "<h1>1</h1>"))

(deftest test-as-react
  (let [ctor (fn [props]
               (is (= @#'bean/Bean (type props)))
               (:x props))
        ftype (-> (uix.core/as-react ctor)
                  ^js (r/createElement)
                  .-type)]
    (is (= (ftype #js {:x 1}) 1))))

(deftest test-create-error-boundary-1
  (let [error->state-called? (atom false)
        handle-catch-called? (atom false)
        err-b (uix.core/create-error-boundary
                {:display-name "err-b-1"
                 :error->state #(reset! error->state-called? true)
                 :handle-catch #(reset! handle-catch-called? true)}
                (fn [err {:keys [done x]}]
                  (is (= nil @err))
                  (is (= 1 x))
                  (is (= false @error->state-called?))
                  (is (= false @handle-catch-called?))
                  (done)
                  x))]
    (async done
      (t/render ($ err-b {:done done :x 1})))))

#_(deftest test-create-error-boundary-2
    (let [handle-catch (atom nil)
          child (fn [] (throw (js/Error. "Hello!")))
          err-b (uix.core/create-error-boundary
                  {:display-name "err-b-2"
                   :error->state ex-message
                   :handle-catch (fn [err info] (reset! handle-catch err))}
                  (fn [cause {:keys [done x child]}]
                    (is (= 1 x))
                    (cond
                      (nil? @cause) child
                    
                      (= :recover @cause)
                      (do
                        (is (some? @handle-catch))
                        (done)
                        x)
                    
                      :else (do (is (= "Hello!" @cause))
                                (js/setTimeout #(reset! cause :recover) 20)
                                x))))]
      (async done
        (t/render ($ err-b {:done done :x 1 :child child})))))

(defn -main []
  (run-tests))
