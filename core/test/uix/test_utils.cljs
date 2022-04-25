(ns uix.test-utils
  (:require ["react-dom/server" :as rserver]
            [goog.object :as gobj]
            [clojure.test :refer [is]]
            [uix.dom :as dom]))

(defn as-string [el]
  (rserver/renderToStaticMarkup el))

(defn js-equal? [a b]
  (gobj/equals a b))

(defn symbol-for [s]
  (js* "Symbol.for(~{})" s))

(defn react-element-of-type? [f type]
  (= (gobj/get f "$$typeof") (symbol-for type)))

(defn with-error [f]
  (let [msgs (atom [])
        cc js/console.error]
    (set! js/console.error #(swap! msgs conj %))
    (f)
    (set! js/console.error cc)
    (is (empty? @msgs))))

(defn render [el]
  (let [node (.createElement js/document "div")
        _ (.append (.getElementById js/document "root") node)
        root (dom/create-root node)]
    (dom/render-root el root)))
