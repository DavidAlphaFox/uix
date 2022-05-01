(ns uix.compiler.aot
  "Runtime helpers for HyperScript compiled into React"
  (:require [react :as react]
            [uix.compiler.input]
            [uix.compiler.alpha :as r]
            [uix.compiler.attributes]
            [uix.lib :refer [doseq-loop]]))

(defn hiccup? [el]
  (when (vector? el)
    (let [tag (nth el 0 nil)]
      (or (keyword? tag)
          (symbol? tag)
          (fn? tag)
          (instance? MultiFn tag)))))

(defn validate-children [children]
  (doseq-loop [child children]
    (cond
      (hiccup? child)
      (throw (js/Error. (str "Hiccup is not valid as UIx child (found: " child ").\n"
                             "If you meant to render UIx element, use `$` macro, i.e. ($ " child ")\n"
                             "If you meant to render Reagent element, wrap it with r/as-element, i.e. (r/as-element " child ")")))

      (sequential? child)
      (validate-children child)))
  true)

(defn >el [tag attrs-children children]
  (let [args (-> #js [tag] (.concat attrs-children) (.concat children))]
    (when ^boolean goog.DEBUG
      (validate-children (.slice args 2)))
    (.apply react/createElement nil args)))

(defn create-uix-input [tag attrs-children children]
  (let [props (aget attrs-children 0)
        children (.concat #js [(aget attrs-children 1)] children)]
    (react/createElement uix.compiler.input/reagent-input #js {:props props :tag tag :children children})))

(def suspense react/Suspense)
(def fragment react/Fragment)

(def ^:dynamic *memo-cache*)

(defn use-memo-cache [f id deps]
  ;; every UIx component gets assigned a use-ref to *memo-cache*
  ;; every element in the component gets a unique ID
  (if *memo-cache*
    (or (get-in @*memo-cache* [id deps])
        (let [el (f)]
          (swap! *memo-cache* assoc id {deps el})
          el))
    (f)))
