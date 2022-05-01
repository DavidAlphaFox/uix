(ns uix.compiler.aot
  "Compiler code that translates HyperScript into React calls at compile-time."
  (:require [uix.compiler.js :as js]
            [uix.compiler.attributes :as attrs]
            [uix.compiler.memo :as memo]))

(defmulti compile-attrs (fn [tag attrs opts] tag))

(defmethod compile-attrs :element [_ attrs {:keys [tag-id-class]}]
  (if (or (map? attrs) (nil? attrs))
    `(cljs.core/array
      ~(cond-> attrs
         (and (some? (:style attrs))
              (not (map? (:style attrs))))
         (assoc :style `(uix.compiler.attributes/convert-props ~(:style attrs) (cljs.core/array) true))
         :always (attrs/set-id-class tag-id-class)
         :always (attrs/compile-attrs {:custom-element? (last tag-id-class)})
         :always js/to-js))
    `(uix.compiler.attributes/interpret-attrs ~attrs (cljs.core/array ~@tag-id-class) false)))

(defmethod compile-attrs :component [_ props _]
  (if (or (map? props) (nil? props))
    `(cljs.core/array ~props)
    `(uix.compiler.attributes/interpret-props ~props)))

(defmethod compile-attrs :fragment [_ attrs _]
  (if (map? attrs)
    `(cljs.core/array ~(-> attrs attrs/compile-attrs js/to-js))
    `(uix.compiler.attributes/interpret-attrs ~attrs (cljs.core/array) false)))

(defmethod compile-attrs :suspense [_ attrs _]
  (if (map? attrs)
    `(cljs.core/array ~(-> attrs attrs/compile-attrs js/to-js))
    `(uix.compiler.attributes/interpret-attrs ~attrs (cljs.core/array) false)))

(defmethod compile-attrs :interop [_ props _]
  (if (map? props)
    `(cljs.core/array
      ~(cond-> props
         :always (attrs/compile-attrs {:custom-element? true})
         :always (js/to-js-map true)))
    `(uix.compiler.attributes/interpret-attrs ~props (cljs.core/array) true)))

;; Compiles HyperScript into React.createElement
(defmulti compile-element
  (fn [[tag] opts]
    (cond
      (= :<> tag) :fragment
      (= :# tag) :suspense
      (= :> tag) :interop
      (keyword? tag) :element
      :else :component)))

(defn- input-component? [x]
  (contains? #{"input" "textarea"} x))

(defmethod compile-element :element [v {:keys [env form]}]
  (let [[tag attrs & children] v
        tag-id-class (attrs/parse-tag tag)
        tag-str (first tag-id-class)
        create-el (fn [attrs children]
                    (let [attrs-children (compile-attrs :element attrs {:tag-id-class tag-id-class})]
                      (if (input-component? tag-str)
                        `(create-uix-input ~tag-str ~attrs-children (cljs.core/array ~@children))
                        `(>el ~tag-str ~attrs-children (cljs.core/array ~@children)))))]
    (memo/memoize-element env form attrs children create-el)))

(defmethod compile-element :component [v {:keys [env form]}]
  (let [[tag props & children] v
        tag (vary-meta tag assoc :tag 'js)
        create-el (fn [props children]
                    (let [props-children (compile-attrs :component props nil)]
                      `(uix.compiler.alpha/component-element ~tag ~props-children (cljs.core/array ~@children))))]
    (memo/memoize-element env form props children create-el)))

(defmethod compile-element :fragment [v opts]
  (let [[_ attrs & children] v
        attrs (compile-attrs :fragment attrs nil)
        ret `(>el fragment ~attrs (cljs.core/array ~@children))]
    ret))

(defmethod compile-element :suspense [v opts]
  (let [[_ attrs & children] v
        attrs (compile-attrs :suspense attrs nil)
        ret `(>el suspense ~attrs (cljs.core/array ~@children))]
    ret))

(defmethod compile-element :interop [v opts]
  (let [[_ tag props & children] v
        props (compile-attrs :interop props nil)]
    `(>el ~tag ~props (cljs.core/array ~@children))))
