(ns uix.core
  "Public API"
  (:refer-clojure :exclude [fn])
  (:require [clojure.core :as core]
            [uix.compiler.aot]
            [uix.source]
            [cljs.core]
            [uix.linter]
            [uix.dev]
            [uix.lib]
            [uix.hooks.alpha :as hooks]))

(def ^:private goog-debug (with-meta 'goog.DEBUG {:tag 'boolean}))
;;没有参数的组件
(defn- no-args-component [sym var-sym body];; 函数名，带命名空间的函数名，函数体
  `(defn ~sym []
     (let [f# (core/fn [] ~@body)]
       (if ~goog-debug
         (binding [*current-component* ~var-sym] (f#))
         (f#)))))

(defn- with-args-component [sym var-sym args body]
  `(defn ~sym [props#] ;; props#是用来生成随机名称的
     (let [clj-props# (glue-args props#)
           ~args (cljs.core/array clj-props#)
           f# (core/fn [] ~@body)]
       (if ~goog-debug
         (binding [*current-component* ~var-sym]
           (assert (or (map? clj-props#)
                       (nil? clj-props#))
                   (str "UIx component expects a map of props, but instead got " clj-props#))
           (f#))
         (f#)))))

(defn- no-args-fn-component [sym var-sym body]
  `(core/fn ~sym []
     (let [f# (core/fn [] ~@body)]
       (if ~goog-debug
         (binding [*current-component* ~var-sym] (f#))
         (f#)))))

(defn- with-args-fn-component [sym var-sym args body]
  `(core/fn ~sym [props#]
     (let [clj-props# (glue-args props#)
           ~args (cljs.core/array clj-props#)
           f# (core/fn [] ~@body)]
       (if ~goog-debug
         (binding [*current-component* ~var-sym]
           (assert (or (map? clj-props#)
                       (nil? clj-props#))
                   (str "UIx component expects a map of props, but instead got " clj-props#))
           (f#))
         (f#)))))

(defn parse-sig [form name fdecl]
  (let [[fdecl m] (if (string? (first fdecl)) ;;如果form第一个元素是string
                    [(next fdecl) {:doc (first fdecl)}]
                    [fdecl {}])
        [fdecl m] (if (map? (first fdecl))
                    [(next fdecl) (conj m (first fdecl))]
                    [fdecl m])
        fdecl (if (vector? (first fdecl))
                (list fdecl)
                fdecl)
        [fdecl m] (if (map? (last fdecl))
                    [(butlast fdecl) (conj m (last fdecl))]
                    [fdecl m])
        m (conj {:arglists (list 'quote (#'cljs.core/sigs fdecl))} m);;获取参数列表
        m (conj (if (meta name) (meta name) {}) m)];;获取meta
    (uix.lib/assert!
     (= 1 (count fdecl));;如果body > 1 说明我们用的是multi-arity，但是当前并不支持
     (str form " doesn't support multi-arity.\n"
          "If you meant to make props an optional argument, you can safely skip it and have a single-arity component.\n
                 It's safe to destructure the props value even if it's `nil`."))
    (let [[args & fdecl] (first fdecl)]
      (uix.lib/assert!
       (>= 1 (count args)) ;;args需要大于1
       (str form " is a single argument component taking a map of props, found: " args "\n"
            "If you meant to retrieve `children`, they are under `:children` field in props map."))
      [(with-meta name m) args fdecl])))

(defmacro
  ^{:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
                [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])}
  defui
  "Creates UIx component. Similar to defn, but doesn't support multi arity.
  A component should have a single argument of props."
  [sym & fdecl]
  (let [[fname args fdecl] (parse-sig `defui sym fdecl)]
    (uix.linter/lint! sym fdecl &form &env)
    (if (uix.lib/cljs-env? &env)
      (let [var-sym (-> (str (-> &env :ns :name) "/" sym) symbol (with-meta {:tag 'js})) ;;创建带有namespace的函数名
            body (uix.dev/with-fast-refresh var-sym fdecl)];;生成可以进行hot reload的代码
        `(do
           ~(if (empty? args) ;;判断参数是否为空
              (no-args-component fname var-sym body)
              (with-args-component fname var-sym args body))
           (set! (.-uix-component? ~var-sym) true);;设置对应的函数为uix-component 例如（set! (.-uix-component? 'uix.examples/toolbar))
           (set! (.-displayName ~var-sym) ~(str var-sym));;设置函数的
           ~(uix.dev/fast-refresh-signature var-sym body))) ;;为开发环境服务？
      `(defn ~fname ~args
         ~@fdecl))));;如果是非cljs环境，直接生成函数

(defmacro fn
  "Creates anonymous UIx component. Similar to fn, but doesn't support multi arity.
  A component should have a single argument of props."
  [& fdecl]
  (let [[sym fdecl] (if (symbol? (first fdecl))
                      [(first fdecl) (rest fdecl)]
                      [(gensym "uix-fn") fdecl])
        [fname args body] (parse-sig `fn sym fdecl)]
    (uix.linter/lint! sym body &form &env)
    (if (uix.lib/cljs-env? &env)
      (let [var-sym (with-meta sym {:tag 'js})]
        `(let [~var-sym ~(if (empty? args)
                           (no-args-fn-component fname var-sym body)
                           (with-args-fn-component fname var-sym args body))]
           (set! (.-uix-component? ~var-sym) true)
           (set! (.-displayName ~var-sym) ~(str var-sym))
           ~var-sym))
      `(core/fn ~fname ~args
         ~@fdecl))))

(defmacro source
  "Returns source string of UIx component"
  [sym]
  (uix.source/source &env sym))

(defmacro $
  "Creates React element

  DOM element: ($ :button#id.class {:on-click handle-click} \"click me\")
  React component: ($ title-bar {:title \"Title\"})"
  ([tag]
   (uix.linter/lint-element* &form &env)
   (uix.compiler.aot/compile-element [tag] {:env &env}))
  ([tag props & children]
   (uix.linter/lint-element* &form &env)
   (uix.compiler.aot/compile-element (into [tag props] children) {:env &env})))

;; === Error boundary ===

(defn create-error-boundary
  "Creates React's error boundary component

  display-name       — the name of the component to be displayed in stack trace
  derive-error-state — maps error object to component's state that is used in render-fn
  did-catch          — 2 arg function for side-effects, logging etc.
  receives the exception and additional component info as args
  render-fn          — takes state value returned from derive-error-state and a vector
  of arguments passed into error boundary"
  [{:keys [display-name derive-error-state did-catch]
    :or   {display-name (str (gensym "uix.error-boundary"))}}
   render-fn]
  ^::error-boundary {:display-name       display-name
                     :render-fn          render-fn
                     :did-catch          did-catch
                     :derive-error-state derive-error-state})

;; === Hooks ===

(defn use-state [value]
  (hooks/use-state value))

(defn use-reducer
  ([f value]
   (hooks/use-reducer f value))
  ([f value init-state]
   (hooks/use-reducer f value init-state)))

(defn use-ref [value]
  (hooks/use-ref value))

(defn use-context [value]
  (hooks/use-context value))

(defn use-debug
  ([v]
   (hooks/use-debug v))
  ([v fmt]
   (hooks/use-debug v fmt)))

(defn use-deferred-value [v]
  (hooks/use-deferred-value v))

(defn use-transition []
  (hooks/use-transition))

(defn use-id []
  (hooks/use-id))

(defn use-sync-external-store
  ([subscribe get-snapshot]
   (hooks/use-sync-external-store subscribe get-snapshot))
  ([subscribe get-snapshot get-server-snapshot]
   (hooks/use-sync-external-store subscribe get-snapshot get-server-snapshot)))

(defn vector->js-array [coll]
  (cond
    (vector? coll) `(jsfy-deps (cljs.core/array ~@coll))
    (some? coll) `(jsfy-deps ~coll)
    :else coll))

(defn- make-hook-with-deps [sym env form f deps]
  (when (uix.lib/cljs-env? env)
    (uix.linter/lint-exhaustive-deps! env form f deps))
  (if deps
    (if (uix.lib/cljs-env? env)
      `(~sym ~f ~(vector->js-array deps))
      `(~sym ~f ~deps))
    `(~sym ~f)))

(defmacro use-effect
  "Takes a function to be executed in an effect and optional vector of dependencies.

  See: https://reactjs.org/docs/hooks-reference.html#useeffect"
  ([f]
   (make-hook-with-deps 'uix.hooks.alpha/use-effect &env &form f nil))
  ([f deps]
   (make-hook-with-deps 'uix.hooks.alpha/use-effect &env &form f deps)))

(defmacro use-layout-effect
  "Takes a function to be executed in a layout effect and optional vector of dependencies.

  See: https://reactjs.org/docs/hooks-reference.html#uselayouteffect"
  ([f]
   (make-hook-with-deps 'uix.hooks.alpha/use-layout-effect &env &form f nil))
  ([f deps]
   (make-hook-with-deps 'uix.hooks.alpha/use-layout-effect &env &form f deps)))

(defmacro use-insertion-effect
  "Takes a function to be executed synchronously before all DOM mutations
  and optional vector of dependencies. Use this to inject styles into the DOM
  before reading layout in `useLayoutEffect`.

  See: https://reactjs.org/docs/hooks-reference.html#useinsertioneffect"
  ([f]
   (make-hook-with-deps 'uix.hooks.alpha/use-insertion-effect &env &form f nil))
  ([f deps]
   (make-hook-with-deps 'uix.hooks.alpha/use-insertion-effect &env &form f deps)))

(defmacro use-memo
  "Takes function f and required vector of dependencies, and returns memoized result of f.

   See: https://reactjs.org/docs/hooks-reference.html#usememo"
  [f deps]
  (make-hook-with-deps 'uix.hooks.alpha/use-memo &env &form f deps))

(defmacro use-callback
  "Takes function f and required vector of dependencies, and returns memoized f.

  See: https://reactjs.org/docs/hooks-reference.html#usecallback"
  [f deps]
  (make-hook-with-deps 'uix.hooks.alpha/use-callback &env &form f deps))

(defmacro use-imperative-handle
  "Customizes the instance value that is exposed to parent components when using ref.

  See: https://reactjs.org/docs/hooks-reference.html#useimperativehandle"
  ([ref f]
   (when (uix.lib/cljs-env? &env)
     (uix.linter/lint-exhaustive-deps! &env &form f nil))
   `(uix.hooks.alpha/use-imperative-handle ~ref ~f))
  ([ref f deps]
   (when (uix.lib/cljs-env? &env)
     (uix.linter/lint-exhaustive-deps! &env &form f deps))
   `(uix.hooks.alpha/use-imperative-handle ~ref ~f ~(vector->js-array deps))))
