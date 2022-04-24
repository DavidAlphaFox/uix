(ns uix.core
  "Public API"
  (:require-macros [uix.core])
  (:require [goog.object :as gobj]
            [react :as r]
            [uix.compiler.debug :as debug]
            [uix.hooks.alpha :as hooks]
            [uix.compiler.alpha :as compiler]
            [uix.compiler.aot]
            [uix.lib :refer [doseq-loop]]))

(def ^:dynamic *current-component*)

;; React's top-level API

(defn create-class
  "Creates class based React component"
  [{:keys [constructor static prototype]}]
  (let [ctor (fn [props]
               (this-as this
                 (.apply r/Component this (js-arguments))
                 (when constructor
                   (constructor this props)))
               nil)]
    (set! (.-prototype ctor) (.create js/Object (.-prototype r/Component)))
    (doseq-loop [[k v] static]
                (aset ctor (name k) v))
    (doseq-loop [[k v] prototype]
                (aset (.-prototype ctor) (name k) v))
    (set! (.-uix-component? ctor) true)
    ctor))

(defn create-error-boundary
  "Creates React's Error Boundary component

    display-name — the name of the component to be displayed in stack trace
    error->state — maps error object to component's state that is used in render-fn
    handle-catch — for side-effects, logging etc.
    render-fn — takes state value returned from error->state and a vector of arguments passed into error boundary"
  [{:keys [display-name error->state handle-catch]
    :or {display-name (str (gensym "error-boundary"))}}
   render-fn]
  (let [constructor (fn [^js/React.Component this _]
                      (set! (.-state this) #js {:argv nil})
                      (specify! (.-state this)
                        IDeref
                        (-deref [o]
                          (.. this -state -argv))
                        IReset
                        (-reset! [o new-value]
                          (.setState this #js {:argv new-value})
                          new-value)
                        ISwap
                        (-swap!
                          ([o f]
                           (-reset! o (f (-deref o))))
                          ([o f a]
                           (-reset! o (f (-deref o) a)))
                          ([o f a b]
                           (-reset! o (f (-deref o) a b)))
                          ([o f a b xs]
                           (-reset! o (apply f (-deref o) a b xs))))))
        derive-state (fn [error] #js {:argv (error->state error)})
        render (fn []
                 (this-as ^react/Component this
                   (let [args (.. this -props -argv)
                         state (.-state this)]
                     ;; `render-fn` should return compiled HyperScript
                     (render-fn state args))))]
    (create-class {:constructor constructor
                   :static {:displayName display-name
                            :getDerivedStateFromError derive-state}
                   :prototype {:componentDidCatch handle-catch
                               :render render}})))

(defn create-ref
  "Creates React's ref type object."
  []
  (r/createRef))

(defn glue-args [^js props]
  (cond-> (.-argv props)
          (.-children props) (assoc :children (.-children props))))

(defn- memo-compare-args [a b]
  (= (glue-args a) (glue-args b)))

(defn memo
  "Takes component `f` and optional comparator function `should-update?`
  that takes previous and next props of the component.
  Returns memoized `f`.

  When `should-update?` is not provided uses default comparator
  that compares props with clojure.core/="
  ([f]
   (memo f memo-compare-args))
  ([^js f should-update?]
   (let [fm (react/memo f should-update?)]
     (when (.-uix-component? f)
       (set! (.-uix-component? fm) true))
     fm)))

(defn use-state
  "Takes initial value or a function that computes it and returns a stateful value,
  and a function to update it.

  See: https://reactjs.org/docs/hooks-reference.html#usestate"
  [value]
  (hooks/use-state value))

(defn use-reducer
  "An alternative to `use-state`. Accepts a reducer of type (state, action) => new-state,
  and returns the current state paired with a dispatch method.

  See: https://reactjs.org/docs/hooks-reference.html#usereducer"
  ([f value]
   (hooks/use-reducer f value))
  ([f value init-state]
   (hooks/use-reducer f value init-state)))

(defn use-ref
  "Takes optional initial value and returns React's ref hook wrapped in atom-like type."
  ([]
   (use-ref nil))
  ([value]
   (let [ref (hooks/use-ref nil)]
     (when (nil? (.-current ref))
       (set! (.-current ref)
             (specify! #js {:current value}
               IDeref
               (-deref [this]
                 (.-current this))

               IReset
               (-reset! [this v]
                 (gobj/set this "current" v)))))
     (.-current ref))))

(defn create-context
  "Creates React Context with an optional default value"
  ([]
   (react/createContext))
  ([default-value]
   (react/createContext default-value)))

(defn use-context
  "Takes React context and returns its current value"
  [context]
  (hooks/use-context context))

(defn use-deferred-value
  "Accepts a value and returns a new copy of the value that will defer to more urgent updates.
  If the current render is the result of an urgent update, like user input,
  React will return the previous value and then render the new value after the urgent render has completed.

  See: https://reactjs.org/docs/hooks-reference.html#usedeferredvalue"
  [v]
  (hooks/use-deferred-value v))

(defn use-transition
  "Returns a stateful value for the pending state of the transition, and a function to start it.

  See: https://reactjs.org/docs/hooks-reference.html#usetransition"
  []
  (hooks/use-transition))

(defn use-id
  "Returns unique ID that is stable across the server and client, while avoiding hydration mismatches.

  See: https://reactjs.org/docs/hooks-reference.html#useid"
  []
  (hooks/use-id))

(defn use-sync-external-store
  "For reading and subscribing from external data sources in a way that’s compatible
  with concurrent rendering features like selective hydration and time slicing.

  subscribe: function to register a callback that is called whenever the store changes
  get-snapshot: function that returns the current value of the store
  get-server-snapshot: function that returns the snapshot used during server rendering

  See: https://reactjs.org/docs/hooks-reference.html#usesyncexternalstore"
  ([subscribe get-snapshot]
   (hooks/use-sync-external-store subscribe get-snapshot))
  ([subscribe get-snapshot get-server-snapshot]
   (hooks/use-sync-external-store subscribe get-snapshot get-server-snapshot)))

(def with-name debug/with-name)

(defn as-react
  "Interop with React components. Takes UIx component function and returns same component wrapped into interop layer."
  [f]
  (compiler/as-react f))
