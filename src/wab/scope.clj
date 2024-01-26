;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scope
;; Abstraction for managing scope. Scope is a collection of global and local
;; variables. Scope works similar to a stack, when a new scope is entered the
;; old scope can be stored on a stack. When the scope is exiteded the old scope
;; can be poped back. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns wab.scope
  (:refer-clojure :exclude [declare pop]))

(defn make
  "creates a new scope"
  []
  {:local #{} :global #{}})

(defn declare-global
  "adds a global variable to the given scope"
  [scope item]
  (update-in scope [:global] conj item))

(defn declare-local
  "adds a local variable to the given scope"
  [scope item]
  (update-in scope [:local] conj item))

(defn declare
  "adds a new variable to the given scope as either global or local"
  [scope item global]
  (if (= global :global) (declare-global scope item)
      (declare-local scope item)))

(defn lookup
  "returns :local if the item is in local scope, :global if it's in global scope or nil"
  [scope item]
  (cond
    (contains? (:local scope) item) :local
    (contains? (:global scope) item) :global
    :else nil))

(defn annotate
  "associates the scope in the given map"
  [map scope]
  (assoc map :scope scope))

(defn push
  "pushes the current scope onto a stack to be popped later."
  [scope]
  (vary-meta scope update :stack conj scope))

(defn pop
  "Pops the current scope and returns the most recent pushed scope."
  [scope]
  (-> scope
      meta
      :stack
      peek))
