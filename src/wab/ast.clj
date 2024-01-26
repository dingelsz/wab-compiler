;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives for manipulating an AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns wab.ast
  (:refer-clojure :exclude [map])
  (:require [clojure.zip :as z]
            [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]]
            [wab.parser :as parser]))

(defn zip
  "Creates a zipper for an AST"
  [root]
  (z/zipper map? 
            :content
            (fn [node children] (assoc node
                                      :content (into [] children)))
            root))

(defn map
  "Does a post order traversal of an AST and applies action to each node"
  [action root]
  (loop [loc (zip root)]
    (if (z/end? loc) (z/node loc)
        (recur (z/next (action loc))))))

(defmacro def-mapping
  "Convenience macro for creating AST mappings."
  [name docstring? [arg] & body]
  `(defn ~name [root#]
     (map (fn [~arg] ~@body) root#)))
