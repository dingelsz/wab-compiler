;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;; Parses WAB source code into an AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns wab.parser
  (:require [instaparse.core :as insta]))

(def grammar-resource
  (clojure.java.io/resource "grammar.bnf"))

(defn -vectorize-content
  "Recursively converts content to vectors"
  [root]
  (cond
    (:content root)
    (assoc root :content (into [] (-vectorize-content (:content root))))

    (seq? root)
    (map -vectorize-content root)

    :else root))

(defn remove-tags
  "Changes {:tag :x :content [xs]} to {:x [xs]}"
  [root]
  (cond
    (:content root)
    (let [{:keys [tag content & rest]} root]
      (-> root
          (dissoc :tag)
          (dissoc :content)
          (assoc tag (remove-tags content))))

    (vector? root)
    (if (= 1 (count root))
      (remove-tags (first root))
      (mapv remove-tags root))

    :else root))

(defn parse
  "Parses the given WAB source code into an AST. See resources/grammar.bnf for spec."
  [text]
  (let [parser (insta/parser grammar-resource :output-format :enlive)
        tree (parser text)
        tree (insta/add-line-and-column-info-to-metadata text tree)
        tree (-vectorize-content tree)]
    tree))
