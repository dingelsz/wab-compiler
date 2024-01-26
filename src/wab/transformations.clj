;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transformations
;; Various functions for transforming AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns wab.transformations
  (:require [clojure.zip :as z]
            [clojure.core.match :refer [match]]
            [wab.ast :as ast]
            [wab.scope :as scope]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn remove-tag
  "Removes all nodes with the given tag in an AST"
  [tag loc]
  (match [(z/node loc)]
         [{:tag tag}] (z/remove loc)
         :else loc))

(defn apply-while-changing
  "Like iterate but stops once the function no longer changes the input."
  [input func]
  (loop [input input]
    (let [output (func input)]
      (if (= input output) input
          (recur output)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST Transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def redundant-tags [:WS :COMMENT :NEWLINE :SEMI])

(defn remove-tags [root tags]
  (loop [root root
         tag (first tags)
         tags (rest tags)]
    (if (nil? tag) root
        (recur
         (ast/map (fn [loc] (remove-tag tag loc)) root)
         (first tags) (rest tags)))))

(ast/def-mapping remove-whitespace
  "Removes all whitespaces from the AST"
  [loc]
  (remove-tag :WS loc))

(ast/def-mapping remove-comments
  "Removes all comments from the AST"  
  [loc]
  (remove-tag :COMMENT loc))

(ast/def-mapping remove-unwanted-tags
  "Removes unwanted tags from the AST"
  [loc]
  (remove-tag :COMMENT))

(ast/def-mapping convert-int
  "Converts all integers to Integer type"
  [loc]
  (let [node (z/node loc)]
    (match [node]
           [{:tag :INTEGER :content [x]}]
           (z/edit loc update-in [:content 0]
                   (fn [x] (Integer/parseInt x)))
           :else loc)))

(ast/def-mapping fold-constants
  "Simplifies constant arithematic."
  [loc]
  (let [node (z/node loc)]
    (match [node]
           [{:tag :expression :content
             [{:tag :term :content [{:tag :INTEGER :content [x]}]}
              {:tag :PLUS }
              {:tag :term :content [{:tag :INTEGER :content [y]}]}]}]
           (z/replace loc
                      {:tag :expression :content [{:tag :term :content [{:tag :INTEGER :content [(+ x y)]}]}]})
           
           [{:tag :expression :content
             [{:tag :term :content [{:tag :INTEGER :content [x]}]}
              {:tag :TIMES }
              {:tag :term :content [{:tag :INTEGER :content [y]}]}]}]
           (z/replace loc
                      {:tag :expression :content [{:tag :term :content [{:tag :INTEGER :content [(* x y)]}]}]})
           
           :else loc)))

(ast/def-mapping flatten-parens
  "Removes parenthesis around a single expression"
  [loc]
  (let [node (z/node loc)]
    (match [node]
           [{:tag :term :content [{:tag :LPAREN} expr {:tag :RPAREN}]}]
           (z/replace loc {:tag :term :content [ expr ]})
           :else loc)))

(ast/def-mapping flatten-term
  "Removes redudant term/expression nestings (ie (term(expression(term...))) -> (term...)"
  [loc]
  (let [node (z/node loc)]
    (match [node]
           [{:tag :term :content [{:tag :expression :content [{:tag :term :content inner-content}]}]}]
           (z/replace loc {:tag :term :content inner-content})
           :else loc)))

(defn fold-constants-recursive
  "Recursively fold constants in an AST until no change is produced."
  [root]
  (apply-while-changing
   root
   (comp fold-constants flatten-parens flatten-term)))

(ast/def-mapping expand-declarations
  "Expands a declaration into a declaration and and assignment"
  [loc]
  (let [node (z/node loc)]
    (match [node]
           [{:tag :statement :content [{:tag :variable_definition :content [var name assign expr semi]}]}]
           (-> loc
               (z/replace {:tag :statement :content [{:tag :variable_definition :content [var name semi]}]})
               (z/insert-right {:tag :statement :content [{:tag :assignment_statement :content [name assign expr semi]}]}))
           :else loc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resolve scope
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn global-definition?
  "true if the definition should be global"
  [loc]
  (= 2 (count (z/path loc))))

(defn resolve-scope
  "Adds scope annotations to variable definitions and references."
  ([root] (resolve-scope (ast/zip root) (scope/make)))
  ([loc scope]
   (if (z/end? loc) (scope/annotate (z/node loc) scope)
       (if (map? (z/node loc))
         (match [(z/node loc)]
                ;; Var Definitions
                [{:tag :statement :content [{:tag :variable_definition} & _]}]
                (let [name (-> loc z/down z/down z/right z/down z/node)
                      global? (global-definition? loc)
                      ;; Skip the definition line before adding it's name to scope but annotate any names in the line
                      next-loc (-> loc    ; statement
                                   z/down ; variable_definition
                                   (z/edit assoc :global global?) ;
                                   z/down  ; var
                                   z/right ; name
                                   (z/edit scope/annotate scope) ;
                                   z/right)] ; semi
                  (resolve-scope next-loc (scope/declare scope name global?)))

                [{:tag :func_definition}]
                (let [name (-> loc z/down z/right z/down z/node)
                      next-loc (->
                                loc
                                z/down
                                z/right
                                (z/edit scope/annotate scope)
                                z/right)
                      scope (scope/declare-global scope name)]
                  (resolve-scope next-loc scope))

                ;; Changing scope (IN)
                [{:tag :LBRACE}]
                (resolve-scope (z/next loc) (scope/push scope))

                ;; Changing scope (OUT)
                [{:tag :RBRACE}]
                (resolve-scope (z/next loc) (scope/pop scope))

                ;; Add scope to names
                [{:tag :NAME}]
                (resolve-scope (z/next (z/edit loc scope/annotate scope)) scope)

                :else (resolve-scope (z/next loc) scope))
         (resolve-scope (z/next loc) scope)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unscript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn generate-main
  "returns a ast representing a main function calling the given list of statements"
  [statements]
  [{:tag :statement :content
    [{:tag :func_definition :content
      [{:tag :FUNC :content ["func"]}
       {:tag :NAME :content ["main" ]}
       {:tag :LPAREN :content ["("]}
       {:tag :NAME :content ["__x"]}
       {:tag :RPAREN :content [")"]}
       {:tag :RBRACE :content ["{"]}
       {:tag :statements :content statements}
       {:tag :LBRACE :content ["}"]}]}]}])

(defn generate-return
  "returns an ast representing a default return statement"
  []
  {:tag :return_statement :content
   [{:tag :RETURN}
    {:tag :expression :content
     [{:tag :term :content
       [{:tag :INTEGER :content [0]}]}]}]})

(defn unscript-statements
  "Transforms statements nested under a program by placing all statements that aren't functions or variable defintions inside a top level main function"
  [loc]
  (let [children (z/children loc)
        funcs (filter (fn [stmt] (= :func_definition (get-in stmt [:content 0 :tag]))) children)
        global-defs (filter (fn [stmt]
                              (and (= :variable_definition (get-in stmt [:content 0 :tag]))
                                   (get-in stmt [:content 0 :global])))
                            children)
        globals (into () (concat global-defs funcs))
        other (filter (fn [stmt] (not (some #{stmt} globals))) children)
        other (conj (into [] other) (generate-return))
        main (generate-main other)]
    (z/replace loc
               {:tag :statements :content (into (into [] globals) main)})))

(defn unscript
  "Transforms a program by placing all statements that aren't functions or variable defintions inside a top level main function"  
  [root]
  (-> root
      ast/zip
      z/down ;; program -> statements
      unscript-statements
      z/root))


