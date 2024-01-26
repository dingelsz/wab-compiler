;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LLVM
;; generates LLVM IR from an AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns wab.llvm
  (:refer-clojure :exclude [reset!])
  (:require [clojure.zip :as z]
            [clojure.core.match :refer [match]]
            [wab.str :as str]
            [wab.writer :as w]))

(def unique (atom 0))

(defn new-label
  "Returns a unique label name"
  []
  (swap! unique inc)
  (format "LB%s" @unique))

(defn new-variable-local
  "Returns a unque variable name"
  []
  (swap! unique inc)
  (format "%%VAR_%s" @unique))

(defn new-variable-global
  "Returns a unique global variable name"
  []
  (swap! unique inc)
  (format "@%s" @unique))

(defn format-var
  "Formats the given variable to be global or local"
  [var scope]
  (if (= scope :local)
    (format "%%VAR_%s" var)
    (format "@%s" var)))

(defn reset!
  "resets the unique number generator"
  []
  (clojure.core/reset! unique 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatch on :tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti generate
  "Returns a writer containing lines of LLVM IR for the given AST node"
  (fn [writer root]
    (:tag root)))

(defmethod generate :default
  [writer root]
  writer)

(defn main [root]
  (generate (w/make) root nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod generate
  :program
  [writer root]
  (let [writer (w/line writer "declare i32 @_print_int(i32)" "runtime")]
    (generate writer (get-in root [:content 0]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod generate
  :statements
  [writer root]
  (reduce generate writer (:content root)))

(defmethod generate
  :statement
  [writer root]
  (reduce generate writer (:content root)))

(defmethod generate
  :print_statement
  [writer root]
  (let [expr   (get-in root [:content 1])
        writer (generate writer expr)
        inst   (format "call i32 (i32) @_print_int(i32 %s)" (w/get-variable writer))
        comm   (format "print %s" (w/get-variable writer))]
    (w/line writer inst comm)))

(defmethod generate
  :variable_definition
  [writer root]
  (if (:global root)
    (let [name (get-in root [:content 1 :content 0])
          inst (format "@%s = global i32 0" name)
          comm (format "global=%s" name)]
      (w/line writer inst comm))
    (let [name (get-in root [:content 1 :content 0])
          inst (format "%%VAR_%s = alloca i32" name)
          comm (format "local=%s" name)]
      (w/line writer inst comm))))

(defmethod generate
  :assignment_statement
  [writer root]
  (let [name   (get-in root [:content 0 :content 0])
        scope  (get-in root [:content 0 :scope])
        expr   (get-in root [:content 2])
        writer (generate writer expr)]
    (if (contains? (:global scope) name)
      (let [inst   (format "store i32 %s, i32* @%s" (w/get-variable writer) name)
            comm   (format "%s = %s" name (w/get-variable writer))]
        (w/line writer inst comm))
      (let [inst   (format "store i32 %s, i32* %%VAR_%s" (w/get-variable writer) name)
            comm   (format "%s = %s" name (w/get-variable writer))]
        (w/line writer inst comm)))))

(defmethod generate
  :if_statement
  [writer root]
  (let [test     (get-in root [:content 1])
        writer       (generate writer test)
        var-relation (w/get-variable writer)
        
        body-true    (get-in root [:content 3])
        body-false   (get-in root [:content 7])
        label-true   (str (new-label) "_TRUE")
        label-false  (str (new-label) "_FALSE")
        label-return (str (new-label) "_RETURN")]
    (-> writer
        (w/line (format "br i1 %s, label %%%s, label %%%s" var-relation label-true label-false) "if")
        (w/line "")

        (w/deindent)
        (w/line (str label-true ":"))
        (w/indent)
        (generate body-true)
        (w/line (format "br label %%%s", label-return))
        (w/deindent)
        (w/line "")
        
        (w/line (str label-false ":"))
        (w/indent)        
        (generate body-false)
        (w/line (format "br label %%%s" label-return))
        (w/deindent)
        (w/line "")
        
        (w/line (str label-return ":"))
        (w/indent))))

(defmethod generate
  :while_statement
  [writer root]
    (let [test         (get-in root [:content 1])
          body         (get-in root [:content 3])
          label-test   (str (new-label) "_TEST")
          label-true   (str (new-label) "_TRUE")
          label-return (str (new-label) "_RETURN")]
      (-> writer
          (w/line (format "br label %%%s" label-test) "test")
          (w/line "")
          
          (w/line (str label-test ":"))
          (w/indent)
          (generate test)
          ((fn [writer]
             (w/line writer (format "br i1 %s, label %%%s, label %%%s" (w/get-variable writer) label-true label-return))))
          (w/deindent)
          (w/line "")
          
          (w/line (str label-true ":"))
          (w/indent)        
          (generate body)
          (w/line (format "br label %%%s", label-test))
          (w/deindent)
          (w/line "")
          
          (w/line (str label-return ":"))
          (w/indent))))

(defmethod generate
  :func_definition
  [writer root]
  (let [name         (get-in root [:content 1 :content 0])
        arg          (get-in root [:content 3 :content 0])
        body         (get-in root [:content 6])]
    (-> writer
        (w/line (format "define i32 @%s(i32 %%.%s)" name arg) (format "func %s(...)" name))
        (w/line "{")
        (w/indent)
        (w/line (format "%%VAR_%s = alloca i32" arg))
        (w/line (format "store i32 %%.%s, i32* %%VAR_%s" arg arg))
        (generate body)
        (w/deindent)
        (w/line "}"))))

(defmethod generate
  :return_statement
  [writer root]
  (let [expr   (get-in root [:content 1])
        writer (generate writer expr)
        inst   (format "ret i32 %s" (w/get-variable writer))
        comm   (format "return" (w/get-variable writer))]
    (w/line writer inst comm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions, Relations and Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod generate
  :expression
  [writer root]
  (match [(:content root)]
         [[{:tag :term :content rhs} {:tag :PLUS} {:tag :term :content lhs}]]
         (let [writer (reduce generate writer lhs)
               lhs-var (w/get-variable writer)
               writer (reduce generate writer rhs)
               rhs-var (w/get-variable writer)
               expr-var (new-variable-local)
               writer (w/set-variable writer expr-var)
               inst   (format "%s = add i32 %s, %s" expr-var lhs-var rhs-var)
               comm   (format "+")]
           (w/line writer inst comm))

         [[{:tag :term :content rhs} {:tag :TIMES} {:tag :term :content lhs}]]
         (let [writer (reduce generate writer lhs)
               lhs-var (w/get-variable writer)
               writer (reduce generate writer rhs)
               rhs-var (w/get-variable writer)
               expr-var (new-variable-local)
               writer (w/set-variable writer expr-var)               
               inst   (format "%s = mul i32 %s, %s" expr-var lhs-var rhs-var)
               comm   (format "*")]
           (w/line writer inst comm))
         
         [[{:tag :term :content term}]]
         (reduce generate writer term)

         :else
         (throw (Exception. (format "Invalid expression: %s" root)))))

(defmethod generate
  :relation
  [writer root]
  (match [(:content root)]
         [[{:tag :expression :content lhs} {:tag :LT} {:tag :expression :content rhs}]]
         (let [writer (reduce generate writer lhs)
               lhs-var (w/get-variable writer)
               writer (reduce generate writer rhs)
               rhs-var (w/get-variable writer)
               res-var (new-variable-local)
               inst   (format "%s = icmp slt i32 %s, %s" res-var lhs-var rhs-var)
               comm   (format "<")]
           (-> writer
               (w/line inst comm)
               (w/set-variable res-var)))

         [[{:tag :expression :content lhs} {:tag :EQ} {:tag :expression :content rhs}]]
         (let [writer (reduce generate writer lhs)
               lhs-var (w/get-variable writer)
               writer (reduce generate writer rhs)
               rhs-var (w/get-variable writer)
               res-var (new-variable-local)               
               inst   (format "%s = icmp eq i32 %s, %s" res-var lhs-var rhs-var)
               comm   (format "==")]
           (-> writer
               (w/line inst comm)
               (w/set-variable res-var)))
         
         :else
         (throw (Exception. (format "Invalid expression: %s" root)))))

(defmethod generate
  :term
  [writer root]
  (reduce generate writer (:content root)))

(defmethod generate
  :call
  [writer root]
  (let [func-name (get-in root [:content 0 :content 0])
        expr (get-in root [:content 2])
        writer (generate writer expr)
        expr-var (w/get-variable writer)
        var (new-variable-local)
        writer (w/set-variable writer var)
        inst   (format "%s = call i32 (i32) @%s(i32 %s)" var func-name expr-var)
        comm   (format "%s(...)" func-name)]
    (w/line writer inst comm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod generate
  :NAME
  [writer root]
  (let [name (get-in root [:content 0])
        scope (if (contains? (:global (:scope root)) name) :global :local)
        var-name (format-var name scope)
        var-local (new-variable-local)
        writer (w/set-variable writer var-local)]
    (if (= scope :global)
      (w/line writer (format "%s = load i32, i32* %s" var-local var-name) (format "%s = %s" var-local name))
      (w/line writer (format "%s = load i32, i32* %s" var-local var-name) (format "%s = %s" var-local name)))))

(defmethod generate
  :INTEGER
  [writer root]
  (w/set-variable writer (get-in root [:content 0])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Codegen
;; Driver for llvm IR generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn codegen
  "returns LLVM IR generated from the given AST. Assumes the AST has been transformed."
  [ast]
  (w/read-writer (generate (w/make) ast)))

