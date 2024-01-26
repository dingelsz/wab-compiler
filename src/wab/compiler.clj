;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler pipeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns wab.compiler
  (:refer-clojure :exclude [compile])
  (:require [wab.ast :as ast]
            [wab.llvm :as llvm]
            [wab.transformations :as transformations]
            [wab.parser :as parser]))

(defn compile
  "Returns a string of LLVM IR for the given WAB program"
  [program]
  (llvm/reset!)
  (-> program
      parser/parse
      transformations/remove-whitespace
      transformations/remove-comments
      transformations/convert-int
      transformations/fold-constants-recursive
      transformations/expand-declarations
      transformations/resolve-scope
      transformations/unscript
      llvm/codegen))

(defn compile-to
  "Compiles the given program to LLVM IR and writes it to the given filepath"
  [& {:keys [program path]}]
  (spit path (compile program)))
