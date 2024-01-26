;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writer
;; An abstraction for writing lines of code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns wab.writer
  (:require [clojure.zip :as z]
            [clojure.core.match :refer [match]]
            [wab.str :as str]))

(defn make
  "creates a new writer"
  []
  {:variable nil :lines [] :mode nil})

(defn indent
  "causes the writer to indent future lines"
  [writer]
  (assoc writer :mode :INDENT))

(defn deindent
  "causes the writer to not indent future lines"
  [writer]
  (assoc writer :mode nil))

(defn write-line
  "writes the given line to the writer"
  [writer line]
  (update writer :lines conj line))

(defn format-line
  "returns a formated line form the given instruction, comment and indentation mode"
  ([instruction comment indent]
   (if indent
     (format "  %-48.48s; %-28.28s" instruction comment)
     (format "%-50.50s; %-28.28s" instruction comment)))
  ([instruction comment]
   (format-line instruction comment nil)))

(defn line
  "Writes the given instruction to the writer. Adds a comment if it's given."
  ([writer instruction] (line writer instruction ""))
  ([writer instruction comment]
   (if (= :INDENT (:mode writer))
     (write-line writer (format-line instruction comment :indent))
     (write-line writer (format-line instruction comment)))))

(defn read-writer
  "returns all lines of a writer joined into a single string"
  [writer]
  (str/join "\n" (:lines writer)))

(defn set-variable
  "sets the internal varaible of the writer to the given value"
  [writer value]
  (assoc writer :variable value))

(defn get-variable
  "returns the internal variable of the writer"
  [writer]
  (:variable writer))

