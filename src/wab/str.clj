;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; str
;; helper functions for strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns wab.str
  (:refer-clojure :exclude [repeat])
  (:require [clojure.string :as str]))

(defn num?
  "returns the string if it's a number, else nil"
  [s]
  (re-matches #"\d+" s))

(defn alpha?
  "returns the string if it's letters, else nil"
  [s]
  (re-matches #"[a-zA-Z]+" s))

(defn split
  "splits the given text by a given sep. sep defaults to a space"
  ([text] (split text #" "))
  ([text sep] (str/split text sep)))

(defn join
  "joins the given collection of strings by a seperator. Default seperator is nothing"
  ([coll]
   (join "" coll))
  ([sep coll]
   (str/join sep coll)))

(defn repeat
  "returns a string thats n repeated copies of the string"
  [n s]
  (join "" (clojure.core/repeat n s)))

(defn fmt
  "wrapper around format"
  [string & args]
  (apply format string args))
