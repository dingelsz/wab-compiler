(ns wab.parser-test
  (:require [clojure.test :refer :all]
            [wab.parser :as parser]))

(deftest test-parser-valid-text
  (are [source] (parser/parse source)
    ;; Normal
    "print x;"

    ;; Spaces
    "print x ;"
    ;; Tabs
    "print x	;"
    ;; Newline
    "print
x ;"
    ;; Double comma
    "print x ;;"
    ;; Double comma
    "print x ;   ;"

    ;; Comments
    " print x; //   foo bar"
    ;; Full function
    "func fact(n) {
    var result = 1;
    var x = 1;
    while x < n {
        result = result * x;
	x = x + 1;
        }
    return result;
    }

    var n = 0;
    while n < 10 {
        print fact(n);
        n = n + 1;
    }"))
