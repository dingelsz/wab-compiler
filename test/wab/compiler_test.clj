(ns wab.compiler-test
  (:require [clojure.test :refer :all]
            [clojure.zip :as z]
            [wab.str :as str]
            [wab.compiler :as compiler]))

(deftest test-compile-smoketest
  (let [program "print 42;"
        llvm (str/join "\n" ["declare i32 @_print_int(i32)                      ; runtime                     "
                             "define i32 @main(i32 %.__x)                       ; func main(...)              "
                             "{                                                 ;                             "
                             "  %VAR___x = alloca i32                           ;                             "
                             "  store i32 %.__x, i32* %VAR___x                  ;                             "
                             "  call i32 (i32) @_print_int(i32 42)              ; print 42                    "
                             "  ret i32 0                                       ; return                      "
                             "}                                                 ;                             "])]
    (is (= llvm
           (compiler/compile "print 42;")))))

