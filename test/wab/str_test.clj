(ns wab.str-test
  (:require [clojure.test :refer :all]
            [wab.str :as str]))

(deftest test-num?
  (is (= "123"
         (str/num? "123")))
  (is (= nil
         (str/num? "123abc"))))

(deftest test-alplha?
  (is (= "abc"
         (str/alpha? "abc")))  
  (is (= nil
         (str/alpha? "ab123cd"))))

(deftest test-split
  (is (= ["a" "b" "c"]
         (str/split "a b c")))
  (is (= ["a" "b" "c"]
         (str/split "a,b,c" #","))))

(deftest test-join
  (is (= "abc"
         (str/join ["a" "b" "c"])))
  (is (= "a b c"
         (str/join " " ["a" "b" "c"])))  )

(deftest test-repeat
  (is (= "aaa"
         (str/repeat 3 "a"))))

(deftest test-fmt
  (is (= "hello world 123"
         (str/fmt "hello %s %d" "world" 123))))
