(ns wab.writer-test
  (:require [clojure.test :refer :all]
            [wab.writer :as w]))

(deftest test-writer
  (is (= "foo                                               ; bar                         \n  foo                                             ; bar                         \nfoo                                               ; bar                         "
         (-> (w/make)
             (w/line "foo" "bar")
             (w/indent)
             (w/line "foo" "bar")
             (w/deindent)
             (w/line "foo" "bar")
             (w/read-writer)))))

(deftest test-set-variable
  (let [writer (w/make)
        writer (w/set-variable writer 123)]
    (is (= 123
           (w/get-variable writer)))))
