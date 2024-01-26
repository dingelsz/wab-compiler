(ns wab.ast-test
  (:require [clojure.test :refer :all]
            [clojure.zip :as z]
            [wab.ast :as ast]))

(deftest test-zip
  (is (= 1
         (-> {:tag :INTEGER :content [1]}
             ast/zip
             z/next
             z/node))))

(deftest test-map
  (let [input {:tag :LIST :content
               [{:tag :INTEGER :content [1]}
                {:tag :INTEGER :content [2]}
                {:tag :INTEGER :content [3]}]}
        expected {:tag :LIST :content
                  [{:tag :INTEGER :content [2]}
                   {:tag :INTEGER :content [3]}
                   {:tag :INTEGER :content [4]}]}
        update-fn (fn [loc]
                    (if (number? (z/node loc)) (z/edit loc inc)
                        loc))]
    (is (= expected
           (ast/map update-fn input)))))


