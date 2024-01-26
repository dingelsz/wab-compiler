(ns wab.scope-test
  (:require [clojure.test :refer :all]
            [wab.scope :as scope]))

(deftest test-make
  (is (= {:local #{} :global #{}}
         (scope/make))))

(deftest test-declare-global
  (is (= {:local #{} :global #{:a}}
         (scope/declare-global (scope/make) :a))))

(deftest test-declare-local
  (is (= {:local #{:a} :global #{}}
         (scope/declare-local (scope/make) :a))))

(deftest test-declare
  (is (= {:local #{} :global #{:a}}
         (scope/declare (scope/make) :a :global)))
  (is (= {:local #{:a} :global #{}}
         (scope/declare (scope/make) :a :local))))

(deftest test-lookup
  (let [scope (-> (scope/make)
                  (scope/declare-global :a)
                  (scope/declare-local :b))]
    (is (= :global
           (scope/lookup scope :a)))
    (is (= :local
           (scope/lookup scope :b)))
    (is (= nil
           (scope/lookup scope :c)))))

(deftest test-annotate
  (is (= {:scope {:local #{} :global #{}}}
         (scope/annotate {} (scope/make)))))

(deftest test-stack
  (let [scope-push (-> (scope/make)
                       (scope/declare-global :a)
                       (scope/declare-local :b)
                       (scope/push))
        scope-pop (scope/pop scope-push)]
    (is (= {:local #{:b} :global #{:a}}
           scope-push))
    (is (= {:stack '({:local #{:b}, :global #{:a}})}
           (meta scope-push)))
    (is (= {:local #{:b} :global #{:a}}
           scope-pop))    
    (is (= nil
           (meta scope-pop)))))
