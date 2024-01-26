(ns wab.transformations-test
  (:require [clojure.test :refer :all]
            [clojure.zip :as z]
            [wab.ast :as ast]            
            [wab.transformations :refer :all]))

(deftest test-remove-tag
  (is (= {:tag :expression,
          :content
          [{:tag :term,
            :content
            []}
           {:tag :PLUS}
           {:tag :term,
            :content
            []}]}
         (ast/map (partial remove-tag :INTEGER) {:tag :expression,
                                                 :content
                                                 [{:tag :term,
                                                   :content
                                                   [{:tag :INTEGER
                                                     :content [5]}]}
                                                  {:tag :PLUS}
                                                  {:tag :term,
                                                   :content
                                                   [{:tag :INTEGER
                                                     :content [5]}]}]}))))

(deftest test-convert-int
  (is (= {:tag :INTEGER :content [5]}
         (convert-int {:tag :INTEGER :content ["5"]}))))

(deftest test-fold-constants
  (is (= {:tag :expression,
          :content
          [{:tag :term,
            :content
            [{:tag :INTEGER
              :content [10]}]}]}
         (fold-constants {:tag :expression,
                          :content
                          [{:tag :term,
                            :content
                            [{:tag :INTEGER
                              :content [5]}]}
                           {:tag :PLUS}
                           {:tag :term,
                            :content
                            [{:tag :INTEGER
                              :content [5]}]}]}))))

(deftest test-flatten-parens
  (is (= {:tag :term,
          :content
          [{:tag :INTEGER :content [5]}]}
         (flatten-parens {:tag :term,
                            :content
                            [{:tag :LPAREN :content []}
                             {:tag :INTEGER :content [5]}
                             {:tag :RPAREN :content []}]}))))


(deftest test-flatten-term
  (is (= {:tag :expression,
          :content
          [{:tag :term,
            :content
            [{:tag :INTEGER
                  :content [10]}]}]}
         (flatten-term {:tag :expression,
                          :content
                          [{:tag :term,
                            :content
                            [{:tag :expression,
                              :content
                              [{:tag :term,
                                :content
                                [{:tag :INTEGER
                                  :content [10]}]}]}]}]}))))

(deftest test-fold-constants-recursive
  (is (= {:tag :expression,
          :content
          [{:tag :term,
            :content
            [{:tag :INTEGER
                  :content [10]}]}]}
         (fold-constants-recursive {:tag :expression,
                                    :content
                                    [{:tag :term,
                                      :content
                                      [{:tag :expression,
                                        :content
                                        [{:tag :term,
                                          :content
                                          [{:tag :LPAREN :content []}
                                           {:tag :INTEGER :content [5]}
                                           {:tag :RPAREN :content []}]}
                                         {:tag :PLUS}
                                         {:tag :term,
                                          :content
                                          [{:tag :INTEGER
                                            :content [5]}]}]}]}]}))))

(deftest test-expand-declarations
  (is (= {:tag :statements, :content
          [{:tag :statement, :content
            [{:tag :variable_definition, :content
              [{:tag :VAR, :content ["var"]}
               {:tag :NAME, :content ["x"]}
               {:tag :SEMI, :content [";"]}]}]}
           {:tag :statement, :content
            [{:tag :assignment_statement, :content
              [{:tag :NAME, :content ["x"]}
               {:tag :EQ}
               {:tag :INTEGER, :content [5]}
               {:tag :SEMI, :content [";"]}]}]}]}
         (expand-declarations {:tag :statements :content
                               [{:tag :statement :content
                                 [{:tag :variable_definition,
                                   :content
                                   [{:tag :VAR, :content ["var"]}
                                    {:tag :NAME :content ["x"]}
                                    {:tag :EQ}                                  
                                    {:tag :INTEGER :content [5]}
                                    {:tag :SEMI :content [";"]}]}]}]}))))

(deftest test-resolve-scope
  (is (= {:tag :program, :content
 [{:tag :statements, :content
   [{:tag :statement, :content
     [{:tag :variable_definition, :content
       [{:tag :VAR, :content ["var"]}
        {:tag :NAME, :content ["pi"], :scope {:local #{}, :global #{}}}
        {:tag :SEMI, :content [";"]}], :global true}]}
    {:tag :statement, :content
     [{:tag :assignment_statement, :content
       [{:tag :NAME, :content ["pi"], :scope {:local #{"pi"}, :global #{}}}
        {:tag :ASSIGN, :content ["="]}
        {:tag :expression, :content
         [{:tag :term, :content
           [{:tag :INTEGER, :content ["3"]}]}]}
        {:tag :SEMI, :content [";"]}]}]}
    {:tag :statement, :content
     [{:tag :while_statement, :content
       [{:tag :WHILE, :content ["while"]}
        {:tag :relation, :content
         [{:tag :expression, :content
           [{:tag :term, :content
             [{:tag :INTEGER, :content ["1"]}]}]}
          {:tag :LT, :content ["<"]}
          {:tag :expression, :content
           [{:tag :term, :content
             [{:tag :INTEGER, :content ["3"]}]}]}]}
        {:tag :LBRACE, :content ["{"]}
        {:tag :statements, :content
         [{:tag :statement, :content
           [{:tag :variable_definition, :content
             [{:tag :VAR, :content ["var"]}
              {:tag :NAME, :content ["x"], :scope {:local #{"pi"}, :global #{}}}
              {:tag :SEMI, :content [";"]}], :global false}]}
          {:tag :statement, :content
           [{:tag :assignment_statement, :content
             [{:tag :NAME, :content ["x"], :scope {:local #{"x" "pi"}, :global #{}}}
              {:tag :ASSIGN, :content ["="]}
              {:tag :expression, :content
               [{:tag :term, :content
                 [{:tag :NAME, :content ["pi"], :scope {:local #{"x" "pi"}, :global #{}}}]}]}
              {:tag :SEMI, :content [";"]}]}]}]}
        {:tag :RBRACE, :content ["}"]}]}]}]}], :scope {:local #{"pi"}, :global #{}}}
         (resolve-scope {:tag :program, :content
                         [{:tag :statements, :content
                           [{:tag :statement, :content
                             [{:tag :variable_definition, :content
                               [{:tag :VAR, :content ["var"]}
                                {:tag :NAME, :content ["pi"]}
                                {:tag :SEMI, :content [";"]}]}]}
                            {:tag :statement, :content
                             [{:tag :assignment_statement, :content
                               [{:tag :NAME, :content ["pi"]}
                                {:tag :ASSIGN, :content ["="]}
                                {:tag :expression, :content
                                 [{:tag :term, :content
                                   [{:tag :INTEGER, :content ["3"]}]}]}
                                {:tag :SEMI, :content [";"]}]}]}
                            {:tag :statement, :content
                             [{:tag :while_statement, :content
                               [{:tag :WHILE, :content ["while"]}
                                {:tag :relation, :content
                                 [{:tag :expression, :content
                                   [{:tag :term, :content
                                     [{:tag :INTEGER, :content ["1"]}]}]}
                                  {:tag :LT, :content ["<"]}
                                  {:tag :expression, :content
                                   [{:tag :term, :content
                                     [{:tag :INTEGER, :content ["3"]}]}]}]}
                                {:tag :LBRACE, :content ["{"]}
                                {:tag :statements, :content
                                 [{:tag :statement, :content
                                   [{:tag :variable_definition, :content
                                     [{:tag :VAR, :content ["var"]}
                                      {:tag :NAME, :content ["x"]}
                                      {:tag :SEMI, :content [";"]}]}]}
                                  {:tag :statement, :content
                                   [{:tag :assignment_statement, :content
                                     [{:tag :NAME, :content ["x"]}
                                      {:tag :ASSIGN, :content ["="]}
                                      {:tag :expression, :content
                                       [{:tag :term, :content
                                         [{:tag :NAME, :content ["pi"]}]}]}
                                      {:tag :SEMI, :content [";"]}]}]}]}
                                {:tag :RBRACE, :content ["}"]}]}]}]}]}))))


(deftest test-unscript
  (is (= {:tag :program, :content
          [{:tag :statements, :content
            [{:tag :statement, :content
              [{:tag :func_definition, :content
                [{:tag :FUNC, :content ["func"]}
                 {:tag :NAME, :content ["main"]}
                 {:tag :LPAREN, :content ["("]}
                 {:tag :NAME, :content ["__x"]}
                 {:tag :RPAREN, :content [")"]}
                 {:tag :RBRACE, :content ["{"]}
                 {:tag :statements, :content
                  [{:tag :statement, :content
                    [{:tag :print_statement, :content
                      [{:tag :PRINT, :content ["print"]}
                       {:tag :expression, :content
                        [{:tag :term, :content
                          [{:tag :INTEGER, :content ["42"]}]}]}
                       {:tag :SEMI, :content [";"]}]}]}
                   {:tag :return_statement, :content
                    [{:tag :RETURN}
                     {:tag :expression, :content
                      [{:tag :term, :content
                        [{:tag :INTEGER, :content [0]}]}]}]}]}
                 {:tag :LBRACE, :content ["}"]}]}]}]}]}
         (unscript {:tag :program, :content
           [{:tag :statements, :content
             [{:tag :statement, :content
               [{:tag :print_statement, :content
                 [{:tag :PRINT, :content ["print"]}
                  {:tag :expression, :content
                   [{:tag :term, :content
                     [{:tag :INTEGER, :content ["42"]}]}]}
                  {:tag :SEMI, :content [";"]}]}]}]}]}))))

