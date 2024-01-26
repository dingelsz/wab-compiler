(ns wab.llvm-test
  (:require [clojure.test :refer :all]
            [wab.llvm :as llvm]
            [wab.writer :as w]))

(defn reset-and-generate [root]
  (llvm/reset!)
  (llvm/generate (w/make) root))

(deftest test-llvm-generate
  (are [root expected] (= expected
                          (reset-and-generate root))

    ;; :INTEGER
    {:tag :INTEGER :content [1]}
    {:variable 1 :lines [] :mode nil}

    ;; :NAME (global)
    {:tag :NAME :content ["x"] :scope {:global #{"x"}}}
    {:variable "%VAR_1"
     :lines ["%VAR_1 = load i32, i32* @x                        ; %VAR_1 = x                  "]
     :mode nil}

    ;; :NAME (local)
    {:tag :NAME :content ["x"] :scope {:local #{"x"}}}
    {:variable "%VAR_1"
     :lines ["%VAR_1 = load i32, i32* %VAR_x                    ; %VAR_1 = x                  "]
     :mode nil}

    ;; :call
    {:tag :call
     :content [{:tag :NAME :content ["fact"] :scope {:local #{} :global #{"fact"}}}
               {:tag :LPAREN :content ["("]}
               {:tag :expression :content
                [{:tag :term :content
                  [{:tag :INTEGER :content [5]}]}]}
               {:tag :RPAREN :content [")"]}]}
    {:variable "%VAR_1"
     :lines ["%VAR_1 = call i32 (i32) @fact(i32 5)              ; fact(...)                   "]
     :mode nil}

    ;; :term -> :INTEGER
    {:tag :term :content [{:tag :INTEGER :content [1]}]}
    {:variable 1 :lines [] :mode nil}

    ;; :term -> :call
    {:tag :term :content [{:tag :call
                           :content [{:tag :NAME :content ["fact"] :scope {:local #{} :global #{"fact"}}}
                                     {:tag :LPAREN :content ["("]}
                                     {:tag :expression :content
                                      [{:tag :term :content
                                        [{:tag :INTEGER :content [5]}]}]}
                                     {:tag :RPAREN :content [")"]}]}]}
    {:variable "%VAR_1"
     :lines ["%VAR_1 = call i32 (i32) @fact(i32 5)              ; fact(...)                   "]
     :mode nil}

    ;; :term -> :name
    {:tag :term :content [{:tag :NAME :content ["x"] :scope {:local #{"x"}}}]}
    {:variable "%VAR_1"
     :lines ["%VAR_1 = load i32, i32* %VAR_x                    ; %VAR_1 = x                  "]
     :mode nil}

    ;; :term -> :parens
    {:tag :term :content [{:tag :LPAREN :content []}
                          {:tag :expression :content
                           [{:tag :term :content
                             [{:tag :INTEGER :content [5]}]}]}
                          {:tag :RPAREN :content []}]
     }
    {:variable 5 :lines [] :mode nil}

    ;; relation EQ
    {:tag :relation,
     :content
     [{:tag :expression,
       :content
       [{:tag :term,
         :content
         [{:tag :INTEGER
           :content [5],}]}]}
      {:tag :EQ, :content ["=="]}
      {:tag :expression,
       :content
       [{:tag :term,
         :content
         [{:tag :INTEGER
           :content [1]}]}]}]}
    {:variable "%VAR_1"
     :lines ["%VAR_1 = icmp eq i32 5, 1                         ; ==                          "]
     :mode nil}

    ;; relation LT
    {:tag :relation,
     :content
     [{:tag :expression,
       :content
       [{:tag :term,
         :content
         [{:tag :NAME,
           :content ["x"],
           :scope
           {:local #{"x" "result"},
            :global #{"fact" "pi"}}}]}]}
      {:tag :LT, :content ["<"]}
      {:tag :expression,
       :content
       [{:tag :term,
         :content
         [{:tag :NAME,
           :content ["n"],
           :scope
           {:local #{"x" "result"},
            :global #{"fact" "pi"}}}]}]}]}
    {:variable "%VAR_3"
     :lines ["%VAR_1 = load i32, i32* %VAR_x                    ; %VAR_1 = x                  "
	     "%VAR_2 = load i32, i32* %VAR_n                    ; %VAR_2 = n                  "
	     "%VAR_3 = icmp slt i32 %VAR_1, %VAR_2              ; <                           "]
     :mode nil}

    ;; Expression -> Add
    {:tag :expression,
     :content
     [{:tag :term,
       :content
       [{:tag :INTEGER
         :content [5],}]}
      {:tag :PLUS}
      {:tag :term,
       :content
       [{:tag :INTEGER
         :content [5],}]}]}
    {:variable "%VAR_1"
     :lines ["%VAR_1 = add i32 5, 5                             ; +                           "]
     :mode nil}

    ;; Expression -> Mul
    {:tag :expression,
     :content
     [{:tag :term,
       :content
       [{:tag :INTEGER
         :content [5],}]}
      {:tag :TIMES}
      {:tag :term,
       :content
       [{:tag :INTEGER
         :content [5],}]}]}
    {:variable "%VAR_1"
     :lines ["%VAR_1 = mul i32 5, 5                             ; *                           "]
     :mode nil}

    ;; Return
    {:tag :return_statement
     :content
     [{:tag :RETURN}
      {:tag :expression,
       :content
       [{:tag :term,
         :content
         [{:tag :NAME
           :content ["foo"]}]}
        {:tag :TIMES}
        {:tag :term,
         :content
         [{:tag :INTEGER
           :content [5]}]}]}]}
    {:variable "%VAR_2"
     :lines ["%VAR_1 = load i32, i32* %VAR_foo                  ; %VAR_1 = foo                "
	     "%VAR_2 = mul i32 5, %VAR_1                        ; *                           "
	     "ret i32 %VAR_2                                    ; return                      "]
     :mode nil}

    ;; Func Defintion
    {:tag :func_definition,
     :content
     [{:tag :FUNC, :content ["func"]}
      {:tag :NAME,
       :content ["fact"],
       :scope {:local #{}, :global #{}}}
      {:tag :LPAREN, :content ["("]}
      {:tag :NAME,
       :content ["n"],
       :scope {:local #{}, :global #{"fact"}}}
      {:tag :RPAREN, :content [")"]}
      {:tag :LBRACE, :content ["{"]}
      {:tag :statements,
       :content
       [{:tag :statement,
         :content
         [{:tag :return_statement,
           :content
           [{:tag :RETURN, :content ["return"]}
            {:tag :expression,
             :content
             [{:tag :term,
               :content
               [{:tag :NAME,
                 :content ["n"],
                 :scope {:local #{}, :global #{"fact"}}}]}]}
            {:tag :SEMI, :content [";"]}]}]}]}
      {:tag :RBRACE, :content ["}"]}]}
    {:variable "%VAR_1",	  
     :lines
     ["define i32 @fact(i32 %.n)                         ; func fact(...)              "
      "{                                                 ;                             "
      "  %VAR_n = alloca i32                             ;                             "
      "  store i32 %.n, i32* %VAR_n                      ;                             "
      "  %VAR_1 = load i32, i32* %VAR_n                  ; %VAR_1 = n                  "
      "  ret i32 %VAR_1                                  ; return                      "
      "}                                                 ;                             "]
     :mode nil}
     

                                        ; While
    {:tag :while_statement,
     :content
     [{:tag :WHILE, :content ["while"]}
      {:tag :relation,
       :content
       [{:tag :expression,
         :content
         [{:tag :term,
           :content
           [{:tag :NAME,
             :content ["x"],
             :scope {:local #{}, :global #{}}}]}]}
        {:tag :LT, :content ["<"]}
        {:tag :expression,
         :content
         [{:tag :term,
           :content [{:tag :INTEGER, :content [1]}]}]}]}
      {:tag :LBRACE, :content ["{"]}
      {:tag :statements,
       :content
       [{:tag :statement,
         :content
         [{:tag :print_statement,
           :content
           [{:tag :PRINT, :content ["print"]}
            {:tag :expression,
             :content
             [{:tag :term,
               :content
               [{:tag :NAME,
                 :content ["x"],
                 :scope {:local #{}, :global #{}}}]}]}
            {:tag :SEMI, :content [";"]}]}]}]}
      {:tag :RBRACE, :content ["}"]}]}
    {:variable "%VAR_6",	  
     :lines
     ["br label %LB1_TEST                                ; test                        "
      "                                                  ;                             "
      "LB1_TEST:                                         ;                             "
      "  %VAR_4 = load i32, i32* %VAR_x                  ; %VAR_4 = x                  "
      "  %VAR_5 = icmp slt i32 %VAR_4, 1                 ; <                           "
      "  br i1 %VAR_5, label %LB2_TRUE, label %LB3_RETURN;                             "
      "                                                  ;                             "
      "LB2_TRUE:                                         ;                             "
      "  %VAR_6 = load i32, i32* %VAR_x                  ; %VAR_6 = x                  "
      "  call i32 (i32) @_print_int(i32 %VAR_6)          ; print %VAR_6                "
      "  br label %LB1_TEST                              ;                             "
      "                                                  ;                             "
      "LB3_RETURN:                                       ;                             "],
     :mode :INDENT}
    
    ;; If 
    {:tag :if_statement,
     :content
     [{:tag :IF, :content ["if"]}
      {:tag :relation,
       :content
       [{:tag :expression,
         :content
         [{:tag :term,
           :content
           [{:tag :NAME,
             :content ["x"],
             :scope {:local #{}, :global #{}}}]}]}
        {:tag :LT, :content ["<"]}
        {:tag :expression,
         :content
         [{:tag :term,
           :content [{:tag :INTEGER, :content [1]}]}]}]}
      {:tag :LBRACE, :content ["{"]}
      {:tag :statements,
       :content
       [{:tag :statement,
         :content
         [{:tag :print_statement,
           :content
           [{:tag :PRINT, :content ["print"]}
            {:tag :expression,
             :content
             [{:tag :term,
               :content
               [{:tag :NAME,
                 :content ["x"],
                 :scope {:local #{}, :global #{}}}]}]}
            {:tag :SEMI, :content [";"]}]}]}]}
      {:tag :RBRACE, :content ["}"]}
      {:tag :ELSE, :content ["else"]}
      {:tag :LBRACE, :content ["{"]}
      {:tag :statements,
       :content
       [{:tag :statement,
         :content
         [{:tag :print_statement,
           :content
           [{:tag :PRINT, :content ["print"]}
            {:tag :expression,
             :content
             [{:tag :term,
               :content
               [{:tag :NAME,
                 :content ["x"],
                 :scope {:local #{}, :global #{}}}]}]}
            {:tag :SEMI, :content [";"]}]}]}]}
      {:tag :RBRACE, :content ["}"]}]}
    {:variable "%VAR_7",	    
     :lines
     ["%VAR_1 = load i32, i32* %VAR_x                    ; %VAR_1 = x                  "
      "%VAR_2 = icmp slt i32 %VAR_1, 1                   ; <                           "
      "br i1 %VAR_2, label %LB3_TRUE, label %LB4_FALSE   ; if                          "
      "                                                  ;                             "
      "LB3_TRUE:                                         ;                             "
      "  %VAR_6 = load i32, i32* %VAR_x                  ; %VAR_6 = x                  "
      "  call i32 (i32) @_print_int(i32 %VAR_6)          ; print %VAR_6                "
      "  br label %LB5_RETURN                            ;                             "
      "                                                  ;                             "
      "LB4_FALSE:                                        ;                             "
      "  %VAR_7 = load i32, i32* %VAR_x                  ; %VAR_7 = x                  "
      "  call i32 (i32) @_print_int(i32 %VAR_7)          ; print %VAR_7                "
      "  br label %LB5_RETURN                            ;                             "
      "                                                  ;                             "
      "LB5_RETURN:                                       ;                             "],
     :mode :INDENT}

    ;; Assignment -> Global
    {:tag :assignment_statement,
     :content
     [{:tag :NAME,
       :content ["x"],
       :scope {:local #{}, :global #{"x"}}}
      {:tag :ASSIGN, :content ["="]}
      {:tag :expression,
       :content
       [{:tag :term, :content [{:tag :INTEGER, :content [42]}]}]}
      {:tag :SEMI, :content [";"]}]}
    {:variable 42,	    
     :lines
     ["store i32 42, i32* @x                             ; x = 42                      "],
     :mode nil}

    ;; Assignment -> Local
    {:tag :assignment_statement,
     :content
     [{:tag :NAME,
       :content ["x"],
       :scope {:local #{"x"}, :global #{}}}
      {:tag :ASSIGN, :content ["="]}
      {:tag :expression,
       :content
       [{:tag :term, :content [{:tag :INTEGER, :content [42]}]}]}
      {:tag :SEMI, :content [";"]}]}
    {:variable 42,	    
     :lines
     ["store i32 42, i32* %VAR_x                         ; x = 42                      "],
     :mode nil}

    {:tag :variable_definition,
     :content
     [{:tag :VAR, :content ["var"]}
      {:tag :NAME,
       :content ["x"],
       :scope {:local #{}, :global #{}}}
      {:tag :SEMI, :content [";"]}],
     :global true}
    {:variable nil,	    
     :lines
     ["@x = global i32 0                                 ; global=x                    "],
     :mode nil}

    {:tag :variable_definition,
     :content
     [{:tag :VAR, :content ["var"]}
      {:tag :NAME,
       :content ["x"],
       :scope {:local #{}, :global #{}}}
      {:tag :SEMI, :content [";"]}],
     :global nil}
    {:variable nil,	    
     :lines
     ["%VAR_x = alloca i32                               ; local=x                     "],
     :mode nil}

    {:tag :program,
     :content
     [{:tag :statements,
       :content
       [{:tag :func_definition,
         :content
         [{:tag :FUNC, :content ["func"]}
          {:tag :NAME, :content ["_main"]}
          {:tag :LPAREN, :content ["("]}
          {:tag :NAME, :content ["__x"]}
          {:tag :RPAREN, :content [")"]}
          {:tag :RBRACE, :content ["{"]}
          {:tag :statements,
           :content
           [{:tag :statement,
             :content
             [{:tag :print_statement,
               :content
               [{:tag :PRINT, :content ["print"]}
                {:tag :expression,
                 :content
                 [{:tag :term, :content [{:tag :INTEGER, :content [42]}]}]}
                {:tag :SEMI, :content [";"]}]}]}]}
          {:tag :LBRACE, :content ["}"]}]}]}],
     :scope {:local #{}, :global #{}}}
    {:variable 42,	    
     :lines
     ["declare i32 @_print_int(i32)                      ; runtime                     "
      "define i32 @_main(i32 %.__x)                      ; func _main(...)             "
      "{                                                 ;                             "
      "  %VAR___x = alloca i32                           ;                             "
      "  store i32 %.__x, i32* %VAR___x                  ;                             "
      "  call i32 (i32) @_print_int(i32 42)              ; print 42                    "
      "}                                                 ;                             "],
     :mode nil}
    ))
