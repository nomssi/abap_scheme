*&---------------------------------------------------------------------*
*&  Include           YY_LISP_AUNIT
*&---------------------------------------------------------------------*
*& ported from ZUSR_LISP_TEST by JNN (www.informatik-dv.com)

*&---------------------------------------------------------------------*
*& https://github.com/nomssi/abap_scheme
*& Tests for the Lisp interpreter written in ABAP
*&---------------------------------------------------------------------*
*& Martin Ceronio, martin.ceronio@infosize.co.za June 2015
*& Jacques Nomssi, nomssi@gmail.com
*& Turtle Graphics from Frederik Hudák, placed under The Unlicense
*& MIT License (see below)
*&---------------------------------------------------------------------*
*  The MIT License (MIT)
*
*  Copyright (c) 2015 Martin Ceronio
*  Copyright (c) 2017, 2018, 2020 Jacques Nomssi Nzali
*
*  Permission is hereby granted, free of charge, to any person obtaining a copy
*  of this software and associated documentation files (the "Software"), to deal
*  in the Software without restriction, including without limitation the rights
*  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*  copies of the Software, and to permit persons to whom the Software is
*  furnished to do so, subject to the following conditions:
*
*  The above copyright notice and this permission notice shall be included in
*  all copies or substantial portions of the Software.
*
*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
*  THE SOFTWARE.

*----------------------------------------------------------------------*
*       CLASS ltc_interpreter DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_interpreter DEFINITION FOR TESTING
     RISK LEVEL HARMLESS DURATION SHORT.
     PROTECTED SECTION.
       DATA mo_int TYPE REF TO lcl_lisp_interpreter.
       DATA mo_port TYPE REF TO lcl_lisp_buffered_port.
*   Initialize Lisp interpreter
       METHODS assert_code IMPORTING title    TYPE string
                                     code     TYPE string
                                     actual   TYPE any
                                     expected TYPE any
                                     level    TYPE aunit_level.
       METHODS test_f IMPORTING title    TYPE string
                                code     TYPE string
                                actual   TYPE tv_real
                                expected TYPE tv_real.

       METHODS scheme IMPORTING code     TYPE string
                                expected TYPE any
                                level    TYPE int1 DEFAULT if_abap_unit_constant=>severity-high.
       METHODS one_parameter IMPORTING code     TYPE string
                                       operation TYPE string.
       METHODS parameter_mismatch IMPORTING code     TYPE string
                                            operation TYPE string.
       METHODS invalid_digit IMPORTING digit TYPE tv_char
                             RETURNING VALUE(text) TYPE string.
       METHODS scheme_argument  IMPORTING code TYPE string
                                          operation TYPE string.
       METHODS scheme_incorrect IMPORTING code TYPE string
                                          operation TYPE string.
       METHODS code_test_f IMPORTING code     TYPE string
                                     expected TYPE tv_real.

       METHODS riff_shuffle_code RETURNING VALUE(code) TYPE string.

       METHODS new_interpreter.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.
       METHODS closing_1 FOR TESTING.
       METHODS closing_2 FOR TESTING.

       METHODS error_1 FOR TESTING.

*   Stability tests - No Dump should occur
       METHODS stability_1 FOR TESTING.
       METHODS stability_2 FOR TESTING.
*--------------------------------------------------------------------*
*   BASIC TESTS
       METHODS: basic_define_error FOR TESTING,
         basic_define_a_23 FOR TESTING,
*     Test strings
         basic_string_value FOR TESTING,
         basic_string_esc_double_quote FOR TESTING,
         basic_string_quot_esc_dbl_quot FOR TESTING,

*     Evaluating multiple expressions
         basic_multiple_expr FOR TESTING.
   ENDCLASS.                    "ltc_interpreter DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_interpreter IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_interpreter IMPLEMENTATION.

     METHOD new_interpreter.
       mo_port ?= lcl_lisp_new=>port(
           iv_port_type =  c_port_textual
           iv_buffered = abap_true
           iv_input  = abap_false
           iv_output = abap_true
           iv_error  = abap_true
           iv_string = abap_false ).
       mo_int = lcl_lisp_interpreter=>new( io_port = mo_port
                                           ii_log = mo_port ).
     ENDMETHOD.                    "new_interpreter

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

* Conduct a test with given code
     METHOD assert_code.
       cl_abap_unit_assert=>assert_equals(
         act = actual
         exp = expected
         msg = |Error { title } :{ code }\nActual : { actual }\nExpected :{ expected }\n|
         level = level ).

*    write:/ '<- ', code.
*    write:/ '-> ', actual.
     ENDMETHOD.                    "test

     METHOD test_f.
       cl_abap_unit_assert=>assert_equals_float(
         act = actual
         exp = expected
         msg = |Error { title } :{ code }\n| ).
     ENDMETHOD.                    "test_f

*    Conduct a test with given code
     METHOD scheme.
       assert_code( code = code
                    actual = mo_int->eval_source( code )
                    expected = expected
                    title = 'CODE'
                    level = level ).
     ENDMETHOD.                    "code_test

     METHOD one_parameter.
       scheme( code = code
               expected = |Eval: { operation } expects only one argument| ).
     ENDMETHOD.

     METHOD parameter_mismatch.
       scheme( code = code
               expected = |Eval: too many arguments in { operation }| ).
     ENDMETHOD.

     METHOD invalid_digit.
       text = |Eval: The argument 'digit { digit }' cannot be interpreted as a number|.
     ENDMETHOD.

     METHOD scheme_argument.
       scheme( code = code
               expected = `Eval: missing argument in ` && operation ).
     ENDMETHOD.

     METHOD scheme_incorrect.
       scheme( code = code
               expected = `Eval: Incorrect input in ` && operation ).
     ENDMETHOD.

     METHOD code_test_f.
       DATA lv_result TYPE tv_real.
       lv_result = mo_int->eval_source( code ).
       test_f( code = code
               actual = lv_result
               expected = expected
               title = 'CODE' ).
     ENDMETHOD.                    "code_test_f

     METHOD closing_1.
       scheme( code = '( + 1'
               expected = |Parse: missing a ) to close expression| ).
     ENDMETHOD.                    "closing_1

     METHOD closing_2.
       scheme( code = '(let ([x 3)] (* x x))'
               expected = |Parse: a ) found while ] expected| ).
     ENDMETHOD.                    "closing_2

     METHOD stability_1.
       scheme( code = 'a'
               expected = `Eval: Symbol a is unbound` ).
     ENDMETHOD.                    "stability_1

     METHOD stability_2.
       scheme_incorrect( code = '(define a)'
                         operation = 'define' ).
     ENDMETHOD.                    "stability_2

     METHOD basic_define_error.
       scheme( code = '(define 22 23)'
               expected = `Eval: 22 cannot be a variable identifier` ).
     ENDMETHOD.                    "basic_define_error

     METHOD basic_define_a_23.
       scheme( code = '(define a 23)'
               expected = `a` ).
       scheme( code = 'a'
               expected = `23` ).
     ENDMETHOD.                    "basic_define_a_23

     METHOD error_1.
       scheme_argument( code = '(error)'
                        operation = 'error' ).
       scheme( code = |(error "{ c_error_message }")|
               expected = |Eval: { c_error_message }| ).
     ENDMETHOD.

     METHOD basic_string_value.
       scheme( code = '"string value"'
               expected = `"string value"` ).
     ENDMETHOD.                    "basic_string_value

     METHOD basic_string_esc_double_quote.
       scheme( code = '"string value with \" escaped double quote"'
               expected = '"string value with \" escaped double quote"' ).
     ENDMETHOD.                    "basic_string_esc_double_quote

     METHOD basic_string_quot_esc_dbl_quot.
       scheme( code = '(quote "string value with \" escaped double quote")'
               expected = '"string value with \" escaped double quote"' ).
     ENDMETHOD.                    "basic_string_quot_esc_dbl_quot

     METHOD basic_multiple_expr.
*   Evaluating multiple expressions
       scheme( code = '(define a (list 1 2 3 4)) (define b (cdr a)) a b'
               expected = 'a b (1 2 3 4) (2 3 4)' ).
     ENDMETHOD.                    "basic_multiple_expr

     METHOD riff_shuffle_code.
       code =
        |(define riff-shuffle | &
        | ( lambda (deck) (begin | &
        | (define take | &
        | (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq)))))) | &
        | (define drop | &
        | (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))| &
        | (define mid | &
        | (lambda (seq) (/ (length seq) 2)))| &
        | ((combine append) (take (mid deck) deck) (drop (mid deck) deck))| &
        | )))|.
     ENDMETHOD.                    "riff_shuffle_code

   ENDCLASS.                    "ltc_interpreter IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_parse DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_parse DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PROTECTED SECTION.
       METHODS parse IMPORTING code TYPE string.
       METHODS assert_parse IMPORTING code     TYPE string
                                      expected TYPE string
                                      level    TYPE aunit_level DEFAULT if_aunit_constants=>critical.
     PRIVATE SECTION.
       METHODS delimiter FOR TESTING.
       METHODS empty FOR TESTING.
       METHODS char_x FOR TESTING.
       METHODS lambda FOR TESTING.
       METHODS lambda_comments FOR TESTING.
       METHODS skip_comments FOR TESTING.
       METHODS skip_comments_1 FOR TESTING.
       METHODS riff_shuffle FOR TESTING.

       METHODS directive FOR TESTING.
       METHODS identifier FOR TESTING.

       METHODS string_controls_0 FOR TESTING.
       METHODS string_controls_1 FOR TESTING.
       METHODS string_controls_2 FOR TESTING.
       METHODS string_controls_3 FOR TESTING.

       METHODS write_escape FOR TESTING.
   ENDCLASS.                    "ltc_parse DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_parse IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_parse IMPLEMENTATION.

     METHOD parse.
       DATA element TYPE REF TO lcl_lisp.

       DATA(elements) = mo_int->parse( code ).
       cl_abap_unit_assert=>assert_not_initial(
         act = lines( elements )
         msg = |No evaluated element from first expression| ).

       LOOP AT elements INTO element.
         mo_port->write( element ).
       ENDLOOP.
     ENDMETHOD.                    "parse

* Test parsing of a given piece of code and write out result
     METHOD assert_parse.
       parse( code ).
       assert_code( actual = mo_port->flush( )
                    code = code
                    expected = expected
                    title = 'PARSE'
                    level = level ).
     ENDMETHOD.                    "assert_parse

     METHOD identifier.
       assert_parse( code = '|two words|'
                     expected = |two words| ).
       assert_parse( code = '|two\x20;words|'
                     expected = |two words| ).
       assert_parse( code = '|H\x65;llo|'
                     expected = |Hello| ).
       scheme( code = `(equal? '|| '| |)`
                     expected = '#f' ).
     ENDMETHOD.

     METHOD directive.
       scheme( code = '#!fold-case'
               expected = space ).
       scheme( code = '#!no-fold-case'
               expected = space ).
       scheme( code = '#!no-foldcase'
               expected = 'Parse: Invalid directive #!no-foldcase' ).
     ENDMETHOD.

     METHOD delimiter.
       assert_parse( code = |list\t; return|
                     expected = |list| ).
     ENDMETHOD.                    "lambda

     METHOD empty.
       assert_parse( code = ''
                     expected = '#!eof' ).
     ENDMETHOD.                    "lambda

     METHOD char_x.
       assert_parse( code = '#\x'
                     expected = '"x"' ).
     ENDMETHOD.

     METHOD lambda.
       assert_parse( code = '(define a(lambda()20))'
                     expected = |(define a (lambda { c_lisp_nil } 20))| ).
     ENDMETHOD.                    "lambda

     METHOD skip_comments.
       scheme( code = |#; 234 (+ 3 4)|
               expected = |7| ).
     ENDMETHOD.

     METHOD skip_comments_1.
       scheme( code = |(if #f | &
                      |#;(= 3 1)| &
                      |(* 2 4) (- 10 3))|
               expected = |7| ).
     ENDMETHOD.

     METHOD lambda_comments.
       assert_parse( code = |;; Comments\n| &
                            |(define a(lambda()20)) ; comments|
                     expected = |(define a (lambda { c_lisp_nil } 20))| ).
     ENDMETHOD.                    "lambda

     METHOD riff_shuffle.
       assert_parse( code = riff_shuffle_code( )
                   expected =
   |(define riff-shuffle (lambda (deck) (begin (define take (lambda| &
   | (n seq) (if (<= n 0) '{ c_lisp_nil } (cons (car seq) (take (- n 1) (cdr seq))))))| &
   | (define drop (lambda (n seq) (if (<= n 0) seq| &
   | (drop (- n 1) (cdr seq))))) (define mid (lambda (seq) (/ (length seq) 2)))| &
   | ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))|
             ).
     ENDMETHOD.                    "riff_shuffle

     METHOD string_controls_0.
       assert_parse( code = '"\a"'
                     expected = '"\a"' ).
       scheme( code = '(eq? "\a" "\x07;")'
               expected = '#t' ).

       assert_parse( code = '"\b"'
                     expected = '"\b"' ).
       scheme( code = '(eq? "\b" "\x08;")'
               expected = '#t' ).

       assert_parse( code = '"\t"'
                     expected = '"\t"' ).
       scheme( code = '(eq? "\t" "\x09;")'
               expected = '#t' ).

       assert_parse( code = '"\n"'
                     expected = '"\n"' ).
       scheme( code = '(eq? "\n" "\x0A;")'
               expected = '#t' ).

       assert_parse( code = '"\r"'
                     expected = '"\r"' ).
       scheme( code = '(eq? "\r" "\x0D;")'
               expected = '#t' ).
     ENDMETHOD.

     METHOD string_controls_1.
       assert_parse( code = '"\""'
                     expected = '"\""' ).
       assert_parse( code = '"\\"'
                     expected = '"\\"' ).
       assert_parse( code = '"\|"'
                     expected = '"|"' ).
     ENDMETHOD.

     METHOD string_controls_2.
       scheme( code = '(define output (open-output-string))'
               expected = 'output' ).

       scheme( code = '(display "\"\\\|" output)'
               expected = '"\"\\|"' ).
       scheme( code = '(get-output-string output)'
               expected = '"\"\\|"' ).

     ENDMETHOD.

     METHOD string_controls_3.
       scheme( code = '(display "\|")'
               expected = '| "|"' ).
     ENDMETHOD.

     METHOD write_escape.
       assert_parse( code = '(write "#\\")'
                     expected = '(write "#\\")' ).

       scheme( code = '(define output (open-output-string))'
               expected = 'output' ).
       scheme( code = '(display "#\\" output)'
               expected = '"#\\"' ).
     ENDMETHOD.

   ENDCLASS.                    "ltc_parse IMPLEMENTATION

   CLASS ltc_grammar DEFINITION INHERITING FROM ltc_parse
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS detect_errors_0 FOR TESTING.
       METHODS detect_errors_1 FOR TESTING.
       METHODS detect_errors_2 FOR TESTING.
       METHODS detect_errors_3 FOR TESTING.
   ENDCLASS.

  CLASS ltc_grammar IMPLEMENTATION.

     METHOD detect_errors_0.
       assert_parse( code = '(define f (lambda (x) (set! 3 x)))'
                     expected = 'Eval: Parse Error' ).
     ENDMETHOD.

     METHOD detect_errors_1.
       assert_parse( code = '(define g (lambda (3) (if (x = 0))))'
                     expected = 'Eval: Parse Error' ).
     ENDMETHOD.

     METHOD detect_errors_2.
       assert_parse( code = '(define h (lambda (x) (if (x = 0) 1 2 3)))'
                     expected = 'Eval: Parse Error' ).
     ENDMETHOD.

     METHOD detect_errors_3.
       assert_parse( code = '(define 1 x)'
                     expected = 'Eval: 1 cannot be a variable identifier' ).
     ENDMETHOD.

  ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltc_basic DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_basic DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PUBLIC SECTION.
       METHODS constructor.

       METHODS call_no_lambda FOR TESTING.

       METHODS quote_2_args FOR TESTING.
       METHODS quote_19 FOR TESTING.
       METHODS quote_a FOR TESTING.
       METHODS quote_symbol_19 FOR TESTING.
       METHODS quote_symbol_a FOR TESTING.
       METHODS quote_list123 FOR TESTING.

       METHODS begin_1 FOR TESTING.

       METHODS set_1 FOR TESTING.
       METHODS set_2 FOR TESTING.
       METHODS set_3 FOR TESTING.

       METHODS set_car_1 FOR TESTING.
       METHODS set_car_2 FOR TESTING.

       METHODS let_1 FOR TESTING.
       METHODS let_2 FOR TESTING.
       METHODS let_3 FOR TESTING.
       METHODS let_4 FOR TESTING.

       METHODS let_star_1 FOR TESTING.

       METHODS do_1 FOR TESTING.
       METHODS do_2 FOR TESTING.
       METHODS do_3 FOR TESTING.
       METHODS do_4 FOR TESTING.
       METHODS do_5 FOR TESTING.

       METHODS named_let_1 FOR TESTING.
       METHODS named_let_2 FOR TESTING.
       METHODS named_let_3 FOR TESTING.

       METHODS let_no_body FOR TESTING.
       METHODS define_no_body FOR TESTING.

       METHODS letrec_1 FOR TESTING.
       METHODS letrec_2 FOR TESTING.

       METHODS letrec_star_0 FOR TESTING.
       METHODS values_0 FOR TESTING.

       METHODS define_values_0 FOR TESTING.
       METHODS define_values_1 FOR TESTING.

       METHODS call_cc_0 FOR TESTING.
       METHODS call_cc_1a FOR TESTING.
       METHODS call_cc_1b FOR TESTING.
       METHODS call_cc_2 FOR TESTING.
       METHODS call_cc_3 FOR TESTING.
       METHODS call_cc_4 FOR TESTING.
       METHODS call_cc_5 FOR TESTING.
       METHODS call_cc_6 FOR TESTING.
       METHODS call_cc_7 FOR TESTING.

       METHODS call_cc_values FOR TESTING.

       METHODS call_with_values_1 FOR TESTING.
       METHODS call_with_values_2 FOR TESTING.

       METHODS symbol_list_equal FOR TESTING.

       METHODS is_symbol_true_1 FOR TESTING.
       METHODS is_symbol_true_2 FOR TESTING.
       METHODS is_symbol_true_3 FOR TESTING.
       METHODS is_symbol_true_4 FOR TESTING.
       METHODS is_symbol_true_5 FOR TESTING.

       METHODS is_symbol_false FOR TESTING.
       METHODS is_symbol_false_1 FOR TESTING.

       METHODS is_hash_true FOR TESTING.
       METHODS is_hash_false FOR TESTING.

       METHODS is_procedure_lambda FOR TESTING.
       METHODS is_procedure_native FOR TESTING.
       METHODS is_procedure_quote FOR TESTING.
       METHODS is_procedure_native_3 FOR TESTING.
       METHODS is_procedure_native_4 FOR TESTING.
       METHODS is_procedure_syntax FOR TESTING.
       METHODS is_procedure_data FOR TESTING.

       METHODS is_string_true FOR TESTING.
       METHODS is_string_false FOR TESTING.
       METHODS is_number_true FOR TESTING.
       METHODS is_number_false FOR TESTING.

       METHODS is_boolean_1 FOR TESTING.
       METHODS is_boolean_2 FOR TESTING.
       METHODS is_boolean_3 FOR TESTING.

       METHODS list_is_boolean_1 FOR TESTING.
       METHODS list_is_boolean_2 FOR TESTING.
       METHODS list_is_boolean_3 FOR TESTING.
       METHODS list_is_boolean_4 FOR TESTING.
       METHODS list_is_boolean_5 FOR TESTING.
       METHODS list_is_boolean_6 FOR TESTING.

   ENDCLASS.                    "ltc_basic DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_string DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_string DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.

       METHODS setup.
       METHODS teardown.

       METHODS char_eq FOR TESTING.
       METHODS char_lt FOR TESTING.
       METHODS char_gt FOR TESTING.
       METHODS char_le FOR TESTING.
       METHODS char_ge FOR TESTING.

       METHODS char_ci_eq FOR TESTING.
       METHODS char_ci_lt FOR TESTING.
       METHODS char_ci_gt FOR TESTING.
       METHODS char_ci_le FOR TESTING.
       METHODS char_ci_ge FOR TESTING.

       METHODS char_in_list FOR TESTING.
       METHODS char_single FOR TESTING.
       METHODS char_unknown FOR TESTING.
       METHODS string_len FOR TESTING.
       METHODS string_delim FOR TESTING.
       METHODS string_parse FOR TESTING.

       METHODS string_empty FOR TESTING.

       METHODS string_set_0 FOR TESTING.
       METHODS string_set_1 FOR TESTING.
       METHODS string_set_2 FOR TESTING.

       METHODS compare_string_list_eq FOR TESTING.
       METHODS compare_string_list_lt FOR TESTING.
       METHODS compare_string_list_gt FOR TESTING.
       METHODS compare_string_list_le FOR TESTING.
       METHODS compare_string_list_ge FOR TESTING.

       METHODS compare_string_ci_list_eq FOR TESTING.
       METHODS compare_string_ci_list_lt FOR TESTING.
       METHODS compare_string_ci_list_gt FOR TESTING.
       METHODS compare_string_ci_list_le FOR TESTING.
       METHODS compare_string_ci_list_ge FOR TESTING.

       METHODS symbol_to_string FOR TESTING.

       METHODS char_alphabetic_1 FOR TESTING.
       METHODS char_alphabetic_2 FOR TESTING.
       METHODS char_alphabetic_3 FOR TESTING.
       METHODS char_alphabetic_4 FOR TESTING.
       METHODS char_alphabetic_space FOR TESTING.

       METHODS char_numeric_1 FOR TESTING.
       METHODS char_numeric_2 FOR TESTING.
       METHODS char_numeric_3 FOR TESTING.

       METHODS char_unicode_1 FOR TESTING.

       METHODS char_whitespace_1 FOR TESTING.
       METHODS char_whitespace_2 FOR TESTING.
       METHODS char_whitespace_3 FOR TESTING.

       METHODS char_upper_case_1 FOR TESTING.
       METHODS char_upper_case_2 FOR TESTING.
       METHODS char_upper_case_3 FOR TESTING.

       METHODS char_lower_case_1 FOR TESTING.
       METHODS char_lower_case_2 FOR TESTING.
       METHODS char_lower_case_3 FOR TESTING.

       METHODS digit_value_1 FOR TESTING.
       METHODS digit_value_2 FOR TESTING.
       METHODS digit_value_3 FOR TESTING.
       METHODS digit_value_4 FOR TESTING.

       METHODS char_to_integer_1 FOR TESTING.
       METHODS char_to_integer_2 FOR TESTING.
       METHODS char_to_integer_3 FOR TESTING.

       METHODS integer_to_char_1 FOR TESTING.
       METHODS integer_to_char_2 FOR TESTING.

       METHODS char_upcase_1 FOR TESTING.

       METHODS char_downcase_1 FOR TESTING.

   ENDCLASS.                    "ltc_string DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_conditionals DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_conditionals DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS if_1 FOR TESTING.
       METHODS if_2 FOR TESTING.
       METHODS if_3 FOR TESTING.

       METHODS and_1 FOR TESTING.
       METHODS and_2 FOR TESTING.
       METHODS and_3 FOR TESTING.
       METHODS and_4 FOR TESTING.

       METHODS or_1 FOR TESTING.
       METHODS or_2 FOR TESTING.
       METHODS or_3 FOR TESTING.
       METHODS or_4 FOR TESTING.

       METHODS cond_1 FOR TESTING.
       METHODS cond_2 FOR TESTING.
       METHODS cond_3 FOR TESTING.
       METHODS cond_4 FOR TESTING.
       METHODS cond_5 FOR TESTING.
       METHODS cond_else FOR TESTING.

       METHODS case_no_args FOR TESTING.
       METHODS case_no_clauses FOR TESTING.

       METHODS case_1 FOR TESTING.
       METHODS case_2 FOR TESTING.
       METHODS case_3 FOR TESTING.

       METHODS not_1 FOR TESTING.
       METHODS not_2 FOR TESTING.
       METHODS not_3 FOR TESTING.
       METHODS not_4 FOR TESTING.
       METHODS not_5 FOR TESTING.
       METHODS not_6 FOR TESTING.
       METHODS not_7 FOR TESTING.
       METHODS not_8 FOR TESTING.

       METHODS when_1 FOR TESTING.

       METHODS unless_1 FOR TESTING.
       METHODS unless_2 FOR TESTING.

   ENDCLASS.                    "ltc_conditionals DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_quote DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_quote DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS quasiquote_1 FOR TESTING.
       METHODS quasiquote_2 FOR TESTING.
       METHODS quasiquote_2_args FOR TESTING.
       METHODS quasiquote_splicing_3 FOR TESTING.
       METHODS quasiquote_splicing_4 FOR TESTING.
       METHODS quasiquote_splicing_5 FOR TESTING.
       METHODS quasiquote_splicing_6 FOR TESTING.

       METHODS quasiquote_7 FOR TESTING.
       METHODS quasiquote_8 FOR TESTING.
       METHODS quasiquote_9 FOR TESTING.
       METHODS quasiquote_10 FOR TESTING.
       METHODS quasiquote_11 FOR TESTING.
       METHODS quasiquote_12 FOR TESTING.
       METHODS quasiquote_13 FOR TESTING.

       METHODS quine_1 FOR TESTING.
       METHODS quine_2 FOR TESTING.
   ENDCLASS.                    "ltc_quote DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_macro DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_macro DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS macro_while FOR TESTING.

       METHODS macro_1 FOR TESTING.
       METHODS macro_2 FOR TESTING.

       METHODS macro_one FOR TESTING.
       METHODS macro_two FOR TESTING.

       METHODS macro_unless_1 FOR TESTING.
       METHODS macro_unless_2 FOR TESTING.

       METHODS macro_eval_1 FOR TESTING.

   ENDCLASS.                    "ltc_macro DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_query DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_query DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.

       METHODS select_1 FOR TESTING.

   ENDCLASS.                    "ltc_query DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_basic IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_basic IMPLEMENTATION.

     METHOD constructor.
       super->constructor( ).
     ENDMETHOD.                    "constructor

     METHOD call_no_lambda.
       scheme( code = '(1 2)'
               expected = 'Eval: attempt to apply 1 - not a procedure' ).
     ENDMETHOD.                    "quote_19

     METHOD quote_19.
       scheme( code = '(quote 19)'
               expected = '19' ).
     ENDMETHOD.                    "quote_19

     METHOD quote_a.
       scheme( code = '(quote a)'
               expected = 'a' ).
     ENDMETHOD.                    "quote_a

     METHOD quote_2_args.
       scheme( code = '(quote a b)'
               expected = 'Eval: quote can only take a single argument' ).
     ENDMETHOD.                    "quote_2_args

     METHOD quote_symbol_19.
       scheme( code = '''19'
               expected = '19' ).
     ENDMETHOD.                    "quote_symbol_19

     METHOD quote_symbol_a.
       scheme( code = '''a'
               expected = 'a' ).
     ENDMETHOD.                    "quote_symbol_a

     METHOD quote_list123.
       scheme( code = '''(list 1 2 3)'
               expected = '(list 1 2 3)' ).
     ENDMETHOD.                    "quote_list123

     METHOD begin_1.
       scheme( code = '(define x 0)'
               expected = 'x' ).
       scheme( code = |(and (= x 0)| &
                         |     (begin (set! x 5)| &
                         |            (+ x 1)))|
               expected = '6' ).
     ENDMETHOD.                    "begin_1

     METHOD set_1.
       scheme( code = '(define x 3)'
               expected = 'x' ).
       scheme( code = '(set! x 7)'
               expected = 'x' ).
       scheme( code = 'x'
               expected = '7' ).
     ENDMETHOD.                    "set_1

     METHOD set_2.
       scheme( code = '(set! x 5)'
               expected = 'Eval: Symbol x is unbound' ).
       scheme( code = '(set! 7 5)'
               expected = 'Eval: 7 is not a bound symbol' ).
       scheme_incorrect( code = '(set!)'
                         operation = 'set!' ).
     ENDMETHOD.                    "set_2

     METHOD set_3.
       scheme( code = '(define *seed* 1)'
               expected = '*seed*' ).
       scheme( code = |(define (srand seed)| &
                         |(set! *seed* seed)| &
                         |*seed*)|
               expected = 'srand' ).
       scheme( code = '(srand 2)'
               expected = '2' ).
     ENDMETHOD.                    "set_3

     METHOD set_car_1.
       scheme( code = |(define (f) (list 'not-a-constant-list))|
               expected = 'f' ).
       scheme( code = |(set-car! (f) 3)|   " unspeciﬁed
               expected = c_lisp_nil ).
     ENDMETHOD.

     METHOD set_car_2.
       scheme( code = |(define (g) '(constant-list))|
               expected = 'g' ).
       scheme( code = |(set-car! (g) 3)|   " error
               expected = 'Eval: constant list in set-car! cannot be changed' ).
     ENDMETHOD.

     METHOD let_1.
       scheme( code = '(let ((x 4) (y 5)) (+ x y))'
               expected = '9' ).
     ENDMETHOD.                    "let_1

     METHOD let_2.
       scheme( code = |(let ((x 2) (y 3))| &
                         |  (let ((foo (lambda (z) (+ x y z)))| &
                         |        (x 7))| &
                         |    (foo 4)))|
               expected = '9' ).
     ENDMETHOD.                    "let_2

     METHOD let_3.
       scheme( code = |(let ((x 2) (x 0))| &
                         |    (+ x 5))|
               expected = 'Eval: variable x appears more than once' ).
     ENDMETHOD.                    "let_3

     METHOD let_4.
       scheme( code = |(let ((x 2))| &
                         | (define foo 2)| &
                         | (define foo 3)| &
                         |    (+ x 5))|
               expected = 'Eval: variable foo appears more than once' ).
     ENDMETHOD.                    "let_4

     METHOD let_star_1.
       scheme( code = |(let ((x 2) (y 3))| &
                         |  (let* ((x 7)| &
                         |        (z (+ x y)))| &
                         |  (* z x)))|
               expected = '70' ).
     ENDMETHOD.                    "let_star_1

     METHOD do_1.
       scheme( code = |(do ((vec (make-vector 5) )| &
                         |    (i 0 (+ i 1) ) )| &
                         |    ((= i 5) vec)| &
                         |   (vector-set! vec i i))|
               expected = '#(0 1 2 3 4)' ).
     ENDMETHOD.                    "do_1

     METHOD do_2.
       scheme( code = |(let ((x '(1 3 5 7 9)))| &
                         |  (do ((x x (cdr x))| &
                         |    (sum 0  (+ sum (car x))))| &
                         |((null? x) sum)))|
               expected = '25' ).
     ENDMETHOD.                    "do_2

     METHOD do_3.
       scheme( code = |(let ((x '(1 3)))| &
                         |  (do ((x x (cdr x))| &
                         |    (sum 0  (+ sum (car x))))| &
                         |((null? x) )))|              " Do without a body
               expected = c_lisp_nil ).                  " unspecified
     ENDMETHOD.                    "do_3

     METHOD do_4.
       scheme( code = |(define lst '())| &
                      |  (do (num (read))| &
                      |      (i 1 (+ 1 i))| &
                      |      ((= i 9) (display lst )| &
                      |       (append num lst)) ))|
               expected = 'Eval: Identifier ) not valid.' ).                  " should not dump!
     ENDMETHOD.                    "do_4

     METHOD do_5.
       scheme( code = '(define output (open-output-string))'
               expected = 'output' ).
       scheme( code = |(do ( (lst '())| &
                      |    (i 1 (+ 1 i)) )| &
                      |((= i 9) (display lst output) )| &
                      |(set! lst (cons i lst)) )|
               expected = '(8 7 6 5 4 3 2 1)' ).
     ENDMETHOD.

     METHOD named_let_1.
       scheme( code = |(define (number->list n)| &
                         |  (let loop ((n n)| &
                         |             (acc '()))| &
                         |    (if (< n 10)| &
                         |        (cons n acc)| &
                         |        (loop (quotient n 10)| &
                         |              (cons (remainder n 10) acc)))))|
               expected = 'number->list' ).
       scheme( code = |(number->list 239056)|
               expected = '(2 3 9 0 5 6)' ).
     ENDMETHOD.                    "named_let_1

     METHOD named_let_2.
       scheme( code = |(let loop ((numbers '(3 -2 1 6 -5))| &
                         |             (nonneg '())| &
                         |             (neg '()))| &
                         |  (cond ((null? numbers) (list nonneg neg))| &
                         |           ((>= (car numbers) 0)| &
                         |            (loop (cdr numbers)| &
                         |                   (cons (car numbers) nonneg)| &
                         |                   neg))| &
                         |           ((< (car numbers) 0)| &
                         |             (loop (cdr numbers)| &
                         |                    nonneg| &
                         |                    (cons (car numbers) neg)))))|
               expected = '((6 1 3) (-5 -2))' ).
     ENDMETHOD.                    "named_let_2

     METHOD named_let_3. " from Racket Guide
       scheme( code =  |(define (duplicate pos lst)| &
                          |  (let dup ([i 0]| &
                          |            [lst lst])| &
                          |   (cond| &
                          |    [(= i pos) (cons (car lst) lst)]| &
                          |    [else (cons (car lst) (dup (+ i 1) (cdr lst)))])))|
               expected = 'duplicate' ).
       scheme( code = |(duplicate 1 (list "apple" "cheese burger!" "banana"))|
               expected = |("apple" "cheese burger!" "cheese burger!" "banana")| ).
     ENDMETHOD.                    "named_let_3

     METHOD let_no_body.
       scheme( code = |(let ((var 10))| &
                         |     )|
               expected = 'Eval: no expression in body' ).
     ENDMETHOD.                    "let_no_body

     METHOD define_no_body.
       scheme( code = |(define (comp? (a b) (eq? a b)))|
               expected = 'Eval: (comp? (a b) (eq? a b)) no expression in body' ).
     ENDMETHOD.                    "define_no_body

     METHOD letrec_1.
       scheme( code = '(define (not x) (if (eq? x #f) #t #f) )'
               expected = 'not' ).
       scheme( code = |(letrec ((is-even? (lambda (n)| &
                         |                     (or (zero? n)| &
                         |                         (is-odd? (- n 1)))))| &
                         |         (is-odd? (lambda (n)| &
                         |                     (and (not (zero? n))| &
                         |                          (is-even? (- n 1))))) )| &
                         |(is-odd? 11))|
               expected = '#t' ).
     ENDMETHOD.                    "letrec_1

     METHOD letrec_2.
       scheme( code = |(letrec ((a 5)| &
                         |         (b (+ a 3)))| &
                         |b)|
               expected = '8' ).
     ENDMETHOD.                    "letrec_2

     METHOD letrec_star_0.
       scheme( code =
          |;; Returns the harmonic means of a nested list of numbers\n| &
          |(define (means ton)| &
          |  (letrec*| &
          |     ((mean (lambda ()| &
          |          (/ (/ (sum / ton) n))))| &
          |      (sum (lambda (g ton)| &
          |          (if (null? ton)| &
          |            (+)| &
          |            (if (number? ton)| &
          |                (g ton)| &
          |                (+ (sum g (car ton))| &
          |                   (sum g (cdr ton)))))))| &
          |      (n (sum (lambda (x) 1) ton)))| &
          |   (mean)))|
               expected = 'means' ).

       scheme( code = |(means '(3 (1 4)))|
               expected = |36/19| ).
     ENDMETHOD.                    "letrec_star_0

     METHOD values_0.
       scheme( code =
          |;; Returns the arithmetic, geometric, and\n| &
          |;; harmonic means of a nested list of numbers\n| &
          |(define (means ton)| &
          |  (letrec*| &
          |     ((mean| &
          |        (lambda (f g)| &
          |          (f (/ (sum g ton) n))))| &
          |      (sum| &
          |        (lambda (g ton)| &
          |          (if (null? ton)| &
          |            (+)| &
          |            (if (number? ton)| &
          |                (g ton)| &
          |                (+ (sum g (car ton))| &
          |                   (sum g (cdr ton)))))))| &
          |      (n (sum (lambda (x) 1) ton)))| &
          |    (values (mean values values)| &
          |            (mean exp log)| &
          |            (mean / /))))|
               expected = 'means' ).

       " evaluating (means '(3 (1 4))) returns three values:
       " 8/3, 2.28942848510666 (approximately), and 36/19.
       scheme( code = |(means '(3 (1 4)))|
               expected = |8/3 2.289428485106663735616084423879354 36/19| ).
     ENDMETHOD.                    "values_0

     METHOD call_cc_0. " continuation is not invoked
       scheme( code = |(+ 2 (call/cc (lambda (cont) 3))) |
               expected = '5' ).
     ENDMETHOD.

     METHOD call_cc_1a.  " invoke the continuation
       scheme( code = |(+ 2 (call/cc (lambda (cont) (cont 10) 3)))|
               expected = '12' ).
     ENDMETHOD.

     METHOD call_cc_1b.  " invoke the continuation
       scheme( code = |(call/cc (lambda (k) (* 5 (k 4))))|
               expected = '4' ).
     ENDMETHOD.

     METHOD call_cc_2.
       scheme( code = |(call-with-current-continuation | &
                         |  (lambda (exit)           | &
                         |    (for-each (lambda (x)  | &
                         |       (if (negative? x)   | &
                         |         (exit x)))        | &
                         |      '(54 0 37 -3 245 19))| &
                         |  #t))|
               expected = '-3' ).
     ENDMETHOD.

     METHOD call_cc_3.
       scheme( code = |(define list-length                              | &
                         |  (lambda (obj)                                  | &
                         |    (call-with-current-continuation              | &
                         |       (lambda (return)                          | &
                         |         (letrec ((r                             | &
                         |                   (lambda (obj)                 | &
                         |                     (cond ((null? obj) 0)       | &
                         |                           ((pair? obj)          | &
                         |                             (+ (r (cdr obj)) 1))| &
                         |                           (else (return #f))))))| &
                         |          (r obj)))))) |
               expected = 'list-length' ).

       scheme( code = |(list-length '(1 2 3 4))|
               expected = '4' ).
       scheme( code = |(list-length '(a b . c))|
               expected = '#f' ).
     ENDMETHOD.

     METHOD call_cc_4.
       scheme( code = |(call/cc (lambda (throw) | &
                      | (+ 5 (* 10 (call/cc (lambda (escape) (* 100 (escape 3))))))))|
               expected = '35' ).
     ENDMETHOD.

     METHOD call_cc_5.
       scheme( code = |(call/cc (lambda (throw) | &
                      |  (+ 5 (* 10 (call/cc (lambda (escape) (* 100 (throw 3))))))))|
               expected = '3' ).
     ENDMETHOD.

     METHOD call_cc_6.
       scheme( code = |(+ 4 (call/cc | &
                      |    (lambda (cont) (cont (+ 1 2))))) |
               expected = '7' ).
     ENDMETHOD.

     METHOD call_cc_7.
       scheme( code = |(define handle #f)|
               expected = 'handle' ).

       scheme( code = |(+ 2 (call/cc | &
                      |    (lambda (k) (set! handle k) 2)))|
               expected = '4' ).
       scheme( code = |(handle 6)|
               expected = '8' ).
       scheme( code = |(handle 20)|
               expected = '22' ).
     ENDMETHOD.

     METHOD call_cc_values.
       scheme( code = |(define (values . things)                 | &
                         |  (call-with-current-continuation          | &
                         |     (lambda (cont) (apply cont things)))) |
               expected = 'values' ).
     ENDMETHOD.                    "call_cc_values

     METHOD define_values_0.
       scheme( code = |(define-values (x y) (exact-integer-sqrt 17))|
               expected = 'x y' ).   " nicht definiert in r7rs
       scheme( code = |(list x y)|
               expected = '(4 1)' ).
       scheme( code = |(define-values (x y) (exact-integer-sqrt 8))|
               expected = 'x y' ).
       scheme( code = |(list x y)|
               expected = '(2 4)' ).
     ENDMETHOD.

     METHOD define_values_1.
       scheme( code = |(define-values (x y) (values 1 2))|
               expected = 'x y' ).    " nicht definiert in r7rs
       scheme( code = |(+ x y)|
               expected = '3' ).
     ENDMETHOD.

     METHOD call_with_values_1.
       scheme( code = |(call-with-values (lambda () (values 4 5)) | &
                         |     (lambda (a b) b))|
               expected = '5' ).
     ENDMETHOD.

     METHOD call_with_values_2.
       scheme( code = |(call-with-values * -)|
               expected = '-1' ).
     ENDMETHOD.

     METHOD symbol_list_equal.
       scheme( code = |(symbol=? 'x 'y 1 2)|
               expected = '#f' ).
       scheme( code = `(symbol=? 'x 'x '|x|)`
               expected = '#t' ).
     ENDMETHOD.

     METHOD is_symbol_true_1.
       scheme( code = |(define x 5)|
               expected = 'x' ).
       scheme( code = |(symbol? 'x)|
               expected = '#t' ).
       scheme( code = |(symbol? x)|
               expected = '#f' ).
     ENDMETHOD.                    "is_symbol_true_1

     METHOD is_symbol_true_2.
       scheme( code = |(symbol? (car '(a b)))|
               expected = '#t' ).
     ENDMETHOD.                    "is_symbol_true_2

     METHOD is_symbol_true_3.
       scheme( code = |(symbol? x)|
               expected = 'Eval: Symbol x is unbound' ).
     ENDMETHOD.                    "is_symbol_true_3

     METHOD is_symbol_true_4.
       scheme( code = |(symbol? 'nil)|
               expected = '#t' ).
     ENDMETHOD.                    "is_symbol_true_4

     METHOD is_symbol_true_5.
       scheme( code = |(apply symbol? '(primitive-procedure-test))|
               expected = '#t' ).
     ENDMETHOD.                    "is_symbol_true_5

     METHOD is_symbol_false.
       scheme( code = |(symbol? "bar")|
               expected = '#f' ).
       scheme( code = |(symbol? 4)|
               expected = '#f' ).
       scheme( code = |(symbol? '())|
               expected = '#f' ).
     ENDMETHOD.                    "is_symbol_false

     METHOD is_symbol_false_1.
       scheme( code = |(symbol? #f)|
               expected = '#f' ).
     ENDMETHOD.                    "is_symbol_false_1

     METHOD is_hash_true.
       scheme( code = |(define h (make-hash '(dog 4 car 5)))|
               expected = 'h' ).
       scheme( code = |(hash? h)|
               expected = '#t' ).
     ENDMETHOD.                    "is_hash_true

     METHOD is_hash_false.
       scheme( code = |(hash? 5)|
               expected = '#f' ).
     ENDMETHOD.                    "is_hash_false

     METHOD is_procedure_lambda.
       scheme( code = |(define (fn x) (+ x 5))|
               expected = 'fn' ).
       scheme( code = |(procedure? fn)|
               expected = '#t' ).
     ENDMETHOD.                    "is_procedure_lambda

     METHOD is_procedure_native.
       scheme( code = |(procedure? car)|
               expected = '#t' ).
       scheme( code = |(procedure? 'car)|
               expected = '#f' ).
     ENDMETHOD.                    "is_procedure_native

     METHOD is_procedure_quote.
       scheme( code = |(procedure? (lambda (x) (* x x)))|
               expected = '#t' ).
       scheme( code = |(procedure? '(lambda (x) (* x x)))|
               expected = '#f' ).
     ENDMETHOD.                    "is_procedure_quote

     METHOD is_procedure_native_3.
       scheme( code = |(procedure? apply)|
               expected = '#t' ).
     ENDMETHOD.                    "is_procedure_native_3

     METHOD is_procedure_native_4.
       scheme( code = |(procedure? map)|
               expected = '#t' ).
     ENDMETHOD.                    "is_procedure_native_4

     METHOD is_procedure_syntax.
       scheme( code = |(procedure? define)|
               expected = '#f' ).
     ENDMETHOD.                    "is_procedure_syntax

     METHOD is_procedure_data.
       scheme( code = |(define x 5)|
               expected = 'x' ).
       scheme( code = |(procedure? x)|
               expected = '#f' ).
     ENDMETHOD.                    "is_procedure_data

     METHOD is_string_true.
       scheme( code = |(define txt "Badenkop")|
               expected = 'txt' ).
       scheme( code = |(string? txt)|
               expected = '#t' ).
     ENDMETHOD.                    "is_string_true

     METHOD is_string_false.
       scheme( code = |(string? 34)|
               expected = '#f' ).
     ENDMETHOD.                    "is_string_false

     METHOD is_number_true.
       scheme( code = |(define n 5)|
               expected = 'n' ).
       scheme( code = |(number? n)|
               expected = '#t' ).
       scheme( code = |(number? 3+4i)|
               expected = '#t' ).
       scheme( code = |(number? -inf.0)|
               expected = '#t' ).
     ENDMETHOD.                    "is_number_true

     METHOD is_number_false.
       scheme( code = |(define d "5")|
               expected = 'd' ).
       scheme( code = |(number? d)|
               expected = '#f' ).
     ENDMETHOD.                    "is_number_false

     METHOD is_boolean_1.
       scheme( code = |(boolean? #f)|
               expected = '#t' ).
     ENDMETHOD.                    "is_boolean_1

     METHOD is_boolean_2.
       scheme( code = |(boolean? 0)|
               expected = '#f' ).
     ENDMETHOD.                    "is_boolean_2

     METHOD is_boolean_3.
       scheme( code = |(boolean? '())|
               expected = '#f' ).
     ENDMETHOD.                    "is_boolean_3

     METHOD list_is_boolean_1.
       scheme( code = |(boolean=? '())|
               expected = |Eval: boolean=? missing boolean argument in { c_lisp_nil }| ).
     ENDMETHOD.                    "list_is_boolean_1

     METHOD list_is_boolean_2.
       scheme( code = |(boolean=? '(#t #f))|
               expected = 'Eval: boolean=? missing boolean argument in (#t #f)' ).
     ENDMETHOD.                    "list_is_boolean_2

     METHOD list_is_boolean_3.
       scheme( code = |(boolean=? #t #f)|
               expected = '#f' ).
     ENDMETHOD.                    "list_is_boolean_3

     METHOD list_is_boolean_4.
       scheme( code = |(boolean=? #t #f 1)|
               expected = '#f' ).
     ENDMETHOD.                    "list_is_boolean_4

     METHOD list_is_boolean_5.
       scheme( code = |(boolean=? #t 1)|
               expected = 'Eval: boolean=? wrong argument 1' ).
     ENDMETHOD.                    "list_is_boolean_5

     METHOD list_is_boolean_6.
       scheme( code = |(boolean=? #f #f #f)|
               expected = '#t' ).
     ENDMETHOD.                    "list_is_boolean_6

   ENDCLASS.                    "ltc_basic IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_string IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_string IMPLEMENTATION.

     METHOD setup.
       mo_port ?= lcl_lisp_new=>port(
           iv_port_type =  c_port_textual
           iv_buffered = abap_true
           iv_input  = abap_false
           iv_output = abap_true
           iv_error  = abap_true
           iv_string = abap_true ).
       mo_int = lcl_lisp_interpreter=>new( io_port = mo_port
                                           ii_log = mo_port ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD char_in_list.
       scheme( code = `'(#\A #\a)`
               expected = '("A" "a")' ).
     ENDMETHOD.                    "char_in_list

     METHOD char_eq.
       scheme( code = `(char=? #\a #\a)`
               expected = '#t' ).
       scheme( code = `(char=? #\A #\a)`
               expected = '#f' ).
       scheme( code = `(char=? #\A)`
               expected = 'Eval: missing argument in char=?' ).
     ENDMETHOD.                    "char_eq

     METHOD char_lt.
       scheme( code = `(char<? #\a #\B)`
               expected = '#f' ).
       scheme( code = `(char<? #\A #\a)`
               expected = '#t' ).
       scheme( code = `(char<?)`
               expected = 'Eval: missing argument in char<?' ).
     ENDMETHOD.                    "char_lt

     METHOD char_gt.
       scheme( code = `(char>? #\B #\a)`
               expected = '#f' ).
       scheme( code = `(char>? #\b #\A)`
               expected = '#t' ).
       scheme( code = `(char>? #\C #\B #\A )`
               expected = '#t' ).
     ENDMETHOD.                    "char_gt

     METHOD char_le.
       scheme( code = `(char<=? #\B #\T #\a #\b )`
               expected = '#t' ).
       scheme( code = `(char<=? #\B #\a)`
               expected = '#t' ).
       scheme( code = `(char<=? #\C)`
               expected = 'Eval: missing argument in char<=?' ).
     ENDMETHOD.                    "char_le

     METHOD char_ge.
       scheme( code = `(char>=? #\b #\a #\V)`
               expected = '#t' ).
       scheme( code = `(char>=? #\e #\D #\B #\b)`
               expected = '#f' ).
       scheme( code = `(char>=? #\c #\b #\B)`
               expected = '#t' ).
     ENDMETHOD.                    "char_ge

     METHOD char_ci_eq.
       scheme( code = `(char-ci=? #\A #\a)`
               expected = '#t' ).
       scheme( code = `(char-ci=? #\A #\b)`
               expected = '#f' ).
       scheme( code = `(char-ci=? #\A)`
               expected = 'Eval: missing argument in char-ci=?' ).
     ENDMETHOD.                    "char_ci_eq

     METHOD char_ci_lt.
       scheme( code = `(char-ci<? #\a #\B)`
               expected = '#t' ).
       scheme( code = `(char-ci<? #\A #\a)`
               expected = '#f' ).
       scheme( code = `(char-ci<?)`
               expected = 'Eval: missing argument in char-ci<?' ).
     ENDMETHOD.                    "char_ci_lt

     METHOD char_ci_gt.
       scheme( code = `(char-ci>? #\B #\a)`
               expected = '#t' ).
       scheme( code = `(char-ci>? #\B #\b)`
               expected = '#f' ).
       scheme( code = `(char-ci>? #\c #\B #\b)`
               expected = '#f' ).
     ENDMETHOD.                    "char_ci_gt

     METHOD char_ci_le.
       scheme( code = `(char-ci<=? #\a #\B #\b #\T)`
               expected = '#t' ).
       scheme( code = `(char-ci<=? #\B #\a)`
               expected = '#f' ).
       scheme( code = `(char-ci<=? #\C)`
               expected = 'Eval: missing argument in char-ci<=?' ).
     ENDMETHOD.                    "char_ci_le

     METHOD char_ci_ge.
       scheme( code = `(char-ci>=? #\b #\a #\V)`
               expected = '#f' ).
       scheme( code = `(char-ci>=? #\e #\D #\B #\b)`
               expected = '#t' ).
       scheme( code = `(char-ci>=? #\c #\B #\b)`
               expected = '#t' ).
     ENDMETHOD.                    "char_ci_ge

     METHOD char_single.
       scheme( code = '#\A'
               expected = '"A"' ).
     ENDMETHOD.                    "char_single

     METHOD char_unknown.
       scheme( code = '#\aA'
               expected = 'Parse: unknown char #\aA found' ).
       DATA lo_no_number TYPE REF TO cx_sy_conversion_no_number.
       CREATE OBJECT lo_no_number
         EXPORTING
           textid = '995DB739AB5CE919E10000000A11447B'
           value  = `digit `.   " The argument digit  cannot be interpreted as a number

       scheme( code = `(char-ci>=? #e #D #\B #\b)`
               expected = |Eval: { lo_no_number->get_text( ) }| ).
     ENDMETHOD.                    "char_unknown

     METHOD string_len.
       scheme( code = '(string-length "Abd#\aA")'
               expected = '6' ).
     ENDMETHOD.                    "string_len

     METHOD string_delim.
       scheme( code = '"Benjamin \"Bugsy\" Siegel"'
               expected = '"Benjamin \"Bugsy\" Siegel"' ).
     ENDMETHOD.                    "string_delim

     METHOD string_parse.
       scheme( code = `"Here#s text \ ` &
                      ` containing just one line"`
               expected = '"Here#s text containing just one line"' ).

       scheme( code = '"\x03B1; is named GREEK SMALL LETTER ALPHA."'
               expected = '"α is named GREEK SMALL LETTER ALPHA."' ).
     ENDMETHOD.                    "string_parse

     METHOD string_empty.
       scheme( code = '(string)'
               expected = '""' ).
     ENDMETHOD.                    "string_set_0

     METHOD string_set_0.
       scheme( code = '(define s (string #\A #\p #\p #\l #\e))'
               expected = 's' ).
       scheme( code = '(string-set! s 4 #\y)'
               expected = '"Apply"' ).
     ENDMETHOD.                    "string_set_0

     METHOD string_set_1.
       scheme( code = '(define (f) (make-string 3 #\*)) '
               expected = 'f' ).
       scheme( code = '(define (g) "***")'
               expected = 'g' ).
       scheme( code = '(string-set! (f) 0 #\?)'
               expected = '"?**"' ).  " <-- unspecified
       scheme( code = '(string-set! (g) 0 #\?)'
               expected = 'Eval: constant in string-set! cannot be changed' ).
     ENDMETHOD.                    "string_set_1

     METHOD string_set_2.
       scheme( code = |(string-set! (symbol->string 'immutable)| && ' 0 #\?)'
               expected = 'Eval: constant in string-set! cannot be changed' ).
     ENDMETHOD.                    "string_set_2

     METHOD symbol_to_string.
       scheme( code = |(symbol->string 'mysymbol)|
               expected = '"mysymbol"' ).
     ENDMETHOD.                    "symbol_to_string

     METHOD compare_string_list_eq.
       scheme( code = |(string=? "Apple" "apple")|
               expected = '#f' ).
       scheme( code = |(string=? "a" "as" "a")|
               expected = '#f' ).
       scheme( code = |(string=? "mom" "mom")|
               expected = '#t' ).
       scheme( code = |(string=? "Mom and Dad" "mom and dad")|
               expected = '#f' ).
     ENDMETHOD.                    "compare_string_list_eq

     METHOD compare_string_list_lt.
       scheme( code = |(string<? "Apple" "Cap")|
               expected = '#t' ).
       scheme( code = |(string<? "a" "bas" "bas")|
               expected = '#f' ).
       scheme( code = |(string<? "mom" "mommy")|
               expected = '#t' ).
       scheme( code = |(string<? "a" "b" "c")|
               expected = '#t' ).
     ENDMETHOD.                    "compare_string_list_lt

     METHOD compare_string_list_gt.
       scheme( code = |(string>? "Tapple" "Mapple" "Apple" )|
               expected = '#t' ).
       scheme( code = |(string>? "tza" "tas" "zca")|
               expected = '#f' ).
       scheme( code = |(string>? "Dad" "Dad")|
               expected = '#f' ).
     ENDMETHOD.                    "compare_string_list_gt

     METHOD compare_string_list_le.
       scheme( code = |(string<=? "Apple" "Cap" "Cap")|
               expected = '#t' ).
       scheme( code = |(string<=? "a" "bsa" "bsa" "bas")|
               expected = '#f' ).
     ENDMETHOD.                    "compare_string_list_le

     METHOD compare_string_list_ge.
       scheme( code = |(string>=? "Tapple" "Mapple" "Apple" "Apple"  )|
               expected = '#t' ).
       scheme( code = |(string>=? "tza" "tzas" "zca")|
               expected = '#f' ).
     ENDMETHOD.                    "compare_string_list_ge

     METHOD compare_string_ci_list_eq.
       scheme( code = |(string-ci=? "Apple" "apple")|
               expected = '#t' ).
       scheme( code = |(string-ci=? "a" "as" "a")|
               expected = '#f' ).
       scheme( code = |(string-ci=? "Mom and Dad" "mom and dad")|
               expected = '#t' ).
       scheme( code = |(string-ci=? "Strasse" "strasse")|
               expected = '#t' ).
     ENDMETHOD.                    "compare_string_ci_list_eq

     METHOD compare_string_ci_list_lt.
       scheme( code = |(string-ci<? "apple" "CAP")|
               expected = '#t' ).
       scheme( code = |(string-ci<? "BAS" "a" "baz")|
               expected = '#f' ).
     ENDMETHOD.                    "compare_string_ci_list_lt

     METHOD compare_string_ci_list_gt.
       scheme( code = |(string-ci>? "Tapple" "MAPPLE" "Apple" )|
               expected = '#t' ).
       scheme( code = |(string-ci>? "tza" "tas" "zca")|
               expected = '#f' ).
       scheme( code = |(string-ci>? "N" "m" "L" "k")|
               expected = '#t' ).
     ENDMETHOD.                    "compare_string_ci_list_gt

     METHOD compare_string_ci_list_le.
       scheme( code = |(string-ci<=? "Apple" "Cap" "CAP")|
               expected = '#t' ).
       scheme( code = |(string-ci<=? "a" "BSA" "bsa" "bas")|
               expected = '#f' ).
       scheme( code = |(string-ci<=? "say what" "Say What!?")|
               expected = '#t' ).
     ENDMETHOD.                    "compare_string_ci_list_le

     METHOD compare_string_ci_list_ge.
       scheme( code = |(string-ci>=? "Tapple" "Mapple" "APPLE" "Apple"  )|
               expected = '#t' ).
       scheme( code = |(string-ci>=? "tza" "tzas" "zca")|
               expected = '#f' ).
     ENDMETHOD.                    "compare_string_ci_list_ge

     METHOD char_alphabetic_1.
       scheme( code = '(char-alphabetic? #\A)'
               expected = '#t' ).
     ENDMETHOD.                    "char_alphabetic_1

     METHOD char_alphabetic_2.
       scheme( code = '(char-alphabetic? #\1)'
               expected = '#f' ).
     ENDMETHOD.                    "char_alphabetic_2

     METHOD char_alphabetic_3.
       scheme( code = '(char-alphabetic? "Not a char")'
               expected = 'Eval: "Not a char" is not a char in char-alphabetic?' ).
     ENDMETHOD.                    "char_alphabetic_3

     METHOD char_alphabetic_4.
       scheme( code = '(char-alphabetic? #\;)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD char_alphabetic_space.
       scheme( code = '(char-alphabetic? #\space)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD char_numeric_1.
       scheme( code = '(char-numeric? #\p)'
               expected = '#f' ).
     ENDMETHOD.                    "char_numeric_1

     METHOD char_numeric_2.
       scheme( code = '(char-numeric? #\1)'
               expected = '#t' ).
     ENDMETHOD.                    "char_numeric_2

     METHOD char_numeric_3.
       scheme( code = '(char-numeric? "Not a char")'
               expected = 'Eval: "Not a char" is not a char in char-numeric?' ).
     ENDMETHOD.                    "char_numeric_3

     METHOD char_unicode_1.
       scheme( code = '(char->integer #\x9E9)'
               expected = '2537' ).
       scheme( code = '\x9E9'
               "expected = invalid_digit( '\' ) ).
               expected = `Eval: Identifier \x9E9 not valid.` ).
     ENDMETHOD.                    "char_unicode_1

     METHOD char_whitespace_1.
       scheme( code = '(char-whitespace? #\1)'
               expected = '#f' ).
     ENDMETHOD.                    "char_whitespace_1

     METHOD char_whitespace_2.
       scheme( code = '(char-whitespace? #\space)'
               expected = '#t' ).
     ENDMETHOD.                    "char_whitespace_2

     METHOD char_whitespace_3.
       scheme( code = '(char-whitespace? "Not a char")'
               expected = 'Eval: "Not a char" is not a char in char-whitespace?' ).
     ENDMETHOD.                    "char_whitespace_3

     METHOD char_upper_case_1.
       scheme( code = '(char-upper-case? #\1)'
               expected = '#f' ).
     ENDMETHOD.                    "char_upper_case_1

     METHOD char_upper_case_2.
       scheme( code = '(char-upper-case? #\C)'
               expected = '#t' ).
     ENDMETHOD.                    "char_upper_case_2

     METHOD char_upper_case_3.
       scheme( code = '(char-upper-case? "Not a char")'
               expected = 'Eval: "Not a char" is not a char in char-upper-case?' ).
     ENDMETHOD.                    "char_upper_case_3

     METHOD char_lower_case_1.
       scheme( code = '(char-lower-case? #\1)'
               expected = '#f' ).
     ENDMETHOD.                    "char_lower_case_1

     METHOD char_lower_case_2.
       scheme( code = '(char-lower-case? #\c)'
               expected = '#t' ).
     ENDMETHOD.                    "char_lower_case_2

     METHOD char_lower_case_3.
       scheme( code = '(char-lower-case? "Not a char")'
               expected = 'Eval: "Not a char" is not a char in char-lower-case?' ).
     ENDMETHOD.                    "char_lower_case_3

     METHOD digit_value_1.
       scheme( code = '(digit-value #\3)'
               expected = '3' ).
     ENDMETHOD.                    "digit_value_1

     METHOD digit_value_2.
       scheme( code = '(digit-value #\x0EA)'
               expected = '#f' ).
     ENDMETHOD.                    "digit_value_2

     METHOD digit_value_3.
       scheme( code = '(digit-value "Not a char")'
               expected = 'Eval: "Not a char" is not a char in digit-value' ).
     ENDMETHOD.                    "digit_value_3

     METHOD digit_value_4.
       scheme( code = '(digit-value #\x0664)'
               expected = '4' ).
       scheme( code = '(digit-value #\x0AE6)'
               expected = '0' ).
     ENDMETHOD.                    "digit_value_4

     METHOD char_to_integer_1.
       scheme( code = '(char->integer #\3)'
               expected = '51' ).
     ENDMETHOD.                    "char_to_integer_1

     METHOD char_to_integer_2.
       scheme( code = '(char->integer #\a)'
               expected = '97' ).
     ENDMETHOD.                    "char_to_integer_2

     METHOD char_to_integer_3.
       scheme( code = '(char->integer #\A)'
               expected = '65' ).
*               expected = '577' ).
*               expected = '262145' ).
     ENDMETHOD.                    "char_to_integer_3

     METHOD integer_to_char_1.
       scheme( code = '(integer->char #\a)'
               expected = 'Eval: "a" is not an integer in integer->char' ).
     ENDMETHOD.                    "integer_to_char_1

     METHOD integer_to_char_2.
       scheme( code = '(char->integer (integer->char 3))'
               expected = '3' ).
     ENDMETHOD.                    "integer_to_char_2

     METHOD char_upcase_1.
       scheme( code = '(char-upcase #\a)'
               expected = '"A"' ).
     ENDMETHOD.                    "char_upcase_1

     METHOD char_downcase_1.
       scheme( code = '(char-downcase #\B)'
               expected = '"b"' ).
     ENDMETHOD.                    "char_downcase_1

   ENDCLASS.                    "ltc_string IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_port DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_port DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS read_char_1 FOR TESTING.
       METHODS read_char_2 FOR TESTING.
       METHODS read_string_1 FOR TESTING.
       METHODS read_string_2 FOR TESTING.
       METHODS input_string_1 FOR TESTING.
       METHODS output_string_1 FOR TESTING.

       METHODS write_1 FOR TESTING.
       METHODS display_1 FOR TESTING.

   ENDCLASS.                    "ltc_port DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_port IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_port IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD read_char_1.
       scheme( code = |(read-char (open-input-string "char"))|
               expected = '"c"' ).
     ENDMETHOD.                    "read_char_1

     METHOD read_char_2.
       scheme( code = |(read-char (open-input-string ""))|
               expected = '#!eof' ).
     ENDMETHOD.                    "read_char_2

     METHOD read_string_1.
       scheme( code = |(read-string 50 (open-input-string ""))|
               expected = '""' ).
     ENDMETHOD.                    "read_string_1

     METHOD read_string_2.
       scheme( code = |(read-string 50 (open-input-string "the string"))|
               expected = '"the string"' ).
     ENDMETHOD.                    "read_string_2

     METHOD input_string_1.
       scheme( code = | (define p (open-input-string "(a . (b . (c . ()))) 34"))|
               expected = 'p' ).
       scheme( code = | (input-port? p)|
               expected = '#t' ).
       scheme( code = | (read p)|
               expected = '(a b c)' ).
       scheme( code = | (read p)|
               expected = '34' ).
       scheme( code = | (eof-object? (peek-char p))|
               expected = '#t' ).
     ENDMETHOD.                    "input_string_1

     METHOD output_string_1.
       scheme( code = |(let ((q (open-output-string))| &
                         |      (x '(a b c)))| &
                         |  (write (car x) q)| &
                         |  (write (cdr x) q)| &
                         |  (get-output-string q))|
               expected = '"a (b c)"' ).
     ENDMETHOD.                    "output_string_1

     METHOD write_1.
       scheme_argument( code = |(write)|
                        operation = 'write' ).
       scheme_incorrect( code = |(write (if (= 1 2)))|
                         operation = 'if' ).
       scheme( code = |(write (if (= 1 2) 5))|
               expected = c_undefined && ` ` && c_undefined ).
     ENDMETHOD.                    "write_1

     METHOD display_1.
       scheme_argument( code = |(display)|
                        operation = 'display' ).
       scheme_incorrect( code = |(display (if (= 1 2)))|
                         operation = 'if' ).
       scheme( code = |(display (if (= 1 2) 5))|
               expected = c_undefined ).  " but output is empty
     ENDMETHOD.                    "display_1

   ENDCLASS.                    "ltc_port IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_conditionals IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_conditionals IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD if_1.
       scheme( code = |(if (> 3 2) 'yes 'no)|
               expected = 'yes' ).
     ENDMETHOD.                    "if_1

     METHOD if_2.
       scheme( code = |(if (> 2 3) 'yes 'no)|
               expected = 'no' ).
     ENDMETHOD.                    "if_2

     METHOD if_3.
       scheme( code = |(if (> 3 2)| &
                         |    (- 3 2)| &
                         |    (+ 3 2))|
               expected = '1' ).
     ENDMETHOD.                    "if_3

     METHOD and_1.
       scheme( code = |(and (= 2 2) (> 2 1))|
               expected = '#t' ).
     ENDMETHOD.                    "and_1

     METHOD and_2.
       scheme( code = |(and (= 2 2) (< 2 1))|
               expected = '#f' ).
     ENDMETHOD.                    "and_2

     METHOD and_3.
       scheme( code = |(and 1 2 'c '(f g)) |
               expected = '(f g)' ).
     ENDMETHOD.                    "and_3

     METHOD and_4.
       scheme( code = |(and)|
               expected = '#t' ).
     ENDMETHOD.                    "and_4

     METHOD or_1.
       scheme( code = |(or (= 2 2) (> 2 1))|
               expected = '#t' ).
     ENDMETHOD.                    "or_1

     METHOD or_2.
       scheme( code = |(or (= 2 2) (< 2 1))|
               expected = '#t' ).
     ENDMETHOD.                    "or_2

     METHOD or_3.
       scheme( code = |(or #f #f #f)|
               expected = '#f' ).
     ENDMETHOD.                    "or_3

     METHOD or_4.
       scheme( code = |(or (memq 'b '(a b c))| &
                         |(/ 3 0))|
               expected = '(b c)' ).
     ENDMETHOD.                    "or_4

     METHOD cond_1.
       scheme( code = |(cond ((> 3 2) 'greater)| &
                         |      ((< 3 2) 'less))|
               expected = 'greater' ).
     ENDMETHOD.                    "cond_1

     METHOD cond_2.
       scheme( code = |(cond ((> 3 3) 'greater)| &
                         |      ((< 3 3) 'less)| &
                         |      (else 'equal))|
               expected = 'equal' ).
     ENDMETHOD.                    "cond_2

     METHOD cond_3.
       scheme( code = |(cond ((assv 'b '((a 1) (b 2))) => cadr)| &
                         |      (else #f))|
               expected = '2' ).
     ENDMETHOD.                    "cond_3

     METHOD cond_4.
       scheme( code = |(cond ('(1 2 3) => cadr)| &
                         |      (else #f))|
               expected = |2| ).
     ENDMETHOD.                    "cond_4

     METHOD cond_5.
       scheme( code = |(cond (#f 'false)| &
                         |      ((cadr '(x y))))|
               expected = |y| ).
     ENDMETHOD.                    "cond_5

     METHOD cond_else.
       scheme( code = |(cond ('(1 2 3) => cadr)| &
                      |      (else #f)|  &
                      |      (#t #f))|
               expected = |Eval: else clause must be last in cond| ).
     ENDMETHOD.

     METHOD case_no_args.
       scheme_incorrect( code = |(case)|  operation = 'case' ).
     ENDMETHOD.                    "case_no_args

     METHOD case_no_clauses.
       scheme_incorrect( code = |(case (* 2 3))|
                         operation = 'case' ).
     ENDMETHOD.                    "case_no_clauses

     METHOD case_1.
       scheme( code = |(case (* 2 3)| &
                         |      ((2 3 5 7) 'prime)| &
                         |      ((1 4 6 8 9) 'composite))|
               expected = 'composite' ).
     ENDMETHOD.                    "case_1

     METHOD case_2.
       scheme( code = |(case (car '(c d))| &
                         |      ((a) 'a)| &
                         |      ((b) 'b))|
               expected = c_lisp_nil ).  " unspecified
     ENDMETHOD.                    "case_2

     METHOD case_3.
       scheme( code = |(case (car '(c d))| &
                         |      ((a e i o u) 'vowel)| &
                         |      ((w y) 'semivowel)| &
                         |      (else => (lambda (x) x)))|
               expected = 'c' ).
     ENDMETHOD.                    "case_3

     METHOD not_1.
       scheme( code = |(not #t)|
               expected = '#f' ).
     ENDMETHOD.                    "not_1

     METHOD not_2.
       scheme( code = |(not 3)|
               expected = '#f' ).
     ENDMETHOD.                    "not_2

     METHOD not_3.
       scheme( code = |(not (list 3))|
               expected = '#f' ).
     ENDMETHOD.                    "not_3

     METHOD not_4.
       scheme( code = |(not #f)|
               expected = '#t' ).
     ENDMETHOD.                    "not_4

     METHOD not_5.
       scheme( code = |(not '())|
               expected = '#f' ).
     ENDMETHOD.                    "not_5

     METHOD not_6.
       scheme( code = |(not (list))|
               expected = '#f' ).
     ENDMETHOD.                    "not_6

     METHOD not_7.
       scheme( code = |(not 'nil)|
               expected = '#f' ).
     ENDMETHOD.                    "not_7

     METHOD not_8.
       scheme( code = |(not (= 2 2))|
               expected = '#f' ).
     ENDMETHOD.                    "not_8

     METHOD when_1.
       scheme( code = |(when (= 1 1.0)| &
                         |(display "1")| &
                         |(display "2"))|
               expected = '1 2 "2"' ).  " prints "12", returns "2"
     ENDMETHOD.                    "when_1

     METHOD unless_1.
       scheme( code = |(unless (= 1 1.0)| &
                         |(display "1")| &
                         |(display "2"))|
               expected = c_lisp_nil ).  " prints nothing
     ENDMETHOD.                    "unless_1

     METHOD unless_2.
       scheme( code = |(unless (= 1 2)| &
                         |(+ 1 2) )|
               expected = '3' ).
     ENDMETHOD.                    "unless_2

   ENDCLASS.                    "ltc_conditionals IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_functional_tests DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_functional_tests DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS combine.
*   COMBINE + ZIP
       METHODS functional_combine_zip FOR TESTING.

       METHODS functional_compose FOR TESTING.

       METHODS functional_fact_accum FOR TESTING.

   ENDCLASS.                    "ltc_functional_tests DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_functional_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_functional_tests IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD combine.
       scheme( code = '(define combine (lambda (f) (lambda (x y) (if (nil? x) (quote ()) (f (list (car x) (car y)) ((combine f) (cdr x) (cdr y)))))))'
               expected = 'combine' ).
     ENDMETHOD.                    "combine

* COMBINE + ZIP
     METHOD functional_combine_zip.
       combine( ).
       scheme( code = '(define zip (combine cons))'
               expected = 'zip' ).
       scheme( code = 'zip'
               expected = '<lambda> (x y)' ).
       scheme( code = '(zip (list 1 2 3 4) (list 5 6 7 8))'
               expected = '((1 5) (2 6) (3 7) (4 8))' ).
     ENDMETHOD.                    "functional_combine_zip

     METHOD functional_compose.
       combine( ).
       scheme( code = '(define compose (lambda (f g) (lambda (x) (f (g x)))))'
               expected = 'compose' ).
       scheme( code = '(define repeat (lambda (f) (compose f f)))'
               expected = 'repeat' ).
       scheme( code = riff_shuffle_code( )
               expected = 'riff-shuffle' ).
       scheme( code = '(riff-shuffle (list 1 2 3 4 5 6 7 8))'
               expected = '(1 5 2 6 3 7 4 8)' ).
       scheme( code = '((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8))'
               expected = '(1 3 5 7 2 4 6 8)' ).
       scheme( code = '(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))'
               expected = '(1 2 3 4 5 6 7 8)' ).
     ENDMETHOD.                    "functional_compose

     METHOD functional_fact_accum.
       scheme( code = '(define (fact x) (define (fact-tail x accum) (if (= x 0) accum (fact-tail (- x 1) (* x accum)))) (fact-tail x 1))'
               expected = 'fact' ).
       scheme( code = '(fact 8)' "FIXME: returns fact-tail
               expected = '40320' ).
     ENDMETHOD.                    "functional_fact_accum

   ENDCLASS.                    "ltc_functional_tests IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_numbers DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_numbers DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS is_even FOR TESTING.
       METHODS is_odd FOR TESTING.
       METHODS is_zero FOR TESTING.
       METHODS is_positive FOR TESTING.
       METHODS is_negative FOR TESTING.

       METHODS is_complex FOR TESTING.
       METHODS is_number FOR TESTING.
       METHODS is_real FOR TESTING.
       METHODS is_real_1 FOR TESTING.
       METHODS is_real_2 FOR TESTING.
       METHODS is_rational FOR TESTING.
       METHODS rationalize FOR TESTING.
       METHODS is_rational_inexact FOR TESTING.
       METHODS is_integer FOR TESTING.

       METHODS rational_inexact FOR TESTING.
       METHODS create_rational FOR TESTING.

       METHODS gcd_1 FOR TESTING.
       METHODS gcd_2 FOR TESTING.
       METHODS lcm_1 FOR TESTING.

       METHODS to_exact_1 FOR TESTING.
       METHODS to_exact_2 FOR TESTING.
       METHODS to_exact_3 FOR TESTING.
       METHODS to_inexact_1 FOR TESTING.

       METHODS finite_1 FOR TESTING.

       METHODS infinite_1 FOR TESTING.
       METHODS infinite_2 FOR TESTING.
       METHODS infinite_3 FOR TESTING.
       METHODS infinite_4 FOR TESTING.

       METHODS nan_1 FOR TESTING.
       METHODS nan_2 FOR TESTING.
       METHODS nan_3 FOR TESTING.
       METHODS nan_4 FOR TESTING.

       METHODS exact_1 FOR TESTING.
       METHODS exact_2 FOR TESTING.
       METHODS exact_3 FOR TESTING.
       METHODS exact_4 FOR TESTING.
       METHODS exact_5 FOR TESTING.
       METHODS exact_6 FOR TESTING.
       METHODS exact_7 FOR TESTING.
       METHODS exact_8 FOR TESTING.
       METHODS exact_9 FOR TESTING.

       METHODS compare_eq FOR TESTING.
       METHODS compare_lt FOR TESTING.
       METHODS compare_gt FOR TESTING.
       METHODS compare_le FOR TESTING.
       METHODS compare_ge FOR TESTING.

       METHODS exact_integer_1 FOR TESTING.
       METHODS exact_integer_2 FOR TESTING.
       METHODS exact_integer_3 FOR TESTING.

       METHODS inexact_1 FOR TESTING.
       METHODS inexact_2 FOR TESTING.
       METHODS inexact_3 FOR TESTING.
       METHODS inexact_4 FOR TESTING.
       METHODS inexact_5 FOR TESTING.

       METHODS binary_1 FOR TESTING.
       METHODS decimal_1 FOR TESTING.
       METHODS octal_1 FOR TESTING.
       METHODS hexadecimal_1 FOR TESTING.
       METHODS radix_exp FOR TESTING.
   ENDCLASS.                    "ltc_numbers DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_numbers IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_numbers IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD is_even.
       scheme( code = '(even? 3)'
               expected = '#f' ).
       scheme( code = '(even? 0)'
               expected = '#t' ).
       scheme( code = '(even? +0.0)'
               expected = '#t' ).
       scheme( code = '(even? +750.0)'
               expected = '#t' ).
       scheme( code = '(even? +75.21)'
               expected = 'Eval: 75.21 invalid number in even?' ).
       scheme( code = '(even? 4/2)'
               expected = '#t' ).
       scheme( code = '(even? 12/5)'
               expected = 'Eval: 12/5 invalid number in even?' ).
       scheme( code = '(even? +inf.0)'
               expected = 'Eval: +inf.0 invalid number in even?' ).
       scheme( code = '(even? +nan.0)'
               expected = 'Eval: +nan.0 invalid number in even?' ).
     ENDMETHOD.                    "is_even

     METHOD is_odd.
       scheme( code = '(odd? 3)'
               expected = '#t' ).
       scheme( code = '(odd? 0)'
               expected = '#f' ).
       scheme( code = '(odd? -1.0)'
               expected = '#t' ).
       scheme( code = '(odd? +7.261)'
               expected = 'Eval: 7.261 invalid number in odd?' ).
       scheme( code = '(odd? -11/5)'
               expected = 'Eval: -11/5 invalid number in odd?' ).
       scheme( code = '(odd? +inf.0)'
               expected = 'Eval: +inf.0 invalid number in odd?' ).
       scheme( code = '(odd? -nan.0)'
               expected = 'Eval: -nan.0 invalid number in odd?' ).
     ENDMETHOD.                    "is_odd

     METHOD is_negative.
       scheme( code = '(negative? 3)'
               expected = '#f' ).
       scheme( code = '(negative? 0)'
               expected = '#f' ).
       scheme( code = '(negative? +inf.0)'
               expected = '#f' ).
       scheme( code = '(negative? -nan.0)'
               expected = '#f' ).
       scheme( code = '(negative? +nan.0)'
               expected = '#f' ).
       scheme( code = '(negative? -inf.0)'
               expected = '#t' ).
       scheme( code = '(negative? 1-i)'
               expected = 'Eval: 1-i complex number not allowed in negative?' ).
       scheme( code = '(negative? "Not a Number")'
               expected = 'Eval: "Not a Number" is not a number in negative?' ).
       scheme( code = '(negative? 0.0)'
               expected = '#f' ).
       scheme( code = '(negative? -1/3)'
               expected = '#t' ).
     ENDMETHOD.                    "is_negative

     METHOD is_zero.
       scheme( code = '(zero? 3)'
               expected = '#f' ).
       scheme( code = '(zero? -0.0)'
               expected = '#t' ).
       scheme( code = '(zero? +0.0)'
               expected = '#t' ).
       scheme( code = '(zero? 0)'
               expected = '#t' ).
       scheme( code = '(zero? 0/2)'
               expected = '#t' ).
       scheme( code = '(zero? -2/2)'
               expected = '#f' ).
       scheme( code = '(zero? 0+1/2i)'
               expected = '#f' ).
       scheme( code = '(zero? 0.0+0.0i)'
               expected = '#t' ).
     ENDMETHOD.

     METHOD is_positive.
       scheme( code = '(positive? 3)'
               expected = '#t' ).
       scheme( code = '(positive? -10)'
               expected = '#f' ).
       scheme( code = '(positive? 0)'
               expected = '#f' ).
       scheme( code = '(positive? 0.0)'
               expected = '#f' ).
       scheme( code = '(positive? +inf.0)'
               expected = '#t' ).
       scheme( code = '(positive? -nan.0)'
               expected = '#f' ).
       scheme( code = '(positive? +nan.0)'
               expected = '#f' ).
       scheme( code = '(positive? -inf.0)'
               expected = '#f' ).
       scheme( code = '(positive? 1-i)'
               expected = 'Eval: 1-i complex number not allowed in positive?' ).
       scheme( code = '(positive? "Not a Number")'
               expected = 'Eval: "Not a Number" is not a number in positive?' ).
     ENDMETHOD.

     METHOD is_complex.
       scheme( code = '(complex? 3+4i)'
               expected = '#t' ).
       scheme( code = '(complex? 2.5+0i)'
               expected = '#t' ).
       scheme( code = '(complex? 3)'
               expected = '#t' ).
       scheme( code = '(complex? 1-i)'
               expected = '#t' ).
       scheme( code = '(complex? 1@1)'
               expected = '#t' ).
     ENDMETHOD.                    "is_complex

     METHOD is_number.
       scheme( code = '(real? #e31l-1)'
               expected = '#t' ).
       scheme( code = '(number? 2.31f0.3)'
               expected = 'Eval: Identifier 2.31f0.3 not valid.' ).
     ENDMETHOD.

     METHOD is_real.
       scheme( code = '(real? 3)'
               expected = '#t' ).
       scheme( code = '(real? 2.5+0i)'
               expected = '#t' ).
       scheme( code = '(real? 2.5+0.0i)'
               expected = '#f' ).
       scheme( code = '(real? 0/0)'
               expected = '#t' ).
       scheme( code = '(real? 1/0)'
               expected = '#t' ).
       scheme( code = '(real? -1/0)'
               expected = '#t' ).
     ENDMETHOD.                    "is_real

     METHOD is_real_1.
       scheme( code = '(real? #e1e10)'
               expected = '#t' ).
       scheme( code = '(real? #e2.31e-2)'
               expected = '#t' ).
       scheme( code = '(real? 2.31f0.3)'
               expected = 'Eval: Identifier 2.31f0.3 not valid.' ).
       scheme( code = '(real? +inf.0)'
               expected = '#t' ).
       scheme( code = '(real? +nan.0)'
               expected = '#t' ).
     ENDMETHOD.                    "is_real_1

     METHOD is_real_2.
       scheme( code = '2.31f0.3'
               expected = 'Eval: Identifier 2.31f0.3 not valid.' ).
     ENDMETHOD.

     METHOD is_rational.
       scheme( code = '(rational? 6/10)'
               expected = '#t' ).
       scheme( code = '(rational? 6/3)'
               expected = '#t' ).
       scheme( code = '(+ (/ 3) (+ 1 (/ 4)))'
               expected = '19/12' ).
       scheme( code = '(rational? 0/0)'
               expected = '#f' ).
       scheme( code = '(rational? 1/0)'
               expected = '#f' ).
       scheme( code = '(rational? -1/0)'
               expected = '#f' ).
     ENDMETHOD.                    "is_rational

     METHOD rational_inexact.
       scheme( code = '24.0/3'
               expected = `Eval: Identifier 24.0/3 not valid.` ).
       scheme( code = '(rational? 24.0/3)'
               expected = `Eval: Identifier 24.0/3 not valid.` ).
     ENDMETHOD.

     METHOD create_rational.
       scheme( code = '-4/3'
               expected = '-4/3' ).
       scheme( code = '-5/0'
               expected = '-inf.0' ).
       scheme( code = '0/0'
               expected = '+nan.0' ).
       scheme( code = '25/0'
               expected = '+inf.0' ).

       scheme( code = '(/ -5 0)'
               expected = '-inf.0' ).
       scheme( code = '(/ 0 0)'
               expected = '+nan.0' ).
       scheme( code = '(/ 25 0)'
               expected = '+inf.0' ).

     ENDMETHOD.

     METHOD rationalize.
       scheme( code = '(rationalize 1/4 1/10)'
               expected = '1/3' ).
       scheme( code = '(rationalize -1/4 1/10)'
               expected = '-1/3' ).
       scheme( code = '(rationalize 1/4 1/4)'
               expected = '0' ).
       scheme( code = '(rationalize 11/40 1/4)'
               expected = '1/2' ).
     ENDMETHOD.

     METHOD is_rational_inexact.
       scheme( code = '(rational? -inf.0)'
               expected = '#f' ).
       scheme( code = '(rational? 3.5)'
               expected = '#t' ).
       scheme( code = '(rational? 3.1416)'
               expected = '#t' ).
       scheme( code = '(rational? 6/10)'
               expected = '#t' ).
       scheme( code = '(rational? 6/3)'
               expected = '#t' ).
     ENDMETHOD.                    "is_rational

     METHOD is_integer.
       scheme( code = '(integer? 3+0i)'
               expected = '#t' ).
       scheme( code = '(integer? 3.0)'
               expected = '#t' ).
       scheme( code = '(integer? 8/4)'
               expected = '#t' ).
       scheme( code = '(integer? 22/7)'
               expected = '#f' ).
     ENDMETHOD.                    "is_integer

     METHOD gcd_1.
       scheme( code = |(gcd)|
               expected = |0| ).
       scheme( code = |(gcd 32 -36)|
               expected = |4| ).
     ENDMETHOD.                    "gcd_1

     METHOD gcd_2.
       scheme( code = |(gcd 32 0)|
               expected = |32| ).
     ENDMETHOD.

     METHOD lcm_1.
       scheme( code = |(lcm)|
               expected = |1| ).
       scheme( code = |(lcm 32 -36)|
               expected = |288| ).
       scheme( code = |(lcm 32.0 -36)|
               expected = |288.0| ).   " inexact
       scheme( code = |(lcm 1/2 2/3)|
               expected = |2| ).
     ENDMETHOD.                    "lcm_1

     METHOD to_exact_1.
       scheme( code = '(exact 53)'
               expected = '53' ).
       scheme( code = '(exact -3+4i)'
               expected = '-3+4i' ).
       scheme( code = '(exact -1/3+4.0i)'
               expected = '-1/3+4i' ).
       scheme( code = '(exact #i-1/3i)'
               expected = '-1/3i' ).
       scheme( code = '(exact 0.5)'
               expected = '1/2' ).
     ENDMETHOD.                    "to_exact_1

     METHOD to_exact_2.
       DATA lv_int TYPE tv_int.
       TRY.
         lv_int = EXACT #( 30000000000 ).
         scheme( code = '(exact 3e10)'
                 expected = '30000000000' ).
         CATCH cx_root.
           scheme( code = '(exact 3e10)'
                   expected = 'Eval: 30000000000.0 no exact representation' ).
       ENDTRY.
     ENDMETHOD.

     METHOD to_exact_3.
       scheme( code = '(define _3 (* 10 (exact .3)))'
               expected = '_3' ).
       scheme( code = '(eq? (* 10.0 (exact .3)) _3)'
               expected = '#t' ).
     ENDMETHOD.

     METHOD to_inexact_1.
       scheme( code = '(inexact 1/2)'
               expected = '0.5' ).
     ENDMETHOD.                    "to_inexact_1

     METHOD finite_1.
       scheme( code = '(finite? 3)'
               expected = '#t' ).
       scheme( code = '(finite? +inf.0)'
               expected = '#f' ).
       scheme( code = '(finite? 3.0+inf.0i)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD infinite_1.
       scheme( code = '(infinite? 3)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD infinite_2.
       scheme( code = '(infinite? +inf.0)'
               expected = '#t' ).
       scheme( code = '(infinite? -inf.0)'
               expected = '#t' ).
     ENDMETHOD.

     METHOD infinite_3.
       scheme( code = '(infinite? +nan.0)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD infinite_4.
       scheme( code = '(infinite? 0-inf.0i)'
               expected = '#t' ).
       scheme( code = '(infinite? 3.0+inf.0i)'
               expected = '#t' ).
     ENDMETHOD.

     METHOD nan_1.
       scheme( code = '(nan? +nan.0)'
               expected = '#t' ).
     ENDMETHOD.

     METHOD nan_2.
       scheme( code = '(nan? 32)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD nan_3.
       scheme( code = '(nan? +nan.0+5.0i)'
               expected = '#t' ).
     ENDMETHOD.

     METHOD nan_4.
       scheme( code = '(nan? 1+2i)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD exact_1.
       scheme( code = '(exact? 22)'
               expected = '#t' ).
     ENDMETHOD.                    "exact_1

     METHOD exact_2.
       scheme( code = '(exact? 1/3)'
               expected = '#t' ).
     ENDMETHOD.                    "exact_2

     METHOD exact_3.
       scheme( code = '(exact? 0.333)'
               expected = '#f' ).
     ENDMETHOD.                    "exact_3

     METHOD exact_4.
       scheme( code = '(exact? 3.0)'
               expected = '#f' ).
     ENDMETHOD.                    "exact_4

     METHOD exact_5.
       scheme( code = '(exact? #e3.0)'
               expected = '#t' ).
     ENDMETHOD.                    "exact_5

     METHOD exact_6.
       scheme( code = '(exact? #i#xff)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD exact_7.
       scheme( code = '(exact? #x#iff)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD exact_8.
       scheme( code = '(eqv? (* #e1.1 #e1.1) 121/100)'
               expected = '#t' ).
     ENDMETHOD.

     METHOD exact_9.
       scheme( code = '#| block comment |# #d#b10'
               expected = 'Parse: Invalid exactness token in number prefix' ).
     ENDMETHOD.                    "exact_5

     METHOD compare_eq.
       scheme_argument( code = '(=)'  operation = '=' ).
       scheme_argument( code = '(= 1)'  operation = '=' ).
       scheme( code = '(= 1 2)'
               expected = '#f' ).
       scheme( code = '(= 1 1)'
               expected = '#t' ).
       scheme( code = '(= 0 -2)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD compare_lt.
       scheme_argument( code = '(<)'  operation = '<' ).
       scheme_argument( code = '(< 1)'  operation = '<' ).
       scheme( code = '(< 1 2)'
               expected = '#t' ).
       scheme( code = '(< 2 2)'
               expected = '#f' ).
       scheme( code = '(< 1 -2)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD compare_gt.
       scheme_argument( code = '(>)'  operation = '>' ).
       scheme_argument( code = '(> 1)' operation = '>' ).
       scheme( code = '(> 1 2)'
               expected = '#f' ).
       scheme( code = '(> 2 2)'
               expected = '#f' ).
       scheme( code = '(> 1 -2)'
               expected = '#t' ).
     ENDMETHOD.

     METHOD compare_le.
       scheme_argument( code = '(<=)'  operation = '<=' ).
       scheme_argument( code = '(<= 1)'  operation = '<=' ).
       scheme( code = '(<= 1 2)'
               expected = '#t' ).
       scheme( code = '(<= 2 2)'
               expected = '#t' ).
       scheme( code = '(<= 1 -2)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD compare_ge.
       scheme_argument( code = '(>=)'  operation = '>=' ).
       scheme_argument( code = '(>= 1)' operation = '>=' ).
       scheme( code = '(>= 1 2)'
               expected = '#f' ).
       scheme( code = '(>= 2 2)'
               expected = '#t' ).
       scheme( code = '(>= 1 -2)'
               expected = '#t' ).
     ENDMETHOD.

     METHOD exact_integer_1.
       scheme( code = '(exact-integer? 32)'
               expected = '#t' ).
     ENDMETHOD.                    "exact_integer_1

     METHOD exact_integer_2.
       scheme( code = '(exact-integer? 32.0)'
               expected = '#f' ).
     ENDMETHOD.                    "exact_integer_2

     METHOD exact_integer_3.
       scheme( code = '(exact-integer? 32/5)'
               expected = '#f' ).
     ENDMETHOD.                    "exact_integer_3

     METHOD inexact_1.
       scheme( code = '(inexact? 0.5)'
               expected = '#t' ).
       scheme( code = '(inexact? 0.0)'
               expected = '#t' ).
       scheme( code = '(inexact? -0.0)'
               expected = '#t' ).
     ENDMETHOD.                    "inexact_1

     METHOD inexact_2.
       scheme( code = '(inexact? (/ 10 3))'
               expected = '#f' ).
     ENDMETHOD.                    "inexact_2

     METHOD inexact_3.
       scheme( code = '(inexact? (/ 10 3.1))'
               expected = '#t' ).
     ENDMETHOD.                    "inexact_3

     METHOD inexact_4.
       scheme( code = '(inexact? 3.)'
               expected = '#t' ).
     ENDMETHOD.                    "inexact_4

     METHOD inexact_5.
       scheme( code = '(eq? 1/3 #i1/3)'
               expected = '#f' ).
     ENDMETHOD.

     METHOD decimal_1.
       scheme( code = '#D23f2'
               expected = '2300.0' ).
     ENDMETHOD.

     METHOD binary_1.
       scheme( code = '#b100101'
               expected = '37' ).
       scheme( code = '(eq? #b101001 #o51)'
               expected = '#t' ).
       scheme( code = '(eq? #b1010111100 #o1274)'
               expected = '#t' ).
       scheme( code = '(eq? #b11100.01001 #o34.22)' " not allowed in R7RS, DrRacket does it anyway
               expected = '#t' ).
     ENDMETHOD.

     METHOD radix_exp.
       scheme( code = '#d31e-3'
               expected = '0.031' ).
       scheme( code = '#d2.41e+2'
               expected = '241.0' ).
       scheme( code = '#b1e10'
               expected = '4.0' ).                  " not allowed in R7RS, DrRacket/ChezScheme do it anyway
     ENDMETHOD.                    "binary_1

     METHOD octal_1.
       scheme( code = '#o175'
               expected = '125' ).
       scheme( code = '#o1604'
               expected = '900' ).
       scheme( code = '#o764'
               expected = '500' ).
       scheme( code = '#o65'
               expected = '53' ).
       scheme( code = '#o0.124'          " not allowed in R7RS, DrRacket does it anyway
               expected = '0.1640625' ).
       scheme( code = '(eq? #o1057 #b001000101111)'
               expected = '#t' ).
       scheme( code = '(eq? #b0011111110100101 #o037645)'
               expected = '#t' ).
     ENDMETHOD.                    "octal_1

     METHOD hexadecimal_1.
       scheme( code = '(eq? #o1057 #x22F)'
               expected = '#t' ).
       scheme( code = '#x1C'
               expected = '28' ).
       scheme( code = '#x1c'
               expected = '28' ).
       scheme( code = '#x1a'
               expected = '26' ).
       scheme( code = '#X1a'
               expected = '26' ).
     ENDMETHOD.                    "hexadecimal_1

   ENDCLASS.                    "ltc_numbers IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_math DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_math DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PUBLIC SECTION.
       METHODS constructor.
     PRIVATE SECTION.

       METHODS math_addition FOR TESTING.
       METHODS math_add_rational FOR TESTING.

       METHODS math_mult_0 FOR TESTING.
       METHODS math_mult_1 FOR TESTING.
       METHODS math_mult_2 FOR TESTING.
       METHODS math_mult_3 FOR TESTING.
       METHODS math_mult_4 FOR TESTING.
       METHODS math_mult_5 FOR TESTING.
       METHODS math_mult_6 FOR TESTING.
       METHODS math_mult_7 FOR TESTING.

       METHODS math_subtract_1 FOR TESTING.
       METHODS math_subtract_2 FOR TESTING.
       METHODS math_subtract_3 FOR TESTING.
       METHODS math_subtract_4 FOR TESTING.
       METHODS math_subtract_5 FOR TESTING.

       METHODS math_division_inverse FOR TESTING.
       METHODS math_division_2 FOR TESTING.
       METHODS math_division_3 FOR TESTING.
       METHODS math_division_4 FOR TESTING.
       METHODS math_division_5 FOR TESTING.
       METHODS math_division_6 FOR TESTING.
       METHODS math_division_7 FOR TESTING.
       METHODS math_division_8 FOR TESTING.
       METHODS math_division_9 FOR TESTING.
       METHODS math_division_10 FOR TESTING.
       METHODS math_division_11 FOR TESTING.

       METHODS math_sin FOR TESTING.
       METHODS math_cos FOR TESTING.
       METHODS math_tan FOR TESTING.
       METHODS math_sinh_1 FOR TESTING.
       METHODS math_cosh_1 FOR TESTING.
       METHODS math_tanh_1 FOR TESTING.

       METHODS math_sinh FOR TESTING.
       METHODS math_cosh FOR TESTING.
       METHODS math_tanh FOR TESTING.
       METHODS math_asinh FOR TESTING.
       METHODS math_acosh FOR TESTING.
       METHODS math_atanh FOR TESTING.
       METHODS math_asin FOR TESTING.
       METHODS math_acos FOR TESTING.
       METHODS math_atan FOR TESTING.
       METHODS math_atan_1 FOR TESTING.
       METHODS math_atan_2 FOR TESTING.

       METHODS math_exp FOR TESTING.
       METHODS math_expt FOR TESTING.
       METHODS math_expt_1 FOR TESTING.
       METHODS math_sqrt FOR TESTING.
       METHODS math_int_sqrt FOR TESTING.
       METHODS math_square FOR TESTING.
       METHODS math_complex FOR TESTING.
       METHODS math_complex_1 FOR TESTING.
       METHODS math_complex_2 FOR TESTING.
       METHODS math_complex_3 FOR TESTING.
       METHODS math_complex_4 FOR TESTING.
       METHODS math_complex_5 FOR TESTING.
       METHODS math_log FOR TESTING.
       METHODS math_log_1 FOR TESTING.
       METHODS math_log_2 FOR TESTING.
       METHODS math_log_3 FOR TESTING.

       METHODS math_floor FOR TESTING.
       METHODS math_floor_new FOR TESTING.
       METHODS math_ceiling FOR TESTING.
       METHODS math_truncate FOR TESTING.
       METHODS math_truncate_new FOR TESTING.
       METHODS math_round FOR TESTING.

       METHODS math_numerator FOR TESTING.
       METHODS math_denominator FOR TESTING.

       METHODS math_remainder FOR TESTING.
       METHODS math_modulo FOR TESTING.
       METHODS math_random FOR TESTING.
       METHODS math_random_invalid FOR TESTING.
       METHODS math_random_too_large FOR TESTING.

       METHODS math_div_test_1 FOR TESTING.

       METHODS math_min_0 FOR TESTING.
       METHODS math_min_1 FOR TESTING.
       METHODS math_min_2 FOR TESTING.
       METHODS math_min_3 FOR TESTING.

       METHODS math_max_0 FOR TESTING.
       METHODS math_max_1 FOR TESTING.
       METHODS math_max_2 FOR TESTING.
       METHODS math_max_3 FOR TESTING.

   ENDCLASS.                    "ltc_math DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_math IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_math IMPLEMENTATION.

     METHOD constructor.
       super->constructor( ).
     ENDMETHOD.

     METHOD math_addition.
       scheme( code = '(+ 22 24 25)'
               expected = '71' ).
*       code_test_f( code = '(inexact (+ 144045379/232792560 1/10))'
*                    expected = '0.7187714031754279432298008149401338' ).
       scheme( code = '(= (inexact (+ 144045379/232792560 1/10)) 0.7187714031754279432298008149401338)'
               expected = '#t' ).
     ENDMETHOD.                    "math_addition

     METHOD math_add_rational.
       scheme( code = '(+ 144045379/232792560 1/10)'
               expected = '33464927/46558512' ).
     ENDMETHOD.

     METHOD math_mult_0.
       scheme( code = '(*)'         " Test multiplication
               expected = '1' ).
     ENDMETHOD.                    "math_mult_0

     METHOD math_mult_1.
       scheme( code = '(* 22)'
               expected = '22' ).
     ENDMETHOD.                    "math_mult_1

     METHOD math_mult_2.
       scheme( code = '(* 11 12)'
               expected = '132' ).
     ENDMETHOD.                    "math_mult_2

     METHOD math_mult_3.
       scheme( code = '(* 11 12 13)'
               expected = '1716' ).
     ENDMETHOD.                    "math_mult_3

     METHOD math_mult_4.
       scheme( code = '(* 0 +inf.0)'
               expected = '+nan.0' ).
       scheme( code = '(* +inf.0 0)'
               expected = '+nan.0' ).

       scheme( code = '(* 0 -inf.0)'
               expected = '-nan.0' ).
       scheme( code = '(* -inf.0 0)'
               expected = '-nan.0' ).

       scheme( code = '(* -inf.0 +0.0)'
               expected = '-nan.0' ).
       scheme( code = '(* +0.0 -inf.0)'
               expected = '-nan.0' ).

       scheme( code = '(* +0.0 +inf.0)'
               expected = '+nan.0' ).
       scheme( code = '(* 0.0 +inf.0)'
               expected = '+nan.0' ).
       scheme( code = '(* +inf.0 0.0)'
               expected = '+nan.0' ).

       scheme( code = '(* -inf.0 -0.0)'
               expected = '+nan.0' ).
       scheme( code = '(* -0.0 -inf.0)'
               expected = '+nan.0' ).

       scheme( code = '(* +inf.0 -0.0)'
               expected = '-nan.0' ).
       scheme( code = '(* -0.0 +inf.0)'
               expected = '-nan.0' ).
     ENDMETHOD.

     METHOD math_mult_5.
       scheme( code = '(* -inf.0 -inf.0)'
               expected = '+inf.0' ).
       scheme( code = '(* -inf.0 +inf.0)'
               expected = '-inf.0' ).
       scheme( code = '(* +inf.0 +inf.0)'
               expected = '+inf.0' ).

       scheme( code = '(* 1/3 -inf.0)'
               expected = '-inf.0' ).
       scheme( code = '(* -1/4 -inf.0)'
               expected = '+inf.0' ).
       scheme( code = '(* -2.133 -inf.0)'
               expected = '+inf.0' ).
       scheme( code = '(* -1/4 +3.25)'
               expected = '-0.8125' ).

       scheme( code = '(* -inf.0 -5/9)'
               expected = '+inf.0' ).
       scheme( code = '(* +inf.0 12)'
               expected = '+inf.0' ).
       scheme( code = '(* +inf.0 -4)'
               expected = '-inf.0' ).
       scheme( code = '(* +inf.0 -492/7)'
               expected = '-inf.0' ).
       scheme( code = '(* +inf.0 -0.2342)'
               expected = '-inf.0' ).

       scheme( code = '(* -1/12 -8)'
               expected = '2/3' ).

       scheme( code = '(* +3 -1/12)'
               expected = '-1/4' ).
     ENDMETHOD.

     METHOD math_mult_6.
       scheme( code = '(* +nan.0 -inf.0)'
               expected = '-nan.0' ).
       scheme( code = '(* -inf.0 +nan.0)'
               expected = '-nan.0' ).
       scheme( code = '(* +nan.0 -nan.0)'
               expected = '-nan.0' ).
       scheme( code = '(* -nan.0 +nan.0)'
               expected = '-nan.0' ).

       scheme( code = '(* +nan.0 +inf.0)'
               expected = '+nan.0' ).
       scheme( code = '(* +inf.0 +nan.0)'
               expected = '+nan.0' ).
       scheme( code = '(* -nan.0 -nan.0)'
               expected = '+nan.0' ).
     ENDMETHOD.

     METHOD math_mult_7.
       scheme( code = '(* +300000000 -1/12)'
               expected = '-25000000' ).

       code_test_f( code = '(* 23344430 8942422)'
                    expected = '208755744409460' ).
*       code_test_f( code = '(* 233444300 89424220)'
*                    expected = '20875574440946000' ).
*
*       code_test_f( code = '(* 34523344430422 -5/9)'
*                    expected = '-19179635794678.89' ).
*       code_test_f( code = '(* -172616722152110/9 -172616722152110/9)'
*                    expected = '367858429216527693703425334.567' ).
     ENDMETHOD.

     METHOD math_subtract_1.
       " one argument: inverse
       scheme( code = '(- 22)'
               expected = '-22' ).
       scheme( code = '(- 6)'
               expected = '-6' ).
     ENDMETHOD.                    "math_subtract_1

     METHOD math_subtract_2.
       scheme( code = '(- 22 23 24)'
               expected = '-25' ).
     ENDMETHOD.                    "math_subtract_2

     METHOD math_subtract_3.
       scheme( code = '(- (- (- (- (- 5 1) 1) 1) 1) 1)'
               expected = '0' ).
     ENDMETHOD.                    "math_subtract_3

     METHOD math_subtract_4.
       scheme_argument( code = '(-)'
                        operation = '-' ).
     ENDMETHOD.                    "math_subtract_4

     METHOD math_subtract_5.
       scheme( code = '(- 0 0 0)'
               expected = '0' ).
     ENDMETHOD.

     METHOD math_division_inverse.
       scheme_argument( code =  '(/)'
                        operation = '/' ).
       scheme( code = '(/ 0)'
               expected = '+inf.0' ).
       scheme( code = '-1/0'
               expected = '-inf.0' ).
       scheme( code = '(/ 2)'         " Only one argument
               expected = '1/2' ).
       scheme( code =  '(/ -10)'
               expected = '-1/10' ).
       scheme( code =  '(/ -2/3)'     " rational
               expected = '-3/2' ).
       scheme( code =  '(/ -0.25)'     " real
               expected = '-4.0' ).
       scheme( code =  '(/ -2/3i)'     " complex
               expected = '+3/2i' ).
     ENDMETHOD.                    "math_division_1

     METHOD math_division_2.
       scheme( code =  '(* 3/2 (/ 10 -2 0.1 1/4 3i))'
               expected = '+100.0i' ).
     ENDMETHOD.                    "math_division_2

     METHOD math_division_3.
       scheme( code =  '(/ 5 10)'
               expected = '1/2' ).
       scheme( code =  '(/ 5 10.0)'
               expected = '0.5' ).
     ENDMETHOD.                    "math_division_3

     METHOD math_division_4.
       scheme( code =  '(/ 11 12 13)'
               expected = '11/156' ).
     ENDMETHOD.                    "math_division_4

     METHOD math_division_5.
       scheme( code =  '(/ 89/11 9 8)'
               expected = '89/792' ).
       scheme( code =  '(/ -1/2 +2/3 -3/5)'
               expected = '5/4' ).
     ENDMETHOD.

     METHOD math_division_6.
       scheme( code = '(/ 1 0)'
               expected = '+inf.0' ).
     ENDMETHOD.

     METHOD math_division_7.
       scheme( code =  '(/ -i/2 1)'
               expected = 'Eval: Symbol -i/2 is unbound' ).
     ENDMETHOD.

     METHOD math_division_8.
       scheme( code = '(/ 0.0 0)'
               expected = '+nan.0' ).
       scheme( code = '(/ #e0 #e0)'
               expected = '+nan.0' ).
     ENDMETHOD.

     METHOD math_division_9.
       scheme( code = '(/ 0 +inf.0)'
               expected = '0' ).
     ENDMETHOD.

     METHOD math_division_10.
       scheme( code = '(/ 0 6 +nan.0)'
               expected = '+nan.0' ).
     ENDMETHOD.

     METHOD math_division_11.
       scheme( code = '(/ +inf.0 -inf.0)'
               expected = '-nan.0' ).
       scheme( code = '(/ -inf.0 +inf.0)'
               expected = '-nan.0' ).
       scheme( code = '(/ +inf.0 -inf.0 2.3 +nan.0)'
               expected = '-nan.0' ).
       scheme( code = '(/ -3 4/5 10)'
               expected = '-3/8' ).
     ENDMETHOD.

     METHOD math_sin.
       scheme_argument( code =  '(sin)'
                        operation = 'sin' ).
       one_parameter( code =  '(sin 0 3)'
                      operation = 'sin' ).
       scheme( code =  '(sin 0)'
               expected = '0' ).
     ENDMETHOD.                    "math_sin

     METHOD math_cos.
       scheme_argument( code =  '(cos)'
                        operation = 'cos' ).
       one_parameter( code =  '(cos 4 3)'
                      operation = 'cos' ).
       scheme( code =  '(cos 0)'
               expected = '1' ).
     ENDMETHOD.                    "math_cos

     METHOD math_tan.
       scheme_argument( code =  '(tan)'
                        operation = 'tan' ).
       one_parameter( code = '(tan 0 3)'
                      operation = 'tan' ).
       scheme( code =  '(tan 0)'
               expected = '0' ).
     ENDMETHOD.                    "math_tan

     METHOD math_sinh.
       scheme_argument( code =  '(sinh)'
                        operation = 'sinh' ).
       one_parameter( code =  '(sinh 0 3)'
                      operation = 'sinh' ).
       scheme( code =  '(sinh 0)'
               expected = '0' ).
     ENDMETHOD.                    "math_sinh

     METHOD math_cosh.
       scheme_argument( code =  '(cosh)'
                        operation = 'cosh' ).
       one_parameter( code =  '(cosh 0 3)'
                      operation = 'cosh' ).
       scheme( code =  '(cosh 0)'
               expected = '1' ).
     ENDMETHOD.                    "math_cosh

     METHOD math_tanh.
       scheme_argument( code =  '(tanh)'
                        operation = 'tanh' ).
       one_parameter( code =  '(tanh 0 3)'
                      operation = 'tanh' ).
       scheme( code =  '(tanh 0)'
               expected = '0' ).
     ENDMETHOD.                    "math_tanh

     METHOD math_sinh_1.
       code_test_f( code =  '(sinh 0.5)'
                 expected = '0.52109530549374736162242562641149' ) ##literal.
     ENDMETHOD.                    "math_sinh_1

     METHOD math_cosh_1.
       code_test_f( code =  '(cosh 1)'
                 expected = '1.5430806348152437784779056207571' ) ##literal.
     ENDMETHOD.                    "math_cosh_1

     METHOD math_tanh_1.
       code_test_f( code =  '(tanh 1)'
                 expected = '0.76159415595576488811945828260479' ) ##literal.
     ENDMETHOD.                    "math_tanh_1

     METHOD math_asinh.
       one_parameter( code =  '(asinh 0 3)'
                      operation = 'asinh' ).
       code_test_f( code =  '(asinh 0)'
                    expected = 0 ).
     ENDMETHOD.                    "math_asinh

     METHOD math_acosh.
       code_test_f( code =  '(acosh 1)'
                 expected = 0 ).
     ENDMETHOD.                    "math_acosh

     METHOD math_atanh.
       code_test_f( code =  '(atanh 0)'
                 expected = 0 ).
     ENDMETHOD.                    "math_atanh

     METHOD math_asin.
       code_test_f( code =  '(asin 1)'
                 expected = '1.5707963267948966192313216916398' ) ##literal.
     ENDMETHOD.                    "math_asin

     METHOD math_acos.
       DATA lv_exp TYPE tv_real.
       code_test_f( code =  '(acos 0)'
                    expected = '1.5707963267948966192313216916398' ) ##literal.
       lv_exp = '1.3770031902399644'.
       code_test_f( code =  '(real-part (acos 1.0+5.0i))'
                    expected = lv_exp ) ##literal.
       lv_exp = '-2.330974653049312448379523056074553'.
       code_test_f( code =  '(imag-part (acos 1.0+5.0i))'
                    expected = lv_exp ) ##literal.
     ENDMETHOD.                    "math_acos

     METHOD math_atan.
       code_test_f( code =  '(atan 1)'
                    expected = '0.78539816339744830961566084581988' ) ##literal.
     ENDMETHOD.                    "math_atan

     METHOD math_atan_1.
       scheme( code =  '(define pi (atan 0 -1))'
               expected = 'pi' ).
       code_test_f( code =  'pi'
                    expected = c_pi ) ##literal.
     ENDMETHOD.                    "math_atan_1

     METHOD math_atan_2.
"       code_test_f( code =  '(atan +inf.0 -inf.0)'
"                    expected = '2.356194490192345' ) ##literal.
     ENDMETHOD.

     METHOD math_exp.
       code_test_f( code =  '(exp 2)'
                    expected = '7.389056098930650227230427460575' ) ##literal.
     ENDMETHOD.                    "math_exp

     METHOD math_expt.
       scheme_argument( code =  '(expt)'
                        operation = 'expt' ).
       scheme_argument( code =  '(expt 2)'
                        operation = 'expt' ).
       parameter_mismatch( code =  '(expt 2 3 4)'
                           operation = 'expt' ).
       scheme( code =  '(expt 2 10)'
               expected = '1024' ).
       code_test_f( code =  '(expt 2 0.5)'
                    expected = '1.4142135623730950488016887242097' ) ##literal.
     ENDMETHOD.                    "math_expt

     METHOD math_expt_1.
       scheme_argument( code =  '(exp)'
                        operation = 'exp' ).
       one_parameter( code =  '(exp 2 10)'
                       operation  = 'exp' ) ##literal.
     ENDMETHOD.                    "math_expt_1

     METHOD math_sqrt.
       scheme_argument( code =  '(sqrt)'
                        operation = 'sqrt' ).
       code_test_f( code =  '(sqrt 2)'
                    expected = '1.4142135623730950488016887242097' ) ##literal.
       scheme( code =  '(sqrt 9)'
               expected = '3' ).
       scheme( code =  '(sqrt -1)'
               expected = '+i' ).
       scheme( code =  '(sqrt -4.0)'
               expected = '+2.0i' ).

     ENDMETHOD.                    "math_sqrt

     METHOD math_int_sqrt.
       scheme_argument( code =  '(exact-integer-sqrt)'
                        operation = 'exact-integer-sqrt' ).
       scheme( code =  '(exact-integer-sqrt 17)'
               expected = '4 1' ).
       scheme( code =  '(exact-integer-sqrt 4)'
               expected = '2 0' ).
       scheme( code =  '(exact-integer-sqrt 5)'
               expected = '2 1' ).
     ENDMETHOD.

     METHOD math_square.
       scheme( code =  '(square 2.0)'
               expected = '4.0' ).
       scheme( code =  '(square 2)'
               expected = '4' ).
       scheme( code =  '(square -i)'
               expected = '-1' ).
       scheme( code =  '(square 1+i)'
               expected = '+2i' ).
       scheme( code =  '(square 42)'
               expected = '1764' ).
     ENDMETHOD.

     METHOD math_complex.
       scheme( code =  '(sqrt -1)'
               expected = '+i' ).
       scheme( code =  '(+ 1 +i)'
               expected = '1+i' ).
       scheme( code =  '(integer? 3+0i)'
               expected = '#t' ).
     ENDMETHOD.

     METHOD math_complex_1.
       scheme( code =  '1/4+6/10i'
               expected = '1/4+3/5i' ).
       scheme( code =  '-inf.0-nan.0i'
               expected = '-inf.0-nan.0i' ).
       scheme( code =  '-inf.0+nan.0i'
               expected = '-inf.0+nan.0i' ).
     ENDMETHOD.

     METHOD math_complex_2.
       scheme( code =  '(make-rectangular 3 4.0)'
               expected = '3+4.0i' ).
       scheme( code =  '(real-part 5.0)'
               expected = '5.0' ).
       scheme( code =  '(magnitude 3+4i)'
               expected = '5' ).
       DATA lv_exp TYPE tv_real VALUE '0.7853981633974483'.
       code_test_f( code = '(angle +inf.0+inf.0i)'
                    expected = lv_exp ).
     ENDMETHOD.

     METHOD math_complex_3.
       scheme( code =  '-i'
               expected = '-i' ).
       scheme( code =  '-1i'
               expected = '-i' ).
       scheme( code =  '+i'
               expected = '+i' ).
       scheme( code =  '+nan.0i'
               expected = '+nan.0i' ).
       scheme( code =  '-nan.0i'
               expected = '-nan.0i' ).
       scheme( code =  '-inf.0'
               expected = '-inf.0' ).
       scheme( code =  '+inf.0i'
               expected = '+inf.0i' ).
       scheme( code =  '-inf.0i'
               expected = '-inf.0i' ).
       scheme( code =  '-1/4+nan.0i'
               expected = '-1/4+nan.0i' ).
       scheme( code =  '-nan.0-inf.0i'
               expected = '-nan.0-inf.0i' ).
       scheme( code =  '+inf.0-i'
               expected = '+inf.0-i' ).
     ENDMETHOD.

     METHOD math_complex_4.
       scheme( code =  '(+ 1+3i 1/4-i)'
               expected = '5/4+2i' ).
       scheme( code =  '(* 1+3i 1/4-i)'
               expected = '13/4-1/4i' ).
       scheme( code =  '(* 1+3i +i)'
               expected = '-3+i' ).
       code_test_f( code = '(real-part (/ -1+2i 2.4+9i))'
                    expected = '0.1798063623789765' ).
       code_test_f( code = '(imag-part (/ -1+2i 2.4+9i))'
                    expected = '0.15905947441217153' ).
       scheme( code =  '(- -inf.0 1-i)'
               expected = '-inf.0+i' ).
       scheme( code =  '(+ 1-inf.0i +nan.0)'
               expected = '+nan.0-inf.0i' ).
     ENDMETHOD.

     METHOD math_complex_5.
       scheme( code =  '(imag-part +nan.0)'
               expected = '0' ).
       scheme( code =  '(real-part +nan.0)'
               expected = '+nan.0' ).
       scheme( code =  '(imag-part +inf.0)'
               expected = '0' ).
       scheme( code =  '(real-part -inf.0i)'
               expected = '0' ).
       scheme( code =  '(angle +nan.0)'
               expected = '+nan.0' ).
       scheme( code =  '(angle +nan.0+inf.0i)'
               expected = '+nan.0' ).
       scheme( code =  '(magnitude +nan.0)'
               expected = '+nan.0' ).

       scheme( code =  '(magnitude +nan.0-nan.0i)'
               expected = '+nan.0' ).
       scheme( code =  '(magnitude +nan.0-inf.0i)'
               expected = '+inf.0' ).
     ENDMETHOD.

     METHOD math_log.
       code_test_f( code =  '(log 7.389056)'
                 expected = '1.999999986611192' ) ##literal.
     ENDMETHOD.                    "math_log

     METHOD math_log_1.
       scheme( code =  '(log 1)'
               expected = '0' ) ##literal.
     ENDMETHOD.

     METHOD math_log_2.
       code_test_f( code = '(log (exp 1))'
                    expected = '1' ) ##literal.
     ENDMETHOD.

     METHOD math_log_3.
       scheme( code =  '(log 100 10)'
               expected = '2.0' ).
       scheme( code =  '(log 8 2)'
               expected = '3.0' ).
       scheme( code =  '(log 5 5)'
               expected = '1.0' ).
     ENDMETHOD.

     METHOD math_floor.
       "(floor x) - This returns the largest integer that is no larger than x.
       scheme( code =  '(floor 7.3890560989306504)'
               expected = '7.0' ).
       scheme( code =  '(floor 3.5)'
               expected = '3.0' ).
       scheme( code =  '(floor 3)'
               expected = '3' ).
     ENDMETHOD.                    "math_floor

     METHOD math_floor_new.
       "Integer division
       scheme( code =  '(floor/ 5 2)'
               expected = '2 1' ).
       scheme( code =  '(floor/ -5 2)'
               expected = '-3 1' ).
       scheme( code =  '(floor/ 5 -2)'
               expected = '-3 -1' ).
       scheme( code =  '(floor/ -5 -2)'
               expected = '2 -1' ).
     ENDMETHOD.

     METHOD math_ceiling.
       "(ceiling x) - This returns the smallest integer that is no smaller than x.
       scheme( code =  '(ceiling 1.4142135623730951)'
               expected = '2.0' ).
       scheme( code =  '(ceiling -4.3)'
               expected = '-4.0' ).
     ENDMETHOD.                    "math_ceiling

     METHOD math_truncate.
       "(truncate x) - returns the integer value closest to x that is no larger than the absolute value of x.
       scheme( code =  '(truncate -2.945)'
               expected = '-2.0' ).
       scheme( code =  '(truncate -4.3)'
               expected = '-4.0' ).
       scheme( code =  '(truncate 3.5)'
               expected = '3.0' ).
       scheme( code =  '(truncate 3)'
               expected = '3' ).
     ENDMETHOD.                    "math_truncate

     METHOD math_truncate_new.
       "Integer division
       scheme( code =  '(truncate/ 5 2)'
               expected = '2 1' ).
       scheme( code =  '(truncate/ -5 2)'
               expected = '-2 -1' ).
       scheme( code =  '(truncate/ 5 -2)'
               expected = '-2 1' ).
       scheme( code =  '(truncate/ -5 -2)'
               expected = '2 -1' ).
       scheme( code =  '(truncate/ -5.0 -2)'
               expected = '2.0 -1.0' ).
     ENDMETHOD.

     METHOD math_round.
       "(round x) -
*   This rounds value of x to the nearest integer as is usual in mathematics.
*   It even works when halfway between values.
       scheme( code =  '(round 7.389056)'
               expected = '7.0' ).
       scheme( code =  '(round 7.789056)'
               expected = '8.0' ).
       scheme( code =  '(round -7.789056)'
               expected = '-8.0' ).
       scheme( code =  '(round -4.3)'
               expected = '-4.0' ).
       scheme( code =  '(round 7/2)'   " exact
               expected = '4' ).
       scheme( code =  '(round 7)'   " exact
               expected = '7' ).
     ENDMETHOD.                    "math_round

     METHOD math_remainder.
       scheme( code =  '(remainder 5 4)'
               expected = '1' ).
       scheme( code =  '(remainder -5 4)'
               expected = '-1' ).
       scheme( code =  '(remainder 5 -4)'
               expected = '1' ).
       scheme( code =  '(remainder -5 -4)'
               expected = '-1' ).
       scheme( code =  '(remainder -17 -9)'
               expected = '-8' ).
     ENDMETHOD.                    "math_remainder

     METHOD math_numerator.
       scheme( code =  '(numerator (/ 6 4))'
               expected = '3' ).
     ENDMETHOD.

     METHOD math_denominator.
       scheme( code =  '(denominator (/ 6 4))'
               expected = '2' ).
       scheme( code =  '(denominator (inexact (/ 6 4)))'
               expected = '2.0' ).
     ENDMETHOD.

     METHOD math_div_test_1.
       scheme( code =  |(define (divtest n1 n2)| &
                          |  (= n1 (+ (* n2 (quotient n1 n2))| &
                          | (remainder n1 n2))))|
               expected = 'divtest' ).
       scheme( code =  '(divtest 238 9)'
               expected = '#t' ).
       scheme( code =  '(divtest -238 9)'
               expected = '#t' ).
       scheme( code =  '(divtest 238 -9)'
               expected = '#t' ).
       scheme( code =  '(divtest -238 -9)'
               expected = '#t' ).
     ENDMETHOD.                    "math_div_test_1

     METHOD math_modulo.
       scheme( code =  '(modulo 5 4)'
               expected = '1' ).
       scheme( code =  '(modulo -5 4)'
               expected = '3' ).
       scheme( code =  '(modulo 5 -4)'
               expected = '-3' ).
       scheme( code =  '(modulo -5 -4)'
               expected = '-1' ).
     ENDMETHOD.                    "math_modulo

     METHOD math_random.
       scheme( code =  '(random 0)'
               expected = '0' ).
       scheme( code =  '(begin (define a (random 1)) (or (= a 0) (= a 1)) )'
               expected = '#t' ).
       one_parameter( code =  '(random -5 4)'
                      operation = 'random' ).
     ENDMETHOD.

     METHOD math_random_invalid.
       DATA lo_rand TYPE REF TO cx_abap_random.
       CREATE OBJECT lo_rand
         EXPORTING
           textid = '68D40B4034D28D24E10000000A114BF5'.

       scheme( code =  '(random -4)'
               expected = |Eval: { lo_rand->get_text( ) }| ). " Invalid interval boundaries
       scheme( code =  '(< (random 10) 11)'
               expected = '#t' ).
     ENDMETHOD.                    "math_modulo

     METHOD math_random_too_large.
       scheme( code =  '(random 100000000000000)'
               expected = |Eval: { NEW cx_sy_conversion_overflow( textid = '5E429A39EE412B43E10000000A11447B'
                                                                  value = '100000000000000' )->get_text( ) }| ). "Overflow converting from &

     ENDMETHOD.                    "math_modulo

     METHOD math_min_0.
       scheme( code =  '(min 0 34)'
               expected = '0' ).
       scheme( code =  '(min 3 4)'
               expected = '3' ).
       scheme( code =  '(min 3.9 4)'
               expected = '3.9' ).
     ENDMETHOD.                    "math_min_0

     METHOD math_min_1.
       scheme( code =  '(min -3 +nan.0 4)'
               expected = '+nan.0' ).
       scheme( code =  '(min -3 +nan.0 -inf.0)'
               expected = '+nan.0' ).
     ENDMETHOD.                    "math_min_1

     METHOD math_min_2.
       scheme_argument( code =  '(min)'
                        operation = 'min' ).
       scheme( code =  '(min 3 4 5 4 5 7)'
               expected = '3' ).
       scheme( code =  '(min 3 4 5 4 5 7 "a")'
               expected = 'Eval: "a" is not a number in min' ).
     ENDMETHOD.                    "math_min_2

     METHOD math_min_3.
       scheme( code =  '(min 0 -2 3.9 4 90)'
               expected = '-2.0' ).
     ENDMETHOD.                    "math_min_3

     METHOD math_max_0.
       scheme( code =  '(max 0 34)'
               expected = '34' ).
       scheme( code =  '(max 3 4)'
               expected = '4' ).
       scheme( code =  '(max 3.9 4)'
               expected = '4.0' ).
     ENDMETHOD.                    "math_max_0

     METHOD math_max_1.
       scheme( code =  '(max -3 +nan.0 4)'
               expected = '+nan.0' ).
       scheme( code =  '(max -3 +nan.0 -inf.0)'
               expected = '+nan.0' ).
       scheme( code =  '(max -3 -nan.0 +inf.0)'
               expected = '-nan.0' ).
     ENDMETHOD.                    "math_max_1

     METHOD math_max_2.
       scheme_argument( code =  '(max)'
                        operation = 'max' ).
       scheme( code =  '(max 3 4 5 4 5 7)'
               expected = '7' ).
       scheme( code =  '(max 3 4 5 4 5 7 "a")'
               expected = 'Eval: "a" is not a number in max' ).
     ENDMETHOD.                    "math_max_2

     METHOD math_max_3.
       scheme( code =  '(max -3 3.9 9 4)'
               expected = '9.0' ).
     ENDMETHOD.                    "math_max_3

   ENDCLASS.                    "ltc_math IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_list DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_list DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS is_list_1 FOR TESTING.
       METHODS is_list_2 FOR TESTING.
       METHODS is_list_3 FOR TESTING.
       METHODS is_list_4 FOR TESTING.
       METHODS is_list_5 FOR TESTING.
       METHODS is_list_6 FOR TESTING.
       METHODS is_list_7 FOR TESTING.
       METHODS is_list_8 FOR TESTING.

       METHODS list_nil_1 FOR TESTING.
       METHODS list_nil_2 FOR TESTING.
       METHODS list_test_1 FOR TESTING.
       METHODS list_test_2 FOR TESTING.
       METHODS list_append_0 FOR TESTING.
       METHODS list_append_1 FOR TESTING.
       METHODS list_append_2 FOR TESTING.
       METHODS list_append_3 FOR TESTING.

       METHODS list_append_4 FOR TESTING.
       METHODS list_append_5 FOR TESTING.
       METHODS list_append_6 FOR TESTING.
       METHODS list_append_7 FOR TESTING.
       METHODS list_append_8 FOR TESTING.
       METHODS list_append_9 FOR TESTING.
       METHODS list_append_10 FOR TESTING.
       METHODS list_append_error FOR TESTING.

       METHODS list_append_arg_0 FOR TESTING.
       METHODS list_append_arg_1 FOR TESTING.
       METHODS list_append_arg_2 FOR TESTING.

       METHODS list_length_0 FOR TESTING.
       METHODS list_length_1 FOR TESTING.
       METHODS list_length_2 FOR TESTING.
       METHODS list_length_3 FOR TESTING.
       METHODS list_length_4 FOR TESTING.
       METHODS list_length_5 FOR TESTING.

       METHODS list_memq_0 FOR TESTING.
       METHODS list_memq_1 FOR TESTING.
       METHODS list_memq_2 FOR TESTING.
       METHODS list_memq_3 FOR TESTING.
       METHODS list_memq_4 FOR TESTING.  " unspecified

       METHODS list_member_1 FOR TESTING.
       METHODS list_member_2 FOR TESTING.
       METHODS list_member_3 FOR TESTING.
       METHODS list_member_4 FOR TESTING.
       METHODS list_member_5 FOR TESTING.

       METHODS list_memv FOR TESTING.

       METHODS list_assq_0 FOR TESTING.
       METHODS list_assq_1 FOR TESTING.
       METHODS list_assq_2 FOR TESTING.
       METHODS list_assq_3 FOR TESTING.
       METHODS list_assq_4 FOR TESTING.

       METHODS list_assv_0 FOR TESTING.
       METHODS list_assoc_0 FOR TESTING.
       METHODS list_assoc_1 FOR TESTING.
       METHODS list_assoc_2 FOR TESTING.
       METHODS list_assoc_3 FOR TESTING.

*   CAR & CDR test
       METHODS list_car_1 FOR TESTING.
       METHODS list_car_2 FOR TESTING.
       METHODS list_car_3 FOR TESTING.
       METHODS list_car_4 FOR TESTING.
       METHODS list_car_5 FOR TESTING.

       METHODS list_cdr_1 FOR TESTING.
       METHODS list_cdr_2 FOR TESTING.
       METHODS list_cdr_3 FOR TESTING.
       METHODS list_cdr_4 FOR TESTING.
       METHODS list_car_car_cdr FOR TESTING.
       METHODS list_car_nil FOR TESTING.
       METHODS list_car_list FOR TESTING.
       METHODS list_cons_two_lists FOR TESTING.
       METHODS list_cons_with_nil FOR TESTING.
       METHODS list_cons_with_list FOR TESTING.
       METHODS list_cons_two_elems FOR TESTING.

       METHODS list_copy_1 FOR TESTING.

       METHODS code_count.
       METHODS list_count_1 FOR TESTING.
       METHODS list_count_2 FOR TESTING.

       METHODS list_reverse_1 FOR TESTING.
       METHODS list_reverse_2 FOR TESTING.

       METHODS list_pair_1 FOR TESTING.
       METHODS list_pair_2 FOR TESTING.
       METHODS list_pair_3 FOR TESTING.
       METHODS list_pair_4 FOR TESTING.

       METHODS list_cons_1 FOR TESTING.
       METHODS list_cons_2 FOR TESTING.
       METHODS list_cons_3 FOR TESTING.
       METHODS list_cons_4 FOR TESTING.
       METHODS list_cons_5 FOR TESTING.

       METHODS list_cons_error_1 FOR TESTING.
       METHODS list_cons_error_2 FOR TESTING.

       METHODS list_make_list FOR TESTING.
       METHODS list_make_list_2 FOR TESTING.
       METHODS list_ref FOR TESTING.
       METHODS list_ref_1 FOR TESTING.
       METHODS list_has FOR TESTING.

       METHODS list_tail FOR TESTING.
       METHODS list_tail_nil FOR TESTING.

       METHODS iota_1 FOR TESTING.
       METHODS iota_2 FOR TESTING.
       METHODS iota_3 FOR TESTING.

       METHODS list_caar_1 FOR TESTING.
       METHODS list_caar_2 FOR TESTING.
       METHODS list_caar_3 FOR TESTING.

       METHODS list_cadr_1 FOR TESTING.
       METHODS list_cadr_2 FOR TESTING.
       METHODS list_cadr_3 FOR TESTING.
       METHODS list_cadr_4 FOR TESTING.

       METHODS list_cdar_1 FOR TESTING.
       METHODS list_cdar_2 FOR TESTING.
       METHODS list_cdar_3 FOR TESTING.
       METHODS list_cdar_4 FOR TESTING.

       METHODS list_cddr_1 FOR TESTING.
       METHODS list_cddr_2 FOR TESTING.
       METHODS list_cddr_3 FOR TESTING.
       METHODS list_cddr_4 FOR TESTING.
       METHODS list_cddr_5 FOR TESTING.

       METHODS list_caaar_1 FOR TESTING.
       METHODS list_cdaaar_1 FOR TESTING.
       METHODS list_caaaar_1 FOR TESTING.
       METHODS list_cddddr_1 FOR TESTING.

       METHODS list_c4xr FOR TESTING.

       METHODS list_shared_1 FOR TESTING.

       METHODS make_string_error   FOR TESTING.
       METHODS make_string_3a      FOR TESTING.
       METHODS make_string_blanks3 FOR TESTING.
       METHODS string_to_list_1   FOR TESTING.
       METHODS string_to_list_2   FOR TESTING.
       METHODS string_to_list_3   FOR TESTING.
       METHODS list_to_string_1   FOR TESTING.

       METHODS string_to_number_empty FOR TESTING.
       METHODS string_to_number_0 FOR TESTING.
       METHODS string_to_number_1 FOR TESTING.
       METHODS string_to_number_2 FOR TESTING.
       METHODS string_to_number_3 FOR TESTING.
       METHODS string_to_number_4 FOR TESTING.
       METHODS string_to_number_5 FOR TESTING.
       METHODS string_to_num_radix FOR TESTING.
       METHODS string_to_num_radix_error FOR TESTING.
       METHODS number_to_string_1 FOR TESTING.
       METHODS number_to_string_2 FOR TESTING.
       METHODS string_append_0    FOR TESTING.
       METHODS string_append_1    FOR TESTING.

   ENDCLASS.                    "ltc_list DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_list IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_list IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD is_list_1.
       scheme( code = |(list? '())|
               expected = '#t' ).
     ENDMETHOD.                    "is_list_1

     METHOD is_list_2.
       scheme( code = |(list? '(1))|
               expected = '#t' ).
     ENDMETHOD.                    "is_list_2

     METHOD is_list_3.
       scheme( code = |(list? 1)|
               expected = '#f' ).
     ENDMETHOD.                    "is_list_3

     METHOD is_list_4.
       scheme( code = |(list? '(a b c))|
               expected = '#t' ).
     ENDMETHOD.                    "is_list_4

     METHOD is_list_5.
       scheme( code = |(define x (append '(1 2) 3))|
               expected = 'x' ).
       scheme( code = '(list? x)'
               expected = '#f' ).
     ENDMETHOD.                    "is_list_5

     METHOD is_list_6.
       scheme( code = |(list? (cons 'a 'b))|
               expected = '#f' ).
     ENDMETHOD.                    "is_list_6

     METHOD is_list_7.
       scheme( code = |(list? '(a . b))|
               expected = '#f' ).
     ENDMETHOD.                    "is_list_7

     METHOD is_list_8.
*      By de#nition, all lists have #nite length and are terminated by the empty list.
*      A circular list is not a list
       scheme( code = |(let ((x (list 'a)))| &
                      |   (set-cdr! x x)| &
                      | (list? x))|
               expected = '#f' ).
     ENDMETHOD.                    "is_list_8

     METHOD list_nil_1.
*      changed to allow nil without quote
       scheme( code = '(list ())'
               expected = |({ c_lisp_nil })| ).
     ENDMETHOD.                    "list_nil_1

     METHOD list_nil_2.
       scheme( code = '(list nil)'
               expected = |({ c_lisp_nil })| ).
     ENDMETHOD.                    "list_nil_2

     METHOD list_test_1.
*   Test list
       scheme( code = '(list 22 23 24)'
               expected = '(22 23 24)' ).
     ENDMETHOD.                    "list_test_1

     METHOD list_test_2.
       scheme( code = '(list 22 (list 23 24))'
               expected = '(22 (23 24))' ).
     ENDMETHOD.                    "list_test_2

     METHOD list_append_0.
       scheme( code = |(append '(x) '(y))|
               expected = '(x y)' ).
     ENDMETHOD.                    "list_append_0

     METHOD list_append_1.
*   Test append
       scheme( code = '(append (list 22 (list 23 24)) 23)'
               expected = '(22 (23 24) . 23)' ).
     ENDMETHOD.                    "list_append_1

     METHOD list_append_2.
       scheme( code = |(append '(1 3) '(4 6) '(9 12) '(56 90 91))|
               expected = '(1 3 4 6 9 12 56 90 91)' ).
     ENDMETHOD.                    "list_append_2

     METHOD list_append_3.
       scheme( code = '(append (list 1) (list 2))'
               expected = '(1 2)' ).
     ENDMETHOD.                    "list_append_3

     METHOD list_append_4.
       scheme( code = '(append 5 (list 22 23))'
               expected = 'Eval: append: 5 is not a proper list' ).
     ENDMETHOD.                    "list_append_4

     METHOD list_append_5.
       scheme( code = |(append (cons 5 6) (list 22 23))|
               expected = 'Eval: append: (5 . 6) is not a proper list' ).
     ENDMETHOD.                    "list_append_5

     METHOD list_append_6.
       scheme( code = '(append (list 22 23) 4)'
               expected = '(22 23 . 4)' ).
     ENDMETHOD.                    "list_append_6

     METHOD list_append_7.
       scheme( code = |(append '() 'a)|
               expected = 'a' ).
     ENDMETHOD.                    "list_append_7

     METHOD list_append_8.
       scheme( code = |(append '(a) '(b c d))|
               expected = '(a b c d)' ).
     ENDMETHOD.                    "list_append_8

     METHOD list_append_9.
       scheme( code = |(append '(a (b)) '((c)))|
               expected = '(a (b) (c))' ).
     ENDMETHOD.                    "list_append_9

     METHOD list_append_10.
       scheme( code = |(append '(a b) (cons 'c  'd))|
               expected = '(a b c . d)' ).
     ENDMETHOD.                    "list_append_10

     METHOD list_append_error.
       scheme( code = '(append (append (list 22 (list 23 24)) 23) 28)'  "Should give an error
               expected = 'Eval: append: (22 (23 24) . 23) is not a proper list' ).
     ENDMETHOD.                    "list_append_error

     METHOD list_append_arg_0.
       scheme( code = '(append)'
               expected = c_lisp_nil ).
     ENDMETHOD.                    "list_append_arg_0

     METHOD list_append_arg_1.
       scheme( code = '(append 3)'
               expected = '3' ).
     ENDMETHOD.                    "list_append_arg_1

     METHOD list_append_arg_2.
       scheme( code = |(append '(3))|
               expected = '(3)' ).
     ENDMETHOD.                    "list_append_arg_2

     METHOD list_length_0.
*   Test length
       scheme( code = '(length nil)'
               expected = '0' ).
     ENDMETHOD.                    "list_length_0

     METHOD list_length_1.
*   Test length
       scheme( code = '(length (list 21 22 23 24))'
               expected = '4' ).
     ENDMETHOD.                    "list_length_1

     METHOD list_length_2.
       scheme( code = '(length (list 22 (list 23 24)))'
               expected = '2' ).
     ENDMETHOD.                    "list_length_2

     METHOD list_length_3.
       scheme( code = |(length '())|
               expected = '0' ).
     ENDMETHOD.                    "list_length_0

     METHOD list_length_4.
       scheme( code = |(length '(a b c))|
               expected = '3' ).
     ENDMETHOD.                    "list_length_1

     METHOD list_length_5.
       scheme( code = |(length '(a (b) (c d e)))|
               expected = '3' ).
     ENDMETHOD.                    "list_length_5

     METHOD list_memq_0.
       scheme( code = |(memq 'a '(a b c))|
               expected = '(a b c)' ).
     ENDMETHOD.                    "list_memq_0

     METHOD list_memq_1.
       scheme( code = |(memq 'b '(a b c))|
               expected = '(b c)' ).
     ENDMETHOD.                    "list_memq_1

     METHOD list_memq_2.
       scheme( code = |(memq 'a '(b c d))|
               expected = '#f' ).
     ENDMETHOD.                    "list_memq_2

     METHOD list_memq_3.
       scheme( code = |(memq (list 'a) '(b (a) c))|
               expected = '#f' ).
     ENDMETHOD.                    "list_memq_3

     METHOD list_member_1.
       scheme( code = |(member (list 'a)| &
                         |        '(b (a) c))|
               expected = '((a) c)' ).
     ENDMETHOD.                    "list_member_1

     METHOD list_member_2.
       scheme( code = |(define (comp? a b) (eq? a b))|
               expected = 'comp?' ).
       scheme( code = |(member 2 (list 1 2 3 4) comp?)|
               expected = '(2 3 4)' ).
     ENDMETHOD.                    "list_member_2

     METHOD list_member_3.
*      This is the normal behavior in other Scheme
       scheme( code = |(member 7 '((1 3) (2 5) (3 7) (4 8)) (lambda (x y) (= x (cadr y))))|
               expected = '((3 7) (4 8))' ).
     ENDMETHOD.                    "list_member_3

     METHOD list_member_4.
       scheme( code = |(member 7 '(1 2 3 4 5 6) (lambda (y z) (> z 3)) )|
               expected = '(4 5 6)' ).
     ENDMETHOD.                    "list_member_4

     METHOD list_member_5.
       scheme( code = |(member 2 (list 1 2 3 4) (lambda (x y) (= x y)) )|
               expected = '(2 3 4)' ).
     ENDMETHOD.                    "list_member_5

     METHOD list_memq_4.
       scheme( code = |(memq 101 '(100 101 102))|
               expected = '(101 102)' ).  " unspecified!!
     ENDMETHOD.                    "list_memq_4

     METHOD list_memv.
       scheme( code = |(memv 101 '(100 101 102))|
               expected = '(101 102)' ).
     ENDMETHOD.                    "list_memv

     METHOD list_assq_0.
       scheme( code = |(define e '((a 1) (b 2) (c 3)))|
               expected = 'e' ).
       scheme( code = |(assq 'a e)|
               expected = '(a 1)' ).
     ENDMETHOD.                    "list_assq_0

     METHOD list_assq_1.
       scheme( code = |(define e '((a 1) (b 2) (c 3)))|
               expected = 'e' ).
       scheme( code = |(assq 'b e)|
               expected = '(b 2)' ).
     ENDMETHOD.                    "list_assq_1

     METHOD list_assq_2.
       scheme( code = |(define e '((a 1) (b 2) (c 3)))|
               expected = 'e' ).
       scheme( code = |(assq 'd e)|
               expected = '#f' ).
     ENDMETHOD.                    "list_assq_2

     METHOD list_assq_3.
       scheme( code = |(assq (list 'a) '(((a)) ((b)) ((c))))|
               expected = '#f' ).
     ENDMETHOD.                    "list_assq_3

     METHOD list_assq_4.
       scheme( code = |(assq 5 '((2 3) (5 7) (11 13)))|
               expected = '(5 7)' ).   " unspecified
     ENDMETHOD.                    "list_assq_4

     METHOD list_assv_0.
       scheme( code = |(assv 5 '((2 3) (5 7) (11 13)))|
               expected = '(5 7)' ).
     ENDMETHOD.                    "list_assv_0

     METHOD list_assoc_0.
       scheme( code = |(assoc 11 '((2 3) (5 7) (11 13)))|
               expected = '(11 13)' ).
     ENDMETHOD.                    "list_assoc_0

     METHOD list_assoc_1.
       scheme( code = |(assoc (list 'a) '(((a)) ((b)) ((c))))|
               expected = '((a))' ).
     ENDMETHOD.                    "list_assoc_1

     METHOD list_assoc_2.
       scheme( code = |(assoc 2.0 '((1 1) (2 4) (3 9)))|
               expected = '#f' ).
     ENDMETHOD.                    "list_assoc_2

     METHOD list_assoc_3.
       scheme( code = |(assoc 2.0 '((1 1) (2 4) (3 9)) =)|
               expected = '(2 4)' ).
     ENDMETHOD.                    "list_assoc_3

* CAR & CDR test
     METHOD list_car_1.
*   Test append
       scheme( code = '(car (list 22 (list 23 24)))'
               expected = '22' ).
     ENDMETHOD.                    "list_car_1

     METHOD list_car_2.
       scheme( code = '(car ''(a b c))'
               expected = 'a' ).
     ENDMETHOD.                    "list_car_2

     METHOD list_car_3.
       scheme( code = |(car '((a) b c d))|
               expected = '(a)' ).
     ENDMETHOD.                    "list_car_3

     METHOD list_car_4.
       scheme( code = |(car '(1 . 2))|
               expected = '1' ).
     ENDMETHOD.                    "list_car_4

     METHOD list_car_5.
       scheme( code = '(car ''())'
               expected = |Eval: car: { c_lisp_nil } is not a pair| ).
     ENDMETHOD.                    "list_car_5

     METHOD list_cdr_1.
       scheme( code = '(cdr (list 22 (list 23 24)))'
               expected = '((23 24))' ).
     ENDMETHOD.                    "list_cdr_1

     METHOD list_cdr_2.
       scheme( code = |(cdr '((a) b c d))|
               expected = '(b c d)' ).
     ENDMETHOD.                    "list_cdr_1

     METHOD list_cdr_3.
       scheme( code = |(cdr '(1 . 2))|
               expected = '2' ).
     ENDMETHOD.                    "list_cdr_1

     METHOD list_cdr_4.
       scheme( code = |(cdr '())|
               expected = |Eval: cdr: { c_lisp_nil } is not a pair| ).
     ENDMETHOD.                    "list_cdr_1

     METHOD list_car_car_cdr.
       scheme( code = '(car (car (cdr (list 22 (list 23 24)))))'
               expected = '23' ).
     ENDMETHOD.                    "list_car_car_cdr

     METHOD list_car_nil.
       scheme( code = '(car nil)'
               expected = |Eval: car: { c_lisp_nil } is not a pair| ).
     ENDMETHOD.                    "list_car_nil

     METHOD list_car_list.
       scheme( code = '(car (list 1))'
               expected = '1' ).
     ENDMETHOD.                    "list_car_list

     METHOD list_caar_1.
       scheme( code = |(caar '(1  2))|
               expected = 'Eval: caar: 1 is not a pair' ).
     ENDMETHOD.                    "list_caar_1

     METHOD list_caar_2.
       scheme( code = |(caar '())|
               expected = |Eval: caar: { c_lisp_nil } is not a pair| ).
     ENDMETHOD.                    "list_caar_2

     METHOD list_caar_3.
       scheme( code = |(caar '((1 2)  2))|
               expected = '1' ).
     ENDMETHOD.                    "list_caar_3

     METHOD list_cadr_1.
       scheme( code = |(cadr '())|
               expected = |Eval: cadr: { c_lisp_nil } is not a pair| ).
     ENDMETHOD.                    "list_cadr_1

     METHOD list_cadr_2.
       scheme( code = |(cadr '(1 2))|
               expected = '2' ).
     ENDMETHOD.                    "list_cadr_2

     METHOD list_cadr_3.
       scheme( code = |(cadr '(1 (2 7)))|
               expected = '(2 7)' ).
     ENDMETHOD.                    "list_cadr_3

     METHOD list_cadr_4.
       scheme( code = |(cadr '((1)))|
               expected = |Eval: cadr: { c_lisp_nil } is not a pair| ).
     ENDMETHOD.                    "list_cadr_4

     METHOD list_cdar_1.
       scheme( code = |(cdar '(1  2))|
               expected = 'Eval: cdar: 1 is not a pair' ).
     ENDMETHOD.                    "list_cdar_1

     METHOD list_cdar_2.
       scheme( code = |(cdar '())|
               expected = |Eval: cdar: { c_lisp_nil } is not a pair| ).
     ENDMETHOD.                    "list_cdar_2

     METHOD list_cdar_3.
       scheme( code = |(cdar '((b c)  2))|
               expected = '(c)' ).
     ENDMETHOD.                    "list_cdar_3

     METHOD list_cdar_4.
       scheme( code = |(cdar '((c) 2))|
               expected = c_lisp_nil ).
     ENDMETHOD.                    "list_cdar_4

     METHOD list_cddr_1.
       scheme( code = |(cddr '())|
               expected = |Eval: cddr: { c_lisp_nil } is not a pair| ).
     ENDMETHOD.                    "list_cddr_1

     METHOD list_cddr_2.
       scheme( code = |(cddr '(1  2))|
               expected = c_lisp_nil ).
     ENDMETHOD.                    "list_cddr_2

     METHOD list_cddr_3.
       scheme( code = |(cddr '(1 (2 6)))|
               expected = c_lisp_nil ).
     ENDMETHOD.                    "list_cddr_3

     METHOD list_cddr_4.
       scheme( code = |(cddr '(1 (2)))|
               expected = c_lisp_nil ).
     ENDMETHOD.                    "list_cddr_4

     METHOD list_cddr_5.
       scheme( code = |(cddr '(1 2 6))|
               expected = '(6)' ).
     ENDMETHOD.                    "list_cddr_5

     METHOD list_caaaar_1.
       scheme( code = |(caaaar '((( ( 1 ) 8 ) 2 ) 6))|
               expected = '1' ).
     ENDMETHOD.

     METHOD list_caaar_1.
       scheme( code = |(caaar '( ( ( 5 ) ) 2 6))|
               expected = '5' ).
     ENDMETHOD.

     METHOD list_cdaaar_1.
       scheme( code = |(cdaaar '( ( ( ( 0 6 ) 7 ) 8 ) 1 2 6))|
               expected = '(6)' ).
     ENDMETHOD.

     METHOD list_c4xr.
       CONSTANTS c_test_list TYPE string VALUE '( ( ( ( 0 6 ) 7 ) 8 ) 1 2 6)'.
       scheme( code = |(cddaar '{ c_test_list })|
               expected = c_lisp_nil ).
       scheme( code = |(cddadr '{ c_test_list })|
               expected = |Eval: cddadr: 1 is not a pair| ).
       scheme( code = |(cdddar '{ c_test_list })|
               expected = |Eval: cdddar: { c_lisp_nil } is not a pair| ).
     ENDMETHOD.

     METHOD list_cddddr_1.
       scheme( code = |(cddddr '(1 2 6 ( ( ( ( 0 6 ) 7 ) ( ( 8 ) ) ) ) ))|
               expected = c_lisp_nil ).
     ENDMETHOD.

     METHOD list_shared_1.
       scheme( code = |(let ((x (list 'a 'b 'c)))| &
                         |  (set-cdr! (cddr x) x)| &
                         |  x)|
               expected = '#0 = (a b c . #0#)' ).
     ENDMETHOD.                    "list_shared_1

     METHOD list_cons_two_lists.
*   Test CONS
       scheme( code = '(cons (list 1 2) (list 3 4))'
               expected = '((1 2) 3 4)' ).
     ENDMETHOD.                    "list_cons_two_lists

     METHOD list_cons_with_nil.
       scheme( code = '(cons 1 nil)'
               expected = '(1)' ).
     ENDMETHOD.                    "list_cons_with_nil

     METHOD list_cons_with_list.
       scheme( code = '(cons 2 (list 3 4))'
               expected = '(2 3 4)' ).
     ENDMETHOD.                    "list_cons_with_list

     METHOD list_cons_two_elems.
       scheme( code = '(cons 2 3)'
               expected = '(2 . 3)' ).
     ENDMETHOD.                    "list_cons_two_elems

     METHOD list_copy_1.
       scheme( code = |(define a '(1 8 2 8)) ; a may be immutable|
               expected = 'a' ).
       scheme( code = |(define b (list-copy a))|
               expected = 'b' ).
       scheme( code = |(set-car! b 3) ; b is mutable|
               expected = c_lisp_nil ).
       scheme( code = |a|
               expected = '(1 8 2 8)' ).
       scheme( code = |b|
               expected = '(3 8 2 8)' ).
     ENDMETHOD.                    "list_copy_1

     METHOD code_count.
       scheme( code = |(define first car)|
               expected = 'first' ).
       scheme( code = |(define rest cdr)|
               expected = 'rest' ).
       scheme( code = |(define (count item L)             | &
                         |  (if (nil? L) 0                   | &
                         |     (+ (if (equal? item (first L)) 1 0)   | &
                         |        (count item (rest L)) )  ))|
               expected = 'count' ).
     ENDMETHOD.                    "code_count

     METHOD list_count_1.
       code_count( ).
       scheme( code = |(count 0 (list 0 1 2 3 0 0))|
               expected = '3' ).
     ENDMETHOD.                    "list_count_1

     METHOD list_count_2.
       code_count( ).
       scheme( code = |(count (quote the) (quote (the more the merrier the bigger the better)))|
               expected = '4' ).
     ENDMETHOD.                    "list_count_2

     METHOD list_reverse_1.
       scheme( code = |(reverse '(a b c))|
               expected = '(c b a)' ).
     ENDMETHOD.                    "list_reverse_1

     METHOD list_reverse_2.
       scheme( code = |(reverse '(a (b c) d (e (f))))|
               expected = '((e (f)) d (b c) a)' ).
     ENDMETHOD.                    "list_reverse_2

     METHOD list_pair_1.
       scheme( code = |(pair? '(a . b))|
               expected = '#t' ).
     ENDMETHOD.                    "list_pair_1

     METHOD list_pair_2.
       scheme( code = |(pair? '(a b c))|
               expected = '#t' ).
     ENDMETHOD.                    "list_pair_2

     METHOD list_pair_3.
       scheme( code = |(pair? '())|
               expected = '#f' ).
     ENDMETHOD.                    "list_pair_3

     METHOD list_pair_4.
       scheme( code = |(pair? '#(a b))|
               expected = '#f' ).
     ENDMETHOD.                    "list_pair_4

     METHOD list_cons_1.
       scheme( code = |(cons 'a '())|
               expected = '(a)' ).
     ENDMETHOD.                    "list_cons_1

     METHOD list_cons_2.
       scheme( code = |(cons '(a) '(b c d))|
               expected = '((a) b c d)' ).
     ENDMETHOD.                    "list_cons_2

     METHOD list_cons_3.
       scheme( code = |(cons "a" '(b c))|
               expected = '("a" b c)' ).
     ENDMETHOD.                    "list_cons_3

     METHOD list_cons_4.
       scheme( code = |(cons 'a 3)|
               expected = '(a . 3)' ).
     ENDMETHOD.                    "list_cons_4

     METHOD list_cons_5.
       scheme( code = |(cons '(a b) 'c)|
               expected = '((a b) . c)' ).
     ENDMETHOD.                    "list_cons_5

     METHOD list_cons_error_1.
       scheme_argument( code = |(cons)|
                        operation = 'cons' ).
     ENDMETHOD.

     METHOD list_cons_error_2.
       scheme_argument( code = |(cons 'a)|
                        operation = 'cons' ).
       parameter_mismatch( code = |(cons 'a 'b 'c)|
                           operation = `cons` ).
     ENDMETHOD.

     METHOD list_make_list.
       scheme( code = '(make-list 5)'
               expected = |({ c_lisp_nil } { c_lisp_nil } { c_lisp_nil } { c_lisp_nil } { c_lisp_nil })| ).
     ENDMETHOD.                    "list_make_list

     METHOD list_make_list_2.
       scheme( code = '(make-list 3 2)'
               expected = '(2 2 2)' ).
     ENDMETHOD.                    "list_make_list_2

     METHOD list_tail.
       scheme( code = |(list-tail '(a b c d) 2)|
               expected = '(c d)' ).
     ENDMETHOD.                    "list_tail

     METHOD list_tail_nil.
       scheme( code = |(list-tail '() 0)|
               expected = |'()| ).
     ENDMETHOD.                    "list_tail_nil

     METHOD iota_1.
       scheme( code = |(iota 3)|
               expected = '(0 1 2)' ).
     ENDMETHOD.                    "iota_1

     METHOD iota_2.
       scheme( code = |(iota 5 2)|
               expected = '(2 3 4 5 6)' ).
     ENDMETHOD.                    "iota_2

     METHOD iota_3.
       scheme( code = |(iota 4 2 -1)|
               expected = '(2 1 0 -1)' ).
     ENDMETHOD.                    "iota_3

     METHOD list_ref.
       scheme( code = |(list-ref '(40 30 11 9) 1)|
               expected = '30' ).
     ENDMETHOD.                    "list_ref

     METHOD list_ref_1.
       scheme( code = |(list-ref '(a b c d) 2)|
               expected = 'c' ).
     ENDMETHOD.                    "list_ref_1

     METHOD list_has.
       scheme( code = |(memq 2 '(4 3 762 2))|
               expected = '(2)' ).
     ENDMETHOD.                    "list_has

     METHOD make_string_error.
       scheme( code = |(make-string 3 "a")|
               expected = 'Eval: "a" is not a char in make-string' ).
       scheme( code = `(list->string '(#\alarm #\null #\backspace #\return #\newline #\tab #\escape))`
               expected = '"\a\x00\b\r\n\t\x1B"' ).
       scheme( code = `"#\alarm #\null #\backspace a \u"`
               expected = 'Parse: invalid escape char u found' ).
     ENDMETHOD.                    "make_string_error

     METHOD make_string_3a.
       scheme( code = '(make-string 3 #\a)'
               expected = '"aaa"' ).
     ENDMETHOD.                    "make_string_3a

     METHOD make_string_blanks3.
       scheme( code = '(make-string 3 #\space)'
               expected = '"   "' ).
     ENDMETHOD.                    "make_string_blanks3

     METHOD string_to_list_1.
       scheme( code = |(string->list "Aali")|
               expected = '("A" "a" "l" "i")' ).
     ENDMETHOD.                    "string_to_list_1

     METHOD string_to_list_2.
       scheme( code = |(string->list "Aali" 1)|
               expected = '("a" "l" "i")' ).
     ENDMETHOD.                    "string_to_list_2

     METHOD string_to_list_3.
       scheme( code = |(string->list "Aali" 2 3)|
               expected = '("l")' ).
     ENDMETHOD.                    "string_to_list_3

     METHOD list_to_string_1.
       scheme( code = '(list->string `( #\A #\a #\l #\i ))'
               expected = '"Aali"' ).
     ENDMETHOD.                    "list_to_string_1

     METHOD string_to_number_empty.
       scheme( code = |(string->number "")|
               expected = '#f' ).
     ENDMETHOD.

     METHOD string_to_number_0.
       scheme( code = |(string->number "Am I a hot number?")|
               expected = '#f' ).
     ENDMETHOD.

     METHOD string_to_number_1.
       scheme( code = |(string->number '( 13 ))|
               expected = 'Eval: (13) is not a string in string->number' ).
     ENDMETHOD.                    "string_to_number_1

     METHOD string_to_number_2.
       scheme( code = |(string->number "#e42e+0")|
               expected = '42' ).
     ENDMETHOD.                    "string_to_number_2

     METHOD string_to_number_3.
       scheme( code = |(string->number "1e2")|
               expected = '100.0' ).
     ENDMETHOD.                    "string_to_number_3

     METHOD string_to_number_4.
       scheme( code = |(string->number "1a2")|
               expected = '#f' ).
     ENDMETHOD.                    "string_to_number_4

     METHOD string_to_number_5.
       scheme( code = |(string->number "3.0+2.5i")|
               expected = '3.0+2.5i' ).
     ENDMETHOD.

     METHOD string_to_num_radix.
       scheme( code = |(string->number "100" 16)|
               expected = '256' ).
     ENDMETHOD.                    "string_to_num_radix

     METHOD string_to_num_radix_error.
       scheme( code = |(string->number "100" 12)|
               expected = 'Radix: Invalid radix (12) must be 2, 8, 10 or 16 in string->number' ).
     ENDMETHOD.                    "string_to_num_radix_error

     METHOD number_to_string_1.
       scheme( code = |(number->string '21)|
               expected = '"21"' ).
       scheme( code = |(number->string -8/3+2.3i)|
               expected = '"-8/3+2.3i"' ).
       scheme( code = |(number->string 26+inf.0i)|
               expected = '"26+inf.0i"' ).
       scheme( code = |(number->string -inf.0+nan.0i)|
               expected = '"-inf.0+nan.0i"' ).
     ENDMETHOD.                    "number_to_string_1

     METHOD number_to_string_2.
       scheme( code = |(number->string 256 16)|
               expected = '"100"' ).
     ENDMETHOD.

     METHOD string_append_0.
       scheme( code = |(string-append)|
               expected = '""' ).
     ENDMETHOD.                    "string_append_1

     METHOD string_append_1.
       scheme( code = |(string-append "ABAP" "Scheme" "Lisp")|
               expected = '"ABAPSchemeLisp"' ).
     ENDMETHOD.                    "string_append_1

  ENDCLASS.                    "ltc_list IMPLEMENTATION

*----------------------------------------------------------------------*

   CLASS ltc_vector DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS make_vector_0 FOR TESTING.

       METHODS vector_0 FOR TESTING.
       METHODS vector_1 FOR TESTING.

       METHODS vector_length_0 FOR TESTING.

       METHODS vector_ref_1 FOR TESTING.
       METHODS vector_ref_2 FOR TESTING.
       METHODS vector_ref_3 FOR TESTING.

       METHODS vector_ref_error FOR TESTING.

       METHODS vector_set_1 FOR TESTING.
       METHODS vector_set_2 FOR TESTING.

       METHODS vector_fill_1 FOR TESTING.

       METHODS vector_to_list_1 FOR TESTING.
       METHODS vector_to_list_2 FOR TESTING.
       METHODS vector_to_list_3 FOR TESTING.
       METHODS vector_to_list_4 FOR TESTING.
       METHODS vector_to_list_5 FOR TESTING.

       METHODS list_to_vector_1 FOR TESTING.
       METHODS list_to_vector_2 FOR TESTING.

   ENDCLASS.                    "ltc_vector DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_vector IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_vector IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD make_vector_0.
       scheme( code = |(make-vector 0)|
               expected = '#()' ).
       scheme( code = |(make-vector 0 '#(a))|
               expected = '#()' ).
       scheme( code = |(make-vector 5 '#(a))|
               expected = '#(#(a) #(a) #(a) #(a) #(a))' ).
     ENDMETHOD.                    "make_vector_0

     METHOD vector_0.
       scheme( code = |(vector? '#())|
               expected = '#t' ).
     ENDMETHOD.                    "vector_0

     METHOD vector_1.
       scheme( code = |(vector 0 '(2 3 4) "Anna")|
               expected = |#(0 (2 3 4) "Anna")| ).
     ENDMETHOD.                    "vector_1

     METHOD vector_length_0.
       scheme( code = |(vector-length '#())|
               expected = '0' ).
       scheme( code = |(vector-length '#(a b c))|
               expected = '3' ).
       scheme( code = |(vector-length (vector 1 '(2) 3 '#(4 5)))|
               expected = '4' ).
       scheme( code = |(vector-length (make-vector 300))|
               expected = '300' ).
     ENDMETHOD.                    "vector_length_0

     METHOD vector_ref_1.
       scheme( code = |(vector-ref '#(1 1 2 3 5 8 13 21) 5)|
               expected = '8' ).
     ENDMETHOD.                    "vector_ref_1

     METHOD vector_ref_2.
       scheme( code = |(vector-ref '#(1 1 2 3 5 8 13 21)| &
                         |   (exact                     | &
                         |    (round (* 2 (acos -1))) ))|
               expected = '13' ).
     ENDMETHOD.                    "vector_ref_2

     METHOD vector_ref_3.
       scheme( code = |(define vec (vector 1 2 3 4 5))|
               expected = 'vec' ).
       scheme( code = |(vector-ref vec 0)|
               expected = '1' ).
     ENDMETHOD.                    "vector_ref_3

     METHOD vector_ref_error.
       scheme( code = |(vector-ref '#(1 1 2 3 5 8 13 21) -1)|
               expected = 'Eval: -1 must be a non-negative integer in vector-ref' ).
       scheme( code = |(vector-ref '#(1 1 2) 5)|
               expected = 'Eval: vector-ref: out-of-bound position 5' ).
       scheme( code = |(vector-ref '#( ) 1)|
               expected = 'Eval: vector-ref: out-of-bound position 1' ).
     ENDMETHOD.

     METHOD vector_set_1.
       scheme( code = |(let ((vec (vector 0 '(2 2 2 2) "Anna"))) | &
                         |  (vector-set! vec 1 '("Sue" "Sue"))| &
                         | vec)|
               expected = '#(0 ("Sue" "Sue") "Anna")' ).
     ENDMETHOD.                    "vector_set_1

     METHOD vector_set_2.
       scheme( code = |(vector-set! '#(0 1 2) 1 "doe")|
               expected = 'Eval: constant vector cannot be changed in vector-set!' ).
     ENDMETHOD.                    "vector_set_2

     METHOD vector_to_list_1.
       scheme( code = |(vector->list '#(dah dah didah))|
               expected = '(dah dah didah)' ).
     ENDMETHOD.                    "vector_to_list_1

     METHOD vector_to_list_2.
       scheme( code = |(vector->list '#(dah dah didah) 1 2)|
               expected = '(dah)' ).
     ENDMETHOD.                    "vector_to_list_2

     METHOD vector_to_list_3.
       scheme( code = |(vector->list (vector)) |
               expected = c_lisp_nil ).
     ENDMETHOD.                    "vector_to_list_3

     METHOD vector_to_list_4.
       scheme( code = |(vector->list '#(a b c))|
               expected = '(a b c)' ).
     ENDMETHOD.                    "vector_to_list_4

     METHOD vector_to_list_5.
       scheme( code = |(let ((v '#(1 2 3 4 5)))| &
                         |  (apply * (vector->list v)))|
               expected = '120' ).
     ENDMETHOD.                    "vector_to_list_5

     METHOD list_to_vector_1.
       scheme( code = |(list->vector '(dididit dah))|
               expected = '#(dididit dah)' ).
     ENDMETHOD.                    "list_to_vector_1

     METHOD list_to_vector_2.
       scheme( code = |(let ([v '#(1 2 3 4 5)])| &
                         |  (let ([ls (vector->list v)])| &
                         |    (list->vector (map * ls ls))))|
               expected = '#(1 4 9 16 25)' ).
     ENDMETHOD.                    "list_to_vector_2

     METHOD vector_fill_1.
       scheme( code = |(define a (vector 1 2 3 4 5))|
               expected = 'a' ).
       scheme( code = |(vector-fill! a 'smash 2 4)|
               expected = '#(1 2 smash smash 5)' ).
     ENDMETHOD.

  ENDCLASS.                    "ltc_vector IMPLEMENTATION

   CLASS ltc_bytevector DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS is_bytevector_0 FOR TESTING.

       METHODS make_bytevector_0 FOR TESTING.
       METHODS make_bytevector_1 FOR TESTING.

       METHODS bytevector_0 FOR TESTING.
       METHODS bytevector_1 FOR TESTING.
       METHODS bytevector_2 FOR TESTING.

       METHODS bytevector_length_0 FOR TESTING.
       METHODS bytevector_length_1 FOR TESTING.
       METHODS bytevector_length_2 FOR TESTING.

       METHODS bytevector_ref_1 FOR TESTING.

       METHODS bytevector_set_1 FOR TESTING.

       METHODS bytevector_copy_new_1 FOR TESTING.

       METHODS bytevector_copy_1 FOR TESTING.

       METHODS bytevector_append_1 FOR TESTING.

       METHODS utf8_to_string_1 FOR TESTING.

       METHODS string_to_utf8_1 FOR TESTING.

   ENDCLASS.

   CLASS ltc_bytevector IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD is_bytevector_0.
       scheme( code = |(bytevector? -2)|
               expected = |#f| ).
       scheme( code = |(bytevector? #u8(0 10 5))|
               expected = |#t| ).
     ENDMETHOD.

     METHOD make_bytevector_0.
       scheme( code = |(make-bytevector 3)|
               expected = |#u8( 0 0 0 )| ).
     ENDMETHOD.

     METHOD make_bytevector_1.
       scheme( code = |(make-bytevector 2 12)|
               expected = |#u8( 12 12 )| ).
     ENDMETHOD.

     METHOD bytevector_0.
       scheme( code = |(bytevector 1 3 5 1 3 5)|
               expected = |#u8( 1 3 5 1 3 5 )| ).
     ENDMETHOD.

     METHOD bytevector_1.
       scheme( code = |(bytevector)|
               expected = |#u8()| ).
     ENDMETHOD.

     METHOD bytevector_2.
       scheme( code = |#u8(0 #e1e2 #xff)|
               expected = |#u8( #x00 #x64 #xFF )| ).
     ENDMETHOD.

     METHOD bytevector_length_0.
       scheme( code = |(bytevector-length #u8(1 3 5 1 3 5))|
               expected = |6| ).
     ENDMETHOD.

     METHOD bytevector_length_1.
       scheme( code = |(bytevector-length #u8())|
               expected = |0| ).
     ENDMETHOD.

     METHOD bytevector_length_2.
       scheme( code = |(bytevector-length 2)|
               expected = |Eval: 2 is not a bytevector in bytevector-length| ).
     ENDMETHOD.

     METHOD bytevector_ref_1.
       scheme( code = |(bytevector-u8-ref '#u8(1 1 2 3 5 8 13 21) 5)|
               expected = |8| ).
     ENDMETHOD.

     METHOD bytevector_set_1.
       scheme( code = |(let ((bv (bytevector 1 2 3 4)))| &
                      | (bytevector-u8-set! bv 1 3)| &
                      | bv)|
               expected = |#u8( 1 3 3 4 )| ).
     ENDMETHOD.

     METHOD bytevector_copy_new_1.
       scheme( code = |(define a #u8(1 2 3 4 5))|
               expected = |a| ).
       scheme( code = |(bytevector-copy a 2 4)|
               expected = |#u8( 3 4 )| ).
     ENDMETHOD.

     METHOD bytevector_copy_1.
       scheme( code = |(define a (bytevector 1 2 3 4 5))| &
                      |(define b (bytevector 10 20 30 40 50))| &
                      |(bytevector-copy! b 1 a 0 2)|
               expected = |a b #u8( 10 1 2 40 50 )| ).
       scheme( code = |b|
               expected = |#u8( 10 1 2 40 50 )| ).
     ENDMETHOD.

     METHOD bytevector_append_1.
       scheme( code = |(bytevector-append #u8(0 1 2) #u8(3 4 5))|
               expected = |#u8( 0 1 2 3 4 5 )| ).
     ENDMETHOD.

     METHOD utf8_to_string_1.
       scheme( code = |(utf8->string #u8(#x41))|
               expected = '"A"' ).
     ENDMETHOD.

     METHOD string_to_utf8_1.
       scheme( code = |(string->utf8 "λ")|
               expected = '#u8( #xCE #xBB )' ).
     ENDMETHOD.


   ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltc_library_function DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_library_function DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
*      merge with LTC_MATH?
       METHODS setup.
       METHODS teardown.
       METHODS radix RETURNING VALUE(code) TYPE string.
       METHODS make_parameter_1.

       METHODS abs_1 FOR TESTING.
       METHODS parameter_object_1 FOR TESTING.
   ENDCLASS.                    "ltc_library_function DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_library_function IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_library_function IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD abs_1.
       scheme( code = |(define (abs n)| &
                         |  (if (< n 0)| &
                         |  (- n)| &
                         |  n) )|
               expected = |abs| ).
       scheme( code = |(abs -2)|
               expected = |2| ).
       scheme( code = |(abs 12)|
               expected = |12| ).
       scheme( code = |(abs 0)|
               expected = |0| ).

       scheme( code = |(equal? 7 (abs 7))|
               expected = '#t' ).
       scheme( code = |(equal? 0 (abs 0))|
               expected = '#t' ).
     ENDMETHOD.                    "abs

     METHOD radix.
       code = |(define radix| &
              |  (make-parameter 10 | &
              |   (lambda (x) (if (and (exact-integer? x) (<= 2 x 16))| &
              |                  x| &
              |                 (error "invalid radix")))))|.
     ENDMETHOD.

     METHOD make_parameter_1.
       scheme( code = radix( )
               expected = 'radix' ).
       scheme( code = |(define (f n) (number->string n (radix)))|
               expected = 'f' ).
       scheme( code = |(f 12)|
               expected = '"12"' ).
     ENDMETHOD.

     METHOD parameter_object_1.
       make_parameter_1( ).
       scheme( code = |(parameterize ((radix 2)) (f 12))|
               expected = '"1100"' ).
       scheme( code = |(f 12)|
               expected = '"12"' ).
       scheme( code = |(radix 16)|
               expected = |'()| ).
       scheme( code = |(parameterize ((radix 0)) (f 12))|
               expected = 'Eval: radix (0) must be 2, 8, 10 or 16 in number->string' ).
     ENDMETHOD.

  ENDCLASS.                    "ltc_library_function IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_higher_order DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_higher_order DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS lambda_dotted FOR TESTING.
       METHODS lambda_variadic FOR TESTING.

       METHODS fold_right RETURNING VALUE(code) TYPE string.

       METHODS foldr FOR TESTING.
       METHODS foldl FOR TESTING.
       METHODS map FOR TESTING.
       METHODS filter FOR TESTING.

       METHODS apply_1 FOR TESTING.
       METHODS apply_2 FOR TESTING.
       METHODS apply_3 FOR TESTING.
       METHODS apply_4 FOR TESTING.
       METHODS apply_5 FOR TESTING.
       METHODS apply_6 FOR TESTING.
       METHODS apply_7 FOR TESTING.
       METHODS apply_8 FOR TESTING.
       METHODS apply_9 FOR TESTING.
       METHODS apply_10 FOR TESTING.

       METHODS map_1 FOR TESTING.
       METHODS map_2 FOR TESTING.
       METHODS map_3 FOR TESTING.
       METHODS map_4 FOR TESTING.
       METHODS map_5 FOR TESTING.
       METHODS map_6 FOR TESTING.
       METHODS map_7 FOR TESTING.
       METHODS map_8 FOR TESTING.

       METHODS for_each_1 FOR TESTING.
       METHODS for_each_2 FOR TESTING.
       METHODS for_each_3 FOR TESTING.
       METHODS for_each_4 FOR TESTING.

   ENDCLASS.                    "ltc_higher_order DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_higher_order IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_higher_order IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD lambda_dotted.
       scheme( code = |((lambda (x y . z) z) 3 4 5 6)|
               expected = |(5 6)| ).
     ENDMETHOD.                    "lambda_dotted

     METHOD lambda_variadic.
       scheme( code = |((lambda x x) 3 4 5 6)|
               expected = |(3 4 5 6)| ).
     ENDMETHOD.                    "lambda_variadic

     METHOD fold_right.
       code = |(define (fold-right f init seq)| &
              |  (if (null? seq)| &
              |  init| &
              |  (f (car seq)| &
              |       (fold-right f init (cdr seq)))))|.
     ENDMETHOD.                    "fold_right

     METHOD foldr.
       scheme( code = fold_right( )
               expected = 'fold-right' ).
       scheme( code = |(fold-right + 1 (list 1 2 3 7))|
               expected = '14' ).
       scheme( code = |(define (last lst)| &
                         |  (if (null? lst)| &
                         |    nil| &
                         |    (if (null? (cdr lst))| &
                         |      (car lst)| &
                         |      (last (cdr lst)) )| &
                         |  ))|
               expected = 'last' ).
       scheme( code = |(define (delete-adjacent-duplicates lst)| &
                         |  (fold-right (lambda (elem ret)| &
                         |                (if (equal? elem (car ret))| &
                         |                    ret| &
                         |                    (cons elem ret)))| &
                         |              (list (last lst))| &
                         |              lst))|
               expected = 'delete-adjacent-duplicates' ).
       scheme( code = |(delete-adjacent-duplicates '(1 2 3 3 4 4 4 5))|
               expected = |(1 2 3 4 5)| ).
     ENDMETHOD.                    "foldr

     METHOD foldl.
       scheme( code = |(define (fold-left f init seq)| &
                         |  (if (null? seq)| &
                         |  init| &
                         |  (fold-left f| &
                         |             (f init (car seq))| &
                         |             (cdr seq))))|
               expected = |fold-left| ).
       scheme( code = |(fold-left + 0 (list 1 2 3))|
               expected = '6' ).

       scheme( code = |(define (reverse l)| &
                         |  (fold-left (lambda (i j)| &
                         |               (cons j i))| &
                         |               '()| &
                         |               l))|
               expected = |reverse| ).
       scheme( code = |(reverse (list 1 2 3))|
               expected = '(3 2 1)' ).

     ENDMETHOD.                    "foldl

     METHOD map.
       scheme( code = |(define (map f lst)| &
                         |  (if (null? lst)| &
                         |    '()| &
                         |    (cons (f (car lst)) (map f (cdr lst)))))|
               expected = |map| ).
       scheme( code = |(map (lambda (n) (+ n 3))| &
                         |     '(1 2 3 4 5) )|
               expected = |(4 5 6 7 8)| ).
     ENDMETHOD.                    "map

     METHOD filter.
       scheme( code = fold_right( )
               expected = 'fold-right' ).
       scheme( code = |(define (filter pred? lst)| &
                         |  (fold-right (lambda (x y) (if (pred? x)| &
                         |                                (cons x y)| &
                         |                                y) )| &
                         |              '() lst))|
               expected = |filter| ).
       scheme( code = |(filter (lambda (n) (> n 4))| &
                         |     '(1 2 3 4 5 7) )|
               expected = |(5 7)| ).
     ENDMETHOD.                    "filter

     METHOD apply_1.
       scheme( code = |(apply + (list 3 4))|
               expected = '7' ).
     ENDMETHOD.                    "apply_1

     METHOD apply_2.
       scheme( code = |(apply + 1 -2 3 '(10 20))|
               expected = |32| ).
     ENDMETHOD.                    "apply_2

     METHOD apply_3.
       scheme( code = |(define arguments '(10 50 100))|
               expected = |arguments| ).
       scheme( code = |(apply + arguments)|
               expected = '160' ).
     ENDMETHOD.                    "apply_3

     METHOD apply_4.
       scheme( code = |(define compose| &
                         |  (lambda (f g)| &
                         |    (lambda args| &
                         |      (f (apply g args)))))|
               expected = |compose| ).
       scheme( code = |((compose sqrt *) 12 75)|
               expected = '30' ).
     ENDMETHOD.                    "apply_4

     METHOD apply_5.
       scheme( code = |(apply apply (list list (list 'apply 'list)))|
               expected = '(apply list)' ).
     ENDMETHOD.                    "apply_5

     METHOD apply_6.
       scheme( code = |(apply (lambda (x y . z) (vector x y z)) '(1 2))|
               expected = |#(1 2 { c_lisp_nil })| ).
     ENDMETHOD.                    "apply_6

     METHOD apply_7.
       scheme( code = |(apply vector 'a 'b '(c d e))|
               expected = |#(a b c d e)| ).
     ENDMETHOD.                    "apply_7

     METHOD apply_8.
       scheme( code = |(define first| &
                         |  (lambda (ls)| &
                         |(apply (lambda (x . y) x) ls)))|
               expected = |first| ).
       scheme( code = |(first '(a b c d))|
               expected = |a| ).
     ENDMETHOD.                    "apply_8

     METHOD apply_9.
       scheme( code = |(define rest| &
                         |  (lambda (ls)| &
                         |(apply (lambda (x . y) y) ls)))|
               expected = |rest| ).
       scheme( code = |(rest '(a b c d))|
               expected = |(b c d)| ).
     ENDMETHOD.                    "apply_9

     METHOD apply_10.
       scheme( code = |(apply append| &
                         |  '(1 2 3)| &
                         |  '((a b) (c d e) (f)))|
               expected = |(1 2 3 a b c d e f)| ).
     ENDMETHOD.                    "apply_10

     METHOD map_1.
       scheme( code = |(map cadr '((a b) (d e) (g h)))|
               expected = '(b e h)' ).
     ENDMETHOD.                    "map_1

     METHOD map_2.
       scheme( code = |(map + (list 3 4))|
               expected = '(3 4)' ).
     ENDMETHOD.                    "map_2

     METHOD map_3.
       scheme( code = |(map (lambda (n) (expt n n))| &
                         |'(1 2 3 4 5))|
               expected = '(1 4 27 256 3125)' ).
     ENDMETHOD.                    "map_3

     METHOD map_4.
       scheme( code = |(map + '(1 2 3) '(4 5 6 7))|
               expected = '(5 7 9)' ).
     ENDMETHOD.                    "map_4

     METHOD map_5.
       scheme( code = |(let ([count 0])| &
                         |  (map [lambda (ignored)| &
                         |         (set! count [+ count 1])| &
                         |          count]| &
                         |       '(a b) ))|
               expected = |(1 2)| ).  " or ( 2 1 )
     ENDMETHOD.                    "map_5

     METHOD map_6.
       scheme( code = |(map (lambda (n) (+ n 3))| &
                         |     '(1 2 3 4 5) )|
               expected = |(4 5 6 7 8)| ).
     ENDMETHOD.                    "map_6

     METHOD map_7.
       scheme( code = |(map car '())|
               expected = c_lisp_nil ).
     ENDMETHOD.                    "map_7

     METHOD map_8.
       scheme( code = |(map (lambda (x y) (+  x y 10))| &
                          |     '(1 2 3 4 5) | &
                          |     '(-1 -2 -3 -4) )|
               expected = |(10 10 10 10)| ).
       scheme( code = |(map (lambda (x y) (+  x y 10))| &
                          |     '(1 2 3 4) | &
                          |     '(-1 -2 -3 -4 -5) )|
               expected = |(10 10 10 10)| ).
     ENDMETHOD.

     METHOD for_each_1.
       scheme( code = |(for-each + (list 3 4))|
               expected = '4' ).  " unspecified
     ENDMETHOD.                    "for_each_1

     METHOD for_each_2.
       scheme( code = |(for-each (lambda (x) x) '(1 2 3 4))|
               expected = '4' ).   " unspecified
     ENDMETHOD.                    "for_each_2

     METHOD for_each_3.
       scheme( code = |(for-each even? '())|
               expected = c_lisp_nil ).   " #f, unspecified
     ENDMETHOD.                    "for_each_3

     METHOD for_each_4.
       scheme( code = |(for-each + (list 3 4) '(4 5))|
               expected = '9' ).  " unspecified
     ENDMETHOD.                    "for_each_4

   ENDCLASS.                    "ltc_higher_order IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_comparison DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_comparison DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS compa_gt_1 FOR TESTING.
       METHODS compa_gt_2 FOR TESTING.
       METHODS compa_gt_3 FOR TESTING.
       METHODS compa_gt_4 FOR TESTING.

       METHODS compa_gte_1 FOR TESTING.
       METHODS compa_gte_2 FOR TESTING.
       METHODS compa_gte_3 FOR TESTING.

       METHODS compa_lte_1 FOR TESTING.
       METHODS compa_lte_2 FOR TESTING.
       METHODS compa_lte_3 FOR TESTING.
       METHODS compa_lte_4 FOR TESTING.

       METHODS compa_equal_1 FOR TESTING.
       METHODS compa_equal_2 FOR TESTING.
       METHODS compa_equal_3 FOR TESTING.
       METHODS compa_equal_4 FOR TESTING.
       METHODS compa_equal_5 FOR TESTING.
       METHODS compa_equal_6 FOR TESTING.
       METHODS compa_equal_7 FOR TESTING.
       METHODS compa_equal_8 FOR TESTING.
       METHODS compa_equal_9 FOR TESTING.
       METHODS compa_equal_10 FOR TESTING.
       METHODS compa_equal_11 FOR TESTING.

       METHODS compa_if_1 FOR TESTING.
       METHODS compa_if_2 FOR TESTING.
       METHODS compa_if_3 FOR TESTING.

       METHODS compa_eq_1 FOR TESTING.
       METHODS compa_eq_2 FOR TESTING.
       METHODS compa_eq_3 FOR TESTING.

       METHODS compa_is_eq_1 FOR TESTING.
       METHODS compa_is_eq_2 FOR TESTING.
       METHODS compa_is_eq_3 FOR TESTING.
       METHODS compa_is_eq_4 FOR TESTING.
       METHODS compa_is_eq_5 FOR TESTING.
       METHODS compa_is_eq_6 FOR TESTING.
       METHODS compa_is_eq_7 FOR TESTING.
       METHODS compa_is_eq_8 FOR TESTING.
       METHODS compa_is_eq_9 FOR TESTING.
       METHODS compa_is_eq_10 FOR TESTING.
       METHODS compa_is_eq_11 FOR TESTING.
       METHODS compa_is_eq_12 FOR TESTING.

       METHODS compa_null_1 FOR TESTING.
       METHODS compa_null_2 FOR TESTING.
       METHODS compa_null_3 FOR TESTING.
       METHODS compa_null_4 FOR TESTING.

       METHODS compa_string FOR TESTING.

       METHODS comp_eqv_1 FOR TESTING.
       METHODS comp_eqv_2 FOR TESTING.
       METHODS comp_eqv_3 FOR TESTING.
       METHODS comp_eqv_4 FOR TESTING.
       METHODS comp_eqv_5 FOR TESTING.
       METHODS comp_eqv_6 FOR TESTING.
       METHODS comp_eqv_7 FOR TESTING.
       METHODS comp_eqv_8 FOR TESTING.
       METHODS comp_eqv_9 FOR TESTING.
       METHODS comp_eqv_10 FOR TESTING.
       METHODS comp_eqv_11 FOR TESTING.
       METHODS comp_eqv_12 FOR TESTING.
       METHODS comp_eqv_13 FOR TESTING.
       METHODS comp_eqv_14 FOR TESTING.
   ENDCLASS.                    "ltc_comparison DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_comparison IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_comparison IMPLEMENTATION.

     METHOD compa_gt_1.
*   Test GT
       scheme( code = '(> 1 2)'
               expected = '#f' ).
     ENDMETHOD.                    "compa_gt_1

     METHOD compa_gt_2.
       scheme( code = '(> 2 1)'
               expected = '#t' ).
     ENDMETHOD.                    "compa_gt_2

     METHOD compa_gt_3.
       scheme( code = '(> 4 3 2 1)'
               expected = '#t' ).
     ENDMETHOD.                    "compa_gt_3

     METHOD compa_gt_4.
       scheme( code = '(> 4 3 2 2)'
               expected = '#f' ).
     ENDMETHOD.                    "compa_gt_4
*
     METHOD compa_gte_1.
*   Test GTE
       scheme( code = '(>= 2 2)'
               expected = '#t' ).
     ENDMETHOD.                    "compa_gte_1

     METHOD compa_gte_2.
       scheme( code = '(>= 4 3 3 2)'
               expected = '#t' ).
     ENDMETHOD.                    "compa_gte_2

     METHOD compa_gte_3.
       scheme( code = '(>= 1 4)'
               expected = '#f' ).
     ENDMETHOD.                    "compa_gte_3

     METHOD compa_lte_1.
*   Test LT
       scheme( code = '(< 1 2 3)'
               expected = '#t' ).
       scheme( code = '(< 1 +inf.0)'
               expected = '#t' ).
     ENDMETHOD.                    "compa_lte_1

     METHOD compa_lte_2.
       scheme( code = '(< 1 2 2)'
               expected = '#f' ).
     ENDMETHOD.                    "compa_lte_2

     METHOD compa_lte_3.
       scheme( code = '(< 3 1)'
               expected = '#f' ).
     ENDMETHOD.                    "compa_lte_3

     METHOD compa_lte_4.
       scheme( code = '(< 1/12 1/3)'
               expected = '#t' ).
     ENDMETHOD.                    "compa_lte_4

     METHOD compa_equal_1.
*   Test equal?
       scheme( code = '(equal? 22 23)'
               expected = '#f' ).
     ENDMETHOD.                    "compa_equal_1

     METHOD compa_equal_2.
       scheme( code = '(equal? 22 22)'
               expected = '#t' ).
     ENDMETHOD.                    "compa_equal_2

     METHOD compa_equal_3.
       scheme( code = '(equal? (list 21) (list 21))'
               expected = '#t' ).
     ENDMETHOD.                    "compa_equal_3

     METHOD compa_equal_4.
       scheme( code = |(equal? (make-vector 5 'a)| &
                      |        (make-vector 5 'a))|
               expected = '#t' ).
     ENDMETHOD.                    "compa_equal_4

     METHOD compa_equal_5.
       scheme( code = |(equal? 'a 'a)|
               expected = '#t' ).
     ENDMETHOD.                    "compa_equal_5

     METHOD compa_equal_6.
       scheme( code = |(equal? '(a) '(a))|
               expected = '#t' ).
     ENDMETHOD.                    "compa_equal_6

     METHOD compa_equal_7.
       scheme( code = |(equal? '(a (b) c)| &
                      |        '(a (b) c))|
               expected = '#t' ).
     ENDMETHOD.                    "compa_equal_7

     METHOD compa_equal_8.
       scheme( code = |(equal? "abc" "abc")|
               expected = '#t' ).
     ENDMETHOD.                    "compa_equal_8

     METHOD compa_equal_9.
       scheme( code = |(equal? '#1=(a b . #1#)| &
                      |        '#2=(a b a b . #2#))|
               expected = '#t' ).
     ENDMETHOD.                    "compa_equal_9

     METHOD compa_equal_10.
       scheme( code = |(equal? (lambda (x) x)| &
                      |        (lambda (y) y))|
               expected = '#f' ).   " unspecified
     ENDMETHOD.                    "compa_equal_10

     METHOD compa_equal_11.
       scheme( code = |(equal? 7 (abs -7))|
               expected = '#t' ).
     ENDMETHOD.                    "compa_equal_11

     METHOD compa_if_1.
*   Test IF
       scheme( code = '(if 22 23)'
               expected = '23' ).
     ENDMETHOD.                    "compa_if_1

     METHOD compa_if_2.
       scheme( code = '(if (< 2 1) 23)'
               expected = c_undefined ).
     ENDMETHOD.                    "compa_if_2

     METHOD compa_if_3.
       scheme( code = '(if (< 2 1) 23 24)'
               expected = '24' ).
     ENDMETHOD.                    "compa_if_3

     METHOD compa_eq_1.
*      Test =
       scheme( code = '(= 2 3)'
               expected = '#f' ).
       scheme( code = '(= 1/3 1/3)'
               expected = '#t' ).
       scheme( code = '(= 1 19/20)'
               expected = '#f' ).
       scheme( code = '(= 2.0 3+i)'
               expected = '#f' ).
       scheme( code = '(= 3.0+i 3+i)'
               expected = '#t' ).
       scheme( code = '(= 3.0+1.0i 3+i)'
               expected = '#t' ).
       scheme( code = '(= 3.0+1.0i 3)'
               expected = '#f' ).
       scheme( code = '(= 2.5i 0.0+5/2i)'
               expected = '#t' ).
       scheme( code = '(= 2.0 2+0.0i)'
               expected = '#t' ).
       scheme( code = '(= 1/4 0.25)'
               expected = '#t' ).
       scheme( code = '(= 3 3)'
               expected = '#t' ).
     ENDMETHOD.                    "compa_eq_1

     METHOD compa_eq_2.
       scheme( code = '(= 2.0 2)'
               expected = '#t' ).
       scheme( code = '(= 0.5 0.5)'
               expected = '#t' ).
       scheme( code = '(= #e0.4 #e0.4)'
               expected = '#t' ).
       scheme( code = '(= 2.0 2 +nan.0)'
               expected = '#f' ).
       scheme( code = '(= +nan.0 +nan.0)'
               expected = '#f' ).
       scheme( code = '(= +inf.0 +inf.0)'
               expected = '#t' ).
     ENDMETHOD.                    "compa_eq_2

     METHOD compa_eq_3.
*      equality of many things
       scheme( code = '(= (+ 3 4) 7 (+ 2 5))'
               expected = '#t' ).
     ENDMETHOD.                    "compa_eq_2

     METHOD compa_is_eq_1.
       scheme( code = |(eq? 'a 'a)|
               expected = '#t' ).
     ENDMETHOD.                    "compa_is_eq_1

     METHOD compa_is_eq_2.
       scheme( code = |(eq? '(a) '(a))|
               expected = '#f' ).   " unspecified
     ENDMETHOD.                    "compa_is_eq_2

     METHOD compa_is_eq_3.
       scheme( code = |(eq? (list 'a) (list 'a))|
               expected = '#f' ).
     ENDMETHOD.                    "compa_is_eq_3

     METHOD compa_is_eq_4.
       scheme( code = |(eq? "a" "a")|
               expected = '#t' ).    " unspecified
     ENDMETHOD.                    "compa_is_eq_4

     METHOD compa_is_eq_5.
       scheme( code = '(eq? "" "")'
               expected = '#t' ).   " unspecified
     ENDMETHOD.                    "compa_is_eq_5

     METHOD compa_is_eq_6.
       scheme( code = |(eq? '() '())|
               expected = '#t' ).
     ENDMETHOD.                    "compa_is_eq_6

     METHOD compa_is_eq_7.
       scheme( code = |(eq? 2 2)|
               expected = '#t' ).    " unspecified
     ENDMETHOD.                    "compa_is_eq_7

     METHOD compa_is_eq_8.
       scheme( code = '(eq? #\A #\A)'
               expected = '#t' ).   " unspecified
     ENDMETHOD.                    "compa_is_eq_8

     METHOD compa_is_eq_9.
       scheme( code = |(eq? car car)|
               expected = '#t' ).
     ENDMETHOD.                    "compa_is_eq_9

     METHOD compa_is_eq_10.
       scheme( code = |(let ((n (+ 2 3)))| &
                      |  (let ((x '(a))) | &
                      | (eq? x x)))|
               expected = '#t' ).
     ENDMETHOD.                    "compa_is_eq_10

     METHOD compa_is_eq_11.
       scheme( code = |(let ((x '#()))| &
                      |  (eq? x x))|
               expected = '#t' ).
     ENDMETHOD.                    "compa_is_eq_11

     METHOD compa_is_eq_12.
       scheme( code = |(let ((p (lambda (x) x)))| &
                      |  (eq? p p))|
               expected = '#t' ).
     ENDMETHOD.                    "compa_is_eq_12

     METHOD compa_null_1.
*      changed to allow nil without quote
       scheme( code = '(nil? ())'
               expected = '#t' ).
       scheme( code = '(null? ())'
               expected = '#t' ).
     ENDMETHOD.                    "compa_nil_1

     METHOD compa_null_2.
       scheme( code = |(null? '())|
               expected = '#t' ).
       scheme( code = '(nil? nil)'
               expected = '#t' ).
     ENDMETHOD.                    "compa_nil_2

     METHOD compa_null_3.
       scheme( code = '(null? (cdr (list 1)))'
               expected = '#t' ).
       scheme( code = '(nil? (cdr (list 1)))'
               expected = '#t' ).
     ENDMETHOD.                    "compa_nil_3

     METHOD compa_null_4.
       scheme( code = '(null? (cdr (list 1 2)))'
               expected = '#f' ).
       scheme( code = '(nil? (cdr (list 1 2)))'
               expected = '#f' ).
     ENDMETHOD.                    "compa_nil_4

     METHOD compa_string.
       scheme( code = '(define str "A string")'
               expected = 'str' ).
       scheme( code = '(< str "The string")'
               expected = 'Eval: "A string" is not a number in <' ).
     ENDMETHOD.                    "compa_string

     METHOD comp_eqv_1.
       scheme( code = |(eqv? 'a 'a)|
               expected = '#t' ).
     ENDMETHOD.                    "comp_eqv_1

     METHOD comp_eqv_2.
       scheme( code = |(eqv? 'a 'b)|
               expected = '#f' ).
     ENDMETHOD.                    "comp_eqv_2

     METHOD comp_eqv_3.
       scheme( code = |(eqv? 2 2)|
               expected = '#t' ).
     ENDMETHOD.                    "comp_eqv_3

     METHOD comp_eqv_4.
       scheme( code = |(eqv? 2 2.0)|
               expected = '#f' ).
     ENDMETHOD.                    "comp_eqv_4

     METHOD comp_eqv_5.
       scheme( code = |(eqv? '() '())|
               expected = '#t' ).
     ENDMETHOD.                    "comp_eqv_5

     METHOD comp_eqv_6.
       scheme( code = |(eqv? 100000000 100000000)|
               expected = '#t' ).
     ENDMETHOD.                    "comp_eqv_6

     METHOD comp_eqv_7.
       scheme( code = |(eqv? 0.0 +nan.0)|
               expected = '#f' ).
     ENDMETHOD.                    "comp_eqv_7

     METHOD comp_eqv_8.
       scheme( code = |(eqv? (cons 1 2) (cons 1 2))|
               expected = '#f' ).
     ENDMETHOD.                    "comp_eqv_8

     METHOD comp_eqv_9.
       scheme( code = |(eqv? (lambda () 1) (lambda () 2))|
               expected = '#f' ).
     ENDMETHOD.                    "comp_eqv_9

     METHOD comp_eqv_10.
       scheme( code = |(let ((p (lambda (x) x)))| &
                      |(eqv? p p))|
               expected = '#t' ).
     ENDMETHOD.                    "comp_eqv_10

     METHOD comp_eqv_11.
       scheme( code = |(eqv? #f 'nil)|
               expected = '#f' ).
     ENDMETHOD.                    "comp_eqv_11

     METHOD comp_eqv_12.
       scheme( code = |(define gen-counter | &
                      | (lambda () | &
                      |   (let ((n 0)) | &
                      |     (lambda () (set! n (+ n 1)) n))))|
               expected = 'gen-counter' ).
       scheme( code = |(let ((g (gen-counter))) | &
                         |  (eqv? g g))|
               expected = '#t' ).
       scheme( code = |(eqv? (gen-counter) (gen-counter))|
               expected = '#f' ).
     ENDMETHOD.                    "comp_eqv_12

     METHOD comp_eqv_13.
       scheme( code = |(define gen-loser | &
                      | (lambda () | &
                      |   (let ((n 0)) | &
                      |     (lambda () (set! n (+ n 1)) 27))))|
               expected = 'gen-loser' ).
       scheme( code = |(let ((g (gen-loser))) | &
                         |  (eqv? g g))|
               expected = '#t' ).
       scheme( code = |(eqv? (gen-loser) (gen-loser))|
               expected = '#f' ).  " unspecfied
     ENDMETHOD.                    "comp_eqv_13

     METHOD comp_eqv_14.
       scheme( code = |(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))| &
                      |         (g (lambda () (if (eqv? f g) 'g 'both))))| &
                      | (eqv? f g))|
               expected = '#f' ).
     ENDMETHOD.                    "comp_eqv_14

   ENDCLASS.                    "ltc_comparison IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_basic_functions DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_basic_functions DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS funct_lambda_0 FOR TESTING.
       METHODS funct_lambda_1 FOR TESTING.
       METHODS funct_lambda_2 FOR TESTING.

       METHODS funct_fact FOR TESTING.

       METHODS funct_arg_count FOR TESTING.
       METHODS funct_arg_missing FOR TESTING.

       METHODS case_lambda_empty FOR TESTING.
       METHODS case_lambda_one FOR TESTING.
       METHODS case_lambda_0 FOR TESTING.
       METHODS case_lambda_1 FOR TESTING.
       METHODS case_lambda_2 FOR TESTING.
       METHODS case_lambda_3 FOR TESTING.
       METHODS case_lambda_error FOR TESTING RAISING cx_static_check.
   ENDCLASS.                    "ltc_basic_functions DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_basic_functions IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_basic_functions IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD funct_lambda_0.
       scheme( code = '(define (b n) (* 11 n))'
               expected = 'b' ).
       scheme( code = 'b'
               expected = '<lambda> (n)' ).
       scheme( code = '(b 20)'
               expected = '220' ).
     ENDMETHOD.                    "funct_lambda_0

     METHOD funct_lambda_1.
*   Test LAMBDA
       scheme( code = '(define b (lambda (b) (* 10 b)))'
               expected = 'b' ).
       scheme( code = 'b'
               expected = '<lambda> (b)' ).
       scheme( code = '(b 20)'
               expected = '200' ).
     ENDMETHOD.                    "funct_lambda_1

     METHOD funct_lambda_2.
       scheme( code = '((lambda (a) (+ a 20)) 10 )'
               expected = '30' ).
     ENDMETHOD.                    "funct_lambda_2

     METHOD funct_fact.
*   Function shorthand
       scheme( code = '(define (fact x) (if (= x 0) 1 (* x (fact (- x 1)))))'
               expected = 'fact' ).
       scheme( code = '(fact 8)'
               expected = '40320' ).
     ENDMETHOD.                    "funct_fact

     METHOD funct_arg_count.
       scheme( code = '(define (f x y) (+ x y))'
               expected = 'f' ).
       scheme( code = '(f 1 2 3)'
               expected = 'Eval: Expected 2 parameter(s), found (1 2 3)' ).
     ENDMETHOD.                    "funct_arg_count

     METHOD funct_arg_missing.
       scheme( code = '(define (add x y) (+ x y))'
               expected = 'add' ).
       scheme( code = '(add 1)'
               expected = 'Eval: Missing parameter(s) (y)' ).
     ENDMETHOD.                    "funct_arg_count

     METHOD case_lambda_empty.
       scheme( code = '(case-lambda)'
               expected = |'()| ).
     ENDMETHOD.

     METHOD case_lambda_one.
       scheme( code = '(case-lambda)'
               expected = |'()| ).
     ENDMETHOD.

     METHOD case_lambda_0.
       scheme( code = |(define range (case-lambda | &
                      | ((e) (range 0 e))| &
                      | ((b e) (do ((r '() (cons e r)) | &
                      |           (e (- e 1) (- e 1)))| &
                      |           ((< e b) r)))))|
               expected = |range| ).
       scheme( code = |(range 3)|
               expected = |(0 1 2)| ).
       scheme( code = |(range 3 5)|
               expected = |(3 4)| ).
     ENDMETHOD.

     METHOD case_lambda_1.
       scheme( code = |(let ([f (case-lambda| &
                      |          [() 10]| &
                      |          [(x) x]| &
                      |          [(x y) (list y x)]| &
                      |          [r r])])| &
                      |  (list (f)| &
                      |        (f 1)| &
                      |        (f 1 2)| &
                      |        (f 1 2 3)))|
               expected = |(10 1 (2 1) (1 2 3))| ).
     ENDMETHOD.

     METHOD case_lambda_2.
       scheme( code = |(define (make-accum n)| &
                      |  (case-lambda| &
                      |    (() n)| &
                      |    ((m) (set! n (+ n m)) n)))|
               expected = |make-accum| ).
       scheme( code = |(define a (make-accum 20))|
               expected = |a| ).
       scheme( code = |(a)|
               expected = |20| ).
       scheme( code = |(a 10)|
               expected = |30| ).
       scheme( code = |(a)|
               expected = |30| ).
     ENDMETHOD.

     METHOD case_lambda_3.
       scheme( code = |(define plus| &
                      |  (case-lambda | &
                      |    (() 0)| &
                      |    ((x) x)| &
                      |    ((x y) (+ x y))| &
                      |    ((x y z) (+ (+ x y) z))| &
                      |    (args (apply + args))))|
               expected = |plus| ).
       scheme( code = |(plus)|
               expected = |0| ).
       scheme( code = |(plus 1)|
               expected = |1| ).
       scheme( code = |(plus 1 2 3)|
               expected = |6| ).
     ENDMETHOD.

     METHOD case_lambda_error.
       scheme( code = |((case-lambda| &
                          |   ((a) a)| &
                          |   ((a b) (* a b)))| &
                          |  1 2 3)|
               expected = `Eval: no clause matching the arguments` ).
     ENDMETHOD.

  ENDCLASS.                    "ltc_basic_functions IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_hash_element DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_hash_element DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS hash FOR TESTING.
   ENDCLASS.                    "ltc_hash_element DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_hash_element IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_hash_element IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD hash.
*   Hash implementation
       scheme( code = '(define h1 (make-hash ''(dog "bow-wow" cat "meow" kennel (dog cat hedgehog))))'
               expected = 'h1' ).
       scheme( code = 'h1'
               expected = '#!hash' ).
       scheme( code = '(hash-keys h1)'
               expected = '(dog cat kennel)' ).
       scheme( code = '(hash-get h1 ''kennel)'
               expected = '(dog cat hedgehog)' ).
       scheme( code = '(hash-remove h1 ''kennel)'
               expected = c_lisp_nil ).
       scheme( code = '(hash-get h1 ''sparrow)'
               expected = c_lisp_nil ).
       scheme( code = '(hash-insert h1 ''sparrow "whoosh")'
               expected = c_lisp_nil ).
       scheme( code = '(hash-get h1 ''sparrow)'
               expected = '"whoosh"' ).
       scheme( code = '(hash-keys h1)'
               expected = '(dog cat sparrow)' ).
     ENDMETHOD.                    "hash

   ENDCLASS.                    "ltc_hash_element IMPLEMENTATION

   CLASS ltc_delayed_evaluation DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PUBLIC SECTION.
       METHODS constructor.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS define_integers.

       METHODS delay_0  FOR TESTING.
       METHODS force_0  FOR TESTING.
       METHODS force_1  FOR TESTING.
       METHODS force_2  FOR TESTING.
       METHODS force_3  FOR TESTING.
       METHODS promise_0  FOR TESTING.
       METHODS delay_force_0  FOR TESTING.
       METHODS delay_force_1  FOR TESTING.
   ENDCLASS.                    "ltc_abap_integration DEFINITION

   CLASS ltc_delayed_evaluation IMPLEMENTATION.

     METHOD constructor.
       super->constructor( ).
     ENDMETHOD.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD delay_0.
       scheme( code = '(delay (+ 1 2))'
               expected = '<promise>' ).
     ENDMETHOD.

     METHOD force_0.
       scheme( code = '(let ((p (delay (+ 1 2))))' &
                      '  (list (force p) (force p)))'
               expected = '(3 3)' ).
     ENDMETHOD.

     METHOD define_integers.
       scheme( code = |(define integers| &
                      |  (letrec ((next| &
                      |     (lambda (n)| &
                      |        (delay (cons n (next (+ n 1)))))))| &
                      |    (next 0)))|
               expected = 'integers' ).

       scheme( code = |(define head| &
                      |  (lambda (stream) (car (force stream))))|
               expected = 'head' ).

       scheme( code = |(define tail| &
                      |  (lambda (stream) (cdr (force stream))))|
               expected = 'tail' ).
     ENDMETHOD.

     METHOD force_1.
       define_integers( ).

       scheme( code = |(head (tail (tail integers)))|
               expected = '2' ).
     ENDMETHOD.

     METHOD force_2.
       scheme( code = |(force (+ 1 2))|
               expected = '3' ).
     ENDMETHOD.

     METHOD force_3.
       scheme( code = |(force 3)|
               expected = '3' ).
     ENDMETHOD.

     METHOD delay_force_0.
       scheme( code = '(delay-force (+ 1 2))'
               expected = '3' ).
     ENDMETHOD.

     METHOD delay_force_1.
       define_integers( ).

       scheme( code = |(define (stream-filter p? s)| &
                      |  (delay-force| &
                      |     (if (null? (force s))| &
                      |        (delay '())| &
                      |        (let ((h (car (force s)))| &
                      |             (t (cdr (force s))))| &
                      |          (if (p? h)| &
                      |             (delay (cons h (stream-filter p? t)))| &
                      |             (stream-filter p? t))))))|
               expected = 'stream-filter' ).

       scheme( code = |(head (tail (stream-filter odd? integers)))|
               expected = '5' ).
     ENDMETHOD.

     METHOD promise_0.
       scheme( code = |(define count 0)|
               expected = 'count' ).
       scheme( code = |(define p| &
                      |   (delay (begin (set! count (+ count 1))| &
                      |      (if (> count x)| &
                      |        count)| &
                      |        (force p)))))|
               expected = 'p' ).
       scheme( code = |(define x 5)|
               expected = 'x' ).
       scheme( code = |p|
               expected = '<promise>' ).
       scheme( code = |(force p)|
               expected = '6' ).
       scheme( code = |p|
               expected = '<promise>' ).
       scheme( code = |(begin (set! x 10)| &
                      |       (force p))|
               expected = '6' ).
     ENDMETHOD.

  ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS ltc_abap_integration DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_abap_integration DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.

       METHODS abap_data_mandt FOR TESTING.
       METHODS abap_data_t005g FOR TESTING.
       METHODS empty_structure FOR TESTING.
       METHODS user_name FOR TESTING.
   ENDCLASS.                    "ltc_abap_integration DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_abap_integration IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_abap_integration IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD abap_data_mandt.
       scheme( code = '(define mandt (ab-data "MANDT"))'
               expected = 'mandt' ).
       scheme( code = '(ab-set-value mandt "000")'
               expected = c_lisp_nil ).
       scheme( code = 'mandt'
               expected = '#!ABAP-Data' ).
     ENDMETHOD.                    "abap_data

     METHOD abap_data_t005g.
       scheme( code = '(define t005g (ab-data "T005G"))'
               expected = 't005g' ).
       scheme( code = '(ab-set t005g "LAND1" "ZA")'  " Set field "LAND1" to "ZA"
               expected = c_lisp_nil ).
       scheme( code = '(ab-get t005g "LAND1")'       " Return the value of field "LAND1"
               expected = '"ZA"' ).
     ENDMETHOD.                    "abap_data

     METHOD empty_structure.
       scheme( code = '(define t005g (ab-data "T005G"))'
               expected = 't005g' ).
       scheme( code = '(ab-set-value t005g ''("000" "ZA" "ABC" "JHB"))'
               expected = c_lisp_nil ).
       scheme( code = '(ab-get-value t005g)'
               expected = '("000" "ZA" "ABC" "JHB")' ).
       scheme( code = '(ab-get t005g "LAND1")'
               expected = '"ZA"' ).
     ENDMETHOD.                    "empty_structure

     METHOD user_name.
       DATA lv_uname TYPE string.
       lv_uname = sy-uname.
       scheme( code = '(ab-get ab-sy "UNAME")'
               expected = |"{ lv_uname }"| ).
     ENDMETHOD.                    "user_name

   ENDCLASS.                    "ltc_abap_integration IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_abap_function_module DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_abap_function_module DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       METHODS setup.
       METHODS teardown.
       METHODS get_first_profile RETURNING VALUE(rv_prof) TYPE xuprofile.
       METHODS get_ip_address RETURNING VALUE(rv_addrstr) TYPE ni_nodeaddr.

       METHODS fm_user_info FOR TESTING.
       METHODS fm_test_rfc FOR TESTING.
       METHODS fm_user_details FOR TESTING.

   ENDCLASS.                    "ltc_abap_function_module DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_abap_function_module IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_abap_function_module IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD get_ip_address.
       CALL FUNCTION 'TH_USER_INFO'
         IMPORTING
           addrstr = rv_addrstr.
     ENDMETHOD.                    "get_ip_address

     METHOD fm_user_info.
*;(let ( ( f1 (ab-function "TH_USER_INFO")  )  )
*;       ( begin (f1) (ab-get f1 "ADDRSTR")  )
       scheme( code = '(ab-function "TH_USER_INFO")'
               expected = '<ABAP function module TH_USER_INFO>' ).
       scheme( code = '(define f1 (ab-function "TH_USER_INFO"))'
               expected = 'f1' ).
       scheme( code = '(f1)'
               expected = '<ABAP function module TH_USER_INFO>' ).
       scheme( code = '(ab-get f1 "ADDRSTR")'
               expected = |"{ get_ip_address( ) }"| ).
     ENDMETHOD.                    "fm_user_info

     METHOD fm_test_rfc.
*; (let ( (f2 (ab-function "TH_TEST_RFC"))  )
*;        ( begin (ab-set f2 "TEXT_IN" "Calling from ABAP Lisp" )
*;                  (f2) (ab-get f2 "TEXT_OUT")  ) )
       scheme( code = '(define f2 (ab-function "TH_TEST_RFC"))'
               expected = 'f2' ).
       scheme( code = '(ab-set f2 "TEXT_IN" "Calling from ABAP Lisp")'
               expected = c_lisp_nil ).
       scheme( code = '(f2)'
               expected = '<ABAP function module TH_TEST_RFC>' ).
       scheme( code = '(ab-get f2 "TEXT_OUT")'
               expected = '"Calling from ABAP Lisp"' ).
     ENDMETHOD.                    "fm_test_rfc

     METHOD get_first_profile.
       DATA lt_profiles TYPE STANDARD TABLE OF bapiprof.
       DATA ls_profiles TYPE bapiprof.
       DATA lt_return TYPE bapiret2_t.

       CALL FUNCTION 'BAPI_USER_GET_DETAIL'
         EXPORTING
           username = sy-uname
         TABLES
           profiles = lt_profiles
           return   = lt_return.

       READ TABLE lt_profiles INDEX 1 INTO ls_profiles.
       rv_prof = ls_profiles-bapiprof.
     ENDMETHOD.                    "get_first_profile


     METHOD fm_user_details.
*(let (( profiles
*        (let ( (f3 (ab-function "BAPI_USER_GET_DETAIL"))  )
*        ( begin (ab-set f3 "USERNAME" (ab-get ab-sy "UNAME") )
*                  (f3) (ab-get f3 "PROFILES")  ) )
*        ) )
*   (let ((profile (ab-get profiles 1)) )
*             (ab-get profile "BAPIPROF" )  )
*)
       scheme( code = '(define f3 (ab-function "BAPI_USER_GET_DETAIL"))'
               expected = 'f3' ).
       scheme( code = '(ab-set f3 "USERNAME" (ab-get ab-sy "UNAME"))'
               expected = c_lisp_nil ).
       scheme( code = '(f3)'
               expected = '<ABAP function module BAPI_USER_GET_DETAIL>' ).
       scheme( code = '(define profiles (ab-get f3 "PROFILES"))'
               expected = 'profiles' ).
       scheme( code = 'profiles'
               expected = '#!ABAP-Table' ).

       scheme( code = '(define profile (ab-get profiles 1))'
               expected = 'profile' ).
       scheme( code = '(ab-get profile "BAPIPROF")'
               expected = |"{ get_first_profile( ) }"| ).
     ENDMETHOD.                    "fm_user_details

   ENDCLASS.                    "ltc_abap_function_module IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_quote IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_quote IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD quasiquote_1.
       scheme( code = '`(list ,(+ 1 2) 4)'
               expected = '(list 3 4)' ).
     ENDMETHOD.                    "quasiquote_1

     METHOD quasiquote_2.
       scheme( code = |(let ((name 'a)) `(list ,name ',name))|
               expected = |(list a 'a)| ).
     ENDMETHOD.                    "quasiquote_2

     METHOD quasiquote_2_args.
       scheme( code = '(quasiquote a b)'
            expected = 'Eval: quasiquote can only take a single argument' ).
     ENDMETHOD.                    "quasiquote_2_args

     METHOD quasiquote_splicing_3.
       scheme( code = |`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)|
               expected = '(a 3 4 5 6 b)' ).
     ENDMETHOD.                    "quasiquote_splicing_3

     METHOD quasiquote_splicing_4.
       scheme( code = |`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))|
               expected = '((foo 7) . cons)' ).
     ENDMETHOD.                    "quasiquote_splicing_4

     METHOD quasiquote_splicing_5.
       scheme( code = |`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)|
               expected = '#(10 5 2 4 3 8)' ).
     ENDMETHOD.                    "quasiquote_splicing_5

     METHOD quasiquote_splicing_6.
       scheme( code = |(let ((foo '(foo bar)) (@baz 'baz))| &
                         |`(list ,@foo , @baz))|
               expected = '(list foo bar baz)' ).
     ENDMETHOD.                    "quasiquote_splicing_6

     METHOD quasiquote_7.
       scheme( code = '`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)'
               expected = '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)' ).
     ENDMETHOD.                    "quasiquote_7

     METHOD quasiquote_8.
       scheme( code = |(let ((name1 'x)| &
                         |      (name2 'y))| &
                         |  `(a `(b ,,name1 ,',name2 d) e))|
               expected = |(a `(b ,x ,'y d) e)|  ).
     ENDMETHOD.                    "quasiquote_8

     METHOD quasiquote_9.
       scheme( code = '(quasiquote (list (unquote (+ 1 2)) 4))'
               expected = '(list 3 4)' ).
     ENDMETHOD.                    "quasiquote_9

     METHOD quasiquote_10.
       scheme( code = |,4|
               expected = 'Eval: unquote not valid outside of quasiquote' ).
     ENDMETHOD.                    "quasiquote_10

     METHOD quasiquote_11.
       scheme( code = |'(quasiquote (list (unquote (+ 1 2)) 4))|
               expected = '`(list ,(+ 1 2) 4)' ).
     ENDMETHOD.                    "quasiquote_11

     METHOD quasiquote_12.
       scheme( code = |`(,'five 5)|
               expected = '(five 5)' ).
     ENDMETHOD.                    "quasiquote_12

     METHOD quasiquote_13.
       scheme( code = |`(,1 2 3)|
               expected = '(1 2 3)' ).
     ENDMETHOD.                    "quasiquote_13

*     METHOD quine_3.
       "http://community.schemewiki.org/?quines
*       scheme( code = |((lambda (q qq) ((lambda (x) `((lambda (q qq) ,(q x)) . ,(q qq)))| &
*                      | '(lambda (x) `((lambda (q qq) ,(q x)) . ,(q qq)))))| &
*                      | (lambda (q) `(,q ',q)) '(lambda (q) `(,q ',q)))|
*           expected = |((lambda (q qq) ((lambda (x) `((lambda (q qq) ,(q x)) ,(q qq)))| &
*                      | '(lambda (x) `((lambda (q qq) ,(q x)) ,(q qq)))))| &
*                      | (lambda (q) `(,q ',q)) '(lambda (q) `(,q ',q)))| ).
*     ENDMETHOD.

     METHOD quine_1.
       scheme( code =     |( ( lambda ( x ) `(,( reverse x ) ',x ) ) '(`(,( reverse x ) ',x ) ( x ) lambda) )|
               expected = |((lambda (x) `(,(reverse x) ',x)) '(`(,(reverse x) ',x) (x) lambda))| ).
     ENDMETHOD.

     METHOD quine_2.
       scheme( code =     |((lambda (q) `(,q ',q)) '(lambda (q) `(,q ',q)))|
               expected = |((lambda (q) `(,q ',q)) '(lambda (q) `(,q ',q)))| ).
     ENDMETHOD.

   ENDCLASS.                    "ltc_quote IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_macro IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_macro IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD macro_while.
*      (define­syntax while
*        (syntax­rules ()
*          ((_ x body ...)
*            (let loop ()
*              (if x
*               (begin body ... (loop)))))))
       scheme( code = '(define-macro (while _ . ...)' &
                      ' (let ((loop (gensym)))' &
                      '   `(let ,loop ()' &
                      '      (if ,_ '     &
                      '          (begin ,@... (,loop))))))'
               expected = 'while' ).
       scheme( code = '(define x 1)'
               expected = 'x' ).
       scheme( code = '(while (< x 10)' &
                      '   (set! x (+ x 1)))'
               expected = c_undefined ).
       scheme( code = 'x'
               expected = '10' ).
     ENDMETHOD.                    "macro_while

     METHOD macro_1.
       scheme( code = '(define-macro (let1 var val . body)' &
                         '`(let ((,var ,val)) ,@body ) )'
               expected = 'let1' ).
       scheme( code = '(let1 foo (+ 2 3)' &
                         '  (* foo foo))'
               expected = '25' ).
     ENDMETHOD.                    "macro_1

     METHOD macro_2.
       scheme( code = '(define-macro (let1 var val . body)' &
                         '`(let ((,var ,val)) ,@body ) )'
               expected = 'let1' ).
       scheme( code = '(macroexpand (let1 foo (+ 2 3) (* foo foo)) )'
               expected = '(let ((foo (+ 2 3))) (* foo foo))' ).
     ENDMETHOD.                    "macro_2

     METHOD macro_one.
       scheme( code = '(define-macro one (lambda () 1))'
               expected = 'one' ).
       scheme( code = '(one)'
               expected = '1' ).
     ENDMETHOD.                    "macro_one

     METHOD macro_two.
       scheme( code = '(define-macro two (lambda () 2))'
               expected = 'two' ).
       scheme( code = '(two)'
               expected = '2' ).
     ENDMETHOD.                    "macro_two

     METHOD macro_unless_1.
       scheme( code = '(define-macro my-unless (lambda (pred a b) `(if ,pred ,b ,a)))'
               expected = 'my-unless' ).
       scheme( code = '(my-unless #f 7 8)'
               expected = '7' ).
       scheme( code = '(my-unless #t 7 8)'
               expected = '8' ).
     ENDMETHOD.                    "macro_unless_1

     METHOD macro_unless_2.
       scheme( code = '(define-macro my-unless (lambda (pred a b) `(if (not ,pred) ,a ,b)))'
               expected = 'my-unless' ).
       scheme( code = '(my-unless #f 7 8)'
               expected = '7' ).
       scheme( code = '(my-unless #t 7 8)'
               expected = '8' ).
       scheme( code = '(macroexpand (my-unless 2 3 4))'
               expected = '(if (not 2) 3 4)' ).
     ENDMETHOD.                    "macro_unless_2

     METHOD macro_eval_1.
       scheme( code = |(define-macro identity (lambda (x) x))|
               expected = 'identity' ).
       scheme( code = |(let* ((a 123)) (identity a))|
               expected = '123' ).
     ENDMETHOD.                    "macro_eval_1

   ENDCLASS.                    "ltc_macro IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_query IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   CLASS ltc_query IMPLEMENTATION.

     METHOD select_1.
       scheme( code = |(sql-query "SELECT * FROM USR01 WHERE BNAME = 'DEVELOPER' ")|
               expected = '#!ABAP-Query-Result-Set' ).
     ENDMETHOD.                    "select_1

   ENDCLASS.                    "ltc_query IMPLEMENTATION

   CLASS ltc_turtles DEFINITION INHERITING FROM ltc_interpreter
     FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
     PRIVATE SECTION.
       CONSTANTS c_abap_turtle TYPE string VALUE '#!ABAP-Turtle'.
       METHODS setup.
       METHODS teardown.
       METHODS new_turtle.

       METHODS is_turtle FOR TESTING.
       METHODS turtle_size FOR TESTING.
       METHODS turtle_move FOR TESTING.
       METHODS turtle_move_offset FOR TESTING.
       METHODS turtle_turn FOR TESTING.
       METHODS turtle_turn_radian FOR TESTING.
       METHODS turtle_set_pen_width FOR TESTING.
       METHODS turtle_set_pen_color FOR TESTING.
       METHODS turtle_examples FOR TESTING.
   ENDCLASS.

   CLASS ltc_turtles IMPLEMENTATION.

     METHOD setup.
       new_interpreter( ).
     ENDMETHOD.                    "setup

     METHOD teardown.
       FREE mo_int.
     ENDMETHOD.                    "teardown

     METHOD new_turtle.
       scheme( code = |(define t (turtles 600 400 300 200 90) )|
               expected = 't' ).
     ENDMETHOD.

     METHOD is_turtle.
       new_turtle( ).
       scheme( code = |(turtles? 1)|
               expected = '#f' ).
       scheme( code = |(turtles? t)|
               expected = '#t' ).
     ENDMETHOD.

     METHOD turtle_size.
       new_turtle( ).
       scheme( code = |(turtles-width t)|
               expected = '600' ).
       scheme( code = |(turtles-height t)|
               expected = '400' ).
       scheme( code = |(turtle-state t)|
               expected = '(#(300 200 90.0))' ).
     ENDMETHOD.

     METHOD turtle_move.
       new_turtle( ).
       scheme( code = |(move 100 t) |
               expected = c_abap_turtle ).
       scheme( code = |(turtle-state t)|
               expected = '(#(300 300 90.0))' ).
     ENDMETHOD.

     METHOD turtle_move_offset.
       new_turtle( ).
       scheme( code = |(move-offset 50 50 t) |
               expected = c_abap_turtle ).
       scheme( code = |(turtle-state t)|
               expected = '(#(350 250 90.0))' ).
     ENDMETHOD.

     METHOD turtle_turn.
       new_turtle( ).
       scheme( code = |(turn 90 t) |
               expected = c_abap_turtle ).
       scheme( code = |(turtle-state t)|
               expected = '(#(300 200 180.0))' ).
     ENDMETHOD.

     METHOD turtle_turn_radian.
       new_turtle( ).
       scheme( code = |(turn/radians { c_pi } t) |
               expected = c_abap_turtle ).
       scheme( code = |(turtle-state t)|
               expected = '(#(300 200 270.0))' ).
     ENDMETHOD.

     METHOD turtle_set_pen_width.
       new_turtle( ).
       scheme( code = |(set-pen-width t 5) |
               expected = c_abap_turtle ).
       scheme( code = |(turtles-pen-width t)|
               expected = '5' ).
     ENDMETHOD.

     METHOD turtle_set_pen_color.
       new_turtle( ).
       scheme( code = |(set-pen-color t "23444") |
               expected = c_abap_turtle ).
       scheme( code = |(turtles-pen-color t)|
               expected = `"23444"` ).
     ENDMETHOD.

     METHOD turtle_examples.
       new_turtle( ).
       scheme( code = |(regular-poly 8 100 t) |  " sides radius
               expected = c_abap_turtle ).
       scheme( code = |(regular-polys 10 100 t) |  " number, side
               expected = c_abap_turtle ).
     ENDMETHOD.

   ENDCLASS.
