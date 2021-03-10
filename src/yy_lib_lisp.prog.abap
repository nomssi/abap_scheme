*&---------------------------------------------------------------------*
*& Include           YY_LIB_LISP
*& https://github.com/nomssi/abap_scheme
*& https://github.com/mydoghasworms/abap-lisp
*& Lisp interpreter written in ABAP
*& Copy and paste this code into a type I (include) program
*& MIT License (see below)
*& Martin Ceronio, martin.ceronio@infosize.co.za June 2015
*& Jacques Nomssi Nzali, www.informatik-dv.com April 2018
*& Turtle Graphics placed under The Unlicense by Frederik Hudák
*&---------------------------------------------------------------------*
*  The MIT License (MIT)
*
*  Copyright (c) 2021 Jacques Nomssi Nzali
*  Copyright (c) 2015 Martin Ceronio
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

  TYPES tv_char TYPE c LENGTH 1.
  TYPES tv_char2 TYPE c LENGTH 2.
  TYPES tv_byte TYPE int1.

  TYPES tv_real TYPE decfloat34.  " floating point data type
  TYPES tv_int TYPE int8.         " integer data type, use int8 and max_int8 if available
  TYPES tv_index TYPE tv_int.
  TYPES tv_flag TYPE abap_bool.

  TYPES tv_hex04 TYPE c LENGTH 4. " should only contain hex digits
  TYPES tv_xword TYPE x LENGTH 2.

  CONSTANTS:
    c_max_int   TYPE tv_int VALUE cl_abap_math=>max_int8,          "  cl_abap_math=>max_int8.
    c_max_float TYPE tv_real VALUE cl_abap_math=>max_decfloat34,
    c_min_float TYPE tv_real VALUE cl_abap_math=>min_decfloat34.

  CONSTANTS:
    c_escape_char           TYPE tv_char VALUE '\',
    c_text_quote            TYPE tv_char VALUE '"',
    c_semi_colon            TYPE tv_char VALUE ';',
    c_vertical_line         TYPE tv_char VALUE '|',

    c_lisp_dot              TYPE tv_char VALUE '.',
    c_lisp_quote            TYPE tv_char VALUE '''', "LISP single quote = QUOTE
    c_lisp_backquote        TYPE tv_char VALUE '`',  " backquote = quasiquote
    c_lisp_unquote          TYPE tv_char VALUE ',',
    c_lisp_splicing         TYPE tv_char VALUE '@',
    c_lisp_unquote_splicing TYPE c LENGTH 2 VALUE '@'.

  CONSTANTS:
    c_lisp_slash     TYPE tv_char VALUE '/',
    c_lisp_directive TYPE tv_char VALUE '!'.

  CONSTANTS:
    c_open_paren  TYPE tv_char VALUE '(',
    c_close_paren TYPE tv_char VALUE ')',
    c_lisp_equal  TYPE tv_char VALUE '='.
  CONSTANTS:
    c_lisp_hash     TYPE tv_char VALUE '#',
    c_lisp_comment  TYPE tv_char VALUE c_semi_colon,
    c_open_curly    TYPE tv_char VALUE '{',
    c_close_curly   TYPE tv_char VALUE '}',
    c_open_bracket  TYPE tv_char VALUE '[',
    c_close_bracket TYPE tv_char VALUE ']'.

  CONSTANTS:
    c_lisp_eof       TYPE tv_hex04 VALUE 'FEFF', " we do not expect this in source code
    c_lisp_input     TYPE string VALUE 'ABAP Lisp Input' ##NO_TEXT,
    c_lisp_nil       TYPE string VALUE `'()`,
    c_expr_separator TYPE string VALUE ` `,   " multiple expression output
    c_undefined      TYPE string VALUE '<undefined>'.

  CONSTANTS:
    c_error_message         TYPE string VALUE 'Error in processing' ##NO_TEXT,
    c_error_incorrect_input TYPE string VALUE 'Incorrect input' ##NO_TEXT,
    c_error_unexpected_end  TYPE string VALUE 'Unexpected end' ##NO_TEXT,
    c_error_eval            TYPE string VALUE 'EVAL( ) came up empty-handed' ##NO_TEXT,
    c_error_no_exp_in_body  TYPE string VALUE 'no expression in body' ##NO_TEXT.

  CONSTANTS:
    c_area_eval  TYPE string VALUE `Eval` ##NO_TEXT,
    c_area_parse TYPE string VALUE `Parse` ##NO_TEXT,
    c_area_radix TYPE string VALUE 'Radix' ##NO_TEXT.

  CONSTANTS:
    c_lisp_else TYPE string VALUE 'else' ##NO_TEXT,
    c_lisp_then TYPE c LENGTH 2 VALUE '=>'.
  CONSTANTS:
    c_eval_append           TYPE string VALUE 'append' ##NO_TEXT,
    c_eval_cons             TYPE string VALUE 'cons' ##NO_TEXT,
    c_eval_list             TYPE string VALUE 'list' ##NO_TEXT,

    c_eval_quote            TYPE string VALUE 'quote' ##NO_TEXT,
    c_eval_quasiquote       TYPE string VALUE 'quasiquote' ##NO_TEXT,
    c_eval_unquote          TYPE string VALUE 'unquote' ##NO_TEXT,
    c_eval_unquote_splicing TYPE string VALUE 'unquote-splicing' ##NO_TEXT.

  CONSTANTS:
    c_binary_digits   TYPE c LENGTH 2 VALUE '01',
    c_octal_digits    TYPE c LENGTH 8 VALUE '01234567',
    c_decimal_digits  TYPE c LENGTH 10 VALUE '0123456789',
    c_hex_digits      TYPE c LENGTH 16 VALUE '0123456789ABCDEF',
    c_hex_digits_long TYPE c LENGTH 22 VALUE '0123456789aAbBcCdDeEfF'.

  CONSTANTS:
    c_hex_alpha_lowercase TYPE c LENGTH 6 VALUE 'abcdef',
    c_abcde               TYPE string VALUE `ABCDEFGHIJKLMNOPQRSTUVWXYZ`, " sy-abcde
    c_special_initial     TYPE string VALUE '!$%&*/:<=>?@^_~'.

  CONSTANTS:
    c_plus_sign            TYPE tv_char VALUE `+`,
    c_minus_sign           TYPE tv_char VALUE `-`,
    c_explicit_sign        TYPE string VALUE '+-',
    c_exponent_marker      TYPE tv_char VALUE `E`,
    c_exponent_marker_long TYPE string VALUE `eEsSfFdDlL`.

  CONSTANTS:
    c_lisp_pos_zero TYPE string VALUE '+0.0',
    c_lisp_neg_zero TYPE string VALUE '-0.0',
    c_lisp_pos_inf  TYPE string VALUE '+INF.0',
    c_lisp_neg_inf  TYPE string VALUE '-INF.0',
    c_lisp_pos_nan  TYPE string VALUE '+NAN.0',
    c_lisp_neg_nan  TYPE string VALUE '-NAN.0',
    c_lisp_pos_img  TYPE string VALUE '+I',
    c_lisp_neg_img  TYPE string VALUE '-I'.

  CONSTANTS:
    c_complex_polar TYPE tv_char VALUE '@',
    c_imaginary_marker  TYPE tv_char VALUE 'I',
    c_imaginary_pos_inf TYPE string VALUE '+INF.0I',
    c_imaginary_neg_inf TYPE string VALUE '-INF.0I',
    c_imaginary_pos_nan TYPE string VALUE '+NAN.0I',
    c_imaginary_neg_nan TYPE string VALUE '-NAN.0I'.

  CONSTANTS:
    c_pattern_radix     TYPE string VALUE 'oObBdDxX',
    c_pattern_exactness TYPE string VALUE 'eEiI'.

  CONSTANTS:
    c_number_exact         TYPE tv_char2 VALUE 'eE',
    c_number_inexact       TYPE tv_char2 VALUE 'iI',
    c_pattern_inexact      TYPE string VALUE '.eE',
    c_pattern_inexact_long TYPE string VALUE '.eEsSfFdDlL'.

  CONSTANTS:
    c_number_octal   TYPE tv_char2 VALUE 'oO',
    c_number_binary  TYPE tv_char2 VALUE 'bB',
    c_number_decimal TYPE tv_char2 VALUE 'dD',
    c_number_hex     TYPE tv_char2 VALUE 'xX'.

  CONSTANTS
    c_pi TYPE tv_real VALUE '3.141592653589793238462643383279502884197169'.

  CLASS lcl_demo_output DEFINITION.
    PUBLIC SECTION.
      METHODS constructor IMPORTING out TYPE REF TO if_demo_output.
      METHODS write IMPORTING iv_text TYPE any.
      METHODS display.
      METHODS begin_section IMPORTING iv_text TYPE any.
    PRIVATE SECTION.
      DATA out TYPE REF TO if_demo_output.
  ENDCLASS.

  DATA gv_lisp_trace TYPE tv_flag VALUE abap_false ##NEEDED.
  DATA go_out TYPE REF TO lcl_demo_output.

  CLASS lcl_demo_output IMPLEMENTATION.

    METHOD constructor.
      me->out = out.
    ENDMETHOD.

    METHOD write.
      out->write( iv_text ).
    ENDMETHOD.

    METHOD begin_section.
      out->begin_section( iv_text ).
    ENDMETHOD.

    METHOD display.
      out->display( ).
    ENDMETHOD.

  ENDCLASS.

* Macro to simplify the definition of a native procedure
  DEFINE _proc_meth.
    METHODS &1 IMPORTING list TYPE REF TO lcl_lisp
               RETURNING VALUE(result) TYPE REF TO lcl_lisp
               RAISING lcx_lisp_exception.
  END-OF-DEFINITION.

  DEFINE _validate.
    IF &1 IS NOT BOUND.
      lcl_lisp=>throw( c_error_incorrect_input ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _validate_mutable.
    _validate &1.
    IF &1->mutable EQ abap_false.
      throw( |constant { &2 } cannot be changed| ) ##NO_TEXT.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _validate_type.
    _validate &1.
    IF &1->type NE type_&3.
      &1->raise( ` is not a ` && &4 && ` in ` && &2 ) ##NO_TEXT.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _validate_integer.
    _validate &1.
    IF &1->type NE type_integer.
      &1->raise( ` is not an integer in ` && &2 ) ##NO_TEXT.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _validate_byte.
    _validate &1.
    IF NOT ( &1->type = type_integer AND CAST lcl_lisp_integer( &1 )->int BETWEEN 0 AND 255 ).
      &1->raise( ` is not a byte in ` && &2 ) ##NO_TEXT.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _validate_index.
    _validate_integer &1 &2.
    IF CAST lcl_lisp_integer( &1 )->int LT 0.
      &1->raise( ` must be non-negative in ` && &2 ) ##NO_TEXT.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _validate_char.
    _validate_type &1 &2 char `char`.
  END-OF-DEFINITION.

  DEFINE _validate_string.
    _validate_type &1 &2 string `string`.
  END-OF-DEFINITION.

  DEFINE _validate_vector.
    _validate_type &1 &2 vector `vector`.
  END-OF-DEFINITION.

  DEFINE _validate_bytevector.
    _validate_type &1 &2 bytevector `bytevector`.
  END-OF-DEFINITION.

  DEFINE _validate_port.
    _validate_type &1 &2 port `port`.
  END-OF-DEFINITION.

  DEFINE _validate_complex.
    _validate_type &1 &2 complex `complex`.
  END-OF-DEFINITION.

  DEFINE _validate_turtle.
    _validate_type &1 &2 abap_turtle `turtle`.
  END-OF-DEFINITION.

  DEFINE _validate_number.
    _validate &1.
    CASE &1->type.
      WHEN type_integer
        OR type_real
        OR type_rational
        OR type_complex.
      WHEN OTHERS.
        &1->raise_nan( &2 ) ##NO_TEXT.
    ENDCASE.
  END-OF-DEFINITION.

  DEFINE _get_arg0_as_number.
    DATA lo_number TYPE REF TO lcl_lisp_number.
    _validate list.
    _validate_number list->car &1.
    _assert_one_param &1.
    lo_number ?= list->car.
  END-OF-DEFINITION.

  DEFINE _validate_escape.
    _validate &1.
    IF &1->type NE type_escape_proc.
      &1->raise( ` is not an escape procedure in ` && &2 ) ##NO_TEXT.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _to_integer.
    &2 = CAST lcl_lisp_integer( &1 )->int.
  END-OF-DEFINITION.

  DEFINE _to_real.
    &2 = CAST lcl_lisp_real( &1 )->real.
  END-OF-DEFINITION.

  DEFINE _values_get_next.
    IF &1->type EQ type_values.
      lo_head = CAST lcl_lisp_values( &1 )->head.

      IF lo_head IS BOUND AND lo_head NE nil.
        _validate lo_head->car.
        &1 = lo_head->car.

        lo_head = lo_head->cdr.
      ELSE.
        &1 = nil.
      ENDIF.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _get_real.
    _validate &2.
    cell = &2.
    _values_get_next cell.
    CASE cell->type.
      WHEN type_integer.
        _to_integer cell &1.
      WHEN type_real.
        _to_real cell &1.
      WHEN type_rational.
        lo_rat ?= cell.
        &1 = lo_rat->int / lo_rat->denominator.
      WHEN type_complex.
        cell->no_complex( &3 ).
      WHEN OTHERS.
        cell->raise_nan( &3 ).
    ENDCASE.
    cell_exact = CAST lcl_lisp_number( cell )->exact.
  END-OF-DEFINITION.

  DEFINE _get_int_definition.
    DATA exact TYPE tv_flag.
    DATA lo_int TYPE REF TO lcl_lisp_integer.
    DATA lo_rat TYPE REF TO lcl_lisp_rational.
    DATA lo_real TYPE REF TO lcl_lisp_real.
  END-OF-DEFINITION.

  DEFINE _get_int.
    _validate: &1, &1->car.

    CASE &1->car->type.
      WHEN type_integer.
         lo_int = CAST lcl_lisp_integer( &1->car ).
         &2 = lo_int->int.
         exact = lo_int->exact.
      WHEN type_rational.
         lo_rat = CAST lcl_lisp_rational( &1->car ).
         IF lo_rat->denominator EQ 1.
           &2 = lo_rat->int.
           exact = lo_rat->exact.
         ELSE.
           &1->car->raise( | is not an integer in [{ &3 }]| ).
         ENDIF.
      WHEN type_real.
         TRY.
             lo_real = CAST lcl_lisp_real( &1->car ).
             IF lo_real->infnan EQ abap_true.
               lo_real->raise( | is not an integer in [{ &3 }]| ).
             ENDIF.
             MOVE EXACT lo_real->real TO &2.
             exact = lo_real->exact.
         CATCH cx_sy_conversion_error.
           list->car->raise( | is not an integer in [{ &3 }]| ).
         ENDTRY.
      WHEN OTHERS.
        list->car->raise( | is not an integer in [{ &3 }]| ).
    ENDCASE.
  END-OF-DEFINITION.

  DEFINE _get_2_ints.
    DATA carry TYPE tv_real.
    DATA nq TYPE tv_int.  " quotient
    DATA n1 TYPE tv_int.
    DATA n2 TYPE tv_int.
    DATA old_exact TYPE tv_flag.
    _get_int_definition.

    _get_int list n1 &1.
    old_exact = exact.
    _get_int list->cdr n2 &1.

    IF n2 EQ 0.
      throw( | second parameter cannot be zero in [{ &1 }]| ).
    ENDIF.
    IF exact EQ abap_true.
      exact = old_exact.
    ENDIF.

    list->cdr->assert_last_param( ).
    carry = n1 / n2.    " first convert to float, or 3 = trunc( 5 / 2 ) coercion happens with ABAP rules
  END-OF-DEFINITION.

  DEFINE _data_local_numeric.
    DATA lo_rat TYPE REF TO lcl_lisp_rational.
    DATA lo_int TYPE REF TO lcl_lisp_integer.
    DATA lo_real TYPE REF TO lcl_lisp_real.
    DATA lo_z TYPE REF TO lcl_lisp_complex.
  END-OF-DEFINITION.

  DEFINE _data_local_numeric_cell.
    DATA cell TYPE REF TO lcl_lisp.
    DATA lo_head TYPE REF TO lcl_lisp.
    DATA cell_exact TYPE tv_flag.
    DATA lv_exact TYPE tv_flag.
    _data_local_numeric.
  END-OF-DEFINITION.

  DEFINE _next_to_compare.
    cell = lo_ptr->car.
    _values_get_next cell.
    _validate_number cell &1.
    other = CAST lcl_lisp_number( cell )->get_state( &1 ).

    IF other-infnan EQ abap_true AND ( other-ref = lcl_lisp_number=>nan OR other-ref = lcl_lisp_number=>neg_nan ).
      RETURN.
    ENDIF.
    IF other-type EQ type_complex.
      cell->raise( 'comparison not defined for complex numbers' ).
    ENDIF.
  END-OF-DEFINITION.

* Macro that implements the logic for the comparison native
* procedures, where only the comparison operator differs
  DEFINE _comparison.
    " currently not transitive !? --------------------------------
    DATA self  TYPE ts_result.
    DATA other  TYPE ts_result.
    DATA cell TYPE REF TO lcl_lisp.
    DATA lo_head TYPE REF TO lcl_lisp.

    result = false.
    _validate: list, list->car, list->cdr.
    IF list->cdr->type NE type_pair.
      throw( c_error_incorrect_input ).
    ENDIF.

    DATA(lo_ptr) = list.
    _next_to_compare &2.

    lo_ptr = lo_ptr->cdr.
    WHILE lo_ptr->type EQ type_pair.
      self = other.
      _next_to_compare &2.

      CASE other-type.
        WHEN type_integer.
          CASE self-type.
            WHEN type_integer.
              IF self-int &1 other-int.
                RETURN.
              ENDIF.
            WHEN type_rational.
              IF self-nummer &1 other-int * self-denom.
                RETURN.
              ENDIF.
            WHEN type_real.
              IF self-real &1 other-int.
                RETURN.
              ENDIF.
            WHEN OTHERS.
              cell->raise_nan( &2 ).
          ENDCASE.

        WHEN type_real.
          CASE self-type.
            WHEN type_integer.
              IF self-int &1 other-real.
                RETURN.
              ENDIF.
            WHEN type_rational.
              IF self-nummer / self-denom &1 other-real.
                RETURN.
              ENDIF.
            WHEN type_real.
              IF self-real &1 other-real.
                RETURN.
              ENDIF.
            WHEN OTHERS.
              cell->raise_nan( &2 ).
          ENDCASE.

        WHEN type_rational.
          CASE self-type.
            WHEN type_integer.
              IF self-int * other-denom &1 other-nummer.
                RETURN.
              ENDIF.
            WHEN type_rational.
              IF self-nummer * other-denom &1 other-nummer * self-denom.
                RETURN.
              ENDIF.
            WHEN type_real.
              IF self-real &1 other-nummer / other-denom.
                RETURN.
              ENDIF.
            WHEN type_complex.
               cell->raise_nan( &2 ).
          ENDCASE.

        WHEN OTHERS.
          cell->raise( `Invalid number type in ` && &2 ).

      ENDCASE.

      lo_ptr = lo_ptr->cdr.
    ENDWHILE.
    result = true.
  END-OF-DEFINITION.

  DEFINE _assert_one_param.
    IF list->cdr NE nil.
      throw( |Only one parameter expected in { &1 }| ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _assert_last_param.
    IF &1->cdr NE nil.
      &1->raise( | Parameter mismatch| ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _arg_is_type. " argument in list->car
    _validate: list, list->car.
    _assert_one_param &2.
    result = false.
    CHECK list->car IS BOUND.
    IF list->car->type EQ type_&1.
      result = true.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _catch_arithmetic_error.
    CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
      throw( lx_error->get_text( ) ).
  END-OF-DEFINITION.

* Macro that implements the logic for call of ABAP math statements
  DEFINE _math_to_int.
    DATA carry TYPE tv_real.
    _data_local_numeric_cell.

    result = nil.
    _validate list.
    _validate_number list->car &2.
    IF CAST lcl_lisp_number( list->car )->infnan EQ abap_true.
      result = list->car.
      RETURN.
    ENDIF.
    TRY.
        _get_real carry list->car &2.
        _assert_one_param &2.

        result = lcl_lisp_new=>integer( value = &1( carry )
                                        iv_exact = cell_exact ).
    _catch_arithmetic_error.
    ENDTRY.
  END-OF-DEFINITION.

  DEFINE _trigonometric_header.
    DATA x TYPE tv_real.
    DATA y TYPE tv_real.
    DATA a TYPE f.
    DATA b TYPE f.
    DATA lo_number TYPE REF TO lcl_lisp_number.
    DATA cell_exact TYPE tv_flag.
    DATA res TYPE ts_result.

    result = nil.
    _validate list.
    _validate_number list->car &1.
    TRY.
         _assert_one_param &1.

         DATA lo_head TYPE REF TO lcl_lisp.
         DATA cell TYPE REF TO lcl_lisp.
         cell = list->car.
         _values_get_next cell.

         lo_number ?= cell.
         DATA(number) =  lo_number->get_state( &1 ).
         cell_exact = lo_number->exact.

         x = lcl_lisp_number=>get_real( self = number-real_part
                                        operation = &1 ).
         y = lcl_lisp_number=>get_real( self = number-imag_part
                                        operation = &1 ).
         a = x.
         b = y.
  END-OF-DEFINITION.

*--------------------------------------------------------------------*
* EXCEPTIONS
*--------------------------------------------------------------------*

*----------------------------------------------------------------------*
*  CLASS lcx_lisp_exception DEFINITION
*----------------------------------------------------------------------*
*  General Lisp exception
*----------------------------------------------------------------------*
  CLASS lcx_lisp_exception DEFINITION INHERITING FROM cx_dynamic_check.
    PUBLIC SECTION.
      METHODS constructor IMPORTING message TYPE string
                                    area    TYPE string OPTIONAL.
      METHODS get_text REDEFINITION.
    PROTECTED SECTION.
      DATA mv_area TYPE string.
      DATA mv_message TYPE string.
  ENDCLASS.                    "lcx_lisp_exception DEFINITION

  CLASS lcx_lisp_radix  DEFINITION INHERITING FROM lcx_lisp_exception.
    PUBLIC SECTION.
      METHODS constructor IMPORTING message TYPE string.
  ENDCLASS.

  CLASS lcl_lisp DEFINITION DEFERRED.
  CLASS lcl_lisp_environment DEFINITION DEFERRED.

  TYPES: BEGIN OF ts_continuation,
           elem TYPE REF TO lcl_lisp,
           env  TYPE REF TO lcl_lisp_environment,
           next TYPE REF TO data,
         END OF ts_continuation.
  TYPES tr_continuation TYPE REF TO ts_continuation.

  CLASS lcx_lisp_escape DEFINITION INHERITING FROM cx_dynamic_check.
    PUBLIC SECTION.
      DATA ms_cont TYPE ts_continuation READ-ONLY.
      DATA mo_param TYPE REF TO lcl_lisp.
      METHODS throw RAISING lcx_lisp_escape.

      METHODS constructor IMPORTING is_cont  TYPE ts_continuation
                                    io_param TYPE REF TO lcl_lisp.
      METHODS get_text REDEFINITION.
      METHODS new_continuation IMPORTING cont           TYPE ts_continuation
                               RETURNING VALUE(rs_cont) TYPE ts_continuation
                               RAISING   lcx_lisp_escape lcx_lisp_exception.
  ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcx_lisp_exception IMPLEMENTATION
*----------------------------------------------------------------------*
  CLASS lcx_lisp_exception IMPLEMENTATION.
    METHOD constructor.
      super->constructor( ).
      IF message IS INITIAL.
        mv_message = c_error_message.
      ELSE.
        mv_message = message.
      ENDIF.
      IF area IS NOT INITIAL.
        mv_area = area && `: `.
      ENDIF.
    ENDMETHOD.                    "constructor

    METHOD get_text.
      result = mv_area && mv_message.
    ENDMETHOD.                    "get_text

  ENDCLASS.                    "lcx_lisp_exception IMPLEMENTATION

  CLASS lcx_lisp_radix IMPLEMENTATION.
    METHOD constructor.
      super->constructor( message = message
                          area = c_area_radix ).
    ENDMETHOD.
  ENDCLASS.

  TYPES tv_type TYPE tv_char.
  TYPES tv_category TYPE tv_char.

  TYPES: BEGIN OF ENUM tv_port_type BASE TYPE tv_char,
           textual VALUE IS INITIAL,
           binary  VALUE 'b',
         END OF ENUM tv_port_type.

  CONSTANTS:
    tv_category_standard TYPE tv_category VALUE space,
    tv_category_macro    TYPE tv_category VALUE 'X',
    tv_category_escape   TYPE tv_category VALUE '@'.

*  Type definitions for the various elements
  CONSTANTS:
    type_symbol        TYPE tv_type VALUE 'S',
    type_integer       TYPE tv_type VALUE 'N',
    type_real          TYPE tv_type VALUE 'R',
    type_complex       TYPE tv_type VALUE 'z',
    type_rational      TYPE tv_type VALUE 'r',
    type_bigint        TYPE tv_type VALUE 'B',   " not used?
    type_string        TYPE tv_type VALUE '"',

    type_boolean       TYPE tv_type VALUE 'b',
    type_char          TYPE tv_type VALUE 'c',
    type_null          TYPE tv_type VALUE '0',
    type_pair          TYPE tv_type VALUE 'C',
    type_lambda        TYPE tv_type VALUE 'L',
    type_call_cc       TYPE tv_type VALUE 'k',
    type_case_lambda   TYPE tv_type VALUE 'A',
    type_native        TYPE tv_type VALUE 'n',
    type_primitive     TYPE tv_type VALUE 'I',
    type_syntax        TYPE tv_type VALUE 'y',
    type_hash          TYPE tv_type VALUE 'h',
    type_vector        TYPE tv_type VALUE 'v',
    type_bytevector    TYPE tv_type VALUE '8',
    type_port          TYPE tv_type VALUE 'o',
    type_not_defined   TYPE tv_type VALUE space,

    type_escape_proc   TYPE tv_type VALUE '@',

    " Types for ABAP integration:
    type_abap_data     TYPE tv_type VALUE 'D',
    type_abap_table    TYPE tv_type VALUE 'T',
    type_abap_query    TYPE tv_type VALUE 'q',
    type_abap_sql_set  TYPE tv_type VALUE 's',
    type_abap_function TYPE tv_type VALUE 'F',
*    type_abap_class    TYPE tv_type VALUE 'a',
*    type_abap_method   TYPE tv_type VALUE 'm',

    "type_env_spec    TYPE tv_type VALUE 'e',
    type_record_type   TYPE tv_type VALUE 'Y',
    type_values        TYPE tv_type VALUE 'V',
    type_abap_turtle   TYPE tv_type VALUE 't'.  " for Turtles graphic

  CLASS lcl_lisp_iterator DEFINITION DEFERRED.
  CLASS lcl_lisp_new DEFINITION DEFERRED.
  CLASS lcl_lisp_number DEFINITION DEFERRED.
  CLASS lcl_lisp_interpreter DEFINITION DEFERRED.

  TYPES: BEGIN OF ts_number,
           subtype TYPE tv_type,
           int     TYPE tv_int,
           real    TYPE tv_real,
           nummer  TYPE tv_int,
           denom   TYPE tv_int,
           infnan  TYPE tv_flag,
           exact   TYPE tv_flag,
           ref     TYPE REF TO lcl_lisp_number,
         END OF ts_number.

  TYPES: BEGIN OF ts_result,
           type      TYPE tv_type.
           INCLUDE TYPE ts_number AS real_part.
  TYPES:   imag_part TYPE ts_number,
           operation TYPE string,
         END OF ts_result.


* Single element that will capture cons cells, atoms etc.
*----------------------------------------------------------------------*
*       CLASS lcl_lisp DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_lisp DEFINITION CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      TYPES tt_element TYPE STANDARD TABLE OF REF TO lcl_lisp WITH DEFAULT KEY.
      " Symbolic expression (S-expression)
      DATA type TYPE tv_type.

*     Can this be replaced by a mesh? cf. DEMO_RND_PARSER_AST
      DATA mutable TYPE tv_flag VALUE abap_true READ-ONLY.

      DATA category TYPE tv_category.
      DATA value TYPE string.

      DATA car TYPE REF TO lcl_lisp.
      DATA cdr TYPE REF TO lcl_lisp.
      DATA mv_label TYPE string.

      CLASS-METHODS class_constructor.

      CLASS-DATA nil        TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA false      TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA true       TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA undefined  TYPE REF TO  lcl_lisp READ-ONLY.

      CLASS-DATA quote            TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA quasiquote       TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA unquote          TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA unquote_splicing TYPE REF TO  lcl_lisp READ-ONLY.

      CLASS-DATA append           TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA cons             TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA list             TYPE REF TO  lcl_lisp READ-ONLY.

      CLASS-DATA char_alarm       TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA char_backspace   TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA char_delete      TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA char_escape      TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA char_linefeed    TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA char_null        TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA char_return      TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA char_space       TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA char_tab         TYPE REF TO  lcl_lisp READ-ONLY.

      CLASS-DATA new_line   TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA eof_object TYPE REF TO  lcl_lisp READ-ONLY.

*     Specifically for lambdas:
      DATA environment TYPE REF TO lcl_lisp_environment.

*     Utilities
      METHODS to_string RETURNING VALUE(str) TYPE string
                        RAISING   lcx_lisp_exception.
      METHODS to_text RETURNING VALUE(str) TYPE string
                      RAISING   lcx_lisp_exception.

      METHODS new_iterator RETURNING VALUE(ro_iter) TYPE REF TO lcl_lisp_iterator
                           RAISING   lcx_lisp_exception.

      METHODS set_shared_structure RAISING lcx_lisp_exception.

      " Predicates
      METHODS is_equivalent IMPORTING io_elem       TYPE REF TO lcl_lisp
                            RETURNING VALUE(result) TYPE tv_flag
                            RAISING   lcx_lisp_exception.

      METHODS is_equal IMPORTING io_elem       TYPE REF TO lcl_lisp
                                 comp          TYPE REF TO lcl_lisp DEFAULT nil
                                 interpreter   TYPE REF TO lcl_lisp_interpreter OPTIONAL
                                 environment   TYPE REF TO lcl_lisp_environment OPTIONAL
                       RETURNING VALUE(result) TYPE tv_flag
                       RAISING   lcx_lisp_exception.

      METHODS is_eof RETURNING VALUE(flag) TYPE tv_flag.
      METHODS is_procedure RETURNING VALUE(result) TYPE tv_flag.
      METHODS is_number RETURNING VALUE(result) TYPE tv_flag.
      METHODS is_nan RETURNING VALUE(flag) TYPE tv_flag
                     RAISING   lcx_lisp_exception.

      " Errors
      METHODS error_not_a_list IMPORTING context TYPE string DEFAULT space
                               RAISING   lcx_lisp_exception.

      METHODS raise IMPORTING context TYPE string DEFAULT space
                              message TYPE string
                    RAISING   lcx_lisp_exception.

      METHODS raise_nan IMPORTING operation TYPE string
                        RAISING   lcx_lisp_exception.

      METHODS no_complex IMPORTING operation TYPE string
                         RAISING   lcx_lisp_exception.

      CLASS-METHODS throw_no_number IMPORTING message TYPE string
                                    RAISING   lcx_lisp_exception cx_sy_conversion_no_number.

      CLASS-METHODS throw IMPORTING message TYPE string
                                    area    TYPE string DEFAULT c_area_eval
                          RAISING   lcx_lisp_exception.

      CLASS-METHODS radix_throw IMPORTING message TYPE string
                                RAISING   lcx_lisp_radix.

      METHODS assert_last_param  RAISING lcx_lisp_exception.

    PROTECTED SECTION.
      METHODS constructor IMPORTING type TYPE tv_type.
      METHODS list_to_string RETURNING VALUE(str) TYPE string
                             RAISING   lcx_lisp_exception.
      METHODS values_to_string RETURNING VALUE(str) TYPE string
                               RAISING   lcx_lisp_exception.

      METHODS format_quasiquote IMPORTING io_elem TYPE REF TO lcl_lisp
                                EXPORTING ev_skip TYPE tv_flag
                                          ev_str  TYPE string.

      METHODS format_string IMPORTING value      TYPE string
                            RETURNING VALUE(str) TYPE string.
      METHODS display_string IMPORTING value      TYPE string
                             RETURNING VALUE(str) TYPE string.
  ENDCLASS.                    "lcl_lisp DEFINITION

  TYPES tt_lisp TYPE STANDARD TABLE OF REF TO lcl_lisp WITH EMPTY KEY.

  TYPES tt_byte TYPE STANDARD TABLE OF tv_byte WITH EMPTY KEY.

  CLASS lcl_lisp_char DEFINITION INHERITING FROM lcl_lisp
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      CLASS-METHODS new IMPORTING value          TYPE any
                        RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_char.
    PROTECTED SECTION.
      TYPES: BEGIN OF ts_char,
               char TYPE tv_char,
               elem TYPE REF TO lcl_lisp_char,
             END OF ts_char.
      CLASS-DATA char_table TYPE HASHED TABLE OF ts_char WITH UNIQUE KEY char.

      METHODS constructor IMPORTING value TYPE any.
  ENDCLASS.

  CLASS lcl_lisp_char IMPLEMENTATION.

    METHOD new.
      DATA lv_char TYPE tv_char.
      lv_char = value.
      ro_elem = VALUE #( char_table[ char = lv_char ]-elem DEFAULT NEW lcl_lisp_char( lv_char ) ).
    ENDMETHOD.

    METHOD constructor.
      super->constructor( type_char ).
      me->value = value.
      IF value EQ space.   " Special treatment for space,
        me->value = ` `.   " see https://blogs.sap.com/2016/08/10/trailing-blanks-in-character-string-processing/
      ENDIF.
      mutable = abap_false.
      INSERT VALUE #( char = value
                      elem = me ) INTO TABLE char_table.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_null DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
  ENDCLASS.

  CLASS lcl_lisp_boolean DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      METHODS constructor IMPORTING value TYPE any.
  ENDCLASS.

  CLASS lcl_lisp_boolean IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_boolean ).
      me->value = value.
      me->mutable = abap_false.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_string DEFINITION INHERITING FROM lcl_lisp
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
    PROTECTED SECTION.
      METHODS constructor IMPORTING value      TYPE any
                                    iv_mutable TYPE tv_flag.
  ENDCLASS.

  CLASS lcl_lisp_string IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_string ).
      me->value = value.
      mutable = iv_mutable.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_number DEFINITION INHERITING FROM lcl_lisp ABSTRACT.
    PUBLIC SECTION.
      CLASS-METHODS class_constructor.

      METHODS to_real IMPORTING operation      TYPE string OPTIONAL
                                infnan_value   TYPE tv_real OPTIONAL
                                  PREFERRED PARAMETER operation
                      RETURNING VALUE(rv_real) TYPE tv_real
                      RAISING   lcx_lisp_exception.

      METHODS number_to_string IMPORTING iv_radix   TYPE tv_int DEFAULT 10
                               RETURNING VALUE(str) TYPE string
                               RAISING   lcx_lisp_exception.

      CLASS-METHODS atom_to_number IMPORTING iv_atom        TYPE any
                                   RETURNING VALUE(element) TYPE REF TO lcl_lisp_number.

      METHODS get_number_info IMPORTING operation TYPE string
                              RETURNING VALUE(rs_info) TYPE ts_number
                              RAISING   lcx_lisp_exception.
      METHODS get_state IMPORTING operation TYPE string OPTIONAL
                        RETURNING VALUE(state) TYPE ts_result
                        RAISING   lcx_lisp_exception.

      CLASS-METHODS scheme_round IMPORTING float         TYPE tv_real
                                 RETURNING VALUE(rdprec) TYPE tv_real.

      " Arithmetic
      METHODS absolute RETURNING VALUE(num) TYPE REF TO lcl_lisp_number.
      METHODS square RETURNING VALUE(result) TYPE REF TO lcl_lisp_number
                     RAISING   lcx_lisp_exception.

      METHODS sum IMPORTING first TYPE ts_result
                            second TYPE ts_result
                            operation TYPE string DEFAULT '+'
                  RETURNING VALUE(result) TYPE ts_result
                  RAISING   lcx_lisp_exception.

      METHODS add IMPORTING other         TYPE REF TO lcl_lisp_number
                  RETURNING VALUE(result) TYPE REF TO lcl_lisp_number
                  RAISING   lcx_lisp_exception.

      METHODS substract IMPORTING other        TYPE REF TO lcl_lisp_number
                        RETURNING VALUE(result) TYPE REF TO lcl_lisp_number
                        RAISING   lcx_lisp_exception.

      CLASS-METHODS get_real IMPORTING self TYPE ts_number
                                       operation TYPE string OPTIONAL
                                       infnan_value   TYPE tv_real DEFAULT c_max_float
                             RETURNING VALUE(rv_real) TYPE tv_real
                             RAISING   lcx_lisp_exception.

      METHODS add_real IMPORTING self  TYPE ts_number
                                 other TYPE ts_number
                                 operation TYPE string
                       RETURNING VALUE(number) TYPE ts_number
                       RAISING   lcx_lisp_exception.

      METHODS negative IMPORTING number TYPE ts_number
                       RETURNING VALUE(res) TYPE ts_number.

      METHODS neg_sign IMPORTING number TYPE ts_result
                       RETURNING VALUE(res) TYPE ts_result.

      METHODS multiply_real IMPORTING self  TYPE ts_number
                                      other TYPE ts_number
                                      operation TYPE string DEFAULT '*'
                            RETURNING VALUE(number) TYPE ts_number
                            RAISING   lcx_lisp_exception.
      METHODS divide_real IMPORTING self  TYPE ts_number
                                    other TYPE ts_number
                                    operation TYPE string DEFAULT '/'
                          RETURNING VALUE(number) TYPE ts_number
                          RAISING   lcx_lisp_exception.

      METHODS product IMPORTING first TYPE ts_result
                                second TYPE ts_result
                                operation TYPE string DEFAULT '*'
                      RETURNING VALUE(result) TYPE ts_result
                      RAISING   lcx_lisp_exception.

      METHODS multiply IMPORTING other         TYPE REF TO lcl_lisp_number
                       RETURNING VALUE(result) TYPE REF TO lcl_lisp_number
                       RAISING   lcx_lisp_exception.

      METHODS quotient IMPORTING numerator TYPE ts_result
                                 denominator TYPE ts_result
                                 operation TYPE string DEFAULT '/'
                       RETURNING VALUE(result) TYPE ts_result
                       RAISING   lcx_lisp_exception.

      METHODS divide IMPORTING other        TYPE REF TO lcl_lisp_number
                     RETURNING VALUE(result) TYPE REF TO lcl_lisp_number
                     RAISING   lcx_lisp_exception.

      METHODS get_numerator RETURNING VALUE(result) TYPE REF TO lcl_lisp_number
                            RAISING   lcx_lisp_exception.
      METHODS get_denominator RETURNING VALUE(result) TYPE REF TO lcl_lisp_number
                              RAISING   lcx_lisp_exception.

      METHODS to_exact RETURNING VALUE(result) TYPE REF TO lcl_lisp_number
                       RAISING   lcx_lisp_exception.
      METHODS to_inexact RETURNING VALUE(result) TYPE REF TO lcl_lisp_number
                         RAISING   lcx_lisp_exception.

      " Predicates
      METHODS real_is_equal IMPORTING res          TYPE ts_number
                            RETURNING VALUE(equal) TYPE tv_flag
                            RAISING   lcx_lisp_exception.
      METHODS number_is_equal IMPORTING other        TYPE REF TO lcl_lisp_number
                              RETURNING VALUE(equal) TYPE tv_flag.
      METHODS is_integer RETURNING VALUE(flag) TYPE tv_flag
                         RAISING   lcx_lisp_exception.
      METHODS is_finite RETURNING VALUE(result) TYPE REF TO lcl_lisp
                        RAISING   lcx_lisp_exception.
      METHODS is_infinite RETURNING VALUE(result) TYPE REF TO lcl_lisp
                          RAISING   lcx_lisp_exception.

      METHODS number_is_zero IMPORTING is_number   TYPE ts_number
                             RETURNING VALUE(null) TYPE tv_flag.
      METHODS number_is_positive IMPORTING is_number       TYPE ts_number
                                 RETURNING VALUE(positive) TYPE tv_flag.

      METHODS negative_infnan RETURNING VALUE(num) TYPE REF TO lcl_lisp_number
                              RAISING   lcx_lisp_exception.

      DATA exact TYPE tv_flag READ-ONLY.
      DATA infnan TYPE tv_flag READ-ONLY.

      " Numbers
      CLASS-DATA zero TYPE REF TO lcl_lisp_number READ-ONLY.
      CLASS-DATA one TYPE REF TO lcl_lisp_number READ-ONLY.
      CLASS-DATA minus_one TYPE REF TO lcl_lisp_number READ-ONLY.

      CLASS-DATA nan TYPE REF TO lcl_lisp_number READ-ONLY.
      CLASS-DATA neg_nan TYPE REF TO lcl_lisp_number READ-ONLY.

      CLASS-DATA pos_zero TYPE REF TO lcl_lisp_number READ-ONLY.
      CLASS-DATA neg_zero TYPE REF TO lcl_lisp_number READ-ONLY.

      CLASS-DATA inf TYPE REF TO lcl_lisp_number READ-ONLY.
      CLASS-DATA neg_inf TYPE REF TO lcl_lisp_number READ-ONLY.

      CLASS-DATA imaginary TYPE REF TO lcl_lisp_number READ-ONLY.
      CLASS-DATA imaginary_neg TYPE REF TO lcl_lisp_number READ-ONLY.

      CLASS-DATA inf_imag TYPE REF TO lcl_lisp_number READ-ONLY.
      CLASS-DATA neg_inf_imag TYPE REF TO lcl_lisp_number READ-ONLY.
      CLASS-DATA nan_imag TYPE REF TO lcl_lisp_number READ-ONLY.
      CLASS-DATA neg_nan_imag TYPE REF TO lcl_lisp_number READ-ONLY.

    PROTECTED SECTION.
      METHODS constructor IMPORTING type TYPE tv_type.
  ENDCLASS.

  CLASS lcl_lisp_integer DEFINITION INHERITING FROM lcl_lisp_number
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA int TYPE tv_int READ-ONLY.
    PROTECTED SECTION.
      METHODS constructor IMPORTING value    TYPE any
                                    iv_exact TYPE tv_flag.
  ENDCLASS.

  CLASS lcl_lisp_bigint DEFINITION INHERITING FROM lcl_lisp_integer CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      TYPES tv_order TYPE i.
      TYPES tv_sign TYPE tv_char.
      CONSTANTS:
        c_order_smaller TYPE tv_order VALUE -1,
        c_order_equal   TYPE tv_order VALUE 0,
        c_order_larger  TYPE tv_order VALUE +1.

      CONSTANTS:
        c_sign_positive TYPE tv_sign VALUE space,
        c_sign_negative TYPE tv_sign VALUE '-'.

      TYPES tr_bigint TYPE REF TO lcl_lisp_bigint.
      TYPES tt_bigint TYPE STANDARD TABLE OF tr_bigint.

    PROTECTED SECTION.
      TYPES tv_chunk TYPE tv_real.
      TYPES tt_chunk TYPE STANDARD TABLE OF tv_chunk.

      CONSTANTS fzero TYPE tv_chunk VALUE 0.
      CONSTANTS fhalf TYPE tv_chunk VALUE '0.5'.

      CLASS-DATA chunksize TYPE tv_int VALUE 16.
      CLASS-DATA dchunksize TYPE tv_int VALUE 32.
      CLASS-DATA chunkmod TYPE tv_chunk VALUE `1e16`.      "#EC NOTEXT.
      CLASS-DATA dchunkmod TYPE tv_chunk VALUE `1e32`.     "#EC NOTEXT.

      DATA sign TYPE tv_sign.
      DATA dat TYPE tt_chunk.
  ENDCLASS.

  CLASS lcl_lisp_rational DEFINITION INHERITING FROM lcl_lisp_integer
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      CLASS-METHODS new IMPORTING nummer        TYPE tv_int
                                  denom         TYPE tv_int
                                  iv_exact      TYPE tv_flag
                        RETURNING VALUE(result) TYPE REF TO lcl_lisp_number
                        RAISING   lcx_lisp_exception.

      DATA denominator TYPE tv_int READ-ONLY.
      CLASS-METHODS gcd IMPORTING n             TYPE numeric
                                  d             TYPE numeric
                        RETURNING VALUE(result) TYPE tv_int
                        RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      METHODS constructor IMPORTING nummer   TYPE tv_int
                                    denom    TYPE tv_int
                                    iv_exact TYPE tv_flag
                          RAISING   lcx_lisp_exception.
      METHODS normalize RAISING lcx_lisp_exception.
  ENDCLASS.

  CLASS lcl_lisp_integer IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_integer ).
      int = value.
      exact = iv_exact.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_rational IMPLEMENTATION.

    METHOD new.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      IF denom = 0.
        IF nummer = 0.
          result = lcl_lisp_number=>nan.
        ELSEIF nummer GT 0.
          result = lcl_lisp_number=>inf.
        ELSE.
          result = lcl_lisp_number=>neg_inf.
        ENDIF.
      ELSE.
        lo_rat = NEW #( nummer   = nummer
                        denom    = denom
                        iv_exact = iv_exact ).
        IF lo_rat->denominator EQ 1.
          result = NEW lcl_lisp_integer( value    = lo_rat->int
                                         iv_exact = iv_exact ).
        ELSE.
          result = lo_rat.
        ENDIF.
      ENDIF.
    ENDMETHOD.

    METHOD constructor.
      super->constructor( value = nummer
                          iv_exact = iv_exact ).
      type = type_rational.
      denominator = denom.
      normalize( ).
    ENDMETHOD.

    METHOD normalize.
      DATA(g) = gcd( n = int
                     d = denominator ).
      int = trunc( int / g ).
      denominator = trunc( denominator / g ).
      IF denominator = 0.
        IF int = 0.
          raise( ` invalid rational 0/0` ).
        ELSEIF int GE 0.
          raise( ` invalid rational +inf.0` ).
        ELSE.
          raise( ` invalid rational -inf.0` ).
        ENDIF.
      ENDIF.
      IF denominator LT 0.
        int = - int.
        denominator = - denominator.
      ENDIF.
      exact = xsdbool( denominator NE 0 AND exact EQ abap_true ).
    ENDMETHOD.

    METHOD gcd.
      DATA num TYPE tv_int.
      DATA den TYPE tv_int.
      DATA lv_save TYPE tv_int.

      num = n.
      den = d.
      WHILE den NE 0.
        lv_save = den.
        TRY.
            den = num MOD den.  " num - den * trunc( num / den ).
          CATCH cx_sy_arithmetic_error INTO DATA(lx_error).
            lcl_lisp=>throw( lx_error->get_text( ) ).
        ENDTRY.
        num = lv_save.
      ENDWHILE.
      result = abs( num ).
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_real DEFINITION INHERITING FROM lcl_lisp_number
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA real TYPE tv_real READ-ONLY.

      METHODS to_rational EXPORTING ev_nummer TYPE tv_int
                                    ev_denom TYPE tv_int
                          RETURNING VALUE(is_rational) TYPE tv_flag.
      METHODS float_eq IMPORTING iv_real       TYPE tv_real
                       RETURNING VALUE(result) TYPE tv_flag.
      CLASS-METHODS gcd IMPORTING n             TYPE numeric
                                  d             TYPE numeric
                        RETURNING VALUE(result) TYPE tv_real
                        RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      METHODS constructor IMPORTING value  TYPE any
                                    exact  TYPE tv_flag
                                    finite TYPE tv_flag.
  ENDCLASS.

  CLASS lcl_lisp_complex DEFINITION INHERITING FROM lcl_lisp_number
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA magnitude TYPE tv_real READ-ONLY.
      DATA angle TYPE tv_real READ-ONLY.
      DATA real_part TYPE tv_real READ-ONLY.
      DATA imaginary_part TYPE tv_real READ-ONLY.

      DATA zreal TYPE REF TO lcl_lisp_number READ-ONLY.
      DATA zimag TYPE REF TO lcl_lisp_number READ-ONLY.

      METHODS to_string REDEFINITION.

      METHODS is_nan REDEFINITION.
      METHODS is_real RETURNING VALUE(flag) TYPE tv_flag.

    PROTECTED SECTION.
      CLASS-METHODS new_rectangular IMPORTING x              TYPE REF TO lcl_lisp_number DEFAULT zero
                                              y              TYPE REF TO lcl_lisp_number DEFAULT zero
                                    RETURNING VALUE(complex) TYPE REF TO lcl_lisp_complex
                                    RAISING   lcx_lisp_exception.

      CLASS-METHODS new_polar IMPORTING r              TYPE REF TO lcl_lisp_number
                                        angle          TYPE REF TO lcl_lisp_number
                              RETURNING VALUE(complex) TYPE REF TO lcl_lisp_complex
                              RAISING   lcx_lisp_exception.

      METHODS set_rectangular IMPORTING x TYPE REF TO lcl_lisp_number
                                        y TYPE REF TO lcl_lisp_number
                              RAISING   lcx_lisp_exception.
      METHODS set_polar IMPORTING r     TYPE tv_real
                                  alpha TYPE tv_real
                        RAISING   lcx_lisp_exception.

      METHODS to_polar.
      METHODS to_rectangular.
      METHODS to_complex IMPORTING n        TYPE REF TO lcl_lisp_number
                         RETURNING VALUE(z) TYPE REF TO lcl_lisp_complex.

      METHODS constructor.

      METHODS set IMPORTING x TYPE REF TO lcl_lisp_number
                            y TYPE REF TO lcl_lisp_number
                  RAISING   lcx_lisp_exception.
  ENDCLASS.

  CLASS lcl_lisp_pair DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
    PROTECTED SECTION.
  ENDCLASS.

  CLASS lcl_lisp_values DEFINITION INHERITING FROM lcl_lisp_pair FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA size TYPE tv_int READ-ONLY.
      DATA head TYPE REF TO lcl_lisp READ-ONLY.
      METHODS constructor IMPORTING io_elem TYPE REF TO lcl_lisp.
      METHODS add IMPORTING io_value      TYPE REF TO lcl_lisp
                  RETURNING VALUE(result) TYPE REF TO lcl_lisp_values.
    PROTECTED SECTION.
      DATA last TYPE REF TO lcl_lisp.
  ENDCLASS.

  CLASS lcl_lisp_lambda DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA parameter_object TYPE tv_flag.
      METHODS constructor IMPORTING io_car              TYPE REF TO lcl_lisp
                                    io_cdr              TYPE REF TO lcl_lisp
                                    iv_category         TYPE tv_category DEFAULT tv_category_standard
                                    iv_parameter_object TYPE tv_flag DEFAULT abap_false.
    PROTECTED SECTION.
  ENDCLASS.

  CLASS lcl_lisp_lambda IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_lambda ).
      car = io_car.
      cdr = io_cdr.
      category = iv_category.
      parameter_object = iv_parameter_object.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_case_lambda DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      METHODS match IMPORTING args          TYPE REF TO lcl_lisp
                    RETURNING VALUE(result) TYPE REF TO lcl_lisp_lambda
                    RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      DATA clauses TYPE tt_lisp.
  ENDCLASS.

  CLASS lcl_lisp_case_lambda IMPLEMENTATION.

    METHOD match.
*     Find the first lambda where we can assign each argument to its corresponding symbol
      DATA lo_lambda TYPE REF TO lcl_lisp_lambda.
      DATA lo_clause TYPE REF TO lcl_lisp.

      LOOP AT clauses INTO lo_clause.
        IF lo_clause->type NE type_lambda.
          EXIT.
        ENDIF.
        lo_lambda ?= lo_clause.
        DATA(lo_var) = lo_lambda->car.      " pointer to formal parameters
        DATA(lo_arg) = args.                " Pointer to arguments

        CASE lo_arg->type.
          WHEN type_pair OR type_null.   "Do we have a proper list?

            WHILE lo_var NE lcl_lisp=>nil.         " Nil would mean no parameters to map

              IF lo_var->type EQ type_symbol.
*               dotted pair after fixed number of parameters, to be bound to a variable number of arguments
                result = lo_lambda.
                RETURN.
              ENDIF.

*             Part of the list with fixed number of parameters
              IF lo_arg = lcl_lisp=>nil.           " Premature end of arguments
                EXIT.
              ENDIF.

              lo_var = lo_var->cdr.
              lo_arg = lo_arg->cdr.
              CHECK lo_arg IS NOT BOUND.
              lo_arg = lcl_lisp=>nil.
            ENDWHILE.

            IF lo_arg EQ lcl_lisp=>nil AND lo_var EQ lcl_lisp=>nil.  " Correct number of arguments
              result = lo_lambda.
              RETURN.
            ENDIF.

          WHEN type_symbol.
*           args is a symbol to be bound to a variable number of parameters
            result = lo_lambda.
            RETURN.

        ENDCASE.

      ENDLOOP.

      lcl_lisp=>throw( `no clause matching the arguments` ).

    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_symbol DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      METHODS constructor IMPORTING value TYPE any
                                    index TYPE tv_int.
      DATA index TYPE tv_int READ-ONLY.
  ENDCLASS.

  CLASS lcl_lisp_symbol IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_symbol ).
      me->index = index.
      me->value = value.
      me->mutable = abap_false.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_data DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA data TYPE REF TO data.            " for ABAP integrations
    PROTECTED SECTION.
  ENDCLASS.

  CLASS lcl_lisp_table DEFINITION INHERITING FROM lcl_lisp_data FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
    PROTECTED SECTION.
  ENDCLASS.

  INTERFACE lif_native.
    METHODS proc IMPORTING list          TYPE REF TO lcl_lisp
                 RETURNING VALUE(result) TYPE REF TO lcl_lisp
                 RAISING   lcx_lisp_exception.
  ENDINTERFACE.

  INTERFACE lif_log.
    METHODS put IMPORTING io_elem TYPE REF TO lcl_lisp.
    METHODS get RETURNING VALUE(result) TYPE string.
  ENDINTERFACE.

  CLASS lcl_lisp_vector DEFINITION DEFERRED.
  CLASS lcl_lisp_bytevector DEFINITION DEFERRED.
  CLASS lcl_lisp_abapfunction DEFINITION DEFERRED.
  CLASS lcl_lisp_hash DEFINITION DEFERRED.
  CLASS lcl_lisp_escape DEFINITION DEFERRED.

  TYPES: BEGIN OF ts_stream_state,
           length TYPE tv_int,
           index  TYPE tv_index,
           ready  TYPE tv_flag,
           char   TYPE tv_char,
         END OF ts_stream_state.

  INTERFACE lif_stream.
    METHODS next_char RAISING lcx_lisp_exception.
    METHODS peek_char RETURNING VALUE(rv_char) TYPE tv_char.
    METHODS match IMPORTING iv_text           TYPE string
                            iv_case_sensitive TYPE tv_flag DEFAULT abap_true
                  RETURNING VALUE(found)      TYPE tv_flag
                  RAISING   lcx_lisp_exception.
    METHODS expect IMPORTING iv_text           TYPE string
                             iv_case_sensitive TYPE tv_flag DEFAULT abap_true
                   RETURNING VALUE(match)      TYPE tv_flag
                   RAISING   lcx_lisp_exception.
    METHODS: bookmark,
             backtrack.
    DATA state TYPE ts_stream_state.
  ENDINTERFACE.

  INTERFACE lif_input_port.
    METHODS read IMPORTING iv_title        TYPE string OPTIONAL
                 RETURNING VALUE(rv_input) TYPE string.
    METHODS peek_char RETURNING VALUE(rv_char) TYPE tv_char.
    METHODS read_char RETURNING VALUE(rv_char) TYPE tv_char.
    METHODS is_char_ready RETURNING VALUE(rv_flag) TYPE tv_flag.
    METHODS put IMPORTING iv_text TYPE string.
  ENDINTERFACE.

  INTERFACE lif_binary_input_port.
    METHODS read_xstring IMPORTING k TYPE tv_index
                         RETURNING VALUE(rv_input) TYPE xstring.
    METHODS peek_u8 RETURNING VALUE(rv_char) TYPE tv_char.
    METHODS read_u8 RETURNING VALUE(rv_char) TYPE tv_char.
    METHODS is_u8_ready RETURNING VALUE(rv_flag) TYPE tv_flag.
    METHODS put IMPORTING iv_bin TYPE xstring.
  ENDINTERFACE.

  INTERFACE lif_binary_output_port.
    METHODS write_xstring IMPORTING iv_xstring TYPE xstring.
    METHODS write_u8 IMPORTING elem TYPE REF TO lcl_lisp_char.
  ENDINTERFACE.

  CLASS lcl_input_port_to_stream DEFINITION.
    PUBLIC SECTION.
      INTERFACES lif_stream.
      METHODS constructor IMPORTING ii_port TYPE REF TO lif_input_port.
    PRIVATE SECTION.
      DATA mi_port TYPE REF TO lif_input_port.
      DATA saved_state TYPE ts_stream_state.
  ENDCLASS.

  CLASS lcl_input_port_to_stream IMPLEMENTATION.

    METHOD constructor.
      mi_port = ii_port.
      lif_stream~state-length = c_max_int.
      lif_stream~state-ready = abap_true.
    ENDMETHOD.

    METHOD lif_stream~next_char.
      lif_stream~state-char = mi_port->read_char( ).
      lif_stream~state-ready = mi_port->is_char_ready( ).
    ENDMETHOD.

    METHOD lif_stream~peek_char.
      rv_char = mi_port->peek_char( ).
    ENDMETHOD.

    METHOD lif_stream~expect.
      lcl_lisp=>throw( `expect( ) not supported` ).
    ENDMETHOD.

    METHOD lif_stream~match.
      IF lif_stream~expect( iv_text = iv_text
                            iv_case_sensitive = iv_case_sensitive ).
        found = abap_true.
        DO strlen( iv_text ) TIMES.
          mi_port->read_char( ).
        ENDDO.
      ELSE.
        found = abap_false.
      ENDIF.
    ENDMETHOD.

    METHOD lif_stream~bookmark.
      saved_state = lif_stream~state.
    ENDMETHOD.

    METHOD lif_stream~backtrack.
      lif_stream~state = saved_state.
    ENDMETHOD.

  ENDCLASS.

  INTERFACE lif_output_port.
    METHODS write IMPORTING element TYPE REF TO lcl_lisp.
    METHODS display IMPORTING element TYPE REF TO lcl_lisp
                    RAISING   lcx_lisp_exception.
  ENDINTERFACE.

  CLASS lcl_lisp_port DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      INTERFACES lif_input_port.
      INTERFACES lif_binary_input_port.
      INTERFACES lif_output_port.
      INTERFACES lif_binary_output_port.

      ALIASES: read FOR lif_input_port~read,
               write FOR lif_output_port~write,
               display FOR lif_output_port~display,
               set_input_string FOR lif_input_port~put.
      METHODS constructor IMPORTING iv_port_type TYPE tv_port_type DEFAULT textual
                                    iv_input     TYPE tv_flag
                                    iv_output    TYPE tv_flag
                                    iv_error     TYPE tv_flag DEFAULT abap_false.
      METHODS close.
      METHODS close_input.
      METHODS close_output.

      METHODS read_stream IMPORTING iv_title        TYPE string OPTIONAL
                          RETURNING VALUE(rv_input) TYPE string.

      DATA port_type TYPE tv_port_type READ-ONLY.
      DATA input TYPE tv_flag READ-ONLY.
      DATA output TYPE tv_flag READ-ONLY.
      DATA error TYPE tv_flag READ-ONLY.
    PROTECTED SECTION.
*     input is always buffered
      DATA finite_size TYPE tv_flag.

      DATA last_index TYPE tv_index.
      DATA last_input TYPE string.
      DATA last_len TYPE tv_index.

      METHODS block_read RETURNING VALUE(rv_char) TYPE tv_char.

      DATA last_xindex TYPE tv_index.
      DATA last_xlen TYPE tv_index.
      DATA last_xinput TYPE xstring.  " Binary buffer

      METHODS xblock_read RETURNING VALUE(rv_char) TYPE tv_char.

  ENDCLASS.

  CLASS lcl_lisp_buffered_port DEFINITION INHERITING FROM lcl_lisp_port FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      INTERFACES lif_log.
      METHODS write REDEFINITION.
      METHODS read REDEFINITION.
      METHODS lif_input_port~peek_char REDEFINITION.
      METHODS display REDEFINITION.
      METHODS flush RETURNING VALUE(rv_text) TYPE string.
      METHODS constructor IMPORTING iv_port_type TYPE tv_port_type DEFAULT textual
                                    iv_input     TYPE tv_flag
                                    iv_output    TYPE tv_flag
                                    iv_error     TYPE tv_flag DEFAULT abap_false
                                    iv_separator TYPE string DEFAULT c_expr_separator
                                    iv_string    TYPE tv_flag.
    PROTECTED SECTION.
      DATA string_mode TYPE tv_flag.
      DATA buffer TYPE string.
      DATA separator TYPE string.

      METHODS add IMPORTING text TYPE string.
  ENDCLASS.

  INCLUDE yy_lib_turtle.

  CLASS lcl_lisp_turtle DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      METHODS constructor IMPORTING width      TYPE REF TO lcl_lisp_integer
                                    height     TYPE REF TO lcl_lisp_integer
                                    init_x     TYPE REF TO lcl_lisp_integer
                                    init_y     TYPE REF TO lcl_lisp_integer
                                    init_angle TYPE REF TO lcl_lisp_real.
      DATA turtle TYPE REF TO lcl_turtle.
  ENDCLASS.

  CLASS lcl_lisp_sql_result DEFINITION INHERITING FROM lcl_lisp_data
    FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      METHODS constructor IMPORTING io_result TYPE REF TO cl_sql_result_set.
      METHODS clear.
      METHODS close.
    PROTECTED SECTION.
      DATA result_set TYPE REF TO cl_sql_result_set.
  ENDCLASS.

  CLASS lcl_stream DEFINITION DEFERRED.

  CLASS lcl_lisp_new DEFINITION.
    PUBLIC SECTION.
      CLASS-METHODS null RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS undefined RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS symbol IMPORTING value          TYPE any
                                     index          TYPE tv_int OPTIONAL
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_symbol.

      CLASS-METHODS atom IMPORTING value          TYPE string
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                         RAISING   lcx_lisp_exception.

      CLASS-METHODS identifier IMPORTING value          TYPE any
                                         index          TYPE tv_int OPTIONAL
                               RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_symbol
                               RAISING   lcx_lisp_exception.
      CLASS-METHODS boolean IMPORTING value          TYPE any
                            RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS integer IMPORTING value          TYPE any
                                      iv_exact       TYPE tv_flag DEFAULT abap_true
                            RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_integer.
      CLASS-METHODS rational IMPORTING nummer         TYPE tv_int
                                       denom          TYPE tv_int
                                       iv_exact       TYPE tv_flag DEFAULT abap_true
                             RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_number
                             RAISING   lcx_lisp_exception.

      CLASS-METHODS real IMPORTING value          TYPE any
                                   iv_exact       TYPE tv_flag
                                   finite         TYPE tv_flag DEFAULT abap_true
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_real.

      CLASS-METHODS rectangular IMPORTING real           TYPE REF TO lcl_lisp_number DEFAULT lcl_lisp_complex=>zero
                                          imag           TYPE REF TO lcl_lisp_number DEFAULT lcl_lisp_complex=>zero
                                RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_complex
                                RAISING   lcx_lisp_exception.

      CLASS-METHODS polar IMPORTING r              TYPE REF TO lcl_lisp_number
                                    angle          TYPE REF TO lcl_lisp_number
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_complex
                          RAISING   lcx_lisp_exception.

      CLASS-METHODS real_number IMPORTING value          TYPE any
                                          iv_exact       TYPE tv_flag OPTIONAL
                                RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_number
                                RAISING   lcx_lisp_exception cx_sy_conversion_no_number.

      CLASS-METHODS numeric IMPORTING record         TYPE ts_result
                            RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_number
                            RAISING   lcx_lisp_exception.

      CLASS-METHODS create_real IMPORTING record         TYPE ts_number
                                RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_number
                                RAISING   lcx_lisp_exception.

      CLASS-METHODS read_complex IMPORTING iv_radix       TYPE tv_int
                                           iv_radix_char  TYPE tv_char
                                           iv_exact_char  TYPE tv_char
                                           io_stream      TYPE REF TO lcl_stream
                                 RETURNING VALUE(element) TYPE REF TO lcl_lisp
                                 RAISING   lcx_lisp_exception cx_sy_conversion_no_number.

      CLASS-METHODS get_radix IMPORTING iv_radix        TYPE tv_int
                                        iv_radix_char   TYPE tv_char
                              RETURNING VALUE(rv_radix) TYPE tv_int.

      CLASS-METHODS digits_radix IMPORTING iv_radix         TYPE tv_int
                                 RETURNING VALUE(rv_digits) TYPE string
                                 RAISING   lcx_lisp_radix.

      CLASS-METHODS string IMPORTING value          TYPE any
                                     iv_mutable     TYPE tv_flag DEFAULT abap_true
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_string.
      CLASS-METHODS char IMPORTING value          TYPE any
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_char.

      CLASS-METHODS eof RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS esc_charx IMPORTING value          TYPE any
                              RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_char
                              RAISING   lcx_lisp_exception.
      CLASS-METHODS xchar IMPORTING value          TYPE tv_hex04
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_char.
      CLASS-METHODS port IMPORTING iv_port_type   TYPE tv_port_type
                                   iv_input       TYPE tv_flag
                                   iv_output      TYPE tv_flag
                                   iv_error       TYPE tv_flag
                                   iv_buffered    TYPE tv_flag
                                   iv_separator   TYPE string OPTIONAL
                                   iv_string      TYPE tv_flag DEFAULT abap_false
                         RETURNING VALUE(ro_port) TYPE REF TO lcl_lisp_port.

      CLASS-METHODS elem IMPORTING type           TYPE tv_type
                                   value          TYPE any OPTIONAL
                                   parameter      TYPE tv_flag DEFAULT abap_false
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS data IMPORTING ref            TYPE REF TO data OPTIONAL
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_data.
      CLASS-METHODS table IMPORTING ref            TYPE REF TO data OPTIONAL
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_table.
      CLASS-METHODS query IMPORTING value          TYPE any OPTIONAL
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS result_set IMPORTING io_result      TYPE REF TO cl_sql_result_set
                               RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_sql_result.

      CLASS-METHODS cons IMPORTING io_car         TYPE REF TO lcl_lisp DEFAULT lcl_lisp=>nil
                                   io_cdr         TYPE REF TO lcl_lisp DEFAULT lcl_lisp=>nil
                         RETURNING VALUE(ro_cons) TYPE REF TO lcl_lisp.

      CLASS-METHODS promise IMPORTING done           TYPE tv_flag
                                      proc           TYPE REF TO lcl_lisp_lambda
                                      io_env         TYPE REF TO lcl_lisp_environment
                            RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS list3 IMPORTING io_first       TYPE REF TO lcl_lisp
                                    io_second      TYPE REF TO lcl_lisp
                                    io_third       TYPE REF TO lcl_lisp
                          RETURNING VALUE(ro_cons) TYPE REF TO lcl_lisp.

      CLASS-METHODS vector IMPORTING it_vector     TYPE tt_lisp
                                     iv_mutable    TYPE tv_flag
                           RETURNING VALUE(ro_vec) TYPE REF TO lcl_lisp_vector.

      CLASS-METHODS bytevector IMPORTING it_byte      TYPE tt_byte
                                         iv_mutable   TYPE tv_flag
                               RETURNING VALUE(ro_u8) TYPE REF TO lcl_lisp_bytevector.

      CLASS-METHODS lambda IMPORTING io_car              TYPE REF TO lcl_lisp
                                     io_cdr              TYPE REF TO lcl_lisp
                                     io_env              TYPE REF TO lcl_lisp_environment
                                     iv_category         TYPE tv_category DEFAULT tv_category_standard
                                     iv_parameter_object TYPE tv_flag DEFAULT abap_false
                           RETURNING VALUE(ro_lambda)    TYPE REF TO lcl_lisp.

      CLASS-METHODS case_lambda IMPORTING it_clauses       TYPE tt_lisp
                                RETURNING VALUE(ro_lambda) TYPE REF TO lcl_lisp.

      CLASS-METHODS function IMPORTING io_list        TYPE REF TO lcl_lisp
                             RETURNING VALUE(ro_func) TYPE REF TO lcl_lisp_abapfunction
                             RAISING   lcx_lisp_exception.

      CLASS-METHODS hash IMPORTING io_list        TYPE REF TO lcl_lisp
                         RETURNING VALUE(ro_hash) TYPE REF TO lcl_lisp_hash
                         RAISING   lcx_lisp_exception.

      CLASS-METHODS box_quote IMPORTING io_elem        TYPE REF TO lcl_lisp
                              RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS box IMPORTING io_proc        TYPE REF TO lcl_lisp
                                  io_elem        TYPE REF TO lcl_lisp
                        RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS quote IMPORTING io_elem        TYPE REF TO lcl_lisp
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS quasiquote IMPORTING io_elem        TYPE REF TO lcl_lisp
                               RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS unquote IMPORTING io_elem        TYPE REF TO lcl_lisp
                            RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS splice_unquote IMPORTING io_elem        TYPE REF TO lcl_lisp
                                   RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS turtles IMPORTING width            TYPE REF TO lcl_lisp_integer
                                      height           TYPE REF TO lcl_lisp_integer
                                      init_x           TYPE REF TO lcl_lisp_integer
                                      init_y           TYPE REF TO lcl_lisp_integer
                                      init_angle       TYPE REF TO lcl_lisp_real
                            RETURNING VALUE(ro_turtle) TYPE REF TO lcl_lisp_turtle.

      CLASS-METHODS throw_radix IMPORTING message TYPE string
                                RAISING   lcx_lisp_radix.

      CLASS-METHODS values IMPORTING io_elem        TYPE REF TO lcl_lisp DEFAULT lcl_lisp=>nil
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_values.

      CLASS-METHODS escape IMPORTING value          TYPE any
                                     param          TYPE REF TO lcl_lisp
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                           RAISING   lcx_lisp_exception.

      CLASS-METHODS match_initial IMPORTING initial      TYPE tv_char
                                  RETURNING VALUE(match) TYPE tv_flag.
    "PROTECTED SECTION.

      CLASS-METHODS binary_integer IMPORTING value         TYPE csequence
                                   RETURNING VALUE(rv_int) TYPE tv_int
                                   RAISING   lcx_lisp_exception cx_sy_conversion_no_number.
      CLASS-METHODS binary IMPORTING value          TYPE any
                                     iv_exact       TYPE tv_flag
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                           RAISING   lcx_lisp_exception cx_sy_conversion_no_number.

      CLASS-METHODS octal IMPORTING value          TYPE any
                                    iv_exact       TYPE tv_flag
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                          RAISING   lcx_lisp_exception cx_sy_conversion_no_number.
      CLASS-METHODS octal_integer IMPORTING value         TYPE csequence
                                  RETURNING VALUE(rv_int) TYPE tv_int
                                  RAISING   lcx_lisp_exception cx_sy_conversion_no_number.

      CLASS-METHODS hex IMPORTING value          TYPE any
                                  iv_exact       TYPE tv_flag
                        RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                        RAISING   lcx_lisp_exception cx_sy_conversion_no_number.
      CLASS-METHODS hex_integer IMPORTING value         TYPE csequence
                                RETURNING VALUE(rv_int) TYPE tv_int
                                RAISING   lcx_lisp_exception cx_sy_conversion_no_number.

    PRIVATE SECTION.
      CLASS-METHODS match_subsequent_list IMPORTING value        TYPE string
                                          RETURNING VALUE(match) TYPE tv_flag.

      CLASS-METHODS match_sign_subsequent IMPORTING initial      TYPE tv_char
                                          RETURNING VALUE(match) TYPE tv_flag.

      CLASS-METHODS match_dot_subsequent IMPORTING initial      TYPE tv_char
                                         RETURNING VALUE(match) TYPE tv_flag.

      CLASS-METHODS match_peculiar_identifier IMPORTING value        TYPE string
                                              RETURNING VALUE(match) TYPE tv_flag.
  ENDCLASS.

  CLASS lcl_lisp_values IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_values ).
      last = head = io_elem.
    ENDMETHOD.

    METHOD add.
      DATA(lo_value) = lcl_lisp_new=>cons( io_car = io_value ).
      IF head = nil.
        head = last = lo_value.
      ELSE.
        last = last->cdr = lo_value.
      ENDIF.
      size = size + 1.
      result = me.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_port_dummy DEFINITION INHERITING FROM lcl_lisp_port.
    PUBLIC SECTION.
      METHODS read REDEFINITION.
      METHODS write REDEFINITION.
  ENDCLASS.                    "lcl_console DEFINITION

  CLASS lcl_port_dummy IMPLEMENTATION.

    METHOD write.
    ENDMETHOD.

    METHOD read.
      rv_input = lcl_lisp=>eof_object->value+0(1).
    ENDMETHOD.

  ENDCLASS.                    "lcl_console DEFINITION

  CLASS lcl_lisp_port IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_port ).

      port_type = iv_port_type.
      input = iv_input.
      output = iv_output.
      error = iv_error.
    ENDMETHOD.

    METHOD close.
      input = abap_false.
      output = abap_false.
    ENDMETHOD.

    METHOD close_input.
      input = abap_false.
    ENDMETHOD.

    METHOD close_output.
      output = abap_false.
    ENDMETHOD.

    METHOD lif_output_port~write.
      IF output EQ abap_false.
        throw( `Error: Output on closed port` ).
      ENDIF.
      RETURN.
    ENDMETHOD.

    METHOD lif_output_port~display.
      CHECK output EQ abap_true.
      RETURN.
    ENDMETHOD.

    METHOD read_stream.
      DATA lt_fields TYPE STANDARD TABLE OF sval.
      DATA lv_user_response TYPE tv_flag.

      CLEAR rv_input.
      lt_fields = VALUE #( ( tabname = 'ABDBG'     " Text: Input Line
                             fieldname = 'LVALUE'
                             fieldtext = iv_title
                             field_obl = 'X' ) ).

      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          popup_title  = c_lisp_input
          start_column = '45'
          start_row    = '11'
        IMPORTING
          returncode   = lv_user_response
        TABLES
          fields       = lt_fields
        EXCEPTIONS
          OTHERS       = 2.

      CHECK sy-subrc EQ 0
        AND lv_user_response NE 'A'
        AND line_exists( lt_fields[ 1 ] ).

      rv_input = lt_fields[ 1 ]-value.
    ENDMETHOD.

    METHOD set_input_string.
      last_input &&= iv_text.
      last_len = strlen( last_input ).
      finite_size = abap_true.
    ENDMETHOD.

    METHOD block_read.
      IF finite_size EQ abap_false.
        last_input = read_stream( ).
        last_len = strlen( last_input ).
        last_index = 0.
      ELSE.
        last_len = 0.
      ENDIF.
      IF last_len = 0.
        last_input = lcl_lisp=>eof_object->value+0(1).
      ENDIF.
      rv_char = last_input+0(1).
    ENDMETHOD.

    METHOD xblock_read.
      IF finite_size EQ abap_true.
        last_xinput = lcl_lisp=>eof_object->value+0(1).
        last_xlen = 0.
      ELSE.
        last_xinput = read_stream( ).
        last_xlen = strlen( last_input ).
      ENDIF.
      rv_char = last_input+0(1).
    ENDMETHOD.

    METHOD lif_input_port~peek_char.
      CHECK input EQ abap_true.
      IF last_index < last_len.
        rv_char = last_input+last_index(1).
      ELSE.
        rv_char = block_read( ).
      ENDIF.
    ENDMETHOD.

    METHOD lif_input_port~read_char.
      CHECK input EQ abap_true.
      IF last_index < last_len.
        rv_char = last_input+last_index(1).
      ELSE.
        rv_char = block_read( ).
      ENDIF.
      ADD 1 TO last_index.
    ENDMETHOD.

    METHOD lif_input_port~is_char_ready.
      rv_flag = abap_false.
      CHECK input EQ abap_true.
      rv_flag = xsdbool( last_index < last_len ).
    ENDMETHOD.

    METHOD lif_input_port~read.
      CHECK input EQ abap_true.
      IF last_index < last_len.
        rv_input = last_input+last_index.
        CLEAR: last_input, last_index, last_len.
      ELSE.
        rv_input = read_stream( iv_title ).
      ENDIF.
    ENDMETHOD.


    " Binary input
    METHOD lif_binary_input_port~is_u8_ready.
      rv_flag = abap_false.
      CHECK input EQ abap_true AND port_type EQ binary.
      rv_flag = xsdbool( last_xindex < last_xlen ).
    ENDMETHOD.

    METHOD lif_binary_input_port~peek_u8.
      CHECK input EQ abap_true
        AND port_type EQ binary.
      IF last_xindex < last_xlen.
        rv_char = last_xinput+last_xindex(1).
      ELSE.
        rv_char = xblock_read( ).
      ENDIF.
    ENDMETHOD.

    METHOD lif_binary_input_port~put.
      IF output EQ abap_false.
        throw( `Error: Output on closed binary port` ).
      ENDIF.
      CHECK port_type EQ binary.
      throw( `put u8 not implemented yet` ).
    ENDMETHOD.

    METHOD lif_binary_input_port~read_u8.
      CHECK input EQ abap_true
        AND port_type EQ binary.
      IF last_xindex < last_xlen.
        rv_char = last_xinput+last_xindex(1).
      ELSE.
        rv_char = xblock_read( ).
      ENDIF.
      ADD 1 TO last_xindex.
    ENDMETHOD.

    METHOD lif_binary_input_port~read_xstring.
      CHECK input EQ abap_true
        AND port_type EQ binary.
      throw( `read xstring / bytevector not implemented yet` ).
    ENDMETHOD.

    METHOD lif_binary_output_port~write_u8.
      IF output EQ abap_false.
        throw( `Error: Output on closed binary port` ).
      ENDIF.
      CHECK port_type EQ binary.
      throw( `write u8 not implemented yet` ).
    ENDMETHOD.

    METHOD lif_binary_output_port~write_xstring.
      IF output EQ abap_false.
        throw( `Error: Output on closed binary port` ).
      ENDIF.
      CHECK port_type EQ binary.
      throw( `write_xstring not implemented yet` ).
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_buffered_port IMPLEMENTATION.

    METHOD constructor.
      super->constructor( iv_port_type = iv_port_type
                          iv_input = iv_input
                          iv_output = iv_output
                          iv_error = iv_error ).
      IF iv_separator EQ space.   " Special treatment for space,
        separator = ` `.   " see https://blogs.sap.com/2016/08/10/trailing-blanks-in-character-string-processing/
      ELSE.
        separator = iv_separator.
      ENDIF.
      string_mode = iv_string.
    ENDMETHOD.

    METHOD lif_log~put.
      write( io_elem ).
    ENDMETHOD.

    METHOD lif_log~get.
      result = flush( ).
    ENDMETHOD.

    METHOD write.
      CHECK element IS BOUND.
      TRY.
          add( element->to_string( ) ).
        CATCH lcx_lisp_exception INTO DATA(lx_error).
          add( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD display.
      CHECK element IS BOUND.
      TRY.
          add( element->to_text( ) ).
        CATCH lcx_lisp_exception INTO DATA(lx_error).
          add( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD read.
      CHECK input EQ abap_true.
      IF string_mode EQ abap_false.
        rv_input = super->read( ).
        RETURN.
      ENDIF.
      IF last_index < last_len.
        rv_input = last_input+last_index.
      ELSE.
        rv_input = lcl_lisp=>eof_object->value+0(1).
      ENDIF.
      CLEAR: last_input, last_index, last_len.
    ENDMETHOD.

    METHOD lif_input_port~peek_char.
      CHECK input EQ abap_true.
      IF string_mode EQ abap_false.
        rv_char = super->lif_input_port~peek_char( ).
        RETURN.
      ENDIF.
      IF last_index < last_len.
        rv_char = last_input+last_index(1).
      ELSE.
        rv_char = lcl_lisp=>eof_object->value+0(1).
      ENDIF.
    ENDMETHOD.

    METHOD flush.
      rv_text = buffer.
      CLEAR buffer.
    ENDMETHOD.

    METHOD add.
      IF buffer IS INITIAL.
        buffer = text.
      ELSE.
        buffer = |{ buffer }{ separator }{ text }|.
      ENDIF.
    ENDMETHOD.                    "add

  ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_iterator DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_iterator DEFINITION CREATE PRIVATE FRIENDS lcl_lisp.
    PUBLIC SECTION.
      DATA first TYPE tv_flag VALUE abap_true READ-ONLY.
      METHODS has_next RETURNING VALUE(rv_flag) TYPE tv_flag.
      METHODS next RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                   RAISING   cx_dynamic_check.
    PRIVATE SECTION.
      DATA elem TYPE REF TO lcl_lisp.

      METHODS constructor IMPORTING io_elem TYPE REF TO lcl_lisp
                          RAISING   lcx_lisp_exception.
  ENDCLASS.                    "lcl_lisp_iterator DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_hash DEFINITION
*----------------------------------------------------------------------*
* Hash is a specialized ABAP Lisp type for quick lookup of elements
* using a symbol or string key (backed by an ABAP hash table)
*----------------------------------------------------------------------*
  CLASS lcl_lisp_hash DEFINITION INHERITING FROM lcl_lisp
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      TYPES: BEGIN OF ts_hash,
               key     TYPE string,
               element TYPE REF TO lcl_lisp,
             END OF ts_hash.
      TYPES tt_hash TYPE HASHED TABLE OF ts_hash WITH UNIQUE KEY key.
      DATA hash TYPE tt_hash READ-ONLY.

      CLASS-METHODS new  IMPORTING it_hash        TYPE tt_hash
                         RETURNING VALUE(ro_hash) TYPE REF TO lcl_lisp_hash
                         RAISING   lcx_lisp_exception.

      CLASS-METHODS from_list IMPORTING list           TYPE REF TO lcl_lisp
                                        msg            TYPE string
                              RETURNING VALUE(ro_hash) TYPE REF TO lcl_lisp_hash
                              RAISING   lcx_lisp_exception.

      METHODS get IMPORTING list          TYPE REF TO lcl_lisp
                  RETURNING VALUE(result) TYPE REF TO lcl_lisp
                  RAISING   lcx_lisp_exception.
      METHODS insert IMPORTING list          TYPE REF TO lcl_lisp
                     RETURNING VALUE(result) TYPE REF TO lcl_lisp
                     RAISING   lcx_lisp_exception.
      METHODS delete IMPORTING list          TYPE REF TO lcl_lisp
                     RETURNING VALUE(result) TYPE REF TO lcl_lisp
                     RAISING   lcx_lisp_exception.
      METHODS get_hash_keys RETURNING VALUE(result) TYPE REF TO lcl_lisp.

    PROTECTED SECTION.
      METHODS fill IMPORTING list TYPE REF TO lcl_lisp
                   RAISING   lcx_lisp_exception.
  ENDCLASS.                    "lcl_lisp_hash DEFINITION

  CLASS lcl_lisp_vector DEFINITION INHERITING FROM lcl_lisp
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA array TYPE tt_lisp READ-ONLY.

      CLASS-METHODS init IMPORTING size             TYPE tv_index
                                   io_fill          TYPE REF TO lcl_lisp DEFAULT nil
                                   iv_mutable       TYPE tv_flag DEFAULT abap_true
                         RETURNING VALUE(ro_vector) TYPE REF TO lcl_lisp_vector
                         RAISING   lcx_lisp_exception.

      CLASS-METHODS from_list IMPORTING io_list          TYPE REF TO lcl_lisp
                                        iv_mutable       TYPE tv_flag DEFAULT abap_true
                              RETURNING VALUE(ro_vector) TYPE REF TO lcl_lisp_vector
                              RAISING   lcx_lisp_exception.

      METHODS to_list         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                              RAISING   lcx_lisp_exception.

      METHODS set IMPORTING index         TYPE tv_index
                            io_elem       TYPE REF TO lcl_lisp
                  RETURNING VALUE(result) TYPE REF TO lcl_lisp_vector
                  RAISING   lcx_lisp_exception.

      METHODS get IMPORTING index          TYPE tv_index
                  RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                  RAISING   lcx_lisp_exception.

      METHODS get_list IMPORTING from           TYPE tv_index DEFAULT 0
                                 to             TYPE tv_index OPTIONAL
                       RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                       RAISING   lcx_lisp_exception.

      METHODS fill IMPORTING from           TYPE tv_index DEFAULT 0
                             to             TYPE tv_index OPTIONAL
                             elem           TYPE REF TO lcl_lisp
                   RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_vector
                   RAISING   lcx_lisp_exception.

      METHODS length RETURNING VALUE(ro_length) TYPE REF TO lcl_lisp.

      METHODS to_string REDEFINITION.
      METHODS is_equal REDEFINITION.
      METHODS set_shared_structure REDEFINITION.

    PROTECTED SECTION.
      DATA mo_length TYPE REF TO lcl_lisp.
  ENDCLASS.

  CLASS lcl_lisp_bytevector DEFINITION INHERITING FROM lcl_lisp
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA bytes TYPE tt_byte READ-ONLY.

      CLASS-METHODS init IMPORTING size         TYPE tv_index
                                   iv_fill      TYPE tv_byte
                                   iv_mutable   TYPE tv_flag DEFAULT abap_true
                         RETURNING VALUE(ro_u8) TYPE REF TO lcl_lisp_bytevector
                         RAISING   lcx_lisp_exception.

      CLASS-METHODS utf8_from_string IMPORTING iv_text      TYPE string
                                               iv_mutable   TYPE tv_flag DEFAULT abap_true
                                     RETURNING VALUE(ro_u8) TYPE REF TO lcl_lisp_bytevector
                                     RAISING   lcx_lisp_exception.

      CLASS-METHODS from_list IMPORTING io_list      TYPE REF TO lcl_lisp
                                        iv_mutable   TYPE tv_flag DEFAULT abap_true
                              RETURNING VALUE(ro_u8) TYPE REF TO lcl_lisp_bytevector
                              RAISING   lcx_lisp_exception.

      METHODS utf8_to_string IMPORTING from           TYPE tv_index DEFAULT 0
                                       to             TYPE tv_index OPTIONAL
                             RETURNING VALUE(rv_text) TYPE string
                             RAISING   lcx_lisp_exception.

      METHODS set IMPORTING index   TYPE tv_index
                            iv_byte TYPE tv_byte
                  RAISING   lcx_lisp_exception.

      METHODS get IMPORTING index          TYPE tv_index
                  RETURNING VALUE(rv_byte) TYPE tv_byte
                  RAISING   lcx_lisp_exception.

      METHODS copy_new IMPORTING from           TYPE tv_index DEFAULT 0
                                 to             TYPE tv_index OPTIONAL
                       RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_bytevector
                       RAISING   lcx_lisp_exception.

      METHODS copy IMPORTING at             TYPE tv_index DEFAULT 0
                             io_from        TYPE REF TO lcl_lisp_bytevector
                             start          TYPE tv_index OPTIONAL
                             end            TYPE tv_index OPTIONAL
                   RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_bytevector
                   RAISING   lcx_lisp_exception.

      METHODS length RETURNING VALUE(ro_length) TYPE REF TO lcl_lisp.

      METHODS to_string REDEFINITION.
      METHODS is_equal REDEFINITION.

    PROTECTED SECTION.
      DATA mo_length TYPE REF TO lcl_lisp.
  ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_abapfunction DEFINITION
*----------------------------------------------------------------------*
* Specialized element representing an ABAP function module that can
* be called
*----------------------------------------------------------------------*
  CLASS lcl_lisp_abapfunction DEFINITION INHERITING FROM lcl_lisp_data CREATE PROTECTED
    FRIENDS lcl_lisp_new.
    PUBLIC SECTION.

      METHODS call IMPORTING list           TYPE REF TO lcl_lisp
                   RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                   RAISING   lcx_lisp_exception.

      METHODS get_function_parameter IMPORTING identifier   TYPE REF TO lcl_lisp
                                     RETURNING VALUE(rdata) TYPE REF TO data
                                     RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      TYPES tt_rsexc TYPE STANDARD TABLE OF rsexc WITH DEFAULT KEY.   " Exceptions
      TYPES tt_rsexp TYPE STANDARD TABLE OF rsexp WITH DEFAULT KEY.   " Exporting
      TYPES tt_rsimp TYPE STANDARD TABLE OF rsimp WITH DEFAULT KEY.   " Importing
      TYPES tt_rscha TYPE STANDARD TABLE OF rscha WITH DEFAULT KEY.   " Changing
      TYPES tt_rstbl TYPE STANDARD TABLE OF rstbl WITH DEFAULT KEY.   " Tables

      TYPES: BEGIN OF ts_interface,
               exc         TYPE tt_rsexc,
               exp         TYPE tt_rsexp,
               imp         TYPE tt_rsimp,
               cha         TYPE tt_rscha,
               tbl         TYPE tt_rstbl,
               enh_exp     TYPE tt_rsexp,
               enh_imp     TYPE tt_rsimp,
               enh_cha     TYPE tt_rscha,
               enh_tbl     TYPE tt_rstbl,
               remote_call TYPE rs38l-remote,
               update_task TYPE rs38l-utask,
             END OF ts_interface.

      DATA parameters TYPE abap_func_parmbind_tab.
      DATA exceptions TYPE abap_func_excpbind_tab.
      DATA param_active TYPE abap_func_parmbind_tab.
      DATA interface TYPE ts_interface.

      METHODS read_interface IMPORTING iv_name              TYPE csequence
                             RETURNING VALUE(function_name) TYPE rs38l-name
                             RAISING   lcx_lisp_exception.
      METHODS create_parameters RAISING   lcx_lisp_exception.
      METHODS create_exceptions.

      METHODS error_message RETURNING VALUE(rv_message) TYPE string.
    PRIVATE SECTION.
      CONSTANTS c_error_message TYPE i VALUE 99.

      DATA parameters_generated TYPE tv_flag.

      METHODS create_table_params IMPORTING it_table TYPE tt_rstbl.
      METHODS create_params IMPORTING it_table TYPE STANDARD TABLE
                                      iv_kind  TYPE i.
  ENDCLASS.                    "lcl_lisp_abapfunction DEFINITION

  CLASS lcl_lisp_query DEFINITION INHERITING FROM lcl_lisp_data
     CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      METHODS constructor IMPORTING osql TYPE string OPTIONAL
                          RAISING   cx_sql_exception.
      METHODS execute IMPORTING query         TYPE string
                      RETURNING VALUE(result) TYPE REF TO lcl_lisp_sql_result
                      RAISING   cx_sql_exception.
    PROTECTED SECTION.
      DATA mv_hold_cursor TYPE tv_flag.
      DATA sql_query TYPE string.
      DATA connection TYPE REF TO cl_sql_connection.
      DATA statement TYPE REF TO cl_sql_statement.
  ENDCLASS.

  CLASS lcl_lisp_env_factory DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_environment DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_environment DEFINITION CREATE PRIVATE FRIENDS lcl_lisp_env_factory.
    PUBLIC SECTION.
      DATA top_level TYPE tv_flag VALUE abap_false READ-ONLY.

      METHODS:
        scope_of IMPORTING symbol     TYPE any
                 RETURNING VALUE(env) TYPE REF TO lcl_lisp_environment
                 RAISING   lcx_lisp_exception,
        get IMPORTING symbol      TYPE any
            RETURNING VALUE(cell) TYPE REF TO lcl_lisp
            RAISING   lcx_lisp_exception,
        set IMPORTING symbol  TYPE string
                      element TYPE REF TO lcl_lisp
                      once    TYPE tv_flag DEFAULT abap_false
            RAISING   lcx_lisp_exception,
*       Convenience methods to add a value and create the cell
        define_value IMPORTING symbol         TYPE string
                               type           TYPE tv_type
                               value          TYPE any OPTIONAL
                     RETURNING VALUE(element) TYPE REF TO lcl_lisp,
        procedure IMPORTING symbol         TYPE string
                            value          TYPE string
                            arity          TYPE tv_int OPTIONAL
                  RETURNING VALUE(element) TYPE REF TO lcl_lisp,
        syntax IMPORTING symbol         TYPE string
               RETURNING VALUE(element) TYPE REF TO lcl_lisp.

      METHODS parameters_to_symbols IMPORTING io_pars TYPE REF TO lcl_lisp
                                              io_args TYPE REF TO lcl_lisp
                                    RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
*     Reference to
      DATA parent TYPE REF TO lcl_lisp_environment.  " outer (parent) environment:

      TYPES: BEGIN OF ts_map,
               symbol TYPE string,
               value  TYPE REF TO lcl_lisp,
             END OF ts_map.
      TYPES tt_map TYPE HASHED TABLE OF ts_map WITH UNIQUE KEY symbol.

      DATA map TYPE tt_map.

      METHODS unbound_symbol IMPORTING symbol TYPE any
                             RAISING   lcx_lisp_exception.
      METHODS prepare.

    PRIVATE SECTION.
      METHODS load_syntax.
      METHODS load_libraries.
      METHODS load_delayed_evaluation.
      METHODS load_list.
      METHODS load_math.
      METHODS load_numbers.
      METHODS load_turtles.
      METHODS load_chars.
      METHODS load_strings.
  ENDCLASS.                    "lcl_lisp_environment DEFINITION

  CLASS lcl_lisp_env_factory DEFINITION ABSTRACT.
    PUBLIC SECTION.
      CLASS-METHODS:
        new   RETURNING VALUE(env)  TYPE REF TO lcl_lisp_environment,
        clone IMPORTING io_outer   TYPE REF TO lcl_lisp_environment
              RETURNING VALUE(env) TYPE REF TO lcl_lisp_environment,
        make_top_level IMPORTING io_outer   TYPE REF TO lcl_lisp_environment OPTIONAL
                       RETURNING VALUE(env) TYPE REF TO lcl_lisp_environment.
  ENDCLASS.

  CLASS lcl_lisp_env_factory  IMPLEMENTATION.

    METHOD new.
      env = NEW lcl_lisp_environment( ).
    ENDMETHOD.                    "new

    METHOD clone.
      env = new( ).
      env->parent = io_outer.
    ENDMETHOD.

    METHOD make_top_level.
      env = clone( io_outer ).
      env->prepare( ).
      env->top_level = abap_true.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_string_stream DEFINITION.
    PUBLIC SECTION.
      INTERFACES lif_stream.
      ALIASES: state FOR lif_stream~state.

      METHODS constructor IMPORTING iv_code TYPE string.
    PROTECTED SECTION.
      DATA code TYPE string.
      DATA saved_state TYPE ts_stream_state.

      METHODS init IMPORTING iv_code TYPE string.
      METHODS throw IMPORTING message TYPE string
                    RAISING   lcx_lisp_exception.
  ENDCLASS.

  CLASS lcl_string_stream IMPLEMENTATION.

    METHOD init.
      code = iv_code.
      state-length = strlen( code ).
      state-index = 0.
      state-ready = xsdbool( state-length GT 0 ).

      CHECK state-ready EQ abap_true.
      state-char = code+state-index(1).           "Kick off things by reading first char
    ENDMETHOD.

    METHOD lif_stream~bookmark.
      saved_state = lif_stream~state.
    ENDMETHOD.

    METHOD lif_stream~backtrack.
      lif_stream~state = saved_state.
    ENDMETHOD.

    METHOD constructor.
      super->constructor( ).
      init( iv_code ).
    ENDMETHOD.

    METHOD lif_stream~next_char.
      state-index = state-index + 1.
      IF state-index < state-length.
        state-char = code+state-index(1).
      ELSEIF state-index = state-length.
        state-char = space.
        state-ready = abap_false.
      ELSEIF state-index > state-length.
        throw( c_error_unexpected_end ).
      ENDIF.
    ENDMETHOD.                    "next_char

    METHOD throw.
      RAISE EXCEPTION TYPE lcx_lisp_exception
        EXPORTING
          message = message
          area    = c_area_parse.
    ENDMETHOD.

    METHOD lif_stream~peek_char.
      DATA(lv_idx) = state-index + 1.

      IF lv_idx < state-length.
        rv_char = code+lv_idx(1).
      ELSE.
        rv_char = c_lisp_eof.
      ENDIF.
    ENDMETHOD.

    METHOD lif_stream~expect.
      match = abap_false.

      DATA(lv_len) = strlen( iv_text ).
      CHECK lv_len GT 0 AND state-index + lv_len LE state-length.

      IF iv_case_sensitive EQ abap_true.
        match = xsdbool( iv_text = code+state-index(lv_len) ).
      ELSE.
        match = xsdbool( to_upper( iv_text ) = to_upper( code+state-index(lv_len) ) ).
      ENDIF.
    ENDMETHOD.

    METHOD lif_stream~match.
      found = lif_stream~expect( iv_text = iv_text
                                 iv_case_sensitive = iv_case_sensitive ).
      CHECK found = abap_true.
      DO strlen( iv_text ) TIMES.
        lif_stream~next_char( ).
      ENDDO.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_stream DEFINITION.
    PUBLIC SECTION.
      CONSTANTS:
        c_esc_a          TYPE tv_char VALUE 'a',
        c_esc_b          TYPE tv_char VALUE 'b',
        c_esc_t          TYPE tv_char VALUE 't',
        c_esc_n          TYPE tv_char VALUE 'n',
        c_esc_r          TYPE tv_char VALUE 'r',
        c_esc_semi_colon TYPE tv_char VALUE c_semi_colon,
        c_esc_vline      TYPE tv_char VALUE c_vertical_line.

      CLASS-DATA mv_whitespace TYPE char07 READ-ONLY. " Case sensitive
      CLASS-DATA gv_line_ending TYPE char04.

      CLASS-METHODS:
        class_constructor.

      METHODS:
        constructor IMPORTING ii_stream TYPE REF TO lif_stream,
        next_char RAISING lcx_lisp_exception,
        peek_char RETURNING VALUE(rv_char) TYPE tv_char.

      METHODS parse RETURNING VALUE(elements) TYPE lcl_lisp=>tt_element
                    RAISING   lcx_lisp_exception.

      METHODS read_stream RETURNING VALUE(element) TYPE REF TO lcl_lisp
                          RAISING   lcx_lisp_exception.

      METHODS read_token RETURNING VALUE(token) TYPE string
                         RAISING   lcx_lisp_exception.

      METHODS:
        skip_while IMPORTING pattern      TYPE csequence
                   RETURNING VALUE(found) TYPE tv_flag
                   RAISING   lcx_lisp_exception,
        skip_until IMPORTING pattern      TYPE csequence
                   RETURNING VALUE(found) TYPE tv_flag
                   RAISING   lcx_lisp_exception,

        skip_comment RETURNING VALUE(found) TYPE tv_flag
                     RAISING   lcx_lisp_exception,

        line_ending RETURNING VALUE(rv_has_next) TYPE tv_flag
                    RAISING   lcx_lisp_exception.

      METHODS peek_bytevector RETURNING VALUE(rv_flag) TYPE tv_flag.

      METHODS decode_hex_digits RETURNING VALUE(pchar) TYPE tv_char
                                RAISING   lcx_lisp_exception.
      METHODS:
        match_label IMPORTING iv_limit        TYPE tv_char
                    EXPORTING ev_label        TYPE string
                    RETURNING VALUE(rv_found) TYPE tv_flag.

      METHODS decode_escape RETURNING VALUE(pchar) TYPE tv_char
                            RAISING   lcx_lisp_exception.
      METHODS match_string CHANGING cv_val TYPE string
                           RAISING  lcx_lisp_exception.
      METHODS:
        parse_pair IMPORTING delim         TYPE tv_char DEFAULT c_open_paren
                   RETURNING VALUE(result) TYPE REF TO lcl_lisp
                   RAISING   lcx_lisp_exception,
        parse_token RETURNING VALUE(element) TYPE REF TO lcl_lisp
                    RAISING   lcx_lisp_exception,
        parse_datum RETURNING VALUE(element) TYPE REF TO lcl_lisp
                    RAISING   lcx_lisp_exception.

      METHODS read_character RETURNING VALUE(element) TYPE REF TO lcl_lisp
                             RAISING   lcx_lisp_exception.

      METHODS read_number IMPORTING iv_peek_char TYPE tv_char
                                    iv_radix     TYPE tv_int DEFAULT 10
                          EXPORTING element      TYPE REF TO lcl_lisp
                          RETURNING VALUE(found) TYPE tv_flag
                          RAISING   lcx_lisp_exception.

      METHODS read_atom RETURNING VALUE(atom) TYPE string
                        RAISING   lcx_lisp_exception.

      METHODS uinteger_radix IMPORTING domain      TYPE csequence
                             RETURNING VALUE(uint) TYPE string
                             RAISING   lcx_lisp_exception cx_sy_conversion_no_number.

      METHODS uint_dec_radix IMPORTING domain      TYPE csequence
                             RETURNING VALUE(uint) TYPE string
                             RAISING   lcx_lisp_exception cx_sy_conversion_no_number.

      METHODS read_real IMPORTING domain        TYPE csequence
                                  iv_exact_char TYPE tv_char
                        RETURNING VALUE(elem) TYPE REF TO lcl_lisp_number
                        RAISING   lcx_lisp_exception cx_sy_conversion_no_number.

      METHODS read_complex IMPORTING domain        TYPE csequence
                                     iv_exact_char TYPE tv_char
                           RETURNING VALUE(elem) TYPE REF TO lcl_lisp_number
                           RAISING   lcx_lisp_exception cx_sy_conversion_no_number.

      METHODS read_suffix IMPORTING ii_stream  TYPE REF TO lif_stream
                          RETURNING VALUE(str) TYPE string
                          RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      TYPES tv_text16 TYPE c LENGTH 16.

      CLASS-DATA mv_open_paren TYPE char03.

      CLASS-DATA mv_newline TYPE tv_char.
      CLASS-DATA mv_space_or_tab TYPE char03.
      CLASS-DATA mv_delimiters TYPE tv_text16. " Case sensitive
      CLASS-DATA mv_decimal_initial TYPE string.

      DATA mi_stream TYPE REF TO lif_stream.
      DATA mv_fold_case TYPE tv_flag.

      METHODS bookmark.
      METHODS backtrack.

      METHODS zero_or_more_digits IMPORTING domain      TYPE csequence
                                  RETURNING VALUE(uint) TYPE string
                                  RAISING   lcx_lisp_exception.

      METHODS skip_one_datum RAISING lcx_lisp_exception.

      METHODS skip_block_comment RAISING lcx_lisp_exception.
      METHODS match_directive RETURNING VALUE(found) TYPE tv_flag
                              RAISING   lcx_lisp_exception.

      METHODS skip_intertoken_space RETURNING VALUE(rv_has_next) TYPE tv_flag
                                    RAISING   lcx_lisp_exception.

      METHODS throw IMPORTING message TYPE string
                              area    TYPE string DEFAULT c_area_parse
                    RAISING   lcx_lisp_exception.

      METHODS get_atom RETURNING VALUE(rv_val) TYPE string
                       RAISING   lcx_lisp_exception.

      METHODS read_exact RETURNING VALUE(rv_exact) TYPE tv_flag
                         RAISING   lcx_lisp_exception.

      METHODS match_token RETURNING VALUE(element) TYPE REF TO lcl_lisp
                          RAISING   lcx_lisp_exception.

      METHODS number_match IMPORTING iv_text TYPE string
                           RETURNING VALUE(found) TYPE tv_flag
                           RAISING   lcx_lisp_exception.

      METHODS match_number RETURNING VALUE(elem) TYPE REF TO lcl_lisp_number
                           RAISING   lcx_lisp_exception.

  ENDCLASS.

  CLASS lcl_stream IMPLEMENTATION.

    METHOD class_constructor.
*     End of line value
      mv_newline = |\n|.  " cl_abap_char_utilities=>newline.

      CLEAR mv_space_or_tab.
      mv_space_or_tab+0(1) = ' '.
      mv_space_or_tab+1(1) = |\t|.  " cl_abap_char_utilities=>horizontal_tab.
      mv_space_or_tab+2(1) = cl_abap_char_utilities=>vertical_tab.
      " line ending
      CLEAR gv_line_ending.
      gv_line_ending+0(1) = mv_newline.     " \n
      gv_line_ending+1(2) = |\r\n|.         " return newline   cl_abap_char_utilities=>cr_lf.
      gv_line_ending+3(1) = |\r|.           " return           cl_abap_char_utilities=>cr_lf(1).
      " Whitespace values
      CLEAR mv_whitespace.
      mv_whitespace+0(3) = mv_space_or_tab.
      mv_whitespace+3(1) = mv_newline.
      mv_whitespace+4(1) = cl_abap_char_utilities=>cr_lf(1).       " \r
      mv_whitespace+5(1) = cl_abap_char_utilities=>cr_lf(2).       " \n
      mv_whitespace+6(1) = cl_abap_char_utilities=>form_feed.

      " <delimiter> -> <whitespace> | <vertical line> | ( | ) | " | ;
      " we add racket like syntax: { | [ | ] | }
      mv_open_paren = c_open_paren && c_open_bracket && c_open_curly.

      mv_delimiters = mv_whitespace.
      mv_delimiters+7(1) = c_close_paren.
      mv_delimiters+8(1) = c_open_paren.
      mv_delimiters+9(1) = c_close_bracket.
      mv_delimiters+10(1) = c_open_bracket.
      mv_delimiters+11(1) = c_close_curly.
      mv_delimiters+12(1) = c_open_curly.
      mv_delimiters+13(1) = c_text_quote.
      mv_delimiters+14(1) = c_semi_colon.

      mv_delimiters+15(1) = c_vertical_line.

      mv_decimal_initial = c_decimal_digits && c_explicit_sign && c_lisp_dot.
    ENDMETHOD.

    METHOD constructor.
      mi_stream = ii_stream.
    ENDMETHOD.

    METHOD next_char.
      mi_stream->next_char( ).
    ENDMETHOD.

    METHOD peek_char.
      rv_char = mi_stream->peek_char( ).
    ENDMETHOD.

    METHOD throw.
      lcl_lisp=>throw( message = message
                       area = area ).
    ENDMETHOD.

    METHOD read_token.
      CLEAR token.
      WHILE mi_stream->state-char CN mv_delimiters
        AND mi_stream->state-ready EQ abap_true.
        next_char( ).
        CONCATENATE token mi_stream->state-char INTO token RESPECTING BLANKS.
      ENDWHILE.
    ENDMETHOD.

    METHOD read_atom.
      CLEAR atom.

*      IF mi_stream->state-ready EQ abap_true.
*        CONCATENATE atom mi_stream->state-char INTO atom RESPECTING BLANKS.
*        next_char( ).
*        IF mi_stream->state-ready EQ abap_false.
*          CONCATENATE atom mi_stream->state-char INTO atom RESPECTING BLANKS.
*        ENDIF.
*      ENDIF.
*      WHILE mi_stream->state-char CN mv_delimiters
*        AND mi_stream->state-ready EQ abap_true.
*        next_char( ).
*        CONCATENATE atom mi_stream->state-char INTO atom RESPECTING BLANKS.
*      ENDWHILE.

      WHILE mi_stream->state-ready EQ abap_true.
        CONCATENATE atom mi_stream->state-char INTO atom RESPECTING BLANKS.
        next_char( ).
        IF mi_stream->state-ready EQ abap_false.
          CONCATENATE atom mi_stream->state-char INTO atom RESPECTING BLANKS.
        ELSEIF mi_stream->state-char CA mv_delimiters.
          EXIT.
        ENDIF.
      ENDWHILE.

      CONDENSE atom.              " 29.04.2018 to be reviewed, remove?
      IF atom = mv_newline.
        atom = space.
      ENDIF.
    ENDMETHOD.

    METHOD zero_or_more_digits.
      CLEAR uint.
      WHILE mi_stream->state-char CO domain.
        uint &&= mi_stream->state-char.
        next_char( ).
      ENDWHILE.
    ENDMETHOD.

    METHOD uInteger_radix.
      DATA lv_int TYPE tv_int.

      IF mi_stream->state-char CN domain.
        lcl_lisp=>throw_no_number( `digit ` && mi_stream->state-char ).
      ENDIF.
      DATA(uint_str) = zero_or_more_digits( domain ).

      CASE domain.
        WHEN c_binary_digits.
          lv_int = lcl_lisp_new=>binary_integer( uInt_str ).
        WHEN c_octal_digits.
          lv_int = lcl_lisp_new=>octal_integer( uInt_str ).
        WHEN c_decimal_digits.
          uInt = uInt_str.
          RETURN.
        WHEN c_hex_digits_long
          OR c_hex_digits.
          lv_int = lcl_lisp_new=>hex_integer( uInt_str ).
      ENDCASE.
       uInt = lv_int.
       condense uInt.
    ENDMETHOD.

    METHOD uInt_dec_radix.
      DATA lv_int TYPE tv_int.
      DATA lv_real TYPE tv_real.
      DATA lv_exp TYPE tv_int.
      DATA base TYPE i.

      DATA(uint_str) = zero_or_more_digits( domain ).
      lv_exp = numofchar( uInt_str ).

      CASE domain.
        WHEN c_binary_digits.
          lv_int = lcl_lisp_new=>binary_integer( uInt_str ).
          base = 2.
        WHEN c_octal_digits.
          lv_int = lcl_lisp_new=>octal_integer( uInt_str ).
          base = 8.
        WHEN c_decimal_digits.
          uInt = uInt_str.
          base = 10.
          RETURN.
        WHEN c_hex_digits_long
          OR c_hex_digits.
          lv_int = lcl_lisp_new=>hex_integer( uInt_str ).
          base = 16.
      ENDCASE.

      lv_real = lv_int / ipow( base = base exp = lv_exp ).
      uInt = lv_real.
      condense uInt.
      uInt = substring_after( val = uInt sub = c_lisp_dot ).

    ENDMETHOD.

    METHOD read_suffix. "  decimal only with radix 10
      str = space.
      CHECK ii_stream->state-ready EQ abap_true.
      IF ii_stream->state-char CA c_exponent_marker_long.
        ii_stream->next_char( ).
        str = c_exponent_marker.
        CASE ii_stream->state-char.
          WHEN c_plus_sign.
          WHEN c_minus_sign.
            str &&= c_minus_sign.
        ENDCASE.
        str &&= zero_or_more_digits( c_decimal_digits ).
      ENDIF.
    ENDMETHOD.

    METHOD number_match.
      found = mi_stream->match( iv_text = iv_text
                                iv_case_sensitive = abap_false ).
    ENDMETHOD.

    METHOD match_number.
      IF number_match( c_lisp_pos_inf ).
        elem = lcl_lisp_number=>inf.
      ELSEIF number_match( c_lisp_neg_inf ).
        elem = lcl_lisp_number=>neg_inf.
      ELSEIF number_match( c_lisp_pos_nan ).
        elem = lcl_lisp_number=>nan.
      ELSEIF number_match( c_lisp_neg_nan ).
        elem = lcl_lisp_number=>neg_nan.
      ELSEIF number_match( c_lisp_pos_img ).          " Plus Imaginary_marker
        elem = lcl_lisp_number=>imaginary.
      ELSEIF number_match( c_lisp_neg_img ).
        elem = lcl_lisp_number=>imaginary_neg.
      ELSEIF number_match( c_imaginary_pos_inf ).     " Pos. Infinity Imaginary_marker
        elem = lcl_lisp_number=>imaginary_neg.
      ELSEIF number_match( c_imaginary_neg_inf ).     " Neg. Infinity Imaginary_marker
        elem = lcl_lisp_number=>neg_inf_imag.
      ELSEIF number_match( c_imaginary_pos_nan ).     " Pos. NaN Imaginary_marker
        elem = lcl_lisp_number=>nan_imag.
      ELSEIF number_match( c_imaginary_neg_nan ).     " Neg. NaN Imaginary_marker
        elem = lcl_lisp_number=>neg_nan_imag.
      ELSE.
        CLEAR elem.
      ENDIF.
    ENDMETHOD.

    METHOD read_real.
      DATA num TYPE ts_number.

      DATA neg_sep TYPE tv_char.
      DATA pos_sep TYPE tv_char.
      DATA sep TYPE tv_char.

      DATA uInt_str TYPE string.
      DATA uDenom_str TYPE string.
      DATA uReal_str TYPE string.
      DATA suffix_str TYPE string.

      DATA lv_denom TYPE tv_int.
      DATA lv_int TYPE tv_int.
      DATA lv_real TYPE tv_real.
      DATA lv_exact TYPE tv_flag.

      elem = match_number( ).
      IF elem IS INITIAL.

        CASE iv_exact_char.
          WHEN c_number_exact+0(1) OR c_number_exact+1(1).     "#e or #E (exact)
            lv_exact = abap_true.
          WHEN c_number_inexact+0(1) OR c_number_inexact+1(1). "#i or #I (inexact)
            lv_exact = abap_false.
          WHEN space.
            lv_exact = abap_true.
          WHEN OTHERS.
            lcl_lisp=>throw( `Invalid exactness token in number prefix` ).
        ENDCASE.

        CASE mi_stream->state-char.
          WHEN c_plus_sign.
            sep = pos_sep = c_plus_sign.
            mi_stream->next_char( ).
          WHEN c_minus_sign.
            sep = neg_sep = c_minus_sign.
            mi_stream->next_char( ).
        ENDCASE.

        CASE mi_stream->state-char.
          WHEN c_lisp_dot.  " One decimal10 variant
            mi_stream->next_char( ).  " skip dot
            ureal_str = neg_sep && `0.` && uint_dec_radix( domain ) && read_suffix( mi_stream ).
            IF iv_exact_char EQ space.
              lv_exact = abap_false.
            ENDIF.

          WHEN OTHERS. " unsigned real
            uint_str = neg_sep && uinteger_radix( domain ).
            IF mi_stream->state-ready EQ abap_true.
              CASE mi_stream->state-char.
                WHEN  c_lisp_slash.
                  mi_stream->next_char( ).  " skip slash
                  udenom_str = uinteger_radix( domain ).

                  num-real = uint_str.
                  lv_real = udenom_str.
                  IF lv_real EQ 0.
                    IF num-real GT 0.
                      elem = lcl_lisp_number=>inf.
                    ELSEIF num-real LT 0.
                      elem = lcl_lisp_number=>neg_inf.
                    ELSE.
                      elem = lcl_lisp_number=>nan.
                    ENDIF.
                  ELSE.
                    TRY.
                        MOVE EXACT uint_str TO lv_int.
                        MOVE EXACT lv_real TO lv_denom.

                        elem = lcl_lisp_new=>rational( nummer = lv_int
                                                       denom = lv_denom
                                                       iv_exact = lv_exact ).
                      CATCH cx_root.
                        num-real = uint_str.
                        num-real = num-real / lv_real.
                        ureal_str = num-real.

                        IF iv_exact_char EQ space.
                          lv_exact = abap_false.
                        ENDIF.
                    ENDTRY.
                  ENDIF.

                WHEN c_lisp_dot.  " another decimal10 variant
                  mi_stream->next_char( ).  " skip dot
                  ureal_str = uint_str && c_lisp_dot && uint_dec_radix( domain ) && read_suffix( mi_stream ).
                  IF iv_exact_char EQ space.
                    lv_exact = abap_false.
                  ENDIF.

                WHEN OTHERS.
                  ureal_str = uint_str && read_suffix( mi_stream ).

                  IF iv_exact_char EQ space.
                    IF domain EQ c_decimal_digits  " radix is 10
                      AND ureal_str NA c_pattern_inexact_long.
                      lv_exact = abap_true.
                    ELSEIF domain NE c_decimal_digits  " radix is not 10
                      AND uint_str NA c_lisp_dot.
                      lv_exact = abap_true.
                    ELSE.
                      lv_exact = abap_false.
                    ENDIF.
                 ENDIF.

              ENDCASE.
            ELSE.
              ureal_str = uint_str.
            ENDIF.

        ENDCASE.

        IF elem IS INITIAL.
          elem = lcl_lisp_new=>real_number( value = ureal_str
                                            iv_exact = lv_exact ).
        ENDIF.

      ENDIF.
    ENDMETHOD.

    METHOD read_complex.
      CONSTANTS c_error_complex_parse TYPE string VALUE `Invalid complex parse`.
      DATA lo_z TYPE REF TO lcl_lisp_complex.

      elem = read_real( domain = domain
                        iv_exact_char = iv_exact_char ).

      CHECK mi_stream->state-ready EQ abap_true
        AND elem->type NE type_complex.

      CASE to_upper( mi_stream->state-char ).
        WHEN c_imaginary_marker.
          DATA(zimag) = elem.
          elem = lcl_lisp_number=>zero.
          mi_stream->next_char( ).

        WHEN c_complex_polar. " <real R> @ <real R>
          mi_stream->next_char( ).
          zimag = read_real( iv_exact_char = iv_exact_char
                             domain = domain ).
          IF zimag->type EQ type_complex.
            throw( c_error_complex_parse ).
          ENDIF.

          elem = lcl_lisp_new=>polar( r = elem
                                      angle = zimag ).
          RETURN.

        WHEN OTHERS.
          "TRY.
              IF mi_stream->state-char CA mv_delimiters.
                RETURN.
              ELSE.
                zimag = read_real( iv_exact_char = iv_exact_char
                                   domain = domain ).
              ENDIF.
          "CATCH lcx_lisp_exception cx_sy_conversion_no_number.
          "  RETURN.  " performance: can we get here without exception?
          "ENDTRY.
          IF zimag->type EQ type_complex.
            lo_z ?= zimag.
            IF lo_z->zreal NE lcl_lisp_number=>zero.
              throw( c_error_complex_parse ).
            ENDIF.
            zimag = lo_z->zimag.
          ELSEIF to_upper( mi_stream->state-char ) NE c_imaginary_marker.
            throw( c_error_complex_parse ).
          ELSE.
            mi_stream->next_char( ).   " Skip Imaginary symbol
          ENDIF.
      ENDCASE.

      elem = lcl_lisp_new=>rectangular( real = elem
                                        imag = zimag ).
    ENDMETHOD.

    METHOD skip_while.
      WHILE mi_stream->state-char CA pattern
        AND mi_stream->state-ready EQ abap_true.
        next_char( ).
        found = abap_true.
      ENDWHILE.
    ENDMETHOD.

    METHOD skip_until.
      WHILE mi_stream->state-char NA pattern
        AND mi_stream->state-ready EQ abap_true.
        next_char( ).
        found = abap_true.
      ENDWHILE.
    ENDMETHOD.

    METHOD line_ending.
      " throw
    ENDMETHOD.

    METHOD parse.
*     Entry point for parsing code. This is not thread-safe, but as an ABAP
*     process does not have the concept of threads, we are safe :-)
      IF mi_stream->state-ready EQ abap_true.
        WHILE skip_intertoken_space( ).
          APPEND parse_token( ) TO elements.
        ENDWHILE.
      ELSE.
        APPEND lcl_lisp_new=>eof( ) TO elements.
      ENDIF.
    ENDMETHOD.

    METHOD read_stream.
      IF mi_stream->state-ready EQ abap_true.
        element = parse_token( ).
      ELSE.
        element = lcl_lisp_new=>eof( ).
      ENDIF.
    ENDMETHOD.                    "parse

    METHOD skip_comment.
      found = abap_false.

      CASE mi_stream->state-char.
        WHEN c_lisp_comment.
          skip_until( mv_newline ).  " line comment until end of line
          found = abap_true.

        WHEN c_lisp_hash.
          CASE peek_char( ).
            WHEN c_lisp_comment.
              skip_one_datum( ).
              found = abap_true.

            WHEN c_vertical_line.
              skip_block_comment( ).
              found = abap_true.
          ENDCASE.
      ENDCASE.
    ENDMETHOD.

    METHOD match_directive.
      CONSTANTS: " #!fold-case or #!no-fold-case
        c_fold_case    TYPE string VALUE `fold-case`,
        c_no_fold_case TYPE string VALUE `no-fold-case`.
      DATA fold_directive TYPE string.

      found = abap_false.

      CHECK mi_stream->state-char EQ c_lisp_hash
        AND peek_char( ) EQ c_lisp_directive.

      next_char( ).   " skip #!
      next_char( ).

      fold_directive = read_token( ).

      CASE fold_directive.
        WHEN c_fold_case.
          mv_fold_case = abap_true.

        WHEN c_no_fold_case.
          mv_fold_case = abap_false.

        WHEN OTHERS.
          throw( `Invalid directive #!` && fold_directive ).
      ENDCASE.

      found = abap_true.
    ENDMETHOD.

    METHOD skip_block_comment.
      " block comment starts with #| and ends with |#
      next_char( ).   " skip |#
      next_char( ).
      WHILE mi_stream->state-ready EQ abap_true
        AND NOT ( mi_stream->state-char EQ c_vertical_line AND peek_char( ) EQ c_lisp_hash ).
        next_char( ).
      ENDWHILE.

      IF mi_stream->state-char EQ c_vertical_line AND peek_char( ) EQ c_lisp_hash.
        next_char( ).  " skip #|
        next_char( ).
      ENDIF.
    ENDMETHOD.

    METHOD match_label.
      rv_found = abap_false.
      CLEAR ev_label.

      bookmark( ).

      TRY.
          WHILE mi_stream->state-ready EQ abap_true.
            next_char( ).
            IF mi_stream->state-char CO c_decimal_digits.
              ev_label &&= mi_stream->state-char.
            ELSEIF mi_stream->state-char = iv_limit AND ev_label IS NOT INITIAL.
              next_char( ).                 "Skip past closing iv_limit ( = or # )
              rv_found = abap_true.
              RETURN.
            ELSE.  " match was not successful
              EXIT.
            ENDIF.
          ENDWHILE.
        CATCH lcx_lisp_exception.
      ENDTRY.

      backtrack( ).  " if match was not successful
    ENDMETHOD.

    METHOD bookmark.
      mi_stream->bookmark( ).
    ENDMETHOD.

    METHOD backtrack.
      mi_stream->backtrack( ).
    ENDMETHOD.

    METHOD skip_one_datum.
*     Character constant  #; comment
      next_char( ).       " skip #;
      next_char( ).
*     skip optional blanks, max. until end of line
      WHILE mi_stream->state-char CN mv_newline
        AND mi_stream->state-ready EQ abap_true
        AND peek_char( ) EQ ` `.
        next_char( ).
      ENDWHILE.
      parse_datum( ).   " skip one datum
    ENDMETHOD.

    METHOD skip_intertoken_space.
      " <intertoken space> -> <atmosphere>*
      " <atmosphere> -> <whitespace> | <comment> | <directive>

      WHILE skip_while( mv_whitespace )
        OR skip_comment( )
        OR match_directive( ).
      ENDWHILE.
      rv_has_next = mi_stream->state-ready.
    ENDMETHOD.

    METHOD decode_escape.
      " <escape> -> <inline hex escape> | <mnemonic escape>

      "  <mnemonic escape> -> \a | \b | \t | \n | \r
      "  <inline hex escape> -> \x<hex scalar value>;

      CASE mi_stream->state-char.
        WHEN c_text_quote    " \" : double quote, U+0022
          OR c_esc_vline     " \| : vertical line, U+007C
          OR c_escape_char.  " \\ : backslash, U+005C
          pchar = mi_stream->state-char.

        WHEN c_esc_a.       " \a : alarm, U+0007
          pchar = lcl_lisp=>char_alarm->value+0(1).

        WHEN c_esc_b.       " \b : backspace, U+0008
          pchar = lcl_lisp=>char_backspace->value+0(1).

        WHEN c_esc_t.       " \t : character tabulation, U+0009
          pchar = lcl_lisp=>char_tab->value+0(1).

        WHEN c_esc_n.       " \n : linefeed, U+000A
          pchar = lcl_lisp=>char_linefeed->value+0(1).

        WHEN c_esc_r.       " \r : return, U+000D
          pchar = lcl_lisp=>char_return->value+0(1).

        WHEN c_number_hex+0(1) OR c_number_hex+1(1).      " inline hex escape:
          pchar = decode_hex_digits( ).

        WHEN OTHERS.
          throw( |invalid Escape char { mi_stream->state-char } found| ).

      ENDCASE.
    ENDMETHOD.

    METHOD match_string.
      "  <string> -> " <string element>* "
      "  <string element> -> <any character other than " or \> | <mnemonic escape> | \" | \\ | \|
      "                      | \<intraline whitespace>*<line ending> <intraline whitespace>*
      "                      | <inline hex escape>
      "  <mnemonic escape> -> \a | \b | \t | \n | \r
      "  <inline hex escape> -> \x<hex scalar value>;
      "  <intraline whitespace> -> <space or tab>
      "  <line ending> -> <newline> | <return> <newline> | <return>

      DATA pchar TYPE tv_char.
      DATA escape_mode TYPE tv_flag VALUE abap_false.
*     " is included in a string as \"

      next_char( ).                 " Skip past opening quote
      WHILE mi_stream->state-ready EQ abap_true
        AND ( mi_stream->state-char NE c_text_quote OR escape_mode EQ abap_true ).
*       cv_val = |{ cv_val }{ char }|.
        pchar = mi_stream->state-char.

        IF pchar EQ c_escape_char AND escape_mode EQ abap_false.
          escape_mode = abap_true.
        ELSE.
          escape_mode = abap_false.
        ENDIF.

        next_char( ).
        IF escape_mode EQ abap_true.

          CASE mi_stream->state-char.
            WHEN space        " \ : intraline whitespace
              OR lcl_lisp=>char_linefeed->value+0(1)
              OR lcl_lisp=>char_return->value+0(1).
              escape_mode = abap_false.
              skip_while( mv_space_or_tab ).  " intraline whitespace

              CONTINUE.

            WHEN OTHERS.
              pchar = decode_escape( ).
          ENDCASE.

          escape_mode = abap_false.
          next_char( ).
        ENDIF.
        CONCATENATE cv_val pchar INTO cv_val RESPECTING BLANKS.
      ENDWHILE.
      next_char( ).                 "Skip past closing quote
    ENDMETHOD.                    "match_string

    METHOD get_atom.
      next_char( ).        " skip #
      next_char( ).        " skip
      rv_val = read_atom( ).
    ENDMETHOD.

    METHOD read_character.
      " <character> -> #\ <any character> | #\ <character name> | #\x<hex scalar value>
      " <character name> -> alarm | backspace | delete | escape | newline | null | return | space | tab

      DATA(sval) = get_atom( ).

      DATA(char_len) = strlen( sval ).
      IF char_len GT 1   " difference between #\x and e.g. #\x30BB
        AND sval+0(1) CO c_number_hex.
        sval = sval+1.            " skip x,  char_len -= 1. not needed anymore
        element = lcl_lisp_new=>esc_charx( sval ).
      ELSE.
        CASE sval.
          WHEN 'alarm'.
            element = lcl_lisp=>char_alarm.
          WHEN 'backspace'.
            element = lcl_lisp=>char_backspace.
          WHEN 'delete'.
            element = lcl_lisp=>char_delete.
          WHEN 'escape'.
            element = lcl_lisp=>char_escape.
          WHEN 'newline'.
            element = lcl_lisp=>char_linefeed.
          WHEN 'null'.
            element = lcl_lisp=>char_null.
          WHEN 'return'.
            element = lcl_lisp=>char_return.
          WHEN 'space' OR space.
            element = lcl_lisp=>char_space.
          WHEN 'tab'.
            element = lcl_lisp=>char_tab.
          WHEN OTHERS.
            IF char_len EQ 1.
              element = lcl_lisp_new=>char( sval ).
            ELSE.
              throw( |unknown char #\\{ sval } found| ).
            ENDIF.
        ENDCASE.
      ENDIF.

    ENDMETHOD.

    METHOD read_exact.
      " Notation for numbers #e or #E (exact) #i o #I (inexact)
      " <exactness> -> <empty> | #i | #e
      next_char( ).        " skip #

      CASE mi_stream->state-char.
        WHEN c_number_exact+0(1) OR c_number_exact+1(1).     "#e or #E (exact)
          rv_exact = abap_true.
        WHEN c_number_inexact+0(1) OR c_number_inexact+1(1). "#i or #I (inexact)
          rv_exact = abap_false.
        WHEN OTHERS.
          throw( `Invalid exactness token in number prefix` ).
      ENDCASE.

      next_char( ).        " skip exactness i/e
    ENDMETHOD.

    METHOD read_number.
      " <num R> -> <prefix R> <complex R>
      " <number> -> <num 2> | <num 8> | <num 10> | <num 16>

      " Possible Cases for number
      " 1. <exactness> <Radix R> <Complex R>
      " 2. <exactness>           <Complex R>
      " 3. <Radix R> <exactness> <Complex R>
      " 4. <Radix R>             <Complex R>
      " 5.                       <Complex R>
      DATA lv_radix_char TYPE tv_char.
      DATA lv_exact_char TYPE tv_char.

      found = abap_true.

      IF iv_peek_char CA c_pattern_exactness.        " <prefix R> -> <exactness> <radix R>
        next_char( ).        " skip #
        lv_exact_char = mi_stream->state-char.
        next_char( ).        " skip exactness i/e

        lv_radix_char = peek_char( ).

        CASE mi_stream->state-char.
          WHEN c_lisp_hash.
            IF lv_radix_char CA c_pattern_radix.
              next_char( ).        " skip #
              next_char( ).        " skip            " Case 1.
            ELSE.
              " a hash but no radix info -> Error
              lcl_lisp=>throw_no_number( read_atom( ) ).
            ENDIF.
          WHEN OTHERS.
            lv_radix_char = space.
            " exactness found but no radix info      " Case 2.
        ENDCASE.

      ELSEIF iv_peek_char CA c_pattern_radix.        " Now check <prefix R> -> <radix R> <exactness>
        lv_radix_char = iv_peek_char.
        next_char( ).        " skip #
        next_char( ).        " skip radix

        " <exactness> -> <empty> | #i | #I | #e | #E
        lv_exact_char = space.                       " Case 4.
        IF mi_stream->state-char EQ c_lisp_hash.
          next_char( ).        " skip #              " Case 3.
          lv_exact_char = mi_stream->state-char.
          next_char( ).        " skip exactness i/e
        ENDIF.

      ELSEIF iv_peek_char EQ space                  " No Prefix, called from (string->number str radix)
        AND mi_stream->state-char CA mv_decimal_initial.  " 1st character is expected in a number

        lv_radix_char = space.
        lv_exact_char = space.                       " Case 5.
      ELSE.
        found = abap_false.
        RETURN.
      ENDIF.

      " <Complex R>
      " <prefix R> -> <radix R> <exactness> | <exactness> <radix R>
      element = lcl_lisp_new=>read_complex( iv_radix = iv_radix
                                            iv_radix_char = lv_radix_char
                                            iv_exact_char = lv_exact_char
                                            io_stream = me ).
    ENDMETHOD.

    METHOD parse_token.
      " <datum> -> <simple datum> | <compound datum> | <label> = <datum> | <label> #
      "<token> -> <identifier> | <boolean> | <number> | <character> | <string> | ( | ) | #( | #u8( | ' | ` | , | ,@ | .
      DATA sval TYPE string.

      skip_intertoken_space( ).
*     create object cell.
      IF mi_stream->state-char CA mv_open_paren.
        element = parse_pair( mi_stream->state-char ).
      ELSE.
        CASE mi_stream->state-char.
          WHEN c_lisp_quote.
*     ' is just a shortcut for QUOTE, so we wrap the consecutive element in a list starting with the quote symbol
*     so that when it is evaluated later, it returns the quote elements unmodified
            next_char( ).            " Skip past single quote
            element = lcl_lisp_new=>quote( parse_token( ) ).

          WHEN c_lisp_backquote.     " Quasiquote
            next_char( ).            " Skip past single quote
            element = lcl_lisp_new=>quasiquote( parse_token( ) ).

          WHEN c_lisp_unquote.
            IF peek_char( ) EQ c_lisp_splicing.  " unquote-splicing ,@
              next_char( ).        " Skip past ,
              next_char( ).        " Skip past @
              element = lcl_lisp_new=>splice_unquote( parse_token( ) ).  " token must evaluate to a list, not be a list
            ELSE.                                " unquote ,
              next_char( ).        " Skip past ,
              element = lcl_lisp_new=>unquote( parse_token( ) ).
            ENDIF.

          WHEN c_text_quote.
            match_string( CHANGING cv_val = sval ).
            element = lcl_lisp_new=>string( value = sval
                                            iv_mutable = abap_false ).

          WHEN c_lisp_hash.
            CASE peek_char( ).
              WHEN c_open_paren.   " Vector constant
                next_char( ).
                element = lcl_lisp_vector=>from_list( io_list = parse_pair( )
                                                      iv_mutable = abap_false ).

              WHEN c_escape_char.  " Character constant  #\a
                element = read_character( ).

              WHEN OTHERS.
*               Boolean #t #f
                "will be handled in match_token( )

                IF read_number( EXPORTING iv_peek_char = peek_char( )
                                IMPORTING element = element ).
                ELSEIF peek_bytevector( ).
                  " Bytevector constant #u8( ... )
                  next_char( ).      " skip #
                  next_char( ).      " skip u
                  next_char( ).      " skip 8
                  element = lcl_lisp_bytevector=>from_list( io_list = parse_pair( )
                                                            iv_mutable = abap_false ).

                ELSE.

                  " Referencing other literal data #<n>= #<n>#
                  DATA lv_label TYPE string.
                  IF match_label( EXPORTING iv_limit = c_lisp_hash
                                  IMPORTING ev_label = lv_label ).
*                   Reference to datum
                    element = lcl_lisp_new=>symbol( |#{ lv_label }#| ).

                  ELSEIF match_label( EXPORTING iv_limit = c_lisp_equal
                                      IMPORTING ev_label = lv_label ).
*                   New datum
*                   Now Add: (set! |#{ lv_label }#| element)
                    element = parse_token( ).
                    element->mv_label = |#{ lv_label }#|.
                  ELSE.
                    " Others <identifier> | <boolean> | <number>
                    element = match_token( ).
                  ENDIF.

                ENDIF.

            ENDCASE.

          WHEN OTHERS.
            " Others <identifier> | <boolean> | <number>
            element = match_token( ).

        ENDCASE.
      ENDIF.
    ENDMETHOD.                    "parse_token

    METHOD match_token.
      " Others <identifier> | <boolean> | <number>
      CONSTANTS:
        c_true_alternative  TYPE string VALUE '#true',
        c_false_alternative TYPE string VALUE '#false'.
      DATA sval TYPE string.

      CASE mi_stream->state-char.
        WHEN c_vertical_line.
          WHILE mi_stream->state-ready EQ abap_true.
            CONCATENATE sval mi_stream->state-char INTO sval RESPECTING BLANKS.
            next_char( ).
            CASE mi_stream->state-char.
              WHEN c_vertical_line.
                CONCATENATE sval mi_stream->state-char INTO sval RESPECTING BLANKS.
                next_char( ).
                EXIT.
              WHEN c_escape_char.
                next_char( ).
                mi_stream->state-char = decode_escape( ).
              WHEN OTHERS.
                CONTINUE.
            ENDCASE.
          ENDWHILE.

        WHEN OTHERS.
          sval = read_atom( ).
      ENDCASE.

      CASE sval.
        WHEN space.
          element = lcl_lisp=>nil.  " or EOF_OBJECT?

        WHEN lcl_lisp=>true->value OR c_true_alternative.
          element = lcl_lisp=>true.

        WHEN lcl_lisp=>false->value OR c_false_alternative.
          element = lcl_lisp=>false.

        WHEN c_lisp_dot
          OR c_lisp_quote
          OR c_lisp_backquote
          OR c_lisp_unquote
          OR ',@'.
          element = lcl_lisp_new=>identifier( sval ).

        WHEN OTHERS.
          " <atom> -> <identifier> | <number>
          element = lcl_lisp_new=>atom( sval ).

      ENDCASE.

    ENDMETHOD.

    METHOD parse_datum.
      parse_token( ).   " Caution: left recursive grammar/expression?
    ENDMETHOD.

    METHOD parse_pair.
      DATA lo_cell TYPE REF TO lcl_lisp.
      DATA lv_empty_list TYPE boole_d VALUE abap_true.
      DATA lv_proper_list TYPE boole_d VALUE abap_true.

*     Set pointer to start of list
      result = lo_cell = lcl_lisp_new=>cons( ).
      DATA(lv_close_delim) = SWITCH #( delim WHEN c_open_bracket THEN c_close_bracket
                                             WHEN c_open_curly THEN c_close_curly
                                             ELSE c_close_paren ).

      next_char( ).                 " Skip past opening paren
      WHILE skip_intertoken_space( ).
        CASE mi_stream->state-char.
          WHEN lv_close_delim.
            IF lv_empty_list = abap_true.
              result = lcl_lisp=>nil.           " Result = empty list
            ELSEIF lv_proper_list EQ abap_true.
              lo_cell->cdr = lcl_lisp=>nil.     " Terminate list
*            ELSE.
*              " pair, no termination with nil, nothing to do
            ENDIF.
            next_char( ).              " Skip past closing paren
            RETURN.

          WHEN c_close_paren OR c_close_bracket OR c_close_curly.
            throw( |a { mi_stream->state-char } found while { lv_close_delim } expected| ).

          WHEN OTHERS.
        ENDCASE.

        IF lv_proper_list EQ abap_false.
*         inconsistent input
          throw( `dotted pair` ).
        ENDIF.

        IF lv_empty_list = abap_true. " First

          lv_empty_list = abap_false. " Next char was not closing paren
          lo_cell->car = parse_token( ).

        ELSE.  " lv_empty_list = abap_false.
*         On at least the second item; add new cell and move pointer

          DATA(lo_peek) = parse_token( ).

          IF lo_peek->type = type_symbol AND lo_peek->value = c_lisp_dot.
            " dotted Pair
            lo_cell->cdr = parse_token( ).
            " match closing parens
            lv_proper_list = abap_false.
          ELSE.

            lo_cell = lo_cell->cdr = lcl_lisp_new=>cons( io_car = lo_peek ).

          ENDIF.

        ENDIF.

      ENDWHILE.
      throw( |missing a { lv_close_delim } to close expression| ).
    ENDMETHOD.

    METHOD peek_bytevector.
      CONSTANTS c_bytevector_prefix TYPE c LENGTH 3 VALUE 'u8('.
      DATA lv_token TYPE string.

      rv_flag = abap_false.
      bookmark( ).

      TRY.
          DO 3 TIMES.
            next_char( ).
            CONCATENATE lv_token mi_stream->state-char INTO lv_token RESPECTING BLANKS.
          ENDDO.
          IF lv_token EQ c_bytevector_prefix.
            rv_flag = abap_true.
          ENDIF.
        CATCH lcx_lisp_exception.
      ENDTRY.
      backtrack( ).
    ENDMETHOD.

    METHOD decode_hex_digits.
      " hex scalar value terminated by semi-colon ;

      "  <inline hex escape> -> \x<hex scalar value>;
      DATA lv_xstr TYPE string.
      DATA lo_char TYPE REF TO lcl_lisp_char.
      next_char( ).
      CLEAR lv_xstr.
      WHILE mi_stream->state-ready EQ abap_true.
        "lv_xstr = |{ lv_xstr }{ char }|.
        CONCATENATE lv_xstr mi_stream->state-char INTO lv_xstr RESPECTING BLANKS.
        next_char( ).
        CHECK mi_stream->state-char EQ c_esc_semi_colon.
        EXIT.
      ENDWHILE.
      CONDENSE lv_xstr.
      IF mi_stream->state-char EQ c_esc_semi_colon.
        lo_char = lcl_lisp_new=>esc_charx( lv_xstr ).
      ELSE.
        throw( |unknown char #\\x{ lv_xstr } found| ).
      ENDIF.

      pchar = lo_char->value+0(1).
    ENDMETHOD.

  ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_parser DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_parser DEFINITION.
    PUBLIC SECTION.
      METHODS
        parse IMPORTING iv_code         TYPE clike
              RETURNING VALUE(elements) TYPE lcl_lisp=>tt_element
              RAISING   lcx_lisp_exception.

      METHODS
        read_from IMPORTING ii_stream      TYPE REF TO lif_stream
                  RETURNING VALUE(element) TYPE REF TO lcl_lisp
                  RAISING   lcx_lisp_exception.

      METHODS
        to_linked_list IMPORTING elements       TYPE lcl_lisp=>tt_element
                                 io_env         TYPE REF TO lcl_lisp_environment
                       RETURNING VALUE(rs_cont) TYPE ts_continuation
                       RAISING   lcx_lisp_exception.
  ENDCLASS.                    "lcl_parser DEFINITION

  CLASS lcx_lisp_escape IMPLEMENTATION.

    METHOD throw.
      RAISE EXCEPTION TYPE lcx_lisp_escape
        EXPORTING
          is_cont  = ms_cont
          io_param = mo_param.
    ENDMETHOD.

    METHOD constructor.
      super->constructor(  ).
      ms_cont = is_cont.
      mo_param = io_param.
    ENDMETHOD.

    METHOD get_text.
      result = `Escape continuation triggered`.
    ENDMETHOD.                    "get_text

    METHOD new_continuation.
      " install the passed continuation as the continuation for the currently
      " executing procedure overriding the previous implicit continuation.
*      IF cont NE ms_cont.
*        throw( ).
*      ENDIF.

      rs_cont = ms_cont.

*      IF ro_elem IS BOUND AND ro_elem->cdr IS BOUND
*        AND ro_elem->cdr->type EQ type_pair.
*        ro_elem = ro_elem->cdr->car.
*      ENDIF.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_escape DEFINITION INHERITING FROM lcl_lisp
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      METHODS constructor IMPORTING cont  TYPE ts_continuation
                                    param TYPE REF TO lcl_lisp
                          RAISING   lcx_lisp_escape.

      METHODS escape IMPORTING is_cont        TYPE ts_continuation
                     RETURNING VALUE(ro_cont) TYPE ts_continuation
                     RAISING   lcx_lisp_escape.
    PRIVATE SECTION.
      DATA error TYPE REF TO lcx_lisp_escape.
  ENDCLASS.

  CLASS lcl_lisp_escape IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_escape_proc ).
      me->error = NEW #( is_cont = cont
                         io_param = param ).
    ENDMETHOD.

    METHOD escape.
      RAISE EXCEPTION error.
    ENDMETHOD.

  ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS lcl_lisp_interpreter DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_interpreter DEFINITION INHERITING FROM lcl_parser.

    PUBLIC SECTION.
      DATA env TYPE REF TO lcl_lisp_environment READ-ONLY. "Global environment
      DATA nil TYPE REF TO lcl_lisp READ-ONLY.
      DATA false TYPE REF TO lcl_lisp READ-ONLY.
      DATA true TYPE REF TO lcl_lisp READ-ONLY.

      CLASS-METHODS new IMPORTING io_port       TYPE REF TO lcl_lisp_port
                                  ii_log        TYPE REF TO lif_log
                        RETURNING VALUE(ro_int) TYPE REF TO lcl_lisp_interpreter.

      METHODS constructor IMPORTING io_port TYPE REF TO lcl_lisp_port
                                    ii_log  TYPE REF TO lif_log.

      METHODS throw IMPORTING message TYPE string
                    RAISING   lcx_lisp_exception.

*     Methods for evaluation
      METHODS:
        eval
          IMPORTING is_cont       TYPE ts_continuation
          RETURNING VALUE(result) TYPE REF TO lcl_lisp
          RAISING   lcx_lisp_exception lcx_lisp_escape,
* To enable a REPL, the following convenience method wraps parsing and evaluating
* and stringifies the response/error
        eval_source
          IMPORTING code            TYPE clike
          RETURNING VALUE(response) TYPE string,
        eval_repl
          IMPORTING code            TYPE clike
          EXPORTING output          TYPE string  " for console output (text format)
          RETURNING VALUE(response) TYPE string
          RAISING   lcx_lisp_exception,
        validate_source
          IMPORTING code            TYPE clike
          RETURNING VALUE(response) TYPE string.

    PROTECTED SECTION.

* Functions for dealing with lists:
      _proc_meth:
      proc_append          ##called,
      proc_append_unsafe   ##called,
      proc_reverse         ##called,
      proc_set_car         ##called,
      proc_set_cdr         ##called,
      proc_car             ##called,
      proc_cdr             ##called,

      proc_caar             ##called,
      proc_cadr             ##called,
      proc_cdar             ##called,
      proc_cddr             ##called,

      proc_caaar            ##called,
      proc_cdaar            ##called,
      proc_caadr            ##called,
      proc_cdadr            ##called,
      proc_cadar            ##called,
      proc_cddar            ##called,
      proc_caddr            ##called,
      proc_cdddr            ##called,
      proc_caaaar           ##called,
      proc_cdaaar           ##called,
      proc_cadaar           ##called,
      proc_cddaar           ##called,
      proc_caaadr           ##called,
      proc_cdaadr           ##called,
      proc_cadadr           ##called,
      proc_cddadr           ##called,
      proc_caadar           ##called,
      proc_cdadar           ##called,
      proc_caddar           ##called,
      proc_cdddar           ##called,
      proc_caaddr           ##called,
      proc_cdaddr           ##called,
      proc_cadddr           ##called,
      proc_cddddr           ##called,

      proc_memq             ##called,
      proc_memv             ##called,
      proc_member           ##called,
      proc_assq             ##called,
      proc_assv             ##called,
      proc_assoc            ##called,

*     Constructor
      proc_cons             ##called,
      proc_list             ##called,
      proc_make_list        ##called,
      proc_iota,            ##called
      proc_list_copy        ##called,

      proc_list_tail        ##called,
      proc_list_ref         ##called,
      proc_list_to_vector   ##called,

      proc_length           ##called,
      proc_nilp             ##called.

* Native functions:
      _proc_meth:
      proc_add       ##called,
      proc_subtract  ##called,
      proc_multiply  ##called,
      proc_divide    ##called,
      proc_gt        ##called,
      proc_gte       ##called,
      proc_lt        ##called,
      proc_lte       ##called,
      proc_eql       ##called,
      proc_eqv       ##called,
      proc_not       ##called,

      proc_values           ##called,

      proc_is_number        ##called,
      proc_is_integer       ##called,
      proc_is_exact_integer ##called,
      proc_is_rational      ##called,
      proc_is_real          ##called,
      proc_is_complex       ##called,

      proc_is_nan           ##called,
      proc_is_finite        ##called,
      proc_is_infinite      ##called,

      proc_is_string       ##called,
      proc_is_char         ##called,
      proc_is_symbol       ##called,
      proc_is_hash         ##called,
      proc_is_procedure    ##called,
      proc_is_list         ##called,
      proc_is_pair         ##called,
      proc_is_boolean      ##called,
      proc_symbol_list_is_equal  ##called,
      proc_boolean_list_is_equal ##called,
      proc_is_vector             ##called,
      proc_is_alist              ##called,

      proc_is_exact              ##called,
      proc_is_inexact            ##called,
      proc_to_exact              ##called,
      proc_to_inexact            ##called,

* Delayed evaluation
      proc_force                 ##called,
      proc_make_promise          ##called,
      proc_is_promise            ##called,

* Math
      proc_abs,      ##called
      proc_quotient, ##called
      proc_sin,      ##called
      proc_cos,      ##called
      proc_tan,      ##called
      proc_asin,     ##called
      proc_acos,     ##called
      proc_atan,     ##called
      proc_sinh,     ##called
      proc_cosh,     ##called
      proc_tanh,     ##called
      proc_asinh,    ##called
      proc_acosh,    ##called
      proc_atanh,    ##called
      proc_exp,      ##called
      proc_expt,     ##called
      proc_log,      ##called
      proc_sqrt,     ##called
      proc_square,   ##called

      proc_make_rectangular, ##called
      proc_make_polar,       ##called
      proc_real_part,        ##called
      proc_imag_part,        ##called
      proc_magnitude,        ##called
      proc_angle,            ##called

      proc_int_sqrt,        ##called
      proc_floor_quotient,  ##called
      proc_floor_remainder, ##called
      proc_trunc_quotient,  ##called
      proc_trunc_remainder, ##called
      proc_rationalize,     ##called

      proc_is_zero,      ##called
      proc_is_positive,  ##called
      proc_is_negative,  ##called
      proc_is_odd,       ##called
      proc_is_even,      ##called

      proc_floor,        ##called
      proc_floor_new,    ##called
      proc_ceiling,      ##called
      proc_truncate,     ##called
      proc_truncate_new, ##called
      proc_round,        ##called

      proc_numerator,    ##called
      proc_denominator,  ##called
      proc_remainder,    ##called
      proc_modulo,       ##called
      proc_max,          ##called
      proc_min,          ##called
      proc_gcd,          ##called
      proc_lcm,          ##called
* Formating
      proc_newline           ##called,
      proc_write             ##called,
      proc_display           ##called,
      proc_read              ##called,
      proc_write_char        ##called,
      proc_write_u8          ##called,
      proc_write_string      ##called,
      proc_write_bytevector  ##called,
      proc_read_char         ##called,
      proc_read_line         ##called,
      proc_read_string       ##called,
      proc_read_bytevector   ##called,
      proc_peek_char         ##called,
      proc_is_char_ready     ##called,

      proc_read_u8            ##called,
      proc_peek_u8            ##called,
      proc_is_u8_ready        ##called,

      proc_is_char_alphabetic ##called,
      proc_is_char_numeric    ##called,
      proc_is_char_whitespace ##called,
      proc_is_char_upper_case ##called,
      proc_is_char_lower_case ##called,
      proc_digit_value        ##called,
      proc_char_to_integer    ##called,
      proc_integer_to_char    ##called,
      proc_char_upcase        ##called,
      proc_char_downcase      ##called,

      proc_char_list_is_eq    ##called,
      proc_char_list_is_lt    ##called,
      proc_char_list_is_gt    ##called,
      proc_char_list_is_le    ##called,
      proc_char_list_is_ge    ##called,

      proc_char_ci_list_is_eq    ##called,
      proc_char_ci_list_is_lt    ##called,
      proc_char_ci_list_is_gt    ##called,
      proc_char_ci_list_is_le    ##called,
      proc_char_ci_list_is_ge    ##called,

      proc_string_list_is_eq    ##called,
      proc_string_list_is_lt    ##called,
      proc_string_list_is_gt    ##called,
      proc_string_list_is_le    ##called,
      proc_string_list_is_ge    ##called,

      proc_string_ci_list_is_eq    ##called,
      proc_string_ci_list_is_lt    ##called,
      proc_string_ci_list_is_gt    ##called,
      proc_string_ci_list_is_le    ##called,
      proc_string_ci_list_is_ge    ##called,

      proc_string                ##called,
      proc_make_string,          ##called
      proc_num_to_string,        ##called
      proc_list_to_string,       ##called
      proc_symbol_to_string,     ##called
      proc_string_length,        ##called
      proc_string_copy,          ##called
      proc_string_to_num,        ##called
      proc_string_ref,           ##called
      proc_string_set,           ##called
      proc_string_append,        ##called
      proc_string_to_list,       ##called
      proc_string_to_symbol,     ##called

* Turtle library
      proc_turtle_new,            ##called "turtles
      proc_turtle_exist,          ##called "turtles?
      proc_turtle_move,           ##called "move
      proc_turtle_draw,           ##called "draw
      proc_turtle_erase,          ##called "erase
      proc_turtle_move_offset,    ##called "move-offset
      proc_turtle_draw_offset,    ##called "draw-offset
      proc_turtle_erase_offset,   ##called "erase-offset
      proc_turtle_turn_degrees,   ##called "turn
      proc_turtle_turn_radians,   ##called "turn/radians
      proc_turtle_set_pen_width,  ##called "set-pen-width
      proc_turtle_set_pen_color,  ##called "set-pen-color
      proc_turtle_merge,          ##called
      proc_turtle_clean,          ##called
      proc_turtle_width,          ##called
      proc_turtle_height,         ##called
      proc_turtle_state,          ##called
      proc_turtle_pen_width,      ##called
      proc_turtle_pen_color,      ##called
      proc_turtle_regular_poly,   ##called
      proc_turtle_regular_polys,  ##called

* Continuation
      proc_call_cc               ##called,
      proc_dynamic_wind          ##called,
      proc_call_with_values      ##called,
* Exceptions
      proc_error,                  ##called
      proc_is_error_object,        ##called
      proc_is_read_error,          ##called
      proc_is_file_error,          ##called
      proc_error_object_message,   ##called
      proc_error_object_irritants, ##called
      proc_raise,                  ##called

* Not in the spec: Just adding it anyway
      proc_random,       ##called
      proc_eq,           ##called
      proc_equal.        ##called

* SQL
      _proc_meth:
      proc_sql_prepare     ##called,
      proc_sql_query       ##called.

* Functions for dealing with vectors:
      _proc_meth:
      proc_make_vector,    ##called
      proc_vector,         ##called
      proc_vector_length,  ##called
      proc_vector_set,     ##called
      proc_vector_fill,    ##called
      proc_vector_ref,     ##called
      proc_vector_to_list. ##called

      " bytevectors:
      _proc_meth:
      proc_is_bytevector,         ##called
      proc_make_bytevector,       ##called
      proc_bytevector,            ##called
      proc_bytevector_length,     ##called
      proc_bytevector_u8_set,     ##called
      proc_bytevector_u8_ref,     ##called
      proc_bytevector_append,     ##called
      proc_bytevector_new_copy,   ##called
      proc_bytevector_copy,       ##called
      proc_string_to_utf8,        ##called
      proc_utf8_to_string.        ##called

* Functions for dealing with hashes:
      _proc_meth:
      proc_make_hash,    ##called "Create new hash
      proc_hash_get,     ##called "Get an element from a hash
      proc_hash_insert,  ##called "Insert a new element into a hash
      proc_hash_remove,  ##called "Delete an item from a hash
      proc_hash_keys.    ##called "Delete an item from a hash

* Ports
      _proc_meth:
      proc_make_parameter         ##called,
      proc_parameterize           ##called,
      proc_is_input_port          ##called,
      proc_is_output_port         ##called,
      proc_is_textual_port        ##called,
      proc_is_binary_port         ##called,
      proc_is_port                ##called,
      proc_is_eof_object          ##called,
      proc_is_open_input_port     ##called,
      proc_is_open_output_port    ##called,
      proc_eof_object             ##called,
      proc_close_input_port       ##called,
      proc_close_output_port      ##called,
      proc_close_port             ##called,
      proc_current_input_port     ##called,
      proc_current_output_port    ##called,
      proc_current_error_port     ##called,
      proc_open_output_string     ##called,
      proc_open_input_string      ##called,
      proc_get_output_string      ##called.

* Built-in functions for ABAP integration:
      _proc_meth:
      proc_abap_data,           ##called
      proc_abap_function,       ##called
      proc_abap_function_param, ##called
      proc_abap_table,          ##called
      proc_abap_append_row,     ##called
      proc_abap_delete_row,     ##called
      proc_abap_get_row,        ##called
      proc_abap_get_value,      ##called
      proc_abap_set_value,      ##called
      proc_abap_set,            ##called
      proc_abap_get,            ##called
* Called internally only:
      proc_abap_function_call. ##called

      METHODS define_syntax
        IMPORTING element       TYPE REF TO lcl_lisp
                  environment   TYPE REF TO lcl_lisp_environment
        RETURNING VALUE(result) TYPE  REF TO lcl_lisp
        RAISING   lcx_lisp_exception.

      METHODS bind_symbol
        IMPORTING element       TYPE REF TO lcl_lisp
                  environment   TYPE REF TO lcl_lisp_environment
                  iv_category   TYPE tv_category DEFAULT tv_category_standard
        RETURNING VALUE(result) TYPE  REF TO lcl_lisp
        RAISING   lcx_lisp_exception.

      METHODS define_values
        IMPORTING element       TYPE REF TO lcl_lisp
                  environment   TYPE REF TO lcl_lisp_environment
                  iv_category   TYPE tv_category DEFAULT tv_category_standard
        RETURNING VALUE(result) TYPE  REF TO lcl_lisp
        RAISING   lcx_lisp_exception.

      METHODS assign_symbol
        IMPORTING element       TYPE REF TO lcl_lisp
                  environment   TYPE REF TO lcl_lisp_environment
        RETURNING VALUE(result) TYPE  REF TO lcl_lisp
        RAISING   lcx_lisp_exception.

      METHODS is_macro_call
        IMPORTING cont          TYPE ts_continuation
        RETURNING VALUE(result) TYPE tv_flag
        RAISING   lcx_lisp_exception.

      METHODS syntax_expand
        IMPORTING cont          TYPE ts_continuation
        RETURNING VALUE(result) TYPE  REF TO lcl_lisp
        RAISING   lcx_lisp_exception.

      METHODS generate_symbol IMPORTING list          TYPE REF TO lcl_lisp
                              RETURNING VALUE(result) TYPE  REF TO lcl_lisp
                              RAISING   lcx_lisp_exception.

*----  ABAP Integration support functions; mapping -----
      METHODS:
*       Convert ABAP data to Lisp element
        data_to_element IMPORTING VALUE(data)    TYPE any
                        RETURNING VALUE(element) TYPE REF TO lcl_lisp
                        RAISING   lcx_lisp_exception,

        structure_to_element IMPORTING VALUE(struct)  TYPE any
                             RETURNING VALUE(element) TYPE REF TO lcl_lisp
                             RAISING   lcx_lisp_exception,

        table_to_element IMPORTING VALUE(data)    TYPE any
                         RETURNING VALUE(element) TYPE REF TO lcl_lisp
                         RAISING   lcx_lisp_exception,
*       Convert Lisp element to ABAP Data
        element_to_data IMPORTING VALUE(element) TYPE REF TO lcl_lisp
                        CHANGING  VALUE(data)    TYPE any "ref to data
                        RAISING   lcx_lisp_exception,
*       Determine an ABAP data component from an element and an identifier
        get_element IMPORTING list         TYPE REF TO lcl_lisp
                    RETURNING VALUE(rdata) TYPE REF TO data
                    RAISING   lcx_lisp_exception.

      CLASS-DATA gi_log TYPE REF TO lif_log.
      CLASS-DATA: go_input_port  TYPE REF TO lcl_lisp_lambda,
                  go_output_port TYPE REF TO lcl_lisp_lambda,
                  go_error_port  TYPE REF TO lcl_lisp_lambda.
      CLASS-DATA gensym_counter TYPE i.

      METHODS write IMPORTING io_elem       TYPE REF TO lcl_lisp
                              io_arg        TYPE REF TO lcl_lisp DEFAULT lcl_lisp=>nil
                    RETURNING VALUE(result) TYPE REF TO lcl_lisp
                    RAISING   lcx_lisp_exception.

      METHODS write_u8 IMPORTING io_elem       TYPE REF TO lcl_lisp
                                 io_arg        TYPE REF TO lcl_lisp DEFAULT lcl_lisp=>nil
                       RETURNING VALUE(result) TYPE REF TO lcl_lisp
                       RAISING   lcx_lisp_exception.

      METHODS display IMPORTING io_elem       TYPE REF TO lcl_lisp
                                io_arg        TYPE REF TO lcl_lisp
                      RETURNING VALUE(result) TYPE REF TO lcl_lisp
                      RAISING   lcx_lisp_exception.

      METHODS read IMPORTING io_arg        TYPE REF TO lcl_lisp
                   RETURNING VALUE(result) TYPE REF TO lcl_lisp
                   RAISING   lcx_lisp_exception.

      METHODS read_char IMPORTING io_arg        TYPE REF TO lcl_lisp
                        RETURNING VALUE(result) TYPE REF TO lcl_lisp
                        RAISING   lcx_lisp_exception.

      METHODS read_u8 IMPORTING io_arg        TYPE REF TO lcl_lisp
                      RETURNING VALUE(result) TYPE REF TO lcl_lisp
                      RAISING   lcx_lisp_exception.

      METHODS read_string IMPORTING io_arg        TYPE REF TO lcl_lisp
                          RETURNING VALUE(result) TYPE REF TO lcl_lisp
                          RAISING   lcx_lisp_exception.

      METHODS read_bytevector IMPORTING io_arg        TYPE REF TO lcl_lisp
                              RETURNING VALUE(result) TYPE REF TO lcl_lisp
                              RAISING   lcx_lisp_exception.

      METHODS read_line IMPORTING io_arg        TYPE REF TO lcl_lisp
                        RETURNING VALUE(result) TYPE REF TO lcl_lisp
                        RAISING   lcx_lisp_exception.

    PRIVATE SECTION.
      TYPES: BEGIN OF ts_digit,
               zero  TYPE x LENGTH 3,  " SCPUCHAR
               langu TYPE string,
             END OF ts_digit.
      TYPES tt_digit TYPE SORTED TABLE OF ts_digit WITH UNIQUE KEY zero.
      DATA mt_zero TYPE tt_digit.

      METHODS string_to_number IMPORTING iv_text       TYPE string
                                         iv_radix      TYPE tv_int DEFAULT 10
                               RETURNING VALUE(result) TYPE REF TO lcl_lisp
                               RAISING   lcx_lisp_exception.

      METHODS simplest_rational IMPORTING low TYPE REF TO lcl_lisp_number
                                          high TYPE REF TO lcl_lisp_number
                                          iv_exact TYPE tv_flag
                                RETURNING VALUE(result) TYPE REF TO lcl_lisp
                                RAISING   lcx_lisp_exception.

      METHODS simplest_rational_internal IMPORTING x TYPE tv_real
                                                   y TYPE tv_real
                                         RETURNING VALUE(num) TYPE ts_number
                                         RAISING   lcx_lisp_exception.

      METHODS complex_log IMPORTING x           TYPE tv_real
                                    y           TYPE tv_real
                                    base        TYPE tv_real DEFAULT 1
                                    iv_exact    TYPE tv_flag DEFAULT abap_false
                          RETURNING VALUE(ln_z) TYPE REF TO lcl_lisp_number.

      METHODS complex_sqrt IMPORTING x           TYPE numeric
                                     y           TYPE numeric
                                     iv_exact    TYPE tv_flag DEFAULT abap_false
                           RETURNING VALUE(sqrt_z) TYPE REF TO lcl_lisp_number.

      METHODS unicode_digit_zero RETURNING VALUE(rt_zero) TYPE tt_digit.

      METHODS unicode_to_digit IMPORTING iv_char         TYPE tv_char
                               RETURNING VALUE(rv_digit) TYPE i.

      METHODS unicode_to_integer IMPORTING iv_char       TYPE tv_char
                                 RETURNING VALUE(rv_int) TYPE tv_int.

      METHODS char_to_integer IMPORTING io_char       TYPE REF TO lcl_lisp
                              RETURNING VALUE(rv_int) TYPE tv_int
                              RAISING   lcx_lisp_exception.

      METHODS fold_case IMPORTING element          TYPE REF TO lcl_lisp
                        RETURNING VALUE(rv_string) TYPE string.

      METHODS char_fold_case_to_integer IMPORTING element       TYPE REF TO lcl_lisp
                                        RETURNING VALUE(rv_int) TYPE tv_int.

      METHODS char_case_identity IMPORTING element          TYPE REF TO lcl_lisp
                                 RETURNING VALUE(rv_string) TYPE string.

      METHODS create_element_from_data
        IMPORTING ir_data       TYPE REF TO data
        RETURNING VALUE(result) TYPE REF TO lcl_lisp.

      METHODS get_structure_field IMPORTING element           TYPE REF TO lcl_lisp_data
                                            VALUE(identifier) TYPE REF TO lcl_lisp
                                  RETURNING VALUE(rdata)      TYPE REF TO data
                                  RAISING   lcx_lisp_exception.
      METHODS get_table_row_with_key IMPORTING element           TYPE REF TO lcl_lisp_data
                                               VALUE(identifier) TYPE REF TO lcl_lisp
                                     RETURNING VALUE(rdata)      TYPE REF TO data
                                     RAISING   lcx_lisp_exception.
      METHODS get_index_table_row IMPORTING element           TYPE REF TO lcl_lisp_data
                                            VALUE(identifier) TYPE REF TO lcl_lisp
                                  RETURNING VALUE(rdata)      TYPE REF TO data
                                  RAISING   lcx_lisp_exception.

      METHODS evaluate_parameters IMPORTING io_list        TYPE REF TO lcl_lisp
                                            environment    TYPE REF TO lcl_lisp_environment
                                  RETURNING VALUE(ro_head) TYPE REF TO lcl_lisp
                                  RAISING   lcx_lisp_exception.

      METHODS expand_apply IMPORTING io_list       TYPE REF TO lcl_lisp
                                     environment   TYPE REF TO lcl_lisp_environment
                           RETURNING VALUE(result) TYPE REF TO lcl_lisp
                           RAISING   lcx_lisp_exception.

      METHODS expand_map IMPORTING io_list       TYPE REF TO lcl_lisp
                                   environment   TYPE REF TO lcl_lisp_environment
                         RETURNING VALUE(result) TYPE REF TO lcl_lisp
                         RAISING   lcx_lisp_exception.

      METHODS expand_for_each IMPORTING io_list       TYPE REF TO lcl_lisp
                                        environment   TYPE REF TO lcl_lisp_environment
                              RETURNING VALUE(result) TYPE REF TO lcl_lisp
                              RAISING   lcx_lisp_exception.

      METHODS eval_list_tco CHANGING  cs_cont       TYPE ts_continuation
                            RETURNING VALUE(result) TYPE REF TO lcl_lisp
                            RAISING   lcx_lisp_exception.

      METHODS lambda_environment IMPORTING io_head       TYPE REF TO lcl_lisp
                                           io_args       TYPE REF TO lcl_lisp
                                           environment   TYPE REF TO lcl_lisp_environment
                                 RETURNING VALUE(ro_env) TYPE  REF TO lcl_lisp_environment
                                 RAISING   lcx_lisp_exception.

      METHODS extract_arguments IMPORTING io_head TYPE REF TO lcl_lisp
                                EXPORTING eo_pars TYPE REF TO lcl_lisp
                                          eo_args TYPE REF TO lcl_lisp
                                RAISING   lcx_lisp_exception.

      METHODS extract_parameter_objects IMPORTING io_head TYPE REF TO lcl_lisp
                                                  io_env  TYPE REF TO lcl_lisp_environment
                                        EXPORTING eo_pars TYPE REF TO lcl_lisp
                                                  eo_args TYPE REF TO lcl_lisp
                                        RAISING   lcx_lisp_exception.

      METHODS eval_do_step IMPORTING io_command TYPE REF TO lcl_lisp
                                     io_steps   TYPE REF TO lcl_lisp
                                     io_env     TYPE REF TO lcl_lisp_environment
                           RAISING   lcx_lisp_exception.

      METHODS eval_do_init IMPORTING io_head       TYPE REF TO lcl_lisp
                                     VALUE(io_env) TYPE REF TO lcl_lisp_environment
                           EXPORTING eo_step       TYPE REF TO lcl_lisp
                                     eo_env        TYPE REF TO lcl_lisp_environment
                           RAISING   lcx_lisp_exception.

      METHODS eval_ast IMPORTING cont          TYPE ts_continuation
                       RETURNING VALUE(result) TYPE REF TO lcl_lisp
                       RAISING   lcx_lisp_exception lcx_lisp_escape.

      METHODS evaluate_in_sequence IMPORTING io_pars TYPE REF TO lcl_lisp
                                             io_args TYPE REF TO lcl_lisp
                                             io_env  TYPE REF TO lcl_lisp_environment
                                   RAISING   lcx_lisp_exception.
      METHODS environment_letrec IMPORTING io_head       TYPE REF TO lcl_lisp
                                           io_env        TYPE REF TO lcl_lisp_environment
                                 RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment
                                 RAISING   lcx_lisp_exception.
      METHODS environment_letrec_star IMPORTING io_head       TYPE REF TO lcl_lisp
                                                io_env        TYPE REF TO lcl_lisp_environment
                                      RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment
                                      RAISING   lcx_lisp_exception.

      METHODS environment_let_star IMPORTING io_head       TYPE REF TO lcl_lisp
                                             io_env        TYPE REF TO lcl_lisp_environment
                                   RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment
                                   RAISING   lcx_lisp_exception.
      METHODS environment_named_let IMPORTING VALUE(io_env) TYPE REF TO lcl_lisp_environment
                                    CHANGING  co_head       TYPE REF TO lcl_lisp
                                    RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment
                                    RAISING   lcx_lisp_exception.
      METHODS environment_parameterize IMPORTING VALUE(io_env)  TYPE REF TO lcl_lisp_environment
                                                 VALUE(io_head) TYPE REF TO lcl_lisp
                                       RETURNING VALUE(ro_env)  TYPE REF TO lcl_lisp_environment
                                       RAISING   lcx_lisp_exception.

      METHODS table_of_lists IMPORTING io_head         TYPE REF TO lcl_lisp
                                       environment     TYPE REF TO lcl_lisp_environment
                             RETURNING VALUE(rt_table) TYPE tt_lisp
                             RAISING   lcx_lisp_exception.

      METHODS map_next_expr IMPORTING io_proc       TYPE REF TO lcl_lisp
                            EXPORTING ev_has_next   TYPE tv_flag
                            CHANGING  ct_list       TYPE tt_lisp
                            RETURNING VALUE(result) TYPE REF TO lcl_lisp
                            RAISING   lcx_lisp_exception.

      METHODS is_constant IMPORTING exp            TYPE REF TO lcl_lisp
                          RETURNING VALUE(rv_flag) TYPE tv_flag.
      METHODS combine IMPORTING left          TYPE REF TO lcl_lisp
                                right         TYPE REF TO lcl_lisp
                                exp           TYPE REF TO lcl_lisp
                                environment   TYPE REF TO lcl_lisp_environment
                      RETURNING VALUE(result) TYPE REF TO lcl_lisp
                      RAISING   lcx_lisp_exception.

      METHODS quasiquote IMPORTING exp           TYPE REF TO lcl_lisp
                                   nesting       TYPE tv_index
                                   environment   TYPE REF TO lcl_lisp_environment
                         RETURNING VALUE(result) TYPE REF TO lcl_lisp
                         RAISING   lcx_lisp_exception.  " ?

      METHODS list_reverse IMPORTING io_list       TYPE REF TO lcl_lisp
                           RETURNING VALUE(result) TYPE REF TO lcl_lisp
                           RAISING   lcx_lisp_exception.

      METHODS list_length IMPORTING list          TYPE REF TO lcl_lisp
                          RETURNING VALUE(result) TYPE tv_int
                          RAISING   lcx_lisp_exception.

      METHODS list_tail IMPORTING list          TYPE REF TO lcl_lisp
                                  k             TYPE tv_index
                                  area          TYPE string
                        RETURNING VALUE(result) TYPE REF TO lcl_lisp
                        RAISING   lcx_lisp_exception.

      METHODS get_equal_params IMPORTING io_list    TYPE REF TO lcl_lisp
                               EXPORTING eo_sublist TYPE REF TO lcl_lisp
                                         eo_compare TYPE REF TO lcl_lisp
                                         eo_key     TYPE REF TO lcl_lisp
                               RAISING   lcx_lisp_exception.
  ENDCLASS.                    "lcl_lisp_interpreter DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_parser IMPLEMENTATION
*----------------------------------------------------------------------*
  CLASS lcl_parser IMPLEMENTATION.

    METHOD parse.
      elements = NEW lcl_stream( NEW lcl_string_stream( iv_code ) )->parse( ).
    ENDMETHOD.                    "parse

    METHOD read_from.
      element = NEW lcl_stream( ii_stream )->read_stream( ).
      "     ii_port->put( substring( val = code
      "                              off = index ) ).
    ENDMETHOD.

    METHOD to_linked_list.
      " Parse and convert to linked list of continuation records
      DATA lr_cont TYPE tr_continuation.
      DATA lr_next TYPE tr_continuation.

      " Convert to linked list
      LOOP AT elements INTO DATA(lo_element).
        lr_next = NEW ts_continuation( elem = lo_element
                                       env = io_env ).
        IF rs_cont IS INITIAL.
          rs_cont-next = lr_cont = lr_next.
        ELSE.
          lr_cont->next = lr_next.
          lr_cont = lr_next.
        ENDIF.
      ENDLOOP.
    ENDMETHOD.

  ENDCLASS.                    "lcl_parser IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_interpreter IMPLEMENTATION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_interpreter IMPLEMENTATION.

    METHOD new.
      CREATE OBJECT ro_int
        EXPORTING
          io_port = io_port
          ii_log  = ii_log.
    ENDMETHOD.

    METHOD throw.
      lcl_lisp=>throw( message ).
    ENDMETHOD.

    METHOD constructor.
      super->constructor( ).
      nil = lcl_lisp=>nil.
      true = lcl_lisp=>true.     " Note: Predicative method calls are not possible and even dangerous (wrong)
      false = lcl_lisp=>false.   "       as IF get_object( ). translate to IF get_object( ) IS NOT INITIAL.

      go_input_port ?= proc_make_parameter( lcl_lisp_new=>cons( io_car = io_port ) ).
      go_output_port = go_error_port = go_input_port.
      gi_log = ii_log.
      mt_zero = unicode_digit_zero( ).

      env = lcl_lisp_env_factory=>make_top_level( ).
    ENDMETHOD.                    "constructor

    METHOD assign_symbol.
      " set! -> reassign a bound symbol
      _validate: element, element->car.
      result = element->car.

      CASE result->type.
        WHEN type_symbol.
*         re-define symbol in the original environment, but evaluate parameters in the current environment
          environment->scope_of( result->value )->set( symbol  = result->value
                                                       element = eval( VALUE #( elem = element->cdr->car
                                                                                env = environment ) ) ).
        WHEN OTHERS.
          result->raise( | is not a bound symbol| ).
      ENDCASE.
    ENDMETHOD.

    METHOD bind_symbol.
      DATA lv_symbol TYPE string.
      DATA lo_params TYPE REF TO lcl_lisp.
      " Scheme does not return a value for define; but we are returning the new symbol reference
      DATA(lo_head) = element->car.
      _validate lo_head.
      CASE lo_head->type.
        WHEN type_symbol.
          " call the set method of the current environment using the unevaluated first parameter
          " (second list element) as the symbol key and the evaluated second parameter as the value.
          lo_params = eval( VALUE #( elem = element->cdr->car
                                     env = environment ) ).
          lo_params->category = iv_category.
          lv_symbol = lo_head->value.

          " Function shorthand (define (id arg ... ) body ...+)
        WHEN type_pair.
          IF element->cdr EQ nil.
            lo_head->raise( ` no expression in body` ).
          ENDIF.
          " define's function shorthand allows us to define a function by specifying a list as the
          " first argument where the first element is a symbol and consecutive elements are arguments
          lo_params = lcl_lisp_new=>lambda( io_car = lo_head->cdr  "List of params following function symbol
                                            io_cdr = element->cdr
                                            io_env = environment
                                            iv_category = iv_category ).
          lv_symbol = lo_head->car->value.

        WHEN OTHERS.
          lo_head->raise( | cannot be a variable identifier| ).
      ENDCASE.

      " Add function to the environment with symbol
      environment->set( symbol  = lv_symbol
                        element = lo_params
                        once = xsdbool( env NE environment ) ). " Internal definition => once!

      result = lcl_lisp_new=>symbol( lv_symbol ).
    ENDMETHOD.

    METHOD define_syntax.
*
      result = bind_symbol( element = element
                            environment = environment
                            iv_category = tv_category_macro ).
    ENDMETHOD.

    METHOD define_values.
      DATA lv_symbol TYPE string.
      DATA lo_params TYPE REF TO lcl_lisp.
      DATA lo_values TYPE REF TO lcl_lisp_values.

      DATA(lo_head) = element->car.
      CASE lo_head->type.
        WHEN type_symbol.
          " call the set method of the current environment using the unevaluated first parameter
          " (second list element) as the symbol key and the evaluated second parameter as the value.
          lo_params = eval( VALUE #( elem = element->cdr->car
                                     env = environment ) ).
          lo_params->category = iv_category.
          " Add function to the environment with symbol
          environment->set( symbol  = lo_head->value
                            element = lo_params
                            once = xsdbool( env NE environment ) ). " Internal definition => once!

        WHEN type_pair.
          IF element->cdr EQ nil.
            lo_head->raise( | no expression in body| ).
          ENDIF.
          lo_params = eval( VALUE #( elem = element->cdr->car
                                     env = environment ) ).
          CASE lo_params->type.
            WHEN type_values.
              lo_values ?= lo_params.
              lo_params = lo_values->head.
              " Add function to the environment with symbol
              environment->parameters_to_symbols( io_pars = lo_head
                                                  io_args = lo_params ).
            WHEN OTHERS.
              lv_symbol = lo_head->car->value.
              " Add function to the environment with symbol
              environment->set( symbol  = lv_symbol
                                element = lo_params
                                once = xsdbool( env NE environment ) ). " Internal definition => once!
          ENDCASE.

        WHEN OTHERS.
          lo_head->raise( | cannot be a variable identifier| ).
      ENDCASE.

      result = lcl_lisp_new=>values( lo_head ).
    ENDMETHOD.

    METHOD is_macro_call.
*     returns true if element is a list that contains a symbol as the first element and that symbol refers to a function
*     in the environment and that function has the macro attribute set to true. Otherwise, it returns false.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      _validate cont-elem.
      result = abap_false.
      CHECK cont-elem->type EQ type_pair AND cont-elem->car->type = type_symbol.
      TRY.
          lo_ptr = cont-env->get( cont-elem->car->value ).
          CHECK lo_ptr->is_procedure( ).
          result = xsdbool( lo_ptr->category EQ tv_category_macro ).
        CATCH lcx_lisp_exception.
          RETURN.
      ENDTRY.
    ENDMETHOD.

    METHOD syntax_expand.
      DATA lo_lambda TYPE REF TO lcl_lisp.
      DATA lo_args TYPE REF TO lcl_lisp.
      _validate cont-elem.

      DATA(ls_cont) = cont.
      WHILE is_macro_call( ls_cont ).
        lo_lambda = cont-env->get( ls_cont-elem->car->value ).
        lo_args = ls_cont-elem->cdr.

        ls_cont-env = lo_lambda->environment.

        ls_cont-env->parameters_to_symbols( io_args = lo_args             " Pointer to argument
                                            io_pars = lo_lambda->car ).   " Pointer to formal parameters

        ls_cont-elem = lcl_lisp_new=>cons( io_car = lo_lambda
                                           io_cdr = lo_args ).
        ls_cont-elem = eval( ls_cont ).
      ENDWHILE.
      result = ls_cont-elem.
    ENDMETHOD.

    METHOD generate_symbol.
      DATA lv_index TYPE tv_index.
      DATA lv_suffix TYPE string VALUE 'G0as'.
      DATA lv_counter TYPE i.
      DATA lo_opt TYPE REF TO lcl_lisp.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_string TYPE REF TO lcl_lisp_string.

      ADD 1 TO gensym_counter.
      lv_counter = lv_index = gensym_counter.

      IF list NE nil AND list->car IS BOUND.
        lo_opt = list->car.
      ENDIF.

      IF lo_opt IS BOUND AND lo_opt->type EQ type_integer.
        lo_int ?= lo_opt.
        IF lo_int->int GE 0.
          lv_counter = lo_int->int.
        ENDIF.
      ENDIF.

      IF lo_opt IS BOUND AND lo_opt->type EQ type_string.
        lo_string ?= lo_opt.
        lv_suffix = lo_string->value.
      ENDIF.

      DATA(lv_name) = |#:{ lv_suffix }{ lv_counter }|.
      result = lcl_lisp_new=>symbol( value = lv_name
                                     index = lv_index ). " uninterned symbols have integer > 0
    ENDMETHOD.

    METHOD evaluate_parameters.
*     This routine is called very, very often!
      DATA lo_arg TYPE REF TO lcl_lisp.
      DATA lo_new TYPE REF TO lcl_lisp.
      DATA cont TYPE ts_continuation.
*     Before execution of the procedure or lambda, all parameters must be evaluated
      _validate io_list.

      ro_head = nil.
      CHECK io_list NE nil. " AND io_list->car NE nil.

      cont-env = environment.
      DATA(elem) = io_list.
*     ##TO DO: check if circular list are allowed
      WHILE elem->type EQ type_pair.
        cont-elem = elem->car.
        lo_new = lcl_lisp_new=>cons( io_car = eval_ast( cont ) ).
        IF ro_head = nil.
          lo_arg = ro_head = lo_new.
        ELSE.
          lo_arg = lo_arg->cdr = lo_new.
        ENDIF.
        elem = elem->cdr.
      ENDWHILE.

      IF elem NE nil.  " _validate_tail
*       if the last element in the list is not a cons cell, we cannot append
        io_list->error_not_a_list( space ).
      ENDIF.

    ENDMETHOD.                    "evaluate_parameters

    METHOD expand_apply.
      _validate io_list.
      DATA(lo_proc) = io_list->car.     " proc
      DATA(lo_arg) = io_list->cdr.      " handle arg1 . . . rest-args

      _validate lo_arg.
*     (apply proc arg1 . . . argn rest)
*     Parameter io_list is list arg1 ... argn rest

      result = io_list.
      CHECK lo_arg NE nil.

*     At least one argument = rest
      DATA(lo_new) = lcl_lisp_new=>cons( io_car = lo_proc ).
      result = lo_new.

*     Collect arg1 to argn
*     ##TO DO: check if circular lists are allowed
      WHILE lo_arg->cdr->type EQ type_pair.
*       At least two entries (argn and rest), build (list arg1 . . argn )

        lo_new = lo_new->cdr = lcl_lisp_new=>cons( io_car = lo_arg->car ).
        lo_arg = lo_arg->cdr.
      ENDWHILE.

*     now (append (list arg1 . . argn ) rest )
      DATA(lo_rest) = eval_ast(  VALUE #( elem = lo_arg->car
                                          env = environment ) ).

*     ##TO DO: check if circular lists are allowed
      WHILE lo_rest->type EQ type_pair.  " e.g. NE nil
        lo_new = lo_new->cdr = lcl_lisp_new=>box_quote( lo_rest->car ).
        lo_rest = lo_rest->cdr.
      ENDWHILE.

      lo_new->cdr = lo_rest.

    ENDMETHOD.

*   (define (map f lst)
*     (if (null? lst)
*       '()
*       (cons (f (car lst)) (map f (cdr lst)))))
    METHOD expand_map.
*     (map proc list1 list2 ... ) The lists should all have the same length.
*     Proc should accept as many arguments as there are lists and return a single value.
*     Proc should not mutate any of the lists.
* The map procedure applies proc element-wise to the elements of the lists and returns a list of the results, in order.
* Proc is always called in the same dynamic environment as map itself. The order in which proc is applied to the
* elements of the list s is unspecified. If multiple returns occur from map, the values returned by earlier returns
* are not mutated.
      DATA lo_map TYPE REF TO lcl_lisp.
      _validate: io_list, io_list->car.

      result = nil.
      DATA(lo_proc) = io_list->car.

      DATA(lt_list) = table_of_lists( io_head = io_list->cdr         " parameter evaluated lists
                                      environment = environment ).

      DATA(lv_has_next) = xsdbool( lines( lt_list ) GT 0 ). " map terminates when the shortest list runs out.

      WHILE lv_has_next EQ abap_true.
        DATA(ls_cont) = VALUE ts_continuation( elem = map_next_expr( EXPORTING io_proc = lo_proc
                                                                     IMPORTING ev_has_next = lv_has_next
                                                                     CHANGING  ct_list = lt_list )
                                               env = environment ).
        DATA(lo_head) = lcl_lisp_new=>cons( io_car = eval( ls_cont ) ).
*       create function call (proc list1[k] list2[k]... listn[k]); add result as k-th list element
        IF result EQ nil. " 1st element of new list
          lo_map = result = lo_head.
        ELSE.
          lo_map = lo_map->cdr = lo_head.
        ENDIF.
      ENDWHILE.

    ENDMETHOD.

    METHOD expand_for_each.
*     (for-each proc list1 list2 ... ) The lists should all have the same length.
*     Proc should accept as many arguments as there are lists and return a single value.
*     Proc should not mutate any of the lists.
* The for-each procedure applies proc element-wise to the elements of the lists for its side effects, in order from the
* first elements to the last.
* Proc is always called in the same dynamic environment as for-each itself. The return values of for-each are unspecified.
      _validate: io_list, io_list->car.

      result = nil.
      DATA(lo_proc) = io_list->car.

      DATA(lt_list) = table_of_lists( io_head = io_list->cdr
                                      environment = environment ).

      DATA(lv_has_next) = xsdbool( lines( lt_list ) GT 0 ).  " for-each terminates when the shortest list runs out.
      WHILE lv_has_next EQ abap_true.
*       evaluate function call (proc list1[k] list2[k]... listn[k])
        DATA(ls_cont) = VALUE ts_continuation( elem = map_next_expr( EXPORTING io_proc = lo_proc
                                                                     IMPORTING ev_has_next = lv_has_next
                                                                     CHANGING  ct_list = lt_list )
                                               env = environment ).
        result = eval( ls_cont ).
      ENDWHILE.
    ENDMETHOD.

    METHOD eval_do_init.
*     <init> expressions are evaluated (in unspecified order), the <variable>s are bound to fresh locations,
*     the results of the <init> expressions are stored in the bindings of the <variable>s.
*     A <step> can be omitted, in which case the effect is the same as if (<variable> <init> <variable>)
*     had been written instead of (<variable> <init>).
      _validate io_head.

      eo_env = lcl_lisp_env_factory=>clone( io_env ).
      eo_step = nil.

      DATA(lo_loop) = io_head.
      WHILE lo_loop->type EQ type_pair AND lo_loop->car->type EQ type_pair.
        DATA(lo_spec) = lo_loop->car.
*       max. 3 entries
*       <variable>
        DATA(lo_var) = lo_spec->car.

*       <init>

        lo_spec = lo_spec->cdr.
        IF lo_spec NE nil.
          DATA(lo_init) = lo_spec->car.
          DATA(ls_cont) = VALUE ts_continuation( elem = lo_init
                                                 env = io_env ).
          eo_env->set( symbol = lo_var->value
                       element = eval_ast( ls_cont )  " inits are evaluated in org. environment
                       once = abap_true ).
          lo_spec = lo_spec->cdr.
          IF lo_spec NE nil.
*           <step>
            DATA(lo_next) = lcl_lisp_new=>cons( io_car = lcl_lisp_new=>cons( io_car = lo_var
                                                                             io_cdr = lo_spec->car ) ).
            IF eo_step EQ nil.  " first
              eo_step = lo_next.
              DATA(lo_ptr) = eo_step.
            ELSE.
              lo_ptr = lo_ptr->cdr = lo_next.
            ENDIF.

          ENDIF.
        ENDIF.
*       Next iteration control
        lo_loop = lo_loop->cdr.
      ENDWHILE.

    ENDMETHOD.

    METHOD eval_do_step.
*     <command> expressions are evaluated in order for effect
      DATA(lo_command) = io_command.

*     Evaluate in order
      WHILE lo_command->type EQ type_pair.
        eval( VALUE #( elem = lo_command->car
                       env = io_env ) ).
        lo_command = lo_command->cdr.
      ENDWHILE.

      DATA(lo_local_env) = lcl_lisp_env_factory=>new( ).
*     the <step> expressions are evaluated in some unspecified order
      DATA(lo_step) = io_steps.
      WHILE lo_step->type EQ type_pair.
        DATA(lo_ptr) = lo_step->car.

*       <variable>s are bound to fresh locations to avoid dependencies in the next step
        lo_local_env->set( symbol = lo_ptr->car->value
                           element = eval_ast( VALUE #( elem = lo_ptr->cdr
                                                        env = io_env ) ) ).
        lo_step = lo_step->cdr.
      ENDWHILE.

      lo_step = io_steps.
      WHILE lo_step->type EQ type_pair.
        DATA(lv_symbol) = lo_step->car->car->value.

*       the results of the <step>s are stored in the bindings of the <variable>s
        io_env->set( symbol = lv_symbol
                     element = lo_local_env->get( lv_symbol ) ).
        lo_step = lo_step->cdr.
      ENDWHILE.

    ENDMETHOD.

    METHOD eval_list_tco. " Tail Call Optimization
*     Evaluate all expressions except the last one to be evaluated as a tail call
*     ( eval LOOP for the last evaluation step )
      _validate cs_cont-elem.
      DATA(lo_head) = cs_cont-elem.
      result = nil.

      CHECK lo_head NE nil.

      WHILE cs_cont-elem IS BOUND
        AND cs_cont-elem->type EQ type_pair
        AND cs_cont-elem->cdr NE nil.  " Do not evaluate the last list element

        result = eval_ast( VALUE #( BASE cs_cont
                                    elem = cs_cont-elem->car ) ).
        cs_cont-elem = cs_cont-elem->cdr.
      ENDWHILE.

      IF cs_cont-elem->cdr NE nil. " _validate_tail cs_cont-elem->cdr lo_head space.
*       if the last element in the list is not a cons cell, we cannot append
        lo_head->error_not_a_list( space ).
      ENDIF.
    ENDMETHOD.

    METHOD lambda_environment.
      DATA lo_args TYPE REF TO lcl_lisp.
*     The function (LAMBDA) receives its own local environment in which to execute,
*     where parameters become symbols that are mapped to the corresponding arguments
      _validate io_head.
      ro_env = lcl_lisp_env_factory=>clone( io_head->environment ).

      IF io_head->category EQ tv_category_macro.
        lo_args = io_args.
      ELSE.
        lo_args = evaluate_parameters( io_list = io_args           " Pointer to arguments
                                       environment = environment ).
      ENDIF.

      ro_env->parameters_to_symbols( io_args = lo_args
                                     io_pars = io_head->car ).   " Pointer to formal parameters
    ENDMETHOD.

    METHOD extract_arguments.
      DATA lo_arg TYPE REF TO lcl_lisp.
      DATA lo_par TYPE REF TO lcl_lisp.
      _validate io_head.
      eo_args = eo_pars = nil.                "list of parameters

      CHECK io_head->car IS BOUND AND io_head->car NE nil.
      DATA(lo_ptr) = io_head->car.

      _validate lo_ptr->car.
      lo_par = lcl_lisp_new=>cons( io_car = lo_ptr->car ).

      IF lo_ptr->cdr IS BOUND AND lo_ptr->cdr NE nil.
        lo_arg = lcl_lisp_new=>cons( io_car = lo_ptr->cdr->car ).
      ENDIF.

      eo_pars = lo_par.
      eo_args = lo_arg.

      lo_ptr = io_head->cdr.
      WHILE lo_ptr->type EQ type_pair. " IS BOUND AND lo_ptr NE nil.
*       Rest of list, pick head
        DATA(lo_first) = lo_ptr->car.
        IF lo_first IS BOUND AND lo_first->car NE nil.
          lo_par = lo_par->cdr = lcl_lisp_new=>cons( io_car = lo_first->car ).
        ENDIF.

        DATA(lo_second) = lo_first->cdr.
        IF lo_second IS BOUND AND lo_second NE nil.
          lo_arg = lo_arg->cdr = lcl_lisp_new=>cons( io_car = lo_second->car ).
        ENDIF.

        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      lo_par->cdr = lo_arg->cdr = nil.

*     Debug help: DATA lv_debug TYPE string.
*      lv_debug = |params { eo_pars->to_string( ) }\n arg { eo_args->to_string( ) }\n|.
    ENDMETHOD.                    "extract_arguments

    METHOD evaluate_in_sequence.
*     Before execution of the procedure or lambda, all parameters must be evaluated
      _validate: io_args, io_pars.
      DATA(lo_args) = io_args->new_iterator( ).
      DATA(lo_pars) = io_pars->new_iterator( ).

      WHILE lo_args->has_next( ) AND lo_pars->has_next( ).
        DATA(lo_par) = lo_pars->next( ).
        CHECK lo_par NE nil.        " Nil would mean no parameters to map
*       Assign argument to its corresponding symbol in the newly created environment
*       NOTE: element of the argument list is evaluated before being defined in the environment
        io_env->set( symbol = lo_par->value
                     element = eval( VALUE #( elem = lo_args->next( )
                                              env = io_env ) )
                     once = abap_false ).
      ENDWHILE.
    ENDMETHOD.

    METHOD extract_parameter_objects.
      DATA lo_val TYPE REF TO lcl_lisp.
      DATA lo_par TYPE REF TO lcl_lisp.
      DATA lo_param TYPE REF TO lcl_lisp.
      DATA lo_values TYPE REF TO lcl_lisp.
      DATA lo_lambda TYPE REF TO lcl_lisp_lambda.
      DATA lv_parameter_object TYPE tv_flag VALUE abap_false.

      DEFINE _to_param_object.
        &2 = eval( VALUE #( elem = &1
                            env = io_env ) ).
        IF &2->type EQ type_lambda.
          lo_lambda ?= &2.
          lv_parameter_object = lo_lambda->parameter_object.
        ENDIF.
        IF lv_parameter_object EQ abap_false.
          lcl_lisp=>throw( |missing parameter object in parameterize| ).
        ENDIF.
      END-OF-DEFINITION.

      _validate io_head.
      lo_values = eo_pars = eo_args = nil.                "list of parameter objects

      CHECK io_head->car IS BOUND AND io_head->car NE nil.
      DATA(lo_ptr) = io_head->car.

      _validate lo_ptr->car.
      _to_param_object lo_ptr->car lo_param.
      lo_par = lcl_lisp_new=>cons( io_car = lo_param ).

      IF lo_ptr->cdr IS BOUND AND lo_ptr->cdr NE nil.
        lo_val = lcl_lisp_new=>cons( io_car = lo_ptr->cdr->car ).
      ENDIF.

      eo_pars = lo_par.
      lo_values = lo_val.

      lo_ptr = io_head->cdr.
      WHILE lo_ptr->type EQ type_pair. " IS BOUND AND lo_ptr NE nil.
*       Rest of list, pick head
        DATA(lo_first) = lo_ptr->car.
        IF lo_first IS BOUND AND lo_first->car NE nil.
          _to_param_object lo_first->car lo_param.
          lo_par = lo_par->cdr = lcl_lisp_new=>cons( io_car = lo_param ).
        ENDIF.

        DATA(lo_second) = lo_first->cdr.
        IF lo_second IS BOUND AND lo_second NE nil.
          lo_val = lo_val->cdr = lcl_lisp_new=>cons( io_car = lo_second->car ).
        ENDIF.

        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      lo_par->cdr = lo_val->cdr = nil.

      eo_args = evaluate_parameters( io_list = lo_values       " Pointer to values
                                     environment = io_env ).
    ENDMETHOD.

* A letrec expression is equivalent to a let where the bindings are initialized with dummy values,
* and then the initial values are computed and assigned into the bindings.
    METHOD environment_letrec.
      extract_arguments( EXPORTING io_head = io_head
                         IMPORTING eo_pars = DATA(lo_pars)
                                   eo_args = DATA(lo_args) ).

      ro_env = lcl_lisp_env_factory=>clone( io_env ).
*     setup the environment before evaluating the initial value expressions
      DATA(lo_par) = lo_pars.
      DATA(lo_arg) = lo_args.
      WHILE lo_par IS BOUND AND lo_par NE nil   " Nil means no parameters to map
        AND lo_arg IS BOUND AND lo_arg NE nil.  " Nil means no arguments

        ro_env->set( symbol = lo_par->car->value
                     element = lo_arg->car
                     once = abap_true ).
        lo_par = lo_par->cdr.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

*     the initial value computions execute inside the new environment
      DATA(lo_new_args) = evaluate_parameters( io_list = lo_args           " Pointer to arguments
                                               environment = ro_env ).

      ro_env->parameters_to_symbols( io_args = lo_new_args
                                     io_pars = lo_pars ).   " Pointer to formal parameters
*     to allow (define) with the same variable name as in (letrec ( )), create new scope
      ro_env = lcl_lisp_env_factory=>clone( ro_env ).
    ENDMETHOD.

    METHOD environment_letrec_star.
      extract_arguments( EXPORTING io_head = io_head
                         IMPORTING eo_pars = DATA(lo_pars)
                                   eo_args = DATA(lo_args) ).

      ro_env = lcl_lisp_env_factory=>clone( io_env ).

      DATA(lo_par) = lo_pars.
      DATA(lo_arg) = lo_args.
      WHILE lo_par IS BOUND AND lo_par NE nil   " Nil means no parameters to map
        AND lo_arg IS BOUND AND lo_arg NE nil.  " Nil means no parameters to map

        ro_env->set( symbol = lo_par->car->value
                     element = lo_arg->car
                     once = abap_true ).
        lo_par = lo_par->cdr.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

      evaluate_in_sequence( io_args = lo_args      " Pointer to arguments e.g. (4, (+ x 4)
                            io_pars = lo_pars      " Pointer to formal parameters (x y)
                            io_env = ro_env ).
*     to allow (define) with the same variable name as in (let* ( )), create new scope
      ro_env = lcl_lisp_env_factory=>clone( ro_env ).
    ENDMETHOD.

*Here's an example loop, which prints out the integers from 0 to 9:
* (  let loop ((i 0))
*     (display i)
*     (if (< i 10)
*         (loop (+ i 1))))
*
*The example is exactly equivalent to:
*  (letrec ((loop (lambda (i)      ; define a recursive
*                    (display i)   ; procedure whose body
*                    (if (< i 10)  ; is the loop body
*                        (loop (+ i 1))))))
*     (loop 0)) ; start the recursion with 0 as arg i
    METHOD environment_named_let.
      CASE co_head->car->type.
        WHEN type_symbol.
*named let:  (let <variable> (bindings) <body>)
          DATA(lo_var) = co_head->car.
          co_head = co_head->cdr.

        WHEN OTHERS. " type_pair.
*(let ((x 10) (y 5)) (+ x y)) is syntactic sugar for  ( (lambda (x y) (+ x y)) 10 5)
          lo_var = nil.
      ENDCASE.

      extract_arguments( EXPORTING io_head = co_head->car
                         IMPORTING eo_pars = DATA(lo_pars)
                                   eo_args = DATA(lo_args) ).
      ro_env = lcl_lisp_env_factory=>clone( io_env ).

      DATA(lo_new_args) = evaluate_parameters( io_list = lo_args       " Pointer to arguments
                                               environment = io_env ).
      ro_env->parameters_to_symbols( io_args = lo_new_args
                                     io_pars = lo_pars ).              " Pointer to formal parameters

      IF lo_var IS BOUND AND lo_var NE nil.
*       named let
        ro_env->set( symbol = lo_var->value
                     element = lcl_lisp_new=>lambda( io_car = lo_pars                " List of parameters
                                                     io_cdr = co_head->cdr           " Body
                                                     io_env = ro_env )
                     once = abap_true ).
      ENDIF.
*     to allow (define) with the same variable name in the body of (let ( )), create new scope
      ro_env = lcl_lisp_env_factory=>clone( ro_env ).
    ENDMETHOD.

    METHOD environment_parameterize.
      extract_parameter_objects( EXPORTING io_head = io_head->car
                                           io_env = io_env
                                 IMPORTING eo_pars = DATA(lo_pars)
                                           eo_args = DATA(lo_new_args) ).

      ro_env = lcl_lisp_env_factory=>clone( io_env ).
      ro_env->parameters_to_symbols( io_args = lo_new_args
                                     io_pars = lo_pars ).              " Pointer to parameter objects

*     to allow (define) with the same variable name in the body of (let ( )), create new scope
      ro_env = lcl_lisp_env_factory=>clone( ro_env ).
    ENDMETHOD.

    METHOD environment_let_star.
      extract_arguments( EXPORTING io_head = io_head
                         IMPORTING eo_pars = DATA(lo_pars)
                                   eo_args = DATA(lo_args) ).
      ro_env = lcl_lisp_env_factory=>clone( io_env ).

      evaluate_in_sequence( io_args = lo_args      " Pointer to arguments e.g. (4, (+ x 4)
                            io_pars = lo_pars      " Pointer to formal parameters (x y)
                            io_env = ro_env ).
*     to allow (define) with the same variable name in the body of (let* ( )), create new scope
      ro_env = lcl_lisp_env_factory=>clone( ro_env ).
    ENDMETHOD.

    METHOD eval_ast.
*     Evaluate element, Element is not a list
      CASE cont-elem->type.
        WHEN type_symbol. "Symbol
*         lookup the symbol in the environment and return the value or raise an error if no value is found
          result = cont-env->get( cont-elem->value ).

        WHEN type_pair. " List
          result = eval( cont ).

        WHEN type_hash. " TEST
          result = lcl_lisp_hash=>new( it_hash = VALUE #( FOR ls_entry IN CAST lcl_lisp_hash( cont-elem )->hash
                                                          ( VALUE #( key = ls_entry-key
                                                                     element = eval( VALUE #( BASE cont
                                                                                        elem = ls_entry-element ) ) ) ) ) ).

        WHEN type_vector. " TEST
          result = lcl_lisp_new=>vector( it_vector = VALUE tt_lisp( FOR lo_elem IN CAST lcl_lisp_vector( cont-elem )->array
                                                        ( eval( VALUE #( BASE cont
                                                                         elem = lo_elem ) ) ) )
                                         iv_mutable = abap_true ).

        WHEN OTHERS.
*         otherwise just return the original AST value
          result = cont-elem.  "Number or string evaluates to itself (also: vector constant)

      ENDCASE.
      IF result IS NOT BOUND.
        lcl_lisp=>throw( c_error_eval ).
      ENDIF.

    ENDMETHOD.

    DEFINE _tail_expression.
      IF &1 NE nil.
        cont-elem = &1->car.    " Tail context
        CONTINUE.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE _tail_sequence.
      IF cont-elem NE nil.
        result = eval_list_tco( CHANGING cs_cont = cont ).
        _tail_expression cont-elem.
      ELSEIF cont-env->top_level EQ abap_false.
        throw( c_error_no_exp_in_body ).
      ELSE.  " empty (begin) ?
        CONTINUE.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE _validate_quote.
      IF NOT ( &1->cdr->type = type_pair AND &1->cdr->cdr = nil ).
        &1->car->raise( | invalid form in { &2 }| ).
      ENDIF.
    END-OF-DEFINITION.

    METHOD is_constant.
*    (define (constant? exp)
*      (if (pair? exp) (eq? (car exp) 'quote) (not (symbol? exp))))
      IF exp->type EQ type_pair.
        rv_flag = xsdbool( exp->car = lcl_lisp=>quote ).
      ELSE.
        rv_flag = xsdbool( exp->type NE type_symbol ).
      ENDIF.
    ENDMETHOD.

    METHOD combine.

      IF is_constant( left ) AND is_constant( right ).
*       (eqv? (eval right) (cdr exp)))
        DATA(eval_left) = eval( VALUE #( elem = left
                                         env = environment ) ).
        DATA(eval_right) = eval( VALUE #( elem = right
                                          env = environment ) ).
        IF eval_left = exp->car AND eval_right = exp->cdr.
*         (list 'quote exp)
          result = lcl_lisp_new=>quote( exp ).
        ELSE.
*         (list 'quote (cons (eval left) (eval right)))))
          result = lcl_lisp_new=>quote(
                      lcl_lisp_new=>cons( io_car = eval_left
                                          io_cdr = eval_right ) ).
        ENDIF.
      ELSEIF right = nil.
*       ((null? right) (list 'list left))
        result = lcl_lisp_new=>box( io_proc = lcl_lisp=>list
                                    io_elem = left ).
      ELSEIF right->type = type_pair AND right->car = lcl_lisp=>list.
*       ((and (pair? right) (eq? (car right) 'list))
*        (cons 'list (cons left (cdr right))))
        result = lcl_lisp_new=>cons( io_car = lcl_lisp=>list
                                     io_cdr = lcl_lisp_new=>cons( io_car = left
                                                                  io_cdr = right->cdr ) ).
      ELSE.
*      (else (list 'cons left right))))
        result = lcl_lisp_new=>list3( io_first = lcl_lisp=>cons
                                      io_second = left
                                      io_third = right ).
      ENDIF.
    ENDMETHOD.

*     `atom/nil  -> 'atom/nil
*     `,expr     -> expr
*     `,@expr    -> error
*     ``expr     -> `expr-expanded
*     `list-expr -> expand each element and handle dotted tails:
*        `(x1 x2 ... xn)     -> (append y1 y2 ... yn)
*        `(x1 x2 ... . xn)   -> (append y1 y2 ... 'xn)
*        `(x1 x2 ... . ,xn)  -> (append y1 y2 ... xn)
*        `(x1 x2 ... . ,@xn) -> error
*      where each yi is the output of (qq-transform xi)
    METHOD quasiquote.
*     adapted from http://norvig.com/jscheme/primitives.scm
      DATA(lo_ptr) = exp.

      CASE lo_ptr->type.
        WHEN type_pair. "non empty list
*         ((and (eq? (car exp) 'unquote) (= (length exp) 2))
          DATA(lo_first) = lo_ptr->car.
          DATA(lo_next) = lo_ptr->cdr.
          _validate lo_next.

          IF ( lo_first = lcl_lisp=>unquote
              OR ( lo_first->type EQ type_symbol AND lo_first->value EQ c_eval_unquote ) )
              AND list_length( exp ) EQ 2.
*           ((and (eq? (first = 'unquote) (= (length exp) 2))
            _validate_quote lo_ptr c_eval_unquote.

            IF nesting = 0.

              result = lo_next->car.

            ELSE.

              result = combine( left = lcl_lisp=>unquote
                                right = quasiquote( exp = lo_next
                                                    nesting = nesting - 1
                                                    environment = environment )
                                exp = exp
                                environment = environment ).
            ENDIF.

          ELSEIF ( lo_first = lcl_lisp=>quasiquote
              OR ( lo_first->type EQ type_symbol AND lo_first->value EQ c_eval_quasiquote ) )
              AND list_length( exp ) EQ 2.
            _validate_quote lo_ptr c_eval_quasiquote.
*           (and (eq? (car exp) 'quasiquote) (= (length exp) 2))

            result = combine( left = lcl_lisp=>quasiquote
                              right = quasiquote( exp = lo_next
                                                  nesting = nesting + 1
                                                  environment = environment )
                              exp = exp
                              environment = environment ).

          ELSEIF lo_first->type EQ type_pair AND
            ( lo_first->car = lcl_lisp=>unquote_splicing OR lo_first->car->value EQ c_eval_unquote_splicing )
            AND list_length( lo_first ) EQ 2.
*           ((and (pair? (car exp))
*                (eq? (caar exp) 'unquote-splicing)
*                (= (length (car exp)) 2))
            _validate_quote lo_first c_eval_unquote_splicing.

            IF nesting = 0.
*             (list 'append (second (first exp))
*                           (expand-quasiquote (cdr exp) nesting))
              result = lcl_lisp_new=>list3( io_first = lcl_lisp=>append
                                            io_second = lo_first->cdr->car
                                            io_third = quasiquote( exp = lo_next
                                                                   nesting = nesting
                                                                   environment = environment ) ).
            ELSE.

              result = combine( left = quasiquote( exp = lo_first
                                                   nesting = nesting - 1
                                                   environment = environment )
                                right = quasiquote( exp = lo_next
                                                    nesting = nesting
                                                    environment = environment )
                                exp = exp
                                environment = environment ).
            ENDIF.

          ELSE.

            result = combine( left = quasiquote( exp = lo_first
                                                 nesting = nesting
                                                 environment = environment )
                              right = quasiquote( exp = lo_next
                                                  nesting = nesting
                                                  environment = environment )
                              exp = exp
                              environment = environment ).
          ENDIF.

        WHEN type_vector.
*         (list 'apply 'vector (expand-quasiquote (vector->list exp) nesting)))
          DATA(lo_vec) = CAST lcl_lisp_vector( exp ).
          result = lcl_lisp_new=>box( io_proc = lcl_lisp_new=>symbol( 'list->vector' )
                                      io_elem = quasiquote( exp = lo_vec->to_list( )
                                                            nesting = nesting
                                                            environment = environment ) ).
        WHEN OTHERS.
*         (if (constant? exp) exp (list 'quote exp)))
          IF is_constant( exp ).
            result = exp.
          ELSE.
            result = lcl_lisp_new=>quote( exp ).
          ENDIF.

      ENDCASE.

    ENDMETHOD.

**********************************************************************
*------------------------------- EVAL( ) ----------------------------
* eval takes an expression and an environment to a value
**********************************************************************
    METHOD eval.
      DATA(cont) = is_cont.   " partial continuation

      DO.
        TRY.

            _validate cont-elem.

            CASE cont-elem.
              WHEN nil OR true OR false.
*           Return predefined symbols as themselves to save having to look them up in the environment
                result = cont-elem.

              WHEN OTHERS.
                IF cont-elem->type EQ type_pair.
                  cont-elem = syntax_expand( cont ).
                ENDIF.

*           Evaluate element
                CASE cont-elem->type.
                  WHEN type_pair. " List
*               return a new list that is the result of calling EVAL on each of the members of the list

*               To evaluate list, we must first evaluate head value
*               Evaluate first element of list to determine if it is a native procedure or lambda
                    DATA(lr_head) = cont-elem->car.
                    DATA(lr_tail) = cont-elem->cdr.

                    CASE lr_head->value.

                      WHEN c_eval_quote. " Literal expression: Return the argument to quote unevaluated
                        IF lr_tail->cdr NE nil.
                          throw( |quote can only take a single argument| ).
                        ENDIF.
                        result = lr_tail->car.

                      WHEN c_eval_quasiquote.
                        IF lr_tail->cdr NE nil.
                          throw( |quasiquote can only take a single argument| ).
                        ENDIF.
                        cont-elem = quasiquote( exp = lr_tail->car
                                                nesting = 0
                                                environment = cont-env ).

                        CONTINUE.  "tail_expression lo_elem.

                      WHEN 'and'.
*                   Derived expression: Conditional (and <expression>* <tail expression>)
                        result = true.
                        DATA(lo_ptr) = lr_tail.
                        WHILE result NE false AND lo_ptr IS BOUND AND lo_ptr NE nil AND lo_ptr->cdr NE nil.
                          result = eval_ast( VALUE #( BASE cont
                                                      elem = lo_ptr->car ) ).
                          lo_ptr = lo_ptr->cdr.
                        ENDWHILE.
                        IF result NE false.
                          _tail_expression lo_ptr.
                        ENDIF.

                      WHEN 'or'.
*                   Derived expression: Conditional (or <expression>* <tail expression>)
                        result = false.
                        lo_ptr = lr_tail.
                        WHILE result EQ false AND lo_ptr IS BOUND AND lo_ptr NE nil AND lo_ptr->cdr NE nil.
                          result = eval_ast( VALUE #( BASE cont
                                                      elem = lo_ptr->car ) ).
                          lo_ptr = lo_ptr->cdr.
                        ENDWHILE.
                        IF result EQ false.
                          _tail_expression lo_ptr.
                        ENDIF.

                      WHEN 'cond'.
*                   Derived expression: Conditional
                        lo_ptr = lr_tail.
                        cont-elem = nil.
                        DATA(lo_test) = false.
                        WHILE lo_ptr->type EQ type_pair.
                          DATA(lo_clause) = lo_ptr->car.
                          lo_ptr = lo_ptr->cdr.

                          IF lo_clause->car->value EQ c_lisp_else.
                            IF lo_ptr->type EQ type_pair.
                              throw( `else clause must be last in cond` ).
                            ENDIF.
                            IF lo_test EQ false.  " only if clause was not previously matched
                              cont-elem = lo_clause->cdr.
                              "lo_test = true.
                            ENDIF.
                            EXIT.
                          ELSEIF lo_test EQ false.
                            lo_test = eval_ast( VALUE #( BASE cont
                                                         elem = lo_clause->car ) ).
                            IF lo_test NE false.
                              cont-elem = lo_clause->cdr.
                            ENDIF.
                            " clause matched, but continue to check if eventual 'else' clause is correctly specified last
                          ENDIF.
                        ENDWHILE.
                        IF cont-elem EQ nil.
                          result = lo_test.
                        ELSEIF cont-elem->car->value = c_lisp_then.
                          cont-elem = lcl_lisp_new=>cons( io_car = cont-elem->cdr->car
                                                          io_cdr = lcl_lisp_new=>box_quote( lo_test ) ).
                          CONTINUE.
                          "tail_expression lo_elem.
                        ELSE.
                          _tail_sequence.
                        ENDIF.

                      WHEN 'define'.
*                   Variable definition:
*           call the set method of the current environment using the unevaluated first parameter
*           (second list element) as the symbol key and the evaluated second parameter as the value.
                        result = bind_symbol( element = lr_tail
                                              environment = cont-env ).

                      WHEN 'define-macro'.
                        result = bind_symbol( element = lr_tail
                                              environment = cont-env
                                              iv_category = tv_category_macro ).

                      WHEN 'define-syntax'.
                        result = define_syntax( element = lr_tail
                                                environment = cont-env ).

                      WHEN 'define-values'.
                        result = define_values( element = lr_tail
                                                environment = cont-env ).

                      WHEN 'define-record-type'.
                        "  (define-record-type <name> <constructor> <pred> <field> ... )
                        " Syntax: <name> and <pred> are identifiers. The <constructor> is of the form
                        " (<constructor name> <field name> ... ) and each <field> is either of the form
                        " (field name> <accessor name>) or of the form (<field name> <accessor name> <modifier name>)
                        throw( |define-record-type not implemented yet| ).

                      WHEN 'set!'.                        " Re-Assign symbol
                        result = assign_symbol( element     = lr_tail
                                                environment = cont-env ).

                      WHEN 'if'.
                        " _validate lr_tail->cdr. "I do not have a test case yet where it fails here
                        IF eval( VALUE #( BASE cont
                                          elem = lr_tail->car ) ) NE false.

                          cont-elem = lr_tail->cdr->car. " Tail context
                          CONTINUE.

                        ELSEIF lr_tail->cdr->cdr = nil.
                          result = lcl_lisp=>undefined.
                        ELSE.
                          _validate lr_tail->cdr->cdr.
                          cont-elem = lr_tail->cdr->cdr->car. " Tail context
                          CONTINUE.

                        ENDIF.

                      WHEN 'when'.
                        result = nil.
                        IF eval( VALUE #( BASE cont
                                          elem = lr_tail->car ) ) NE false.
                          cont-elem = lr_tail->cdr. "  _validate lr_tail->cdr. "I do not have a test case yet where it fails here
                          _tail_sequence.
                        ENDIF.

                      WHEN 'unless'.
                        result = nil.
                        IF eval( VALUE #( BASE cont
                                          elem = lr_tail->car ) ) EQ false.
                          cont-elem = lr_tail->cdr.  "  _validate lr_tail->cdr. "I do not have a test case yet where it fails here
                          _tail_sequence.
                        ENDIF.

                      WHEN 'begin'.
                        cont-elem = lr_tail.
*                    _tail_sequence.
                        IF cont-elem NE nil.
                          result = eval_list_tco( CHANGING cs_cont = cont ).
                          _tail_expression cont-elem.
                        ELSE.  " empty (begin) ?
                          CONTINUE.
                        ENDIF.

                      WHEN 'let'.
                        cont-env = environment_named_let( EXPORTING io_env = cont-env
                                                          CHANGING co_head = lr_tail ).
                        cont-elem = lr_tail->cdr.
                        _tail_sequence.

                      WHEN 'let*'.
                        cont-env = environment_let_star( io_head = lr_tail->car
                                                         io_env = cont-env ).
                        cont-elem = lr_tail->cdr.
                        _tail_sequence.

                      WHEN 'letrec'.
*                   (letrec ((a 5) (b (+ a 3)) b)
                        cont-env = environment_letrec( io_head = lr_tail->car
                                                       io_env = cont-env ).
                        cont-elem = lr_tail->cdr.
                        _tail_sequence.

                      WHEN 'letrec*'.
                        cont-env = environment_letrec_star( io_head = lr_tail->car
                                                            io_env = cont-env ).

                        cont-elem = lr_tail->cdr.
                        _tail_sequence.

*                  WHEN 'let-values'.
*                  WHEN 'let*-values'.
*                  WHEN 'let-syntax'.
*                  WHEN 'letrec-syntax'.

                      WHEN 'parameterize'.
                        cont-env = environment_parameterize( io_env = cont-env
                                                             io_head = lr_tail ).
                        cont-elem = lr_tail->cdr.
                        _tail_sequence.

                      WHEN 'lambda'.
                        result = lcl_lisp_new=>lambda( io_car = lr_tail->car         " List of parameters
                                                       io_cdr = lr_tail->cdr         " Body
                                                       io_env = cont-env ).

                      WHEN 'case-lambda'.
                        DATA lt_clauses TYPE tt_lisp.
                        _validate lr_tail.
                        CLEAR lt_clauses.
                        lo_clause = lr_tail.
                        WHILE lo_clause->type = type_pair.
                          lr_tail = lo_clause->car.
                          lo_ptr = lcl_lisp_new=>lambda( io_car = lr_tail->car         " List of parameters
                                                         io_cdr = lr_tail->cdr         " Body
                                                         io_env = cont-env ).
                          APPEND lo_ptr TO lt_clauses.
                          lo_clause = lo_clause->cdr.
                        ENDWHILE.
                        IF lo_clause NE nil.
                          throw( `Invalid case-lambda clause` ).
                        ENDIF.
                        result = lcl_lisp_new=>case_lambda( lt_clauses ).

*(do ((<variable1> <init1> <step1>) ... ) <-- iteration spec
*     (<test> <do result> ... )           <-- tail sequence
*     <command> ... )
* Example:
*   (do ((vec (make-vector 5) )
*         (i 0 (+ i 1) ) )
*         ((= i 5) vec)
*       (vector-set! vec i i))  => #(0 1 2 3 4)
* A do expression is an iteration construct. It specifies a set of variables to be bound,
* how they are to be initialized at the start, and how they are to be updated on each iteration.
* When a termination condition is met, the loop exits after evaluating the <expression>s.
                      WHEN 'do'.
                        DATA(lo_head) = lr_tail.
                        _validate: lo_head, lo_head->cdr, lo_head->cdr->cdr.

*                   Initialization
                        eval_do_init( EXPORTING io_head = lo_head->car
                                                io_env = cont-env
                                      IMPORTING eo_step = DATA(lo_steps)
                                                eo_env = cont-env ).
*                   Iteration
                        lo_test = lo_head->cdr->car.
                        DATA(lo_command) = lo_head->cdr->cdr.

                        DO.
*                     evaluate <test>;
                          CASE eval_ast( VALUE #( BASE cont
                                                  elem = lo_test->car ) ).
                            WHEN false.
                              eval_do_step( io_command = lo_command
                                            io_steps = lo_steps
                                            io_env = cont-env ).
*                         and the next iteration begins.

                            WHEN OTHERS. " <test> evaluates to a true value
* <expression>s are evaluated from left to right and the values of the last <expression> are returned.
* If no <expression>s are present, then the value of the do expression is unspecified.

                              cont-elem = lo_test->cdr.
                              result = nil.
                              EXIT.
                          ENDCASE.

                        ENDDO.

                        IF cont-elem EQ nil.
                          result = nil.
                        ELSE.
                          _tail_sequence.
                        ENDIF.

                      WHEN 'case'.
* (case <key> <clause1> <clause2> <clause3> ... )
* <key> can be any expression. Each <clause> has the form ((<datum1> ...) <expression1> <expression2> ...)
* It is an error if any of the <datum> are the same anywhere in the expression
* Alternatively, a <clause> can be of the form  ((<datum1> ...) => <expression1> )
* The last <clause> can be an "else clause" which has one of the forms
*  (else <expression1> <expression2> ... )
*  or
*  (else => <expression>).
                        _validate lr_tail.
                        result = nil.

                        DATA(lo_key) = eval( VALUE #( BASE cont
                                                      elem = lr_tail->car ) ).
                        lr_tail = lr_tail->cdr.
                        _validate: lr_tail, lr_tail->car, lo_key.

                        IF lr_tail EQ nil.
                          throw( `case: no clause` ).
                        ENDIF.

                        cont-elem = nil.
                        DATA(lv_match) = abap_false.
                        WHILE lr_tail->type EQ type_pair AND lv_match EQ abap_false.
                          lo_clause = lr_tail->car.

                          DATA(lo_datum) = lo_clause->car.
                          _validate lo_datum.

                          WHILE lo_datum NE nil.

                            IF lo_datum->value EQ c_lisp_else.
                              IF lr_tail->cdr NE nil.
                                throw( `case: else must be the last clause` ).
                              ENDIF.
                              cont-elem = lo_clause->cdr.
                              lv_match = abap_true.
                              EXIT.
                            ENDIF.

                            " eqv? match
                            IF lo_key->is_equivalent( lo_datum->car ).
                              cont-elem = lo_clause->cdr.
                              lv_match = abap_true.
                              EXIT.
                            ENDIF.

                            lo_datum = lo_datum->cdr.
                          ENDWHILE.

                          lr_tail = lr_tail->cdr.
                        ENDWHILE.

                        IF cont-elem EQ nil.
                          result = nil.

                        ELSEIF cont-elem->car->value = c_lisp_then.

                          cont-elem = lcl_lisp_new=>cons(
                            io_car = cont-elem->cdr->car
                            io_cdr = lcl_lisp_new=>box_quote( lo_key ) ).
                          CONTINUE.
                          "tail_expression lo_elem.

                        ELSE.

                          _tail_sequence.

                        ENDIF.

                      WHEN 'for-each'.
                        result = expand_for_each( io_list = lr_tail
                                                  environment = cont-env ).

                      WHEN 'map'.
                        result = expand_map( io_list = lr_tail
                                             environment = cont-env ).

                      WHEN 'apply'.
                        " (apply proc arg1 ... argn rest-args)
                        cont-elem = lcl_lisp_new=>cons( io_car = expand_apply( io_list = lr_tail
                                                                               environment = cont-env ) ).
                        _tail_expression cont-elem.

                      WHEN 'macroexpand'.
*                   for debugging
                        _validate lr_tail->car.
                        result = syntax_expand( VALUE #( BASE cont
                                                         elem = lr_tail->car ) ).

                      WHEN 'gensym'.
                        result = generate_symbol( lr_tail ).

                      WHEN c_eval_unquote
                        OR c_eval_unquote_splicing.
                        throw( |{ lr_head->value } not valid outside of quasiquote| ).

                      WHEN 'guard'.
                        throw( |guard not implemented yet| ).

                      WHEN 'delay'.
                        " (delay <expression>) lazy library syntax
                        " returns an object called a promise which at some point in the future can be asked
                        " (by the force procedure) to evaluate <expression>, and deliver the resulting value.
                        " The delay construct is used together with the procedure force to implement lazy evaluation or call by need.
                        " The effect of <expression> returning multiple values is unspecified.
                        "result = bind_symbol( element = lr_tail
                        "                      environment = cont-env
                        "                      iv_category = tv_category_macro ).
                        result = lcl_lisp_new=>lambda( io_car = lcl_lisp=>nil        " List of parameters
                                                       io_cdr = lr_tail->car         " Body
                                                       io_env = cont-env ).
*(define-macro (delay exp)
*    (define (make-promise proc)
*      (let ((result-ready? #f)
*        (result #f))
*    (lambda ()
*      (if result-ready?
*          result
*          (let ((x (proc)))
*        (if result-ready?
*            result
*            (begin (set! result-ready? #t)
*               (set! result x)
*               result)))))))
*    `(,make-promise (lambda () ,exp)))

                        "throw( |delay not implemented yet| ).

                      WHEN 'delay-force'.
                        cont-elem = lr_tail.
                        "throw( |delay-force not implemented yet| ).
                        _tail_expression cont-elem.

                      WHEN 'call-with-values'.
                        " we are trying to: (define (call-with-values producer consumer) (apply consumer (producer)))
                        _validate: lr_tail,
                                   lr_tail->car.      " producer
                        " producer
                        DATA(lo_values) = eval_ast( VALUE #( BASE cont
                                                             elem = lr_tail->car ) ).
                        " extract the consumer
                        _validate: lr_tail->cdr,
                                   lr_tail->cdr->car. " consumer
                        DATA(lo_consumer) = lr_tail->cdr->car.

                        lr_tail = lo_values.

                        cont-elem = lcl_lisp_new=>cons( io_car = expand_apply( io_list = lo_consumer
                                                                               environment = cont-env ) ).
                        throw( `call-with-values not implemented yet` ).
                        _tail_expression cont-elem.

                      WHEN 'dynamic-wind'.
                        _validate: lr_tail, lr_tail->car,
                                   lr_tail->cdr, lr_tail->cdr->car,
                                   lr_tail->cdr->cdr, lr_tail->cdr->cdr->car.
                        throw( `dynamic-wind not not implemented yet` ).
                        DATA(before) = lr_tail->car.
                        DATA(thunk) = lr_tail->cdr->car.
                        DATA(after) = lr_tail->cdr->cdr->car.

                        cont-elem = thunk.
                        _tail_expression cont-elem.

*                      WHEN 'define-library'.
                      WHEN 'export'
                        OR 'import'
                        OR 'cond-expand'.
                        result = nil.

*                      WHEN 'cond-expand'.
*                      WHEN 'import'.
*                      WHEN 'rename'.
*                      WHEN 'only'.
*                      WHEN 'include-library-declarations'.
                      WHEN 'library'.
                        " not implemented: for now, try to skip library code
                        IF lr_tail IS BOUND.
                          cont-elem = lr_tail->cdr.
                          IF cont-elem NE nil.
                            result = eval_list_tco( CHANGING cs_cont = cont ).
                            _tail_expression cont-elem.
                          ELSE.  " empty (library) ?
                            CONTINUE.
                          ENDIF.
                        ENDIF.

                      WHEN 'call/cc' OR 'call-with-current-continuation'.
                        DATA lo_proc TYPE REF TO lcl_lisp.

                        _validate: lr_tail, lr_tail->car, lr_tail->car->cdr.

                        " extract the procedure to be called
                        lo_head = lr_tail->car->cdr.
                        lo_head->car->assert_last_param( ).       "  (A single parameter)

                        lo_proc = lcl_lisp_new=>lambda( io_car = lo_head->car         " Params
                                                        io_cdr = lo_head->cdr         " Body
                                                        io_env = cont-env ).
                        " capture the current execution state
                        DATA(lo_escape_proc) = lcl_lisp_new=>escape( value = cont
                                                                     param = lo_head->car ).

                        cont-env = lambda_environment( io_head = lo_proc
                                                       io_args = lo_escape_proc
                                                       environment = cont-env ).
                        cont-elem = lo_proc->cdr.

                        _tail_sequence.
                        "cont-elem = eval( cont ).

                      WHEN OTHERS.

*                   EXECUTE PROCEDURE (native or lambda)
*                   Take the first item of the evaluated list and call it as function
*                   using the rest of the evaluated list as its arguments.

*                   The evaluated head must be a native procedure or a lambda or an ABAP function module
                        lo_proc = eval_ast( VALUE #( BASE cont
                                                          elem = lr_head ) ).
                        IF gv_lisp_trace EQ abap_true.
                          go_out->write( |call { lo_proc->value } { lo_proc->to_string( ) } param { lr_tail->to_string( ) }| ).
                        ENDIF.

                        CASE lo_proc->type.

                          WHEN type_lambda.
                            cont-env = lambda_environment( io_head = lo_proc
                                                           io_args = lr_tail
                                                           environment = cont-env ).
                            cont-elem = lo_proc->cdr.
                            _tail_sequence.

                          WHEN type_native.
*                       Evaluate native function:
                            CALL METHOD (lo_proc->value)
                              EXPORTING
                                list   = evaluate_parameters( io_list = lr_tail
                                                              environment = cont-env )
                              RECEIVING
                                result = result.

                          WHEN type_primitive OR type_syntax.
                            cont-elem = lcl_lisp_new=>cons( io_car = lo_proc
                                                            io_cdr = lr_tail ).
                            CONTINUE. "tail_expression lo_elem.

                          WHEN type_abap_function.
*              >> TEST: Support evaluation of ABAP function directly
*  Recompose as if calling a PROC (which we are). This is part of the test. If we make an ABAP function call
*  first-class, then we would need to revisit evaluating the whole of ELEMENT in one shot
                            result = proc_abap_function_call( lcl_lisp_new=>cons( io_car = lo_proc
                                                                                  io_cdr = lr_tail ) ).
*              << TEST
*                      WHEN type_abap_method.
*              >> TEST: Support evaluation of ABAP methods directly
*              << TEST

                          WHEN type_case_lambda.
                            DATA lo_case TYPE REF TO lcl_lisp_case_lambda.
                            lo_case ?= lo_proc.
                            lo_proc = lo_case->match( lr_tail ).
                            cont-env = lambda_environment( io_head = lo_proc
                                                           io_args = lr_tail
                                                           environment = cont-env ).
                            cont-elem = lo_proc->cdr.
                            _tail_sequence.

                          WHEN type_escape_proc.
                            cont = CAST lcl_lisp_escape( lo_proc )->escape( cont ).

                            _tail_sequence.

                          WHEN OTHERS.
                            throw( |attempt to apply { lo_proc->to_string( ) } - not a procedure| ).

                        ENDCASE.

                    ENDCASE.

                  WHEN type_values.
                    cont-elem = CAST lcl_lisp_values( cont-elem->car )->head.

                    _tail_sequence.

                  WHEN OTHERS.
                    result = eval_ast( cont ).

                ENDCASE.

            ENDCASE.

          CATCH lcx_lisp_escape INTO DATA(lx_esc).
            " reinstall the passed continuation as the continuation for the currently
            " executing procedure overriding the previous implicit continuation.
            cont = lx_esc->new_continuation( cont ).

            _tail_sequence.
        ENDTRY.

*       Circular references
        result->set_shared_structure( ).
        IF gv_lisp_trace EQ abap_true.
          go_out->write( |=> { result->to_string( ) }| ).
        ENDIF.

        RETURN.

      ENDDO.


    ENDMETHOD.

    DEFINE _optional_port_template.
      TRY.
          IF &1->type EQ type_pair.
            _validate_port &1->car &3.
            li_port ?= &1->car.
          ELSE.
            li_port ?= proc_current_&2_port( nil ).
          ENDIF.
      CATCH cx_root INTO DATA(lx_error).
        throw( lx_error->get_text( ) ).
      ENDTRY.
    END-OF-DEFINITION.

    DEFINE _optional_binary_port.
      DATA li_port TYPE REF TO lif_binary_&2_port.

      _optional_port_template &1 &2 &3.
    END-OF-DEFINITION.

    DEFINE _optional_port.
      DATA li_port TYPE REF TO lif_&2_port.

      _optional_port_template &1 &2 &3.
    END-OF-DEFINITION.

    DEFINE _optional_port_arg.
      _optional_port io_arg &1 &2.
    END-OF-DEFINITION.

    METHOD write.
      _optional_port_arg output `write`.
      li_port->write( io_elem ).
      result = io_elem.
    ENDMETHOD.

    METHOD write_u8.
      _optional_binary_port io_arg output `write-u8`.
      _validate_char io_elem `write-u8`.
      li_port->write_u8( CAST lcl_lisp_char( io_elem ) ).
      result = io_elem.
    ENDMETHOD.

    METHOD display.
      _optional_port_arg output `display`.
      li_port->display( io_elem ).
      result = io_elem.
    ENDMETHOD.

    METHOD read.
      DATA li_port TYPE REF TO lif_input_port.

      TRY.
          IF io_arg->type EQ type_pair.
            _validate_port io_arg->car `read`.
            li_port ?= io_arg->car.
          ELSE.
            li_port ?= proc_current_input_port( nil ).
          ENDIF.

          result = read_from( NEW lcl_input_port_to_stream( li_port ) ).
        CATCH cx_root INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD read_char.
      _optional_port_arg input `read-char`.

      result = lcl_lisp_new=>char( li_port->read_char( ) ).
    ENDMETHOD.

    METHOD read_u8.
      _optional_binary_port io_arg input `read-u8`.

      result = lcl_lisp_new=>char( li_port->read_u8( ) ).
    ENDMETHOD.

    METHOD read_line.
      DATA lv_input TYPE string.
      DATA lv_char TYPE tv_char.
      DATA li_port TYPE REF TO lif_input_port.

      _validate io_arg.

      IF io_arg->cdr->type EQ type_pair.
        _validate_port io_arg->cdr->car `read-line`.
        li_port ?= io_arg->cdr->car.
      ELSE.
        li_port ?= proc_current_input_port( nil ).
      ENDIF.

      IF NOT li_port->is_char_ready( ).
        result = lcl_lisp_new=>eof( ).
      ELSE.
        WHILE li_port->is_char_ready( ) AND li_port->peek_char( ) CN lcl_stream=>gv_line_ending.
          lv_char = li_port->read_char( ).
          CONCATENATE lv_input lv_char INTO lv_input RESPECTING BLANKS.
        ENDWHILE.
        " Skip end-of-line sequence
        WHILE li_port->is_char_ready( ) AND li_port->peek_char( ) CA lcl_stream=>gv_line_ending.
          li_port->read_char( ).
        ENDWHILE.

        result = lcl_lisp_new=>string( lv_input ).
      ENDIF.
    ENDMETHOD.

    METHOD read_string.
      DATA k TYPE tv_index.
      DATA lv_input TYPE string.
      DATA lv_char TYPE tv_char.
      DATA li_port TYPE REF TO lif_input_port.

      _validate io_arg.
      _validate_index io_arg->car `read-string`.
      k = CAST lcl_lisp_integer( io_arg->car )->int.

      IF io_arg->cdr->type EQ type_pair.
        _validate_port io_arg->cdr->car `read-string`.
        li_port ?= io_arg->cdr->car.
      ELSE.
        li_port ?= proc_current_input_port( nil ).
      ENDIF.

      DO k TIMES.
        IF li_port->is_char_ready( ).
          lv_char = li_port->read_char( ).
          CONCATENATE lv_input lv_char INTO lv_input RESPECTING BLANKS.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
      result = lcl_lisp_new=>string( lv_input ).
    ENDMETHOD.

    METHOD read_bytevector.
      DATA k TYPE tv_index.
      DATA lt_byte TYPE tt_byte.
      DATA lv_byte TYPE tv_byte.
      DATA li_port TYPE REF TO lif_binary_input_port.

      _validate io_arg.
      _validate_index io_arg->car `read-bytevector`.
      k = CAST lcl_lisp_integer( io_arg->car )->int.

      IF io_arg->cdr->type EQ type_pair.
        _validate_port io_arg->cdr->car `read-bytevector`.
        li_port ?= io_arg->cdr->car.
      ELSE.
        li_port ?= proc_current_input_port( nil ).
      ENDIF.

      DO k TIMES.
        IF li_port->is_u8_ready( ).
          lv_byte = li_port->read_u8( ).
          APPEND lv_byte TO lt_byte.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
      result = lcl_lisp_new=>bytevector( it_byte = lt_byte
                                         iv_mutable = abap_true ).
    ENDMETHOD.

    METHOD eval_source.
      TRY.
          response = eval_repl( code ).
        CATCH cx_root INTO DATA(lx_root).
          response = lx_root->get_text( ).
      ENDTRY.
    ENDMETHOD.                    "eval_source

    METHOD eval_repl.
      " Entry point for parsing code. This is not thread-safe, but as an ABAP
      " process does not have the concept of threads, we are safe :-)
      DATA ls_cont TYPE ts_continuation.
      DATA lo_result TYPE REF TO lcl_lisp.
      FIELD-SYMBOLS <ls_cont> TYPE ts_continuation.

      DATA(lt_elem) = parse( code ).

      " Parse and convert to linked list of continuation records
*        ls_cont = to_linked_list( elements = lt_elem
*                                  io_env = env ).
      DATA lr_cont TYPE tr_continuation.
      DATA lr_next TYPE tr_continuation.

      " Convert to linked list
      LOOP AT lt_elem INTO DATA(lo_element).
        lr_next = NEW ts_continuation( elem = lo_element
                                       env = env ).
        IF ls_cont IS INITIAL.
          ls_cont-next = lr_cont = lr_next.
        ELSE.
          lr_cont->next = lr_next.
          lr_cont = lr_next.
        ENDIF.
      ENDLOOP.

      lo_result = lcl_lisp_new=>eof( ).

      WHILE ls_cont-next IS BOUND.
        ASSIGN ls_cont-next->* TO <ls_cont>.
        ls_cont = <ls_cont>.

        lo_result = eval( ls_cont ).

        gi_log->put( lo_result ).
      ENDWHILE.

      output = lo_result->to_text( ).
      response = gi_log->get( ).
    ENDMETHOD.

    METHOD validate_source.
      TRY.
          LOOP AT parse( code ) INTO DATA(lo_element).
            gi_log->put( lo_element ).
          ENDLOOP.
          response = gi_log->get( ).
        CATCH cx_root INTO DATA(lx_root).
          response = lx_root->get_text( ).
      ENDTRY.
    ENDMETHOD.

**********************************************************************
* NATIVE PROCEDURES
**********************************************************************
    METHOD proc_append.
*     Creates a new list appending all parameters
*     All parameters except the last must be lists, the last must be a cons cell.
      _validate list.

*     No arguments: return nil
      result = list.
      CHECK list NE nil.

*     One argument: return argument
      result = list->car.

      CHECK list->cdr NE nil.

      DATA(lo_iter) = list->new_iterator( ).
      WHILE lo_iter->has_next( ).

*       copy first list, reassign result
        DATA(first) = lo_iter->next( ).
        CHECK first NE nil.

        IF first->type = type_pair.
          result = lcl_lisp_new=>cons( io_car = first->car ).

*         ##TO DO: Test for circular list! ------------------------------
          DATA(lo_last) = result.
          DATA(lo_arg) = first->cdr.
          WHILE lo_arg->type = type_pair.
            lo_last = lo_last->cdr = lcl_lisp_new=>cons( io_car = lo_arg->car ).
            lo_arg = lo_arg->cdr.
          ENDWHILE.

          IF lo_arg NE nil.
            lo_last = lo_last->cdr = lo_arg.
          ENDIF.

        ELSE.

          lo_arg = result = first.
        ENDIF.

        EXIT.
      ENDWHILE.

*     Append next list
      WHILE lo_iter->has_next( ).

        IF lo_arg NE nil.  " _validate_tail lo_arg first `append`.
*           if the last element in the list is not a cons cell, we cannot append
          first->error_not_a_list( `append` ).
        ENDIF.

        first = lo_arg = lo_iter->next( ).
        CHECK first NE nil.

*       ##TO DO: Check for circular list
*       Append lo_arg to result, from last element on
        WHILE lo_arg->type = type_pair.
          lo_last = lo_last->cdr = lcl_lisp_new=>cons( io_car = lo_arg->car ).
          lo_arg = lo_arg->cdr.
        ENDWHILE.

        CHECK lo_arg NE nil.
        lo_last = lo_last->cdr = lo_arg.

      ENDWHILE.

    ENDMETHOD.

    METHOD list_reverse.
      _validate io_list.

      result = nil.
      DATA(lo_ptr) = io_list.

*     ##TO DO: check if circular lists are allowed
      WHILE lo_ptr->type EQ type_pair.
        result = lcl_lisp_new=>cons( io_car = lo_ptr->car
                                     io_cdr = result ).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
    ENDMETHOD.

    METHOD proc_reverse.
      _validate list.

      result = list_reverse( list->car ).
    ENDMETHOD.                    "proc_reverse

    METHOD proc_values.
      _validate list.

      result = lcl_lisp_new=>values( list ).

*      result = lcl_lisp_new=>values( evaluate_parameters( io_list = lr_tail
*                                                          environment = environment ) ).
    ENDMETHOD.

    METHOD table_of_lists.
      _validate io_head.

      CLEAR rt_table.
      CHECK io_head NE nil.

*     build internal table of list interators
      DATA(iter) = io_head->new_iterator( ).
      WHILE iter->has_next( ).
*       Evaluate next list entry
        DATA(ls_cont) = VALUE ts_continuation( elem = iter->next( )
                                               env = environment ).
        DATA(lo_next) = eval( ls_cont ).
        IF lo_next = nil.  " if any list is empty, the table is empty
          CLEAR rt_table.
          RETURN.
        ENDIF.
        APPEND lo_next TO rt_table.
      ENDWHILE.
    ENDMETHOD.

    METHOD map_next_expr.
*     determine expression (proc list1[k] list2[k]... listn[k])
      ev_has_next = abap_true.

      DATA(lo_next) = lcl_lisp_new=>cons( io_car = io_proc ).
      result = lo_next.
      LOOP AT ct_list ASSIGNING FIELD-SYMBOL(<lo_list>).
        IF <lo_list> EQ nil.
          ev_has_next = abap_false.
          result = nil.
          RETURN.
        ELSE.
*         Parameters are already evaluated, use special form to avoid repeated evaluation
          lo_next = lo_next->cdr = lcl_lisp_new=>box_quote( <lo_list>->car ).
          <lo_list> = <lo_list>->cdr.
        ENDIF.
        CHECK <lo_list> EQ nil.
        ev_has_next = abap_false.
      ENDLOOP.
    ENDMETHOD.

    METHOD proc_append_unsafe.  " append! (non functional)
*     Takes two parameters: the first must be a list, and the second can
*     be of any type. Appends the second param to the first.

*     But if the last element in the list is not a cons cell, we cannot append
      _validate: list, list->car, list->cdr.

      IF list->car EQ nil.
        result = list->cdr->car.
      ELSE.
*       Get to last element in list - this can make APPEND expensive, like LENGTH
        DATA(lo_last) = list->car.
        IF lo_last->type NE type_pair.
          list->error_not_a_list( `append!` ).
        ENDIF.

        WHILE lo_last->cdr IS BOUND AND lo_last->cdr NE nil.
          lo_last = lo_last->cdr.
        ENDWHILE.

        "##TO DO - replace with _validate_tail lo_last (?) list->car.
        IF lo_last->type NE type_pair.
*         If the last item is not a cons cell, return an error
          list->car->error_not_a_list( `append!` ).
        ENDIF.

*       Last item is a cons cell; tack on the new value
        lo_last->cdr = list->cdr->car.
        result = list->car.
      ENDIF.
    ENDMETHOD.                    "proc_append_unsafe

    DEFINE _validate_pair.
      IF &1->type NE type_pair.
        &1->raise( context = &2
                   message = ` is not a pair` ).
      ENDIF.
    END-OF-DEFINITION.

    METHOD proc_car.
      _validate: list, list->car.

      DATA(lo_arg) = list->car.
      _validate_pair lo_arg `car: `.
      result = lo_arg->car.
    ENDMETHOD.                    "proc_car

    METHOD proc_set_car.
      _validate: list, list->cdr.

      DATA(lo_arg) = list->car.
      _validate_pair lo_arg `set-car!: `.
      _validate_mutable lo_arg `list in set-car!`.

      lo_arg->car = list->cdr->car.
      result = nil.
    ENDMETHOD.                    "proc_car

    METHOD proc_set_cdr.
      _validate: list, list->cdr.

      DATA(lo_arg) = list->car.
      _validate_pair lo_arg `set-cdr!: `.
      _validate_mutable lo_arg `list in set-cdr!`.

      lo_arg->cdr = list->cdr->car.
      result = nil.
    ENDMETHOD.

    METHOD proc_cdr.
      _validate list.

      DATA(lo_arg) = list->car.
      _validate_pair lo_arg `cdr: `.
      result = lo_arg->cdr.
    ENDMETHOD.                    "proc_cdr

    METHOD proc_cons.
*     Create new cell and prepend it to second parameter
      _validate: list, list->car, list->cdr.

      list->cdr->assert_last_param(  ).

      result = lcl_lisp_new=>cons( io_car = list->car
                                   io_cdr = list->cdr->car ).
    ENDMETHOD.                    "proc_cons

    METHOD proc_not.
*     Create new cell and prepend it to second parameter
      _validate list.

      IF list->car EQ false.
        result = true.
      ELSE.
        result = false.
      ENDIF.
    ENDMETHOD.                    "proc_cons

    DEFINE _validate_cxxr.
      _validate list.
      _validate_pair list &1.

      DATA(lo_arg) = list->car.
      _validate_pair lo_arg &1.
    END-OF-DEFINITION.

    DEFINE _execute_cxxr.
      _validate_cxxr &1.

      lo_arg = lo_arg->&2.
      _validate_pair lo_arg &1.

      result = lo_arg->&3.
    END-OF-DEFINITION.

    METHOD proc_caar.
      _execute_cxxr `caar: ` car car.
    ENDMETHOD.                    "proc_caar

    METHOD proc_cadr.
      _execute_cxxr `cadr: ` cdr car.
    ENDMETHOD.                    "proc_cadr

    METHOD proc_cdar.
      _execute_cxxr `cdar: ` car cdr.
    ENDMETHOD.                    "proc_cdar

    METHOD proc_cddr.
      _execute_cxxr `cddr: ` cdr cdr.
    ENDMETHOD.                    "proc_cddr

    DEFINE _execute_cx3r.
      _validate_cxxr &1.

      lo_arg = lo_arg->&2.
      _validate_pair lo_arg &1.

      lo_arg = lo_arg->&3.
      _validate_pair lo_arg &1.

      result = lo_arg->&4.
    END-OF-DEFINITION.

    METHOD proc_caaar.
      _execute_cx3r `caaar: ` car car car.
    ENDMETHOD.

    METHOD proc_cdaar.
      _execute_cx3r `cdaar: ` car car cdr.
    ENDMETHOD.

    METHOD proc_caadr.
      _execute_cx3r `caadr: ` cdr car car.
    ENDMETHOD.

    METHOD proc_cdadr.
      _execute_cx3r `cdadr: ` cdr car cdr.
    ENDMETHOD.

    METHOD proc_cadar.
      _execute_cx3r `cadar: ` car cdr car.
    ENDMETHOD.

    METHOD proc_cddar.
      _execute_cx3r `cddar: ` car cdr cdr.
    ENDMETHOD.

    METHOD proc_caddr.
      _execute_cx3r `caddr: ` cdr cdr car.
    ENDMETHOD.

    METHOD proc_cdddr.
      _execute_cx3r `cdddr: ` cdr cdr cdr.
    ENDMETHOD.

    DEFINE _execute_cx4r.
      _validate_cxxr &1.

      lo_arg = lo_arg->&2.
      _validate_pair lo_arg &1.

      lo_arg = lo_arg->&3.
      _validate_pair lo_arg &1.

      lo_arg = lo_arg->&4.
      _validate_pair lo_arg &1.

      result = lo_arg->&5.
    END-OF-DEFINITION.

    METHOD proc_caaaar.
      _execute_cx4r `caaaar: ` car car car car.
    ENDMETHOD.

    METHOD proc_cdaaar.
      _execute_cx4r `cdaaar: ` car car car cdr.
    ENDMETHOD.

    METHOD proc_cadaar.
      _execute_cx4r `cadaar: ` car car cdr car.
    ENDMETHOD.

    METHOD proc_cddaar.
      _execute_cx4r `cddaar: ` car car cdr cdr.
    ENDMETHOD.

    METHOD proc_caaadr.
      _execute_cx4r `caaadr: ` cdr car car car.
    ENDMETHOD.

    METHOD proc_cdaadr.
      _execute_cx4r `cdaadr: ` cdr car car cdr.
    ENDMETHOD.

    METHOD proc_cadadr.
      _execute_cx4r `cadadr: ` cdr car cdr car.
    ENDMETHOD.

    METHOD proc_cddadr.
      _execute_cx4r `cddadr: ` cdr car cdr cdr.
    ENDMETHOD.

    METHOD proc_caadar.
      _execute_cx4r `caadar: ` car cdr car car.
    ENDMETHOD.

    METHOD proc_cdadar.
      _execute_cx4r `cdadar: ` car cdr car cdr.
    ENDMETHOD.

    METHOD proc_caddar.
      _execute_cx4r `caddar: ` car cdr cdr car.
    ENDMETHOD.

    METHOD proc_cdddar.
      _execute_cx4r `cdddar: ` car cdr cdr cdr.
    ENDMETHOD.

    METHOD proc_caaddr.
      _execute_cx4r `caaddr: ` cdr cdr car car.
    ENDMETHOD.

    METHOD proc_cdaddr.
      _execute_cx4r `cdaddr: ` cdr cdr car cdr.
    ENDMETHOD.

    METHOD proc_cadddr.
      _execute_cx4r `cadddr: ` cdr cdr cdr car.
    ENDMETHOD.

    METHOD proc_cddddr.
      _execute_cx4r `cddddr: ` cdr cdr cdr cdr.
    ENDMETHOD.

    METHOD list_length.
      DATA lo_fast TYPE REF TO lcl_lisp.
      DATA lo_slow TYPE REF TO lcl_lisp.
      _validate list.

      result = 0.
      lo_slow = lo_fast = list.
*     Iterate over list
      WHILE lo_fast->type EQ type_pair.
        ADD 1 TO result.  " count the number of items
        lo_fast = lo_fast->cdr.
        lo_slow = lo_slow->cdr.

        CHECK lo_fast->type EQ type_pair.
        ADD 1 TO result.  " count the number of items (using the fast pointer)
        lo_fast = lo_fast->cdr.

        CHECK lo_fast = lo_slow.           " are we stuck in a circular list?
*       If we are stuck in a circular list, then the fast pointer will eventually
*       be equal to the slow pointer. That fact justifies this implementation.
        RETURN.
      ENDWHILE.
      CHECK lo_fast NE nil.
*     If the last item is not a cons cell, return an error
      list->error_not_a_list( `list-length` ).
    ENDMETHOD.

    METHOD proc_length.
      _validate: list, list->car, list->cdr.
      IF list->cdr NE nil.
        throw( |length takes only one argument| ).
      ENDIF.

      result = lcl_lisp_new=>integer( list_length( list->car ) ).
    ENDMETHOD.                    "proc_length

    METHOD proc_list_copy.
      DATA lo_slow TYPE REF TO lcl_lisp.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      DATA lo_new TYPE REF TO lcl_lisp.

      _validate: list, list->cdr.
      IF list->cdr NE nil.
        throw( |list-copy takes only one argument| ).
      ENDIF.

      _validate list->car.
      result = list->car.
      CHECK result->type EQ type_pair.

      lo_slow = lo_ptr = result->cdr.
      result = lo_new = lcl_lisp_new=>cons( io_car = result->car ).

*     Iterate over list to count the number of items
      WHILE lo_ptr->type EQ type_pair.
        lo_new = lo_new->cdr = lcl_lisp_new=>cons( io_car = lo_ptr->car ).
        lo_ptr = lo_ptr->cdr.
        lo_slow = lo_slow->cdr.
        CHECK lo_ptr->type EQ type_pair.
        lo_new = lo_new->cdr = lcl_lisp_new=>cons( io_car = lo_ptr->car ).
        lo_ptr = lo_ptr->cdr.
        CHECK lo_ptr = lo_slow.
        throw( |list-copy: circular list| ).
      ENDWHILE.
      lo_new->cdr = lo_ptr.
    ENDMETHOD.                    "proc_length

    METHOD proc_list.
*     The items given to us are already in a list and evaluated; we just need to return the head
      result = list.
    ENDMETHOD.                    "proc_list

    METHOD proc_nilp.
      _validate: list, list->car.
      result = COND #( WHEN list->car = nil THEN true ELSE false ).
    ENDMETHOD.                    "proc_nilp

    METHOD proc_make_list.
      DATA k TYPE tv_int.
*     returns a list of length k and every atom is the default fill value supplied or the empty list
      _validate list.
      _validate_index list->car 'make-list'.

      result = lcl_lisp=>nil.

      k = CAST lcl_lisp_integer( list->car )->int.
      CHECK k GT 0.

      IF list->cdr EQ nil.
        DATA(lo_default) = nil.
      ELSE.
        lo_default = list->cdr->car.
      ENDIF.

      result = lcl_lisp_new=>cons( io_car = lo_default ).  " first
      DATA(lo_ptr) = result.

      DO k - 1 TIMES.
        lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = lo_default ).
      ENDDO.
    ENDMETHOD.

    METHOD proc_list_tail.
      _validate: list, list->cdr.
      _validate_index list->cdr->car 'list-tail'.

      result = list_tail( list = list->car
                          k  = CAST lcl_lisp_integer( list->cdr->car )->int
                          area = 'list-tail' ).
    ENDMETHOD.

    METHOD list_tail.
*     (list-tail list k) procedure
*     List should be a list of size at least k.  The list-tail procedure returns the subchain
*     of list obtained by omitting the first k elements:  (list-tail '(a b c d) 2)  => (c d)
*
*     we must check that list is a chain of pairs whose length is at least k.
*     we should not check that it is a list of pairs beyond this length.
      _validate list.
      result = list.

      IF k EQ 0 AND list EQ nil.
        RETURN.
      ENDIF.
      _validate list->cdr.

      DO k TIMES.
        IF result->type NE type_pair.
          throw( area && `: list too short` ).
        ENDIF.
        result = result->cdr.
        CHECK result IS NOT BOUND.
        throw( area && |: an entry before index { k } is not a pair| ).
      ENDDO.
    ENDMETHOD.

    METHOD proc_iota.
      DATA lv_count TYPE tv_int.
      DATA lv_start TYPE tv_int VALUE 0.
      DATA lv_step TYPE tv_int VALUE 1.
      DATA lo_ptr TYPE REF TO lcl_lisp.

      _validate list.

      DATA(lo_count) = list->car.
      _validate_index lo_count 'iota count'.
      _to_integer lo_count lv_count.

      result = nil.
      CHECK lv_count GT 0.

      _validate list->cdr.
      IF list->cdr NE nil.
        DATA(lo_start) = list->cdr->car.
        _validate_integer lo_start 'iota start'.
        _to_integer lo_start lv_start.

        _validate list->cdr->cdr.
        IF list->cdr->cdr NE nil.
          DATA(lo_step) = list->cdr->cdr->car.
          _validate_integer lo_step 'iota step'.
          _to_integer lo_step lv_step.
        ENDIF.
      ENDIF.

      result = lo_ptr = lcl_lisp_new=>cons( io_car = lcl_lisp_new=>integer( lv_start ) ).

      DO lv_count - 1 TIMES.
        ADD lv_step TO lv_start.
        lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = lcl_lisp_new=>integer( lv_start ) ).
      ENDDO.
    ENDMETHOD.

*(car list-tail list k)
    METHOD proc_list_ref.
*    (list-ref list k) procedure
*    List must be a list whose length is at least k + 1.  The list-ref procedure returns the kth element of list.
*    (list-ref '(a b c d) 2) => c
*
*    The implementation must check that list is a chain of pairs whose length is at least k + 1.
*    It should not check that it is a list of pairs beyond this length.

      _validate: list, list->cdr.
      _validate_index list->cdr->car 'list-ref'.

      result = list_tail( list = list->car
                          k = CAST lcl_lisp_integer( list->cdr->car )->int
                          area = 'list-ref' ).
      result = result->car.
    ENDMETHOD.

    METHOD proc_make_vector.
      DATA lo_fill TYPE REF TO lcl_lisp.
      _validate: list, list->cdr.

      DATA(lo_size) = list->car.

      _validate_index lo_size `make-vector`.

      IF list->cdr NE lcl_lisp=>nil.
        lo_fill = list->cdr->car.
      ELSE.
        lo_fill = lcl_lisp=>nil.
      ENDIF.

      result = lcl_lisp_vector=>init( size = CAST lcl_lisp_integer( lo_size )->int
                                      io_fill = lo_fill ).
    ENDMETHOD.

    METHOD proc_vector.
*     The items given to us are already in a list and evaluated; we just need to return the head
      result = lcl_lisp_vector=>from_list( list ).
    ENDMETHOD.

    METHOD proc_vector_length.
      _validate list.
      _validate_vector list->car 'vector-length'.

      result = CAST lcl_lisp_vector( list->car )->length( ).
    ENDMETHOD.

    METHOD proc_vector_ref.
*    (vector-ref vector k) procedure
      _validate list.
      _validate_vector list->car 'vector-ref'.
      DATA(lo_vec) = CAST lcl_lisp_vector( list->car ).

      _validate list->cdr.
      DATA(lo_idx) = list->cdr->car.
      _validate_index lo_idx 'vector-ref'.

      result = lo_vec->get( CAST lcl_lisp_integer( lo_idx )->int ).

    ENDMETHOD.

    METHOD proc_vector_set.
*    (vector-set! vector k obj) procedure
      _validate list.
      _validate_vector list->car 'vector-set!'.
      DATA(lo_vec) = CAST lcl_lisp_vector( list->car ).

      _validate: list->cdr.
      DATA(lo_idx) = list->cdr->car.
      _validate_index lo_idx 'vector-set!'.

      _validate: list->cdr->cdr.

      DATA(lo_obj) = list->cdr->cdr.
      IF lo_obj NE nil.
        lo_obj = lo_obj->car.
      ENDIF.

      lo_vec->set( index = CAST lcl_lisp_integer( lo_idx )->int
                   io_elem = lo_obj ).
*     Result is undefined, but must be valid
      result = lo_obj.
    ENDMETHOD.

    METHOD proc_vector_fill.
* (vector-fill! vector fill) procedure
* (vector-fill! vector fill start) procedure
* (vector-fill! vector fill start end) procedure
*The vector-fill! procedure stores fill in the elements of vector between start and end.
      _validate list.
      DATA(lo_ptr) = list.

      _validate_vector lo_ptr->car 'vector-fill!'.
      DATA(lo_vec) = CAST lcl_lisp_vector( lo_ptr->car ).

      lo_ptr = lo_ptr->cdr.
      _validate: lo_ptr, lo_ptr->car.
      DATA(lo_fill) = lo_ptr->car.

      lo_ptr = lo_ptr->cdr.
      _validate lo_ptr.
      IF lo_ptr NE nil.
        _validate_index lo_ptr->car 'vector-fill! start'.
        DATA lv_start TYPE tv_int.
        _to_integer lo_ptr->car lv_start.

        lo_ptr = lo_ptr->cdr.
        _validate lo_ptr.
        IF lo_ptr NE nil.
          _validate_index lo_ptr->car 'vector-fill! end'.

          result = lo_vec->fill( from = lv_start
                                 to = CAST lcl_lisp_integer( lo_ptr->car )->int
                                 elem = lo_fill ).
        ELSE.
          result = lo_vec->fill( from = lv_start
                                 elem = lo_fill ).
        ENDIF.

      ELSE.
        result = lo_vec->fill( elem = lo_fill ).
      ENDIF.

    ENDMETHOD.

    METHOD proc_vector_to_list.
      "   (vector->list vector)
      "   (vector->list vector start) procedure
      "   (vector->list vector start end) procedure
* The vector->list procedure returns a newly allocated list of the objects contained
* in the elements of vector between start and end. Order is preserved.
      _validate list.
      _validate_vector list->car 'vector->list'.
      DATA lv_start TYPE tv_int.
      DATA(lo_vec) = CAST lcl_lisp_vector( list->car ).

      _validate list->cdr.
      IF list->cdr NE nil.
        DATA(lo_start) = list->cdr->car.
        _validate_index lo_start 'vector->list start'.
        _to_integer lo_start lv_start.

        _validate list->cdr->cdr.
        IF list->cdr->cdr NE nil.
          DATA(lo_end) = list->cdr->cdr->car.
          _validate_index lo_end 'vector->list end'.

          result = lo_vec->get_list( from = lv_start
                                     to = CAST lcl_lisp_integer( lo_end )->int ).
        ELSE.
          result = lo_vec->get_list( from = lv_start ).
        ENDIF.

      ELSE.
        result = lo_vec->to_list( ).
      ENDIF.
    ENDMETHOD.

    METHOD proc_list_to_vector.
      "   (list->vector list)
* The list->vector procedure returns a newly created vector initialized
* to the elements of the list list. Order is preserved.
      _validate list.

      result = lcl_lisp_vector=>from_list( list->car ).
    ENDMETHOD.

    METHOD proc_bytevector.
      "(bytevector 1 3 5 1 3 5) => #u8(1 3 5 1 3 5)
      "(bytevector) => #u8()
      _validate list.

      result = lcl_lisp_bytevector=>from_list( list ).
    ENDMETHOD.

    METHOD proc_bytevector_length.
      _validate list.
      _validate_bytevector list->car 'bytevector-length'.
      _assert_one_param 'bytevector-length'.

      result = CAST lcl_lisp_bytevector( list->car )->length( ).
    ENDMETHOD.

    METHOD proc_bytevector_u8_set.
      " (bytevector-u8-set! bytevector k byte)
      DATA lo_u8 TYPE REF TO lcl_lisp_bytevector.
      DATA lv_byte TYPE tv_byte.

      _validate: list, list->cdr, list->cdr->cdr.
      _validate_bytevector list->car `bytevector-u8-set!`.
      lo_u8 ?= list->car.

      _validate_index list->cdr->car `bytevector-u8-set!`.
      DATA(lv_index) = CAST lcl_lisp_integer( list->cdr->car )->int.

      DATA(lo_last) = list->cdr->cdr.
      _validate_byte lo_last->car `bytevector-u8-set!`.
      lv_byte = CAST lcl_lisp_integer( lo_last->car )->int.

      _assert_last_param lo_last.

      lo_u8->set( index = lv_index
                  iv_byte = lv_byte ).
      result = lo_u8.
    ENDMETHOD.

    METHOD proc_bytevector_u8_ref.
      " (bytevector-u8-ref bytevector k)
      DATA lo_u8 TYPE REF TO lcl_lisp_bytevector.
      DATA lv_index TYPE tv_int.

      _validate: list, list->cdr.
      _validate_bytevector list->car `bytevector-u8-ref`.
      lo_u8 ?= list->car.

      DATA(lo_last) = list->cdr.
      _validate_index lo_last->car `bytevector-u8-ref`.
      lv_index = CAST lcl_lisp_integer( lo_last->car )->int.

      _assert_last_param lo_last.

      result = lcl_lisp_new=>integer( value = lo_u8->get( index = lv_index )
                                      iv_exact = abap_true ).
    ENDMETHOD.

    METHOD proc_bytevector_append.
      " (bytevector-append bytevector ... ) procedure
      " Returns a newly allocated bytevector whose elements are the concatenation of the elements in the given bytevectors.
      DATA lt_byte TYPE tt_byte.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      DATA lo_bytes TYPE REF TO lcl_lisp_bytevector.
      DATA lv_mutable TYPE tv_flag.
      _validate list.

      lo_ptr = list.
      lv_mutable = abap_true.
      WHILE lo_ptr->type = type_pair AND lo_ptr->car->type EQ type_bytevector.
        lo_bytes = CAST lcl_lisp_bytevector( lo_ptr->car ).
        APPEND LINES OF lo_bytes->bytes TO lt_byte.
        IF lo_bytes->mutable EQ abap_false.
          lv_mutable = abap_false.
        ENDIF.
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      IF lo_ptr NE nil.
        lo_ptr->car->raise( ` is not a bytevector in bytevector-append` ).
      ENDIF.
      result = lcl_lisp_new=>bytevector( it_byte = lt_byte
                                         iv_mutable = lv_mutable ).
    ENDMETHOD.

    METHOD proc_is_bytevector.
      _arg_is_type bytevector `bytevector?`.
    ENDMETHOD.

    METHOD proc_make_bytevector.
      DATA lo_fill TYPE REF TO lcl_lisp_integer.
      DATA lv_fill TYPE tv_byte.
      _validate: list, list->cdr.

      DATA(lo_size) = list->car.

      _validate_index lo_size `make-bytevector`.

      IF list->cdr NE lcl_lisp=>nil.
        _validate_byte list->cdr->car `make-bytevector`.
        lo_fill ?= list->cdr->car.
        lv_fill = lo_fill->int.
        _assert_last_param list->cdr.
      ELSE.
        lv_fill = 0.
        _assert_last_param list.
      ENDIF.

      result = lcl_lisp_bytevector=>init( size = CAST lcl_lisp_integer( lo_size )->int
                                          iv_fill = lv_fill ).
    ENDMETHOD.

    METHOD proc_utf8_to_string.
      " (utf8->string bytevector)
      " (utf8->string bytevector start)
      " (utf8->string bytevector start end)
      "     The utf8->string procedure decodes the bytes of a bytevector
      "     between start and end and returns the corresponding string;
      " (utf8->string #u8(#x41)) =) "A"
      DATA lo_u8 TYPE REF TO lcl_lisp_bytevector.
      DATA lv_start TYPE tv_index.
      DATA lv_end TYPE tv_index.

      _validate list.
      _validate_bytevector list->car `utf8->string`.
      lo_u8 ?= list->car.

      lv_start = 0.
      lv_end = lines( lo_u8->bytes ).

      IF list->cdr IS BOUND AND list->cdr NE nil.
        _validate_index list->cdr->car `utf8->string start`.
        lv_start = CAST lcl_lisp_integer( list->cdr->car )->int.

        IF list->cdr->cdr IS BOUND.
          DATA(lo_last) = list->cdr->cdr.
          IF lo_last NE nil.
            _validate_index lo_last->car `utf8->string end`.
            lv_end = CAST lcl_lisp_integer( lo_last->car )->int.

            _assert_last_param lo_last.
          ENDIF.
        ENDIF.

      ENDIF.

      result = lcl_lisp_new=>string( value = lo_u8->utf8_to_string( from = lv_start
                                                                    to = lv_end )
                                     iv_mutable = lo_u8->mutable ).
    ENDMETHOD.

    METHOD proc_string_to_utf8.
      " (string->utf8 string)
      " (string->utf8 string start)
      " (string->utf8 string start end)
      "the string->utf8 procedure encodes the characters of a string between start and end and returns the corresponding bytevector.
      DATA lo_string TYPE REF TO lcl_lisp_string.
      DATA lv_start TYPE tv_index.
      DATA lv_end TYPE tv_index.

      _validate list.
      _validate_string list->car `string->utf8`.
      lo_string ?= list->car.

      lv_start = 0.
      lv_end = numofchar( lo_string->value ).

      IF list->cdr IS BOUND AND list->cdr NE nil.
        _validate_index list->cdr->car `string->utf8 start`.
        lv_start = CAST lcl_lisp_integer( list->cdr->car )->int.

        IF list->cdr->cdr IS BOUND.
          DATA(lo_last) = list->cdr->cdr.
          IF lo_last NE nil.
            _validate_index lo_last->car `string->utf8 end`.
            lv_end = CAST lcl_lisp_integer( lo_last->car )->int.

            _assert_last_param lo_last.
          ENDIF.
        ENDIF.

      ENDIF.

      result = lcl_lisp_bytevector=>utf8_from_string( iv_text = lo_string->value
                                                      iv_mutable = lo_string->mutable ).
    ENDMETHOD.

    METHOD proc_bytevector_new_copy.
*    (bytevector-copy bytevector) procedure
*    (bytevector-copy bytevector start) procedure
*    (bytevector-copy bytevector start end) procedure
*    Returns a newly allocated bytevector containing the bytes
*    in bytevector between start and end.
      " (define a #u8(1 2 3 4 5))
      " (bytevector-copy a 2 4)) =) #u8(3 4)

      DATA lo_u8 TYPE REF TO lcl_lisp_bytevector.
      DATA lv_start TYPE tv_index.
      DATA lv_end TYPE tv_index.

      _validate list.
      _validate_bytevector list->car `bytevector-copy`.
      lo_u8 ?= list->car.

      lv_start = 0.
      lv_end = lines( lo_u8->bytes ).

      IF list->cdr IS BOUND AND list->cdr NE nil.
        _validate_index list->cdr->car `bytevector-copy start`.
        lv_start = CAST lcl_lisp_integer( list->cdr->car )->int.

        IF list->cdr->cdr IS BOUND.
          DATA(lo_last) = list->cdr->cdr.
          IF lo_last NE nil.
            _validate_index lo_last->car `bytevector-copy end`.
            lv_end = CAST lcl_lisp_integer( lo_last->car )->int.

            _assert_last_param lo_last.
          ENDIF.
        ENDIF.

      ENDIF.

      result = lo_u8->copy_new( from = lv_start
                                to = lv_end ).
    ENDMETHOD.

    METHOD proc_bytevector_copy.
      "(bytevector-copy! to at from) procedure
      "(bytevector-copy! to at from start) procedure
      "(bytevector-copy! to at from start end) procedure
      " Copies the bytes of bytevector from between start and end to bytevector to, starting at at.

      " The order in which bytes are copied is unspecified, except that if the source and destination overlap,
      " copying takes place as if the source is first copied into a temporary bytevector and then into the destination.
      " This can be achieved without allocating storage by making sure to copy in the correct direction in such circumstances.
      DATA lo_u8_to TYPE REF TO lcl_lisp_bytevector.
      DATA lo_u8_from TYPE REF TO lcl_lisp_bytevector.
      DATA lv_start TYPE tv_index.
      DATA lv_end TYPE tv_index.
      DATA lv_at TYPE tv_index.
      DATA lv_length_to TYPE tv_index.
      DATA lo_ptr TYPE REF TO lcl_lisp.

      _validate: list, list->cdr, list->cdr->cdr.
      _validate_bytevector list->car `bytevector-copy! to`.
      lo_u8_to ?= list->car.
      lv_length_to = lines( lo_u8_to->bytes ).

      lo_ptr = list->cdr.
      _validate_index lo_ptr->car `bytevector-copy! at`.
      _to_integer lo_ptr->car lv_at.
      IF lv_at GT lv_length_to.
        lo_ptr->car->raise( ` "at" is greater than the length of "to" in bytevector-copy!` ).
      ENDIF.

      lo_ptr = lo_ptr->cdr.
      _validate_bytevector lo_ptr->car `bytevector-copy! from`.
      lo_u8_from ?= lo_ptr->car.

      lv_start = 0.
      lv_end = lines( lo_u8_from->bytes ) - 1.
      lo_ptr = lo_ptr->cdr.
      IF lo_ptr IS BOUND AND lo_ptr NE nil.
        _validate_index lo_ptr->car `bytevector-copy! start`.
        _to_integer lo_ptr->car lv_start.

        lo_ptr = lo_ptr->cdr.
        IF lo_ptr IS BOUND AND lo_ptr NE nil.
          _validate_index lo_ptr->car `bytevector-copy! end`.
          _to_integer lo_ptr->car lv_end.

          _assert_last_param lo_ptr.
        ENDIF.
      ENDIF.

      "  It is also an error if (- (bytevector-length to) at) is less than (- end start).
      IF ( lv_length_to - lv_at ) < ( lv_end - lv_start ).
        lo_u8_to->raise( ` (- (bytevector-length to) at) is less than (- end start) to in bytevector-copy!` ).
      ENDIF.

      lo_u8_to->copy( at = lv_at
                      io_from = lo_u8_from
                      start = lv_start
                      end = lv_end ).

      result = lo_u8_to.
    ENDMETHOD.

* (memq obj list) return the first sublist of list whose car is obj
* where  the  sublists  of list are the non-empty lists returned by (list-tail list  k)
* for k less than the length of list.
* If obj does not occur in list, then #f (not the empty list) is returned.
* Memq uses eq? to compare obj with the elements of list
    METHOD proc_memq.
      _validate: list, list->car, list->cdr.

      result = false.

      CHECK list->cdr NE nil.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_item) = list->car.
      WHILE lo_sublist NE nil AND lo_sublist->car->type EQ lo_item->type.

        CASE lo_item->type.
          WHEN type_integer  " number
            OR type_rational
            OR type_real
            OR type_complex.
            IF CAST lcl_lisp_number( lo_item )->number_is_equal( CAST lcl_lisp_number( lo_sublist->car ) ).
              result = lo_sublist.
              RETURN.
            ENDIF.

          WHEN type_symbol OR type_string.
            DATA(lo_symbol) = CAST lcl_lisp_symbol( lo_item ).
            DATA(lo_s_car) = CAST lcl_lisp_symbol( lo_sublist->car ).
            IF lo_symbol->value = lo_s_car->value AND lo_symbol->index = lo_s_car->index.
              result = lo_sublist.
              RETURN.
            ENDIF.

          WHEN OTHERS.
            IF lo_item = lo_sublist->car.
              result = lo_sublist.
              RETURN.
            ENDIF.
        ENDCASE.

        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
    ENDMETHOD.

    METHOD proc_memv.
      _validate: list, list->car, list->cdr.

      result = false.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_item) = list->car.
      WHILE lo_sublist->type EQ type_pair.
        IF lo_sublist->car->is_equivalent( lo_item ).
          result = lo_sublist.
          RETURN.
        ENDIF.
        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
*      CHECK lo_sublist NE nil.
*      list->error_not_a_list( ).
    ENDMETHOD.

    METHOD get_equal_params.
      _validate: io_list, io_list->car, io_list->cdr.

      eo_sublist = io_list->cdr->car.
      eo_key = io_list->car.
      eo_compare = io_list->cdr->cdr.
    ENDMETHOD.

    METHOD proc_member.
      result = false.
      get_equal_params( EXPORTING io_list = list
                        IMPORTING eo_sublist = DATA(lo_sublist)
                                  eo_compare = DATA(lo_compare)
                                  eo_key = DATA(lo_key) ).

      WHILE lo_sublist->type EQ type_pair.
        IF lo_key->is_equal( io_elem = lo_sublist->car
                             comp = lo_compare
                             interpreter = me
                             environment = env ).
          result = lo_sublist.
          RETURN.
        ENDIF.
        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
*      CHECK lo_sublist NE nil.
*      list->error_not_a_list( ).
    ENDMETHOD.

    METHOD proc_assoc.

      result = false.
      get_equal_params( EXPORTING io_list = list
                        IMPORTING eo_sublist = DATA(lo_sublist)
                                  eo_compare = DATA(lo_compare)
                                  eo_key = DATA(lo_key) ).

      WHILE lo_sublist->type EQ type_pair.
        DATA(lo_pair) = lo_sublist->car.
        IF lo_key->is_equal( io_elem = lo_pair->car
                             comp = lo_compare
                             interpreter = me
                             environment = env ).
          result = lo_pair.
          RETURN.
        ENDIF.
        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
    ENDMETHOD.

* ( assq obj alist) - alist (for association list") must be a list of pairs.
* Find the first pair in alist whose car field is obj, and returns that pair.
* If no pair in alist has obj as its car, then #f (not the empty list) is returned.
* Assq uses eq? to compare obj with the car fields of the pairs in alist, while
* assv uses eqv? and assoc uses equal?
    METHOD proc_assq.
      _validate: list, list->car, list->cdr.

      result = false.

      CHECK list->cdr NE nil.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_key) = list->car.
      WHILE lo_sublist->type EQ type_pair.
        DATA(lo_pair) = lo_sublist->car.
        IF lo_pair->car->type EQ lo_key->type.

          CASE lo_key->type.
            WHEN type_integer   " number
              OR type_rational
              OR type_real
              OR type_complex.

              DATA(lo_number) = CAST lcl_lisp_number( lo_key ).
              DATA(lo_other) = CAST lcl_lisp_number( lo_pair->car ).

              IF lo_number->number_is_equal( lo_other ).
                result = lo_pair.
                RETURN.
              ENDIF.

            WHEN type_symbol OR type_string.
              IF lo_key->value = lo_pair->car->value.
                result = lo_pair.
                RETURN.
              ENDIF.

            WHEN OTHERS.
              IF lo_key = lo_pair->car.
                result = lo_pair.
                RETURN.
              ENDIF.
          ENDCASE.

        ENDIF.

        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
    ENDMETHOD.

    METHOD proc_assv.
      _validate: list, list->car, list->cdr.

      result = false.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_key) = list->car.

      WHILE lo_sublist->type EQ type_pair.
        DATA(lo_pair) = lo_sublist->car.
        _validate lo_pair->car.
        IF lo_pair->car->is_equivalent( lo_key ).
          result = lo_pair.
          RETURN.
        ENDIF.
        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
    ENDMETHOD.

**********************************************************************

    METHOD proc_add.
      DATA res TYPE ts_result.
      DATA num TYPE REF TO lcl_lisp_number.
      DATA next TYPE REF TO lcl_lisp.
      DATA lo_head TYPE REF TO lcl_lisp.

      _validate list.

      res = lcl_lisp_number=>zero->get_state( '+' ).
      DATA(iter) = list->new_iterator( ).

      WHILE iter->has_next( ).
        next = iter->next( ).

        _values_get_next next.
        _validate_number next `+`.
        num = CAST lcl_lisp_number( next ).

        res = num->sum( first = res
                        second = num->get_state( '+' ) ).
      ENDWHILE.

      result = lcl_lisp_new=>numeric( res ).
    ENDMETHOD.

    METHOD proc_multiply.
      DATA res TYPE ts_result.
      DATA num TYPE REF TO lcl_lisp_number.
      DATA next TYPE REF TO lcl_lisp.
      DATA lo_head TYPE REF TO lcl_lisp.

      _validate list.
      res = lcl_lisp_number=>one->get_state( '*' ).
      DATA(iter) = list->new_iterator( ).

      WHILE iter->has_next( ).
        next = iter->next( ).

        _values_get_next next.
        _validate_number next `*`.
        num = CAST lcl_lisp_number( next ).

        res = num->product( first = res
                            second = num->get_state( '*' ) ).
      ENDWHILE.
      result = lcl_lisp_new=>numeric( res ).
    ENDMETHOD.                    "proc_multiply

    METHOD proc_subtract.
      DATA res TYPE ts_result.
      DATA num TYPE REF TO lcl_lisp_number.
      DATA next TYPE REF TO lcl_lisp.
      DATA lo_head TYPE REF TO lcl_lisp.
      _validate list.

      DATA(iter) = list->new_iterator( ).

      IF iter->has_next( ) EQ abap_false.
        throw( |no number in [-]| ).
      ENDIF.

      next = iter->next( ).
      _values_get_next next.
      _validate_number next `-`.
      num = CAST lcl_lisp_number( next ).

      IF iter->has_next( ) EQ abap_false.
        result = lcl_lisp_number=>zero->substract( num ).
      ELSE.
        " Subtract all subsequent numbers from the first
        res = num->get_state( '-' ).

        WHILE iter->has_next( ).
          next = iter->next( ).
          _values_get_next next.
          _validate_number next `-`.
          num = CAST lcl_lisp_number( next ).

          res = num->sum( first = res
                          second = num->neg_sign( num->get_state( '-' ) ) ).
        ENDWHILE.

        result = lcl_lisp_new=>numeric( res ).
      ENDIF.

    ENDMETHOD.

    METHOD proc_divide.
      DATA num TYPE REF TO lcl_lisp_number.
      DATA next TYPE REF TO lcl_lisp.
      DATA lo_head TYPE REF TO lcl_lisp.
      _validate list.

      DATA(iter) = list->new_iterator( ).

      IF iter->has_next( ) EQ abap_false.
        throw( |no number in [/]| ).
      ENDIF.

      next = iter->next( ).
      _values_get_next next.
      _validate_number next `/`.
      num = CAST lcl_lisp_number( next ).

      IF iter->has_next( ) EQ abap_false.
        result = lcl_lisp_number=>one->divide( num ).
      ELSE.
        " Divide by all subsequent numbers
        DATA(res) = num->get_state( '/' ).

        WHILE iter->has_next( ).
          next = iter->next( ).
          _values_get_next next.
          _validate_number next `/`.
          num = CAST lcl_lisp_number( next ).

          res = num->quotient( numerator = res
                               denominator = num->get_state( '/' ) ).
        ENDWHILE.

        result = lcl_lisp_new=>numeric( res ).
      ENDIF.

    ENDMETHOD.

**********************************************************************
    METHOD proc_gt.
      _comparison <= '>'.
    ENDMETHOD.                    "proc_gt

    METHOD proc_gte.
      _comparison < '>='.
    ENDMETHOD.                    "proc_gte

    METHOD proc_lt.
      _comparison >= '<'.
    ENDMETHOD.                    "proc_lt

    METHOD proc_lte.
      _comparison > '<='.
    ENDMETHOD.                    "proc_lte

    METHOD proc_is_zero.
      DATA cell TYPE REF TO lcl_lisp.
      DATA num TYPE REF TO lcl_lisp_number.

      result = false.
      _validate list.
      _validate_number list->car '[zero?]'.
      cell = list->car.
      "_get
      num ?= cell.

      CASE num->type.
        WHEN type_complex.
          IF CAST lcl_lisp_complex( num )->is_real( ) AND sign( num->to_real( ) ) EQ 0.
            result = true.
          ENDIF.

        WHEN OTHERS.
          IF sign( num->to_real( ) ) EQ 0.
            result = true.
          ENDIF.
      ENDCASE.
    ENDMETHOD.

    METHOD proc_is_positive.
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_head TYPE REF TO lcl_lisp.
      DATA num TYPE REF TO lcl_lisp_number.

      result = false.
      _validate list.
      _validate_number list->car '[positive?]'.
      cell = list->car.
      _values_get_next cell.
      num ?= cell.

      IF sign( num->to_real( ) ) EQ 1.
        result = true.
      ENDIF.
    ENDMETHOD.

    METHOD proc_is_negative.
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_head TYPE REF TO lcl_lisp.
      DATA num TYPE REF TO lcl_lisp_number.

      result = false.
      _validate list.
      _validate_number list->car '[negative?]'.
      cell = list->car.
      _values_get_next cell.
      num ?= cell.

      IF sign( num->to_real( ) ) EQ -1.
        result = true.
      ENDIF.
    ENDMETHOD.

    METHOD proc_is_odd.
      result = false.
      _validate list.
      _validate_integer list->car 'odd?'.
      CHECK CAST lcl_lisp_integer( list->car )->int MOD 2 NE 0.
      result = true.
    ENDMETHOD.                    "proc_lte

    METHOD proc_is_even.
      result = false.
      _validate list.
      _validate_integer list->car 'even?'.
      CHECK CAST lcl_lisp_integer( list->car )->int MOD 2 EQ 0.
      result = true.
    ENDMETHOD.                    "proc_lte

**********************************************************************

    METHOD proc_eql.
      _data_local_numeric.
      DATA lo_number TYPE REF TO lcl_lisp_number.

      _validate: list, list->car, list->cdr.

      result = false.
      DATA(lo_ptr) = list.

      _validate_number lo_ptr->car '='.
      lo_number ?= lo_ptr->car.
      IF lo_number->is_nan( ).
        RETURN.
      ENDIF.

      DATA(res) = lo_number->get_state( '=' ).
      lo_ptr = lo_ptr->cdr.

      IF lo_ptr->type NE type_pair.
        throw( c_error_incorrect_input ).
      ENDIF.

      WHILE lo_ptr NE nil.
        _validate_number lo_ptr->car '='.
        lo_number ?= lo_ptr->car.
        IF lo_number->is_nan( ).
          result = false.
          RETURN.
        ENDIF.

        CASE lo_number->type.
          WHEN type_integer
            OR type_rational
            OR type_real.

            IF res-type EQ type_complex.
              " returns false if imaginary part is not 0
              IF NOT lo_number->number_is_zero( res-imag_part ).
                result = false.
                EXIT.
              ENDIF.
            ENDIF.

            " check if real part matches
            IF lo_number->real_is_equal( res-real_part ).
              result = true.
            ELSE.
              result = false.
            ENDIF.

          WHEN type_complex.
            lo_z ?= lo_number.

            IF lo_z->zreal->real_is_equal( res-real_part )         " check if real part matches
              AND lo_z->zimag->real_is_equal( res-imag_part ).     " check if imaginary part matches
              result = true.
            ELSE.
              result = false.
            ENDIF.

        ENDCASE.

        IF result = false.
          EXIT.
        ENDIF.

        lo_ptr = lo_ptr->cdr.
      ENDWHILE.

    ENDMETHOD.                    "proc_eql

    METHOD proc_eq.
      _validate: list, list->car,
                 list->cdr, list->cdr->car.
      list->cdr->assert_last_param( ).

      DATA(lo_ref) = list->car.
      DATA(lo_target) = list->cdr->car.

      IF lo_ref = lo_target.
        result = true.
        RETURN.
      ELSE.
        result = false.
      ENDIF.

      CHECK lo_ref->type EQ lo_target->type.

      CASE lo_ref->type.
        WHEN type_integer
          OR type_rational
          OR type_real
          OR type_complex.
          DATA(lo_ref_number) = CAST lcl_lisp_number( lo_ref ).
          DATA(lo_target_number) = CAST lcl_lisp_number( lo_target ).
          IF lo_ref_number->exact EQ lo_target_number->exact AND lo_ref_number->number_is_equal( lo_target_number ).
            result = true.
          ELSE.
            result = false.
            RETURN.
          ENDIF.

        WHEN type_string.
          IF CAST lcl_lisp_string( lo_ref )->value = CAST lcl_lisp_string( lo_target )->value.
            result = true.
          ELSE.
            result = false.
            RETURN.
          ENDIF.

        WHEN type_symbol.
          DATA(lo_symbol) = CAST lcl_lisp_symbol( lo_ref ).
          DATA(lo_s_car) = CAST lcl_lisp_symbol( lo_target ).
          IF lo_symbol->value = lo_s_car->value
            AND lo_symbol->index = lo_s_car->index.  " for uninterned symbols
            result = true.
          ELSE.
            result = false.
            RETURN.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.

    ENDMETHOD.                    "proc_eq

    METHOD proc_equal.
* equal? returns the same as eqv? when applied to booleans, symbols, numbers, characters, ports,
* procedures, and the empty list.
* In all other cases, equal? may return either #t or #f.
* Even if its arguments are circular data structures, equal? must always terminate.
      _validate: list, list->car.
      result = false.
      DATA(lo_ptr) = list.
      DATA(lo_slow) = list.

      WHILE lo_ptr->cdr NE nil.
        DATA(lo_next) = lo_ptr->cdr->car.
        _validate lo_next.

        IF lo_next->is_equal( lo_ptr->car ).
          result = true.
        ELSE.
          result = false.
          RETURN.
        ENDIF.
        lo_ptr = lo_ptr->cdr.
        lo_slow = lo_slow->cdr.

        CHECK lo_ptr->cdr NE nil.
        lo_next = lo_ptr->cdr->car.
        _validate lo_next.

        IF lo_next->is_equal( lo_ptr->car ).
          result = true.
        ELSE.
          result = false.
          RETURN.
        ENDIF.
        lo_ptr = lo_ptr->cdr.
        CHECK lo_ptr EQ lo_slow.
*       Circular list
        RETURN.
      ENDWHILE.

    ENDMETHOD.                    "proc_equal

    METHOD proc_eqv. " eqv?
      _validate: list, list->car, list->cdr, list->cdr->car.
      result = false.
      list->cdr->assert_last_param(  ).

      DATA(lo_obj1) = list->car.
      DATA(lo_obj2) = list->cdr->car.
      IF lo_obj1->is_equivalent( lo_obj2 ).
        result = true.
      ENDIF.
    ENDMETHOD.

*--------------------------------------------------------------------*
*   Delayed evaluation functions
    METHOD proc_make_promise.
      " (make-promise? obj)
      " The make-promise procedure returns a promise which, when forced, will return obj.
      " It is similar to delay, but does not delay its argument: it is a procedure rather than syntax.
      " If obj is already a promise, it is returned.
      throw( |make-promise not implemented yet| ).
      "result = lcl_lisp_new=>promise( ).
    ENDMETHOD.

    METHOD proc_force.
      " (force promise) lazy library procedure

      " The force procedure forces the value of a promise created by delay, delay-force, or make-promise.
      " If no value has been computed for the promise, then a value is computed and returned.
      " The value of the promise must be cached (or "memoized") so that if it is forced a second time,
      " the previously computed value is returned. consequently, a delayed expression is evaluated
      " using the parameter values and exception handler of the call to force which first requested its value.

      " IF promise IS NOT a promise, it may be returned unchanged.
      result = list. " evaluated param           throw( |force not implemented yet| ).

    ENDMETHOD.

    METHOD proc_is_promise.
      " (promise? obj)
      " The promise? procedure returns #t if its argument is a promise, and #f otherwise.
      " Note that promises are not necessarily disjoint from other Scheme types such as procedures.
      throw( |promise? not implemented yet| ).
      result = nil.
    ENDMETHOD.

*--------------------------------------------------------------------*
*   Hash-related functions
    METHOD proc_make_hash.
      result = lcl_lisp_new=>hash( list ).
    ENDMETHOD.                    "proc_make_hash

*   Get an element from a hash
    METHOD proc_hash_get.
      result = lcl_lisp_hash=>from_list( list = list
                                         msg = 'HASH-GET' )->get( list->cdr ).
    ENDMETHOD.                    "proc_hash_get

*   Insert an element into a hash
    METHOD proc_hash_insert.
      result = lcl_lisp_hash=>from_list( list = list
                                         msg = 'HASH-INSERT' )->insert( list->cdr ).
    ENDMETHOD.                    "proc_hash_insert

*   Remove an element from a hash
    METHOD proc_hash_remove.
      result = lcl_lisp_hash=>from_list( list = list
                                         msg = 'HASH-REMOVE' )->delete( list->cdr ).
    ENDMETHOD.                    "proc_hash_delete

*   Return the keys of a hash
    METHOD proc_hash_keys.
      result = lcl_lisp_hash=>from_list( list = list
                                         msg = 'HASH-KEYS' )->get_hash_keys( ).
    ENDMETHOD.                    "proc_hash_keys

    METHOD proc_is_string.
      _arg_is_type string `string?`.
    ENDMETHOD.                    "proc_is_string

    METHOD proc_is_char.
      _arg_is_type char `char?`.
    ENDMETHOD.                    "proc_is_char

    METHOD proc_is_hash.
      _arg_is_type hash `hash?`.
    ENDMETHOD.                    "proc_is_hash

    METHOD proc_is_nan. " argument in list->car
      result = false.
      CHECK list IS BOUND AND list->car IS BOUND AND list->car->is_nan( ).
      result = true.
    ENDMETHOD.

    METHOD proc_is_finite. " argument in list->car
      _get_arg0_as_number `finite?`.

      result = lo_number->is_finite( ).
    ENDMETHOD.

    METHOD proc_is_infinite. " argument in list->car
      _get_arg0_as_number `infinite?`.

      result = lo_number->is_infinite( ).
    ENDMETHOD.

    METHOD proc_is_number. " argument in list->car
      _validate: list, list->car.
      _assert_one_param `number?`.
      result = false.
      IF list->car->is_number( ).
        result = true.
      ENDIF.
    ENDMETHOD.                    "proc_is_number

    METHOD proc_is_complex.
      _validate: list, list->car.
      _assert_one_param `complex?`.
      IF list->car->is_number( ).
        result = true.
      ENDIF.
    ENDMETHOD.

    METHOD proc_is_real.
*     If z is a complex number, then (real? z) is true if and only if (zero? (imag-part z)) is true.
      result = false.
      CHECK list IS BOUND AND list->car IS BOUND.
      CASE list->car->type.
        WHEN type_real
          OR type_rational
          OR type_integer.
          result = true.
        WHEN type_complex.
          IF CAST lcl_lisp_complex( list->car )->is_real( ).
            result = true.
          ENDIF.
      ENDCASE.
    ENDMETHOD.

    METHOD proc_is_rational.
      DATA lo_real TYPE REF TO lcl_lisp_real.
      result = false.
      CHECK list IS BOUND AND list->car IS BOUND.
      CASE list->car->type.
        WHEN type_rational
          OR type_integer.
          result = true.
        WHEN type_real.
          " All finite reals are rational in this implementation
          " The numbers +inf.0, -inf.0 and +nan.0 are real but not rational
          lo_real ?= list->car.
          IF lo_real->infnan = abap_false.
            result = true.
          ENDIF.
      ENDCASE.
    ENDMETHOD.

    METHOD proc_is_exact_integer.
      _validate list.
      _assert_one_param 'exact-integer?'.
*     Returns #t if z is both exact and an integer; otherwise returns #f.
      result = false.
      CHECK list->car IS BOUND.
      CASE list->car->type.
        WHEN type_integer
          OR type_rational
          OR type_real
          OR type_complex.
          DATA(num) = CAST lcl_lisp_number( list->car ).
          IF num->exact EQ abap_true AND num->is_integer( ).
            result = true.
          ENDIF.
      ENDCASE.
    ENDMETHOD.

    METHOD proc_is_integer.
      _validate list.
      _assert_one_param 'integer?'.
*     If x is an inexact real number, then (integer? x) is true if and only if (= x (round x)).

      result = false.
      CHECK list->car IS BOUND.
      CASE list->car->type.
        WHEN type_integer
          OR type_rational
          OR type_real
          OR type_complex.
          IF CAST lcl_lisp_number( list->car )->is_integer( ).
            result = true.
          ENDIF.
      ENDCASE.
    ENDMETHOD.                    "proc_is_integer

    METHOD proc_is_symbol.
      _arg_is_type symbol `symbol?`.
    ENDMETHOD.

    METHOD proc_symbol_list_is_equal.
      DATA lv_ref TYPE string.
      DATA lo_test TYPE REF TO lcl_lisp.
      DATA lo_arg TYPE REF TO lcl_lisp.

      _validate list.

      result = false.
      lo_arg = list.

      lo_test = nil.
      IF lo_arg->type EQ type_pair AND lo_arg->car->type EQ type_symbol.
        lo_test = lo_arg->car.
        lv_ref = lo_test->value.
        lo_arg = lo_arg->cdr.
      ENDIF.
      IF lo_test EQ nil OR lo_arg EQ nil.
        throw( |symbol=? missing symbol argument in { lo_arg->car->to_string( ) }| ).
      ENDIF.

      WHILE lo_arg->type EQ type_pair AND lo_arg->car->type EQ type_symbol.
        IF lo_arg->car->value NE lv_ref.
          RETURN.
        ENDIF.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

      IF lo_arg NE nil.
        throw( |symbol=? wrong argument { lo_arg->car->to_string( ) }| ).
      ENDIF.
      CHECK lo_arg = nil.
      result = true.

    ENDMETHOD.

    METHOD proc_is_pair. " argument in list->car
      _arg_is_type pair `pair?`.
    ENDMETHOD.                    "proc_is_pair

    METHOD proc_is_boolean. " argument in list->car
      _arg_is_type boolean `boolean?`.
    ENDMETHOD.

    METHOD proc_is_vector.  " argument in list->car
      _arg_is_type vector `vector?`.
    ENDMETHOD.

    METHOD proc_is_list.  " argument in list->car
      _validate: list, list->car, list->cdr.
      IF list->cdr NE nil.
        throw( |list? takes only one argument| ).
      ENDIF.

      result = false.

      DATA(lo_ptr) = list->car.
      DATA(lo_slow) = lo_ptr.
*     Iterate over list
      WHILE lo_ptr->type EQ type_pair.
        lo_ptr = lo_ptr->cdr.
        lo_slow = lo_slow->cdr.
        CHECK lo_ptr->type EQ type_pair.
*       fast pointer takes 2 steps while slow pointer takes one
        lo_ptr = lo_ptr->cdr.
        CHECK lo_ptr = lo_slow.
*       If fast pointer eventually equals slow pointer, then we must be stuck in a circular list.
*       By deﬁnition, all lists have ﬁnite length and are terminated by the empty list,
*       so a circular list is not a list
        RETURN.
      ENDWHILE.

      CHECK lo_ptr EQ nil.
*     the last element of a list must be nil
      result = true.
    ENDMETHOD.                    "proc_is_list

    METHOD proc_boolean_list_is_equal.
      DATA lo_test TYPE REF TO lcl_lisp.
      DATA lo_arg TYPE REF TO lcl_lisp.

      _validate list.

      result = false.
      lo_arg = list.

      lo_test = nil.
      IF lo_arg->type EQ type_pair AND lo_arg->car->type EQ type_boolean.
        lo_test = lo_arg->car.
        lo_arg = lo_arg->cdr.
      ENDIF.
      IF lo_test EQ nil.
        throw( |boolean=? missing boolean argument in { lo_arg->car->to_string( ) }| ).
      ENDIF.

      WHILE lo_arg->type EQ type_pair AND lo_arg->car->type EQ type_boolean.
        IF lo_arg->car NE lo_test.
          RETURN.
        ENDIF.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

      IF lo_arg NE nil.
        throw( |boolean=? wrong argument { lo_arg->car->to_string( ) }| ).
      ENDIF.
      CHECK lo_arg = nil.
      result = true.
    ENDMETHOD.

    METHOD proc_is_procedure.
      result = false.
      CHECK list IS BOUND        " paramater (car) must not be valid
        AND list->car IS BOUND  " Body
        AND list->car->is_procedure( ).
      result = true.
    ENDMETHOD.

    METHOD proc_is_alist. " not in standard?
      _validate list.

      result = false.

      DATA(lo_arg) = list->car.
      WHILE lo_arg->type = type_pair.
        IF lo_arg->car->type = type_pair.
          RETURN.
        ENDIF.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

      CHECK lo_arg EQ nil.
      result = true.
    ENDMETHOD.                    "proc_is_alist

    METHOD proc_make_rectangular.
      _validate: list, list->car, list->cdr.
      IF list->cdr->cdr NE nil.
        throw( |make-rectangular takes only two argument| ).
      ENDIF.
      _validate_number: list->car '[make-rectangular]',
                        list->cdr->car '[make-rectangular]'.

      result = lcl_lisp_new=>rectangular( real = CAST lcl_lisp_number( list->car )
                                          imag = CAST lcl_lisp_number( list->cdr->car ) ).
    ENDMETHOD.

    METHOD proc_make_polar.
      _validate: list, list->car, list->cdr.
      IF list->cdr->cdr NE nil.
        throw( |make-polar takes only two argument| ).
      ENDIF.
      _validate_number: list->car '[make-polar]',
                        list->cdr->car '[make-polar]'.

      result = lcl_lisp_new=>polar( r = CAST lcl_lisp_number( list->car )
                                    angle = CAST lcl_lisp_number( list->cdr->car ) ).
    ENDMETHOD.

    METHOD proc_real_part.
      _get_arg0_as_number `real-part`.

      IF lo_number->type EQ type_complex.
        result = CAST lcl_lisp_complex( lo_number )->zreal.
      ELSE.
        result = lo_number.
      ENDIF.
    ENDMETHOD.

    METHOD proc_imag_part.
      _get_arg0_as_number `imag-part`.

      IF lo_number->type EQ type_complex.
        result = CAST lcl_lisp_complex( lo_number )->zimag.
      ELSE.
        result = lcl_lisp_number=>zero.
      ENDIF.
    ENDMETHOD.

    METHOD proc_magnitude.
      DATA z TYPE REF TO lcl_lisp_complex.
      _get_arg0_as_number `magnitude`.

      IF lo_number->type EQ type_complex.
        z ?= lo_number.
        IF z->infnan EQ abap_true.
          IF z->zreal = lcl_lisp_number=>nan OR z->zimag = lcl_lisp_number=>nan.
            result = lcl_lisp_number=>nan.
          ELSE.
            result = lcl_lisp_number=>inf.
          ENDIF.
        ELSE.
          result = lcl_lisp_new=>real_number( value = abs( z->magnitude )
                                              iv_exact = z->exact ).
        ENDIF.
      ELSE.
        result = lo_number->absolute( ).
      ENDIF.
    ENDMETHOD.

    METHOD proc_angle.
      DATA z TYPE REF TO lcl_lisp_complex.
      _get_arg0_as_number `angle`.

      IF lo_number->type EQ type_complex.
        z ?= lo_number.
        result = lcl_lisp_new=>real_number( value = z->angle
                                            iv_exact = z->exact ).
      ELSEIF sign( lo_number->to_real( ) ) GE 0.
        result = lo_number->zero.
      ELSE.
        result = lcl_lisp_new=>real_number( value = c_pi
                                            iv_exact = abap_false ).
      ENDIF.
    ENDMETHOD.

    METHOD proc_abs.
      _get_arg0_as_number `abs`.

      TRY.
          result = lo_number->absolute( ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_abs

    METHOD proc_sin.
      " sin(x + iy) = sin(x) cosh(y) + i cos(x) sinh(y)
      _trigonometric_header 'sin'.
          IF y = 0.
            result = lcl_lisp_new=>real( value = sin( a )
                                         iv_exact = cell_exact ).
          ELSE.
            result = lcl_lisp_new=>rectangular( real = lcl_lisp_new=>real( value = sin( a ) * cosh( b )
                                                                           iv_exact = cell_exact )
                                                imag = lcl_lisp_new=>real( value = cos( a ) * sinh( b )
                                                                           iv_exact = cell_exact ) ) .
          ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_sin

    METHOD proc_cos.
      " cos(x + iy) = cos(x) cosh(y) - i sin(x) sinh(y)
      _trigonometric_header 'cos'.
          IF y = 0.
            result = lcl_lisp_new=>real( value = cos( a )
                                         iv_exact = cell_exact ).
          ELSE.
            result = lcl_lisp_new=>rectangular( real = lcl_lisp_new=>real( value = cos( a ) * cosh( b )
                                                                           iv_exact = cell_exact )
                                                imag = lcl_lisp_new=>real( value = - sin( a ) * sinh( b )
                                                                           iv_exact = cell_exact ) ) .
          ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_cos

    METHOD proc_tan.
      _trigonometric_header 'tan'.
          IF y EQ 0.
            result = lcl_lisp_new=>real( value = tan( a )
                                         iv_exact = cell_exact ).
          ELSE.
            DATA(magnitude) = cos( a ) ** 2 + sinh( b ) ** 2.
            result = lcl_lisp_new=>rectangular( real = lcl_lisp_new=>real( value = sin( a ) * cos( a ) / magnitude
                                                                           iv_exact = cell_exact )
                                                imag = lcl_lisp_new=>real( value = sinh( b ) * cosh( b ) / magnitude
                                                                           iv_exact = cell_exact ) ) .
          ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_tan

    METHOD proc_asin.
      _trigonometric_header '[asin]'.
      DATA z TYPE REF TO lcl_lisp_complex.

      IF y EQ 0.
        result = lcl_lisp_new=>real( value = asin( a )
                                     iv_exact = cell_exact ).
      ELSE.
        "sin^-1 z = -i log(iz + sqrt(1 - z^2))
        z ?= complex_sqrt( x = 1 - x * x + y * y
                           y = - 2 * x * y ).

        z ?= complex_log( x = z->zreal->to_real( ) - y
                          y = z->zimag->to_real( ) + x ).

        result = lcl_lisp_new=>rectangular( real = z->zimag
                                            imag = lcl_lisp_new=>real( value = - z->zreal->to_real( )
                                                                       iv_exact = cell_exact ) ) .
      ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_asin

    METHOD proc_acos.
      _trigonometric_header '[acos]'.
      DATA z TYPE REF TO lcl_lisp_complex.

      IF y EQ 0.
        result = lcl_lisp_new=>real( value = acos( a )
                                     iv_exact = cell_exact ).
      ELSE.
        "cos^-1 z = -i log( z + i sqrt( 1 - z^2 ) )
        z ?= complex_sqrt( x = 1 - x * x + y * y
                           y = - 2 * x * y ).

        z ?= complex_log( x = x - z->zimag->to_real( )
                          y = y + z->zreal->to_real( ) ).

        result = lcl_lisp_new=>rectangular( real = z->zimag
                                            imag = lcl_lisp_new=>real( value = - z->zreal->to_real( )
                                                                       iv_exact = cell_exact ) ) .
      ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.

    METHOD proc_atan.
      "tan^-1 z = (log(1+iz) - log(1-iz))/(2i)
      " (atan y x) cf. table page 38 in r7rs
      DATA y TYPE tv_real.
      DATA x TYPE tv_real.
      DATA arctan TYPE f.
      DATA lv_tan TYPE f.
      DATA magnitude TYPE f.
      DATA lo_y TYPE REF TO lcl_lisp_number.
      DATA lo_x TYPE REF TO lcl_lisp_number.
      _data_local_numeric_cell.

      result = nil.
      _validate: list, list->car.
      TRY.
          lo_y ?= list->car.

          IF list->cdr IS BOUND AND list->cdr->car IS BOUND.
            _get_real y lo_y '[atan y x]'.

            lo_x ?= list->cdr->car.
            _get_real x list->cdr->car '[atan y x]'.

            list->cdr->assert_last_param( ).
          ELSE.
            _assert_one_param '[atan z]'.
            IF lo_y->type = type_complex.
              DATA ln_z TYPE REF TO lcl_lisp_complex.
              DATA i_quotient TYPE REF TO lcl_lisp_complex.

              cell_exact = lo_y->exact.

              DATA(i_minus_z) = lo_y->imaginary->substract( lo_y ).
              DATA(i_plus_z) = lo_y->imaginary->add( lo_y ).
              i_quotient ?= i_minus_z->divide( i_plus_z ).
              ln_z ?= complex_log( x = i_quotient->zreal->to_real( )
                                   y = i_quotient->zimag->to_real( )
                                   iv_exact = cell_exact ).
              DATA(z2i) = lo_z->imaginary->add( lo_z->imaginary ).
              result = ln_z->divide( z2i ).
              RETURN.
            ELSE.
              _get_real y list->car '[atan z]'.
              x = 1.
            ENDIF.
          ENDIF.
          cell_exact = abap_false.

          IF y = 0.
            IF x = 0.
              result = lcl_lisp_number=>nan.
              RETURN.
            ELSE.
              arctan = c_pi.
            ENDIF.
          ELSEIF lo_y->infnan EQ abap_true OR ( lo_x IS BOUND AND lo_x->infnan EQ abap_true ).
            "##TO DO fix the logic.
            IF lo_x IS BOUND. " lo_y or lo_x is InfNaN
              CASE lo_y.
                WHEN lcl_lisp_number=>inf
                  OR lcl_lisp_number=>neg_inf
                  OR lcl_lisp_number=>nan
                  OR lcl_lisp_number=>neg_nan.
                  result = lcl_lisp_number=>nan.
                  RETURN.
                WHEN OTHERS.
                  CASE lo_x.
                    WHEN lcl_lisp_number=>inf.
                      arctan = 2 * atan( 0 ).

                    WHEN lcl_lisp_number=>neg_inf
                      OR lcl_lisp_number=>nan
                      OR lcl_lisp_number=>neg_nan.
                      result = lcl_lisp_number=>nan.
                      RETURN.
                  ENDCASE.
              ENDCASE.
            ELSE. " lo_y is InfNaN
              arctan = 2 * atan( 1 ).
            ENDIF.
          ELSE.
            magnitude = sqrt( x * x  + y * y ).
            lv_tan = y / ( x + magnitude ).
            arctan = 2 * atan( lv_tan ).
          ENDIF.

          result = lcl_lisp_new=>real( value = arctan
                                       iv_exact = cell_exact ).

          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_atan

    METHOD proc_sinh.
      _trigonometric_header '[sinh]'.

          IF y EQ 0.
            result = lcl_lisp_new=>real( value = sinh( a )
                                         iv_exact = cell_exact ).
          ELSE.
            throw( `sinh not implemented yet for complex arguments` ).
          ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_sinh

    METHOD proc_cosh.
      _trigonometric_header '[cosh]'.
          IF y EQ 0.
            result = lcl_lisp_new=>real( value = cosh( a )
                                         iv_exact = cell_exact ).
          ELSE.
            throw( `cosh not implemented yet for complex arguments` ).
          ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_cosh

    METHOD proc_tanh.
      _trigonometric_header '[tanh]'.
          IF y EQ 0.
            result = lcl_lisp_new=>real( value = tanh( a )
                                         iv_exact = cell_exact ).
          ELSE.
            throw( `tanh not implemented yet for complex arguments` ).
          ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_tanh

    METHOD proc_asinh.
      _trigonometric_header '[asinh]'.
          IF y = 0.
            result = lcl_lisp_new=>real( value = log( x + sqrt( x ** 2 + 1 ) )
                                         iv_exact = abap_false ).
          ELSE.
            throw( `asinh not implemented yet for complex arguments` ).
          ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_asinh

    METHOD proc_acosh.
      _trigonometric_header '[acosh]'.
          IF y = 0.
            result = lcl_lisp_new=>real( value = log( x + sqrt( x ** 2 - 1 ) )
                                         iv_exact = abap_false ).
          ELSE.
            throw( `acosh not implemented yet for complex arguments` ).
          ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_acosh

    METHOD proc_atanh.
      _trigonometric_header '[atanh]'.
          IF y = 0.
            result = lcl_lisp_new=>real( value = ( log( 1 + x ) - log( 1 - x ) ) / 2
                                         iv_exact = abap_false ).
          ELSE.
            throw( `atanh not implemented yet for complex arguments` ).
          ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_atanh

    METHOD proc_expt.
      DATA base1 TYPE tv_real.
      DATA exp1 TYPE tv_real.
      _data_local_numeric_cell.

      result = nil.
      _validate: list, list->cdr.
      _assert_last_param list->cdr.

      _get_real base1 list->car '[expt]'.
      lv_exact = cell_exact.

      TRY.
*          _get_real exp1 list->cdr->car '[expt]'.
          _validate list->cdr->car.
          cell = list->cdr->car.
          _values_get_next cell.
          IF lv_exact EQ abap_true.
            lv_exact = CAST lcl_lisp_number( cell )->exact.
          ENDIF.

          CASE cell->type.
            WHEN type_integer.
              _to_integer cell exp1.
              result = lcl_lisp_new=>real_number( value = ipow( base = base1  exp = exp1 )
                                                  iv_exact = lv_exact ).
            WHEN type_real.
              lo_real = CAST lcl_lisp_real( cell ).
              IF lo_real->infnan EQ abap_false.
                exp1 = lo_real->real.
                result = lcl_lisp_new=>real_number( value = base1 ** exp1
                                                    iv_exact = lv_exact ).
              ELSE.
                result = lo_real.
              ENDIF.
            WHEN type_rational.
              lo_rat ?= cell.
              exp1 = lo_rat->int / lo_rat->denominator.
              result = lcl_lisp_new=>real_number( value = base1 ** exp1
                                                  iv_exact = lv_exact ).
            WHEN type_complex.
              cell->raise( '[expt] not implemented yet for complex numbers'  ).

            WHEN OTHERS.
              cell->raise_nan( |[expt]| ) ##NO_TEXT.
          ENDCASE.

          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_expt

    METHOD proc_exp.
      _trigonometric_header '[exp]'.
          IF y = 0.
            result = lcl_lisp_new=>real_number( value = exp( x )
                                                iv_exact = cell_exact ).
          ELSE.
            DATA siny TYPE f.
            DATA cosy TYPE f.
            DATA exp_real TYPE tv_real.

            siny = sin( b ).
            cosy = cos( b ).
            exp_real = exp( x ).
            result = lcl_lisp_new=>rectangular( real = lcl_lisp_new=>real_number( value = exp_real * cosy
                                                                                  iv_exact = cell_exact )
                                                imag = lcl_lisp_new=>real_number( value = exp_real * siny
                                                                                 iv_exact = cell_exact ) ).
          ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.

    METHOD complex_log.
      DATA arctan TYPE f.
      DATA lv_tan TYPE f.
      DATA magnitude TYPE f.

      IF y = 0.
        IF x = 0.
          ln_z = lcl_lisp_number=>nan.
          RETURN.
        ELSE.
          arctan = c_pi.
          magnitude = abs( x ).
        ENDIF.
      ELSE.
        magnitude = sqrt( x * x  + y * y ).
        lv_tan = y / ( x + magnitude ).
        arctan = 2 * atan( lv_tan ).
      ENDIF.

      ln_z = lcl_lisp_new=>rectangular( real = lcl_lisp_new=>real( value = log( magnitude ) / base
                                                                   iv_exact = abap_false )
                                        imag = lcl_lisp_new=>real( value = arctan / base
                                                                   iv_exact = abap_false ) ).
    ENDMETHOD.

    METHOD proc_log.
      "(log z1 z2)
      " log 0.0  => -inf.0
      " log -0.0 => -inf.0 + pi * i
      DATA carry TYPE tv_real.
      DATA z2 TYPE tv_real.
      DATA base TYPE tv_real VALUE 1.
      _data_local_numeric_cell.

      result = nil.
      _validate list.
      _validate_number list->car 'log'.
      TRY.
          cell = list->car.
          _values_get_next cell.
          DATA(lo_number) = CAST lcl_lisp_number( cell ).

          IF list->cdr IS BOUND AND list->cdr->car IS BOUND.
            cell = list->cdr->car.
            list->cdr->assert_last_param( ).

            _values_get_next cell.
            base = log( CAST lcl_lisp_number( cell )->to_real( ) ).
          ELSE.
            list->assert_last_param( ).
          ENDIF.

          IF lo_number->type EQ type_complex.
            lo_z ?= lo_number.
            result = complex_log( x = lo_z->zreal->to_real( )
                                  y = lo_z->zimag->to_real( )
                                  base = base ).
            RETURN.
          ELSE.
            carry = lo_number->to_real( ).

            IF carry EQ 0.
              result = lcl_lisp_number=>neg_inf.
              RETURN.
            ENDIF.

            IF carry LT 0.
              result = complex_log( x = carry
                                    y = 0
                                    base = base ).
              " base
            ELSE.
              result = lcl_lisp_new=>real_number( value = log( carry ) / base
                                                  iv_exact = abap_false ).
            ENDIF.
          ENDIF.

        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_log

    METHOD complex_sqrt.
      DATA(r) = sqrt( x ** 2 + y ** 2 ).
      DATA(p) = sqrt( ( r + x ) / 2 ).
      DATA(q) = sqrt( ( r - x ) / 2 ).
      IF y LT 0.
        q = - q.
      ENDIF.

      sqrt_z = lcl_lisp_new=>rectangular( real = lcl_lisp_new=>real_number( value = p
                                                                            iv_exact = iv_exact )
                                          imag = lcl_lisp_new=>real_number( value = q
                                                                            iv_exact = iv_exact ) ).
    ENDMETHOD.

    METHOD proc_sqrt.
      _trigonometric_header '[sqrt]'.
          IF y = 0.
            IF x GE 0.
              result = lcl_lisp_new=>real_number( value = sqrt( x )
                                                  iv_exact = cell_exact ).
            ELSE.
              result = lcl_lisp_new=>rectangular( imag = lcl_lisp_new=>real_number( value = sqrt( - x )
                                                                                    iv_exact = cell_exact ) ).
            ENDIF.
          ELSE.
            result = complex_sqrt( x = x
                                   y = y
                                   iv_exact = cell_exact ).
          ENDIF.
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_sqrt

    METHOD proc_square.
      DATA lo_head TYPE REF TO lcl_lisp.
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_number TYPE REF TO lcl_lisp_number.

      result = nil.
      _validate list.
      _validate_number list->car 'square'.
      TRY.
           _assert_one_param 'square'.
           cell ?= list->car.
           _values_get_next cell.
           lo_number ?= cell.

          result = lo_number->square( ).
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_square

    METHOD proc_int_sqrt.
      " procedure (exact-integer-sqrt k) returns two non-negative exact integers s and r where k = s^2 + r and k < (s + 1)^2.
      " (exact-integer-sqrt 4) =) 2 0
      " (exact-integer-sqrt 5) =) 2 1
      " (exact-integer-sqrt 8) =) 2 4
      DATA lv_int TYPE tv_int.
      DATA lv_rest TYPE tv_int.
      _get_int_definition.

      result = nil.
      TRY.
          _validate list->cdr.
          _get_int list lv_int 'exact-integer-sqrt'.

          IF exact EQ abap_false.
            list->car->raise( | is a not an exact integer in [exact-integer-sqrt]| ).
          ENDIF.
          IF lv_int LT 0.
            list->car->raise( | is a negative integer in [exact-integer-sqrt]| ).
          ELSEIF lv_int GT 1.
            lv_int = CAST lcl_lisp_number( list->car )->scheme_round( sqrt( lv_int ) ).
          ENDIF.
          lv_rest = lo_int->int - lv_int * lv_int.
          IF lv_rest LT 0.
            " Adjust
            lv_int = lv_int - 1.
            lv_rest = lo_int->int - lv_int * lv_int.
          ENDIF.

          list->assert_last_param( ).

          result = lcl_lisp_new=>values(
                  )->add( lcl_lisp_new=>integer( value = lv_int
                                                 iv_exact = abap_true )
                  )->add( lcl_lisp_new=>integer( value = lv_rest
                                                 iv_exact = abap_true ) ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.

    METHOD proc_floor_new.
      DATA nr TYPE tv_int.  " remainder

      TRY.
          "_get_2_ints n1 n2 exact list 'floor/'.
          _get_2_ints 'floor/'.
          nq = floor( carry ).
          nr = n1 - n2 * nq.

          DATA(lo_values) = lcl_lisp_new=>values( ).

          lo_values->add( lcl_lisp_new=>integer( value = nq
                                                 iv_exact = exact ) ).

          lo_values->add( lcl_lisp_new=>integer( value = nr
                                                 iv_exact = exact ) ).
          result = lo_values.
        CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD proc_floor_quotient.
      TRY.
          _get_2_ints 'floor-quotient'.
          nq = floor( carry ).
          result = lcl_lisp_new=>integer( value = nq
                                          iv_exact = exact ).
        CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD proc_floor_remainder.
      " (floor-remainder n1 n2) -- equivalent to (modulo n1 n2)
      DATA nr TYPE tv_int.  " remainder

      TRY.
          _get_2_ints 'floor-remainder'.
          nq = floor( carry ).
          nr = n1 - n2 * nq.

          result = lcl_lisp_new=>real_number( value = nr
                                              iv_exact = exact ).
        CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD proc_truncate_new.
      DATA nr TYPE tv_int.  " remainder

      TRY.
          _get_2_ints 'truncate/'.
          nq = trunc( carry ).
          nr = n1 - n2 * nq.

          DATA(lo_values) = lcl_lisp_new=>values( ).
          lo_values->add( lcl_lisp_new=>integer( value = nq
                                                 iv_exact = exact ) ).
          lo_values->add( lcl_lisp_new=>integer( value = nr
                                                 iv_exact = exact ) ).
          result = lo_values.
        CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD proc_trunc_quotient.
      " (truncate-quotient n1 n2) -- equivalent to (quotient n1 n2)
      TRY.
          _get_2_ints 'truncate-quotient'.
          nq = trunc( carry ).
          result = lcl_lisp_new=>integer( value = nq
                                          iv_exact = exact ).
        CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD proc_trunc_remainder.
      " (truncate-remainder n1 n2) -- equivalent to (remainder n1 n2)
      DATA nr TYPE tv_int.  " remainder

      TRY.
          _get_2_ints 'truncate-remainder/'.
          nr = n1 - n2 * nq.     " n1 = n2 * nq + nr.
          result = lcl_lisp_new=>integer( value = nr
                                          iv_exact = exact ).
        CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD simplest_rational_internal.
     " assumes 0 < X < Y
     DATA fx TYPE tv_int.
     DATA fy TYPE tv_int.

     fx = floor( x ).
     fy = floor( y ).

     num-subtype = type_rational.
     num-denom = 1.

      IF fx >= x.
        num-nummer = fx.
      ELSEIF fx EQ fy.
        DATA(help) = simplest_rational_internal( x = 1 / ( y - fy )
                                                 y = 1 / ( x - fx ) ).
        num = help.
        num-nummer = help-denom.
        num-denom = help-nummer.

        num-nummer = fx * num-denom + 1.
      ELSE.
        num-nummer = fx + 1.
      ENDIF.
    ENDMETHOD.

    METHOD simplest_rational.
      DATA res TYPE ts_result.
      DATA is_rational TYPE tv_flag.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      DATA(x) = low->to_real( ).
      DATA(y) = high->to_real( ).
      " do some juggling to satisfy preconditions of simplest-rational-internal.
      IF y < x.
        result = simplest_rational( low = high
                                    high = low
                                    iv_exact = iv_exact ).
      ELSEIF x >= y.
        " is low rational?
        is_rational = abap_false.

        CASE low->type.
        WHEN type_rational
          OR type_integer.
          is_rational = abap_true.
*        WHEN type_real.
*          lo_real ?= low.
*          IF lo_real->infnan EQ abap_false.
*          ENDIF.
        ENDCASE.

        IF is_rational EQ abap_true.
          result = low.
        ELSE.
          throw( 'Error in rationalize' ).
        ENDIF.
      ELSEIF x GE 0.
        res-real_part = simplest_rational_internal( x = x
                                                    y = y ).
        res-type = res-real_part-subtype.
        res-exact = iv_exact.
        result = lcl_lisp_new=>numeric( res ).
      ELSEIF y LE 0.
        res-real_part = simplest_rational_internal( x = - y
                                                    y = - x ).
        res-type = res-real_part-subtype.
        res-exact = iv_exact.
        " result = - result.
        res-real_part = low->negative( res-real_part ).

        result = lcl_lisp_new=>numeric( res ).
      ELSE.
        IF low->exact EQ abap_true AND high->exact EQ abap_true.
          result = lcl_lisp_number=>zero.
        ELSE.
          result = lcl_lisp_number=>pos_zero.
        ENDIF.
      ENDIF.
    ENDMETHOD.

    METHOD proc_rationalize.
      DATA num TYPE REF TO lcl_lisp_number.
      DATA delta TYPE REF TO lcl_lisp_number.

      _validate: list, list->car, list->cdr, list->cdr->car.
      _validate_number list->car `rationalize`.
      _validate_number list->cdr->car `rationalize`.
      num ?= list->car.            " x
      delta ?= list->cdr->car.     " e
" https://small.r7rs.org/wiki/RationalizeDefinition/
" This implementation of rationalize is taken from the IEEE Scheme standard, which is not freely available.
" The code is by Alan Bawden, and the theory is from Hardy and Wright's Introduction to the Theory of Numbers, 5th edition (1979).

      result = simplest_rational( low = num->substract( delta )   " x - e
                                  high = num->add( delta )        " x + e
                                  iv_exact = num->exact ).
    ENDMETHOD.

    METHOD proc_floor.
      _math_to_int floor '[floor]'.
    ENDMETHOD.                    "proc_floor

    METHOD proc_ceiling.
      _math_to_int ceil '[ceil]'.
    ENDMETHOD.                    "proc_ceiling

    METHOD proc_truncate.
      _math_to_int trunc '[truncate]'.
    ENDMETHOD.                    "proc_truncate

    METHOD proc_round.
      DATA carry TYPE tv_real.
      _data_local_numeric_cell.

      result = nil.
      _validate_number list->car '[round]'.
      IF CAST lcl_lisp_number( list->car )->infnan EQ abap_true.
        result = list->car.
        RETURN.
      ENDIF.
      _get_real carry list->car '[round]'.
      _assert_one_param `round`.
      TRY.
          result = lcl_lisp_new=>real_number( value = lcl_lisp_number=>scheme_round( carry )
                                              iv_exact = cell_exact ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_round

    METHOD proc_numerator.
      _get_arg0_as_number `numerator`.

      result = lo_number->get_numerator( ).
    ENDMETHOD.

    METHOD proc_denominator.
      _get_arg0_as_number `denominator`.

      result = lo_number->get_denominator( ).
    ENDMETHOD.

    METHOD proc_remainder.
      DATA numerator TYPE tv_real.
      DATA denominator TYPE tv_real.
      DATA lv_exact TYPE tv_flag.

      result = nil.
      _validate: list, list->cdr.

      _validate_number list->car 'remainder'.
      DATA(num) = CAST lcl_lisp_number( list->car ).
      numerator = num->to_real( 'remainder' ).
      lv_exact = num->exact.

      _validate_number list->cdr->car 'remainder'.
      num = CAST lcl_lisp_number( list->cdr->car ).
      denominator = num->to_real( 'remainder' ).
      IF lv_exact EQ abap_true.
        lv_exact = num->exact.
      ENDIF.
      _assert_last_param list->cdr.

      TRY.
          result = lcl_lisp_new=>real_number( value = numerator - denominator * trunc( numerator / denominator )
                                              iv_exact = lv_exact ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_remainder

    METHOD proc_quotient.
      DATA numerator TYPE tv_real.
      DATA denominator TYPE tv_real.
      DATA lv_exact TYPE tv_flag.

      result = nil.
      _validate: list, list->cdr.

      _validate_number list->car 'quotient'.
      DATA(num) = CAST lcl_lisp_number( list->car ).
      numerator = num->to_real( 'quotient' ).
      lv_exact = num->exact.

      _validate_number list->cdr->car 'quotient'.
      num = CAST lcl_lisp_number( list->cdr->car ).
      denominator = num->to_real( 'quotient' ).
      IF lv_exact EQ abap_true.
        lv_exact = num->exact.
      ENDIF.
      _assert_last_param list->cdr.
      TRY.
          result = lcl_lisp_new=>real_number( value = trunc( numerator / denominator )
                                              iv_exact = lv_exact ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_quotient

    METHOD proc_modulo.
      DATA numerator TYPE tv_real.
      DATA base TYPE tv_real.
      DATA mod TYPE tv_real.
      _data_local_numeric_cell.

      result = nil.
      _validate: list, list->cdr.

      _validate_number list->car 'modulo'.
      DATA(num) = CAST lcl_lisp_number( list->car ).
      numerator = num->to_real( 'modulo' ).
      lv_exact = num->exact.

      _validate_number list->cdr->car 'modulo'.
      num = CAST lcl_lisp_number( list->cdr->car ).
      base = num->to_real( 'modulo' ).
      IF lv_exact EQ abap_true.
        lv_exact = num->exact.
      ENDIF.

      _assert_last_param list->cdr.
      TRY.
          mod = numerator MOD base.
          IF sign( base ) LE 0 AND mod NE 0.
            mod += base.
          ENDIF.
          result = lcl_lisp_new=>real_number( value = mod
                                              iv_exact = lv_exact ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_modulo

    METHOD proc_random.
      DATA lv_high TYPE i.

      result = nil.
      _validate list.
      _validate_integer list->car '[random]'.
      _assert_one_param 'random'.
      TRY.
          DATA(lo_rnd) = cl_abap_random=>create( cl_abap_random=>seed( ) ).
          _to_integer list->car lv_high.
          result = lcl_lisp_new=>real_number( value = lo_rnd->intinrange( high = lv_high )
                                              iv_exact = abap_true ).
        CATCH cx_dynamic_check INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.                    "proc_random

    METHOD proc_is_exact.
      _get_arg0_as_number `exact?`.

      result = false.
      IF lo_number->exact EQ abap_true.
        result = true.
      ENDIF.
    ENDMETHOD.

    METHOD proc_is_inexact.
      _get_arg0_as_number `inexact?`.

      result = false.
      IF lo_number->exact EQ abap_false.
        result = true.
      ENDIF.
    ENDMETHOD.

    METHOD proc_to_exact.
      _get_arg0_as_number `exact`.

      result = CAST lcl_lisp_number( list->car )->to_exact( ).
    ENDMETHOD.

    METHOD proc_to_inexact.
      _get_arg0_as_number `inexact`.

      result = CAST lcl_lisp_number( list->car )->to_inexact( ).
    ENDMETHOD.

    METHOD proc_num_to_string.
      DATA lv_radix TYPE tv_int VALUE 10.

      _validate list.
      _validate_number list->car `number->string`.
      " Optional radix
      _validate list->cdr.
      IF list->cdr NE nil.
        _validate_integer list->cdr->car `number->string`.
        _to_integer list->cdr->car lv_radix.
      ENDIF.

      result = lcl_lisp_new=>string( CAST lcl_lisp_number( list->car )->number_to_string( lv_radix ) ).
    ENDMETHOD.

    METHOD string_to_number.
      result = false.
      CHECK iv_text NE space.
      TRY.
          DATA(li_string_stream) = NEW lcl_string_stream( iv_text ).
          DATA(lo_stream) = NEW lcl_stream( li_string_stream ).
          IF lo_stream->read_number( EXPORTING iv_peek_char = space
                                               iv_radix = iv_radix
                                     IMPORTING element = result ).
            lo_stream->skip_while( lcl_stream=>mv_whitespace ).
            IF li_string_stream->state-ready = abap_true.
              result = false.
            ENDIF.
          ENDIF.
        CATCH lcx_lisp_radix INTO DATA(lx_radix).
          " Repeat: generate error
          RAISE EXCEPTION lx_radix.
        CATCH cx_sy_conversion_error lcx_lisp_exception.
          result = false.
      ENDTRY.
    ENDMETHOD.

    METHOD proc_string_to_num.
      DATA lv_radix TYPE tv_int VALUE 10.

      _validate list.
      _validate_string list->car `string->number`.
*     Optional radix
      _validate list->cdr.
      IF list->cdr NE nil.
        _validate_integer list->cdr->car `string->number`.
        _to_integer list->cdr->car lv_radix.
      ENDIF.

      result = string_to_number( iv_text = list->car->value
                                 iv_radix = lv_radix ).
    ENDMETHOD.

    METHOD proc_newline.
      result = write( io_elem = lcl_lisp=>new_line
                      io_arg = list ).
    ENDMETHOD.

    METHOD proc_write.
      _validate: list, list->car.
      result = write( io_elem = list->car
                      io_arg = list->cdr ).
    ENDMETHOD.

    METHOD proc_write_string.
      _validate list.
      _validate_string list->car `write-string`.
      result = write( io_elem = list->car
                      io_arg = list->cdr ).
    ENDMETHOD.

    METHOD proc_write_bytevector.
      _validate list.
      _validate_bytevector list->car `write-bytevector`.
      result = write( io_elem = list->car
                      io_arg = list->cdr ).
    ENDMETHOD.

    METHOD proc_write_char.
      _validate list.
      _validate_char list->car `write-char`.
      result = write( io_elem = list->car
                      io_arg = list->cdr ).
    ENDMETHOD.

    METHOD proc_write_u8.
      _validate list.
      _validate_bytevector list->car `write-u8`.
      result = write_u8( io_elem = list->car
                         io_arg = list->cdr ).
    ENDMETHOD.

    METHOD proc_display.
      _validate: list, list->car.
      result = display( io_elem = list->car
                        io_arg = list->cdr ).
    ENDMETHOD.

    METHOD proc_read.
      result = read( io_arg = list ).
    ENDMETHOD.

    METHOD proc_read_char.
      result = read_char( io_arg = list ).
    ENDMETHOD.

    METHOD proc_read_u8.
      result = read_u8( io_arg = list ).
    ENDMETHOD.

    METHOD proc_read_string.
      result = read_string( io_arg = list ).
    ENDMETHOD.

    METHOD proc_read_bytevector.
      result = read_bytevector( io_arg = list ).
    ENDMETHOD.

    METHOD proc_read_line.
      result = read_line( io_arg = list ).
    ENDMETHOD.

    METHOD proc_peek_char.
      _optional_port list input `peek-char`.
      result = lcl_lisp_new=>char( li_port->peek_char( ) ).
    ENDMETHOD.

    METHOD proc_peek_u8.
      _optional_binary_port list input `peek-u8`.
      result = lcl_lisp_new=>char( li_port->peek_u8( ) ).
    ENDMETHOD.

    METHOD proc_is_u8_ready.
      _optional_binary_port list input `u8-ready?`.

      result = false.
      CHECK li_port->is_u8_ready( ).
      result = true.
    ENDMETHOD.

    METHOD proc_is_char_ready.
      _optional_port list input `char-ready?`.

      result = false.
      CHECK li_port->is_char_ready( ).
      result = true.
    ENDMETHOD.

    METHOD proc_string.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      DATA lv_text TYPE string.

      _validate list.
      lo_ptr = list.
      WHILE lo_ptr->type EQ type_pair AND lo_ptr->car->type EQ type_char.
        lv_text &&= lo_ptr->car->value+0(1).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      IF lo_ptr NE nil.
        lo_ptr->raise( | is not a list of char | ).
      ENDIF.

      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_make_string.
      DATA lv_len TYPE tv_index.
      DATA lv_char TYPE tv_char.
      DATA lv_text TYPE string.

      _validate: list.
      _validate_integer list->car 'make-string'.
      _to_integer list->car lv_len.

      IF list->cdr NE nil.
        DATA(lo_char) = list->cdr->car.
        _validate_char lo_char 'make-string'.
        lv_char = lo_char->value+0(1).
      ENDIF.

      DO lv_len TIMES.
        CONCATENATE lv_text lv_char INTO lv_text RESPECTING BLANKS.
      ENDDO.
      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_string_to_list.
      DATA lv_char TYPE tv_char.
      DATA lv_start TYPE tv_index.
      DATA lv_len TYPE tv_index.
      DATA lv_len_1 TYPE tv_index.
      DATA lv_text TYPE string.
      DATA lo_int TYPE REF TO lcl_lisp_integer.

      _validate: list, list->car.

      _validate: list->cdr.
      IF list->cdr NE nil.
        _validate_integer list->cdr->car 'string->list start'.
        _to_integer list->cdr->car lv_start.

        _validate list->cdr->cdr.
        IF list->cdr->cdr NE nil.
          _validate_integer list->cdr->cdr->car 'string->list end'.
          _to_integer list->cdr->cdr->car lv_len.
          lv_len = lv_len - lv_start.
          lv_text = list->car->value+lv_start(lv_len).
        ELSE.
          lv_text = list->car->value+lv_start.
        ENDIF.

      ELSE.
        lv_text = list->car->value.
      ENDIF.

      result = nil.
      CHECK lv_text IS NOT INITIAL.

      lv_char = lv_text+0(1).
      result = lcl_lisp_new=>cons( io_car = lcl_lisp_new=>char( lv_char ) ).
      lv_len_1 = strlen( lv_text ) - 1.
      DATA(lo_ptr) = result.

      DO lv_len_1 TIMES.
        lv_char = lv_text+sy-index.
        lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = lcl_lisp_new=>char( lv_char ) ).
      ENDDO.
    ENDMETHOD.

    METHOD proc_string_length.
      _validate: list.
      _validate_string list->car 'string-length'.

      result = lcl_lisp_new=>integer( strlen( list->car->value ) ).
    ENDMETHOD.

    METHOD proc_string_copy.
      DATA lv_start TYPE tv_index.
      DATA lv_len TYPE tv_index.

      DATA lv_text TYPE string.
      DATA lo_int TYPE REF TO lcl_lisp_integer.

      _validate list.
      _validate_string list->car 'string-copy'.
      _validate list->cdr.

*     lv_text = substring( val = list->car->value off = lv_start len = lv_len ).
      IF list->cdr EQ nil.
        lv_text = list->car->value.
      ELSE.
        _validate_integer list->cdr->car 'string-copy start'.
        _to_integer list->cdr->car lv_start.

        _validate list->cdr->cdr.
        IF list->cdr->cdr NE nil.
          _validate_integer list->cdr->cdr->car 'string-copy end'.
          _to_integer list->cdr->cdr->car lv_len.
          lv_len = lv_len - lv_start.
          lv_text = list->car->value+lv_start(lv_len).
        ELSE.
          lv_text = list->car->value+lv_start.
        ENDIF.
      ENDIF.

      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_string_ref.
      DATA lv_index TYPE tv_index.
      DATA lv_char TYPE tv_char.
      DATA lo_int TYPE REF TO lcl_lisp_integer.

      _validate: list, list->cdr.
      _validate_string list->car 'string-ref'.
      _validate_index list->cdr->car 'string-ref'.

      _to_integer list->cdr->car lv_index.
      lv_char = list->car->value+lv_index(1).

      result = lcl_lisp_new=>char( lv_char ).
    ENDMETHOD.

    METHOD proc_string_set.
      DATA lv_index TYPE tv_index.
      DATA lv_char TYPE tv_char.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_char TYPE REF TO lcl_lisp_char.
      DATA lo_string TYPE REF TO lcl_lisp_string.

      DATA lv_len TYPE tv_index.
      DATA lv_text TYPE string.

      _validate: list, list->cdr, list->cdr->cdr.
      _validate_string list->car 'string-set!'.
      _validate_index list->cdr->car 'string-set!'.
      _validate_char list->cdr->cdr->car 'string-set!'.

      lo_char ?= list->cdr->cdr->car.
      lv_char = lo_char->value(1).

      _to_integer list->cdr->car lv_index.

      lo_string ?= list->car.
      _validate_mutable lo_string `in string-set!`.
*     lo_string->value+lv_index(1) = lv_char.  "Not allowed so split in (left, new char, right)

*     left part
      IF lv_index GT 0.
        lv_len = lv_index.
        lv_text = lo_string->value+0(lv_len).
      ENDIF.
*     compose with new char and right part
      lv_index = lv_index + 1.
      lv_text &&= lv_char && lo_string->value+lv_index.

      lo_string->value = lv_text.
      result = lo_string.
    ENDMETHOD.

    METHOD proc_list_to_string.
      DATA lv_text TYPE string.
      DATA lo_ptr TYPE REF TO lcl_lisp.

      _validate: list, list->car.

      lo_ptr = list->car.
      WHILE lo_ptr->type = type_pair AND lo_ptr->car->type EQ type_char.
        lv_text &&= lo_ptr->car->value+0(1).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      IF lo_ptr NE nil.
        lo_ptr->raise( | is not a list of char| ).
      ENDIF.
      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_symbol_to_string.
      _validate: list, list->car.

      IF list->car->type = type_symbol.
        result = lcl_lisp_new=>string( value = list->car->value
                                       iv_mutable = abap_false ).
      ELSE.
        list->car->raise( | is not a symbol| ).
      ENDIF.
    ENDMETHOD.

    METHOD proc_string_to_symbol.
      _validate: list, list->car.

      IF list->car->type = type_string.
        result = lcl_lisp_new=>symbol( list->car->value ).
      ELSE.
        list->car->raise( | is not a string| ).
      ENDIF.
    ENDMETHOD.

    METHOD proc_string_append.
      DATA lv_text TYPE string.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      _validate list.

      lo_ptr = list.
      WHILE lo_ptr->type = type_pair AND lo_ptr->car->type EQ type_string.
        lv_text &&= lo_ptr->car->value.
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      IF lo_ptr NE nil.
        lo_ptr->car->raise( | is not a string| ).
      ENDIF.
      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_max.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      _data_local_numeric_cell.

      DATA carry TYPE tv_real.
      DATA lv_max TYPE tv_real.

      _validate: list, list->cdr.
      _get_real lv_max list->car '[max]'.
      lv_exact = cell_exact.

      lo_ptr = list->cdr.
      WHILE lo_ptr->type EQ type_pair.
        _get_real carry lo_ptr->car '[max]'.
        IF lv_exact EQ abap_true.
          lv_exact = cell_exact.
        ENDIF.
        lv_max = nmax( val1 = carry val2 = lv_max ).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      result = lcl_lisp_new=>real_number( value = lv_max
                                          iv_exact = lv_exact ).
    ENDMETHOD.

    METHOD proc_min.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      _data_local_numeric_cell.

      DATA carry TYPE tv_real.
      DATA lv_min TYPE tv_real.

      _validate: list, list->cdr.
      _get_real lv_min list->car '[min]'.
      lv_exact = cell_exact.

      lo_ptr = list->cdr.
      WHILE lo_ptr->type EQ type_pair.
        _get_real carry lo_ptr->car '[min]'.
        IF lv_exact EQ abap_true.
          lv_exact = CAST lcl_lisp_number( cell )->exact.
        ENDIF.
        lv_min = nmin( val1 = carry
                       val2 = lv_min ).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      result = lcl_lisp_new=>real_number( value = lv_min
                                          iv_exact = lv_exact ).
    ENDMETHOD.

    METHOD proc_gcd.
*     non-negative greatest common divisor of the arguments
      DATA lo_ptr TYPE REF TO lcl_lisp.
      _data_local_numeric_cell.

      DATA carry TYPE tv_real.
      DATA lv_gcd TYPE tv_real VALUE 0.
      lv_exact = abap_true.

      _validate list.
      IF list NE nil.

        _get_real lv_gcd list->car '[gcd]'.
        lv_exact = cell_exact.

        lo_ptr = list->cdr.
        WHILE lo_ptr->type EQ type_pair.
          _get_real carry lo_ptr->car '[gcd]'.
          IF lv_exact EQ abap_true.
            lv_exact = cell_exact.
          ENDIF.
          lv_gcd = lcl_lisp_rational=>gcd( n = carry
                                           d = lv_gcd ).
          lo_ptr = lo_ptr->cdr.
        ENDWHILE.
      ENDIF.
      result = lcl_lisp_new=>real_number( value = lv_gcd
                                          iv_exact = lv_exact ).
    ENDMETHOD.

    METHOD proc_lcm.
*     non-negative least common multiple of the arguments
      DATA lo_ptr TYPE REF TO lcl_lisp.
      _data_local_numeric_cell.

      DATA carry TYPE tv_real.
      DATA lv_lcm TYPE tv_real VALUE 1.
      lv_exact = abap_true.

      _validate list.
      IF list NE nil.

        _get_real lv_lcm list->car '[lcm]'.
        lv_exact = cell_exact.

        lo_ptr = list->cdr.
        WHILE lo_ptr->type EQ type_pair.
          _get_real carry lo_ptr->car '[lcm]'.
          IF lv_exact EQ abap_true.
            lv_exact = cell_exact.
          ENDIF.
          lv_lcm = lv_lcm * carry / lcl_lisp_rational=>gcd( n = carry d = lv_lcm ).
          lo_ptr = lo_ptr->cdr.
        ENDWHILE.
      ENDIF.
      result = lcl_lisp_new=>real_number( value = abs( lv_lcm )
                                          iv_exact = lv_exact ).
    ENDMETHOD.

    METHOD proc_is_textual_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.

      _validate: list, list->car.
      result = false.
      CHECK list->car->type EQ type_port.
      lo_port ?= list->car.
      CHECK lo_port->port_type EQ textual.
      result = true.
    ENDMETHOD.

    METHOD proc_is_binary_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.

      _validate: list, list->car.
      result = false.
      CHECK list->car->type EQ type_port.
      lo_port ?= list->car.
      CHECK lo_port->port_type EQ binary.
      result = true.
    ENDMETHOD.

    METHOD proc_is_port.
      _validate: list, list->car.
      result = false.
      CHECK list->car->type EQ type_port.
      result = true.
    ENDMETHOD.

    METHOD proc_is_input_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.

      _validate: list, list->car.
      result = false.
      CHECK list->car->type EQ type_port.
      lo_port ?= list->car.
      CHECK lo_port->input EQ abap_true.
      result = true.
    ENDMETHOD.

    METHOD proc_is_output_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.

      _validate: list, list->car.
      result = false.
      CHECK list->car->type EQ type_port.
      lo_port ?= list->car.
      CHECK lo_port->output EQ abap_true.
      result = true.
    ENDMETHOD.

    METHOD proc_is_open_input_port.
      result = false.
      CHECK proc_is_input_port( list ) EQ true.
      result = true.
    ENDMETHOD.

    METHOD proc_is_open_output_port.
      result = false.
      CHECK proc_is_output_port( list ) EQ true.
      result = true.
    ENDMETHOD.

    METHOD proc_current_output_port.
      result = eval( VALUE #( elem = lcl_lisp_new=>cons( io_car = go_output_port )  " no parameters
                              env = env ) ).
    ENDMETHOD.

    METHOD proc_current_input_port.
      result = eval( VALUE #( elem = lcl_lisp_new=>cons( io_car = go_input_port )
                              env = env ) ).
    ENDMETHOD.

    METHOD proc_current_error_port.
      result = eval( VALUE #( elem = lcl_lisp_new=>cons( io_car = go_error_port )
                              env = env ) ).
    ENDMETHOD.

    METHOD proc_close_output_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.
      _validate list.
      _validate_port list->car `close-output-port`.
      lo_port ?= list->car.
      lo_port->close_output( ).
      result = nil.
    ENDMETHOD.

    METHOD proc_close_input_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.
      _validate list.
      _validate_port list->car `close-input-port`.
      lo_port ?= list->car.
      lo_port->close_input( ).
      result = nil.
    ENDMETHOD.

    METHOD proc_close_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.
      _validate list.
      _validate_port list->car `close-port`.
      lo_port ?= list->car.
      lo_port->close( ).
      result = nil.
    ENDMETHOD.

    METHOD proc_open_output_string.
      result = lcl_lisp_new=>port( iv_port_type = textual
                                   iv_output = abap_true
                                   iv_input = abap_false
                                   iv_error = abap_false
                                   iv_buffered = abap_true ).
    ENDMETHOD.

    METHOD proc_open_input_string.
      DATA lo_port TYPE REF TO lcl_lisp_port.
      _validate list.
      _validate_string list->car `open-input-string`.

      lo_port = lcl_lisp_new=>port( iv_port_type = textual
                                    iv_output = abap_false
                                    iv_input = abap_true
                                    iv_error = abap_false
                                    iv_buffered = abap_true   " only buffered input, but currently needed for peek_char
                                    iv_string = abap_true ).
      lo_port->set_input_string( list->car->value ).
      result = lo_port.
    ENDMETHOD.

    METHOD proc_get_output_string.
      DATA lo_port TYPE REF TO lcl_lisp_buffered_port.
      _validate list.
      _validate_port list->car `get-output-string`.

      lo_port ?= list->car.
      result = lcl_lisp_new=>string( lo_port->flush( ) ).
    ENDMETHOD.

    METHOD proc_eof_object.
      result = lcl_lisp_new=>eof( ). " cannot be read using (read), must not be unique
    ENDMETHOD.

    METHOD proc_is_eof_object.
      _validate: list, list->car.

      IF list->car->is_eof( ).
        result = true.
      ELSE.
        result = false.
      ENDIF.
    ENDMETHOD.

    METHOD proc_make_parameter.
* (make-parameter init) procedure
* (make-parameter init converter) procedure
*  Returns a newly allocated parameter object, which is a procedure that accepts zero arguments
*    and returns the value associated with the parameter object. Initially, this value is the
*    value of (converter init), or of init if the conversion procedure converter is not specified.
*  The associated value can be temporarily changed using parameterize, which is described below.
*  The effect of passing arguments to a parameter object is implementation-dependent.

* The following implementation of make-parameter and parameterize is suitable for an implementation with
* no threads. Parameter objects are implemented here as procedures, using two arbitrary unique objects
* <param-set!> and <param-convert>:
*(define (make-parameter init . o)
*  (let* ((converter
*           (if (pair? o) (car o) (lambda (x) x)))
*           (value (converter init)))
*    (lambda args (cond ((null? args)
*                          value)
*                       ((eq? (car args) <param-set!>)
*                         (set! value (cadr args)))
*                       ((eq? (car args) <param-convert>)
*                         converter)
*                       (else (error "bad parameter syntax"))))))

      DATA lo_converter TYPE REF TO lcl_lisp.
      DATA lo_init TYPE REF TO lcl_lisp.
      DATA lo_value TYPE REF TO lcl_lisp.
      DATA lo_env TYPE REF TO lcl_lisp_environment.

      _validate: list, list->car, list->cdr.
      lo_init = list->car.

      DATA(lo_ptr) = list->cdr.
      IF lo_ptr NE nil.
        _validate lo_ptr->car.
        IF lo_ptr->cdr NE nil.
          throw( |unexpected parameter { lo_ptr->cdr->to_string( ) } in make-parameter| ).
        ENDIF.

        lo_converter = lo_ptr->car.
        IF NOT lo_converter->is_procedure( ).
          lo_converter->raise( | is not a procedure in make-parameter| ).
        ENDIF.

        lo_value = eval( VALUE #( elem = lcl_lisp_new=>box( io_proc = lo_converter
                                                            io_elem = lo_init )
                                  env = env ) ).
      ELSE.
        lo_value = lo_init.
      ENDIF.
      result = lcl_lisp_new=>lambda( io_car = nil
                                     io_cdr = lcl_lisp_new=>cons( io_car = lo_value )
                                     io_env = env
                                     iv_parameter_object = abap_true ).
    ENDMETHOD.

    METHOD proc_parameterize.
      result = nil.
    ENDMETHOD.

    METHOD proc_is_char_alphabetic.
      DATA lv_char TYPE tv_char.
      _validate list.
      _validate_char list->car `char-alphabetic?`.
      result = false.
      lv_char = list->car->value.
      CHECK lv_char NE space AND to_upper( lv_char ) CO c_abcde.
      result = true.
    ENDMETHOD.

    METHOD proc_is_char_numeric.
      DATA lv_char TYPE tv_char.

      _validate list.
      _validate_char list->car `char-numeric?`.
      lv_char = list->car->value.

      IF unicode_to_digit( lv_char ) BETWEEN 0 AND 9.
        result = true.
      ELSE.
        result = false.
      ENDIF.
    ENDMETHOD.

    METHOD unicode_to_integer.
      FIELD-SYMBOLS <xword> TYPE x.
      FIELD-SYMBOLS <xint> TYPE x.
      DATA lv_int TYPE int2.

      ASSIGN iv_char TO <xword> CASTING.
      ASSIGN lv_int TO <xint> CASTING.
      <xint> = <xword>.                 " conversion
      rv_int = lv_int.
    ENDMETHOD.

    METHOD unicode_to_digit.
      DATA lv_int TYPE tv_int.
      DATA lv_xdigit TYPE ts_digit-zero.

      DATA ls_digit TYPE ts_digit.
      DATA lv_zero TYPE i.
      DATA lv_index TYPE sytabix.

      rv_digit = -1.
      lv_xdigit = lv_int = unicode_to_integer( iv_char ).

      READ TABLE mt_zero INTO ls_digit WITH TABLE KEY zero = lv_xdigit.
      CASE sy-subrc.
        WHEN 0.

        WHEN 4.
          lv_index = sy-tabix - 1.
          IF lv_index LT 1.
            lv_index = 1.
          ENDIF.
          READ TABLE mt_zero INDEX lv_index INTO ls_digit.
          IF sy-subrc NE 0.
            RETURN.
          ENDIF.

        WHEN OTHERS.
          RETURN.
      ENDCASE.
      lv_zero = ls_digit-zero.
      rv_digit = lv_int - lv_zero.
    ENDMETHOD.

    METHOD proc_digit_value.
      DATA lv_char TYPE tv_char.
      DATA lv_int TYPE tv_int.

      _validate list.
      _validate_char list->car `digit-value`.
      lv_char = list->car->value.

      lv_int = unicode_to_digit( lv_char ).
      IF lv_int BETWEEN 0 AND 9.
        result = lcl_lisp_new=>integer( lv_int ).
      ELSE.
        result = false.
      ENDIF.
    ENDMETHOD.

    METHOD unicode_digit_zero.
      " https://www.unicode.org/versions/Unicode13.0.0/ch04.pdf#G134153
      "SELECT charid FROM tcpucattr INTO TABLE rt_zero
      "  WHERE attr LIKE '%DIGIT ZERO%'.
*     https://www.fileformat.info/info/unicode/category/Nd/list.htm
      rt_zero = VALUE #(
         ( zero = '000030' )       " Default
         ( zero = '000660' )       " ARABIC-INDIC DIGIT ZERO
         ( zero = '0006F0' )       " EXTENDED ARABIC-INDIC DIGIT ZERO
         ( zero = '0007C0' )       " NKO DIGIT ZERO
         ( zero = '000966' )       " DEVANAGARI DIGIT ZERO
         ( zero = '0009E6' )       " BENGALI DIGIT ZERO
         ( zero = '000A66' )       " GURMUKHI DIGIT ZERO
         ( zero = '000AE6' )       " GUJARATI DIGIT ZERO
         ( zero = '000B66' )       " ORIYA DIGIT ZERO
         ( zero = '000BE6' )       " TAMIL DIGIT ZERO
         ( zero = '000C66' )       " TELUGU DIGIT ZERO
         ( zero = '000CE6' )       " KANNADA DIGIT ZERO
         ( zero = '000D66' )       " MALAYALAM DIGIT ZERO
         ( zero = '000DE6' )       " SINHALA LITH DIGIT ZERO
         ( zero = '000E50' )       " THAI DIGIT ZERO
         ( zero = '000ED0' )       " LAO DIGIT ZERO
         ( zero = '000F20' )       " TIBETAN DIGIT ZERO
         ( zero = '001040' )       " MYANMAR DIGIT ZERO
         ( zero = '001090' )       " MYANMAR SHAN DIGIT ZERO
         ( zero = '0017E0' )       " KHMER DIGIT ZERO
         ( zero = '001810' )       " MONGOLIAN DIGIT ZERO
         ( zero = '001946' )       " LIMBU DIGIT ZERO
         ( zero = '0019D0' )       " NEW TAI LUE DIGIT ZERO
         ( zero = '001A80' )       " TAI THAM HORA DIGIT ZERO
         ( zero = '001A90' )       " TAI THAM THAM DIGIT ZERO
         ( zero = '001B50' )       " BALINESE DIGIT ZERO
         ( zero = '001BB0' )       " SUNDANESE DIGIT ZERO
         ( zero = '001C40' )       " LEPCHA DIGIT ZERO
         ( zero = '001C50' )       " OL CHIKI DIGIT ZERO
"          ( zero = '0024EA' )       " CIRCLED DIGIT ZERO
"          ( zero = '0024FF' )       " NEGATIVE CIRCLED DIGIT ZERO
         ( zero = '00A620' )       " VAI DIGIT ZERO
         ( zero = '00A8D0' )       " SAURASHTRA DIGIT ZERO
         ( zero = '00A900' )       " KAYAH LI DIGIT ZERO
         ( zero = '00A9D0' )       " JAVANESE DIGIT ZERO
         ( zero = '00A9F0' )       " MYANMAR TAI LAING DIGIT ZERO
         ( zero = '00AA50' )       " CHAM DIGIT ZERO
         ( zero = '00ABF0' )       " MEETEI MAYEK DIGIT ZERO
         ( zero = '00FF10' )       " FULLWIDTH DIGIT ZERO
         ( zero = '0104A0' )       " OSMANYA DIGIT ZERO
         ( zero = '011066' )       " BRAHMI DIGIT ZERO
         ( zero = '0110F0' )       " SORA SOMPENG DIGIT ZERO
         ( zero = '011136' )       " CHAKMA DIGIT ZERO
         ( zero = '0111D0' )       " SHARADA DIGIT ZERO
         ( zero = '0112F0' )       " KHUDAWADI DIGIT ZERO
         ( zero = '011450' )       " NEWA DIGIT ZERO
         ( zero = '0114D0' )       " TIRHUTA DIGIT ZERO
         ( zero = '011650' )       " MODI DIGIT ZERO
         ( zero = '0116C0' )       " TAKRI DIGIT ZERO
         ( zero = '011730' )       " AHOM DIGIT ZERO
         ( zero = '0118E0' )       " WARANG CITI DIGIT ZERO
         ( zero = '011C50' )       " BHAIKSUKI DIGIT ZERO
         ( zero = '011D50' )       " MASARAM GONDI DIGIT ZERO
         ( zero = '016A60' )       " MRO DIGIT ZERO
         ( zero = '016B50' )       " PAHAWH HMONG DIGIT ZERO
         ( zero = '01D7CE' )       " MATHEMATICAL BOLD DIGIT ZERO
         ( zero = '01D7D8' )       " MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO
         ( zero = '01D7E2' )       " MATHEMATICAL SANS-SERIF DIGIT ZERO
         ( zero = '01D7EC' )       " MATHEMATICAL SANS-SERIF BOLD DIGIT ZERO
         ( zero = '01D7F6' )       " MATHEMATICAL MONOSPACE DIGIT ZERO
         ( zero = '01E950' ) ).    " ADLAM DIGIT ZERO
    ENDMETHOD.

    METHOD proc_is_char_whitespace.
      _validate list.
      _validate_char list->car `char-whitespace?`.

      CASE list->car.
        WHEN lcl_lisp=>char_space OR lcl_lisp=>char_tab OR lcl_lisp=>char_linefeed.
          result = true.
        WHEN OTHERS.
          result = false.
      ENDCASE.
    ENDMETHOD.

    METHOD proc_is_char_upper_case.
      _validate list.
      _validate_char list->car `char-upper-case?`.
      result = false.
      CHECK list->car->value NE to_lower( list->car->value ).
      result = true.
    ENDMETHOD.

    METHOD proc_is_char_lower_case.
      _validate list.
      _validate_char list->car `char-lower-case?`.
      result = false.
      CHECK list->car->value NE to_upper( list->car->value ).
      result = true.
    ENDMETHOD.

    METHOD fold_case.
      rv_string = to_upper( element->value ).
    ENDMETHOD.

    METHOD char_case_identity.
      rv_string = element->value.
    ENDMETHOD.

    METHOD char_to_integer.
      DATA lv_char TYPE tv_char.

      lv_char = io_char->value+0(1).
      rv_int = unicode_to_integer( lv_char ).
    ENDMETHOD.

    METHOD char_fold_case_to_integer.
      DATA lv_char TYPE tv_char.

      lv_char = to_upper( element->value+0(1) ).
      rv_int = unicode_to_integer( lv_char ).
    ENDMETHOD.

    DEFINE _proc_list_compare.
      DATA lo_test TYPE REF TO lcl_lisp.
      DATA lo_arg TYPE REF TO lcl_lisp.
      DATA lv_ref TYPE &4.
      DATA lv_test TYPE &4.

      _validate list.

      result = false.
      lo_arg = list.

      lo_test = nil.
      IF lo_arg->type EQ type_pair AND lo_arg->car->type EQ &5.
        lo_test = lo_arg->car.
        lv_ref = &3( lo_test ).
        lo_arg = lo_arg->cdr.
      ENDIF.
      IF lo_test EQ nil OR lo_arg EQ nil.
        throw( |{ &1 } missing argument| ).
      ENDIF.

      WHILE lo_arg->type EQ type_pair AND lo_arg->car->type EQ &5.
        lv_test = &3( lo_arg->car ).
        IF lv_ref &2 lv_test.
          lv_ref = lv_test.
        ELSE.
          RETURN.
        ENDIF.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

      IF lo_arg NE nil.
        throw( |{ &1 } wrong argument in { lo_arg->car->to_string( ) }| ).
      ENDIF.
      CHECK lo_arg = nil.
      result = true.
    END-OF-DEFINITION.

    DEFINE _proc_string_list_compare.
      _proc_list_compare &1 &2 char_case_identity string type_string.
    END-OF-DEFINITION.

    DEFINE _proc_string_ci_list_compare.
      _proc_list_compare &1 &2 fold_case string type_string.
    END-OF-DEFINITION.

    DEFINE _proc_char_list_compare.
      _proc_list_compare &1 &2 char_to_integer tv_int type_char.
    END-OF-DEFINITION.

    DEFINE _proc_char_ci_list_compare.
      _proc_list_compare &1 &2 char_fold_case_to_integer tv_int type_char.
    END-OF-DEFINITION.

*----- Char
    METHOD proc_char_list_is_eq.
      _proc_char_list_compare `char=?` =.
    ENDMETHOD.

    METHOD proc_char_list_is_lt.
      _proc_char_list_compare `char<?` <.
    ENDMETHOD.

    METHOD proc_char_list_is_gt.
      _proc_char_list_compare `char>?` >.
    ENDMETHOD.

    METHOD proc_char_list_is_le.
      _proc_char_list_compare `char<=?` <=.
    ENDMETHOD.

    METHOD proc_char_list_is_ge.
      _proc_char_list_compare `char>=?` >=.
    ENDMETHOD.

    METHOD proc_char_ci_list_is_eq.
      _proc_char_ci_list_compare `char-ci=?` =.
    ENDMETHOD.

    METHOD proc_char_ci_list_is_lt.
      _proc_char_ci_list_compare `char-ci<?` <.
    ENDMETHOD.

    METHOD proc_char_ci_list_is_gt.
      _proc_char_ci_list_compare `char-ci>?` >.
    ENDMETHOD.

    METHOD proc_char_ci_list_is_le.
      _proc_char_ci_list_compare `char-ci<=?` <=.
    ENDMETHOD.

    METHOD proc_char_ci_list_is_ge.
      _proc_char_ci_list_compare `char-ci>=?` >=.
    ENDMETHOD.

*----- String
    METHOD proc_string_list_is_eq.
      _proc_string_list_compare `string=?` =.
    ENDMETHOD.

    METHOD proc_string_list_is_lt.
      _proc_string_list_compare `string<?` <.
    ENDMETHOD.

    METHOD proc_string_list_is_gt.
      _proc_string_list_compare `string>?` >.
    ENDMETHOD.

    METHOD proc_string_list_is_le.
      _proc_string_list_compare `string<=?` <=.
    ENDMETHOD.

    METHOD proc_string_list_is_ge.
      _proc_string_list_compare `string>=?` >=.
    ENDMETHOD.

*----- String CI

    METHOD proc_string_ci_list_is_eq.
      _proc_string_ci_list_compare `string-ci=?` =.
    ENDMETHOD.

    METHOD proc_string_ci_list_is_lt.
      _proc_string_ci_list_compare `string-ci<?` <.
    ENDMETHOD.

    METHOD proc_string_ci_list_is_gt.
      _proc_string_ci_list_compare `string-ci>?` >.
    ENDMETHOD.

    METHOD proc_string_ci_list_is_le.
      _proc_string_ci_list_compare `string-ci<=?` <=.
    ENDMETHOD.

    METHOD proc_string_ci_list_is_ge.
      _proc_string_ci_list_compare `string-ci>=?` >=.
    ENDMETHOD.

*--- End string

    METHOD proc_char_to_integer.
      _validate list.
      _validate_char list->car `char->integer`.
      result = lcl_lisp_new=>integer( char_to_integer( list->car ) ).
    ENDMETHOD.

    METHOD proc_integer_to_char.
      DATA lv_char TYPE tv_char.
      FIELD-SYMBOLS <xchar> TYPE x.
      FIELD-SYMBOLS <xint> TYPE x.
      DATA lv_int TYPE int2.
      DATA lo_int TYPE REF TO lcl_lisp_integer.

      _validate list.
      _validate_integer list->car `integer->char`.
      _to_integer list->car lv_int.

      ASSIGN lv_int TO <xint> CASTING.
      ASSIGN lv_char TO <xchar> CASTING.
      <xchar> = <xint>.

      result = lcl_lisp_new=>char( lv_char ).
    ENDMETHOD.

    METHOD proc_char_upcase.
      _validate list.
      _validate_char list->car `char-upcase`.
      result = lcl_lisp_new=>char( to_upper( list->car->value ) ).
    ENDMETHOD.

    METHOD proc_char_downcase.
      _validate list.
      _validate_char list->car `char-downcase`.
      result = lcl_lisp_new=>char( to_lower( list->car->value ) ).
    ENDMETHOD.

    METHOD proc_error.
      " (error message obj ...)         message should be a string.
      " Raises an exception as if by calling raise on a newly allocated implementation-defined object
      " which encapsulates the information provided by message, as well as any objs, known as the irritants.
      " The procedure error-object? must return #t on such objects.
      _validate list.
      list->raise( `` ).
    ENDMETHOD.

    METHOD proc_is_error_object.
      _validate list.
      _validate list->car.
      throw( 'error-object? is not implemented yet' ).
    ENDMETHOD.

    METHOD proc_is_read_error.
      _validate list.
      _validate list->car.
      throw( 'read-error? is not implemented yet' ).
    ENDMETHOD.

    METHOD proc_is_file_error.
      _validate list.
      _validate list->car.
      throw( 'file-error? is not implemented yet' ).
    ENDMETHOD.

    METHOD proc_error_object_message.
      _validate list.
      _validate list->car.
      throw( 'error-object-message is not implemented yet' ).
    ENDMETHOD.

    METHOD proc_error_object_irritants.
      _validate list.
      _validate list->car.
      throw( 'error-object-irritants is not implemented yet' ).
    ENDMETHOD.

    METHOD proc_raise.
      _validate list.
      _validate list->car.
      list->car->raise( `` ).
    ENDMETHOD.

    METHOD proc_call_cc.
* A continuation denotes a suspended computation that is awaiting a value

* http://www.ccs.neu.edu/home/dherman/browse/projects/vm/implementation-strategies2.pdf
* Implementation Strategies for First-Class Continuations
* by William D. Clinger Anne H. Hartheimer Eric M. Ost
* Higher-Order and Symbolic Computation. April 1999, Volume 12, Issue 1, pp 7–45
*(define (call-with-current-continuation f)
*  (let ((k (creg-get)))
*    (f (lambda (v)
*      (creg-set! k)
*      v))))

*implementation in terms of low-level procedures creg-get and creg-set!
* A)creg-get - capture procedure - converts the implicit continuation passed to
*             call-with-current-continuation into some kind of Scheme object with unlimited extent
*B) creg-set! - throw procedure - takes such an object and installs it as the continuation
*      for the currently executing procedure overriding the previous implicit continuation.
* f is called an escape procedure, because a call to the escape procedure will allow control
*   to bypass the implicit continuation.

*simplest implementation strategy: allocate storage for each continuation frame (activation record)
*  on a heap and reclaim that storage through garbage collection or reference counting [23].
*  With this strategy, which we call the gc strategy, creg-get can just return the contents of a
*  continuation register (which is often called the dynamic link, stack pointer, or frame pointer),
*  and creg-set! can just store its argument into that register.
      DATA lo_esc TYPE REF TO lcl_lisp_escape.

      _validate_escape list `call/cc`.
      lo_esc ?= list.

      _validate: list, list->car, list->car->cdr.


      result = nil.
    ENDMETHOD.

    METHOD proc_dynamic_wind.
      throw( `dynamic-wind not implemented yet` ).
    ENDMETHOD.

    METHOD proc_call_with_values.
      _validate: list, list->cdr.
      _validate list->car.       " producer
      _validate list->cdr->car.  " consumer
      throw( `call-with-values not implemented yet` ).
    ENDMETHOD.

**********************************************************************
*       _                   _           _ _ _        _
*  __ _| |__   __ _ _ __   | |__  _   _(_) | |_     (_)_ __  ___
* / _` | '_ \ / _` | '_ \  | '_ \| | | | | | __|____| | '_ \/ __|
*| (_| | |_) | (_| | |_) | | |_) | |_| | | | ||_____| | | | \__ \
* \__,_|_.__/ \__,_| .__/  |_.__/ \__,_|_|_|\__|    |_|_| |_|___/
*                  |_|
**********************************************************************

    METHOD proc_abap_data.
      DATA lo_result TYPE REF TO lcl_lisp_data.
      _validate: list, list->car.

      IF list->car = nil OR ( list->car->type NE type_string
                            AND list->car->type NE type_symbol ).
        throw( |ab-data: String or symbol required as name of type| ).
      ENDIF.

      cl_abap_typedescr=>describe_by_name( EXPORTING p_name = list->car->value
                                           RECEIVING p_descr_ref = DATA(lr_desc)
                                           EXCEPTIONS OTHERS = 1 ).
      IF sy-subrc NE 0.
        throw( |ab-data: type { list->car->value } not found | ).
      ENDIF.

      CASE lr_desc->kind.
        WHEN cl_abap_typedescr=>kind_table.
          result = lcl_lisp_new=>table( ).
        WHEN cl_abap_typedescr=>kind_elem OR cl_abap_typedescr=>kind_struct.
          result = lcl_lisp_new=>data( ).
        WHEN OTHERS.
          throw( |ab-data: type kind { lr_desc->kind } not supported yet| ).
      ENDCASE.
*     Create data as given type
      lo_result ?= result.
      CREATE DATA lo_result->data TYPE (list->car->value).
*     Set value if supplied as second parameter
      IF list->cdr NE nil.
        FIELD-SYMBOLS <data> TYPE any.
        ASSIGN lo_result->data->* TO <data>.
        element_to_data(
          EXPORTING
            element = list->cdr->car
          CHANGING
            data    = <data> ).
      ENDIF.
    ENDMETHOD.                    "proc_abap_data

**********************************************************************
    METHOD proc_abap_function.
      result = lcl_lisp_new=>function( list ).
    ENDMETHOD.                    "proc_abap_function

    METHOD proc_abap_function_param.
      result = lcl_lisp=>nil.
    ENDMETHOD.

    METHOD proc_abap_table. "Create a table data
      DATA lo_result TYPE REF TO lcl_lisp_table.
      _validate: list, list->car.
*     First input: name of data type, second input: value
      result = lo_result = lcl_lisp_new=>table( ).
      CREATE DATA lo_result->data TYPE TABLE OF (list->car->value).
*     Set value if supplied as second parameter
      IF list->cdr NE nil.
        FIELD-SYMBOLS <data> TYPE any.
        ASSIGN lo_result->data->* TO <data>.
        element_to_data( EXPORTING element = list->cdr->car
                         CHANGING data    = <data> ).
      ENDIF.
    ENDMETHOD.                    "proc_abap_table

**********************************************************************
    METHOD proc_abap_append_row.
      _validate: list, list->car.
      DATA(lo_ref) = list->car.
      IF lo_ref->type NE type_abap_table.
        throw( |ab-append-row requires ABAP table as parameter| ).
      ENDIF.
      throw( `ab-append-row not implemented yet` ).
    ENDMETHOD.                    "proc_abap_append_row

    METHOD proc_abap_delete_row.
      _validate: list, list->car.
      DATA(lo_ref) = list->car.
      IF lo_ref->type NE type_abap_table.
        throw( |ab-delete-row requires ABAP table as parameter| ).
      ENDIF.
      throw( `ab-delete-row not implemented yet` ).
    ENDMETHOD.                    "proc_abap_delete_row

    METHOD proc_abap_get_row.
      _validate: list, list->car.
      DATA(lo_ref) = list->car.
      IF lo_ref->type NE type_abap_table.
        throw( |ab-get-row requires ABAP table as parameter| ).
      ENDIF.
      throw( `ab-get-row not implemented yet` ).
    ENDMETHOD.                    "proc_abap_get_row

**********************************************************************
    METHOD proc_abap_get_value. "Convert ABAP to Lisp data
      DATA lo_ref TYPE REF TO lcl_lisp_data.
      FIELD-SYMBOLS <data> TYPE any.

      _validate: list, list->car.
      IF list->car->type NE type_abap_data AND
         list->car->type NE type_abap_table.
        throw( |ab-get-value requires ABAP data or table as parameter| ).
      ENDIF.
      lo_ref ?= list->car.
      TRY.
          ASSIGN lo_ref->data->* TO <data>.
          result = data_to_element( <data> ).
        CATCH cx_root INTO DATA(lx_root).
          throw( |Mapping error: { lx_root->get_text( ) }| ).
      ENDTRY.
    ENDMETHOD.                    "proc_abap_get_value

    METHOD proc_abap_set_value. "Convert Lisp to ABAP data
      DATA lo_ref TYPE REF TO lcl_lisp_data.
      FIELD-SYMBOLS <data> TYPE any.

      _validate: list, list->car.
      IF list->car->type NE type_abap_data AND
         list->car->type NE type_abap_table.
        throw( |ab-set-value requires ABAP data or table as first parameter| ).
      ENDIF.
      lo_ref ?= list->car.
      TRY.
          ASSIGN lo_ref->data->* TO <data>.
          element_to_data(
            EXPORTING
              element = list->cdr->car
            CHANGING
              data    = <data> ).
        CATCH cx_root INTO DATA(lx_root).
          throw( |Mapping error: { lx_root->get_text( ) }| ).
      ENDTRY.
      result = nil. "TODO: What should we return here?
    ENDMETHOD.                    "proc_abap_set_value

    METHOD proc_abap_function_call. "Called internally only for execution of function module
      DATA lo_func TYPE REF TO lcl_lisp_abapfunction.
      _validate: list, list->car.
*     The first parameter must be a function module instance
      IF list->car->type NE type_abap_function.
        throw( |{ list->car->value } is not a function module reference| ).
      ENDIF.

      TRY.
          lo_func ?= list->car.
          result = lo_func->call( list->car ).
        CATCH cx_root INTO DATA(lx_root).
          throw( |Function call error: { lx_root->get_text( ) }| ).
      ENDTRY.
    ENDMETHOD.                    "proc_abap_function_call

    METHOD create_element_from_data.
*     Perform RTTI on determined data and generate appropriate response
      DATA(lv_kind) = cl_abap_typedescr=>describe_by_data_ref( ir_data )->kind.
      CASE lv_kind.
        WHEN cl_abap_typedescr=>kind_table.
          result = lcl_lisp_new=>table( ir_data ).
        WHEN cl_abap_typedescr=>kind_struct.
          result = lcl_lisp_new=>data( ir_data ).
        WHEN cl_abap_typedescr=>kind_elem.
*         Give back immediate value
          FIELD-SYMBOLS <value> TYPE any.

          ASSIGN ir_data->* TO <value>.
          result = data_to_element( <value> ).
        WHEN OTHERS.
          throw( |ab-get: type kind { lv_kind } not supported yet| ). "Can do AB-TAB-WHERE some other time
      ENDCASE.
    ENDMETHOD.                    "create_element_from_data

    METHOD proc_abap_get.
      DATA lo_ref TYPE REF TO lcl_lisp_data.
      _validate: list, list->car.

*     Ensure a valid first parameter is passed
      IF list->car->type NE type_abap_data
        AND list->car->type NE type_abap_function
        AND list->car->type NE type_abap_table.
        throw( |ab-get: First parameter must be ABAP data or table or a function| ).
      ENDIF.
      lo_ref ?= list->car.

*     Determine whether the data is elementary or not to decide if we need to get the element by identifier
      IF lo_ref->data IS NOT INITIAL AND
        cl_abap_typedescr=>describe_by_data_ref( lo_ref->data )->kind = cl_abap_typedescr=>kind_elem.
*       Elementary type; can return the value without mapping
        DATA(lr_data) = lo_ref->data.
      ELSEIF list->cdr = nil.
*       Could short-cut here and provide the value right away
        throw( |ab-get: Complex type requires identifier for lookup| ).
      ELSE.
        lr_data = get_element( list ).
      ENDIF.

      result = create_element_from_data( lr_data ).

    ENDMETHOD. "proc_abap_get

    METHOD proc_abap_set.
      DATA lo_ref TYPE REF TO lcl_lisp_data.
      DATA lo_source TYPE REF TO lcl_lisp.
      DATA lr_target TYPE REF TO data.
      FIELD-SYMBOLS <target> TYPE any.
      FIELD-SYMBOLS <source> TYPE any.

      _validate: list, list->car.

*     Ensure a valid first parameter is passed
      IF list->car->type NE type_abap_data
        AND list->car->type NE type_abap_function
        AND list->car->type NE type_abap_table.
        throw( |ab-set: First parameter must be ABAP data or table or a function| ).
      ENDIF.
      lo_ref ?= list->car.

*     Determine whether the data is elementary or not to decide if we need to get the element by identifier
      IF lo_ref->data IS NOT INITIAL AND
        cl_abap_typedescr=>describe_by_data_ref( lo_ref->data )->kind = cl_abap_typedescr=>kind_elem.
*       Elementary type; can return the value without mapping
        lr_target = lo_ref->data.
        lo_source = list->cdr->car.  "Value to set is data ref from second argument
      ELSEIF list->cdr = nil.
        throw( |ab-set: Complex type requires identifier for lookup| ).
      ELSE.
        lr_target = get_element( list ).
        lo_source = list->cdr->cdr->car.  "Value to set is data ref from third argument
      ENDIF.

*     Do we just assign the reference now?
*     Probably should dereference source value and copy the value...
*     Perform RTTI on determined data and generate appropriate response
      ASSIGN lr_target->* TO <target>.

*     For elementary types, set value from second parameter, otherwise third
      IF cl_abap_typedescr=>describe_by_data( <target> )->kind = cl_abap_typedescr=>kind_elem.
*       For now, we will support setting data from a number, string or symbol
        CASE lo_source->type.
          WHEN type_string OR type_symbol.
            <target> = lo_source->value.
          WHEN type_integer.
            _to_integer lo_source <target>.
          WHEN type_real.
            _to_real lo_source <target>.
        ENDCASE.
      ELSE.
*       Complex types will just copy the whole value across
        lo_ref ?= lo_source.
        ASSIGN lo_ref->data->* TO <source>.
        <target> = <source>.                        "Set the value
      ENDIF.

      result = nil.

    ENDMETHOD. "proc_abap_set

    METHOD structure_to_element.
      FIELD-SYMBOLS <field> TYPE any.

      element = nil.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE struct TO <field>.
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.
        IF sy-index EQ 1.
          element = lcl_lisp_new=>cons( io_car = data_to_element( <field> ) ).
          DATA(lo_ptr) = element.
        ELSE.          "Move pointer only from second field onward
          lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = data_to_element( <field> ) ).
        ENDIF.
      ENDDO.
    ENDMETHOD.                    "structure_to_element

    METHOD table_to_element.
*     map ABAP Data to Lisp element
      FIELD-SYMBOLS <table> TYPE ANY TABLE.          " ABAP-side (source)
      DATA line TYPE REF TO data.
*     Table type
      FIELD-SYMBOLS <line> TYPE any.

      ASSIGN data TO <table>.
      CREATE DATA line LIKE LINE OF <table>.
      ASSIGN line->* TO <line>.

      element = nil.
*     Create list with cell for each row AND Set pointer to start of list
      LOOP AT <table> INTO <line>.
        IF sy-tabix EQ 1.
          element = lcl_lisp_new=>cons( io_car = data_to_element( <line> ) ). "recursive call
          DATA(lo_ptr) = element.
        ELSE.   "Move pointer only from second line onward
          lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = data_to_element( <line> ) ).
        ENDIF.
      ENDLOOP.
    ENDMETHOD.

    METHOD data_to_element.
*     Determine type of the ABAP value
      DATA(lr_ddesc) = cl_abap_typedescr=>describe_by_data( data ).
      CASE lr_ddesc->kind.

        WHEN cl_abap_typedescr=>kind_table.
          element = table_to_element( data ).

        WHEN cl_abap_typedescr=>kind_struct.
          element = structure_to_element( data ).

        WHEN cl_abap_typedescr=>kind_elem.
*         Elementary type
          element = SWITCH #( lr_ddesc->type_kind
                       WHEN cl_abap_typedescr=>typekind_numeric OR cl_abap_typedescr=>typekind_num
                       THEN lcl_lisp_new=>real_number( data )
                       ELSE lcl_lisp_new=>string( data ) ).
      ENDCASE.
    ENDMETHOD.                    "data_to_element

*   Map Lisp element to ABAP Data
    METHOD element_to_data.
*     ABAP-side (target) mapping:
      FIELD-SYMBOLS <field> TYPE any.
      FIELD-SYMBOLS <line> TYPE any.
      FIELD-SYMBOLS <table> TYPE ANY TABLE.
      FIELD-SYMBOLS <sotab> TYPE SORTED TABLE.
      FIELD-SYMBOLS <sttab> TYPE STANDARD TABLE.

      DATA line TYPE REF TO data.
      DATA table TYPE REF TO data.

*     Determine type of the ABAP value
      DATA(lr_ddesc) = cl_abap_typedescr=>describe_by_data( data ).
      CASE lr_ddesc->kind.
*       Table type
        WHEN cl_abap_typedescr=>kind_table.
*         For this mapping to happen, the element must be a cons cell
          IF element->type NE type_pair.
            throw( |Mapping failed: Non-cell to table| ).
          ENDIF.
*         Provide reference to table and line
          table = REF #( data ).
          ASSIGN table->* TO <table>.
          CASE CAST cl_abap_tabledescr( lr_ddesc )->table_kind.
            WHEN cl_abap_tabledescr=>tablekind_sorted OR cl_abap_tabledescr=>tablekind_hashed.
              ASSIGN table->* TO <sotab>. "Sorted table type
              CREATE DATA line LIKE LINE OF <sotab>.
            WHEN OTHERS.
              ASSIGN table->* TO <sttab>. "Standard table type
              CREATE DATA line LIKE LINE OF <sttab>.
          ENDCASE.
          ASSIGN line->* TO <line>.

          DATA(lr_conscell) = element. "Set pointer to start of list
          WHILE lr_conscell NE nil.
            element_to_data( EXPORTING element = lr_conscell->car
                             CHANGING data    = <line> ).
*            Append or insert, depending on table type (what is assigned)
            IF <sotab> IS ASSIGNED.
              INSERT <line> INTO TABLE <sotab>.
            ELSE.
              APPEND <line> TO <sttab>.
            ENDIF.
            CLEAR <line>.
            lr_conscell = lr_conscell->cdr.
          ENDWHILE.

        WHEN cl_abap_typedescr=>kind_struct.
*         Structure
          IF element->type NE type_pair.
            throw( |Mapping failed: Non-cell to structure| ).
          ENDIF.

          lr_conscell = element. "Set pointer to start of list
          ASSIGN data TO <line>.
          DO.
            ASSIGN COMPONENT sy-index OF STRUCTURE <line> TO <field>.
            IF sy-subrc NE 0.
              EXIT.
            ENDIF.

            IF sy-index > 1. "Move cons cell pointer only from second element on
              lr_conscell = lr_conscell->cdr.
            ENDIF.
*           Don't map nil values
            CHECK lr_conscell->car NE nil.

            element_to_data( EXPORTING element = lr_conscell->car
                             CHANGING data    = <field> ).
          ENDDO.

        WHEN cl_abap_typedescr=>kind_elem.
*         Elementary type
          ASSIGN data TO <field>.
          CASE element->type.
            WHEN type_integer.
              _to_integer element <field>.
            WHEN type_real.
              _to_real element <field>.
            WHEN OTHERS.
              <field> = element->value.
          ENDCASE.
        WHEN OTHERS.
*          Not supported yet
          throw( |Mapping failed: unsupported type| ).
      ENDCASE.
    ENDMETHOD.                    "element_to_data

    METHOD get_structure_field.
      FIELD-SYMBOLS <value> TYPE any.
      FIELD-SYMBOLS <struct> TYPE any.

      IF identifier = nil OR
        ( identifier->type NE type_string AND identifier->type NE type_symbol ).
        throw( `ab-get: String or symbol required to access structure field` ).
      ENDIF.

      ASSIGN element->data->* TO <struct>.
      ASSIGN COMPONENT identifier->value OF STRUCTURE <struct> TO <value>.
      IF sy-subrc NE 0.
        throw( |ab-get: Structure has no component { identifier->value }| ).
      ENDIF.
      rdata = REF #( <value> ).
    ENDMETHOD.                    "get_structure_field

    METHOD get_index_table_row.
*     Second input for reading an index table must be a number (row index)
      FIELD-SYMBOLS <idxtab> TYPE INDEX TABLE.
      DATA lo_int TYPE REF TO lcl_lisp_integer.

      _validate_integer identifier `ab-get`.
      ASSIGN element->data->* TO <idxtab>.

      lo_int ?= identifier.
      READ TABLE <idxtab> REFERENCE INTO rdata INDEX lo_int->int.
      IF sy-subrc NE 0.
        throw( |ab-get: No entry at index { lo_int->int }| ). "Can do AB-TAB-WHERE some other time
      ENDIF.
    ENDMETHOD.                    "get_index_table_row

    METHOD get_table_row_with_key.
*      Read with key, which is a bit more effort
*      FIELD-SYMBOLS <wa> TYPE any.
*      FIELD-SYMBOLS <tab> TYPE table.
*      DATA line TYPE REF TO data.


      rdata = get_index_table_row( element = element
                                   identifier = identifier ).
*       IF identifier = nil.
*         throw( |AB-GET: Key required to read table| ).
*       ENDIF.

*       ASSIGN element->data->* TO <tab>.
*       CREATE DATA line LIKE LINE OF <tab>.
*       ASSIGN line->* TO <wa>.
*
*       READ TABLE <tab> FROM <wa> REFERENCE INTO rdata
*  example:
*        WITH TABLE KEY ('CARRID') = 'SQ'
*                       ('CONNID') = '0002'
*                       ('FLDATE') = '20170915'
*                       ('BOOKID') = '00000002'.
*
*       CHECK sy-subrc NE 0.
*       throw( |AB-GET: No entry at key| ).
    ENDMETHOD.                    "get_table_row_with_key

    METHOD get_element.
      DATA element TYPE REF TO lcl_lisp_data.
*     RDATA <- Data reference to value pointed to
      element ?= list->car.         " Lisp element containing an ABAP value (data, table or function)
      DATA(identifier) = list->cdr->car. " Lisp element, string or symbol or index, to identify subcomponent of value

      IF element->type = type_abap_function.
*       Get function parameter by name
        rdata = CAST lcl_lisp_abapfunction( element )->get_function_parameter( identifier ).
      ELSE.
*       First parameter is not function, but table or other data; examine the data
        DATA(lo_ddesc) = cl_abap_typedescr=>describe_by_data_ref( element->data ).

        CASE lo_ddesc->kind.
          WHEN cl_abap_typedescr=>kind_struct.
*           Structure: Use second parameter as field name
            rdata = get_structure_field( element = element
                                         identifier = identifier ).
          WHEN cl_abap_typedescr=>kind_elem.
*           Elementary data: No qualifier / second parameter required
            rdata = element->data.

          WHEN cl_abap_typedescr=>kind_table.
*           Table: Second parameter is index (std table) or key (sorted table)
            CASE CAST cl_abap_tabledescr( lo_ddesc )->table_kind.
              WHEN cl_abap_tabledescr=>tablekind_sorted
                OR cl_abap_tabledescr=>tablekind_hashed.
                rdata = get_table_row_with_key( element = element
                                                identifier = identifier ).
              WHEN cl_abap_tabledescr=>tablekind_std.
                "OR cl_abap_tabledescr=>tablekind_index.  - No Test data for this case yet
                rdata = get_index_table_row( element = element
                                             identifier = identifier ).
            ENDCASE.

        ENDCASE.

      ENDIF.
    ENDMETHOD. "get_element

    METHOD proc_sql_prepare.
*     define-query
      DATA lo_string TYPE REF TO lcl_lisp_string.
      _validate: list, list->car.
      lo_string ?= list->car.

      result = lcl_lisp_new=>query( value = lo_string->value ).
    ENDMETHOD.

    METHOD proc_sql_query.
      DATA lo_sql TYPE REF TO lcl_lisp_query.
      DATA lo_string TYPE REF TO lcl_lisp_string.
*     sql-query
      _validate: list, list->cdr.

      TRY.
          lo_string ?= list->car.
          lo_sql ?= lcl_lisp_new=>query( value = lo_string->value ).
          result = lo_sql->execute( lo_string->value ).
        CATCH cx_sql_exception INTO DATA(lx_error).
          throw( |SQL query { lx_error->get_text( ) }| ).
      ENDTRY.
    ENDMETHOD.

    " Turtle library
    METHOD proc_turtle_new. "turtles
*    (turtles width
*          height
*        [  init-x
*         init-y
*         init-angle])    →   turtles?
*      width : real?
*      height : real?
*      init-x : real? = (/ width 2)
*      init-y : real? = (/ height 2)
*      init-angle : real? = 0
      DATA lo_width TYPE REF TO lcl_lisp_integer.
      DATA lo_next TYPE REF TO lcl_lisp.
      DATA lo_height TYPE REF TO lcl_lisp_integer.
      DATA lo_init_x TYPE REF TO lcl_lisp_integer.
      DATA lo_init_y TYPE REF TO lcl_lisp_integer.
      DATA lo_init_angle TYPE REF TO lcl_lisp_real.
      _data_local_numeric_cell.

      _validate: list, list->cdr.
      _validate_integer list->car `turtles`.
      lo_width ?= list->car.
      _validate_integer list->cdr->car `turtles`.
      lo_height ?= list->cdr->car.

      lo_next = list->cdr->cdr.

      IF lo_next->car IS BOUND.
        IF lo_next->car->type EQ type_integer.
          lo_init_x ?= lo_next->car.

          lo_next = lo_next->cdr.
          IF lo_next->car IS BOUND.
            IF lo_next->car->type EQ type_integer.
              lo_init_y ?= lo_next->car.

              lo_next = lo_next->cdr.
              IF lo_next->car IS BOUND.
                DATA lv_real TYPE tv_real.

                _get_real lv_real lo_next->car `turtles`.
                lo_init_angle = lcl_lisp_new=>real( value = lv_real
                                                    iv_exact = cell_exact ).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lo_init_x IS NOT BOUND.
        lo_init_x = lcl_lisp_new=>integer( lo_width->int DIV 2 ).
      ENDIF.

      IF lo_init_y IS NOT BOUND.
        lo_init_y = lcl_lisp_new=>integer( lo_height->int DIV 2 ).
      ENDIF.

      IF lo_init_angle IS NOT BOUND.
        lo_init_angle = lcl_lisp_new=>real( value = 0
                                            iv_exact = abap_true ).
      ENDIF.

      result = lcl_lisp_new=>turtles( width = lo_width
                                      height = lo_height
                                      init_x = lo_init_x
                                      init_y = lo_init_y
                                      init_angle = lo_init_angle ).
    ENDMETHOD.

    METHOD proc_turtle_merge.
      DATA lo_width TYPE REF TO lcl_lisp_integer.
      DATA lo_height TYPE REF TO lcl_lisp_integer.
      DATA lo_init_x TYPE REF TO lcl_lisp_integer.
      DATA lo_init_y TYPE REF TO lcl_lisp_integer.
      DATA lo_init_angle TYPE REF TO lcl_lisp_real.
      _validate: list, list->cdr.
*    (merge turtles1 turtles2) → turtles?
*      turtles1 : turtles?
*      turtles2 : turtles?
      _validate_turtle list->car `merge`.
      DATA lo_turtle1 TYPE REF TO lcl_lisp_turtle.
      lo_turtle1 ?= list->car.
      _validate_turtle list->cdr->car `merge`.
      DATA lo_turtle2 TYPE REF TO lcl_lisp_turtle.
      lo_turtle2 ?= list->cdr->car.

      DATA(lo_turtles) = lcl_turtle=>compose( VALUE #( ( lo_turtle1->turtle ) ( lo_turtle2->turtle ) ) ).

      lo_width = lcl_lisp_new=>integer( nmax( val1 = lo_turtle1->turtle->width
                                              val2 = lo_turtle2->turtle->width ) ).
      lo_height = lcl_lisp_new=>integer( nmax( val1 = lo_turtle1->turtle->height
                                               val2 = lo_turtle2->turtle->height ) ).

      lo_init_x = lcl_lisp_new=>integer( lo_width->int DIV 2 ).
      lo_init_y = lcl_lisp_new=>integer( lo_height->int DIV 2 ).
      lo_init_angle = lcl_lisp_new=>real( value = 0
                                          iv_exact = abap_true ).
      result = lcl_lisp_new=>turtles( width = lo_width
                                      height = lo_height
                                      init_x = lo_init_x
                                      init_y = lo_init_y
                                      init_angle = lo_init_angle ).
    ENDMETHOD.

    METHOD proc_turtle_exist. "turtles?
      _validate: list, list->car.
      "(turtles? v) → boolean?
      IF list->car->type EQ type_abap_turtle.
        result = true.
      ELSE.
        result = false.
      ENDIF.
    ENDMETHOD.

    METHOD proc_turtle_move. "move
      _validate: list, list->cdr.
*    (move n turtles) → turtles?
*      n : real?  (integer)
*      turtles : turtles?
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.

      _validate_integer list->car `turtles move n`.
      _validate_turtle list->cdr->car `turtles move`.
      lo_turtles = CAST lcl_lisp_turtle( list->cdr->car ).
      result = lo_turtles.

      lo_turtles->turtle->pen_up( ).
      lo_turtles->turtle->forward( CAST lcl_lisp_integer( list->car )->int  ).
    ENDMETHOD.

    METHOD proc_turtle_draw. "draw
      _validate: list, list->cdr.
*    (draw n turtles) → turtles?
*      n : real? (integer)
*      turtles : turtles?
      DATA lo_dist_n TYPE REF TO lcl_lisp_integer.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.

      _validate_integer list->car `turtles draw n`.
      _validate_turtle list->cdr->car `turtles draw`.
      lo_dist_n ?= list->car.
      lo_turtles ?= list->cdr->car.
      result = lo_turtles.

      lo_turtles->turtle->pen_down( ).
      lo_turtles->turtle->forward( lo_dist_n->int ).
    ENDMETHOD.

    METHOD proc_turtle_erase.
      _validate: list, list->cdr.
*    (erase n turtles) → turtles?
*
*      n : real?
*      turtles : turtles?
      DATA lo_dist_n TYPE REF TO lcl_lisp_integer.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.

      _validate_integer list->car `turtles erase n`.
      _validate_turtle list->cdr->car `turtles erase`.
      lo_dist_n ?= list->car.
      lo_turtles ?= list->cdr->car.
      result = lo_turtles.

      lo_turtles->turtle->pen_down( ).
      lo_turtles->turtle->forward( lo_dist_n->int ).
    ENDMETHOD.

    METHOD proc_turtle_move_offset.
      _validate: list, list->cdr, list->cdr->cdr.
*    (move-offset h v turtles) → turtles?
*      h : real? (integer)
*      v : real? (integer)
*      turtles : turtles?
      DATA lo_off_h TYPE REF TO lcl_lisp_integer.
      DATA lo_off_v TYPE REF TO lcl_lisp_integer.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.

      _validate_integer list->car `turtles move-offset h`.
      _validate_integer list->cdr->car `turtles move-offset v`.
      _validate_turtle list->cdr->cdr->car `move-offset`.
      lo_off_h ?= list->car.
      lo_off_v ?= list->cdr->car.
      lo_turtles ?= list->cdr->cdr->car.
      result = lo_turtles.

      lo_turtles->turtle->pen_up( ).
      lo_turtles->turtle->to_offset( delta_x = lo_off_h->int
                                     delta_y = lo_off_v->int ).
    ENDMETHOD.

    METHOD proc_turtle_draw_offset.
      _validate: list, list->cdr, list->cdr->cdr.
*    (draw-offset h v turtles) → turtles?
*      h : real? (integer)
*      v : real? (integer)
*      turtles : turtles?
      DATA lo_off_h TYPE REF TO lcl_lisp_integer.
      DATA lo_off_v TYPE REF TO lcl_lisp_integer.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.

      _validate_integer list->car `turtles draw-offset h`.
      _validate_integer list->cdr->car `turtles move-offset v`.
      _validate_turtle list->cdr->cdr->car `draw-offset`.
      lo_off_h ?= list->car.
      lo_off_v ?= list->cdr->car.
      lo_turtles ?= list->cdr->cdr->car.

      lo_turtles->turtle->pen_down( ).
      lo_turtles->turtle->to_offset( delta_x = lo_off_h->int
                                     delta_y = lo_off_v->int ).
    ENDMETHOD.

    METHOD proc_turtle_erase_offset.
      _validate: list, list->cdr, list->cdr->cdr.
*    (erase-offset n turtles) → turtles?
*      n : real?
*      turtles : turtles?
      DATA lo_off_h TYPE REF TO lcl_lisp_integer.
      DATA lo_off_v TYPE REF TO lcl_lisp_integer.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.

      _validate_integer list->car `turtles erase-offset h`.
      _validate_integer list->cdr->car `turtles erase-offset v`.
      _validate_turtle list->cdr->cdr->car `erase-offset`.
      lo_off_h ?= list->car.
      lo_off_v ?= list->cdr->car.
      lo_turtles ?= list->cdr->cdr->car.

      lo_turtles->turtle->pen_down( ).
      lo_turtles->turtle->to_offset( delta_x = lo_off_h->int
                                     delta_y = lo_off_v->int ).
    ENDMETHOD.

    METHOD proc_turtle_turn_degrees. "turn
      DATA theta TYPE tv_real.
      _data_local_numeric_cell.

      _validate: list, list->cdr.
*      (turn theta turtles) → turtles?
*        theta : real?
*        turtles : turtles?
      _validate_number list->car `turtles turn`.
      _get_real theta list->car `turtles turn`.

      _validate_turtle list->cdr->car `turn`.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.
      lo_turtles ?= list->cdr->car.

      DATA(angle) = ( lo_turtles->turtle->position-angle + theta ) MOD 360.
      lo_turtles->turtle->set_position( VALUE #( x = lo_turtles->turtle->position-x
                                                 y = lo_turtles->turtle->position-y
                                                 angle = angle ) ).
      result = lo_turtles.
    ENDMETHOD.

    METHOD proc_turtle_turn_radians. "turn/radians
      DATA theta TYPE tv_real.
      _data_local_numeric_cell.

      _validate: list, list->cdr.
*      (turn/radians theta turtles) → turtles?
*        theta : real?
*        turtles : turtles?
      _get_real theta list->car `turtles turn/radians theta`.

      _validate_turtle list->cdr->car `turn/radians`.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.
      lo_turtles ?= list->cdr->car.

      DATA(angle) = lcl_turtle_convert=>degrees_to_radians( lo_turtles->turtle->position-angle MOD 360 ) + theta.
      angle = lcl_turtle_convert=>radians_to_degrees( angle ) MOD 360.

      lo_turtles->turtle->set_position( VALUE #( x = lo_turtles->turtle->position-x
                                                 y = lo_turtles->turtle->position-y
                                                 angle = angle ) ).
      result = lo_turtles.
    ENDMETHOD.

    METHOD proc_turtle_set_pen_width. "set-pen-width
      _validate: list, list->cdr.
*      (set-pen-width turtles width) → turtles?
*        turtles : turtles?
*        width : (real-in 0 255)
      _validate_turtle list->car `set-pen-width`.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.
      lo_turtles ?= list->car.

      _validate_index list->cdr->car `set-pen-width`.
      DATA lo_width TYPE REF TO lcl_lisp_integer.
      lo_width ?= list->cdr->car.

      lo_turtles->turtle->set_pen( VALUE #( BASE lo_turtles->turtle->pen
                                            stroke_width = lo_width->int ) ).
      result = lo_turtles.
    ENDMETHOD.

    METHOD proc_turtle_set_pen_color. "set-pen-color
      _validate: list, list->cdr.
*    (set-pen-color turtles color) → turtles?
*      turtles : turtles?
*      color : (or/c string? (is-a?/c color%))
      _validate_turtle list->car `set-pen-color`.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.
      lo_turtles ?= list->car.

      _validate_string list->cdr->car `set-pen-color`.
      DATA lo_color TYPE REF TO lcl_lisp_string.
      lo_color ?= list->cdr->car.

      lo_turtles->turtle->set_pen( VALUE #( BASE lo_turtles->turtle->pen
                                            stroke_color = lo_color->value ) ).
      result = lo_turtles.
    ENDMETHOD.

    METHOD proc_turtle_state.
      _validate list.
*    (turtle-state turtles) → (listof (vector/c real? real? real?)
      _validate_turtle list->car `turtle-state`.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.
      lo_turtles ?= list->car.
      DATA(position) = lo_turtles->turtle->get_position( ).

      result = lcl_lisp_new=>cons( io_car = lcl_lisp_new=>vector(
                  it_vector = VALUE tt_lisp( ( lcl_lisp_new=>integer( position-x ) )
                                             ( lcl_lisp_new=>integer( position-y ) )
                                             ( lcl_lisp_new=>real( value = position-angle
                                                                   iv_exact = abap_false ) )  )
                  iv_mutable = abap_false ) ).
    ENDMETHOD.

    METHOD proc_turtle_clean.
      _validate list.
*    (clean turtles) → turtles?
*      turtles : turtles?
      _validate_turtle list->car `clean`.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.
      lo_turtles ?= list->car.
      throw( `turtle clean not implemented yet` ).
      result = lo_turtles.
    ENDMETHOD.

    METHOD proc_turtle_width.
*    (turtles-width turtles) → (and/c real? positive?)
*      turtles : turtles?
      _validate list.
      _validate_turtle list->car `turtles-width`.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.
      lo_turtles ?= list->car.

      result = lcl_lisp_new=>integer( lo_turtles->turtle->width ).
    ENDMETHOD.

    METHOD proc_turtle_height.
*    (turtles-height turtles) → (and/c real? positive?)
*      turtles : turtles?
      _validate list.
      _validate_turtle list->car `turtles-height`.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.
      lo_turtles ?= list->car.

      result = lcl_lisp_new=>integer( lo_turtles->turtle->height ).
    ENDMETHOD.

    METHOD proc_turtle_pen_width.
*      (turtles-pen-width turtles) → (real-in 0 255)
*        turtles : turtles?
      _validate list.
      _validate_turtle list->car `turtles-pen-width`.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.
      lo_turtles ?= list->car.

      result = lcl_lisp_new=>integer( lo_turtles->turtle->pen-stroke_width ).
    ENDMETHOD.

    METHOD proc_turtle_pen_color.
*     (turtles-pen-color turtles) → (is-a?/c color%)
*       turtles : turtles?
      _validate list.
      _validate_turtle list->car `turtles-pen-color`.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.
      lo_turtles ?= list->car.

      result = lcl_lisp_new=>string( lo_turtles->turtle->pen-stroke_color ).
    ENDMETHOD.

    METHOD proc_turtle_regular_poly.
      _validate: list, list->cdr, list->cdr->cdr.
*      (regular-poly sides radius turtles) → turtles?
*        sides : exact-nonnegative-integer?
*        radius : real?
*        turtles : turtles?
      _validate_index list->car `regular-poly sides`.
      DATA lo_sides TYPE REF TO lcl_lisp_integer.
      lo_sides ?= list->car.

      _validate_integer list->cdr->car `regular-poly radius`.
      DATA lo_radius TYPE REF TO lcl_lisp_integer.
      lo_radius ?= list->cdr->car.

      _validate_turtle list->cdr->cdr->car `regular-poly`.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.
      lo_turtles ?= list->cdr->cdr->car.

      lo_turtles->turtle->regular_polygon( num_sides = lo_sides->int
                                           side_length = lo_radius->int ).
      result = lo_turtles.
    ENDMETHOD.

    METHOD proc_turtle_regular_polys.
      _validate: list, list->cdr, list->cdr->cdr.
*      Draws n regular polys each with n sides centered at the turtle.
*      (regular-polys n s turtles) → turtles?
*        n : exact-nonnegative-integer?
*        s : any/c
*        turtles : turtles?
      _validate_integer list->car `regular-polys n`.
      DATA lo_n TYPE REF TO lcl_lisp_integer.
      lo_n ?= list->car.

      _validate_index list->cdr->car `regular-polys s`.
      DATA lo_side TYPE REF TO lcl_lisp_integer.
      lo_side ?= list->cdr->car.

      _validate_turtle list->cdr->cdr->car `regular-polys`.
      DATA lo_turtles TYPE REF TO lcl_lisp_turtle.
      lo_turtles ?= list->cdr->cdr->car.

      lo_turtles->turtle->polygon_flower( number_of_polygons = lo_n->int
                                          polygon_sides = lo_n->int
                                          side_length = lo_side->int ).
      result = lo_turtles.
    ENDMETHOD.

  ENDCLASS.                    "lcl_lisp_interpreter IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_abapfunction IMPLEMENTATION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_abapfunction IMPLEMENTATION.

    METHOD read_interface.
*      Determine the parameters of the function module to populate parameter table
*  TODO: At the moment, we do not support reference types in function module interfaces
      function_name = iv_name.          "Name of function module
      parameters_generated = abap_false.

*     Read the function module interface
      CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
        EXPORTING
          funcname           = function_name  " Name of the function module
          with_enhancements  = 'X'            " X = Enhancement Parameters Will Be Provided
*         ignore_switches    = SPACE           " X = Switches Are Ignored
        IMPORTING
          remote_call        = interface-remote_call
          update_task        = interface-update_task
        TABLES
          exception_list     = interface-exc
          export_parameter   = interface-exp
          import_parameter   = interface-imp
          changing_parameter = interface-cha
          tables_parameter   = interface-tbl
          enha_exp_parameter = interface-enh_exp
          enha_imp_parameter = interface-enh_imp
          enha_cha_parameter = interface-enh_cha
          enha_tbl_parameter = interface-enh_tbl
        EXCEPTIONS
          error_message      = 1
          function_not_found = 2
          invalid_name       = 3
          OTHERS             = 4.
      IF sy-subrc <> 0.
        throw( |Function { function_name }: { error_message( ) }| ).
      ENDIF.
    ENDMETHOD.                    "read_interface

*(define bapi-userdetail (ab-function "BAPI_USER_GET_DETAIL"))  ;; Assign interface of BAPI_USER_GET_DETAIL to a symbol
*(ab-set bapi-userdetail "USERNAME" (ab-get ab-sy "UNAME"))     ;; Set parameter "USERNAME" to current user

    METHOD call.
      create_parameters( ).
*TODO: Map given list to parameters of function module

*     First parameter: Name of function to call;
*     second parameter: data to pass to interface
      CALL FUNCTION list->value
        PARAMETER-TABLE param_active
        EXCEPTION-TABLE exceptions.

      IF sy-subrc EQ c_error_message.
        throw( |Call { list->value }: { error_message( ) }| ).
      ENDIF.

*     Map output parameters to new list
      ro_elem = list.      "Function reference is updated with values after call
    ENDMETHOD.                    "call

    METHOD error_message.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
         INTO rv_message.
    ENDMETHOD.

    METHOD get_function_parameter.
*     Get function parameter by name
*     IDENTIFIER -> Lisp element, string or symbol or index, to identify subcomponent of value
      IF identifier = nil OR
        ( identifier->type NE type_string AND identifier->type NE type_symbol ).
        throw( `ab-get: String or symbol required to access function parameter` ).
      ENDIF.

      create_parameters( ).

      TRY.
          DATA(lv_parmname) = CONV abap_parmname( identifier->value ).
          rdata = VALUE #( param_active[ name = lv_parmname ]-value
                      DEFAULT parameters[ name = lv_parmname ]-value ). "#EC CI_SORTSEQ
        CATCH cx_sy_itab_line_not_found.
          throw( |ab-get: No parameter { lv_parmname } in function| ).
      ENDTRY.
    ENDMETHOD.                    "get_function_parameter

    METHOD create_table_params.
*     Create structures in parameter - TABLES
      LOOP AT it_table INTO DATA(ls_table).
        DATA(ls_par) = VALUE abap_func_parmbind( kind = abap_func_tables
                                                 name = ls_table-parameter ).

        DATA(lv_type) = COND rs38l_typ( WHEN ls_table-typ IS INITIAL THEN ls_table-dbstruct ELSE ls_table-typ ).
        CREATE DATA ls_par-value TYPE TABLE OF (lv_type).
        CREATE DATA ls_par-tables_wa TYPE (lv_type).

        INSERT ls_par INTO TABLE: parameters,
                                  param_active.
      ENDLOOP.
    ENDMETHOD.

    METHOD create_params.
      TYPES: BEGIN OF ts_params,
               parameter TYPE parameter,
               dbfield   TYPE likefield,
               typ       TYPE rs38l_typ,

               default   TYPE default__3,
               optional  TYPE rs38l_opti,
             END OF ts_params.

      DATA(ls_par) = VALUE abap_func_parmbind( kind = iv_kind ).
      LOOP AT it_table ASSIGNING FIELD-SYMBOL(<row>).
        DATA(ls_params) = CORRESPONDING ts_params( <row> ).

        DATA(lv_type) = COND rs38l_typ( WHEN ls_params-dbfield IS NOT INITIAL THEN ls_params-dbfield
                                        WHEN ls_params-typ IS NOT INITIAL THEN ls_params-typ
                                        ELSE 'TEXT100' ).   "Fallback for untyped parameters
        CREATE DATA ls_par-value TYPE (lv_type).

        ls_par-name = ls_params-parameter.
        INSERT ls_par INTO TABLE: parameters,
                                  param_active.
      ENDLOOP.
    ENDMETHOD.

    METHOD create_parameters.
      CHECK parameters_generated EQ abap_false.

      create_exceptions( ).
*     Tables
      create_table_params( interface-tbl ).         "    input TABLES parameter
      create_table_params( interface-enh_tbl ).
*     Import
      create_params( it_table = interface-imp
                     iv_kind = abap_func_exporting ).
      create_params( it_table = interface-enh_imp
                     iv_kind = abap_func_exporting ).
*     Export
      create_params( it_table = interface-exp
                     iv_kind = abap_func_importing ).
      create_params( it_table = interface-enh_exp
                     iv_kind = abap_func_importing ).
*     Changing
      create_params( it_table = interface-cha
                     iv_kind = abap_func_changing ).
      create_params( it_table = interface-enh_cha
                     iv_kind = abap_func_changing ).

      parameters_generated = abap_true.
    ENDMETHOD.

    METHOD create_exceptions.
      exceptions = VALUE #( ( name = 'OTHERS'        value = 10 )
                            ( name = 'error_message' value = c_error_message ) ).
    ENDMETHOD.

  ENDCLASS.                    "lcl_lisp_abapfunction IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_environment IMPLEMENTATION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_environment IMPLEMENTATION.

    METHOD scope_of.
      " find the environment where the symbol is defined
      env = me.
      WHILE env IS BOUND.
        IF line_exists( env->map[ symbol = symbol ] ).
          RETURN.                      " found
        ENDIF.
        env = env->parent.
      ENDWHILE.
      unbound_symbol( symbol ).
    ENDMETHOD.

    METHOD get.
**     Clever but probably expensive TRY / CATCH. This logic is called often
*      TRY.
*          cell = VALUE #( map[ symbol = symbol ]-value DEFAULT outer->get( symbol ) ).
*        CATCH cx_root.
*          unbound_symbol( symbol ).
*      ENDTRY.
      DATA ls_map LIKE LINE OF map.
      DATA lo_env TYPE REF TO lcl_lisp_environment.
*     lookup/locate a symbol key and in this environment or a parent,
*     then returns the matching value.
      lo_env = me.
      WHILE lo_env IS BOUND.
        READ TABLE lo_env->map INTO ls_map WITH KEY symbol = symbol.
        IF sy-subrc EQ 0.
          cell = ls_map-value.
          RETURN.
        ENDIF.
        lo_env = lo_env->parent.
      ENDWHILE.
      " raises an "unbound" error if key is not found
      unbound_symbol( symbol ).
    ENDMETHOD.

    METHOD unbound_symbol.
      " symbol not found in the environment chain
      lcl_lisp=>throw( |Symbol { symbol } is unbound| ).
    ENDMETHOD.

    METHOD define_value.
      element = lcl_lisp_new=>elem( type = type
                                    value = value ).
      set( symbol = symbol
           element = element ).
    ENDMETHOD.

    METHOD syntax.
      element = define_value( symbol = symbol
                              type = type_syntax
                              value = symbol ).
    ENDMETHOD.

    METHOD procedure.
      "##TO DO: parameters for min., max. number of arguments
      element = define_value( symbol = symbol
                              type = type_native
                              value = value ).
    ENDMETHOD.

    METHOD set.
*     Add a value to the (local) environment
      DATA(ls_map) = VALUE ts_map( symbol = symbol
                                   value = element ).
      INSERT ls_map INTO TABLE map.
      CHECK sy-subrc = 4.
      IF once EQ abap_true.
*       The value must not exist yet in scope
*       It is an error for a <variable> to appear more than once in the list of variables.
        lcl_lisp=>throw( |variable { symbol } appears more than once| ).
      ELSE.
        MODIFY TABLE map FROM ls_map.         " overwrite existing defined values
      ENDIF.
    ENDMETHOD.

    METHOD parameters_to_symbols.
*     The lambda receives its own local environment in which to execute, where parameters
*     become symbols that are mapped to the corresponding arguments
*     Assign each argument to its corresponding symbol in the newly created environment
      DATA lv_count TYPE i.

      CASE io_pars->type.
        WHEN type_pair.   "Do we have a proper list?

          DATA(lo_var) = io_pars.                " Pointer to formal parameters
          DATA(lo_arg) = io_args.                " Pointer to arguments
*         local environment used to detect duplicate variable usage (invalid)
          DATA(local) = lcl_lisp_env_factory=>clone( me ).

          WHILE lo_var NE lcl_lisp=>nil.         " Nil would mean no parameters to map

            IF lo_var->type EQ type_symbol.
*             dotted pair after fixed number of parameters, to be bound to a variable number of arguments

*             1) Read the next parameter, bind to the (rest) list of arguments
              local->set( symbol = lo_var->value
                          element = lcl_lisp=>nil
                          once = abap_true ).
              set( symbol = lo_var->value
                   element = lo_arg ).
*             2) Exit
              RETURN.
            ENDIF.

*           Part of the list with fixed number of parameters

            IF lo_arg = lcl_lisp=>nil.           " Premature end of arguments
              lcl_lisp=>throw( |Missing parameter(s) { lo_var->to_string( ) }| ).
            ENDIF.

            ADD 1 TO lv_count.

*           NOTE: Each element of the argument list is evaluated before being defined in the environment
            local->set( symbol = lo_var->car->value
                        element = lcl_lisp=>nil
                        once = abap_true ).
            set( symbol = lo_var->car->value
                 element = lo_arg->car ).

            lo_var = lo_var->cdr.
            lo_arg = lo_arg->cdr.
            CHECK lo_arg IS NOT BOUND.
            lo_arg = lcl_lisp=>nil.
          ENDWHILE.

          IF lo_arg NE lcl_lisp=>nil.  " Excessive number of arguments
            lcl_lisp=>throw( |Expected { lv_count } parameter(s), found { io_args->to_string( ) }| ).
          ENDIF.

        WHEN type_symbol.
*         args is a symbol to be bound to a variable number of parameters
          set( symbol = io_pars->value
               element = io_args ).

      ENDCASE.

    ENDMETHOD.                    "parameters_to_symbols

    METHOD load_syntax.
      " Add primitive functions to environment
      syntax( 'define' ).
      syntax( 'lambda' ).
      syntax( symbol = 'if' ).
      define_value( symbol = c_eval_quote      type = type_syntax value   = `'` ).
      define_value( symbol = c_eval_quasiquote type = type_syntax value   = '`' ).
      syntax( 'set!' ).

      syntax( 'define-values' ).
      syntax( 'define-macro' ).
      syntax( 'define-syntax' ).
      syntax( 'macroexpand' ).
      syntax( 'gensym' ).
      syntax( 'case-lambda' ).
      syntax( 'parameterize' ).

      syntax( 'and' ).
      syntax( 'or' ).
      syntax( 'cond' ).
      syntax( 'unless' ).
      syntax( 'when' ).
      syntax( 'begin' ).
      syntax( 'let' ).
*      syntax( 'let-values' ).
*      syntax( 'let*-values' ).
*      syntax( 'let-syntax' ).
      syntax( 'let*' ).
      syntax( 'letrec' ).
      syntax( 'letrec*' ).
      syntax( 'do' ).
      syntax( 'case' ).

      define_value( symbol = c_eval_unquote          type = type_syntax value   = ',' ).
      define_value( symbol = c_eval_unquote_splicing type = type_syntax value   = ',@' ).
      syntax( 'define-record-type' ).

      syntax( 'guard' ).
    ENDMETHOD.

    METHOD load_libraries.
      syntax( 'define-library' ).
      syntax( 'export'  ).
      syntax( 'cond-expand' ).
      syntax( 'import' ).
      syntax( 'rename' ).
      syntax( 'only' ).
      syntax( 'library' ).

      syntax( 'include-library-declarations' ).
    ENDMETHOD.

    METHOD load_delayed_evaluation.
      syntax( 'delay' ).
      syntax( 'delay-force' ).

      define_value( symbol = 'force'        type = type_native value   = 'PROC_FORCE' ).
      define_value( symbol = 'make-promise' type = type_native value   = 'PROC_MAKE_PROMISE' ).
      define_value( symbol = 'promise?'     type = type_native value   = 'PROC_IS_PROMISE' ).
    ENDMETHOD.

    METHOD load_list.
*     Compatibility
      procedure( symbol = 'empty?'  value   = 'PROC_NILP' ).
      procedure( symbol = 'first'   value   = 'PROC_CAR' ).
      procedure( symbol = 'rest'    value   = 'PROC_CDR' ).

      procedure( symbol = 'memq'    value   = 'PROC_MEMQ' ).
      procedure( symbol = 'memv'    value   = 'PROC_MEMV' ).
      procedure( symbol = 'member'  value   = 'PROC_MEMBER' ).

      procedure( symbol = 'assq'    value   = 'PROC_ASSQ' ).
      procedure( symbol = 'assv'    value   = 'PROC_ASSV' ).
      procedure( symbol = 'assoc'   value   = 'PROC_ASSOC' ).

      procedure( symbol = 'car'     value   = 'PROC_CAR' ).
      procedure( symbol = 'cdr'     value   = 'PROC_CDR' ).
      procedure( symbol = c_eval_cons    value   = 'PROC_CONS' ).
      procedure( symbol = 'nil?'    value   = 'PROC_NILP' ).
      procedure( symbol = 'null?'   value   = 'PROC_NILP' ).

      procedure( symbol = 'set-car!' value   = 'PROC_SET_CAR' ).
      procedure( symbol = 'set-cdr!' value   = 'PROC_SET_CDR' ).

      " CxR library
      procedure( symbol = 'caar'     value   = 'PROC_CAAR' ).
      procedure( symbol = 'cadr'     value   = 'PROC_CADR' ).
      procedure( symbol = 'cdar'     value   = 'PROC_CDAR' ).
      procedure( symbol = 'cddr'     value   = 'PROC_CDDR' ).
      procedure( symbol = 'caaar'    value   = 'PROC_CAAAR' ).
      procedure( symbol = 'cdaar'    value   = 'PROC_CDAAR' ).
      procedure( symbol = 'caadr'    value   = 'PROC_CAADR' ).
      procedure( symbol = 'cdadr'    value   = 'PROC_CDADR' ).
      procedure( symbol = 'cadar'    value   = 'PROC_CADAR' ).
      procedure( symbol = 'cddar'    value   = 'PROC_CDDAR' ).
      procedure( symbol = 'caddr'    value   = 'PROC_CADDR' ).
      procedure( symbol = 'cdddr'    value   = 'PROC_CDDDR' ).

      procedure( symbol = 'caaaar'    value   = 'PROC_CAAAAR' ).
      procedure( symbol = 'cdaaar'    value   = 'PROC_CDAAAR' ).
      procedure( symbol = 'cadaar'    value   = 'PROC_CADAAR' ).
      procedure( symbol = 'cddaar'    value   = 'PROC_CDDAAR' ).
      procedure( symbol = 'caaadr'    value   = 'PROC_CAAADR' ).
      procedure( symbol = 'cdaadr'    value   = 'PROC_CDAADR' ).
      procedure( symbol = 'cadadr'    value   = 'PROC_CADADR' ).
      procedure( symbol = 'cddadr'    value   = 'PROC_CDDADR' ).
      procedure( symbol = 'caadar'    value   = 'PROC_CAADAR' ).
      procedure( symbol = 'cdadar'    value   = 'PROC_CDADAR' ).
      procedure( symbol = 'caddar'    value   = 'PROC_CADDAR' ).
      procedure( symbol = 'cdddar'    value   = 'PROC_CDDDAR' ).
      procedure( symbol = 'caaddr'    value   = 'PROC_CAADDR' ).
      procedure( symbol = 'caaddr'    value   = 'PROC_CAADDR' ).
      procedure( symbol = 'cadddr'    value   = 'PROC_CADDDR' ).
      procedure( symbol = 'cddddr'    value   = 'PROC_CDDDDR' ).

      procedure( symbol = 'append!'  value   = 'PROC_APPEND_UNSAFE' ).
      procedure( symbol = 'list'     value   = 'PROC_LIST' ).
      procedure( symbol = 'length'   value   = 'PROC_LENGTH' ).
      procedure( symbol = 'reverse'  value   = 'PROC_REVERSE' ).

      procedure( symbol = c_eval_append  value   = 'PROC_APPEND' ).
      procedure( symbol = 'make-list'    value   = 'PROC_MAKE_LIST' ).
      procedure( symbol = 'list-tail'    value   = 'PROC_LIST_TAIL' ).
      procedure( symbol = 'list-ref'     value   = 'PROC_LIST_REF' ).
      procedure( symbol = 'list-copy'    value   = 'PROC_LIST_COPY' ).
      procedure( symbol = 'list->vector' value   = 'PROC_LIST_TO_VECTOR' ).
      procedure( symbol = 'iota'         value   = 'PROC_IOTA' ).
    ENDMETHOD.

    METHOD load_numbers.
      procedure( symbol = 'number?'   value = 'PROC_IS_NUMBER' ).

      procedure( symbol = 'exact'     value = 'PROC_TO_EXACT' ).
      procedure( symbol = 'inexact'   value = 'PROC_TO_INEXACT' ).

      procedure( symbol = 'floor'     value = 'PROC_FLOOR' ).
      procedure( symbol = 'floor/'    value = 'PROC_FLOOR_NEW' ).
      procedure( symbol = 'ceiling'   value = 'PROC_CEILING' ).
      procedure( symbol = 'truncate'  value = 'PROC_TRUNCATE' ).
      procedure( symbol = 'truncate/' value = 'PROC_TRUNCATE_NEW' ).
      procedure( symbol = 'round'     value = 'PROC_ROUND' ).

      procedure( symbol = 'numerator'   value = 'PROC_NUMERATOR' ).
      procedure( symbol = 'denominator' value = 'PROC_DENOMINATOR' ).
      procedure( symbol = 'remainder' value = 'PROC_REMAINDER' ).
      procedure( symbol = 'modulo'    value = 'PROC_MODULO' ).
      procedure( symbol = 'quotient'  value = 'PROC_QUOTIENT' ).
      procedure( symbol = 'random'    value = 'PROC_RANDOM' ).
      procedure( symbol = 'max'       value = 'PROC_MAX' ).
      procedure( symbol = 'min'       value = 'PROC_MIN' ).
      procedure( symbol = 'gcd'       value = 'PROC_GCD' ).
      procedure( symbol = 'lcm'       value = 'PROC_LCM' ).

      procedure( symbol = 'make-rectangular' value = 'PROC_MAKE_RECTANGULAR' ).
      procedure( symbol = 'make-polar'       value = 'PROC_MAKE_POLAR' ).
      procedure( symbol = 'real-part'        value = 'PROC_REAL_PART' ).
      procedure( symbol = 'imag-part'        value = 'PROC_IMAG_PART' ).
      procedure( symbol = 'magnitude'        value = 'PROC_MAGNITUDE' ).
      procedure( symbol = 'angle'            value = 'PROC_ANGLE' ).
    ENDMETHOD.

    METHOD load_math.
*     Math
      procedure( symbol = 'abs'   value = 'PROC_ABS' ).

      procedure( symbol = 'square'             value = 'PROC_SQUARE' ).
      procedure( symbol = 'exact-integer-sqrt' value = 'PROC_INT_SQRT' ).
      procedure( symbol = 'floor-quotient'     value = 'PROC_FLOOR_QUOTIENT' ).
      procedure( symbol = 'floor-remainder'    value = 'PROC_FLOOR_REMAINDER' ).
      procedure( symbol = 'truncate-remainder' value = 'PROC_TRUNC_REMAINDER' ).
      procedure( symbol = 'truncate-quotient'  value = 'PROC_TRUNC_QUOTIENT' ).

      " Inexact Library
      procedure( symbol = 'sin'   value = 'PROC_SIN' ).
      procedure( symbol = 'cos'   value = 'PROC_COS' ).
      procedure( symbol = 'tan'   value = 'PROC_TAN' ).
      procedure( symbol = 'asin'  value = 'PROC_ASIN' ).
      procedure( symbol = 'acos'  value = 'PROC_ACOS' ).
      procedure( symbol = 'atan'  value = 'PROC_ATAN' ).
      procedure( symbol = 'exp'   value = 'PROC_EXP' ).
      procedure( symbol = 'log'   value = 'PROC_LOG' ).
      procedure( symbol = 'sqrt'  value = 'PROC_SQRT' ).
      procedure( symbol = 'nan?'        value = 'PROC_IS_NAN' ).
      procedure( symbol = 'finite?'     value = 'PROC_IS_FINITE' ).
      procedure( symbol = 'infinite?'   value = 'PROC_IS_INFINITE' ).

      procedure( symbol = 'expt'  value = 'PROC_EXPT' ).
      procedure( symbol = 'sinh'  value = 'PROC_SINH' ).
      procedure( symbol = 'cosh'  value = 'PROC_COSH' ).
      procedure( symbol = 'tanh'  value = 'PROC_TANH' ).
      procedure( symbol = 'asinh' value = 'PROC_ASINH' ).
      procedure( symbol = 'acosh' value = 'PROC_ACOSH' ).
      procedure( symbol = 'atanh' value = 'PROC_ATANH' ).
    ENDMETHOD.

    METHOD load_chars.
      " Char Library
      procedure( symbol = 'char-alphabetic?'  value   = 'PROC_IS_CHAR_ALPHABETIC' ).
      procedure( symbol = 'char-numeric?'     value   = 'PROC_IS_CHAR_NUMERIC' ).
      procedure( symbol = 'char-whitespace?'  value   = 'PROC_IS_CHAR_WHITESPACE' ).
      procedure( symbol = 'char-upper-case?'  value   = 'PROC_IS_CHAR_UPPER_CASE' ).
      procedure( symbol = 'char-lower-case?'  value   = 'PROC_IS_CHAR_LOWER_CASE' ).

      procedure( symbol = 'digit-value'       value   = 'PROC_DIGIT_VALUE' ).
      procedure( symbol = 'char->integer'     value   = 'PROC_CHAR_TO_INTEGER' ).
      procedure( symbol = 'integer->char'     value   = 'PROC_INTEGER_TO_CHAR' ).
      procedure( symbol = 'char-upcase'       value   = 'PROC_CHAR_UPCASE' ).
      procedure( symbol = 'char-downcase'     value   = 'PROC_CHAR_DOWNCASE' ).

      procedure( symbol = 'char=?'     value   = 'PROC_CHAR_LIST_IS_EQ' ).
      procedure( symbol = 'char<?'     value   = 'PROC_CHAR_LIST_IS_LT' ).
      procedure( symbol = 'char>?'     value   = 'PROC_CHAR_LIST_IS_GT' ).
      procedure( symbol = 'char<=?'    value   = 'PROC_CHAR_LIST_IS_LE' ).
      procedure( symbol = 'char>=?'    value   = 'PROC_CHAR_LIST_IS_GE' ).

      procedure( symbol = 'char-ci=?'     value   = 'PROC_CHAR_CI_LIST_IS_EQ' ).
      procedure( symbol = 'char-ci<?'     value   = 'PROC_CHAR_CI_LIST_IS_LT' ).
      procedure( symbol = 'char-ci>?'     value   = 'PROC_CHAR_CI_LIST_IS_GT' ).
      procedure( symbol = 'char-ci<=?'    value   = 'PROC_CHAR_CI_LIST_IS_LE' ).
      procedure( symbol = 'char-ci>=?'    value   = 'PROC_CHAR_CI_LIST_IS_GE' ).
    ENDMETHOD.

    METHOD load_strings.
      procedure( symbol = 'number->string' value = 'PROC_NUM_TO_STRING' ).
      procedure( symbol = 'string->number' value = 'PROC_STRING_TO_NUM' ).
      procedure( symbol = 'make-string'    value = 'PROC_MAKE_STRING' ).
      procedure( symbol = 'string'         value = 'PROC_STRING' ).
      procedure( symbol = 'string->list'   value = 'PROC_STRING_TO_LIST' ).
      procedure( symbol = 'list->string'   value = 'PROC_LIST_TO_STRING' ).
      procedure( symbol = 'symbol->string' value = 'PROC_SYMBOL_TO_STRING' ).
      procedure( symbol = 'string->symbol' value = 'PROC_STRING_TO_SYMBOL' ).
      procedure( symbol = 'string-append'  value = 'PROC_STRING_APPEND' ).
      procedure( symbol = 'string-length'  value = 'PROC_STRING_LENGTH' ).
      procedure( symbol = 'string-copy'    value = 'PROC_STRING_COPY' ).
      procedure( symbol = 'substring'      value = 'PROC_STRING_COPY' ).
      procedure( symbol = 'string-ref'     value = 'PROC_STRING_REF' ).
      procedure( symbol = 'string-set!'    value = 'PROC_STRING_SET' ).

      procedure( symbol = 'string=?'     value   = 'PROC_STRING_LIST_IS_EQ' ).
      procedure( symbol = 'string<?'     value   = 'PROC_STRING_LIST_IS_LT' ).
      procedure( symbol = 'string>?'     value   = 'PROC_STRING_LIST_IS_GT' ).
      procedure( symbol = 'string<=?'    value   = 'PROC_STRING_LIST_IS_LE' ).
      procedure( symbol = 'string>=?'    value   = 'PROC_STRING_LIST_IS_GE' ).

      procedure( symbol = 'string-ci=?'   value  = 'PROC_STRING_CI_LIST_IS_EQ' ).
      procedure( symbol = 'string-ci<?'   value  = 'PROC_STRING_CI_LIST_IS_LT' ).
      procedure( symbol = 'string-ci>?'   value  = 'PROC_STRING_CI_LIST_IS_GT' ).
      procedure( symbol = 'string-ci<=?'  value  = 'PROC_STRING_CI_LIST_IS_LE' ).
      procedure( symbol = 'string-ci>=?'  value  = 'PROC_STRING_CI_LIST_IS_GE' ).
    ENDMETHOD.

    METHOD load_turtles.
      procedure( symbol = 'turtles'       value   = 'PROC_TURTLE_NEW' ).
      procedure( symbol = 'turtles?'      value   = 'PROC_TURTLE_EXIST' ).
      procedure( symbol = 'move'          value   = 'PROC_TURTLE_MOVE' ).
      procedure( symbol = 'draw'          value   = 'PROC_TURTLE_DRAW' ).
      procedure( symbol = 'erase'         value   = 'PROC_TURTLE_ERASE' ).
      procedure( symbol = 'move-offset'   value   = 'PROC_TURTLE_MOVE_OFFSET' ).
      procedure( symbol = 'draw-offset'   value   = 'PROC_TURTLE_DRAW_OFFSET' ).
      procedure( symbol = 'erase-offset'  value   = 'PROC_TURTLE_ERASE_OFFSET' ).
      procedure( symbol = 'turn'          value   = 'PROC_TURTLE_TURN_DEGREES' ).
      procedure( symbol = 'turn/radians'  value   = 'PROC_TURTLE_TURN_RADIANS' ).
      procedure( symbol = 'set-pen-width' value   = 'PROC_TURTLE_SET_PEN_WIDTH' ).
      procedure( symbol = 'set-pen-color' value   = 'PROC_TURTLE_SET_PEN_COLOR' ).

      procedure( symbol = 'merge'             value = 'PROC_TURTLE_MERGE' ).
      procedure( symbol = 'clean'             value = 'PROC_TURTLE_CLEAN' ).
      procedure( symbol = 'turtle-state'      value = 'PROC_TURTLE_STATE' ).
      procedure( symbol = 'turtles-height'    value = 'PROC_TURTLE_HEIGHT' ).
      procedure( symbol = 'turtles-width'     value = 'PROC_TURTLE_WIDTH' ).
      procedure( symbol = 'turtles-pen-color' value = 'PROC_TURTLE_PEN_COLOR' ).
      procedure( symbol = 'turtles-pen-width' value = 'PROC_TURTLE_PEN_WIDTH' ).
      procedure( symbol = 'regular-poly'      value = 'PROC_TURTLE_REGULAR_POLY' ).
      procedure( symbol = 'regular-polys'     value = 'PROC_TURTLE_REGULAR_POLYS' ).
    ENDMETHOD.

    METHOD prepare.
      " Create symbols for nil, true and false values
      set( symbol = 'nil' element = lcl_lisp=>nil ).
      set( symbol = '#f'  element = lcl_lisp=>true ).
      set( symbol = '#t'  element = lcl_lisp=>false ).

      load_syntax( ).
      load_delayed_evaluation( ).

      " Procedures
      define_value( symbol = 'apply'        type = type_primitive value   = 'apply' ).
      define_value( symbol = 'for-each'     type = type_primitive value   = 'for-each' ).
      define_value( symbol = 'map'          type = type_primitive value   = 'map' ).
      define_value( symbol = 'call-with-current-continuation' type = type_native value = 'PROC_CALL_CC' ).
      define_value( symbol = 'call/cc'                        type = type_native value = 'PROC_CALL_CC' ).

*     Add native functions to environment
      procedure( symbol = '+'      value = 'PROC_ADD' ).
      procedure( symbol = '-'      value = 'PROC_SUBTRACT' ).
      procedure( symbol = '*'      value = 'PROC_MULTIPLY' ).
      procedure( symbol = '/'      value = 'PROC_DIVIDE' ).

      procedure( symbol = 'values' value = 'PROC_VALUES' ).

      procedure( symbol = '>'      value = 'PROC_GT' ).
      procedure( symbol = '>='     value = 'PROC_GTE' ).
      procedure( symbol = '<'      value = 'PROC_LT' ).
      procedure( symbol = '<='     value = 'PROC_LTE' ).
      procedure( symbol = '='      value = 'PROC_EQL' ). "Math equal
      procedure( symbol = 'eq?'    value = 'PROC_EQ' ).
      procedure( symbol = 'eqv?'   value = 'PROC_EQV' ).
      procedure( symbol = 'equal?' value = 'PROC_EQUAL' ).

      procedure( symbol = 'not'    value = 'PROC_NOT' ).

      load_list( ).

      procedure( symbol = 'make-parameter' value = 'PROC_MAKE_PARAMETER' ).

      " vector-related functions
      procedure( symbol = 'vector'        value = 'PROC_VECTOR' ).
      procedure( symbol = 'vector-length' value = 'PROC_VECTOR_LENGTH' ).
      procedure( symbol = 'vector-set!'   value = 'PROC_VECTOR_SET' ).
      procedure( symbol = 'vector-fill!'  value = 'PROC_VECTOR_FILL' ).
      procedure( symbol = 'vector-ref'    value = 'PROC_VECTOR_REF' ).
      procedure( symbol = 'vector->list'  value = 'PROC_VECTOR_TO_LIST' ).
      procedure( symbol = 'make-vector'   value = 'PROC_MAKE_VECTOR' ).

      " bytevector-related functions
      procedure( symbol = 'bytevector'         value = 'PROC_BYTEVECTOR' ).
      procedure( symbol = 'bytevector-length'  value = 'PROC_BYTEVECTOR_LENGTH' ).
      procedure( symbol = 'bytevector-u8-set!' value = 'PROC_BYTEVECTOR_U8_SET' ).
      procedure( symbol = 'bytevector-u8-ref'  value = 'PROC_BYTEVECTOR_U8_REF' ).
      procedure( symbol = 'bytevector-append'  value = 'PROC_BYTEVECTOR_APPEND' ).
      procedure( symbol = 'bytevector-copy'    value = 'PROC_BYTEVECTOR_NEW_COPY' ).
      procedure( symbol = 'bytevector-copy!'   value = 'PROC_BYTEVECTOR_COPY' ).
      procedure( symbol = 'utf8->string'       value = 'PROC_UTF8_TO_STRING' ).
      procedure( symbol = 'string->utf8'       value = 'PROC_STRING_TO_UTF8' ).
      procedure( symbol = 'make-bytevector'    value = 'PROC_MAKE_BYTEVECTOR' ).

*     Hash-related functions
      procedure( symbol = 'make-hash'   value   = 'PROC_MAKE_HASH' ).
      procedure( symbol = 'hash-get'    value   = 'PROC_HASH_GET' ).
      procedure( symbol = 'hash-insert' value   = 'PROC_HASH_INSERT' ).
      procedure( symbol = 'hash-remove' value   = 'PROC_HASH_REMOVE' ).
      procedure( symbol = 'hash-keys'   value   = 'PROC_HASH_KEYS' ).
*     Functions for type:
      procedure( symbol = 'string?'     value = 'PROC_IS_STRING' ).
      procedure( symbol = 'char?'       value = 'PROC_IS_CHAR' ).
      procedure( symbol = 'hash?'       value = 'PROC_IS_HASH' ).

      procedure( symbol = 'exact-integer?' value = 'PROC_IS_EXACT_INTEGER' ).

      procedure( symbol = 'integer?'    value = 'PROC_IS_INTEGER' ).
      procedure( symbol = 'complex?'    value = 'PROC_IS_COMPLEX' ).
      procedure( symbol = 'real?'       value = 'PROC_IS_REAL' ).
      procedure( symbol = 'rational?'   value = 'PROC_IS_RATIONAL' ).
      procedure( symbol = 'list?'       value = 'PROC_IS_LIST' ).
      procedure( symbol = 'pair?'       value = 'PROC_IS_PAIR' ).
      procedure( symbol = 'vector?'     value = 'PROC_IS_VECTOR' ).
      procedure( symbol = 'bytevector?' value = 'PROC_IS_BYTEVECTOR' ).
      procedure( symbol = 'boolean?'    value = 'PROC_IS_BOOLEAN' ).
      procedure( symbol = 'alist?'      value = 'PROC_IS_ALIST' ).
      procedure( symbol = 'procedure?'  value = 'PROC_IS_PROCEDURE' ).
      procedure( symbol = 'symbol?'     value = 'PROC_IS_SYMBOL' ).
      procedure( symbol = 'symbol=?'    value = 'PROC_SYMBOL_LIST_IS_EQUAL' ).
      procedure( symbol = 'port?'       value = 'PROC_IS_PORT' ).
      procedure( symbol = 'boolean=?'   value = 'PROC_BOOLEAN_LIST_IS_EQUAL' ).
      procedure( symbol = 'exact?'      value = 'PROC_IS_EXACT' ).
      procedure( symbol = 'inexact?'    value = 'PROC_IS_INEXACT' ).

*     Format
      procedure( symbol = 'newline'     value = 'PROC_NEWLINE' ).
      procedure( symbol = 'display'     value = 'PROC_DISPLAY' ).

      procedure( symbol = 'read'         value = 'PROC_READ' ).
      procedure( symbol = 'read-char'    value = 'PROC_READ_CHAR' ).
      procedure( symbol = 'read-line'    value = 'PROC_READ_LINE' ).
      procedure( symbol = 'read-string'  value = 'PROC_READ_STRING' ).

      procedure( symbol = 'read-u8'      value = 'PROC_READ_U8' ).

      procedure( symbol = 'peek-char'    value = 'PROC_PEEK_CHAR' ).
      procedure( symbol = 'peek-u8'      value = 'PROC_PEEK_U8' ).

      procedure( symbol = 'write'        value = 'PROC_WRITE' ).
      procedure( symbol = 'write-char'   value = 'PROC_WRITE_CHAR' ).
      procedure( symbol = 'write-u8'     value = 'PROC_WRITE_U8' ).
      procedure( symbol = 'write-string' value = 'PROC_WRITE_STRING' ).

      procedure( symbol = 'write-bytevector' value = 'PROC_WRITE_BYTEVECTOR' ).
      procedure( symbol = 'read-bytevector'  value = 'PROC_READ_BYTEVECTOR' ).

      procedure( symbol = 'char-ready?'  value = 'PROC_IS_CHAR_READY' ).
      procedure( symbol = 'u8-ready?'    value = 'PROC_IS_U8_READY' ).

      load_strings( ).
      load_numbers( ).
      load_math( ).

      " not implemented yet
      procedure( symbol = 'rationalize' value = 'PROC_RATIONALIZE' ).

      procedure( symbol = 'zero?'     value = 'PROC_IS_ZERO' ).
      procedure( symbol = 'positive?' value = 'PROC_IS_POSITIVE' ).
      procedure( symbol = 'negative?' value = 'PROC_IS_NEGATIVE' ).
      procedure( symbol = 'odd?'      value = 'PROC_IS_ODD' ).
      procedure( symbol = 'even?'     value = 'PROC_IS_EVEN' ).
*     Continuation
      procedure( symbol = 'dynamic-wind'     value = 'PROC_DYNAMIC_WIND' ).
      procedure( symbol = 'call-with-values' value = 'PROC_CALL_WITH_VALUES' ).

*     Native functions for ABAP integration
      procedure( symbol = 'ab-data'       value = 'PROC_ABAP_DATA' ).
      procedure( symbol = 'ab-function'   value = 'PROC_ABAP_FUNCTION' ).
      procedure( symbol = 'ab-func-param' value = 'PROC_ABAP_FUNCTION_PARAM' ).
      procedure( symbol = 'ab-table'      value = 'PROC_ABAP_TABLE' ).
      procedure( symbol = 'ab-append-row' value = 'PROC_ABAP_APPEND_ROW' ).
      procedure( symbol = 'ab-delete-row' value = 'PROC_ABAP_DELETE_ROW' ).
      procedure( symbol = 'ab-get-row'    value = 'PROC_ABAP_GET_ROW' ).
      procedure( symbol = 'ab-get-value'  value = 'PROC_ABAP_GET_VALUE' ).
      procedure( symbol = 'ab-set-value'  value = 'PROC_ABAP_SET_VALUE' ).

      procedure( symbol = 'ab-get' value = 'PROC_ABAP_GET' ).
      procedure( symbol = 'ab-set' value = 'PROC_ABAP_SET' ).

*     Errors
      procedure( symbol = 'raise'  value = 'PROC_RAISE' ).
      procedure( symbol = 'error'  value = 'PROC_ERROR' ).

*     Ports
      procedure( symbol = 'current-input-port'  value = 'PROC_CURRENT_INPUT_PORT' ).
      procedure( symbol = 'current-output-port' value = 'PROC_CURRENT_OUTPUT_PORT' ).
      procedure( symbol = 'current-error-port'  value = 'PROC_CURRENT_ERROR_PORT' ).

      procedure( symbol = 'close-input-port'    value = 'PROC_CLOSE_INPUT_PORT' ).
      procedure( symbol = 'close-output-port'   value = 'PROC_CLOSE_OUTPUT_PORT' ).
      procedure( symbol = 'close-port'          value = 'PROC_CLOSE_PORT' ).

      procedure( symbol = 'input-port?'         value = 'PROC_IS_INPUT_PORT' ).
      procedure( symbol = 'output-port?'        value = 'PROC_IS_OUTPUT_PORT' ).
      procedure( symbol = 'textual-port?'       value = 'PROC_IS_TEXTUAL_PORT' ).
      procedure( symbol = 'binary-port?'        value = 'PROC_IS_BINARY_PORT' ).
      procedure( symbol = 'input-port-open?'    value = 'PROC_IS_OPEN_INPUT_PORT' ).
      procedure( symbol = 'output-port-open?'   value = 'PROC_IS_OPEN_OUTPUT_PORT' ).

      procedure( symbol = 'open-output-string'  value = 'PROC_OPEN_OUTPUT_STRING' ).
      procedure( symbol = 'open-input-string'   value = 'PROC_OPEN_INPUT_STRING' ).
      procedure( symbol = 'get-output-string'   value = 'PROC_GET_OUTPUT_STRING' ).
      procedure( symbol = 'eof-object'          value = 'PROC_EOF_OBJECT' ).
      procedure( symbol = 'eof-object?'         value = 'PROC_IS_EOF_OBJECT' ).

      load_chars( ).

      procedure( symbol = 'sql-query'    value = 'PROC_SQL_QUERY' ).
      procedure( symbol = 'define-query' value = 'PROC_SQL_PREPARE' ).

      load_turtles( ).

      DATA lr_ref TYPE REF TO data.
*     Define a value in the environment for SYST
      GET REFERENCE OF syst INTO lr_ref.
      set( symbol = 'ab-sy' element = lcl_lisp_new=>data( lr_ref ) ).

    ENDMETHOD.

  ENDCLASS.                    "lcl_lisp_environment IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_profiler DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_profiler DEFINITION INHERITING FROM lcl_lisp_interpreter.
    PUBLIC SECTION.
      METHODS eval_repl REDEFINITION.
      DATA runtime TYPE i READ-ONLY.
  ENDCLASS.                    "lcl_lisp_profiler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_profiler IMPLEMENTATION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_profiler IMPLEMENTATION.

    METHOD eval_repl.
      GET RUN TIME FIELD DATA(lv_start).         " Start timer
      response = super->eval_repl( EXPORTING code = code
                                   IMPORTING output = output ).       " Evaluate given code
      GET RUN TIME FIELD runtime.                " Stop time

      SUBTRACT lv_start FROM runtime.
    ENDMETHOD.                    "eval_repl

  ENDCLASS.                    "lcl_lisp_profiler IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp IMPLEMENTATION
*----------------------------------------------------------------------*
  CLASS lcl_lisp IMPLEMENTATION.

    METHOD class_constructor.
      nil = lcl_lisp_new=>null( ).
      false = lcl_lisp_new=>boolean( '#f' ).
      true = lcl_lisp_new=>boolean( '#t' ).
      undefined = lcl_lisp_new=>undefined( ).

      quote = lcl_lisp_new=>symbol( c_eval_quote ).
      quasiquote = lcl_lisp_new=>symbol( c_eval_quasiquote ).
      unquote = lcl_lisp_new=>symbol( c_eval_unquote ).
      unquote_splicing = lcl_lisp_new=>symbol( c_eval_unquote_splicing ).
      append = lcl_lisp_new=>symbol( c_eval_append ).
      cons = lcl_lisp_new=>symbol( c_eval_cons ).
      list = lcl_lisp_new=>symbol( c_eval_list ).

      char_alarm = lcl_lisp_new=>xchar( '0007' ).
      char_backspace = lcl_lisp_new=>xchar( '0008' ).
      char_delete = lcl_lisp_new=>xchar( '007F' ).
      char_escape = lcl_lisp_new=>xchar( '001B' ).
      char_linefeed = lcl_lisp_new=>xchar( '000A' ).
      char_null = lcl_lisp_new=>xchar( '0000' ).
      char_return = lcl_lisp_new=>xchar( '000D' ).
      char_space = lcl_lisp_new=>char( ` ` ).
      char_tab = lcl_lisp_new=>xchar( '0009' ).

      new_line = lcl_lisp_new=>string( |\n| ).
      eof_object = lcl_lisp_new=>xchar( c_lisp_eof ).

    ENDMETHOD.

    METHOD constructor.
      super->constructor( ).
      me->type = type.
    ENDMETHOD.

    METHOD is_equivalent. "eqv?
      _validate io_elem.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_real TYPE REF TO lcl_lisp_real.
      DATA lo_z TYPE REF TO lcl_lisp_complex.

      result = abap_false.

      DATA(b) = io_elem.
*     Object a and Object b are both #t or both #f or both the empty list.
      IF ( me EQ true AND b EQ true )
        OR ( me EQ false AND b EQ false )
        OR ( me EQ nil AND b EQ nil ).
        result = abap_true.
        RETURN.
      ENDIF.

      CHECK type EQ b->type.

      CASE type.
        WHEN type_integer
          OR type_rational
          OR type_real
          OR type_complex.
* obj1 and obj2 are both exact numbers and are numerically equal (in the sense of =).

*obj1 and obj2 are both inexact numbers such that they are numerically equal (in the sense of =)
*and they yield the same results (in the sense of eqv?) when passed as arguments to any other
*procedure that can be defined as a finite composition of Scheme’s standard arithmetic procedures,
*provided it does not result in a NaN value.
          DATA(lo_number) = CAST lcl_lisp_number( me ).
          DATA(lo_other) = CAST lcl_lisp_number( b ).

          CHECK lo_number->exact = lo_other->exact
            AND lo_number->number_is_equal( lo_other ).

        WHEN type_symbol.
* obj1 and obj2 are both symbols and are the same symbol according to the symbol=? procedure (section 6.5).
          DATA(lo_symbol) = CAST lcl_lisp_symbol( me ).
          DATA(lo_s_b) = CAST lcl_lisp_symbol( b ).
          CHECK lo_symbol->value = lo_s_b->value
            AND lo_symbol->index = lo_s_b->index.  " for uninterned symbols

        WHEN type_string.
* obj1 and obj2 are both characters and are the same character according to the char=? procedure (section 6.6).
          DATA(lo_string) = CAST lcl_lisp_string( me ).
          DATA(lo_str_b) = CAST lcl_lisp_string( b ).
          CHECK lo_string->value = lo_str_b->value.

        WHEN type_pair.
* obj1 and obj2 are procedures whose location tags are equal (section 4.1.4).
          DATA(lo_pair) = CAST lcl_lisp_pair( me ).
          DATA(lo_p_b) = CAST lcl_lisp_pair( b ).
          CHECK lo_pair->car EQ lo_p_b->car AND lo_pair->cdr EQ lo_p_b->cdr.

        WHEN type_lambda OR type_case_lambda .
* obj1 and obj2 are procedures whose location tags are equal (section 4.1.4).
          DATA(lo_lambda) = CAST lcl_lisp_lambda( me ).
          DATA(lo_l_b) = CAST lcl_lisp_lambda( b ).
          CHECK lo_lambda->car EQ lo_l_b->car AND lo_lambda->cdr EQ lo_l_b->cdr
            AND lo_lambda->category EQ lo_l_b->category AND lo_lambda->environment = b->environment.

        WHEN OTHERS.
* obj1 and obj2 are pairs, vectors, bytevectors, records, or strings that denote the same location in the store (section 3.4).

          CHECK me = b.
      ENDCASE.
      result = abap_true.
    ENDMETHOD.

    METHOD is_equal. "equal?
* The equal? procedure, when applied to pairs, vectors, strings and bytevectors, recursively compares them,
* returning #t when the unfoldings of its arguments into (possibly infinite) trees are equal (in the sense
* of equal?) as ordered trees, and #f otherwise.
* equal? returns the same as eqv? when applied to booleans, symbols, numbers, characters, ports, procedures,
* and the empty list. If two objects are eqv?, they must be equal? as well.
* In all other cases, equal? may return either #t or #f.
* Even if its arguments are circular data structures, equal? must always terminate.
      _validate: io_elem.
      result = abap_false.

      IF comp NE nil.
        DATA(lo_lambda) = comp->car.
        DATA(lo_env) = lo_lambda->environment.
        IF lo_env IS NOT BOUND.
          lo_env = environment.
        ENDIF.
        DATA(lo_head) = lcl_lisp_new=>list3( io_first = lo_lambda
                                             io_second = lcl_lisp_new=>quote( me )
                                             io_third = lcl_lisp_new=>quote( io_elem ) ).

        IF interpreter->eval( VALUE #( elem = lo_head
                                       env = lo_env ) ) NE false.
          result = abap_true.
        ENDIF.
        RETURN.
      ENDIF.

      CHECK type EQ io_elem->type.

      CASE type.

        WHEN type_pair.
          DATA(lo_a) = me.
          DATA(lo_slow_a) = me.

          DATA(lo_b) = io_elem.
          DATA(lo_slow_b) = io_elem.

          WHILE lo_a->type EQ type_pair AND lo_b->type EQ type_pair.
            IF lo_a->car EQ lo_a AND lo_b->car EQ lo_b.
*             Circular list
              result = abap_true.
              RETURN.
            ELSEIF NOT lo_a->car->is_equal( lo_b->car ).
              RETURN.
            ENDIF.
            lo_slow_a = lo_slow_a->cdr.
            lo_a = lo_a->cdr.
            lo_slow_b = lo_slow_b->cdr.
            lo_b = lo_b->cdr.
            CHECK lo_a->type EQ type_pair AND lo_b->type EQ type_pair.
            IF NOT lo_a->car->is_equal( lo_b->car ).
              RETURN.
            ENDIF.
            lo_a = lo_a->cdr.
            lo_b = lo_b->cdr.
            CHECK lo_slow_a EQ lo_a AND lo_slow_b EQ lo_b.
*           Circular list
            result = abap_true.
            RETURN.
          ENDWHILE.

          result = lo_a->is_equal( lo_b ).

        WHEN type_vector.
          DATA lo_vec TYPE REF TO lcl_lisp_vector.
          DATA lo_elem_vec TYPE REF TO lcl_lisp_vector.
          lo_vec ?= me.
          lo_elem_vec ?= io_elem.
          result = lo_vec->to_list( )->is_equal( lo_elem_vec->to_list( ) ).

        WHEN type_string.
          CHECK me->value EQ io_elem->value.
          result = abap_true.

*        WHEN type_bytevector.

        WHEN OTHERS.
          result = is_equivalent( io_elem ).

      ENDCASE.

    ENDMETHOD.

    METHOD new_iterator.
      ro_iter = NEW lcl_lisp_iterator( me ).
    ENDMETHOD.

    METHOD format_quasiquote.
*     Quasiquoting output (quasiquote x) is displayed as `x without parenthesis
      ev_skip = abap_false.
      CHECK io_elem->type EQ type_pair.

      IF io_elem->car->type EQ type_symbol OR io_elem->car->type EQ type_syntax.
        CASE io_elem->car->value.
          WHEN c_eval_quote OR `'`.
            ev_str &&= `'`.
            ev_skip = abap_true.

          WHEN c_eval_quasiquote OR '`'.
            ev_str &&= '`'.
            ev_skip = abap_true.

          WHEN c_eval_unquote OR ','.
            ev_str &&= ','.
            ev_skip = abap_true.

          WHEN c_eval_unquote_splicing OR ',@'.
            ev_str &&= ',@'.
            ev_skip = abap_true.

        ENDCASE.
      ENDIF.
    ENDMETHOD.

    METHOD set_shared_structure.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      DATA lo_last_pair TYPE REF TO lcl_lisp.

      CHECK mv_label IS NOT INITIAL.

      CASE type.
        WHEN type_pair.
          lo_last_pair = lo_ptr = me.
          WHILE lo_ptr->type EQ type_pair.
            lo_last_pair = lo_ptr.
            lo_ptr = lo_ptr->cdr.
          ENDWHILE.
          IF lo_ptr->type = type_symbol AND lo_ptr->value = mv_label.
            lo_last_pair->cdr = me.
          ENDIF.

        WHEN OTHERS.
          RETURN.
      ENDCASE.
    ENDMETHOD.

    METHOD values_to_string.
      DATA lo_values TYPE REF TO lcl_lisp_values.
      DATA lo_elem TYPE REF TO lcl_lisp.
      DATA lv_str TYPE string.

      lo_values ?= me.
      lo_elem = lo_values->head.

      WHILE lo_elem IS BOUND AND lo_elem NE nil.
        _validate lo_elem->car.
        lv_str = lo_elem->car->to_string( ).
        IF str IS INITIAL.
          str = lv_str.
        ELSE.
          str &&= ` ` && lv_str.
        ENDIF.

        lo_elem = lo_elem->cdr.
      ENDWHILE.

    ENDMETHOD.

    METHOD list_to_string.
      DATA lv_str TYPE string.
      DATA lv_skip TYPE tv_flag.
      DATA lv_separator TYPE string.
      DATA lv_parens TYPE tv_flag.
      DATA lv_first TYPE tv_flag VALUE abap_true.
      DATA lo_elem TYPE REF TO lcl_lisp.
      DATA lo_fast TYPE REF TO lcl_lisp.
      DATA lv_shared TYPE i VALUE -1.

      lo_elem = me.
      lo_fast = cdr.

      lv_parens = abap_true.
      WHILE lo_elem IS BOUND AND lo_elem NE nil.

*       Quasiquoting output (quasiquote x) is displayed as `x without parenthesis
        lv_skip = abap_false.
        IF lv_first EQ abap_true AND lo_elem->type EQ type_pair.
          lv_first = abap_false.

          format_quasiquote( EXPORTING io_elem = lo_elem
                             IMPORTING ev_skip = lv_skip
                                       ev_str = lv_str ).
          IF lv_skip EQ abap_true.
            lv_parens = abap_false.
          ENDIF.
        ENDIF.

        IF lo_fast IS BOUND AND lo_fast->type EQ type_pair.
          lo_fast = lo_fast->cdr.

          IF lo_fast IS BOUND AND lo_fast->type EQ type_pair AND lo_elem NE lo_fast.
            lo_fast = lo_fast->cdr.
          ENDIF.

          IF lo_elem = lo_fast.
*           Circular list
            ADD 1 TO lv_shared.
            WHILE lo_elem->cdr NE lo_fast.
              lv_str &&= lv_separator && lo_elem->car->to_string( ).
              lo_elem = lo_elem->cdr.
              lv_separator = ` `.
            ENDWHILE.
            lv_str &&= lv_separator && |{ lo_elem->car->to_string( ) } . #{ lv_shared }#|.
            EXIT.
          ENDIF.

        ENDIF.

        IF lv_skip EQ abap_false.
          lv_str &&= lv_separator && COND string(
                       WHEN lo_elem->type NE type_pair     " If item is not a cons cell
                         THEN |. { lo_elem->to_string( ) }|      " indicate with dot notation:
                         ELSE |{ lo_elem->car->to_string( ) }| ).
          lv_separator = ` `.
        ENDIF.

        lo_elem = lo_elem->cdr.

      ENDWHILE.

      IF lv_parens EQ abap_true.
        str = |{ c_open_paren }{ lv_str }{ c_close_paren }|.
      ELSE.
*       Quasiquoting output
        str = lv_str.
      ENDIF.

      IF lv_shared GE 0.
        str = |#{ lv_shared } = { str }|.
      ENDIF.

    ENDMETHOD.                    "list_to_string

    METHOD format_string.
      " give back the string as a quoted string
      str = |"{ escape( val = value format = cl_abap_format=>e_html_js ) }"|.
      " cl_abap_format=>e_html_js replace table
      "  " -> \"
      "  ' -> \'
      "  alarm -> \x07
      "  BS    -> \b    " \x08
      "  TAB   -> \t    " \x09
      "  LF    -> \n    " \x0A
      "  CR    -> \r    " \x0D
      " ctrl-char -> \xhh

      " Target Replace table
      "  " -> \"
      "  x07 alarm -> \a
      "  x08 BS -> \b
      "  x09 TAB -> \t
      "  x0A LF -> \n
      "  x0D CR -> \r
      " ctrl-char -> \xhh

      " Manual replacements needed
      REPLACE ALL OCCURRENCES OF:
         `\'` IN str WITH `'`,
         '\x07' IN str WITH '\a'.
    ENDMETHOD.

    METHOD display_string.
      str = value.
    ENDMETHOD.

    METHOD to_string.
      CASE type.
        WHEN type_lambda.
          str = |<lambda> { car->list_to_string( ) }|.
        WHEN type_null.
          str = c_lisp_nil.

        WHEN type_syntax
          OR type_primitive
          OR type_boolean.
          str = value.

        WHEN type_symbol.
          CASE me.
            WHEN unquote.
              str = c_lisp_unquote.
            WHEN quasiquote.
              str = c_lisp_backquote.
            WHEN unquote_splicing.
              str = c_lisp_unquote_splicing.

            WHEN OTHERS.
              str = value.
          ENDCASE.

        WHEN type_not_defined.
          str = c_undefined.

        WHEN type_string.
          IF me EQ lcl_lisp=>new_line.
            str = |\n|.
          ELSE.
            str = format_string( value ).
          ENDIF.

        WHEN type_char.
          CASE me.
            WHEN lcl_lisp=>new_line.
              str = |\n|.
            WHEN lcl_lisp=>char_alarm.
              str = |\\a|.
            WHEN OTHERS.
              IF is_eof( ).
                str = '<eof>'.
              ELSE.
                str = format_string( value ).
              ENDIF.
          ENDCASE.

        WHEN type_integer
          OR type_rational
          OR type_real
          OR type_complex.
          str = CAST lcl_lisp_number( me )->number_to_string( iv_radix = 10 ).

        WHEN type_native.
          str = |<native> { to_lower( value ) }|.
        WHEN type_pair.
          str = list_to_string( ).
        WHEN type_hash.
          str = '<hash>'.
        WHEN type_vector.
          DATA lo_vec TYPE REF TO lcl_lisp_vector.
          lo_vec ?= me.
          str = |#{ lo_vec->to_list( )->to_string( ) }|.

          "WHEN bytevector.

        WHEN type_port.
          str = '<port>'.
        WHEN type_values.
          str = values_to_string( ).

*--------------------------------------------------------------------*
*        Additions for ABAP Types:
        WHEN type_abap_function.
          str = |<ABAP function module { value }>|.
*          TODO
*        WHEN type_abap_class.
*          str = |<ABAP class { value }>|.
*        WHEN type_abap_method.
*          str = |<ABAP method { car->value }->{ cdr->value }( ) >|.
        WHEN type_abap_data.
          str = |<ABAP Data>|.
        WHEN type_abap_table.
          str = |<ABAP Table>|.
        WHEN type_abap_query.
          str = |<ABAP Query>|.
        WHEN type_abap_sql_set.
          str = |<ABAP Query Result Set>|.

        WHEN type_abap_turtle.
          str = |<ABAP turtle>|.
      ENDCASE.
    ENDMETHOD.                    "to_string

    METHOD to_text.
      CASE type.
        WHEN type_string OR type_char.
          CASE me.
            WHEN lcl_lisp=>char_linefeed
              OR lcl_lisp=>new_line.
              str = |\n|.
            WHEN OTHERS.
              IF is_eof( ).
                str = space.
              ELSE.
                str = display_string( value ).
              ENDIF.
          ENDCASE.

        WHEN type_null OR type_not_defined.
          str = space.
        WHEN OTHERS.
          str = to_string( ).
      ENDCASE.
    ENDMETHOD.

    METHOD throw.
      RAISE EXCEPTION TYPE lcx_lisp_exception
        EXPORTING
          message = message
          area    = area.
    ENDMETHOD.

    METHOD radix_throw.
      RAISE EXCEPTION TYPE lcx_lisp_radix
        EXPORTING
          message = message.
    ENDMETHOD.

    METHOD raise.
      throw( context && to_string( ) && message ).
    ENDMETHOD.

    METHOD raise_nan.
      raise( | is not a number in [{ operation }]| ).
    ENDMETHOD.

    METHOD no_complex.
      raise( | complex number not allowed in [{ operation }]| ).
    ENDMETHOD.

    METHOD throw_no_number.
      DATA lx_no_number TYPE REF TO cx_sy_conversion_no_number.

      CREATE OBJECT lx_no_number
        EXPORTING
          textid = '995DB739AB5CE919E10000000A11447B'
          value  = message.   " The argument #D cannot be interpreted as a number
      "throw( lx_no_number->get_text( ) ).
      RAISE EXCEPTION lx_no_number.
    ENDMETHOD.

    METHOD assert_last_param.
      CHECK cdr NE nil.
      raise( | Parameter mismatch| ).
    ENDMETHOD.

    METHOD error_not_a_list.
      raise( context = context && `: `
             message = ` is not a proper list` ).
    ENDMETHOD.

    METHOD is_eof.
      flag = xsdbool( me = eof_object ).
    ENDMETHOD.

    METHOD is_procedure.
      CASE type.
        WHEN type_lambda
          OR type_native
          OR type_primitive
          OR type_escape_proc
          OR type_abap_function.  " really?
          result = abap_true.
        WHEN OTHERS.
          result = abap_false.
      ENDCASE.
    ENDMETHOD.

    METHOD is_number.
      CASE type.
        WHEN type_integer
          OR type_rational
          OR type_real
          OR type_complex.
          result = abap_true.
        WHEN OTHERS.
          result = abap_false.
      ENDCASE.
    ENDMETHOD.

    METHOD is_nan.
      CASE type.
        WHEN type_integer
          OR type_rational
          OR type_real
          OR type_complex.
          flag = xsdbool( me = lcl_lisp_number=>nan OR me = lcl_lisp_number=>neg_nan ).
        WHEN OTHERS.
          raise_nan( `nan?` ).
      ENDCASE.
    ENDMETHOD.

  ENDCLASS.                    "lcl_lisp IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_iterator IMPLEMENTATION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_iterator IMPLEMENTATION.

    METHOD constructor.
      elem = io_elem.
      first = abap_true.
    ENDMETHOD.

    METHOD has_next.
*     if the last element in the list is not a cons cell, we cannot append
      rv_flag = xsdbool( elem NE lcl_lisp=>nil AND
               ( first EQ abap_true OR ( elem->cdr IS BOUND AND elem->cdr NE lcl_lisp=>nil ) ) ).
    ENDMETHOD.                    "has_next

    METHOD next.
      IF first EQ abap_true.
        first = abap_false.
      ELSE.
        IF elem->cdr->type NE type_pair.
          elem->raise( | is not a proper list| ).
        ENDIF.
        elem = elem->cdr.
      ENDIF.
      ro_elem = elem->car.
    ENDMETHOD.                    "next

  ENDCLASS.                    "lcl_lisp_iterator IMPLEMENTATION

  CLASS lcl_lisp_new IMPLEMENTATION.

    METHOD elem.
      CASE type.
        WHEN type_integer.
          ro_elem = integer( value ).

        WHEN type_real.
          ro_elem = real( value = value
                          iv_exact = abap_false ).

        WHEN type_string.
          ro_elem = string( value ).

        WHEN type_char.
          ro_elem = char( value ).

        WHEN type_boolean.
          ro_elem = boolean( value ).

        WHEN type_abap_data.
          ro_elem = data( value ).

        WHEN type_abap_table.
          ro_elem = table( value ).

        WHEN OTHERS.
          CREATE OBJECT ro_elem
            EXPORTING
              type = type.
          ro_elem->value = value.

      ENDCASE.
    ENDMETHOD.

    METHOD string.
      ro_elem = NEW lcl_lisp_string( value = value
                                     iv_mutable = iv_mutable ).
    ENDMETHOD.

    METHOD char.
      ro_elem = lcl_lisp_char=>new( value ).
    ENDMETHOD.

    METHOD eof.
      ro_elem = lcl_lisp=>eof_object.
      "ro_elem = lcl_lisp_char=>new( c_lisp_eof ).
    ENDMETHOD.

    METHOD esc_charx.
      DATA lv_numc TYPE tv_hex04.
      DATA lv_str TYPE string.

      lv_str = to_upper( value ).
      DATA(lv_times) = 4 - numofchar( lv_str ).

      IF lv_times GE 0 AND lv_str CO c_hex_digits.
        lv_numc = lv_str.
        DO lv_times TIMES.
          lv_numc = '0' && lv_numc.   " insert leading zero
        ENDDO.

        ro_elem = xchar( lv_numc ).
      ELSE.
        lcl_lisp=>throw( |unknown char #\\x{ value } found| ).
      ENDIF.
    ENDMETHOD.

    METHOD xchar.
      FIELD-SYMBOLS <xword> TYPE x.
      FIELD-SYMBOLS <xchar> TYPE x.
      DATA xword TYPE tv_xword.
      DATA lv_char TYPE tv_char.
      DATA lv_int TYPE int4.  " int2 does not handle EOF

      xword = value.
      lv_int = xword.
      ASSIGN lv_int TO <xword> CASTING.
      ASSIGN lv_char TO <xchar> CASTING.
      <xchar> = <xword>.

      ro_elem = char( lv_char ).
    ENDMETHOD.

    METHOD match_initial.
      " <initial> -> <letter> | <special initial>
      match = xsdbool( initial CO c_abcde  " letter
                    OR initial CO c_special_initial ).
    ENDMETHOD.

    METHOD match_subsequent_list.
      DATA idx TYPE i.
      DATA char TYPE tv_char.

      DATA(len) = strlen( value ).
      match = abap_false.

      WHILE idx < len.
        char = value+idx(1).
        idx = idx + 1.
        " <subsequent> -> <initial> | <digit> | <special subsequent>
        " <special subsequent> -> <explicit sign> | . | @
        IF NOT ( match_initial( char )
              OR char CO c_decimal_digits
              OR char CO c_explicit_sign
              OR char EQ c_lisp_dot
              OR char EQ c_lisp_splicing ).
          RETURN.
        ENDIF.
      ENDWHILE.
      match = abap_true.
    ENDMETHOD.

    METHOD match_sign_subsequent.
      " <sign subsequent> -> <initial> | <explicit sign> | @
      match = xsdbool( match_initial( initial )
                    OR initial CO c_explicit_sign
                    OR initial EQ c_lisp_splicing ).
    ENDMETHOD.

    METHOD match_dot_subsequent.
      " <dot subsequent -> <sign subsequent> | .
      match = xsdbool( match_sign_subsequent( initial )  OR initial EQ c_lisp_dot ).
    ENDMETHOD.

    METHOD match_peculiar_identifier.
      " <peculiar identifier> -> <explicit sign>
      "    | <explicit sign> <sign subsequent> <subsequent>*
      "    | <explicit sign> . <dot subsequent> <subsequent>*
      "    | . <dot subsequent> <subsequent>*
      DATA char TYPE tv_char.
      DATA idx TYPE i.
      DATA len TYPE i.
      DATA rest TYPE string.

      match = abap_false.
      len = strlen( value ).
      CHECK len GT 0.

      char = value+idx(1).
      idx = idx + 1.

      IF char CO c_explicit_sign.
        " <peculiar identifier> -> <explicit sign>
        IF idx = len.
          match = abap_true.
          RETURN.
        ENDIF.

        " <peculiar identifier> -> <explicit sign> <sign subsequent> <subsequent>*
        char = value+idx(1).
        idx = idx + 1.
        IF match_sign_subsequent( char ).

          rest = value+idx.
          match = match_subsequent_list( rest ).

          " <peculiar identifier> -> <explicit sign> . <dot subsequent> <subsequent>*
        ELSEIF char EQ c_lisp_dot.

          IF idx = len.
            RETURN.
          ELSE.
            char = value+idx(1).
            idx = idx + 1.
          ENDIF.

          IF match_dot_subsequent( char ).

            IF idx = len.
              match = abap_true.
              RETURN.
            ENDIF.

            rest = value+idx.
            match = match_subsequent_list( rest ).
          ENDIF.

        ENDIF.

        " <peculiar identifier> -> . <dot subsequent> <subsequent>*
      ELSEIF char EQ c_lisp_dot.

        IF idx = len.
          RETURN.
        ELSE.
          char = value+idx(1).
          idx = idx + 1.
        ENDIF.

        IF match_dot_subsequent( char ).

          IF idx = len.
            match = abap_true.
            RETURN.
          ENDIF.

          rest = value+idx.
          match = match_subsequent_list( rest ).

        ENDIF.

      ENDIF.

    ENDMETHOD.

    METHOD atom.
      " <atom> -> <identifier> | <number>
      DATA lv_upcase TYPE string.
      DATA initial TYPE tv_char.

      IF strlen( value ) LE 0.
        lcl_lisp=>throw( |Missing Identifier| ).
      ENDIF.

      " +i, -i and <infnan> are parsed as number, not as peculiar identifier
      ro_elem = lcl_lisp_number=>atom_to_number( value ).
      CHECK ro_elem IS INITIAL.

      lv_upcase = to_upper( value ).

      initial = lv_upcase+0(1).
      " <identifier> -> <initial> <subsequent>* |  <vertical line> <symbol element>* <vertical line> | <peculiar identifier>
      IF match_initial( initial ) OR initial EQ c_vertical_line OR match_peculiar_identifier( value ).
        ro_elem = identifier( value ).
      ELSE.
        TRY.
            ro_elem = read_complex( iv_radix = 10
                                    iv_radix_char = space
                                    iv_exact_char = space
                                    io_stream =  NEW lcl_stream( NEW lcl_string_stream( value ) ) ).

          CATCH cx_sy_conversion_no_number.
            lcl_lisp=>throw( `Invalid number ` && value ).
        ENDTRY.

      ENDIF.

    ENDMETHOD.

    METHOD identifier.
      " <identifier> -> <initial> <subsequent>* |  <vertical line> <symbol element>* <vertical line> | <peculiar identifier>
      " <initial> -> <letter> | <special initial>
      " <letter> -> a | b | c | ... | z | A | B | C | ... | Z
      " <special initial> -> ! | $ | % | & | * | / | : | < | = | > | ? | @ | ^ | _ | ~
      " <subsequent> -> <initial> | <digit> | <special subsequent>
      " <digit> -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
      " <hex digit> -> <digit> | a | b | c | d | e | f
      " <explicit sign> -> + | -
      " <special subsequent> -> <explicit sign> | . | @
      " <inline hex escape> -> \x<hex scalar value>

      DATA lv_char TYPE tv_char.
      DATA lv_symbol TYPE string.
      DATA lv_index TYPE sytabix.
      DATA lv_len_1 TYPE i.
      DATA lv_escape_mode TYPE tv_flag.
      DATA lv_escape_hex_mode TYPE tv_flag.
      DATA lv_escape_char TYPE string.

      lv_len_1 = numofchar( value ) - 1.
      IF lv_len_1 LT 0.
        lcl_lisp=>throw( |Missing Identifier| ).
      ENDIF.

      " initial
      lv_char = value+lv_index(1).
      IF to_upper( lv_char ) CO c_abcde.
        " letter
      ELSEIF lv_char CO c_special_initial.
        " special initial
      ELSEIF lv_char EQ c_vertical_line.
        " search <symbol element> ->
        " <any character other than <vertical line> or \>
        " | <inline hex escape> | <mnemonic escape> | \| | \" | \\
        DO lv_len_1 TIMES.
          lv_index = lv_index + 1.
          lv_char = value+lv_index(1).

          CASE lv_char.
            WHEN c_escape_char.
              IF lv_escape_mode = abap_false.
                lv_escape_mode = abap_true.
                lv_escape_hex_mode = abap_false.
                CLEAR lv_escape_char.
              ELSE.
                lcl_lisp=>throw( |Identifier { value } not valid.| ).
              ENDIF.
            WHEN c_semi_colon.
              IF lv_escape_hex_mode = abap_true.

                DATA(lo_char) = lcl_lisp_new=>esc_charx( lv_escape_char ).
                lv_char = lo_char->value.

                lv_escape_mode = abap_false.
                lv_escape_hex_mode = abap_false.
              ENDIF.
              CONCATENATE lv_symbol lv_char INTO lv_symbol RESPECTING BLANKS.

            WHEN c_vertical_line.
              IF lv_index NE lv_len_1.      "|2|s  len = 4, len_1 = 3
                lcl_lisp=>throw( |Identifier { value } not valid.| ).
              ENDIF.
            WHEN OTHERS.
              IF lv_escape_mode EQ abap_true.
                IF lv_escape_hex_mode EQ abap_true.
                  lv_char = to_upper( lv_char ).
                  CONCATENATE lv_escape_char lv_char INTO lv_escape_char RESPECTING BLANKS.
                ELSE.
                  CASE lv_char.
                    WHEN 'x' OR 'X'.
                      lv_escape_hex_mode = abap_true.

                    WHEN 't'.
                      lv_char = lcl_lisp=>char_tab->value+0(1).
                      lv_symbol &&= lv_char.  " CONCATENATE lv_symbol lv_char INTO lv_symbol RESPECTING BLANKS.
                      lv_escape_mode = abap_false.
                    WHEN 'n'.
                      lv_char = lcl_lisp=>char_linefeed->value+0(1).
                      lv_symbol &&= lv_char.  " CONCATENATE lv_symbol lv_char INTO lv_symbol RESPECTING BLANKS.
                      lv_escape_mode = abap_false.
                    WHEN 'r'.
                      lv_char = lcl_lisp=>char_return->value+0(1).
                      lv_symbol &&= lv_char.  " CONCATENATE lv_symbol lv_char INTO lv_symbol RESPECTING BLANKS.
                      lv_escape_mode = abap_false.
                    WHEN 'a'.
                      lv_char = lcl_lisp=>char_alarm->value+0(1).
                      lv_symbol &&= lv_char.  " CONCATENATE lv_symbol lv_char INTO lv_symbol RESPECTING BLANKS.
                      lv_escape_mode = abap_false.
                    WHEN 'b'.
                      lv_char = lcl_lisp=>char_backspace->value+0(1).
                      lv_symbol &&= lv_char.  " CONCATENATE lv_symbol lv_char INTO lv_symbol RESPECTING BLANKS.
                      lv_escape_mode = abap_false.
                  ENDCASE.
                ENDIF.
              ELSE.
                CONCATENATE lv_symbol lv_char INTO lv_symbol RESPECTING BLANKS.
              ENDIF.
          ENDCASE.
        ENDDO.
        IF lv_char = c_vertical_line.
          " lv_symbol might be empty,  || is valid
          ro_elem = symbol( value = lv_symbol
                            index = index ).
          RETURN.
        ELSE.
          lcl_lisp=>throw( |Identifier { value } not valid.| ).
        ENDIF.

      ELSE.
        "<peculiar identifier> -> <explicit sign>
        "  | <explicit sign> <sign subsequent> <subsequent>*
        "  | <explicit sign> . <hdot subsequent> subsequent>*
        "  | . <dot subsequent> <subsequent>*
        "<dot subsequent> -> <sign subsequent> | .
        "<sign subsequent> -> <initial> | <explicit sign> | @

        " if peculiar identifier
        IF lv_char CO c_explicit_sign.
          "##TO DO
        ELSEIF lv_char EQ c_lisp_dot.
          "##TO DO
        ELSE.
          " if not peculiar identifier
          lcl_lisp=>throw( |Identifier { value } not valid.| ).
        ENDIF.
        ro_elem = symbol( value = value
                          index = index ).
        RETURN.
      ENDIF.
      " subsequent
      DO lv_len_1 TIMES.
        lv_index += 1.
        lv_char = value+lv_index(1).

        IF to_upper( lv_char ) CO c_abcde.
          " letter
        ELSEIF lv_char CO c_special_initial.
          " special initial
        ELSEIF lv_char CO c_decimal_digits.
          " digit
        ELSEIF lv_char CO c_explicit_sign.
          " special subsequent
        ELSEIF lv_char CO c_lisp_dot.
          " special subsequent
        ELSEIF lv_char CO c_lisp_splicing.
          " special subsequent
        ELSE.
          lcl_lisp=>throw( |Identifier { value } not valid.| ).
        ENDIF.
      ENDDO.
      ro_elem = symbol( value = value
                        index = index ).
    ENDMETHOD.

    METHOD symbol.
      ro_elem = NEW lcl_lisp_symbol( value = value
                                     index = index ).
    ENDMETHOD.

    METHOD boolean.
      ro_elem = NEW lcl_lisp_boolean( value ).
    ENDMETHOD.

    METHOD null.
      ro_elem = NEW lcl_lisp_null( type_null ).
      ro_elem->value = c_lisp_nil.
      ro_elem->mutable = abap_false.
    ENDMETHOD.

    METHOD undefined.
      ro_elem = NEW lcl_lisp( type_not_defined ).
    ENDMETHOD.

    METHOD integer.
      ro_elem = NEW lcl_lisp_integer( value = value
                                      iv_exact = iv_exact ).
    ENDMETHOD.

    METHOD real.
      ro_elem = NEW lcl_lisp_real( value = value
                                   exact = iv_exact
                                   finite = finite ).
    ENDMETHOD.

    METHOD rectangular.
      ro_elem = lcl_lisp_complex=>new_rectangular( x = real
                                                   y = imag ).
    ENDMETHOD.

    METHOD polar.
      ro_elem = lcl_lisp_complex=>new_polar( r = r
                                             angle = angle ).
    ENDMETHOD.

    METHOD real_number.
      DATA lv_nummer_str TYPE string.
      DATA lv_denom TYPE tv_int.

      DATA lv_real TYPE tv_real.
      DATA lv_int TYPE tv_int.
      DATA lv_exact TYPE tv_flag.

      TRY.
*         Check whether the token can be converted to a float
*         use ABAP string -> number conversion to cover all manner of number formats, including scientific
          lv_real = value.
          lv_exact = iv_exact.

          IF iv_exact IS NOT SUPPLIED OR iv_exact EQ abap_true.
              " check integer?
              TRY.
                  lv_nummer_str = value.

                  IF lv_nummer_str NA c_pattern_inexact OR iv_exact EQ abap_true.
                    MOVE EXACT lv_real TO lv_int.

                    ro_elem = integer( value = lv_int
                                       iv_exact = abap_true ).
                    RETURN.
                  ENDIF.
                CATCH cx_sy_conversion_error ##NO_HANDLER.
              ENDTRY.

              " check rational?
              IF lv_nummer_str CA c_pattern_inexact AND iv_exact EQ abap_true.

                DATA lv_int_str TYPE string.
                DATA lv_dec_str TYPE string.

                SPLIT lv_nummer_str AT c_lisp_dot INTO lv_int_str lv_dec_str.
                lv_int_str &&= lv_dec_str.
                TRY.
                    lv_int = lv_int_str.
                    lv_denom = ipow( base = 10 exp = numofchar( lv_dec_str ) ).
                    ro_elem = rational( nummer = lv_int
                                        denom = lv_denom
                                        iv_exact = iv_exact ).
                    RETURN.
                  CATCH cx_sy_conversion_overflow cx_sy_conversion_no_number.
                    lv_exact = abap_false.      " force inexact
                ENDTRY.
              ENDIF.

          ENDIF.

          ro_elem = real( value = lv_real
                          iv_exact = lv_exact ).

        CATCH cx_sy_conversion_error INTO DATA(lx_conv_error).
          RAISE EXCEPTION TYPE cx_sy_conversion_no_number
            EXPORTING
              value = value.
      ENDTRY.

    ENDMETHOD.

    METHOD numeric.
      CASE record-type.
        WHEN type_integer
          OR type_rational
          OR type_real.
          ro_elem = create_real( record-real_part ).

        WHEN type_complex.
          ro_elem = rectangular( real = create_real( record-real_part )
                                 imag = create_real( record-imag_part ) ).

        WHEN OTHERS.
          lcl_lisp=>throw( |Error in result of [{ record-operation }]| ).
      ENDCASE.
    ENDMETHOD.

    METHOD create_real.
      CASE record-subtype.
        WHEN type_integer.
          ro_elem = integer( value = record-int
                             iv_exact = record-exact ).

        WHEN type_rational.
          IF record-denom EQ 1.
            ro_elem = integer( value = record-nummer
                               iv_exact = record-exact ).
          ELSE.
            ro_elem = rational( nummer = record-nummer
                                denom = record-denom
                                iv_exact = record-exact ).
          ENDIF.

        WHEN type_real.
          IF record-infnan EQ abap_false.
            ro_elem = real( value = record-real
                            iv_exact = record-exact ).
          ELSE.
            ro_elem = record-ref.
          ENDIF.

        WHEN OTHERS.
          lcl_lisp=>throw_no_number( `NaN` ).
      ENDCASE.
    ENDMETHOD.

    METHOD get_radix.
      CASE to_lower( iv_radix_char ).
        WHEN c_number_octal+0(1).     "#o (octal)       <radix 8> -> #o #O (octal)
          rv_radix = 8.
        WHEN c_number_binary+0(1).    "#b (binary)      <radix 2> -> #b #B (binary)
          rv_radix = 2.
        WHEN c_number_decimal+0(1).   "#d (decimal)     <radix 10> -> <empty> | #d #D (decimal)
          rv_radix = 10.
        WHEN c_number_hex+0(1).       "#x (hexadecimal) <radix 16> -> #x  #X (hexadecimal)
          rv_radix = 16.
        WHEN OTHERS.
          rv_radix = iv_radix.
      ENDCASE.
    ENDMETHOD.

    METHOD digits_radix.
      CASE iv_radix.
        WHEN 8.
          rv_digits = c_octal_digits.
        WHEN 2.
          rv_digits = c_binary_digits.
        WHEN 10.
          rv_digits = c_decimal_digits.
        WHEN 16.    " hexadecimal
          rv_digits = c_hex_digits_long.
        WHEN OTHERS.
          throw_radix( |Invalid radix ({ iv_radix }) must be 2, 8, 10 or 16 in string->number| ).
      ENDCASE.
    ENDMETHOD.

    METHOD read_complex.
      DATA(lv_digits_domain) = digits_radix( get_radix( iv_radix_char = iv_radix_char
                                                        iv_radix = iv_radix ) ).

      TRY.
          element = io_stream->read_complex( iv_exact_char = iv_exact_char
                                             domain = lv_digits_domain ).

        CATCH cx_sy_conversion_no_number INTO DATA(lx_error).
          lcl_lisp=>throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD hex_integer.
      CONSTANTS c_radix TYPE i VALUE 16.
      DATA lv_text TYPE string VALUE '0000000000000000'. " 2x8 = 16
      DATA lv_len TYPE tv_int.
      DATA lv_hex TYPE x LENGTH 8.

      lv_len = c_radix - strlen( value ).
      IF lv_len GT 0.
        lv_text = lv_text+0(lv_len) && value.
      ELSE.
        lv_text = value.
      ENDIF.
      IF lv_text CA c_hex_alpha_lowercase.
        lv_text = to_upper( lv_text ).
      ENDIF.
      IF lv_text CO c_hex_digits.
        lv_hex = lv_text.
        rv_int = lv_hex.
      ELSE.
        throw_radix( `Invalid hexadecimal number ` && value ).
      ENDIF.
    ENDMETHOD.

    METHOD hex.
      DATA lv_text TYPE string.

      DATA lv_trunc_str TYPE string.
      DATA lv_decimal_str TYPE string.
      DATA lv_int TYPE tv_int.
      DATA lv_exp TYPE tv_int.
      DATA lv_dec TYPE tv_int.
      DATA lv_real TYPE tv_real.

      lv_text = value.
      SPLIT lv_text AT c_lisp_dot INTO lv_trunc_str lv_decimal_str.

      lv_int = hex_integer( lv_trunc_str ).
      IF lv_decimal_str IS INITIAL.
        ro_elem = integer( value = lv_int
                           iv_exact = iv_exact ).
      ELSE.
        lv_dec = hex_integer( lv_decimal_str ).
        lv_real = lv_int.
        IF lv_dec EQ 0.
          ro_elem = real( value = lv_real
                          iv_exact = iv_exact ).
        ELSE.
          lv_exp = numofchar( lv_decimal_str ).
          ro_elem = real( value = lv_real + lv_dec / ipow( base = 16 exp = lv_exp )
                          iv_exact = iv_exact ).
        ENDIF.
      ENDIF.
    ENDMETHOD.

    METHOD octal_integer.
      CONSTANTS c_radix TYPE i VALUE 8.
      DATA lv_text TYPE string.
      DATA lv_index TYPE sytabix.
      DATA lv_size TYPE sytabix.
      DATA lv_char TYPE tv_char.
      DATA lv_scale TYPE i VALUE 1.

      CLEAR rv_int.
      lv_text = value.
      IF lv_text CN c_octal_digits.
        throw_radix( `Invalid octal number` ).
      ENDIF.

      lv_index = lv_size = strlen( lv_text ).
      DO lv_size TIMES.
        SUBTRACT 1 FROM lv_index.
        lv_char = lv_text+lv_index(1).

        rv_int = rv_int + lv_char * lv_scale.
        lv_scale = lv_scale * c_radix.
      ENDDO.
    ENDMETHOD.

    METHOD octal.
      DATA lv_text TYPE string.

      DATA lv_trunc_str TYPE string.
      DATA lv_decimal_str TYPE string.
      DATA lv_int TYPE tv_int.
      DATA lv_exp TYPE tv_int.
      DATA lv_real TYPE tv_real.
      DATA lv_dec TYPE tv_int.

      lv_text = value.
      SPLIT lv_text AT c_lisp_dot INTO lv_trunc_str lv_decimal_str.

      lv_int = octal_integer( lv_trunc_str ).
      IF lv_decimal_str IS INITIAL.
        ro_elem = integer( value = lv_int
                           iv_exact = iv_exact ).
      ELSE.
        lv_dec = octal_integer( lv_decimal_str ).
        lv_real = lv_int.
        IF lv_dec EQ 0.
          ro_elem = real( value = lv_real
                          iv_exact = iv_exact ).
        ELSE.
          lv_exp = numofchar( lv_decimal_str ).
          ro_elem = real( value = lv_real + lv_dec / ipow( base = 8 exp = lv_exp )
                          iv_exact = iv_exact ).
        ENDIF.
      ENDIF.
    ENDMETHOD.

    METHOD binary_integer.
      CONSTANTS c_radix TYPE i VALUE 2.
      DATA lv_text TYPE string.
      DATA lv_index TYPE sytabix.
      DATA lv_size TYPE sytabix.
      DATA lv_char TYPE tv_char.
      DATA lv_scale TYPE i VALUE 1.

      lv_text = value.
      IF lv_text CN c_binary_digits.
        throw_radix( `Invalid binary number` ).
      ENDIF.

      lv_index = lv_size = strlen( lv_text ).
      DO lv_size TIMES.
        SUBTRACT 1 FROM lv_index.
        lv_char = lv_text+lv_index(1).

        IF lv_char EQ '1'.
          rv_int = rv_int + lv_scale.
        ENDIF.
        lv_scale = lv_scale * c_radix.
      ENDDO.
    ENDMETHOD.

    METHOD binary.
      DATA lv_text TYPE string.

      DATA lv_trunc_str TYPE string.
      DATA lv_decimal_str TYPE string.
      DATA lv_int TYPE tv_int.
      DATA lv_exp TYPE tv_int.
      DATA lv_frac_bin TYPE tv_int.
      DATA lv_frac_real TYPE tv_real.

      lv_text = value.
      SPLIT lv_text AT c_lisp_dot INTO lv_trunc_str lv_decimal_str.

      lv_int = binary_integer( lv_trunc_str ).
      IF lv_decimal_str IS INITIAL.
        ro_elem = integer( value = lv_int
                           iv_exact = iv_exact ).
      ELSE.
        lv_frac_bin = binary_integer( lv_decimal_str ).

        IF lv_frac_bin EQ 0.
          lv_frac_real = 0.
        ELSE.
          lv_exp = numofchar( lv_decimal_str ).
          lv_frac_real = lv_frac_bin / ipow( base = 2 exp = lv_exp ).
        ENDIF.
        ro_elem = real( value = lv_int + lv_frac_real
                        iv_exact = iv_exact ).
      ENDIF.
    ENDMETHOD.

    METHOD port.
      IF iv_buffered EQ abap_true.
        CREATE OBJECT ro_port TYPE lcl_lisp_buffered_port
          EXPORTING
            iv_port_type = iv_port_type
            iv_input     = iv_input
            iv_output    = iv_output
            iv_error     = iv_error
            iv_separator = iv_separator
            iv_string    = iv_string.
      ELSE.
        CREATE OBJECT ro_port
          EXPORTING
            iv_port_type = iv_port_type
            iv_input     = iv_input
            iv_output    = iv_output
            iv_error     = iv_error.
      ENDIF.
    ENDMETHOD.

    METHOD rational.
      ro_elem = lcl_lisp_rational=>new( nummer = nummer
                                        denom = denom
                                        iv_exact = iv_exact ).
    ENDMETHOD.

    METHOD data.
      ro_elem = NEW lcl_lisp_data( type_abap_data ).
      ro_elem->data = ref.
    ENDMETHOD.                    "new_data

    METHOD table.
      ro_elem = NEW lcl_lisp_table( type_abap_table ).
      ro_elem->data = ref.
    ENDMETHOD.                    "new_table

    METHOD query.
      TRY.
          ro_elem = NEW lcl_lisp_query( ).
        CATCH cx_sql_exception.
          ro_elem = lcl_lisp=>nil.
      ENDTRY.
    ENDMETHOD.

    METHOD result_set.
      ro_elem = NEW lcl_lisp_sql_result( io_result ).
    ENDMETHOD.

    METHOD cons.
      ro_cons = NEW lcl_lisp_pair( type_pair ).
      ro_cons->car = io_car.
      ro_cons->cdr = io_cdr.
    ENDMETHOD.                    "new_cons

    METHOD promise.
      " (define make-promise
      "     (lambda (done? proc)
      "       (list (cons done? proc))))
*      ro_elem = lambda( io_car = done
*                        io_cdr = box( io_proc = cons( io_car = done
*                                                      io_cdr = proc ) )
*                        io_env = io_env ).
    ENDMETHOD.                    "new_cons

    METHOD box.
      ro_elem = cons( io_car = io_proc
                      io_cdr = cons( io_car = io_elem )  ).
    ENDMETHOD.

    METHOD list3.
      ro_cons = cons( io_car = io_first
                      io_cdr = box( io_proc = io_second
                                    io_elem = io_third ) ).
    ENDMETHOD.

    METHOD vector.
      ro_vec = NEW lcl_lisp_vector( type_vector ).
      ro_vec->array = it_vector.
      ro_vec->mutable = iv_mutable.
      ro_vec->mo_length = integer( lines( it_vector ) ).
    ENDMETHOD.

    METHOD bytevector.
      ro_u8 = NEW lcl_lisp_bytevector( type_bytevector ).
      ro_u8->bytes = it_byte.
      ro_u8->mutable = iv_mutable.
      ro_u8->mo_length = integer( lines( it_byte ) ).
    ENDMETHOD.

    METHOD values.
      ro_elem = NEW lcl_lisp_values( io_elem ).
    ENDMETHOD.

    METHOD escape.
      " capture procedure - converts the implicit continuation passed to call-with-current-continuation
      " into some kind of Scheme object with unlimited extent (we use a special kind of procedure)
      DATA(lo_esc) = NEW lcl_lisp_escape( cont = value
                                          param = param ).
      ro_elem = box_quote( lo_esc ).
    ENDMETHOD.

    METHOD turtles.
      ro_turtle = NEW lcl_lisp_turtle( width = width
                                       height = height
                                       init_x = init_x
                                       init_y = init_y
                                       init_angle = init_angle ).
    ENDMETHOD.

    METHOD lambda.
*     The lambda is a special cell that stores a pointer to a list of parameters
*     and a pointer to a list which is the body to be evaluated later on
      DATA(lo_lambda) = NEW lcl_lisp_lambda( io_car = io_car               " List of parameters
                                             io_cdr = io_cdr               " Body
                                             iv_category = iv_category
                                             iv_parameter_object = iv_parameter_object ).
*     Store the reference to the environment in which the lambda was created (lexical scope)
*     e.g. if the lambda is created inside another lambda we want that environment to be present
*     when we evaluate the new lambda
      lo_lambda->environment = io_env.
      ro_lambda = lo_lambda.
    ENDMETHOD.                    "new_lambda

    METHOD case_lambda.
      IF it_clauses IS INITIAL.
        ro_lambda = lcl_lisp=>nil.
      ELSE.
        DATA(lo_lambda) = NEW lcl_lisp_case_lambda( type_case_lambda ).
        lo_lambda->clauses = it_clauses.
        ro_lambda = lo_lambda.
      ENDIF.
    ENDMETHOD.

    METHOD hash.
      _validate io_list.

      ro_hash = NEW lcl_lisp_hash( type_hash ).
      ro_hash->fill( io_list->car ).
    ENDMETHOD.

    METHOD quote.
      ro_elem = box( io_proc = lcl_lisp=>quote
                     io_elem = io_elem ).
      ro_elem->mutable = ro_elem->cdr->mutable = abap_false.
    ENDMETHOD.

    METHOD box_quote.
*     quote to avoid double eval
      ro_elem = cons( io_car = quote( io_elem ) ).
      ro_elem->cdr->mutable = abap_false.
    ENDMETHOD.

    METHOD unquote.
      ro_elem = box( io_proc = lcl_lisp=>unquote
                     io_elem = io_elem ).
      ro_elem->cdr->mutable = abap_true.
    ENDMETHOD.

    METHOD splice_unquote.
      ro_elem = box( io_proc = lcl_lisp=>unquote_splicing
                     io_elem = io_elem ).
      ro_elem->cdr->mutable = abap_false.  " true?
    ENDMETHOD.

    METHOD quasiquote.
      ro_elem = box( io_proc = lcl_lisp=>quasiquote
                     io_elem = io_elem ).
      ro_elem->cdr->mutable = abap_false.
    ENDMETHOD.

    METHOD function.
      _validate: io_list, io_list->car.

      ro_func = NEW lcl_lisp_abapfunction( type_abap_function ).
*     Determine the parameters of the function module to populate parameter table
      ro_func->value = ro_func->read_interface( io_list->car->value ).
    ENDMETHOD.

    METHOD throw_radix.
      lcl_lisp=>radix_throw(  message ).
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_real IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_real ).
      IF finite EQ abap_true.
        me->infnan = abap_false.
        real = value.
        IF exact EQ abap_true AND trunc( real ) = real.
          me->exact = abap_true.
        ELSE.
          me->exact = abap_false.
        ENDIF.
      ELSE.
        me->infnan = abap_true.
        me->value = value.
        CASE value.
          WHEN c_lisp_pos_inf.
            real = c_max_float.
          WHEN c_lisp_neg_inf.
            real = - c_max_float.
          WHEN c_lisp_pos_zero.
            real = c_min_float.
          WHEN c_lisp_neg_zero.
            real = - c_min_float.
        ENDCASE.
      ENDIF.
    ENDMETHOD.

    METHOD float_eq.
      CONSTANTS: c_epsilon    TYPE tv_real VALUE cl_abap_math=>min_decfloat16,
                 c_min_normal TYPE tv_real VALUE cl_abap_math=>min_decfloat34,
                 c_max_value  TYPE tv_real VALUE cl_abap_math=>max_decfloat34.
      DATA diff TYPE tv_real.
      DATA sum TYPE tv_real.
      DATA abs_a TYPE tv_real.
      DATA abs_b TYPE tv_real.

      result = abap_false.
      CHECK infnan EQ abap_false.
      IF real EQ iv_real.
        result = abap_true.
      ELSEIF exact EQ abap_false.
        abs_a = abs( real ).
        abs_b = abs( iv_real ).
        diff = abs( real - iv_real ).
        sum = abs_a + abs_b.

        IF real = 0 OR iv_real = 0 OR sum < c_min_normal.
*         real or iv_real is zero or both are extremely close to it
*         relative error is less meaningful here
          IF diff < c_epsilon * c_min_normal.
            result = abap_true.
          ELSE. " use relative error
            IF diff / nmin( val1 = sum val2 = c_max_value ) < c_epsilon.
              result = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDMETHOD.

    METHOD gcd.
      DATA num TYPE REF TO lcl_lisp_real.
      DATA den TYPE REF TO lcl_lisp_real.
      DATA lo_save TYPE REF TO lcl_lisp_real.

      num = lcl_lisp_new=>real( value = n iv_exact = abap_false ).
      den = lcl_lisp_new=>real( value = d iv_exact = abap_false ).
      WHILE NOT den->float_eq( 0 ).
        lo_save = den.
        TRY.
            den = lcl_lisp_new=>real( value = num->real MOD den->real
                                      iv_exact = abap_false  ).
*            den = NEW #( num->real - den->real * trunc( num->real / den->real ) ).
          CATCH cx_sy_arithmetic_error INTO DATA(lx_error).
            throw( lx_error->get_text( ) ).
        ENDTRY.
        num = lo_save.
      ENDWHILE.
      result = abs( num->real ).
    ENDMETHOD.

    METHOD to_rational.
      is_rational = abap_true.
      IF infnan EQ abap_false.
        IF abs( real ) GT 1.
          ev_denom = floor( c_max_int / real ).
          ev_nummer = scheme_round( real * ev_denom ).      "ev_nummer = trunc( real * ev_denom ).
        ELSE.
          ev_nummer = floor( c_max_int * real ).
          ev_denom = scheme_round( ev_nummer / real ).      "ev_denom = trunc( ev_nummer / real ).
        ENDIF.
        DATA(lv_gcd) = lcl_lisp_real=>gcd( n = ev_nummer
                                           d = ev_denom ).
        ev_nummer = scheme_round( ev_nummer / lv_gcd ).
        ev_denom = scheme_round( ev_denom / lv_gcd ).
      ELSE.
        is_rational = abap_false.
      ENDIF.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_number IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type ).
      mutable = abap_false.
      infnan = abap_false.
    ENDMETHOD.

    METHOD scheme_round.
      rdprec = round( val = float
                      dec = 0
                      mode = cl_abap_math=>round_half_even ).
    ENDMETHOD.

    METHOD class_constructor.
      zero = lcl_lisp_new=>integer( value = 0  iv_exact = abap_true ).        " exact 0
      one = lcl_lisp_new=>integer( value = 1 iv_exact = abap_true ).          " exact 1
      minus_one = lcl_lisp_new=>integer( value = -1 iv_exact = abap_true ).   " exact -1

      pos_zero = lcl_lisp_new=>real( value = c_lisp_pos_zero iv_exact = abap_false ).
      neg_zero = lcl_lisp_new=>real( value = c_lisp_neg_zero iv_exact = abap_false ).

      nan = lcl_lisp_new=>real( value = c_lisp_pos_nan iv_exact = abap_false finite = abap_false ).
      neg_nan = lcl_lisp_new=>real( value = c_lisp_neg_nan iv_exact = abap_false finite = abap_false ).

      inf = lcl_lisp_new=>real( value = c_lisp_pos_inf iv_exact = abap_false finite = abap_false ).
      neg_inf = lcl_lisp_new=>real( value = c_lisp_neg_inf iv_exact = abap_false finite = abap_false ).

      imaginary = lcl_lisp_new=>rectangular( imag = one ).
      imaginary_neg = lcl_lisp_new=>rectangular( imag = minus_one ).

      inf_imag = lcl_lisp_new=>rectangular( imag = inf ).
      neg_inf_imag = lcl_lisp_new=>rectangular( imag = neg_inf ).

      nan_imag = lcl_lisp_new=>rectangular( imag = nan ).
      neg_nan_imag = lcl_lisp_new=>rectangular( imag = neg_nan ).
    ENDMETHOD.

    METHOD absolute.
      DATA new_number TYPE tv_flag.

      DATA(res) = get_state( 'abs' ).

      CASE type.
        WHEN type_integer.
          IF res-int LT 0.
            new_number = abap_true.
            res = neg_sign( res ).
          ENDIF.

        WHEN type_rational.
          IF res-nummer LT 0 AND res-nummer GT 0
            OR ( res-nummer GT 0 AND res-nummer LT 0 ).  " should not happen
            new_number = abap_true.
            res = neg_sign( res ).
          ENDIF.

        WHEN type_real.
          IF ( res-infnan EQ abap_false AND res-real LT 0 )
            OR ( res-ref EQ neg_nan OR  res-ref EQ neg_inf ).
            new_number = abap_true.
            res = neg_sign( res ).
          ENDIF.

        WHEN type_complex.
          new_number = abap_true.
          DATA(z) = CAST lcl_lisp_complex( me ).
          res-type = res-subtype = type_real.
          IF res-infnan EQ abap_false AND res-imag_part-infnan EQ abap_false.
            res-real = sqrt( z->zreal->square( )->to_real( ) + z->zimag->square( )->to_real( )  ).
          ELSE.
            res-infnan = abap_true.
            IF z->zreal EQ lcl_lisp_number=>nan OR z->zreal EQ lcl_lisp_number=>neg_nan
              OR z->zimag EQ lcl_lisp_number=>nan OR z->zimag EQ lcl_lisp_number=>neg_nan.
                res-ref = lcl_lisp_number=>nan.
            ELSE.
              res-ref = lcl_lisp_number=>inf.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
          raise_nan( `abs` ).
      ENDCASE.

      IF new_number EQ abap_true.
        num = lcl_lisp_new=>numeric( res ).
      ELSE.
        num = me.
      ENDIF.

    ENDMETHOD.

    METHOD get_number_info.
      rs_info = VALUE #( subtype = me->type
                         exact = me->exact ).
      CASE type.
        WHEN type_integer.
          rs_info-int = CAST lcl_lisp_integer( me )->int.

        WHEN type_rational.
          DATA(lo_rat) = CAST lcl_lisp_rational( me ).
          rs_info-nummer = lo_rat->int.
          rs_info-denom = lo_rat->denominator.

        WHEN type_real.
          DATA(lo_real) = CAST lcl_lisp_real( me ).
          rs_info-ref = lo_real.
          rs_info-infnan = lo_real->infnan.
          rs_info-real = lo_real->real.

        WHEN type_complex.
          throw( `Invalid complex part in [get_number_info]` ).

        WHEN OTHERS.
          raise( `not a number in [get_number_info]` ).
      ENDCASE.
    ENDMETHOD.

    METHOD get_state.
      DATA lo_z TYPE REF TO lcl_lisp_complex.

      state = VALUE ts_result( type = me->type
                               real_part-subtype = type_integer
                               real_part-denom = 1
                               real_part-exact = abap_true
                               imag_part-subtype = type_integer
                               imag_part-exact = abap_true
                               operation = operation ).
      CASE type.
        WHEN type_real
          OR type_rational
          OR type_integer.
          state-real_part = get_number_info( operation ).

        WHEN type_complex.
          lo_z ?= me.
          state-real_part = lo_z->zreal->get_number_info( operation ).
          state-imag_part = lo_z->zimag->get_number_info( operation ).
      ENDCASE.
    ENDMETHOD.

    METHOD get_real.
      CASE self-subtype.
        WHEN type_integer.
          rv_real = self-int.

        WHEN type_rational.
          IF self-denom NE 0.
            rv_real = self-nummer / self-denom.
          ELSE.
            throw( `Not a rational in ` && operation ).
          ENDIF.

        WHEN type_real.
          IF self-infnan EQ abap_false.
            rv_real = self-real.
          ELSEIF infnan_value IS SUPPLIED.
            rv_real = infnan_value.
          ELSE.
            throw( `Not a rational in ` && operation ).
          ENDIF.

        WHEN OTHERS.
          throw( operation && ` number cannot be converted [get_real]` ).
      ENDCASE.
    ENDMETHOD.

    METHOD add_real.
      DATA lv_gcd TYPE tv_int.

      number = self.
      IF other-exact EQ abap_false.
        number-exact = abap_false.
      ENDIF.

      CASE other-subtype.
        WHEN type_integer.
          CASE number-subtype.
            WHEN type_integer.  " Integer + Integer
              TRY.
                  number-int = number-int + other-int.
                CATCH cx_sy_arithmetic_overflow.
                  number-real = number-int + other-int.
                  number-subtype = type_real.
                  number-exact = abap_false.
              ENDTRY.

            WHEN type_rational. " Rational + Integer
              TRY.
                  number-nummer = number-nummer + other-int * number-denom.
                  lv_gcd = lcl_lisp_rational=>gcd( n = number-nummer
                                                   d = number-denom ).
                  number-nummer = number-nummer DIV lv_gcd.
                  number-denom = number-denom DIV lv_gcd.
                CATCH cx_sy_arithmetic_overflow.
                  number-real = number-nummer / number-denom + other-int.
                  number-subtype = type_real.
                  number-exact = abap_false.
              ENDTRY.
            WHEN type_real. " Real + Integer
              IF number-infnan EQ abap_false.
                number-real = number-real + other-int.
              ENDIF.

            WHEN OTHERS.
              raise_nan( operation ).  "assert violation - Not a Number
          ENDCASE.

        WHEN type_rational.
          CASE number-subtype.
            WHEN type_integer.  " Integer + Rational
              TRY.
                  number-subtype = type_rational.
                  number-nummer = number-int * other-denom + other-nummer.
                  number-denom = other-denom.
                  lv_gcd = lcl_lisp_rational=>gcd(  n = number-nummer
                                                    d = number-denom ).
                  number-nummer = number-nummer DIV lv_gcd.
                  number-denom = number-denom DIV lv_gcd.
                CATCH cx_sy_arithmetic_overflow.
                  number-real = number-int + other-nummer / other-denom.
                  number-subtype = type_real.
                  number-exact = abap_false.
              ENDTRY.

            WHEN type_rational. " Rational + Rational
              TRY.
                  number-nummer = number-nummer * other-denom + other-nummer * number-denom.
                  number-denom = number-denom * other-denom.
                  lv_gcd = lcl_lisp_rational=>gcd(  n = number-nummer
                                                    d = number-denom ).
                  number-nummer = number-nummer DIV lv_gcd.
                  number-denom = number-denom DIV lv_gcd.
                CATCH cx_sy_arithmetic_overflow.
                  number-real = number-nummer / number-denom + other-nummer / other-denom.
                  number-subtype = type_real.
                  number-exact = abap_false.
              ENDTRY.

            WHEN type_real. " Real + Rational
              IF number-infnan EQ abap_false.
                number-real = number-real + other-nummer / other-denom.
              ENDIF.

            WHEN OTHERS.
              raise_nan( operation ).  "assert violation - Not a Number
          ENDCASE.

        WHEN type_real.
          IF other-infnan EQ abap_true.
            IF number-infnan EQ abap_false.
              number-ref = other-ref.
              number-infnan = abap_true.
              number-subtype = type_real.
            ELSEIF ( number-ref EQ inf AND other-ref = neg_inf )
                OR ( number-ref = neg_inf AND other-ref = inf ).
                " special case: +inf.0 -inf.0 -> NaN
                number-ref = nan.
            "ELSE. " keep value for other InfNan + InfNan case
            ENDIF.

          ELSE.

            CASE number-subtype.
              WHEN type_integer.  " Integer + Real
                number-subtype = type_real.
                number-real = number-int + other-real.

              WHEN type_rational.  " Rational + Real
                number-subtype = type_real.
                number-real = number-nummer / number-denom + other-real.

              WHEN type_real. " Real + Real
                "number-subtype = type_real.
                IF number-infnan EQ abap_false.
                  number-real = number-real + other-real.
                ELSE.
                  " inf / nan + finite number -> no change
                ENDIF.

              WHEN OTHERS.
                raise_nan( operation ).  "assert violation - Not a Number
            ENDCASE.

          ENDIF.

        WHEN OTHERS.
          raise_nan( operation ).  "assert violation - Real part of other is Not a Number
      ENDCASE.

    ENDMETHOD.

    METHOD sum.
      result = first.
      result-real_part = add_real( self = first-real_part
                                   other = second-real_part
                                   operation = operation ).    " add real parts
      IF second-type EQ type_complex.
        result-type = type_complex.
        result-imag_part = add_real( self = first-imag_part
                                     other = second-imag_part
                                     operation = operation ).  " add imag parts
      ENDIF.
    ENDMETHOD.

    METHOD add.
      result = lcl_lisp_new=>numeric( sum( first = me->get_state( '+' )
                                           second = other->get_state( '+' ) ) ).
    ENDMETHOD.

    METHOD substract.
      result = lcl_lisp_new=>numeric( sum( first = me->get_state( '-' )
                                           second = neg_sign( other->get_state( '-' ) )
                                           operation = '-' ) ).
    ENDMETHOD.

    METHOD negative.
      res = number.
      res-int = 0 - number-int.
      res-nummer = 0 - number-nummer.
      res-real = 0 - number-real.
      IF number-infnan EQ abap_true.
        res-ref = number-ref->negative_infnan( ).
      ENDIF.
    ENDMETHOD.

    METHOD neg_sign.
      res = number.
      res-real_part = negative( res-real_part ).
      res-imag_part = negative( res-imag_part ).
    ENDMETHOD.

    METHOD multiply_real.
      DATA lv_gcd TYPE tv_int.

      number = self.
      IF other-exact EQ abap_false.
        number-exact = abap_false.
      ENDIF.

      CASE other-subtype.
        WHEN type_integer.
          CASE number-subtype.
            WHEN type_integer.  " Integer * Integer
              TRY.
                  number-int = number-int * other-int.
                CATCH cx_sy_arithmetic_overflow.
                  number-real = number-int * other-int.
                  number-subtype = type_real.
                  number-exact = abap_false.
              ENDTRY.

            WHEN type_rational. " Rational * Integer
              TRY.
                  number-nummer = number-nummer * ( other-int * number-denom ).
                  lv_gcd = lcl_lisp_rational=>gcd( n = number-nummer
                                                   d = number-denom ).
                  number-nummer = number-nummer DIV lv_gcd.
                  number-denom = number-denom DIV lv_gcd.
                CATCH cx_sy_arithmetic_overflow.
                  number-real = number-nummer / number-denom *  other-int.
                  number-subtype = type_real.
                  number-exact = abap_false.
              ENDTRY.

            WHEN type_real. " Real * Integer
              IF number-infnan EQ abap_false.
                number-real = number-real * other-int.
              ELSE.
                " Sonderfall (* +Inf.0 0)
                IF other-int EQ 0.
                  IF number-ref = inf OR number-ref EQ neg_inf.
                    number-ref = nan.
                  ENDIF.
                ELSEIF other-int LT 0.
                  number-ref = number-ref->negative_infnan( ).
                ENDIF.
              ENDIF.

            WHEN OTHERS.
              raise_nan( operation ).  "assert violation - Not a Number
          ENDCASE.

        WHEN type_rational.
          CASE number-subtype.
            WHEN type_integer.  " Integer * Rational
              TRY.
                  number-subtype = type_rational.
                  number-nummer = number-int * other-nummer.
                  number-denom = other-denom.
                  lv_gcd = lcl_lisp_rational=>gcd(  n = number-nummer
                                                    d = number-denom ).
                  number-nummer = number-nummer DIV lv_gcd.
                  number-denom = number-denom DIV lv_gcd.
                CATCH cx_sy_arithmetic_overflow.
                  number-real = number-int *  other-nummer / other-denom.
                  number-subtype = type_real.
                  number-exact = abap_false.
              ENDTRY.

            WHEN type_rational. " Rational * Rational
              TRY.
                  number-nummer = number-nummer * other-nummer.
                  number-denom = number-denom * other-denom.
                  lv_gcd = lcl_lisp_rational=>gcd(  n = number-nummer
                                                    d = number-denom ).
                  number-nummer = number-nummer DIV lv_gcd.
                  number-denom = number-denom DIV lv_gcd.
                CATCH cx_sy_arithmetic_overflow.
                  number-real = ( number-nummer / number-denom ) * other-nummer / other-denom.
                  number-subtype = type_real.
                  number-exact = abap_false.
              ENDTRY.

            WHEN type_real. " Real * Rational
              IF number-infnan EQ abap_false.
                number-real = number-real * other-nummer / other-denom.
              ELSE.
                " Sonderfall (* +Inf.0 0/1)
                IF other-nummer EQ 0 AND ( number-ref = inf OR number-ref EQ neg_inf ).
                  number-ref = nan.
                ELSEIF other-nummer LT 0.
                  number-ref = number-ref->negative_infnan( ).
                ENDIF.
              ENDIF.

            WHEN OTHERS.
              raise_nan( operation ).  "assert violation - Not a Number
          ENDCASE.

        WHEN type_real.
            CASE number-subtype.
              WHEN type_integer.  " Integer * Real
                IF other-infnan EQ abap_true.
                  IF number-int EQ 0 AND ( other-ref = inf OR other-ref = neg_inf ).
                    number-ref = nan.
                  ELSEIF number-int LE 0.
                    number-ref = other-ref->negative_infnan( ).
                  ELSE.
                    number-ref = other-ref.
                  ENDIF.
                  number-infnan = abap_true.
                ELSE.
                  number-real = number-int * other-real.
                ENDIF.

              WHEN type_rational.  " Rational * Real
                IF other-infnan EQ abap_true.
                  IF number-nummer EQ 0 AND ( other-ref = inf OR other-ref = neg_inf ).
                    number-ref = nan.
                  ELSE.
                    number-ref = other-ref.
                  ENDIF.
                  number-infnan = abap_true.
                ELSE.
                  number-real = number-nummer / number-denom * other-real.
                ENDIF.

              WHEN type_real. " Real * Real
                IF other-infnan EQ abap_true.
                  IF number-infnan EQ abap_false.
                    " Inf / NaN * finite number
                    number-ref = other-ref.
                    IF number-real EQ 0 AND ( other-ref = inf OR other-ref = neg_inf ).
                      number-ref = nan.
                    ELSEIF number-real LT 0.
                      number-ref = number-ref->negative_infnan( ).
                    ENDIF.
                  ENDIF.
                  number-infnan = abap_true.
                ELSEIF number-infnan EQ abap_true.
                  " Inf / NaN *  Inf / NaN
                  CASE number-ref.
                    WHEN nan.
                      IF other-ref EQ neg_inf.
                        number-ref = neg_nan.
                      ENDIF.
                    WHEN neg_nan.
                      IF other-ref EQ neg_inf.
                        number-ref = nan.
                      ENDIF.
                    WHEN inf.
                      number-ref = other-ref.
                    WHEN neg_inf.
                      number-ref = other-ref->negative_infnan( ).
                  ENDCASE.
                ELSE. " IF number-infnan EQ abap_false.
                  number-real = number-real * other-real.
                ENDIF.

              WHEN OTHERS.
                raise_nan( operation ).  "assert violation - Not a Number
            ENDCASE.
          number-subtype = type_real.


        WHEN OTHERS.
          raise_nan( operation ).  "assert violation - Real part of other is Not a Number
      ENDCASE.

    ENDMETHOD.

    METHOD product.
      DATA term_real TYPE ts_number.
      DATA term_imag TYPE ts_number.

      IF first-type EQ type_complex OR second-type EQ type_complex.
        term_real = add_real( self = multiply_real( self = first-real_part
                                                    other = second-real_part )
                              other = negative( multiply_real( self = first-imag_part
                                                               other = second-imag_part ) )
                              operation = operation ).

        term_imag = add_real( self = multiply_real( self = first-real_part
                                                    other = second-imag_part )
                              other = multiply_real( self = first-imag_part
                                                     other = second-real_part )
                              operation = operation ).

        result = VALUE #( type = type_complex
                          real_part = term_real
                          imag_part = term_imag ).
      ELSE.
        result = first.
        result-real_part = multiply_real( self = first-real_part
                                          other = second-real_part ).    " multiply real parts
      ENDIF.
    ENDMETHOD.

    METHOD multiply.
      result = lcl_lisp_new=>numeric( product( first = me->get_state( '*' )
                                               second = other->get_state( '*' ) ) ).
    ENDMETHOD.

    METHOD divide_real.
      "##TO DO: Check overflow cases and convert to real
      DATA lv_gcd TYPE tv_int.

      number = self.
      IF other-exact EQ abap_false.
        number-exact = abap_false.
      ENDIF.

      CASE other-subtype.
        WHEN type_integer.
          CASE number-subtype.
            WHEN type_integer.  " Integer / Integer
              IF other-int EQ 0.
                IF number-int EQ 0.
                  number-ref = nan.     " 0/0
                ELSEIF number-int GT 0.
                  number-ref = inf.     " cell->raise( ' is invalid in [/]' ).
                ELSE.
                  number-ref = neg_inf.
                ENDIF.
                number-infnan = abap_true.
                number-subtype = type_real.
                number-exact = abap_false.
              ELSE.
                number-nummer = number-int.
                number-denom = other-int.
                lv_gcd = lcl_lisp_rational=>gcd( n = number-nummer
                                                 d = number-denom ).
                number-nummer = number-nummer DIV lv_gcd.
                number-denom = number-denom DIV lv_gcd.
                number-subtype = type_rational.
              ENDIF.

            WHEN type_rational. " Integer / Rational
              IF other-int EQ 0.
                IF number-nummer EQ 0.
                  number-ref = nan.     " 0/0
                ELSEIF ( number-nummer GT 0 AND number-denom GT 0 )
                  OR ( number-nummer LT 0 AND number-denom LT 0 ).
                  number-ref = inf.     " cell->raise( ' is invalid in [/]' ).
                ELSE.
                  number-ref = neg_inf.
                ENDIF.
                number-infnan = abap_true.
                number-subtype = type_real.
                number-exact = abap_false.
              ELSE.
                number-denom = number-denom * other-int.
                lv_gcd = lcl_lisp_rational=>gcd( n = number-nummer
                                                 d = number-denom ).
                number-nummer = number-nummer DIV lv_gcd.
                number-denom = number-denom DIV lv_gcd.
              ENDIF.

            WHEN type_real. " Real / Integer
              IF other-int EQ 0.            " division real / integer -> real
                IF number-real EQ 0.
                  number-ref = nan.     " 0.0/0
                ELSEIF number-real GT 0.
                  number-ref = inf.
                ELSE.
                  number-ref = neg_inf.
                ENDIF.
                number-infnan = abap_true.
                number-exact = abap_false.
              ELSE.
                IF number-infnan EQ abap_false.
                  number-real = number-real / other-int.
                ELSEIF other-int LT 0.
                  number-ref = number-ref->negative_infnan( ).
                ENDIF.
              ENDIF.

            WHEN OTHERS.
              raise_nan( operation ).  "assert violation - Not a Number
          ENDCASE.

        WHEN type_rational.
          CASE number-subtype.
            WHEN type_integer.  " Integer / Rational
              number-nummer = number-int * other-denom.
              number-denom = other-nummer.
              lv_gcd = lcl_lisp_rational=>gcd(  n = number-nummer
                                                d = number-denom ).
              number-nummer = number-nummer DIV lv_gcd.
              number-denom = number-denom DIV lv_gcd.
              number-subtype = type_rational.

            WHEN type_rational. " Rational / Rational
              number-nummer = number-nummer * other-denom.
              number-denom = number-denom * other-nummer.
              lv_gcd = lcl_lisp_rational=>gcd(  n = number-nummer
                                                d = number-denom ).
              number-nummer = number-nummer DIV lv_gcd.
              number-denom = number-denom DIV lv_gcd.

            WHEN type_real. " Real / Rational
              IF number-infnan EQ abap_false.
                number-real = number-real * other-denom / other-nummer.
              ELSE.
                " Sonderfall (* 0.0 +Inf.0)
                IF other-nummer EQ 0 AND ( number-ref = inf OR number-ref EQ neg_inf ).
                  number-ref = nan.
                ELSEIF other-nummer LT 0.
                  number-ref = number-ref->negative_infnan( ).
                ENDIF.
              ENDIF.
              number-subtype = type_real.

            WHEN OTHERS.
              raise_nan( operation ).  "assert violation - Not a Number
          ENDCASE.

        WHEN type_real.
          CASE number-subtype.
            WHEN type_integer.  " Integer / Real
              IF other-infnan EQ abap_true.
                number-infnan = abap_false.

                IF number-int EQ 0.   " 0 / Inf.0 , 0 / -Inf,  0 / Nan
                  " real 0 or Nan
                  CASE other-ref.
                    WHEN inf OR neg_inf.
                      number-real = 0.
                      number-exact = abap_true.
                      number-ref = zero.
                    WHEN nan OR neg_nan.      " 0 / Nan -> NaN
                      number-ref = other-ref.
                      number-infnan = abap_true.
                  ENDCASE.

                ELSEIF number-int LT 0. " -1 / Inf, -1 / NaN
                  CASE other-ref.
                    WHEN inf.
                      number-real = 0.
                      number-ref = pos_zero.
                    WHEN neg_inf.
                      number-real = 0.
                      number-ref = neg_zero.
                    WHEN nan OR neg_nan.
                      number-ref = other-ref->negative_infnan( ).
                      number-infnan = abap_true.
                  ENDCASE.

                ELSE. " 1 / Inf, 1 / NaN
                  CASE other-ref.
                    WHEN inf OR neg_inf.
                      number-real = 0.
                      number-ref = pos_zero.
                    WHEN nan OR neg_nan.
                      number-ref = other-ref->negative_infnan( ).
                      number-infnan = abap_true.
                  ENDCASE.

                ENDIF.
              ELSE.
                number-real = number-int / other-real.
              ENDIF.
              number-subtype = type_real.

            WHEN type_rational.  " Rational / Real
              IF other-infnan EQ abap_true.
                number-infnan = abap_false.

                IF number-nummer EQ 0.   " 0 / Inf.0 , 0 / -Inf,  0 / Nan
                  " real 0 or Nan
                  CASE other-ref.
                    WHEN inf OR neg_inf.
                      number-real = 0.
                      number-ref = zero.
                    WHEN nan OR neg_nan.      " 0 / Nan -> NaN
                      number-ref = other-ref.
                      number-infnan = abap_true.
                  ENDCASE.

                ELSEIF number-nummer LT 0. " -1 / Inf, -1 / NaN
                  CASE other-ref.
                    WHEN inf.
                      number-real = 0.
                      number-ref = pos_zero.
                    WHEN neg_inf.
                      number-real = 0.
                      number-ref = neg_zero.
                    WHEN nan OR neg_nan.
                      number-ref = other-ref->negative_infnan( ).
                      number-infnan = abap_true.
                  ENDCASE.

                ELSE. " 1 / Inf, 1 / NaN
                  CASE other-ref.
                    WHEN inf OR neg_inf.
                      number-real = 0.
                      number-ref = pos_zero.
                    WHEN nan OR neg_nan.
                      number-ref = other-ref->negative_infnan( ).
                      number-infnan = abap_true.
                  ENDCASE.

                ENDIF.
              ELSE.
                number-real = number-nummer / number-denom / other-real.
              ENDIF.
              number-subtype = type_real.

            WHEN type_real. " Real / Real
              IF other-infnan EQ abap_true.
                IF number-infnan = abap_false.

                  IF number-real EQ 0.   " 0 / Inf.0 , 0 / -Inf,  0 / Nan
                    " real 0 or Nan
                    CASE other-ref.
                        WHEN inf OR neg_inf.
                          number-real = 0.
                          number-ref = zero.
                        WHEN nan OR neg_nan.      " 0 / Nan -> NaN
                          number-ref = other-ref.
                          number-infnan = abap_true.
                    ENDCASE.

                  ELSEIF number-nummer LT 0. " -1 / Inf, -1 / NaN
                    CASE other-ref.
                        WHEN inf.
                          number-real = 0.
                          number-ref = pos_zero.
                        WHEN neg_inf.
                          number-real = 0.
                          number-ref = neg_zero.
                        WHEN nan OR neg_nan.
                          number-ref = other-ref->negative_infnan( ).
                          number-infnan = abap_true.
                    ENDCASE.

                  ELSE. " 1 / Inf, 1 / NaN
                    CASE other-ref.
                        WHEN inf OR neg_inf.
                          number-real = 0.
                          number-ref = pos_zero.
                        WHEN nan OR neg_nan.
                          number-ref = other-ref->negative_infnan( ).
                          number-infnan = abap_true.
                    ENDCASE.

                  ENDIF.
                ELSE.  " Inf / NaN  div. Inf / Nan

                  number-ref = nan.
                  number-infnan = abap_true.

                ENDIF.
              ELSE.
                number-real = number-real / other-real.
              ENDIF.
              number-subtype = type_real.

            WHEN OTHERS.
              raise_nan( operation ).  "assert violation - Not a Number
          ENDCASE.
          number-subtype = type_real.


        WHEN OTHERS.
          raise_nan( operation ).  "assert violation - Real part of other is Not a Number
      ENDCASE.

    ENDMETHOD.

    METHOD quotient.
      DATA denom TYPE ts_number.
      DATA nummer_real TYPE ts_number.
      DATA nummer_imag TYPE ts_number.

      result = numerator.

      IF numerator-type EQ type_complex OR denominator-type EQ type_complex.
        denom = add_real( self = multiply_real( self = denominator-real_part
                                                other = denominator-real_part )
                          other = multiply_real( self = denominator-imag_part
                                                 other = denominator-imag_part )
                          operation = operation ).

        nummer_real = add_real( self = multiply_real( self = numerator-real_part
                                                      other = denominator-real_part )
                                other = multiply_real( self = numerator-imag_part
                                                       other = denominator-imag_part )
                                operation = operation ).

        nummer_imag = add_real( self = multiply_real( self = denominator-real_part
                                                      other = numerator-imag_part )
                                other = negative( multiply_real( self = numerator-real_part
                                                                 other = denominator-imag_part ) )
                                operation = operation ).

        result-type = type_complex.
        result-real_part = divide_real( self = nummer_real
                                      other = denom ).
        result-imag_part = divide_real( self = nummer_imag
                                      other = denom ).
      ELSE.
        result-real_part = divide_real( self = numerator-real_part
                                        other = denominator-real_part ).    " divide real parts
      ENDIF.

    ENDMETHOD.

    METHOD divide.
      result = lcl_lisp_new=>numeric( quotient( numerator = me->get_state( '/' )
                                                denominator = other->get_state( '/' ) ) ).
    ENDMETHOD.

    METHOD square.
      _data_local_numeric.

      CASE type.
        WHEN type_integer.
          lo_int ?= me.
          result = lcl_lisp_new=>real_number( value = lo_int->int * lo_int->int
                                              iv_exact = exact ).
        WHEN type_rational.
          lo_rat ?= me.
          result = lcl_lisp_new=>rational( nummer = lo_rat->int * lo_rat->int
                                           denom = lo_rat->denominator * lo_rat->denominator
                                           iv_exact = exact ).
        WHEN type_real.
          lo_real ?= me.
          IF infnan EQ abap_true.
            IF me EQ neg_inf.
              result = inf.
            ELSE.
              result = me.
            ENDIF.
          ELSE.
            result = lcl_lisp_new=>real_number( value = lo_real->real * lo_real->real
                                                iv_exact = exact ).
          ENDIF.

        WHEN type_complex.
          lo_z ?= me.
          DATA(real_part) = lo_z->zreal->square( ).
          DATA(imag_part) = lo_z->zimag->square( ).
          IF lo_z->is_real( ).
            result = real_part.
          ELSE.
            result = lcl_lisp_new=>rectangular(
              real = lcl_lisp_new=>real_number( real_part->to_real( ) - imag_part->to_real( ) )
              imag = lcl_lisp_new=>real_number( 2 * lo_z->zreal->to_real( ) * lo_z->zimag->to_real( ) ) ).
          ENDIF.

        WHEN OTHERS.
          raise_nan( `square` ).
      ENDCASE.
    ENDMETHOD.

    METHOD get_numerator.
      CASE type.
        WHEN type_integer.
          result = me.

        WHEN type_rational.
          DATA(lo_rat) = CAST lcl_lisp_rational( me ).
          result = lcl_lisp_new=>integer( value = lo_rat->int
                                          iv_exact = lo_rat->exact ).
        WHEN type_real.
          TRY.
              DATA(lo_real) = CAST lcl_lisp_real( me ).
              DATA lv_nummer TYPE tv_int.
              IF lo_real->to_rational( IMPORTING ev_nummer = lv_nummer ).
                result = lcl_lisp_new=>real_number( value = lv_nummer
                                                    iv_exact = lo_real->exact ).
              ELSE.
                lo_real->raise( ` is not a rational in numerator` ).
              ENDIF.

*              DATA(lv_num) = lo_real->real.
*              result = lcl_lisp_new=>real_number( lcl_lisp_real=>gcd( n = lv_num
*                                                                      d = 1 ) * lv_num ).
              _catch_arithmetic_error.
                lo_real->raise( ` is not a rational in numerator` ).
          ENDTRY.

        WHEN type_complex.
          DATA(lo_z) = CAST lcl_lisp_complex( me ).
          IF lo_z->is_real( ).
            result = lo_z->zreal->get_numerator( ).
          ELSE.
            no_complex( 'numerator' ).
          ENDIF.

        WHEN OTHERS.
          raise_nan( `numerator` ).
      ENDCASE.
    ENDMETHOD.

    METHOD get_denominator.
      DATA lo_frac TYPE REF TO lcl_lisp_number.
      DATA lv_nummer TYPE tv_int.
      DATA lv_denom TYPE tv_int.
      DATA lv_error TYPE tv_flag VALUE abap_false.

      CASE type.
        WHEN type_integer.
          DATA(lo_int) = CAST lcl_lisp_integer( me ).
          result = lcl_lisp_new=>integer( value = 1
                                          iv_exact = lo_int->exact ).
        WHEN type_rational.
          DATA(lo_rat) = CAST lcl_lisp_rational( me ).
          result = lcl_lisp_new=>integer( value = lo_rat->denominator
                                          iv_exact = lo_rat->exact ).
        WHEN type_real.
          TRY.
              DATA(lo_real) = CAST lcl_lisp_real( me ).

              IF lo_real->to_rational( IMPORTING ev_nummer = lv_nummer
                                                 ev_denom = lv_denom ).
                lo_frac = lcl_lisp_new=>rational( nummer = lv_nummer
                                                  denom = lv_denom
                                                  iv_exact = lo_real->exact ).
                CASE lo_frac->type.
                  WHEN type_integer.
                    result = lcl_lisp_new=>integer( value = 1
                                                    iv_exact = lo_int->exact ).
                  WHEN type_rational.
                    lo_rat ?= lo_frac.
                    IF lo_real->float_eq( lo_rat->to_real( ) ).
                      result = lcl_lisp_new=>integer( value = lo_rat->denominator
                                                      iv_exact = lo_real->exact ).
                    ELSE.
                      lv_error = abap_true.
                    ENDIF.
                ENDCASE.
              ELSE.
                lv_error = abap_true.
              ENDIF.

              _catch_arithmetic_error.
                lv_error = abap_true.
          ENDTRY.
          IF lv_error EQ abap_true.
            lo_real->raise( ` is not a rational in denominator` ).
          ENDIF.

        WHEN type_complex.
          DATA(lo_z) = CAST lcl_lisp_complex( me ).
          IF lo_z->is_real( ).
            result = lo_z->zreal->get_denominator( ).
          ELSE.
            no_complex( 'denominator' ).
          ENDIF.

        WHEN OTHERS.
          raise_nan( `denominator` ).
      ENDCASE.
    ENDMETHOD.

    METHOD to_exact.
      DATA lo_real TYPE REF TO lcl_lisp_real.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lv_denom TYPE tv_int.
      DATA lv_nummer TYPE tv_int.
      DATA lv_error TYPE tv_flag VALUE abap_false.

      IF exact EQ abap_true.
        result = me.
      ELSE.
        CASE type.
          WHEN type_real.
            lo_real ?= me.
            IF NOT lo_real->to_rational( IMPORTING ev_nummer = lv_nummer
                                                   ev_denom = lv_denom ).
              lv_error = abap_true.
            ENDIF.

          WHEN type_integer.
            lv_nummer = CAST lcl_lisp_integer( me )->int.
            lv_denom = 1.

          WHEN type_rational.
            lo_rat = CAST lcl_lisp_rational( me ).
            lv_nummer = lo_rat->int.
            lv_denom = lo_rat->denominator.

          WHEN type_complex.
            DATA(lo_z) = CAST lcl_lisp_complex( me ).
            result ?= lcl_lisp_new=>rectangular( real = lo_z->zreal->to_exact( )
                                                 imag = lo_z->zimag->to_exact( ) ).
            RETURN.

          WHEN OTHERS.
            lv_error = abap_true.
        ENDCASE.

        TRY.
            DATA(lo_num) = lcl_lisp_new=>rational( nummer = lv_nummer
                                                   denom = lv_denom
                                                   iv_exact = abap_true ).
            result ?= lo_num.
            lv_error = xsdbool( lo_num->exact NE abap_true ).
          CATCH lcx_lisp_exception cx_sy_conversion_no_number.
            lv_error = abap_true.
        ENDTRY.
      ENDIF.
      IF lv_error = abap_true.
        throw( |no exact representation of { to_string( ) }| ).
      ENDIF.
    ENDMETHOD.

    METHOD to_inexact.
      IF exact EQ abap_true.
        DATA(res) = get_state( 'to_inexact' ).
        res-exact = abap_false.
        res-imag_part-exact = abap_false.
        result = lcl_lisp_new=>numeric( res ).
      ELSE.
        result = me.
      ENDIF.
    ENDMETHOD.

    METHOD number_is_zero.
      CASE is_number-subtype.
        WHEN type_integer.
          null = xsdbool( is_number-int EQ 0 ).

        WHEN type_rational.
          null = xsdbool( is_number-nummer EQ 0 ).

        WHEN type_real.       " should logic check exactness?
          null = xsdbool( is_number-infnan EQ abap_false AND is_number-real EQ 0 ).

        WHEN OTHERS.
          raise_nan( `Number type not supported (check zero)` ).
      ENDCASE.
    ENDMETHOD.

    METHOD number_is_positive.
      CASE is_number-subtype.
        WHEN type_integer.
          positive = xsdbool( is_number-int GE 0 ).

        WHEN type_rational.
          positive = xsdbool( is_number-nummer GE 0 ).

        WHEN type_real.
          " should logic check exactness?
          positive = xsdbool( ( is_number-infnan EQ abap_false AND is_number-real GE 0 )
                             OR is_number-ref EQ inf
                             OR is_number-ref EQ nan ).
        WHEN OTHERS.
          throw( `Number type not supported (check positive)` ).
      ENDCASE.
    ENDMETHOD.

    METHOD negative_infnan.
      CASE me.
        WHEN inf.
          num = neg_inf.
        WHEN neg_inf.
          num = inf.
        WHEN zero OR pos_zero.
          num = neg_zero.
        WHEN neg_zero.
          num = pos_zero.
        WHEN nan.
          num = neg_nan.
        WHEN neg_nan.
          num = nan.
        WHEN OTHERS.
          throw( `Negative_infnan not correctly called` ).
      ENDCASE.
    ENDMETHOD.

    METHOD number_is_equal.
      DATA lo_z TYPE REF TO lcl_lisp_complex.

      equal = abap_false.

      DATA(self) = me->get_state( '=' ).
      DATA(number) = other->get_state( '=' ).

      CASE type.
        WHEN type_integer
          OR type_rational
          OR type_real.

          IF number-type EQ type_complex.
            " returns false if imaginary part of "other" is not 0
            IF NOT number_is_zero( number-imag_part ).
              RETURN.
            ENDIF.
          ENDIF.

          equal = real_is_equal( number-real_part ).  " check if real part matches

        WHEN type_complex.
          lo_z ?= me.

          CASE number-type.
            WHEN type_integer
              OR type_rational
              OR type_real.

              " returns false if imaginary part of "me" is not 0
              IF NOT number_is_zero( self-imag_part ).
                RETURN.
              ENDIF.

              equal = lo_z->zreal->real_is_equal( number-real_part ).    " check if real part matches

            WHEN type_complex.

              IF lo_z->zreal->real_is_equal( number-real_part ).         " check if real part matches
                equal = lo_z->zimag->real_is_equal( number-imag_part ).  " check if imag part matches
              ENDIF.

          ENDCASE.

      ENDCASE.

    ENDMETHOD.

    METHOD real_is_equal.
      CONSTANTS c_not_a_real TYPE string VALUE `Number type not supported (check equality)`.

      DATA(state) = get_state( 'real_is_equal' ).
      equal = abap_false.

      CASE type.
        WHEN type_integer.

          CASE res-subtype.
            WHEN type_integer.
              equal = xsdbool( state-int = res-int ).

            WHEN type_rational.
              equal = xsdbool( state-int * res-denom = res-nummer ).

            WHEN type_real.
              IF res-infnan EQ abap_false
                AND state-int = scheme_round( res-real )
                AND frac( res-real ) EQ 0.
                equal = abap_true.
              "ELSE. " either NaN of Infinity
              ENDIF.

            WHEN OTHERS.
              throw( c_not_a_real ).

          ENDCASE.

        WHEN type_rational.
          CASE res-subtype.
            WHEN type_integer.
              equal = xsdbool( state-nummer = res-int * state-denom ).

            WHEN type_rational.
              equal = xsdbool( state-nummer = res-nummer AND state-denom EQ res-denom ).

            WHEN type_real.
              equal = xsdbool( res-infnan EQ abap_false AND state-nummer = res-real * state-denom ).
              "ELSE. " either NaN of Infinity

            WHEN OTHERS.
              throw( c_not_a_real ).

          ENDCASE.

        WHEN type_real.
          IF is_nan( ). " NaN is not equal to NaN
            RETURN.
          ENDIF.

          CASE res-subtype.
            WHEN type_integer.
              IF state-infnan EQ abap_false
                AND frac( state-real ) EQ 0
                AND scheme_round( state-real ) = res-int.
                equal = abap_true.
              ENDIF.

            WHEN type_rational.
              equal = xsdbool( state-infnan EQ abap_false
                           AND res-nummer = state-real * res-denom ).

            WHEN type_real.
              IF state-infnan EQ abap_false AND res-infnan EQ abap_false.

                IF ( state-exact EQ abap_true AND state-real EQ res-real )
                  OR ( state-exact EQ abap_false AND CAST lcl_lisp_real( state-ref )->float_eq( res-real ) ).
                  equal = abap_true.
                ENDIF.

              ELSEIF state-ref EQ res-ref.  " NaN was already checked before hand
                equal = abap_true.
              ENDIF.

            WHEN OTHERS.
              throw( c_not_a_real ).

          ENDCASE.

      ENDCASE.

    ENDMETHOD.

    METHOD to_real.
*    _validate &2.
*    cell = &2.
*    _values_get_next cell.

      DATA(num) = get_state( operation ).

      IF infnan_value IS SUPPLIED.
        rv_real = get_real( self = num-real_part
                            operation = operation
                            infnan_value = infnan_value ).
      ELSE.
        rv_real = get_real( self = num-real_part
                            operation = operation ).
      ENDIF.

      IF num-type EQ type_complex.
        DATA(zimag) = CAST lcl_lisp_complex( me )->zimag.
        IF zimag NE zero.
          throw( `complex number not supported` ).
        ENDIF.
      ENDIF.
    ENDMETHOD.

    METHOD atom_to_number.
      CASE to_upper( iv_atom ).
        WHEN c_lisp_neg_img.
          element = imaginary_neg.
        WHEN c_imaginary_pos_inf.   " Pos. Infinity Imaginary_marker
          element = inf_imag.
        WHEN c_imaginary_neg_inf.   " Neg. Infinity Imaginary_marker
          element = neg_inf_imag.
        WHEN c_imaginary_pos_nan.   " Pos. NaN Imaginary_marker
          element = nan_imag.
        WHEN c_imaginary_neg_nan.   " Neg. NaN Imaginary_marker
          element = neg_nan_imag.

        WHEN inf->value.
          element = inf.
        WHEN neg_inf->value.
          element = neg_inf.
        WHEN nan->value.
          element = nan.
        WHEN neg_nan->value.
          element = neg_nan.

        WHEN c_lisp_pos_img.        " Plus Imaginary_marker
          element = imaginary.
      ENDCASE.
    ENDMETHOD.

    METHOD number_to_string.
      " A numerical constant can be specified to be either exact or inexact by a prefix #e for exact and #i for inexact.
      " An exactness prefix can appear before or after any radix prefix that is used.
      " If the written representation of a number has no exactness prefix, the constant is inexact if it contains a
      " decimal point or an exponent. Otherwise, it is exact.
      DATA lv_text TYPE string.
      DATA lv_digit TYPE i.
      DATA lv_real TYPE tv_real.
      DATA lv_radix_error TYPE tv_flag VALUE abap_false.

      CASE type.
        WHEN type_integer.
          DATA(lo_int) = CAST lcl_lisp_integer( me ).

          CASE iv_radix.
            WHEN 10.
              lv_real = lo_int->int.
              str = lv_real.
              str = condense( str ).

              IF lo_int->exact EQ abap_false AND str CN `.`.
                str &&= `.0`.
              ENDIF.

            WHEN 2 OR 8 OR 16.
              DATA(lv_int) = lo_int->int.
              WHILE lv_int GT 0.
                lv_digit = lv_int MOD iv_radix.
                lv_int = lv_int DIV iv_radix.
                str = c_hex_digits+lv_digit(1) && str.
              ENDWHILE.

            WHEN OTHERS.
              lv_radix_error = abap_true.
          ENDCASE.

        WHEN type_real.
          lv_radix_error = xsdbool( iv_radix NE 10 ).
          DATA(lo_real) = CAST lcl_lisp_real( me ).
          IF lo_real->infnan EQ abap_false.
            lv_real = lo_real->real.
            str = condense( CAST lcl_lisp_real( lo_real )->real ). " condense( CONV #( lo_real->real ) ).

            IF lo_real->exact EQ abap_false AND frac( lv_real ) EQ 0 AND str NA c_pattern_inexact.
              str &&= `.0`.
            ENDIF.
          ELSE.
            str = to_lower( lo_real->value ).
          ENDIF.

        WHEN type_rational.
          lv_radix_error = xsdbool( iv_radix NE 10 ).
          DATA(lo_rat) = CAST lcl_lisp_rational( me ).
          IF lo_rat->exact EQ abap_false AND lo_rat->denominator NE 0.
            lv_real = lo_rat->to_real( ).
            str = lv_real.
            str = condense( str ).

            IF strlen( str ) GT 5.    " arbitrary cut-off limit for display
              str = |#i{ lo_rat->int }/{ lo_rat->denominator }|.
            ELSEIF frac( lv_real ) EQ 0 AND str NA c_pattern_inexact.
              str &&= `.0`.
            ENDIF.
          ELSE.
            str &&= |{ lo_rat->int }/{ lo_rat->denominator }|.
          ENDIF.

        WHEN type_complex.
          DATA(lo_z) = CAST lcl_lisp_complex( me ).
          IF lo_z->zreal EQ lcl_lisp_number=>zero.
            str = ``.
          ELSE.
            str = lo_z->zreal->number_to_string( iv_radix ).
          ENDIF.
          DATA(str_imag) = lo_z->zimag->number_to_string( iv_radix ).
          IF str_imag+0(1) NA c_explicit_sign AND  strlen( str_imag ) GT 0.
            str_imag = c_plus_sign && str_imag.
          ENDIF.
          str &&= str_imag && c_imaginary_marker.
          str = to_lower( str ).

        WHEN OTHERS.
          raise_nan( `number->string` ).
      ENDCASE.
      str = condense( str ).
      IF lv_radix_error EQ abap_true.
        throw( |Radix { iv_radix } not supported in number->string| ) ##NO_TEXT.
      ENDIF.

    ENDMETHOD.

    METHOD is_integer.
      DATA(res) = get_state( 'integer?' ).
      CASE type.
        WHEN type_integer.
          flag = abap_true.

        WHEN type_rational.
          flag = xsdbool( res-denom EQ 1 ).

        WHEN type_real.
          " If x is an inexact real number, then (integer? x) is true if and only if (= x (round x)).
          flag = xsdbool( res-infnan EQ abap_false AND scheme_round( res-real ) = res-real ).

        WHEN type_complex.
          DATA(z) = CAST lcl_lisp_complex( me ).
          flag = xsdbool( z->zimag->number_is_equal( zero ) AND z->zreal->is_integer( ) ).

        WHEN OTHERS.
          raise_nan( `integer?` ).
      ENDCASE.
    ENDMETHOD.

    METHOD is_finite.
      "The finite? procedure returns #t on all real numbers except +inf.0, -inf.0, and +nan.0,
      " and on complex numbers if their real and imaginary parts are both finite.
      " Otherwise it returns #f.
      IF infnan EQ abap_false.
        result = true.
      ELSE.
        result = false.
      ENDIF.
    ENDMETHOD.

    METHOD is_infinite.
      " The infinite? procedure returns #t on the real numbers +inf.0 and -inf.0, and on complex
      " numbers if their real or imaginary parts or both are infinite. Otherwise it returns #f.
      result = false.
      CASE type.
        WHEN type_complex.
          DATA(state) = get_state( 'infinite?' ).
          IF state-ref EQ inf OR state-ref EQ neg_inf
            OR state-imag_part-ref EQ inf OR state-imag_part-ref EQ neg_inf.
            result = true.
          ENDIF.

        WHEN OTHERS.
          IF me EQ inf OR me EQ neg_inf.
            result = true.
          ENDIF.
      ENDCASE.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_complex IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_complex ).
    ENDMETHOD.

    METHOD to_string.
      CONSTANTS c_imaginary TYPE tv_char VALUE `i`.
      DATA lv_sep TYPE tv_char.
      DATA lv_img TYPE string.

      IF zreal NE zero.
        str = zreal->to_string( ).
      ENDIF.

      IF zimag NE zero.
        lv_img = zimag->to_string( ).
        IF lv_img NE space.
          IF lv_img = '1'.
            lv_img = c_imaginary.
          ELSE.
            lv_img &&= c_imaginary.
          ENDIF.
          IF zimag->infnan EQ abap_false.
            IF lv_img+0(1) NE c_minus_sign.
              lv_sep = c_plus_sign.
            ENDIF.
          ENDIF.
        ENDIF.
        str &&= lv_sep && lv_img.
      ENDIF.
    ENDMETHOD.

    METHOD set_rectangular.
      set( x = x
           y = y ).
      real_part = zreal->to_real( infnan_value = c_max_float ).
      imaginary_part = zimag->to_real( infnan_value = c_max_float ).
      to_polar( ).
    ENDMETHOD.

    METHOD set.
      IF x->number_is_equal( zero ).
        IF x->exact EQ abap_true.
          zreal = zero.
        ELSEIF x->value GT 0.
          zreal = pos_zero.
        ELSE.
          zreal = neg_zero.
        ENDIF.
      ELSE.
        zreal = x.
      ENDIF.
      IF y->number_is_equal( zero ).
        IF y->exact EQ abap_true.
          zimag = zero.
        ELSEIF y->value GT 0.
          zimag = pos_zero.
        ELSE.
          zimag = neg_zero.
        ENDIF.
      ELSE.
        zimag = y.
      ENDIF.
      exact = xsdbool( zreal->exact EQ abap_true AND zimag->exact EQ abap_true ).
      infnan = xsdbool( zreal->infnan EQ abap_true OR zimag->infnan EQ abap_true ).
    ENDMETHOD.

    METHOD set_polar.
      magnitude = r.
      angle = alpha.
      to_rectangular( ).

      set( x = lcl_lisp_new=>real_number( value = real_part )
           y = lcl_lisp_new=>real_number( value = imaginary_part ) ).
    ENDMETHOD.

    METHOD to_polar.
      DATA lv_tan TYPE f.
      DATA lv_angle TYPE f.

      IF real_part EQ 0.
        IF imaginary_part GT 0.
          magnitude = imaginary_part.
          angle = c_pi / 2.
        ELSEIF imaginary_part LT 0.
          magnitude = - imaginary_part.
          angle = - c_pi / 2.
        ELSE.
          magnitude = 0.
          angle = 0.  " undefined
        ENDIF.
      ELSEIF real_part EQ c_max_float.
        magnitude = c_max_float.
        IF imaginary_part EQ c_max_float.
          lv_angle = c_pi / 4.
        ELSEIF imaginary_part EQ c_min_float.
          lv_angle = - c_pi / 4.
        ELSE.
          lv_angle = 0.
        ENDIF.
      ELSEIF real_part EQ c_min_float.
        magnitude = c_max_float.
        IF imaginary_part EQ c_max_float.
          lv_angle = 3 * c_pi / 4.
        ELSEIF imaginary_part EQ c_min_float.
          lv_angle = - 3 * c_pi / 4.
        ELSE.
          lv_angle = - c_pi.
        ENDIF.
      ELSEIF real_part GT 0.
        IF imaginary_part EQ c_max_float.
          magnitude = c_max_float.
          lv_angle = c_pi / 2.
        ELSEIF imaginary_part EQ c_min_float.
          magnitude = c_max_float.
          lv_angle = - c_pi / 2.
        ELSE.
          magnitude = sqrt( real_part ** 2 + imaginary_part ** 2 ).
          lv_tan = imaginary_part / real_part.
          lv_angle = atan( lv_tan ).
        ENDIF.
      ELSEIF real_part LT 0.
        IF imaginary_part EQ c_max_float.
          magnitude = c_max_float.
          lv_angle = c_pi / 2.
        ELSEIF imaginary_part EQ c_min_float.
          magnitude = c_max_float.
          lv_angle = - c_pi / 2.
        ELSE.
          magnitude = sqrt( real_part ** 2 + imaginary_part ** 2 ).
          lv_tan = imaginary_part / real_part.
          lv_angle = atan( lv_tan ).
          lv_angle = lv_angle + c_pi.
        ENDIF.
      ENDIF.
      angle = lv_angle.
    ENDMETHOD.

    METHOD to_rectangular.
      DATA lv_angle TYPE f.
      DATA lv_cos TYPE f.
      DATA lv_sin TYPE f.

      lv_angle = angle.
      lv_cos = cos( lv_angle ).
      real_part = magnitude * lv_cos.
      lv_sin = sin( lv_angle ).
      imaginary_part = magnitude * lv_sin.
    ENDMETHOD.

    METHOD new_rectangular.
      complex = NEW lcl_lisp_complex( ).
      complex->set_rectangular( x = x
                                y = y ).
    ENDMETHOD.

    METHOD new_polar.
      complex = NEW lcl_lisp_complex( ).
      complex->set_polar( r = r->to_real( )
                          alpha = angle->to_real( ) ).
    ENDMETHOD.

    METHOD to_complex.
      IF n->type EQ type_complex.
        z ?= n.
      ELSE.
        z = new_rectangular( x = n ).
      ENDIF.
    ENDMETHOD.

    METHOD is_nan.
      flag = xsdbool( zreal->is_nan( ) OR zimag->is_nan( ) ). " real OR imaginary parts must be NaN
    ENDMETHOD.

    METHOD is_real.
      flag = abap_false.
      CHECK zimag->exact EQ abap_true.

      CASE zimag->type.
        WHEN type_integer.
          flag = xsdbool( CAST lcl_lisp_integer( zimag )->int EQ 0 ).

        WHEN type_rational.
          flag = xsdbool( CAST lcl_lisp_rational( zimag )->int EQ 0 ).

        WHEN type_real.
          flag = xsdbool( CAST lcl_lisp_real( zimag )->real EQ 0 ).

      ENDCASE.

    ENDMETHOD.

  ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_hash IMPLEMENTATION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_hash IMPLEMENTATION.

    METHOD new.
      ro_hash = NEW lcl_lisp_hash( type_hash ).
      ro_hash->hash = it_hash.
    ENDMETHOD.

    METHOD fill.
      _validate list.

      DATA(lo_head) = list.
      CHECK lo_head->type = type_pair.

*     Can accept a parameter which should be a list of alternating symbols/strings and elements
      DATA(lo_iter) = lo_head->new_iterator( ).
      WHILE lo_iter->has_next( ).
        DATA(lo_key) = lo_iter->next( ).
        IF lo_key->type NE type_symbol AND lo_key->type NE type_string.
          throw( |make-hash: Use only symbol or string as a key| ).
        ENDIF.
        CHECK lo_iter->has_next( ).
        INSERT VALUE #( key = lo_key->value
                        element = lo_iter->next( ) ) INTO TABLE hash.
      ENDWHILE.
    ENDMETHOD.                    "new_hash

    METHOD get.
      _validate: list, list->car.
      IF list->car = nil.
        throw( |hash-get requires a key to access an element| ).
      ENDIF.

*      TODO: Additional check for key type
      result = VALUE #( hash[ key = list->car->value ]-element DEFAULT nil ).
    ENDMETHOD.                    "get

    METHOD insert.
      _validate: list, list->car, list->cdr.

* TODO: Check number and type of parameters
      INSERT VALUE #( key = list->car->value
                      element = list->cdr->car ) INTO TABLE hash.
* TODO: Should we overwrite existing keys?
      result = nil.
    ENDMETHOD.                    "insert

    METHOD delete.
      _validate: list, list->car.
* TODO: Check number and type of parameters
      DELETE hash WHERE key = list->car->value.
      result = nil.
    ENDMETHOD.                    "delete

    METHOD get_hash_keys.
      DATA lo_ptr TYPE REF TO lcl_lisp.

      result = nil.
      LOOP AT hash INTO DATA(ls_entry).
        DATA(lo_last) = lcl_lisp_new=>cons( io_car = lcl_lisp_new=>symbol( ls_entry-key ) ).

        IF result EQ nil.
          result = lo_ptr = lo_last.
        ELSE.
          lo_ptr = lo_ptr->cdr = lo_last.
        ENDIF.
      ENDLOOP.
    ENDMETHOD.                    "get_hash_keys

    METHOD from_list.
      _validate: list, list->car.
      IF list->car->type NE type_hash.
        throw( |{ msg } only works on hashes| ).
      ENDIF.
      ro_hash = CAST #( list->car ).
    ENDMETHOD.                    "from_list

  ENDCLASS.                    "lcl_lisp_hash IMPLEMENTATION

  CLASS lcl_lisp_vector IMPLEMENTATION.

    METHOD init.
      DATA lt_vector TYPE tt_lisp.

      DO size TIMES.
        APPEND io_fill TO lt_vector.
      ENDDO.
      ro_vector = lcl_lisp_new=>vector( it_vector = lt_vector
                                        iv_mutable = iv_mutable ).
    ENDMETHOD.

    METHOD from_list.
      DATA lt_vector TYPE tt_lisp.

      DATA(lo_ptr) = io_list.
      WHILE lo_ptr NE nil.
        APPEND lo_ptr->car TO lt_vector.
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      ro_vector = lcl_lisp_new=>vector( it_vector = lt_vector
                                        iv_mutable = iv_mutable ).
    ENDMETHOD.

    METHOD to_list.
      DATA lo_ptr TYPE REF TO lcl_lisp.

      ASSIGN array[ 1 ] TO FIELD-SYMBOL(<vec>).
      IF sy-subrc EQ 0.
        lo_ptr = ro_elem = lcl_lisp_new=>cons( io_car = <vec> ).
      ELSE.
        ro_elem = nil.
      ENDIF.

      LOOP AT array FROM 2 ASSIGNING <vec>.
        lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = <vec> ).
      ENDLOOP.
    ENDMETHOD.

    METHOD get_list.
      DATA lv_end TYPE tv_index.

      DATA(lv_start) = from + 1.         " start is Inclusive

      IF to IS SUPPLIED.
        lv_end = to.                     " end is Exclusive
      ELSE.
        lv_end = lines( array ).        " End of vector
      ENDIF.

      IF lv_end LT 1 OR lv_start GT lv_end.
        throw( |vector-ref: out-of-bound range| ).
      ENDIF.

      ro_elem = nil.
      CHECK lv_start BETWEEN 1 AND lv_end.

      ro_elem = lcl_lisp_new=>cons( io_car = array[ lv_start ] ).

      DATA(lo_ptr) = ro_elem.
      LOOP AT array FROM lv_start + 1 TO lv_end ASSIGNING FIELD-SYMBOL(<vec>).
        lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = <vec> ).
      ENDLOOP.
    ENDMETHOD.

    METHOD get.
      DATA(lv_start) = index + 1.

      IF lv_start BETWEEN 1 AND lines( array ).
        ro_elem = array[ lv_start ].
      ELSE.
        throw( |vector-ref: out-of-bound position { index }| ).
      ENDIF.
    ENDMETHOD.

    METHOD set.
      _validate_mutable me `vector`.

      DATA(lv_start) = index + 1.

      IF lv_start BETWEEN 1 AND lines( array ).
        array[ lv_start ] = io_elem.
      ELSE.
        throw( |vector-set!: out-of-bound position { index }| ).
      ENDIF.
    ENDMETHOD.

    METHOD fill.
      DATA lv_end TYPE tv_index.
      _validate_mutable me `vector`.
      ro_elem = me.

      DATA(lv_start) = from + 1.         " start is Inclusive
      DATA(lv_size) = lines( array ).
      IF to IS SUPPLIED.
        lv_end = to.                     " end is Exclusive
      ELSE.
        lv_end = lv_size.        " End of vector
      ENDIF.

      IF lv_end LT 1 OR lv_end GT lv_size OR lv_start GT lv_end.
        throw( |vector-fill!: out-of-bound range| ).
      ENDIF.

      LOOP AT array FROM lv_start TO lv_end ASSIGNING FIELD-SYMBOL(<vec>).
        <vec> = elem.
      ENDLOOP.
    ENDMETHOD.

    METHOD length.
      ro_length = mo_length.
    ENDMETHOD.

    METHOD to_string.
      DATA(lo_list) = to_list( ).
      IF lo_list EQ nil.
        str = |#()|.
      ELSE.
        str = |#{ lo_list->to_string( ) }|.
      ENDIF.
    ENDMETHOD.

    METHOD is_equal.
      DATA lo_vec TYPE REF TO lcl_lisp_vector.
      DATA lo_ptr TYPE REF TO lcl_lisp.

      result = abap_false.
      CHECK io_elem->type EQ type_vector.

      lo_vec ?= io_elem.
      CHECK lines( array ) = lines( lo_vec->array ).

      LOOP AT lo_vec->array INTO DATA(lo_elem).
        lo_ptr = array[ sy-tabix ].
        CHECK NOT lo_ptr->is_equal( io_elem = lo_elem
                                    comp = comp
                                    interpreter = interpreter ).
        RETURN.
      ENDLOOP.
      result = abap_true.
    ENDMETHOD.

    METHOD set_shared_structure.
      FIELD-SYMBOLS <lo_elem> TYPE REF TO lcl_lisp.

      CHECK mv_label IS NOT INITIAL.

      CASE type.
        WHEN type_vector.
          LOOP AT array ASSIGNING <lo_elem> WHERE table_line->type = type_symbol
                                              AND table_line->value = mv_label.
            <lo_elem> = me.
            RETURN.
          ENDLOOP.

        WHEN OTHERS.
          super->set_shared_structure( ).
      ENDCASE.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_bytevector IMPLEMENTATION.

    METHOD init.
      DATA lt_byte TYPE tt_byte.

      DO size TIMES.
        APPEND iv_fill TO lt_byte.
      ENDDO.
      ro_u8 = lcl_lisp_new=>bytevector( it_byte = lt_byte
                                        iv_mutable = iv_mutable ).
    ENDMETHOD.

    METHOD from_list.
      DATA lt_byte TYPE tt_byte.
      DATA lv_int TYPE tv_int.

      DATA(lo_ptr) = io_list.
      WHILE lo_ptr NE nil.
        _validate_byte lo_ptr->car `bytevector`.
        _to_integer lo_ptr->car lv_int.
        APPEND lv_int TO lt_byte.
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      ro_u8 = lcl_lisp_new=>bytevector( it_byte = lt_byte
                                        iv_mutable = iv_mutable ).
    ENDMETHOD.

    METHOD to_string.
      CONSTANTS c_max_ascii TYPE x VALUE '7F'.
      CONSTANTS c_low_byte TYPE x VALUE '0F'.
      CONSTANTS c_high_byte TYPE x VALUE 'F0'.

      DATA lv_str TYPE string.
      DATA lv_char1 TYPE tv_char.
      DATA lv_char2 TYPE c LENGTH 2.
      DATA lv_hex TYPE x.
      FIELD-SYMBOLS <byte> TYPE tv_byte.

      DATA lv_idx TYPE x.

      LOOP AT bytes TRANSPORTING NO FIELDS WHERE table_line GT c_max_ascii.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        " non ASCII chars
        LOOP AT bytes ASSIGNING <byte>.
          lv_hex = <byte>.
          "WRITE lv_hex TO lv_char2.
          "lv_str &&= ` #x` && lv_char2.

          lv_idx = lv_hex BIT-AND c_high_byte.
          lv_idx = lv_idx DIV 16.
          lv_char1 = c_hex_digits+lv_idx(1).
          lv_str &&= ` #x` && lv_char1.

          lv_idx = lv_hex BIT-AND c_low_byte.
          lv_char1 = c_hex_digits+lv_idx(1).
          lv_str &&= lv_char1.
        ENDLOOP.
      ELSE.
        LOOP AT bytes ASSIGNING <byte>.
          lv_hex = <byte>.
          "WRITE <byte> TO lv_char2.
          "lv_str = lv_str && ` ` && lv_char2.
          DATA lv_int TYPE i.
          lv_int = lv_hex.

          lv_char2 = lv_int.
          lv_str &&= ` ` && lv_char2.
        ENDLOOP.
      ENDIF.

      IF lv_str IS NOT INITIAL.
        lv_str &&= ` `.
      ENDIF.

      str = |#u8{ c_open_paren }{ lv_str }{ c_close_paren }|.

    ENDMETHOD.

    METHOD utf8_from_string.
      " UTF-16 String to UTF-8 bytes
      DATA lv_idx TYPE tv_int.
      DATA lv_byte TYPE tv_byte.
      DATA lt_byte TYPE tt_byte.
      DATA lv_buffer TYPE xstring.

      DATA(lo_conv) = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
      lo_conv->convert( EXPORTING data   = iv_text
                        IMPORTING buffer = lv_buffer ).
      DO xstrlen( lv_buffer ) TIMES.
        lv_byte = lv_buffer+lv_idx(1).
        lv_idx = lv_idx + 1.
        APPEND lv_byte TO lt_byte.
      ENDDO.

      ro_u8 = lcl_lisp_new=>bytevector( it_byte = lt_byte
                                        iv_mutable = iv_mutable ).
    ENDMETHOD.

    METHOD utf8_to_string.
      DATA lv_buffer TYPE xstring.
      DATA lv_hex TYPE x LENGTH 1.

      DATA(lv_from) = from + 1.
      DATA(lv_to) = to.
      LOOP AT bytes FROM lv_from TO lv_to INTO DATA(lv_byte).
        lv_hex = lv_byte.
        lv_buffer &&= lv_hex.
      ENDLOOP.

      DATA(lo_conv) = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
      lo_conv->convert( EXPORTING input = lv_buffer
                        IMPORTING data = rv_text ).
    ENDMETHOD.

    METHOD set.
      _validate_mutable me `bytevector`.

      DATA(lv_start) = index + 1.

      IF lv_start BETWEEN 1 AND lines( bytes ).
        bytes[ lv_start ] = iv_byte.
      ELSE.
        throw( |bytevector-u8-set!: out-of-bound position { index }| ).
      ENDIF.
    ENDMETHOD.

    METHOD get.
      DATA(lv_start) = index + 1.

      IF lv_start BETWEEN 1 AND lines( bytes ).
        rv_byte = bytes[ lv_start ].
      ELSE.
        throw( |bytevector-u8-ref: out-of-bound position { index }| ).
      ENDIF.
    ENDMETHOD.

    METHOD copy_new.
      DATA lt_byte TYPE tt_byte.
      DATA lv_u8 TYPE tv_byte.
      DATA lv_to TYPE tv_index.

      DATA(lv_from) = from + 1.          " from is Inclusive

      IF to IS INITIAL.
        lv_to = lines( bytes ).          " to is Exclusive
      ELSE.
        lv_to = to.                      " End of bytevector
      ENDIF.

      LOOP AT bytes INTO lv_u8 FROM lv_from TO lv_to.
        APPEND lv_u8 TO lt_byte.
      ENDLOOP.

      ro_elem = lcl_lisp_new=>bytevector( it_byte = lt_byte
                                          iv_mutable = mutable ).
    ENDMETHOD.

    METHOD copy.
      " bytevector-copy!
      DATA lv_end TYPE tv_index.

      DATA(lv_start) = start + 1.           " from is Inclusive

      IF end IS INITIAL.
        lv_end = lines( io_from->bytes ).   " to is Exclusive
      ELSE.
        lv_end = end.                       " End of bytevector
      ENDIF.

      DATA(lv_at) = at + 1.
      DATA(lv_max) = lv_end - lv_start + lv_at.

      DATA(lv_idx) = lv_start.
      LOOP AT bytes FROM lv_at TO lv_max ASSIGNING FIELD-SYMBOL(<lv_byte>).
        <lv_byte> = io_from->bytes[ lv_idx ].
        lv_idx = lv_idx + 1.
      ENDLOOP.

    ENDMETHOD.

    METHOD length.
      ro_length = mo_length.
    ENDMETHOD.

    METHOD is_equal.
      DATA lo_u8 TYPE REF TO lcl_lisp_bytevector.

      result = abap_false.
      CHECK io_elem->type EQ type_bytevector.

      lo_u8 ?= io_elem.
      CHECK bytes = lo_u8->bytes.

      result = abap_true.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_query IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_abap_query ).
      sql_query = osql.
      connection = cl_sql_connection=>get_connection( ).
      IF value IS INITIAL.
        statement = connection->create_statement( ).
      ELSE.
        statement = connection->prepare_statement( sql_query ).
      ENDIF.
      mv_hold_cursor = abap_false.
    ENDMETHOD.

    METHOD execute.
*     Development not completed yet
      DATA lo_set TYPE REF TO cl_sql_result_set.
      IF sql_query IS NOT INITIAL.
*       prepared statement
*       statement->set_param( dref1 ).
        lo_set = statement->execute_query( hold_cursor = mv_hold_cursor ).
      ELSEIF query IS NOT INITIAL.
        lo_set = statement->execute_query( statement = query
                                           hold_cursor = mv_hold_cursor ).
*     ELSE ? which query to execute
      ENDIF.
      result = lcl_lisp_new=>result_set( lo_set ).
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_sql_result IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_abap_sql_set ).
      result_set = io_result.
    ENDMETHOD.

    METHOD clear.
      CHECK result_set IS BOUND.
      result_set->clear_parameters( ).
    ENDMETHOD.

    METHOD close.
      CHECK result_set IS BOUND.
      result_set->close( ).
      FREE result_set.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_turtle IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_abap_turtle ).
      turtle = lcl_turtle=>new( height = height->int
                                width = width->int
                                title = `SchemeTurtle` ).
      turtle->set_position( VALUE #( x = init_x->int
                                     y = init_y->int
                                     angle = init_angle->real ) ).
    ENDMETHOD.

  ENDCLASS.
