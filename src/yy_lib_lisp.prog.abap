*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include           YY_LIB_LISP
*& https://github.com/nomssi/abap_scheme
*& https://github.com/mydoghasworms/abap-lisp
*& Lisp interpreter written in ABAP
*& Copy and paste this code into a type I (include) program
*&---------------------------------------------------------------------*
*& MIT License (see below)
*& Martin Ceronio, martin.ceronio@infosize.co.za June 2015
*& Jacques Nomssi Nzali, www.informatik-dv.com April 2018
*& Turtle Graphics placed under The Unlicense by Frederik Hudák
*&---------------------------------------------------------------------*
*  The MIT License (MIT)
*
*  Copyright (c) 2020 Jacques Nomssi Nzali
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

  DATA gv_lisp_trace TYPE flag VALUE abap_false ##NEEDED.

  CONSTANTS:
    c_lisp_input     TYPE string VALUE 'ABAP Lisp Input' ##NO_TEXT,
    c_lisp_eof       TYPE x LENGTH 2 VALUE 'FFFF', " we do not expect this in source code
    c_lisp_nil       TYPE string VALUE '''()',
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
    c_decimal_digits TYPE char10 VALUE '0123456789',
    c_hex_digits     TYPE c LENGTH 16 VALUE '0123456789ABCDEF',
    c_abcde          TYPE string VALUE `ABCDEFGHIJKLMNOPQRSTUVWXYZ`. " sy-abcde

  TYPES tv_char TYPE c LENGTH 1.
  TYPES tv_int TYPE int8.         " integer data type, use int8 if available
  TYPES tv_index TYPE tv_int.
  TYPES tv_real TYPE decfloat34.  " floating point data type
  TYPES tv_xword TYPE x LENGTH 2.

  CLASS lcl_demo_output DEFINITION.
    PUBLIC SECTION.
      METHODS constructor IMPORTING out TYPE REF TO if_demo_output.
      METHODS write IMPORTING iv_text TYPE any.
      METHODS display.
      METHODS begin_section IMPORTING iv_text TYPE any.
    PRIVATE SECTION.
      DATA out TYPE REF TO if_demo_output.
  ENDCLASS.

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

  DEFINE _trace_call.
    IF gv_lisp_trace EQ abap_true.
      cl_demo_output=>write( |call { &1->value } { &1->to_string( ) } param { &2->to_string( ) }| ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _trace_result.
    IF gv_lisp_trace EQ abap_true.
      cl_demo_output=>write( |=> { &1->to_string( ) }| ).
    ENDIF.
  END-OF-DEFINITION.

* Macro to simplify the definition of a native procedure
  DEFINE _proc_meth.
    METHODS &1 IMPORTING list TYPE REF TO lcl_lisp
               RETURNING VALUE(result) TYPE REF TO lcl_lisp
               RAISING lcx_lisp_exception.
  END-OF-DEFINITION.

  DEFINE _assert_is_bound.
    IF &1 IS NOT BOUND.
      lcl_lisp=>throw( &2 ).
    ENDIF.
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
    _validate_integer &1 &2.
    IF CAST lcl_lisp_integer( &1 )->int NOT BETWEEN 0 AND 255.
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

  DEFINE _error_no_list.
    throw( |{ &2 }: { &1->to_string( ) } is not a proper list| ) ##NO_TEXT.
  END-OF-DEFINITION.

  DEFINE _to_integer.
    &2 = CAST lcl_lisp_integer( &1 )->int.
  END-OF-DEFINITION.

  DEFINE _to_real.
    &2 = CAST lcl_lisp_real( &1 )->real.
  END-OF-DEFINITION.

  DEFINE _validate_tail.
    IF &1 NE nil.
*     if the last element in the list is not a cons cell, we cannot append
      _error_no_list &2 &3.
    ENDIF.
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

  DEFINE _get_number.
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
*      WHEN type_complex.
      WHEN OTHERS.
        cell->raise_nan( &3 ).
    ENDCASE.
  END-OF-DEFINITION.

  DEFINE _get_2_ints.
    _validate: &4, &4->car, &4->cdr, &4->cdr->car.

    CASE &4->car->type.
      WHEN type_integer.
         lo_int = CAST lcl_lisp_integer( &4->car ).
         &1 = lo_int->int.
         &3 = lo_int->exact.
      WHEN type_real.
         TRY.
           lo_real = CAST lcl_lisp_real( &4->car ).
           MOVE EXACT lo_real->real TO &1.
           &3 = lo_real->exact.
         CATCH cx_sy_conversion_error.
           throw( lo_real->to_string( ) && | is not an integer in [{ &5 }]| ).
         ENDTRY.
      WHEN OTHERS.
        throw( &4->car->to_string( ) && | is not an integer in [{ &5 }]| ).
    ENDCASE.

    CASE &4->cdr->car->type.
      WHEN type_integer.
         lo_int = CAST lcl_lisp_integer( &4->cdr->car ).
         &2 = lo_int->int.
        IF &2 EQ 0.
          throw( | second parameter cannot be zero in [{ &5 }]| ).
        ENDIF.
        IF exact EQ abap_true.
          exact = lo_int->exact.
        ENDIF.
      WHEN OTHERS.
        throw( &4->cdr->car->to_string( ) && | is not an integer in [{ &5 }]| ).
    ENDCASE.

    &4->cdr->assert_last_param( ).
  END-OF-DEFINITION.

  DEFINE _data_local_numeric.
    DATA lo_rat TYPE REF TO lcl_lisp_rational.
    DATA lo_int TYPE REF TO lcl_lisp_integer.
    DATA lo_real TYPE REF TO lcl_lisp_real.
  END-OF-DEFINITION.

  DEFINE _data_local_numeric_cell.
    DATA cell TYPE REF TO lcl_lisp.
    DATA lo_head TYPE REF TO lcl_lisp.
    _data_local_numeric.
  END-OF-DEFINITION.

* Macro that implements the logic for the comparison native
* procedures, where only the comparison operator differs
  DEFINE _comparison.
    DATA carry TYPE tv_real.
    DATA carry_int TYPE tv_int.
    DATA carry_is_int TYPE flag.
    _data_local_numeric_cell.

    result = false.
    _validate: list, list->car, list->cdr.
    IF list->cdr->type NE type_pair.
      throw( c_error_incorrect_input ).
    ENDIF.

    cell = list->car.
    _values_get_next cell.
    carry_is_int = abap_false.
    CASE cell->type.
      WHEN type_integer.
        carry_is_int = abap_true.
        _to_integer cell carry_int.
        carry = carry_int.

      WHEN type_real.
        _to_real cell carry.

      WHEN type_rational.
        lo_rat ?= cell.
        carry = lo_rat->int / lo_rat->denominator.
*          WHEN type_complex.
      WHEN OTHERS.
        cell->raise_nan( &2 ).
    ENDCASE.

    cell = list->cdr.
    _values_get_next cell.
    WHILE cell->type EQ type_pair.
      _validate cell->car.

      CASE cell->car->type.
        WHEN type_integer.
          lo_int ?= cell->car.
          IF carry_is_int = abap_true.
            IF carry_int &1 lo_int->int.
              RETURN.
            ENDIF.
            carry_int = lo_int->int.
          ELSE.
            IF carry &1 lo_int->int.
              RETURN.
            ENDIF.
          ENDIF.
          carry = lo_int->int.

        WHEN type_real.
          carry_is_int = abap_false.
          lo_real ?= cell->car.
          IF carry &1 lo_real->real.
            RETURN.
          ENDIF.
          carry = lo_real->real.

        WHEN type_rational.
          carry_is_int = abap_false.
          lo_rat ?= cell->car.
          IF carry * lo_rat->denominator &1 lo_rat->int.
            RETURN.
          ENDIF.
          carry = lo_rat->int / lo_rat->denominator.

      " WHEN type_complex.

        WHEN OTHERS.
          cell->car->raise_nan( &2 ).
      ENDCASE.
      cell = cell->cdr.
    ENDWHILE.
    result = true.
  END-OF-DEFINITION.

  DEFINE _sign.
    DATA carry TYPE tv_real.
    _data_local_numeric_cell.

    result = false.
    _validate list.
    _get_number carry list->car &2.

    IF sign( carry ) NE &1.
      RETURN.
    ENDIF.
    result = true.
  END-OF-DEFINITION.

  DEFINE _is_type. " argument in list->car
    _validate list.
    result = false.
    CHECK list->car IS BOUND.
    IF list->car->type EQ type_&1.
      result = true.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _assert_last_param.
    IF &1->cdr NE nil.
      throw( |{ &1->to_string( ) } Parameter mismatch| ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _catch_arithmetic_error.
    CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
      throw( lx_error->get_text( ) ).
  END-OF-DEFINITION.

* Macro that implements the logic for call of ABAP math statements
  DEFINE _math.
    DATA carry TYPE tv_real.
    _data_local_numeric_cell.

    result = nil.
    _validate list.
    TRY.
        _get_number carry list->car &2.
        list->assert_last_param( ).

        result = lcl_lisp_new=>number( value = &1( carry )
                                       iv_exact = CAST lcl_lisp_number( cell )->exact ).
      _catch_arithmetic_error.
    ENDTRY.
  END-OF-DEFINITION.

  DEFINE _trigonometric.
    DATA carry TYPE f.
    DATA lo_number TYPE REF TO lcl_lisp_number.
    _data_local_numeric_cell.

    result = nil.
    _validate list.
    TRY.
        _get_number carry list->car &2.
        _assert_last_param list.
        lo_number ?= list->car.
        result = lcl_lisp_new=>real( value = &1( carry )
                                     exact = lo_number->exact ).
      _catch_arithmetic_error.
    ENDTRY.
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

  CLASS lcl_lisp_environment DEFINITION DEFERRED.

  TYPES tv_type TYPE tv_char.
  TYPES tv_category TYPE tv_char.

  CONSTANTS:
    tv_category_standard TYPE tv_category VALUE space,
    tv_category_macro TYPE tv_category VALUE 'X',
    tv_category_escape TYPE tv_category VALUE '@'.

*  Type definitions for the various elements
  CONSTANTS:
    type_symbol   TYPE tv_type VALUE 'S',
    type_integer  TYPE tv_type VALUE 'N',
    type_real     TYPE tv_type VALUE 'R',
    type_complex  TYPE tv_type VALUE 'z',
    type_rational TYPE tv_type VALUE 'r',
    type_string   TYPE tv_type VALUE '"',

    type_boolean     TYPE tv_type VALUE 'b',
    type_char        TYPE tv_type VALUE 'c',
    type_null        TYPE tv_type VALUE '0',
    type_pair        TYPE tv_type VALUE 'C',
    type_lambda      TYPE tv_type VALUE 'L',
    type_case_lambda TYPE tv_type VALUE 'A',
    type_native      TYPE tv_type VALUE 'n',
    type_primitive   TYPE tv_type VALUE 'I',
    type_syntax      TYPE tv_type VALUE 'y',
    type_hash        TYPE tv_type VALUE 'h',
    type_vector      TYPE tv_type VALUE 'v',
    type_bytevector  TYPE tv_type VALUE '8',
    type_port        TYPE tv_type VALUE 'o',
    type_not_defined TYPE tv_type VALUE space,

     " Types for ABAP integration:
    type_abap_data     TYPE tv_type VALUE 'D',
    type_abap_table    TYPE tv_type VALUE 'T',
    type_abap_query    TYPE tv_type VALUE 'q',
    type_abap_sql_set  TYPE tv_type VALUE 's',
    type_abap_function TYPE tv_type VALUE 'F',
*    type_abap_class    TYPE tv_type VALUE 'a',
*    type_abap_method   TYPE tv_type VALUE 'm',

    "type_env_spec    TYPE tv_type VALUE 'e',
    type_values      TYPE tv_type VALUE 'V',
    type_abap_turtle TYPE tv_type VALUE 't'.  " for Turtles graphic

  TYPES: BEGIN OF ts_result,
              type TYPE tv_type,
              int TYPE tv_int,
              real TYPE tv_real,
              nummer TYPE tv_int,
              denom TYPE tv_int,
              exact TYPE flag,
              operation TYPE string,
         END OF ts_result.

  CLASS lcl_lisp_iterator DEFINITION DEFERRED.
  CLASS lcl_lisp_new DEFINITION DEFERRED.
  CLASS lcl_lisp_interpreter DEFINITION DEFERRED.

* Single element that will capture cons cells, atoms etc.
*----------------------------------------------------------------------*
*       CLASS lcl_lisp DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_lisp DEFINITION CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
    " Symbolic expression (S-expression)
      DATA type TYPE tv_type.

*     Can this be replaced by a mesh? cf. DEMO_RND_PARSER_AST
      DATA mutable TYPE flag VALUE abap_true READ-ONLY.

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
      CLASS-DATA char_newline     TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA char_null        TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA char_return      TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA char_space       TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA char_tab         TYPE REF TO  lcl_lisp READ-ONLY.

      CLASS-DATA new_line   TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA eof_object TYPE REF TO  lcl_lisp READ-ONLY.

*     Specifically for lambdas:
      DATA environment TYPE REF TO lcl_lisp_environment.
*     Format
      METHODS to_string RETURNING VALUE(str) TYPE string
                        RAISING   lcx_lisp_exception.
      METHODS to_text RETURNING VALUE(str) TYPE string
                      RAISING   lcx_lisp_exception.
*     Utilities
      METHODS new_iterator RETURNING VALUE(ro_iter) TYPE REF TO lcl_lisp_iterator
                           RAISING   lcx_lisp_exception.

      METHODS is_equivalent IMPORTING io_elem       TYPE REF TO lcl_lisp
                            RETURNING VALUE(result) TYPE REF TO lcl_lisp
                            RAISING   lcx_lisp_exception.

      METHODS is_equal IMPORTING io_elem       TYPE REF TO lcl_lisp
                                 comp          TYPE REF TO lcl_lisp DEFAULT nil
                                 interpreter   TYPE REF TO lcl_lisp_interpreter OPTIONAL
                                 environment   TYPE REF TO lcl_lisp_environment OPTIONAL
                       RETURNING VALUE(result) TYPE REF TO lcl_lisp
                       RAISING   lcx_lisp_exception.

      METHODS set_shared_structure RAISING lcx_lisp_exception.

      METHODS is_procedure RETURNING VALUE(result) TYPE REF TO lcl_lisp.

      METHODS is_number RETURNING VALUE(result) TYPE REF TO lcl_lisp.

      METHODS error_not_a_pair IMPORTING context TYPE string DEFAULT space
                               RAISING   lcx_lisp_exception.

      METHODS raise IMPORTING context TYPE string DEFAULT space
                              message TYPE string
                    RAISING   lcx_lisp_exception.

      METHODS raise_NaN IMPORTING operation TYPE string
                        RAISING   lcx_lisp_exception.

      METHODS assert_last_param  RAISING lcx_lisp_exception.

      CLASS-METHODS throw IMPORTING message TYPE string
                          RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      METHODS constructor IMPORTING type TYPE tv_type.
      METHODS list_to_string RETURNING VALUE(str) TYPE string
                             RAISING   lcx_lisp_exception.
      METHODS values_to_string RETURNING VALUE(str) TYPE string
                               RAISING   lcx_lisp_exception.

      METHODS format_quasiquote IMPORTING io_elem TYPE REF TO lcl_lisp
                                EXPORTING ev_skip TYPE flag
                                          ev_str  TYPE string.
  ENDCLASS.                    "lcl_lisp DEFINITION

  TYPES tt_lisp TYPE STANDARD TABLE OF REF TO lcl_lisp WITH EMPTY KEY.

  TYPES tv_byte TYPE int1.
  TYPES tt_byte TYPE STANDARD TABLE OF tv_byte WITH EMPTY KEY.

  CLASS lcl_lisp_char DEFINITION INHERITING FROM lcl_lisp
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      CLASS-METHODS new IMPORTING value          TYPE any
                        RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_char.
    PROTECTED SECTION.
      TYPES: BEGIN OF ts_char,
               char TYPE c LENGTH 1,
               elem TYPE REF TO lcl_lisp_char,
             END OF ts_char.
      CLASS-DATA char_table TYPE HASHED TABLE OF ts_char WITH UNIQUE KEY char.

      METHODS constructor IMPORTING value TYPE any.
  ENDCLASS.

  CLASS lcl_lisp_char IMPLEMENTATION.

    METHOD new.
      DATA lv_char TYPE char01.
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
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_string DEFINITION INHERITING FROM lcl_lisp
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
    PROTECTED SECTION.
      METHODS constructor IMPORTING value      TYPE any
                                    iv_mutable TYPE flag.
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
      METHODS is_exact RETURNING VALUE(result) TYPE REF TO lcl_lisp.
      METHODS is_inexact RETURNING VALUE(result) TYPE REF TO lcl_lisp.
      DATA exact TYPE flag READ-ONLY.
  ENDCLASS.

  CLASS lcl_lisp_number IMPLEMENTATION.

    METHOD is_exact.
      result = false.
      CHECK exact EQ abap_true.
      result = true.
    ENDMETHOD.

    METHOD is_inexact.
      result = true.
      CHECK exact EQ abap_true.
      result = false.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_integer DEFINITION INHERITING FROM lcl_lisp_number
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA int TYPE tv_int READ-ONLY.
    PROTECTED SECTION.
      METHODS constructor IMPORTING value TYPE any
                                    iv_exact TYPE flag.
  ENDCLASS.

  CLASS lcl_lisp_rational DEFINITION INHERITING FROM lcl_lisp_integer
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      CLASS-METHODS new IMPORTING nummer        TYPE tv_int
                                  denom         TYPE tv_int
                                  iv_exact      TYPE flag
                        RETURNING VALUE(result) TYPE REF TO lcl_lisp_integer.

      DATA denominator TYPE tv_int READ-ONLY.
      METHODS to_string REDEFINITION.
      CLASS-METHODS gcd IMPORTING n             TYPE numeric
                                  d             TYPE numeric
                        RETURNING VALUE(result) TYPE tv_int
                        RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      METHODS constructor IMPORTING nummer TYPE tv_int
                                    denom  TYPE tv_int
                                    iv_exact TYPE flag.
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
      CREATE OBJECT lo_rat
        EXPORTING
          nummer = nummer
          denom  = denom
          iv_exact = iv_exact.
      IF lo_rat->denominator EQ 1.
        CREATE OBJECT result TYPE lcl_lisp_integer
          EXPORTING value = lo_rat->int
                     iv_exact = iv_exact.
      ELSE.
        result = lo_rat.
      ENDIF.
    ENDMETHOD.

    METHOD constructor.
      super->constructor( value = nummer
                          iv_exact = iv_exact ).
      type = type_rational.
      denominator = denom.
      normalize( ).
    ENDMETHOD.

    METHOD to_string.
      IF exact EQ abap_false.
        str = `#i`.
      ENDIF.
      str = str && |{ int }/{ denominator }|.
    ENDMETHOD.

    METHOD normalize.
      DATA(g) = gcd( n = int
                     d = denominator ).
      int = trunc( int / g ).
      denominator = trunc( denominator / g ).
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
            throw( lx_error->get_text( ) ).
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
      METHODS float_eq IMPORTING iv_real       TYPE tv_real
                       RETURNING VALUE(result) TYPE flag.
      CLASS-METHODS gcd IMPORTING n             TYPE numeric
                                  d             TYPE numeric
                        RETURNING VALUE(result) TYPE tv_real
                        RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      METHODS constructor IMPORTING value TYPE any
                                    exact TYPE flag.
  ENDCLASS.

  CLASS lcl_lisp_real IMPLEMENTATION.

    METHOD constructor.
      super->constructor( type_real ).
      real = value.
      IF exact EQ abap_true AND trunc( real ) = real.
        me->exact = abap_true.
      ELSE.
        me->exact = abap_false.
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
      IF real EQ iv_real.
        result = abap_true.
      ELSEIF exact EQ abap_false.
        abs_a = abs( real ).
        abs_b = abs( iv_real ).
        diff = abs( real - iv_real ).
        sum = abs_a + abs_b.

        IF real = 0 OR iv_real = 0 OR sum < c_min_normal.
*         real or iv_real is zero or both are extremely close to it
*         relative error is less meaningfull here
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

      num = NEW #( value = n exact = abap_false ).
      den = NEW #( value = d exact = abap_false ).
      WHILE NOT den->float_eq( 0 ).
        lo_save = den.
        TRY.
            den = NEW #( value = num->real MOD den->real
                         exact = abap_false  ).
*            den = NEW #( num->real - den->real * trunc( num->real / den->real ) ).
          CATCH cx_sy_arithmetic_error INTO DATA(lx_error).
            throw( lx_error->get_text( ) ).
        ENDTRY.
        num = lo_save.
      ENDWHILE.
      result = abs( num->real ).
    ENDMETHOD.

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
      METHODS add IMPORTING io_value TYPE REF TO lcl_lisp
                  RETURNING VALUE(result) TYPE REF TO lcl_lisp_values.
    PROTECTED SECTION.
      DATA last TYPE REF TO lcl_lisp.
  ENDCLASS.

  CLASS lcl_lisp_lambda DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA parameter_object TYPE flag.
      METHODS constructor IMPORTING io_car              TYPE REF TO lcl_lisp
                                    io_cdr              TYPE REF TO lcl_lisp
                                    iv_category         TYPE tv_category DEFAULT tv_category_standard
                                    iv_parameter_object TYPE flag DEFAULT abap_false.
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

      throw( `no clause matching the arguments` ).

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

  INTERFACE lif_input_port.
    METHODS read IMPORTING iv_title        TYPE string OPTIONAL
                 RETURNING VALUE(rv_input) TYPE string.
    METHODS peek_char RETURNING VALUE(rv_char) TYPE char01.
    METHODS is_char_ready RETURNING VALUE(rv_flag) TYPE flag.
    METHODS read_char RETURNING VALUE(rv_char) TYPE char01.
    METHODS put IMPORTING iv_text TYPE string.
  ENDINTERFACE.

  INTERFACE lif_output_port.
    METHODS write IMPORTING element TYPE REF TO lcl_lisp.
    METHODS display IMPORTING element TYPE REF TO lcl_lisp
                    RAISING   lcx_lisp_exception.
  ENDINTERFACE.

  CLASS lcl_lisp_port DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      TYPES tv_port_type TYPE char01.
      CONSTANTS:
        c_port_textual TYPE tv_port_type VALUE 't',
        c_port_binary  TYPE tv_port_type VALUE 'b'.

      INTERFACES lif_input_port.
      INTERFACES lif_output_port.

      ALIASES: read FOR lif_input_port~read,
               write FOR lif_output_port~write,
               display FOR lif_output_port~display,
               set_input_string FOR lif_input_port~put.
      METHODS constructor IMPORTING iv_port_type TYPE lcl_lisp_port=>tv_port_type DEFAULT c_port_textual
                                    iv_input     TYPE flag
                                    iv_output    TYPE flag
                                    iv_error     TYPE flag DEFAULT abap_false.
      METHODS close.
      METHODS close_input.
      METHODS close_output.

      METHODS read_stream IMPORTING iv_title        TYPE string OPTIONAL
                          RETURNING VALUE(rv_input) TYPE string.

      DATA port_type TYPE tv_port_type READ-ONLY.
      DATA input TYPE flag READ-ONLY.
      DATA output TYPE flag READ-ONLY.
      DATA error TYPE flag READ-ONLY.
    PROTECTED SECTION.
*     input is always buffered
      DATA last_input TYPE string.
      DATA last_index TYPE tv_index.
      DATA last_len TYPE tv_index.
      DATA finite_size TYPE flag.

      METHODS block_read RETURNING VALUE(rv_char) TYPE char01.
  ENDCLASS.

  CLASS lcl_lisp_buffered_port DEFINITION INHERITING FROM lcl_lisp_port FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      INTERFACES lif_log.
      METHODS write REDEFINITION.
      METHODS read REDEFINITION.
      METHODS lif_input_port~peek_char REDEFINITION.
      METHODS display REDEFINITION.
      METHODS flush RETURNING VALUE(rv_text) TYPE string.
      METHODS constructor IMPORTING iv_port_type TYPE lcl_lisp_port=>tv_port_type DEFAULT c_port_textual
                                    iv_input     TYPE flag
                                    iv_output    TYPE flag
                                    iv_error     TYPE flag DEFAULT abap_false
                                    iv_separator TYPE string DEFAULT c_expr_separator
                                    iv_string    TYPE flag.
    PROTECTED SECTION.
      DATA string_mode TYPE flag.
      DATA buffer TYPE string.
      DATA separator TYPE string.

      METHODS add IMPORTING text TYPE string.
  ENDCLASS.

  INCLUDE yy_lib_turtle.

  CLASS lcl_lisp_turtle DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      METHODS constructor IMPORTING width TYPE REF TO lcl_lisp_integer
                                    height TYPE REF TO lcl_lisp_integer
                                    init_x TYPE REF TO lcl_lisp_integer
                                    init_y TYPE REF TO lcl_lisp_integer
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

  CLASS lcl_lisp_new DEFINITION.
    PUBLIC SECTION.
      CLASS-METHODS atom IMPORTING value          TYPE string
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS null RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS undefined RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS symbol IMPORTING value          TYPE any
                                     index          TYPE tv_int OPTIONAL
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_symbol.
      CLASS-METHODS boolean IMPORTING value          TYPE any
                            RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS integer IMPORTING value          TYPE any
                                      iv_exact       TYPE flag DEFAULT abap_true
                            RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_integer.
      CLASS-METHODS rational IMPORTING nummer         TYPE tv_int
                                       denom          TYPE tv_int
                                       iv_exact       TYPE flag DEFAULT abap_true
                             RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_integer
                             RAISING   lcx_lisp_exception.

      CLASS-METHODS real IMPORTING value          TYPE any
                                   exact          TYPE flag
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_real.

      CLASS-METHODS number IMPORTING value          TYPE any
                                     iv_exact       TYPE flag OPTIONAL
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                           RAISING   cx_sy_conversion_no_number.

      CLASS-METHODS numeric IMPORTING record TYPE ts_result
                            RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                            RAISING   lcx_lisp_exception.

      CLASS-METHODS binary_integer IMPORTING value         TYPE csequence
                                   RETURNING VALUE(rv_int) TYPE tv_int
                                   RAISING   cx_sy_conversion_no_number.
      CLASS-METHODS binary IMPORTING value          TYPE any
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                           RAISING   cx_sy_conversion_no_number.
      CLASS-METHODS octal IMPORTING value          TYPE any
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                          RAISING   lcx_lisp_exception cx_sy_conversion_no_number.
      CLASS-METHODS octal_integer IMPORTING value         TYPE csequence
                                  RETURNING VALUE(rv_int) TYPE tv_int
                                  RAISING   lcx_lisp_exception cx_sy_conversion_no_number.

      CLASS-METHODS hex IMPORTING value          TYPE any
                        RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                        RAISING   lcx_lisp_exception cx_sy_conversion_no_number.
      CLASS-METHODS hex_integer IMPORTING value         TYPE csequence
                                RETURNING VALUE(rv_int) TYPE tv_int
                                RAISING   lcx_lisp_exception cx_sy_conversion_no_number.

      CLASS-METHODS string IMPORTING value          TYPE any
                                     iv_mutable     TYPE flag DEFAULT abap_true
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_string.
      CLASS-METHODS char IMPORTING value          TYPE any
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_char.
      CLASS-METHODS charx IMPORTING value          TYPE any
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_char.
      CLASS-METHODS hex_to_char IMPORTING value          TYPE any
                                RETURNING VALUE(rv_char) TYPE tv_char.
      CLASS-METHODS port IMPORTING iv_port_type   TYPE lcl_lisp_port=>tv_port_type
                                   iv_input       TYPE flag
                                   iv_output      TYPE flag
                                   iv_error       TYPE flag
                                   iv_buffered    TYPE flag
                                   iv_separator   TYPE string OPTIONAL
                                   iv_string      TYPE flag DEFAULT abap_false
                         RETURNING VALUE(ro_port) TYPE REF TO lcl_lisp_port.

      CLASS-METHODS elem IMPORTING type           TYPE tv_type
                                   value          TYPE any OPTIONAL
                                   parameter      TYPE flag DEFAULT abap_false
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS data IMPORTING ref            TYPE REF TO data OPTIONAL
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_data.
      CLASS-METHODS table IMPORTING ref            TYPE REF TO data OPTIONAL
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_table.
      CLASS-METHODS query IMPORTING value          TYPE any OPTIONAL
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS result_set IMPORTING io_result TYPE REF TO cl_sql_result_set
                               RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_sql_result.

      CLASS-METHODS cons IMPORTING io_car         TYPE REF TO lcl_lisp DEFAULT lcl_lisp=>nil
                                   io_cdr         TYPE REF TO lcl_lisp DEFAULT lcl_lisp=>nil
                         RETURNING VALUE(ro_cons) TYPE REF TO lcl_lisp.

      CLASS-METHODS list3 IMPORTING io_first       TYPE REF TO lcl_lisp
                                    io_second      TYPE REF TO lcl_lisp
                                    io_third       TYPE REF TO lcl_lisp
                          RETURNING VALUE(ro_cons) TYPE REF TO lcl_lisp.

      CLASS-METHODS vector IMPORTING it_vector     TYPE tt_lisp
                                     iv_mutable    TYPE flag
                           RETURNING VALUE(ro_vec) TYPE REF TO lcl_lisp_vector.

      CLASS-METHODS bytevector IMPORTING it_byte       TYPE tt_byte
                                         iv_mutable    TYPE flag
                               RETURNING VALUE(ro_u8) TYPE REF TO lcl_lisp_bytevector.

      CLASS-METHODS lambda IMPORTING io_car              TYPE REF TO lcl_lisp
                                     io_cdr              TYPE REF TO lcl_lisp
                                     io_env              TYPE REF TO lcl_lisp_environment
                                     iv_category         TYPE tv_category DEFAULT tv_category_standard
                                     iv_parameter_object TYPE flag DEFAULT abap_false
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


      CLASS-METHODS values IMPORTING io_elem TYPE REF TO lcl_lisp DEFAULT lcl_lisp=>nil
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_values.
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
      rv_input = c_lisp_eof.
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
      CHECK output EQ abap_true.
      RETURN.
    ENDMETHOD.

    METHOD lif_output_port~display.
      CHECK output EQ abap_true.
      RETURN.
    ENDMETHOD.

    METHOD read_stream.
      DATA lt_fields TYPE STANDARD TABLE OF sval.
      DATA lv_user_response TYPE flag.

      CLEAR rv_input.
      lt_fields = VALUE #( ( tabname = 'ABDBG'     " Text: Input Line
                             fieldname = 'LTEXT'
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
      last_input = last_input && iv_text.
      last_len = strlen( last_input ).
      finite_size = abap_true.
    ENDMETHOD.

    METHOD block_read.
      IF finite_size EQ abap_true.
        last_input = c_lisp_eof.
        last_len = 0.
      ELSE.
        last_input = read_stream( ).
        last_len = strlen( last_input ).
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
        rv_input = c_lisp_eof.
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
        rv_char = c_lisp_eof.
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
      DATA first TYPE flag VALUE abap_true READ-ONLY.
      METHODS has_next RETURNING VALUE(rv_flag) TYPE flag.
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

      METHODS eval IMPORTING environment   TYPE REF TO lcl_lisp_environment
                             interpreter   TYPE REF TO lcl_lisp_interpreter
                   RETURNING VALUE(result) TYPE REF TO lcl_lisp_hash
                   RAISING   lcx_lisp_exception.

    PROTECTED SECTION.
      TYPES: BEGIN OF ts_hash,
               key     TYPE string,
               element TYPE REF TO lcl_lisp,
             END OF ts_hash.
      TYPES tt_hash TYPE HASHED TABLE OF ts_hash WITH UNIQUE KEY key.
      DATA hash TYPE tt_hash.

      METHODS fill IMPORTING list TYPE REF TO lcl_lisp
                   RAISING   lcx_lisp_exception.
  ENDCLASS.                    "lcl_lisp_hash DEFINITION

  CLASS lcl_lisp_vector DEFINITION INHERITING FROM lcl_lisp
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.

      CLASS-METHODS init IMPORTING size             TYPE tv_index
                                   io_fill          TYPE REF TO lcl_lisp DEFAULT nil
                                   iv_mutable       TYPE flag DEFAULT abap_true
                         RETURNING VALUE(ro_vector) TYPE REF TO lcl_lisp_vector
                         RAISING   lcx_lisp_exception.

      CLASS-METHODS from_list IMPORTING io_list          TYPE REF TO lcl_lisp
                                        iv_mutable       TYPE flag DEFAULT abap_true
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

      METHODS eval IMPORTING environment   TYPE REF TO lcl_lisp_environment
                             interpreter   TYPE REF TO lcl_lisp_interpreter
                   RETURNING VALUE(result) TYPE REF TO lcl_lisp_vector
                   RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      DATA vector TYPE tt_lisp.
      DATA mo_length TYPE REF TO lcl_lisp.
  ENDCLASS.

  CLASS lcl_lisp_bytevector DEFINITION INHERITING FROM lcl_lisp
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA bytes TYPE tt_byte READ-ONLY.

      CLASS-METHODS init IMPORTING size         TYPE tv_index
                                   iv_fill      TYPE tv_byte
                                   iv_mutable   TYPE flag DEFAULT abap_true
                         RETURNING VALUE(ro_u8) TYPE REF TO lcl_lisp_bytevector
                         RAISING   lcx_lisp_exception.

      CLASS-METHODS utf8_from_string IMPORTING iv_text      TYPE string
                                               iv_mutable   TYPE flag DEFAULT abap_true
                                     RETURNING VALUE(ro_u8) TYPE REF TO lcl_lisp_bytevector
                                     RAISING   lcx_lisp_exception.

      CLASS-METHODS from_list IMPORTING io_list      TYPE REF TO lcl_lisp
                                        iv_mutable   TYPE flag DEFAULT abap_true
                              RETURNING VALUE(ro_u8) TYPE REF TO lcl_lisp_bytevector
                              RAISING   lcx_lisp_exception.

      METHODS utf8_to_string IMPORTING from           TYPE tv_index DEFAULT 0
                                       to             TYPE tv_index OPTIONAL
                             RETURNING VALUE(rv_text) TYPE string
                             RAISING   lcx_lisp_exception.

      METHODS set IMPORTING index         TYPE tv_index
                            iv_byte       TYPE tv_byte
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

      DATA parameters_generated TYPE flag.

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
      DATA mv_hold_cursor TYPE flag.
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
      DATA top_level TYPE flag VALUE abap_false READ-ONLY.

      METHODS:
        scope_of IMPORTING symbol     TYPE any
                 RETURNING VALUE(env) TYPE REF TO lcl_lisp_environment
                 RAISING   lcx_lisp_exception,
        get IMPORTING symbol      TYPE any
            RETURNING VALUE(cell) TYPE REF TO lcl_lisp
            RAISING   lcx_lisp_exception,
        set IMPORTING symbol  TYPE string
                      element TYPE REF TO lcl_lisp
                      once    TYPE flag DEFAULT abap_false
            RAISING   lcx_lisp_exception,
*       Convenience method to add a value and create the cell
        define_value IMPORTING symbol         TYPE string
                               type           TYPE tv_type
                               value          TYPE any OPTIONAL
                     RETURNING VALUE(element) TYPE REF TO lcl_lisp.

      METHODS parameters_to_symbols IMPORTING io_pars TYPE REF TO lcl_lisp
                                              io_args TYPE REF TO lcl_lisp
                                    RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
*     Reference to
      DATA outer TYPE REF TO lcl_lisp_environment.  " outer (parent) environment:

      TYPES: BEGIN OF ts_map,
               symbol TYPE string,
               value  TYPE REF TO lcl_lisp,
             END OF ts_map.
      TYPES tt_map TYPE HASHED TABLE OF ts_map WITH UNIQUE KEY symbol.

      DATA map TYPE tt_map.

      METHODS unbound_symbol IMPORTING symbol TYPE any
                             RAISING   lcx_lisp_exception.
      METHODS prepare.
  ENDCLASS.                    "lcl_lisp_environment DEFINITION

  TYPES: BEGIN OF ts_continuation,
           elem TYPE REF TO lcl_lisp,
           env TYPE REF TO lcl_lisp_environment,
         END OF ts_continuation.

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
      env->outer = io_outer.
    ENDMETHOD.

    METHOD make_top_level.
      env = clone( io_outer ).
      env->prepare( ).
      env->top_level = abap_true.
    ENDMETHOD.

  ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_parser DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_parser DEFINITION.
    PUBLIC SECTION.
      TYPES tt_element TYPE STANDARD TABLE OF REF TO lcl_lisp WITH DEFAULT KEY.
      TYPES tv_char2 TYPE c LENGTH 2.
      CONSTANTS:
        c_lisp_dot    TYPE char1 VALUE '.',
        c_open_paren  TYPE char1 VALUE '(',
        c_close_paren TYPE char1 VALUE ')',
        c_lisp_equal  TYPE char1 VALUE '=',
        c_lisp_xx     TYPE tv_char2 VALUE 'xX'.
      CONSTANTS:
        c_escape_char    TYPE char1 VALUE '\',
        c_text_quote     TYPE char1 VALUE '"',
        c_lisp_quote     TYPE char1 VALUE '''', "LISP single quote = QUOTE
        c_lisp_backquote TYPE char1 VALUE '`',  " backquote = quasiquote
        c_lisp_unquote   TYPE char1 VALUE ',',
        c_lisp_splicing  TYPE char1 VALUE '@',
        c_lisp_hash      TYPE char1 VALUE '#',
        c_lisp_comment   TYPE char1 VALUE ';',
        c_block_comment  TYPE char1 VALUE '|', " start #|, end |#
        c_open_curly     TYPE char1 VALUE '{',
        c_close_curly    TYPE char1 VALUE '}',
        c_open_bracket   TYPE char1 VALUE '[',
        c_close_bracket  TYPE char1 VALUE ']'.
      CONSTANTS:
        c_number_exact   TYPE tv_char2 VALUE 'eE',
        c_number_inexact TYPE tv_char2 VALUE 'iI',
        c_number_octal   TYPE tv_char2 VALUE 'oO',
        c_number_binary  TYPE tv_char2 VALUE 'bB',
        c_number_decimal TYPE tv_char2 VALUE 'dD'.

      METHODS:
        constructor,
        parse IMPORTING iv_code         TYPE clike
              RETURNING VALUE(elements) TYPE tt_element
              RAISING   lcx_lisp_exception,
        read_from IMPORTING ii_port        TYPE REF TO lif_input_port

                  RETURNING VALUE(element) TYPE REF TO lcl_lisp
                  RAISING   lcx_lisp_exception.

    PRIVATE SECTION.
      TYPES tv_text13 TYPE c LENGTH 13.
      DATA code TYPE string.
      DATA length TYPE i.
      DATA index TYPE i.
      DATA char TYPE char1.

      DATA mv_eol TYPE char1.
      DATA mv_whitespace TYPE char07. " Case sensitive
      DATA mv_delimiters TYPE tv_text13. " Case sensitive

      METHODS:
        next_char RAISING lcx_lisp_exception,
        peek_char RETURNING VALUE(rv_char) TYPE char1,
        peek_bytevector RETURNING VALUE(rv_flag) TYPE flag,
        match_label IMPORTING iv_limit        TYPE char1
                    EXPORTING ev_label        TYPE string
                    RETURNING VALUE(rv_found) TYPE flag,
        skip_label,
        skip_whitespace
          RETURNING VALUE(rv_has_next) TYPE flag
          RAISING   lcx_lisp_exception,
        parse_list IMPORTING delim         TYPE char01 DEFAULT c_open_paren
                   RETURNING VALUE(result) TYPE REF TO lcl_lisp
                   RAISING   lcx_lisp_exception,
        parse_token RETURNING VALUE(element) TYPE REF TO lcl_lisp
                    RAISING   lcx_lisp_exception.
      METHODS match_string CHANGING cv_val TYPE string
                           RAISING  lcx_lisp_exception.
      METHODS match_atom CHANGING cv_val TYPE string.

      METHODS throw IMPORTING message TYPE string
                    RAISING   lcx_lisp_exception.
      METHODS skip_one_datum RAISING lcx_lisp_exception.
      METHODS skip_block_comment RAISING lcx_lisp_exception.

  ENDCLASS.                    "lcl_parser DEFINITION

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

*     Methods for evaluation
      METHODS:
        eval
          IMPORTING is_cont TYPE ts_continuation
          RETURNING VALUE(result) TYPE REF TO lcl_lisp
          RAISING   lcx_lisp_exception,
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

      proc_is_string       ##called,
      proc_is_char         ##called,
      proc_is_symbol       ##called,
      proc_is_hash         ##called,
      proc_is_procedure    ##called,
      proc_is_list         ##called,
      proc_is_pair         ##called,
      proc_is_boolean      ##called,
      proc_boolean_list_is_equal ##called,
      proc_is_vector             ##called,
      proc_is_alist              ##called,

      proc_is_exact              ##called,
      proc_is_inexact            ##called,
      proc_to_exact              ##called,
      proc_to_inexact            ##called,

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
      proc_write_string      ##called,
      proc_read_char         ##called,
      proc_read_string       ##called,
      proc_peek_char         ##called,
      proc_is_char_ready     ##called,

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
      proc_call_cc,              ##called
* Exceptions
      proc_error,                ##called
      proc_raise,                ##called
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
        RETURNING VALUE(result) TYPE flag
        RAISING   lcx_lisp_exception.

      METHODS syntax_expand
        IMPORTING cont TYPE ts_continuation
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

      METHODS read_string IMPORTING io_arg        TYPE REF TO lcl_lisp
                          RETURNING VALUE(result) TYPE REF TO lcl_lisp
                          RAISING   lcx_lisp_exception.
    PRIVATE SECTION.
      TYPES: BEGIN OF ts_digit,
               zero  TYPE x LENGTH 3,
               langu TYPE string,
             END OF ts_digit.
      TYPES tt_digit TYPE SORTED TABLE OF ts_digit WITH UNIQUE KEY zero.
      DATA mt_zero TYPE tt_digit.

      METHODS string_to_number IMPORTING iv_text       TYPE string
                                         iv_radix      TYPE i DEFAULT 10
                               RETURNING VALUE(result) TYPE REF TO lcl_lisp
                               RAISING   lcx_lisp_exception.

      METHODS unicode_digit_zero RETURNING VALUE(rt_zero) TYPE tt_digit.

      METHODS unicode_to_digit IMPORTING iv_char         TYPE char01
                               RETURNING VALUE(rv_digit) TYPE i.

      METHODS char_to_integer IMPORTING io_char       TYPE REF TO lcl_lisp
                              RETURNING VALUE(rv_int) TYPE tv_int
                              RAISING   lcx_lisp_exception.

      METHODS fold_case IMPORTING element          TYPE REF TO lcl_lisp
                        RETURNING VALUE(rv_string) TYPE string.

      METHODS char_fold_case_to_integer IMPORTING element       TYPE REF TO lcl_lisp
                                        RETURNING VALUE(rv_int) TYPE tv_int.

      METHODS char_case_identity IMPORTING element          TYPE REF TO lcl_lisp
                                 RETURNING VALUE(rv_string) TYPE string.

      METHODS throw IMPORTING message TYPE string
                    RAISING   lcx_lisp_exception.

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

      METHODS eval_list_tco IMPORTING VALUE(io_head) TYPE REF TO lcl_lisp
                                      io_environment TYPE REF TO lcl_lisp_environment
                            EXPORTING eo_elem        TYPE REF TO lcl_lisp
                            RETURNING VALUE(result)  TYPE REF TO lcl_lisp
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
                       RAISING   lcx_lisp_exception.

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
                            EXPORTING ev_has_next   TYPE flag
                            CHANGING  ct_list       TYPE tt_lisp
                            RETURNING VALUE(result) TYPE REF TO lcl_lisp
                            RAISING   lcx_lisp_exception.

      METHODS is_constant IMPORTING exp            TYPE REF TO lcl_lisp
                          RETURNING VALUE(rv_flag) TYPE flag.
      METHODS combine IMPORTING left          TYPE REF TO lcl_lisp
                                right         TYPE REF TO lcl_lisp
                                exp           TYPE REF TO lcl_lisp
                                environment   TYPE REF TO lcl_lisp_environment
                      RETURNING VALUE(result) TYPE REF TO lcl_lisp.

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

    METHOD constructor.
*     End of line value
      mv_eol = cl_abap_char_utilities=>newline.
*     Whitespace values
      CLEAR mv_whitespace.
      mv_whitespace+0(1) = ' '.
      mv_whitespace+1(1) = cl_abap_char_utilities=>newline.        " \n
      mv_whitespace+2(1) = cl_abap_char_utilities=>cr_lf(1).       " \r
      mv_whitespace+3(1) = cl_abap_char_utilities=>cr_lf(2).       " \n
      mv_whitespace+4(1) = cl_abap_char_utilities=>horizontal_tab. " \t
      mv_whitespace+5(1) = cl_abap_char_utilities=>vertical_tab.
      mv_whitespace+6(1) = cl_abap_char_utilities=>form_feed.

*     Delimiters value
      mv_delimiters = mv_whitespace.
      mv_delimiters+7(1) = c_close_paren.
      mv_delimiters+8(1) = c_open_paren.
      mv_delimiters+9(1) = c_close_bracket.
      mv_delimiters+10(1) = c_open_bracket.
      mv_delimiters+11(1) = c_close_curly.
      mv_delimiters+12(1) = c_open_curly.
    ENDMETHOD.                    "constructor

    METHOD skip_whitespace.
      WHILE char CA mv_whitespace AND index LT length.
        next_char( ).
      ENDWHILE.
      rv_has_next = xsdbool( index LT length ).

      IF char EQ c_lisp_comment AND rv_has_next EQ abap_true.
*       skip until end of line
        WHILE char CN mv_eol AND index LT length.
          next_char( ).
        ENDWHILE.
        rv_has_next = skip_whitespace( ).
      ENDIF.

      IF char EQ c_lisp_hash AND rv_has_next EQ abap_true AND peek_char( ) EQ c_lisp_comment.
        skip_one_datum( ).
        rv_has_next = skip_whitespace( ).
      ENDIF.

      IF char EQ c_lisp_hash AND rv_has_next EQ abap_true AND peek_char( ) EQ c_block_comment.
        skip_block_comment( ).
        rv_has_next = skip_whitespace( ).
      ENDIF.

    ENDMETHOD.

    METHOD skip_block_comment.
*     skip block comment, from #| to |#
      next_char( ).   " skip |#
      next_char( ).
      WHILE index LT length AND NOT ( char EQ c_block_comment AND peek_char( ) EQ c_lisp_hash ).
        next_char( ).
      ENDWHILE.

      IF char EQ c_block_comment AND peek_char( ) EQ c_lisp_hash.
        next_char( ).  " skip #|
        next_char( ).
      ENDIF.
    ENDMETHOD.

    METHOD skip_one_datum.
*     Character constant  #; comment
      next_char( ).       " skip #;
      next_char( ).
*     skip optional blanks, max. until end of line
      WHILE char CN mv_eol AND index LT length AND peek_char( ) EQ ` `.
        next_char( ).
      ENDWHILE.
*     skip one datum
      DATA sval TYPE string.
      match_atom( CHANGING cv_val = sval ).
    ENDMETHOD.

    METHOD next_char.
      index = index + 1.
      IF index < length.
        char = code+index(1).
      ELSEIF index = length.
        char = space.
      ELSEIF index > length.
        throw( c_error_unexpected_end ).
      ENDIF.
    ENDMETHOD.                    "next_char

    METHOD throw.
      RAISE EXCEPTION TYPE lcx_lisp_exception
        EXPORTING
          message = message
          area    = c_area_parse.
    ENDMETHOD.

    METHOD peek_char.
      DATA(lv_idx) = index + 1.

      IF lv_idx < length.
        rv_char = code+lv_idx(1).
      ELSE.
        rv_char = c_lisp_eof.
      ENDIF.
    ENDMETHOD.

    METHOD peek_bytevector.
      CONSTANTS c_prefix TYPE char03 VALUE 'u8('.
      DATA lv_token TYPE string.

      DATA(lv_idx) = index.
      rv_flag = abap_false.
      DO 3 TIMES.  " length( 'u8(' ) = 3
        lv_idx = lv_idx + 1.

        IF lv_idx < length.
          "lv_token = to_upper( lv_token ).
          CONCATENATE lv_token code+lv_idx(1) INTO lv_token RESPECTING BLANKS.
        ELSE.
          RETURN.
        ENDIF.
      ENDDO.
      CHECK lv_token EQ c_prefix.
      rv_flag = abap_true.
    ENDMETHOD.

    METHOD parse.
*     Entry point for parsing code. This is not thread-safe, but as an ABAP
*     process does not have the concept of threads, we are safe :-)
      code = iv_code.
      length = strlen( code ).

      IF length = 0.
        APPEND lcl_lisp=>eof_object TO elements.
        RETURN.
      ENDIF.

      index = 0.
      char = code+index(1).           "Kick off things by reading first char
      WHILE skip_whitespace( ).
        IF char = c_open_paren OR char = c_open_bracket OR char = c_open_curly.
          APPEND parse_list( char ) TO elements.
        ELSEIF index < length.
          APPEND parse_token( ) TO elements.
        ENDIF.
      ENDWHILE.

    ENDMETHOD.                    "parse

    METHOD read_from.
      code = ii_port->read( ).
      length = strlen( code ).
      element = lcl_lisp=>eof_object.

      CHECK code NE c_lisp_eof AND length GT 0.

      index = 0.
      char = code+index(1).           "Kick off things by reading first char
      skip_whitespace( ).
      IF char = c_open_paren OR char = c_open_bracket OR char = c_open_curly.
        element = parse_list( char ).
      ELSEIF index < length.
        element = parse_token( ).
      ENDIF.
      ii_port->put( substring( val = code off = index ) ).
    ENDMETHOD.

    METHOD parse_list.
      DATA lo_cell TYPE REF TO lcl_lisp.
      DATA lv_empty_list TYPE boole_d VALUE abap_true.
      DATA lv_proper_list TYPE boole_d VALUE abap_true.

*     Set pointer to start of list
      result = lo_cell = lcl_lisp_new=>cons( ).
      DATA(lv_close_delim) = SWITCH #( delim WHEN c_open_bracket THEN c_close_bracket
                                             WHEN c_open_curly THEN c_close_curly
                                             ELSE c_close_paren ).

      next_char( ).                 " Skip past opening paren
      WHILE skip_whitespace( ).
        CASE char.
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
            throw( |a { char } found while { lv_close_delim } expected| ).

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
    ENDMETHOD.                    "parse_list

    METHOD match_string.
      CONSTANTS:
        c_esc_a          TYPE char1 VALUE 'a',
        c_esc_b          TYPE char1 VALUE 'b',
        c_esc_t          TYPE char1 VALUE 't',
        c_esc_n          TYPE char1 VALUE 'n',
        c_esc_r          TYPE char1 VALUE 'r',
        c_esc_semi_colon TYPE char1 VALUE ';',
        c_esc_vline      TYPE char1 VALUE '|'.

      DATA pchar TYPE char1.
*     " is included in a string as \"

      next_char( ).                 " Skip past opening quote
      WHILE index < length AND NOT ( char = c_text_quote AND pchar NE c_escape_char ).
*       cv_val = |{ cv_val }{ char }|.
        pchar = char.
        next_char( ).
        IF pchar EQ c_escape_char.
          CASE char.
            WHEN space.         " \ : intraline whitespace
              skip_whitespace( ).
              CONTINUE.

            WHEN c_text_quote   " \" : double quote, U+0022
              OR c_escape_char  " \\ : backslash, U+005C
              OR c_esc_vline.   " \| : vertical line, U+007C
              pchar = char.
              next_char( ).

            WHEN c_esc_a.       " \a : alarm, U+0007
              pchar = lcl_lisp=>char_alarm->value+0(1).
              next_char( ).

            WHEN c_esc_b.       " \b : backspace, U+0008
              pchar = lcl_lisp=>char_backspace->value+0(1).
              next_char( ).

            WHEN c_esc_t.       " \t : character tabulation, U+0009
              pchar = lcl_lisp=>char_tab->value+0(1).
              next_char( ).

            WHEN c_esc_n.       " \n : linefeed, U+000A
              pchar = lcl_lisp=>char_newline->value+0(1).
              next_char( ).

            WHEN c_esc_r.       " \r : return, U+000D
              pchar = lcl_lisp=>char_return->value+0(1).
              next_char( ).

            WHEN c_lisp_xx+0(1) OR c_lisp_xx+1(1).      " hex scalar value terminated by semi-colon ;
              DATA lv_xstr TYPE string.
              DATA lo_char TYPE REF TO lcl_lisp_char.
              next_char( ).
              CLEAR lv_xstr.
              WHILE index < length.
                lv_xstr = |{ lv_xstr }{ char }|.
                next_char( ).
                CHECK char EQ c_esc_semi_colon.
                EXIT.
              ENDWHILE.
              CONDENSE lv_xstr.
              IF strlen( lv_xstr ) LE 4 AND char EQ c_esc_semi_colon.
                lo_char = lcl_lisp_new=>charx( lv_xstr ).
              ELSE.
                throw( |unknown char #\\x{ lv_xstr } found| ).
              ENDIF.

              pchar = lo_char->value+0(1).
              next_char( ).
          ENDCASE.
        ENDIF.
        CONCATENATE cv_val pchar INTO cv_val RESPECTING BLANKS.
      ENDWHILE.
      next_char( ).                 "Skip past closing quote
    ENDMETHOD.                    "match_string

    METHOD skip_label.
*     Skip if match was successful
      next_char( ).                 " Skip past hash
      WHILE index < length AND char CO c_decimal_digits.
        next_char( ).
      ENDWHILE.
      next_char( ).                 "Skip past closing iv_limit ( = or # )
    ENDMETHOD.

    METHOD match_label.
      rv_found = abap_false.
      CLEAR ev_label.

      DATA(lv_idx) = index.

      DATA(len_1) = length - 1.
      WHILE lv_idx < len_1.
        lv_idx = lv_idx + 1.

        DATA(lv_char) = code+lv_idx(1).
        IF lv_char CO c_decimal_digits.
          ev_label = ev_label && lv_char.
        ELSEIF lv_char = iv_limit AND ev_label IS NOT INITIAL.
          rv_found = abap_true.
          skip_label( ).
        ELSE.
          RETURN.
        ENDIF.
      ENDWHILE.
    ENDMETHOD.

    METHOD match_atom.              " run_to_delimiter.
      WHILE index < length.
        CONCATENATE cv_val char INTO cv_val RESPECTING BLANKS.
        next_char( ).
        CHECK char CA mv_delimiters.
        EXIT.
      ENDWHILE.
      CONDENSE cv_val.              " 29.04.2018 to be reviewed, remove?
      IF cv_val = cl_abap_char_utilities=>newline.
        cv_val = space.
      ENDIF.
    ENDMETHOD.                    "run_to_delimiter

    DEFINE _get_atom.
      next_char( ).        " skip #
      next_char( ).        " skip
      match_atom( CHANGING cv_val = &1 ).
    END-OF-DEFINITION.

    METHOD parse_token.
      DATA sval TYPE string.

      skip_whitespace( ).
*     create object cell.
      CASE char.
        WHEN c_open_paren OR c_open_bracket OR c_open_curly.
          element = parse_list( char ).
          RETURN.

        WHEN c_lisp_quote.
* ' is just a shortcut for QUOTE, so we wrap the consecutive element in a list starting with the quote symbol
* so that when it is evaluated later, it returns the quote elements unmodified
          next_char( ).            " Skip past single quote
          element = lcl_lisp_new=>quote( parse_token( ) ).
          RETURN.

        WHEN c_lisp_backquote.     " Quasiquote, TO DO
          next_char( ).            " Skip past single quote
          element = lcl_lisp_new=>quasiquote( parse_token( ) ).
          RETURN.

        WHEN c_lisp_unquote.
          IF peek_char( ) EQ c_lisp_splicing.  " unquote-splicing ,@
            next_char( ).        " Skip past ,
            next_char( ).        " Skip past @
            element = lcl_lisp_new=>splice_unquote( parse_token( ) ).  " token must evaluate to a list, not be a list
          ELSE.                                " unquote ,
            next_char( ).        " Skip past ,
            element = lcl_lisp_new=>unquote( parse_token( ) ).
          ENDIF.
          RETURN.

        WHEN c_text_quote.
          match_string( CHANGING cv_val = sval ).
          element = lcl_lisp_new=>string( value = sval
                                          iv_mutable = abap_false ).
          RETURN.

        WHEN c_lisp_hash.
          CASE peek_char( ).
            WHEN c_open_paren.   " Vector constant
              next_char( ).
              element = lcl_lisp_vector=>from_list( io_list = parse_list( )
                                                    iv_mutable = abap_false ).
              RETURN.

            WHEN c_escape_char.  " Character constant  #\a
              next_char( ).      " skip #
              next_char( ).      " skip \
              IF char EQ c_lisp_xx+0(1) OR char EQ c_lisp_xx+1(1).
                next_char( ).      " skip x
                match_atom( CHANGING cv_val = sval ).
                IF strlen( sval ) LE 4.
                  element = lcl_lisp_new=>charx( sval ).
                ELSE.
                  throw( |unknown char #\\x{ sval } found| ).
                ENDIF.
              ELSE.
                match_atom( CHANGING cv_val = sval ).
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
                    element = lcl_lisp=>char_newline.
                  WHEN 'null'.
                    element = lcl_lisp=>char_null.
                  WHEN 'return'.
                    element = lcl_lisp=>char_return.
                  WHEN 'space' OR space.
                    element = lcl_lisp=>char_space.
                  WHEN 'tab'.
                    element = lcl_lisp=>char_tab.
                  WHEN OTHERS.
                    IF strlen( sval ) EQ 1.
                      element = lcl_lisp_new=>char( sval ).
                    ELSE.
                      throw( |unknown char #\\{ sval } found| ).
                    ENDIF.
                ENDCASE.
              ENDIF.

              RETURN.

*           Notation for numbers #e (exact) #i (inexact) #b (binary) #o (octal) #d (decimal) #x (hexadecimal)
*           further, instead of exp:  s (short), f (single), d (double), l (long)
*           positive infinity, negative infinity -inf / -inf.0, NaN +nan.0, positive zero, negative zero
            WHEN c_number_exact+0(1) OR c_number_exact+1(1).   "#e (exact)
              DATA lx_error TYPE REF TO cx_root.
              _get_atom sval.
              TRY.
                  element = lcl_lisp_new=>number( value = sval
                                                  iv_exact = abap_true ).
                  RETURN.
                CATCH cx_sy_conversion_no_number INTO lx_error.
                  throw( lx_error->get_text( ) ).
              ENDTRY.

            WHEN c_number_inexact+0(1) OR c_number_inexact+1(1). "#i (inexact)
              _get_atom sval.
              element = lcl_lisp_new=>number( value = sval
                                              iv_exact = abap_false ).
              RETURN.

            WHEN c_number_octal+0(1) OR c_number_octal+1(1).     "#o (octal)
              _get_atom sval.
              element = lcl_lisp_new=>octal( sval ).
              RETURN.

            WHEN c_number_binary+0(1) OR c_number_binary+1(1).  "#b (binary)
              _get_atom sval.
              element = lcl_lisp_new=>binary( sval ).
              RETURN.

            WHEN c_number_decimal+0(1) OR c_number_decimal+1(1). "#d (decimal)
              _get_atom sval.
              element = lcl_lisp_new=>number( sval ).
              RETURN.

            WHEN c_lisp_xx+0(1) OR c_lisp_xx+1(1).                "#x (hexadecimal)
              _get_atom sval.
              element = lcl_lisp_new=>hex( sval ).
              RETURN.

            WHEN OTHERS.
*             Boolean #t #f
              "will be handled in match_atom( )

              IF peek_bytevector( ).
                " Bytevector constant #u8( ... )
                next_char( ).      " skip #
                next_char( ).      " skip U
                next_char( ).      " skip 8
                element = lcl_lisp_bytevector=>from_list( io_list = parse_list( )
                                                          iv_mutable = abap_false ).
                RETURN.
              ENDIF.

*             Referencing other literal data #<n>= #<n>#
              DATA lv_label TYPE string.
              IF match_label( EXPORTING iv_limit = c_lisp_equal
                              IMPORTING ev_label = lv_label ).
*               New datum
*               Now Add: (set! |#{ lv_label }#| element)
                element = parse_token( ).
                element->mv_label = |#{ lv_label }#|.

                RETURN.
              ELSEIF match_label( EXPORTING iv_limit = c_lisp_hash
                                  IMPORTING ev_label = lv_label ).
*               Reference to datum
                element = lcl_lisp_new=>symbol( |#{ lv_label }#| ).
                RETURN.
              ENDIF.

          ENDCASE.

      ENDCASE.
*     Others
      match_atom( CHANGING cv_val = sval ).
      element = lcl_lisp_new=>atom( sval ).
    ENDMETHOD.                    "parse_token

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

    METHOD constructor.
      super->constructor( ).
      nil = lcl_lisp=>nil.
      true = lcl_lisp=>true.
      false = lcl_lisp=>false.

      go_input_port ?= proc_make_parameter( lcl_lisp_new=>cons( io_car = io_port ) ).
      go_output_port = go_error_port = go_input_port.
      gi_log = ii_log.
      mt_zero = unicode_digit_zero( ).

      env = lcl_lisp_env_factory=>make_top_level( ).
    ENDMETHOD.                    "constructor

    METHOD throw.
      lcl_lisp=>throw( message ).
    ENDMETHOD.                    "throw

    METHOD bind_symbol.
      DATA lv_symbol TYPE string.
      DATA lo_params TYPE REF TO lcl_lisp.
      " Scheme does not return a value for define; but we are returning the new symbol reference
      DATA(lo_head) = element->car.
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
    ENDMETHOD.                    "assign_symbol

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
          CHECK lo_ptr->is_procedure( ) EQ true.
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

    METHOD assign_symbol.
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
    ENDMETHOD.                    "re_assign_symbol

    METHOD evaluate_parameters.
*     This routine is called very, very often!
      DATA lo_arg TYPE REF TO lcl_lisp.
      DATA lo_new TYPE REF TO lcl_lisp.
      DATA ctx TYPE ts_continuation.
*     Before execution of the procedure or lambda, all parameters must be evaluated
      _validate io_list.

      ro_head = nil.
      CHECK io_list NE nil. " AND io_list->car NE nil.

      ctx-env = environment.
      DATA(elem) = io_list.
*     TO DO: check if circular list are allowed
      WHILE elem->type EQ type_pair.
        ctx-elem = elem->car.
        lo_new = lcl_lisp_new=>cons( io_car = eval_ast( ctx ) ).
        IF ro_head = nil.
          lo_arg = ro_head = lo_new.
        ELSE.
          lo_arg = lo_arg->cdr = lo_new.
        ENDIF.
        elem = elem->cdr.
      ENDWHILE.

      _validate_tail elem io_list space.

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
*     TO DO: check if circular lists are allowed
      WHILE lo_arg->cdr->type EQ type_pair.
*       At least two entries (argn and rest), build (list arg1 . . argn )

        lo_new = lo_new->cdr = lcl_lisp_new=>cons( io_car = lo_arg->car ).
        lo_arg = lo_arg->cdr.
      ENDWHILE.

*     now (append (list arg1 . . argn ) rest )
      DATA(lo_rest) = eval_ast(  VALUE #( elem = lo_arg->car
                                          env = environment ) ).

*     TO DO: check if circular lists are allowed
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
      _validate io_head.
      result = nil.
      eo_elem = io_head.

      CHECK io_head NE nil.

      WHILE eo_elem IS BOUND AND eo_elem->type EQ type_pair
        AND eo_elem->cdr NE nil.  " Do not evaluate the last list element

        DATA(ls_cont) = VALUE ts_continuation( elem = eo_elem->car
                                               env = io_environment ).
        result = eval_ast( ls_cont ).
        eo_elem = eo_elem->cdr.
      ENDWHILE.

      _validate_tail eo_elem->cdr io_head space.
    ENDMETHOD.

*    METHOD eval_list.
*      _validate io_head.
*      result = nil.
*
*      DATA(elem) = io_head.
*      WHILE elem IS BOUND AND elem->type EQ type_pair.
*        result = eval_ast( element = elem->car
*                           environment = io_environment ).
*        elem = elem->cdr.
*      ENDWHILE.
*
*      _validate_tail elem io_head space.
*    ENDMETHOD.

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
      DATA lv_parameter_object TYPE flag VALUE abap_false.

      DEFINE _to_param_object.
        &2 = eval( VALUE #( elem = &1
                            env = io_env ) ).
        IF &2->type EQ type_lambda.
          lo_lambda ?= &2.
          lv_parameter_object = lo_lambda->parameter_object.
        ENDIF.
        IF lv_parameter_object EQ abap_false.
          throw( |missing parameter object in parameterize| ).
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
          result = CAST lcl_lisp_hash( cont-elem )->eval( environment = cont-env
                                                          interpreter = me ).

        WHEN type_vector. " TEST
          result = CAST lcl_lisp_vector( cont-elem )->eval( environment = cont-env
                                                            interpreter = me ).

        WHEN OTHERS.
*         otherwise just return the original AST value
          result = cont-elem.  "Number or string evaluates to itself (also: vector constant)

      ENDCASE.
      _assert_is_bound result c_error_eval.

    ENDMETHOD.

    DEFINE _tail_expression.
      IF &1 NE nil.
        cont-elem = &1->car.    " Tail context
        CONTINUE.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE _tail_sequence.
      IF cont-elem NE nil.
*       result = eval_list( io_head = lo_elem
*                           io_environment = lo_env ).
        result = eval_list_tco( EXPORTING io_head = cont-elem
                                          io_environment = cont-env
                                IMPORTING eo_elem = cont-elem ).
        _tail_expression cont-elem.
      ELSEIF cont-env->top_level EQ abap_false.
        throw( c_error_no_exp_in_body ).
      ELSE.  " empty (begin) ?
        CONTINUE.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE _validate_quote.
      IF NOT ( &1->cdr->type = type_pair AND &1->cdr->cdr = nil ).
        throw( |invalid form { &1->car->to_string( ) } in { &2 }| ).
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
          result = lcl_lisp_new=>quote( eval_left ).
          result->cdr->cdr = eval_right.
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
*          	     (eq? (caar exp) 'unquote-splicing)
*          	     (= (length (car exp)) 2))
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
*                   Derived expression: Conditional (and <expression>* >tail expression>)
                    result = true.
                    DATA(lo_ptr) = lr_tail.
                    WHILE result NE false AND lo_ptr IS BOUND AND lo_ptr NE nil AND lo_ptr->cdr NE nil.
                      result = eval_ast( VALUE #( elem = lo_ptr->car
                                                  env = cont-env ) ).
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
                      result = eval_ast( VALUE #( elem = lo_ptr->car
                                                  env = cont-env ) ).
                      lo_ptr = lo_ptr->cdr.
                    ENDWHILE.
                    IF result EQ false.
                      _tail_expression lo_ptr.
                    ENDIF.

                  WHEN 'cond'.
*                   Derived expression: Conditional
                    lo_ptr = lr_tail.
                    cont-elem = nil.
                    WHILE lo_ptr->type EQ type_pair.
                      DATA(lo_clause) = lo_ptr->car.
                      IF lo_clause->car->value EQ c_lisp_else.
                        cont-elem = lo_clause->cdr.
                        EXIT.
                      ENDIF.
                      DATA(lo_test) = eval_ast( VALUE #( elem = lo_clause->car
                                                         env = cont-env ) ).
                      IF lo_test NE false.
                        cont-elem = lo_clause->cdr.
                        EXIT.
                      ENDIF.
                      lo_ptr = lo_ptr->cdr.
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

                  WHEN 'set!'.                        " Re-Assign symbol
                    result = assign_symbol( element     = lr_tail
                                            environment = cont-env ).

                  WHEN 'if'.
                    " _validate lr_tail->cdr. "I do not have a test case yet where it fails here
                    IF eval( VALUE #( elem = lr_tail->car
                                      env = cont-env ) ) NE false.

                      cont-elem = lr_tail->cdr->car. " Tail context
                      CONTINUE.

                    ELSEIF lr_tail->cdr->cdr = nil.
                      result = lcl_lisp=>undefined.
                    ELSE.
                      _validate lr_tail->cdr->cdr.
                      cont-elem = lr_tail->cdr->cdr->car. " Tail context
                      CONTINUE.

                    ENDIF.

                  WHEN 'begin'.
                    cont-elem = lr_tail.
*                    _tail_sequence.
                    IF cont-elem NE nil.
                      result = eval_list_tco( EXPORTING io_head = cont-elem
                                                        io_environment = cont-env
                                              IMPORTING eo_elem = cont-elem ).
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

                  WHEN 'unless'.
                    result = nil.
                    IF eval( VALUE #( elem = lr_tail->car
                                      env = cont-env ) ) EQ false.
                      "  _validate lr_tail->cdr. "I do not have a test case yet where it fails here
                      cont-elem = lr_tail->cdr.
                      _tail_sequence.
                    ENDIF.

                  WHEN 'when'.
                    result = nil.
                    IF eval( VALUE #( elem = lr_tail->car
                                      env = cont-env ) ) NE false.
                      "  _validate lr_tail->cdr. "I do not have a test case yet where it fails here
                      cont-elem = lr_tail->cdr.
                      _tail_sequence.
                    ENDIF.

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
                      CASE eval_ast( VALUE #( elem = lo_test->car
                                              env = cont-env ) ).
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

                    DATA(lo_key) = eval( VALUE #( elem = lr_tail->car
                                                  env = cont-env ) ).
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
                        IF lo_key->is_equivalent( lo_datum->car ) NE false.
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
                    result = syntax_expand( VALUE #( elem = lr_tail->car
                                                     env = cont-env ) ).

                  WHEN 'gensym'.
                    result = generate_symbol( lr_tail ).

                  WHEN c_eval_unquote
                    OR c_eval_unquote_splicing.
                    throw( |{ lr_head->value } not valid outside of quasiquote| ).

*                  WHEN 'error'.

                  WHEN OTHERS.

*                   EXECUTE PROCEDURE (native or lambda)
*                   Take the first item of the evaluated list and call it as function
*                   using the rest of the evaluated list as its arguments.

*                   The evaluated head must be a native procedure or a lambda or an ABAP function module
                    DATA(lo_proc) = eval_ast( VALUE #( elem = lr_head
                                                       env = cont-env ) ).
                    _trace_call lo_proc lr_tail.

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

                      WHEN OTHERS.
                        throw( |attempt to apply { lo_proc->to_string( ) } - not a procedure| ).

                    ENDCASE.

                ENDCASE.

              WHEN type_values.
                DATA lo_values TYPE REF TO lcl_lisp_values.

                lo_values ?= cont-elem->car.
                cont-elem = lo_values->head.
                _tail_sequence.

              WHEN OTHERS.
                result = eval_ast( cont ).

            ENDCASE.

        ENDCASE.
*       Circular references
        result->set_shared_structure( ).
        _trace_result result.
        RETURN.

      ENDDO.
    ENDMETHOD.

    DEFINE _optional_port.
      DATA li_port TYPE REF TO lif_&2_port.

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

    DEFINE _optional_port_arg.
      _optional_port io_arg &1 &2.
    END-OF-DEFINITION.

    METHOD write.
      _optional_port_arg output `write`.
      li_port->write( io_elem ).
      result = io_elem.
    ENDMETHOD.

    METHOD display.
      _optional_port_arg output `display`.
      li_port->display( io_elem ).
      result = io_elem.
    ENDMETHOD.

    METHOD read.
      _optional_port_arg input `read`.
      result = read_from( li_port ).
    ENDMETHOD.

    METHOD read_char.
      _optional_port_arg input `read-char`.

      result = lcl_lisp_new=>char( li_port->read_char( ) ).
    ENDMETHOD.

    METHOD read_string.
      DATA k TYPE tv_index.
      DATA lv_input TYPE string.
      DATA lv_char TYPE char01.
      DATA li_port TYPE REF TO lif_input_port.

      _validate io_arg.
      _validate_integer io_arg->car `read-string`.
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

    METHOD eval_source.
      TRY.
          response = eval_repl( code ).
        CATCH cx_root INTO DATA(lx_root).
          response = lx_root->get_text( ).
      ENDTRY.
    ENDMETHOD.                    "eval_source

    METHOD eval_repl.
      LOOP AT parse( code ) INTO DATA(lo_element).
        DATA(lo_result) = eval( VALUE #( elem = lo_element
                                         env = env ) ).
        gi_log->put( lo_result ).
      ENDLOOP.
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

*         TO DO: Test for circular list! ------------------------------
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

        _validate_tail lo_arg first `append`.

        first = lo_arg = lo_iter->next( ).
        CHECK first NE nil.

*       TO DO: Check for circular list
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

*     TO DO: check if circular lists are allowed
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
          _error_no_list list `append!`.
        ENDIF.

        WHILE lo_last->cdr IS BOUND AND lo_last->cdr NE nil.
          lo_last = lo_last->cdr.
        ENDWHILE.

        "TO DO - replace with _validate_tail lo_last (?) list->car.
        IF lo_last->type NE type_pair.
*         If the last item is not a cons cell, return an error
          _error_no_list list->car  `append!`.
        ENDIF.

*       Last item is a cons cell; tack on the new value
        lo_last->cdr = list->cdr->car.
        result = list->car.
      ENDIF.
    ENDMETHOD.                    "proc_append_unsafe

    DEFINE _validate_pair.
      IF &1->type NE type_pair.
        &1->error_not_a_pair( &2 ).
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

      IF list->cdr->cdr NE nil.
        throw( `cons: only 2 arguments allowed` ).
      ENDIF.

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
      _error_no_list list `list-length`.
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

      _validate: list, list->cdr.

      result = list.
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
      _assert_last_param list.

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
      DATA lv_mutable TYPE flag.
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
      _validate list.
      _is_type bytevector.
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

* (memq obj list)  return the first sublist of
* list whose car is obj,  where  the  sublists  of list are the non-empty lists
* returned by (list-tail list  k) for k less than the length of list.
* If obj does not occur in list, then #f (not the empty list) is returned.
* Memq uses eq? to compare obj with the elements  of list
    METHOD proc_memq.
      _validate: list, list->car, list->cdr.

      result = false.

      CHECK list->cdr NE nil.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_item) = list->car.
      WHILE lo_sublist NE nil AND lo_sublist->car->type EQ lo_item->type.

        CASE lo_item->type.
          WHEN type_integer.
            IF CAST lcl_lisp_integer( lo_item )->int = CAST lcl_lisp_integer( lo_sublist->car )->int.
              result = lo_sublist.
              RETURN.
            ENDIF.

          WHEN type_real.
            IF CAST lcl_lisp_real( lo_item )->float_eq( CAST lcl_lisp_real( lo_sublist->car )->real ).
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
        IF lo_sublist->car->is_equivalent( lo_item ) NE false.
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
                             environment = env ) NE false.
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
                             environment = env ) NE false.
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
            WHEN type_integer.
              IF CAST lcl_lisp_integer( lo_key )->int = CAST lcl_lisp_integer( lo_pair->car )->int.
                result = lo_pair.
                RETURN.
              ENDIF.

            WHEN type_rational.
              DATA(lo_key_rat) = CAST lcl_lisp_rational( lo_key ).
              DATA(lo_target_rat) = CAST lcl_lisp_rational( lo_pair->car ).
              IF lo_key_rat->int = lo_target_rat->int AND lo_key_rat->denominator = lo_target_rat->denominator.
                result = lo_pair.
                RETURN.
              ENDIF.

            WHEN type_real.
              IF CAST lcl_lisp_real( lo_key )->float_eq( CAST lcl_lisp_real( lo_pair->car )->real ).
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
        IF lo_pair->car->is_equivalent( lo_key ) NE false.
          result = lo_pair.
          RETURN.
        ENDIF.
        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
    ENDMETHOD.

**********************************************************************
    DEFINE _cell_arith_definition.
      DATA res  TYPE ts_result.
      DATA lv_gcd TYPE tv_int.
      _data_local_numeric_cell.
    END-OF-DEFINITION.

    DEFINE _cell_arith.
      _values_get_next cell.
      CASE cell->type.
        WHEN type_integer.
          lo_int ?= cell.

          CASE res-type.
            WHEN type_real.
              res-real = res-real &1 lo_int->int.

            WHEN type_rational.
              res-nummer = res-nummer &1 ( lo_int->int * res-denom ).
              lv_gcd = lcl_lisp_rational=>gcd(  n = res-nummer
                                                d = res-denom ).
              res-nummer = res-nummer DIV lv_gcd.
              res-denom = res-denom DIV lv_gcd.

            WHEN type_integer.
              res-int = res-int &1 lo_int->int.

            WHEN OTHERS.
              res-type = type_integer.
              res-int = lo_int->int.
          ENDCASE.

        WHEN type_real.
          lo_real ?= cell.

          CASE res-type.
            WHEN type_real.
              res-real = res-real &1 lo_real->real.

            WHEN type_rational.
              res-real = res-nummer / res-denom &1 lo_real->real.
              res-type = type_real.

            WHEN type_integer.
              res-real = res-int &1 lo_real->real.
              res-type = type_real.

            WHEN OTHERS.
              res-type = type_real.
              res-real = lo_real->real.
          ENDCASE.

        WHEN type_rational.
          lo_rat ?= cell.

          CASE res-type.
            WHEN type_real.
              res-real = res-real &1 lo_rat->int / lo_rat->denominator.
              res-type = type_real.

            WHEN type_rational.
              res-nummer = res-nummer * lo_rat->denominator &1 ( lo_rat->int * res-denom ).
              res-denom = res-denom * lo_rat->denominator.
              lv_gcd = lcl_lisp_rational=>gcd(  n = res-nummer
                                                d = res-denom ).
              res-nummer = res-nummer DIV lv_gcd.
              res-denom = res-denom DIV lv_gcd.


            WHEN type_integer.
              res-nummer = ( res-int * lo_rat->denominator ) &1 lo_rat->int.
              res-denom = lo_rat->denominator.

              lv_gcd = lcl_lisp_rational=>gcd(  n = res-nummer
                                                d = res-denom ).
              res-nummer = res-nummer DIV lv_gcd.
              res-denom = res-denom DIV lv_gcd.
              res-type = type_rational.

            WHEN OTHERS.
              res-type = type_rational.
              res-nummer = lo_rat->int.
              res-denom = lo_rat->denominator.
          ENDCASE.

*        WHEN type_complex.

        WHEN OTHERS.
          cell->raise_nan( &2 ).
      ENDCASE.
    END-OF-DEFINITION.

    METHOD proc_add.
      _cell_arith_definition.

      _validate list.
      DATA(iter) = list->new_iterator( ).
      res = VALUE #( type = type_integer
                     denom = 1
                     exact = abap_false
                     operation = '+' ).
      WHILE iter->has_next( ).
        cell = iter->next( ).
        _cell_arith + `+`.
      ENDWHILE.

      result = lcl_lisp_new=>numeric( res ).
    ENDMETHOD.                    "proc_add

    METHOD proc_multiply.
      _cell_arith_definition.

      _validate list.
      DATA(iter) = list->new_iterator( ).
      res = VALUE #( type = type_integer
                     int = 1
                     denom = 1
                     exact = abap_false
                     operation = '*' ).

      WHILE iter->has_next( ).
        cell = iter->next( ).

        _values_get_next cell.
        CASE cell->type.
          WHEN type_integer.
            lo_int ?= cell.

            CASE res-type.
              WHEN type_real.
                res-real = res-real * lo_int->int.

              WHEN type_rational.
                res-nummer = res-nummer * lo_int->int.
                lv_gcd = lcl_lisp_rational=>gcd(  n = res-nummer
                                                  d = res-denom ).
                res-nummer = res-nummer DIV lv_gcd.
                res-denom = res-denom DIV lv_gcd.

              WHEN type_integer.
                res-int = res-int * lo_int->int.

              WHEN OTHERS.
                res-type = type_integer.
                res-int = lo_int->int.
            ENDCASE.

          WHEN type_real.
            lo_real ?= cell.

            CASE res-type.
              WHEN type_real.
                res-real = res-real * lo_real->real.

              WHEN type_rational.
                res-real = res-nummer / res-denom * lo_real->real.
                res-type = type_real.

              WHEN type_integer.
                res-real = res-int * lo_real->real.
                res-type = type_real.

              WHEN OTHERS.
                res-type = type_real.
                res-real = lo_real->real.
            ENDCASE.

          WHEN type_rational.
            lo_rat ?= cell.

            CASE res-type.
              WHEN type_real.
                res-real = res-real * lo_rat->int / lo_rat->denominator.
                res-type = type_real.

              WHEN type_rational.
                res-nummer = res-nummer * lo_rat->int.
                res-denom = res-denom * lo_rat->denominator.
                lv_gcd = lcl_lisp_rational=>gcd(  n = res-nummer
                                                  d = res-denom ).
                res-nummer = res-nummer DIV lv_gcd.
                res-denom = res-denom DIV lv_gcd.

              WHEN type_integer.
                res-nummer = res-int * lo_rat->int.
                res-denom = lo_rat->denominator.

                lv_gcd = lcl_lisp_rational=>gcd(  n = res-nummer
                                                  d = res-denom ).
                res-nummer = res-nummer DIV lv_gcd.
                res-denom = res-denom DIV lv_gcd.
                res-type = type_rational.

              WHEN OTHERS.
                res-type = type_rational.
                res-nummer = lo_rat->int.
                res-denom = lo_rat->denominator.
            ENDCASE.

*          WHEN type_complex.
          WHEN OTHERS.
            cell->raise_nan( |*| ).
        ENDCASE.

      ENDWHILE.

      result = lcl_lisp_new=>numeric( res ).
    ENDMETHOD.                    "proc_multiply

    METHOD proc_subtract.
      _cell_arith_definition.
      _validate list.

      DATA(iter) = list->new_iterator( ).
      IF iter->has_next( ) EQ abap_false.
        throw( |no number in [-]| ).
      ENDIF.

      cell = iter->next( ).
      res = VALUE #( denom = 1
                     exact = abap_false
                     operation = '-' ).

      _cell_arith - `[-]`.

      IF iter->has_next( ) EQ abap_false.
        res-int = 0 - res-int.
        res-real = 0 - res-real.
        res-nummer = 0 - res-nummer.
      ELSE.
*       Subtract all consecutive numbers from the first
        WHILE iter->has_next( ).
          cell = iter->next( ).
          _cell_arith - `[-]`.
        ENDWHILE.
      ENDIF.

      result = lcl_lisp_new=>numeric( res ).
    ENDMETHOD.                    "proc_subtract

    METHOD proc_divide.
      _cell_arith_definition.

      _validate list.
      DATA(iter) = list->new_iterator( ).
      IF iter->has_next( ) EQ abap_false.
        throw( |no number in [/]| ).
      ENDIF.
      cell = iter->next( ).
      res = VALUE #( denom = 1
                     exact = abap_false
                     operation = '/' ).
      _cell_arith / `[/]`.

      TRY.
          IF iter->has_next( ) EQ abap_false.
            CASE res-type.
              WHEN type_integer.
                IF res-int EQ 0.
                  cell->raise( ' division error [1/0]' ).
                ENDIF.
                res-denom = res-int.
                res-nummer = 1.
                res-type = type_rational.

              WHEN type_rational.
                IF res-nummer EQ 0.
                  cell->raise( ' division error [1/0]' ).
                ENDIF.

                DATA(lv_saved_nummer) = res-nummer.
                res-nummer = res-denom.
                res-denom = lv_saved_nummer.

              WHEN type_real.
                IF res-real EQ 0.
                  cell->raise( ' division error [1/0]' ).
                ENDIF.

                res-real = 1 / res-real.
            ENDCASE.
          ELSE.
            IF res-type EQ type_integer.
              res-nummer = res-int.
              res-denom = 1.
              res-type = type_rational.
            ENDIF.

            WHILE iter->has_next( ).
              cell = iter->next( ).
              _values_get_next cell.

              CASE cell->type.
                WHEN type_integer.
                  lo_int ?= cell.

                  IF lo_int->int EQ 0 AND lo_int->exact EQ abap_true.
                    cell->raise( ' is invalid in [/]' ).
                  ENDIF.

                  CASE res-type.
                    WHEN type_real.
                      res-real = res-real / lo_int->int.

                    WHEN type_rational.
                      res-denom = res-denom * lo_int->int.
                      lv_gcd = lcl_lisp_rational=>gcd(  n = res-nummer
                                                        d = res-denom ).
                      res-nummer = res-nummer DIV lv_gcd.
                      res-denom = res-denom DIV lv_gcd.

                    WHEN type_integer.
                      res-nummer = res-int.
                      res-denom = lo_int->int.
                      res-type = type_rational.

                    WHEN OTHERS.
                      throw( 'internal error [/]' ).
                      res-type = type_integer.
                      res-int = lo_int->int.
                  ENDCASE.

                WHEN type_real.
                  lo_real ?= cell.

                  IF lo_real->exact EQ abap_true AND lo_real->real EQ 0.
                    cell->raise( ' is invalid in [/]' ).
                  ENDIF.

                  CASE res-type.
                    WHEN type_real.
                      res-real = res-real / lo_real->real.

                    WHEN type_rational.
                      res-real = res-nummer / res-denom / lo_real->real.
                      res-type = type_real.

                    WHEN type_integer.
                      res-real = res-int / lo_real->real.
                      res-type = type_real.

                    WHEN OTHERS.
                      res-type = type_real.
                      res-real = lo_real->real.
                  ENDCASE.

                WHEN type_rational.
                  lo_rat ?= cell.

                  IF lo_rat->int = 0 AND lo_rat->exact EQ abap_true.
                    cell->raise( ' is invalid in [/]' ).
                  ENDIF.

                  CASE res-type.
                    WHEN type_real.
                      res-real = res-real * lo_rat->denominator / lo_rat->int.
                      res-type = type_real.

                    WHEN type_rational.
                      res-nummer = res-nummer * lo_rat->denominator.
                      res-denom = res-denom * lo_rat->int.
                      lv_gcd = lcl_lisp_rational=>gcd(  n = res-nummer
                                                        d = res-denom ).
                      res-nummer = res-nummer DIV lv_gcd.
                      res-denom = res-denom DIV lv_gcd.

                    WHEN type_integer.
                      res-nummer = res-int * lo_rat->denominator.
                      res-denom = lo_rat->int.

                      res-type = type_rational.

                    WHEN OTHERS.
                      res-type = type_rational.
                      res-nummer = lo_rat->int.
                      res-denom = lo_rat->denominator.
                  ENDCASE.

*                WHEN type_complex.
                WHEN OTHERS.
                  cell->raise_nan( |/| ).
              ENDCASE.

            ENDWHILE.
          ENDIF.
        _catch_arithmetic_error.
      ENDTRY.

      result = lcl_lisp_new=>numeric( res ).
    ENDMETHOD.                    "proc_divide

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
      _sign 0 '[zero?]'.
    ENDMETHOD.                    "proc_gt

    METHOD proc_is_positive.
      _sign 1 '[positive?]'.
    ENDMETHOD.                    "proc_gte

    METHOD proc_is_negative.
      _sign -1 '[negative?]'.
    ENDMETHOD.                    "proc_lt

    METHOD proc_is_odd.
      result = false.
      _validate: list.
      _validate_integer list->car '[odd?]'.
      CHECK CAST lcl_lisp_integer( list->car )->int MOD 2 NE 0.
      result = true.
    ENDMETHOD.                    "proc_lte

    METHOD proc_is_even.
      result = false.
      _validate: list.
      _validate_integer list->car '[even?]'.
      CHECK CAST lcl_lisp_integer( list->car )->int MOD 2 EQ 0.
      result = true.
    ENDMETHOD.                    "proc_lte

**********************************************************************

    METHOD proc_eql.
      _data_local_numeric.
      DATA lo_rat_2 TYPE REF TO lcl_lisp_rational.
      DATA lv_int TYPE tv_int.
      DATA lv_real TYPE tv_real.

      _validate: list, list->car, list->cdr.

      result = false.
      DATA(lo_ptr) = list.
      IF lo_ptr->cdr->type NE type_pair.
        throw( c_error_incorrect_input ).
      ENDIF.

      WHILE lo_ptr->cdr NE nil.
        _validate_number: lo_ptr->car '[=]',
                          lo_ptr->cdr->car '[=]'.
        CASE lo_ptr->car->type.
          WHEN type_integer.
            _to_integer lo_ptr->car lv_int.
            CASE lo_ptr->cdr->car->type.
              WHEN type_integer.
                IF lv_int = CAST lcl_lisp_integer( lo_ptr->cdr->car )->int.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

              WHEN type_real.
                _to_real lo_ptr->cdr->car lv_real.
                IF lv_int = trunc( lv_real ) AND frac( lv_real ) EQ 0.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

              WHEN type_rational.
                lo_rat ?= lo_ptr->cdr->car.
                IF lv_int = lo_rat->int AND lo_rat->denominator EQ 1.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

*              WHEN type_complex.
            ENDCASE.

          WHEN type_real.
            lv_real = CAST lcl_lisp_real( lo_ptr->car )->real.
            CASE lo_ptr->cdr->car->type.
              WHEN type_integer.
                IF trunc( lv_real ) = CAST lcl_lisp_integer( lo_ptr->cdr->car )->int AND frac( lv_real ) EQ 0.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

              WHEN type_real.
                IF CAST lcl_lisp_real( lo_ptr->cdr->car )->float_eq( lv_real ).
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

              WHEN type_rational.
                lo_rat ?= lo_ptr->cdr->car.
                lv_real = lv_real * lo_rat->denominator.

                IF trunc( lv_real ) = lo_rat->int AND frac( lv_real ) EQ 0.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

*              WHEN type_complex.
            ENDCASE.

          WHEN type_rational.
            lo_rat = CAST lcl_lisp_rational( lo_ptr->car ).
            CASE lo_ptr->cdr->car->type.
              WHEN type_integer.
                IF lo_rat->int = CAST lcl_lisp_integer( lo_ptr->cdr->car )->int
                  AND lo_rat->denominator EQ 1.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

              WHEN type_real.
                lv_real = CAST lcl_lisp_real( lo_ptr->cdr->car )->real * lo_rat->denominator.

                IF lo_rat->int = trunc( lv_real ) AND frac( lv_real ) EQ 0.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

              WHEN type_rational.
                lo_rat_2 ?= lo_ptr->cdr->car.
                IF lo_rat->int = lo_rat_2->int
                  AND lo_rat->denominator EQ lo_rat_2->denominator.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

*              WHEN type_complex.
            ENDCASE.

*         WHEN type_complex.

        ENDCASE.

        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
    ENDMETHOD.                    "proc_eql

    METHOD proc_eq.
      _validate: list, list->car, list->cdr.

      result = nil.
      DATA(lo_ptr) = list.
      DATA(lo_ref) = lo_ptr->car.
      IF lo_ptr->cdr NE nil.
        IF lo_ref->type NE lo_ptr->cdr->car->type.
          result = false.
          RETURN.
        ENDIF.
        CASE lo_ptr->car->type.
          WHEN type_integer.
            DATA(lo_int) = CAST lcl_lisp_integer( lo_ref ).
            DATA(lo_next_int) = CAST lcl_lisp_integer( lo_ptr->cdr->car ).
            IF lo_int->int = lo_next_int->int AND lo_int->exact = lo_next_int->exact.
              result = true.
            ELSE.
              result = false.
              RETURN.
            ENDIF.

          WHEN type_rational.
            DATA(lo_rat) = CAST lcl_lisp_rational( lo_ref ).
            DATA(lo_next_rat) = CAST lcl_lisp_rational( lo_ptr->cdr->car ).
            IF lo_rat->int = lo_next_rat->int AND lo_rat->exact = lo_next_rat->exact
              AND lo_rat->denominator = lo_next_rat->denominator.
              result = true.
            ELSE.
              result = false.
              RETURN.
            ENDIF.

          WHEN type_real.
            DATA(lo_real) = CAST lcl_lisp_real( lo_ref ).
            DATA(lo_real_next) = CAST lcl_lisp_real( lo_ptr->cdr->car ).
            IF lo_real->exact EQ lo_real_next->exact AND lo_real->float_eq( lo_real_next->real ).
              result = true.
            ELSE.
              result = false.
              RETURN.
            ENDIF.

          WHEN type_string.
            IF CAST lcl_lisp_string( lo_ref )->value = CAST lcl_lisp_string( lo_ptr->cdr->car )->value.
              result = true.
            ELSE.
              result = false.
              RETURN.
            ENDIF.

          WHEN type_symbol.
            DATA(lo_symbol) = CAST lcl_lisp_symbol( lo_ref ).
            DATA(lo_s_car) = CAST lcl_lisp_symbol( lo_ptr->cdr->car ).
            IF lo_symbol->value = lo_s_car->value
              AND lo_symbol->index = lo_s_car->index.  " for uninterned symbols
              result = true.
            ELSE.
              result = false.
              RETURN.
            ENDIF.

          WHEN OTHERS.
            IF lo_ref = lo_ptr->cdr->car.
              result = true.
            ELSE.
              result = false.
              RETURN.
            ENDIF.
        ENDCASE.

        lo_ptr = lo_ptr->cdr.
      ENDIF.
    ENDMETHOD.                    "proc_eq

    METHOD proc_equal.
* equal? returns the same as eqv? when applied to booleans, symbols, numbers, characters, ports,
* procedures, and the empty list. If two objects are eqv?, they must be equal? as well.
* In all other cases, equal? may return either #t or #f.
* Even if its arguments are circular data structures, equal? must always terminate.
      _validate: list, list->car.
      result = false.
      DATA(lo_ptr) = list.
      DATA(lo_slow) = list.

      WHILE lo_ptr->cdr NE nil.
        DATA(lo_next) = lo_ptr->cdr->car.
        _validate lo_next.

        result = lo_next->is_equal( lo_ptr->car ).
        IF result EQ false.
          RETURN.
        ENDIF.
        lo_ptr = lo_ptr->cdr.
        lo_slow = lo_slow->cdr.

        CHECK lo_ptr->cdr NE nil.
        lo_next = lo_ptr->cdr->car.
        _validate lo_next.

        result = lo_next->is_equal( lo_ptr->car ).
        IF result EQ false.
          RETURN.
        ENDIF.
        lo_ptr = lo_ptr->cdr.
        CHECK lo_ptr EQ lo_slow.
*       Circular list
        RETURN.
      ENDWHILE.

    ENDMETHOD.                    "proc_equal

    METHOD proc_eqv. " eqv?
      _validate: list, list->car.
      result = false.

      DATA(lo_ptr) = list.
      WHILE lo_ptr->cdr NE nil.
        DATA(lo_next) = lo_ptr->cdr->car.
        _validate lo_next.
        result = lo_next->is_equivalent( lo_ptr->car ).
        IF result EQ false.
          RETURN.
        ENDIF.
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.

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
      _is_type string.
    ENDMETHOD.                    "proc_is_string

    METHOD proc_is_char.
      _is_type char.
    ENDMETHOD.                    "proc_is_char

    METHOD proc_is_hash.
      _is_type hash.
    ENDMETHOD.                    "proc_is_hash

    METHOD proc_is_number. " argument in list->car
      result = false.
      CHECK list IS BOUND AND list->car IS BOUND.
      result = list->car->is_number( ).
    ENDMETHOD.                    "proc_is_number

    METHOD proc_is_complex.
      result = false.
      CHECK list IS BOUND AND list->car IS BOUND.
      result = list->car->is_number( ).
*      _is_type complex.
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
      ENDCASE.
    ENDMETHOD.

    METHOD proc_is_rational.
      result = false.
      CHECK list IS BOUND AND list->car IS BOUND.
      CASE list->car->type.
        WHEN type_rational
          OR type_integer.
          result = true.
      ENDCASE.
    ENDMETHOD.

    METHOD proc_is_exact_integer.
      _validate list.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
*     (exact-integer? z) procedure
*     Returns #t if z is both exact and an integer; otherwise returns #f.
      result = false.
      CHECK list->car IS BOUND.
      CASE list->car->type.
        WHEN type_integer.
          result = true.
        WHEN type_rational.
          lo_rat ?= list->car.
          CHECK lo_rat->denominator EQ 1.
          result = true.
      ENDCASE.
    ENDMETHOD.

    METHOD proc_is_integer.
      _validate list.
*     If x is an inexact real number, then (integer? x) is true if and only if (= x (round x)).
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_real TYPE REF TO lcl_lisp_real.
      DATA lv_real TYPE tv_real.

      result = false.
      CHECK list->car IS BOUND.
      CASE list->car->type.
        WHEN type_integer.
          result = true.
        WHEN type_rational.
          lo_rat ?= list->car.
          CHECK lo_rat->denominator EQ 1.
          result = true.
        WHEN type_real.
          _to_real list->car lv_real.
          CHECK trunc( lv_real ) EQ lv_real.
          result = true.
      ENDCASE.
    ENDMETHOD.                    "proc_is_integer

    METHOD proc_is_symbol.
      _is_type symbol.
    ENDMETHOD.

    METHOD proc_is_pair. " argument in list->car
      _validate list.
      _is_type pair.
    ENDMETHOD.                    "proc_is_pair

    METHOD proc_is_boolean. " argument in list->car
      _validate list.
      _is_type boolean.
    ENDMETHOD.

    METHOD proc_is_vector.  " argument in list->car
      _validate: list, list->car.
      _is_type vector.
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
    ENDMETHOD.                    "proc_is_boolean

    METHOD proc_is_procedure.
      result = false.
      CHECK list IS BOUND        " paramater (car) must not be valid
        AND list->car IS BOUND.  " Body
      result = list->car->is_procedure( ).
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

    METHOD proc_abs.
      _data_local_numeric.

      result = nil.
      _validate list.
      _assert_last_param list.

      TRY.
          _validate list->car.
          CASE list->car->type.
            WHEN type_integer.
              lo_int ?= list->car.
              result = lcl_lisp_new=>integer( abs( lo_int->int ) ).
            WHEN type_real.
              lo_real ?= list->car.
              result = lcl_lisp_new=>integer( abs( lo_real->real ) ).
            WHEN type_rational.
              lo_rat ?= list->car.
              result = lcl_lisp_new=>rational( nummer = abs( lo_rat->int )
                                               denom = lo_rat->denominator ).
*            WHEN type_complex.
            WHEN OTHERS.
              list->car->raise_nan( |[abs]| ).
          ENDCASE.

        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_abs

    METHOD proc_sin.
      _trigonometric sin '[sin]'.
    ENDMETHOD.                    "proc_sin

    METHOD proc_cos.
      _trigonometric cos '[cos]'.
    ENDMETHOD.                    "proc_cos

    METHOD proc_tan.
      _trigonometric tan '[tan]'.
    ENDMETHOD.                    "proc_tan

    METHOD proc_asin.
      _trigonometric asin '[asin]'.
    ENDMETHOD.                    "proc_asin

    METHOD proc_acos.
      _trigonometric acos '[acos]'.
    ENDMETHOD.                    "proc_acos

    METHOD proc_atan.
      _trigonometric atan '[atan]'.
    ENDMETHOD.                    "proc_atan

    METHOD proc_sinh.
      _trigonometric sinh '[sinh]'.
    ENDMETHOD.                    "proc_sinh

    METHOD proc_cosh.
      _trigonometric cosh '[cosh]'.
    ENDMETHOD.                    "proc_cosh

    METHOD proc_tanh.
      _trigonometric tanh '[tanh]'.
    ENDMETHOD.                    "proc_tanh

    METHOD proc_asinh.
      DATA carry TYPE f.
      _data_local_numeric_cell.

      result = nil.
      _validate list.
      TRY.
          _get_number carry list->car '[asinh]'.
          _assert_last_param list.
          result = lcl_lisp_new=>real( value = log( carry + sqrt( carry ** 2 + 1 ) )
                                       exact = abap_false ).
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_asinh

    METHOD proc_acosh.
      DATA carry TYPE f.
      _data_local_numeric_cell.

      result = nil.
      _validate list.
      TRY.
          _get_number carry list->car '[acosh]'.
          _assert_last_param list.
          result = lcl_lisp_new=>real( value = log( carry + sqrt( carry ** 2 - 1 ) )
                                       exact = abap_false ).
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_acosh

    METHOD proc_atanh.
      DATA carry TYPE f.
      _data_local_numeric_cell.

      result = nil.
      _validate list.
      TRY.
          _get_number carry list->car '[atanh]'.
          _assert_last_param list.
          result = lcl_lisp_new=>real( value = ( log( 1 + carry ) - log( 1 - carry ) ) / 2
                                       exact = abap_false ).
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

      _get_number base1 list->car '[expt]'.

      TRY.
*          _get_number exp1 list->cdr->car '[expt]'.
          _validate list->cdr->car.
          cell = list->cdr->car.
          CASE cell->type.
            WHEN type_integer.
              _to_integer cell exp1.
              result = lcl_lisp_new=>number( value = ipow( base = base1  exp = exp1 )
                                             iv_exact = CAST lcl_lisp_number( cell )->exact ).
            WHEN type_real.
              _to_real cell exp1.
              result = lcl_lisp_new=>number( value = base1 ** exp1
                                             iv_exact = CAST lcl_lisp_number( cell )->exact ).
            WHEN type_rational.
              lo_rat ?= cell.
              exp1 = lo_rat->int / lo_rat->denominator.
              result = lcl_lisp_new=>number( value = base1 ** exp1
                                             iv_exact = CAST lcl_lisp_number( cell )->exact ).
*            WHEN type_complex.
            WHEN OTHERS.
              cell->raise_nan( |[expt]| ) ##NO_TEXT.
          ENDCASE.

        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_expt

    METHOD proc_exp.
      _math exp '[exp]'.
    ENDMETHOD.                    "proc_exp

    METHOD proc_log.
      _math log '[log]'.
    ENDMETHOD.                    "proc_log

    METHOD proc_sqrt.
      _math sqrt '[sqrt]'.
    ENDMETHOD.                    "proc_sqrt

    METHOD proc_square.
      "_math square '[square]' real.
      DATA carry TYPE tv_real.
      _data_local_numeric_cell.

      result = nil.
      _validate list.
      TRY.
          _get_number carry list->car 'square'.
          list->assert_last_param( ).

          result = lcl_lisp_new=>number( value = carry * carry
                                         iv_exact = CAST lcl_lisp_number( cell )->exact ).
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_square

    METHOD proc_floor.
      _math floor '[floor]'.
    ENDMETHOD.                    "proc_floor

    METHOD proc_int_sqrt.
      " procedure (exact-integer-sqrt k) returns two non-negative exact integers s and r where k = s^2 + r and k < (s + 1)^2.
      " (exact-integer-sqrt 4) =) 2 0
      " (exact-integer-sqrt 5) =) 2 1
      DATA lv_int TYPE tv_int.
      DATA lv_rest TYPE tv_int.
      DATA lo_int TYPE REF TO lcl_lisp_integer.

      result = nil.
      TRY.
            _validate: list, list->car, list->cdr.

            CASE list->car->type.
              WHEN type_integer.
                 lo_int = CAST lcl_lisp_integer( list->car ).
                 IF lo_int->exact EQ abap_false.
                   throw( list->car->to_string( ) && | is a not an exact integer in [exact-integer-sqrt]| ).
                 ENDIF.
                 lv_int = lo_int->int.
                 IF lv_int LT 0.
                   throw( list->car->to_string( ) && | is a negative integer in [exact-integer-sqrt]| ).
                 ELSEIF lv_int GT 1.
                   lv_int = trunc( sqrt( lo_int->int ) ).
                 ENDIF.
                 lv_rest = lo_int->int - lv_int * lv_int.
              WHEN OTHERS.
                throw( list->car->to_string( ) && | is not an integer in [exact-integer-sqrt]| ).
            ENDCASE.

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
      DATA carry TYPE tv_real.
      DATA n1 TYPE tv_int.
      DATA n2 TYPE tv_int.
      DATA nq TYPE tv_int.  " quotient
      DATA nr TYPE tv_int.  " remainder
      DATA exact TYPE flag.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      TRY.
          _get_2_ints n1 n2 exact list 'floor/'.

          carry = n1 / n2.    " first convert to float, or 3 = trunc( 5 / 2 ) coercion happens with ABAP rules
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
      DATA carry TYPE tv_real.
      DATA n1 TYPE tv_int.
      DATA n2 TYPE tv_int.
      DATA nq TYPE tv_int.  " quotient
      DATA exact TYPE flag.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      TRY.
          _get_2_ints n1 n2 exact list 'floor-quotient'.

          carry = n1 / n2.    " first convert to float, or 3 = trunc( 5 / 2 ) coercion happens with ABAP rules
          nq = floor( carry ).
          result = lcl_lisp_new=>integer( value = nq
                                          iv_exact = exact ).
        CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD proc_floor_remainder.
      " (floor-remainder n1 n2) -- equivalent to (modulo n1 n2)
      DATA carry TYPE tv_real.
      DATA n1 TYPE tv_int.
      DATA n2 TYPE tv_int.
      DATA nq TYPE tv_int.  " quotient
      DATA nr TYPE tv_int.  " remainder
      DATA exact TYPE flag.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      TRY.
          _get_2_ints n1 n2 exact list 'floor-remainder'.

          carry = n1 / n2.    " first convert to float, or 3 = trunc( 5 / 2 ) coercion happens with ABAP rules
          nq = floor( carry ).
          nr = n1 - n2 * nq.

          result = lcl_lisp_new=>number( value = nr
                                         iv_exact = exact ).

        CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD proc_truncate_new.
      DATA carry TYPE tv_real.
      DATA n1 TYPE tv_int.
      DATA n2 TYPE tv_int.
      DATA nq TYPE tv_int.  " quotient
      DATA nr TYPE tv_int.  " remainder
      DATA exact TYPE flag.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      TRY.
          _get_2_ints n1 n2 exact list 'truncate/'.

          carry = n1 / n2.    " first convert to float, or 3 = trunc( 5 / 2 ) coercion happens with ABAP rules
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
      DATA carry TYPE tv_real.
      DATA n1 TYPE tv_int.
      DATA n2 TYPE tv_int.
      DATA nq TYPE tv_int.  " quotient
      DATA exact TYPE flag.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      TRY.
          _get_2_ints n1 n2 exact list 'truncate-quotient'.

          carry = n1 / n2.    " first convert to float, or 3 = trunc( 5 / 2 ) coercion happens with ABAP rules
          nq = trunc( carry ).
          result = lcl_lisp_new=>integer( value = nq
                                          iv_exact = exact ).
        CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD proc_trunc_remainder.
    " (truncate-remainder n1 n2) -- equivalent to (remainder n1 n2)
      DATA carry TYPE tv_real.
      DATA n1 TYPE tv_int.
      DATA n2 TYPE tv_int.
      DATA nq TYPE tv_int.  " quotient
      DATA nr TYPE tv_int.  " remainder
      DATA exact TYPE flag.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      TRY.
          _get_2_ints n1 n2 exact list 'truncate-remainder/'.

          carry = n1 / n2.    " first convert to float, or 3 = trunc( 5 / 2 ) coercion happens with ABAP rules
          nq = trunc( carry ).
          nr = n1 - n2 * nq.     " n1 = n2 * nq + nr.
          result = lcl_lisp_new=>integer( value = nr
                                          iv_exact = exact ).
        CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.

    METHOD proc_rationalize.
      throw( `rationalize not implemented yet` ).
    ENDMETHOD.

    METHOD proc_ceiling.
      _math ceil '[ceil]'.
    ENDMETHOD.                    "proc_ceiling

    METHOD proc_truncate.
      _math trunc '[truncate]'.
    ENDMETHOD.                    "proc_truncate

    METHOD proc_round.
      DATA carry TYPE tv_real.
      _data_local_numeric_cell.

      result = nil.
      _validate list.
      _get_number carry list->car '[round]'.
      _assert_last_param list.
      TRY.
          result = lcl_lisp_new=>number( value = round( val = carry dec = 0 )
                                         iv_exact = CAST lcl_lisp_number( cell )->exact ).
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_round

    METHOD proc_numerator.
      DATA lv_num TYPE tv_real.
      _data_local_numeric_cell.

      _validate: list, list->car.
      _assert_last_param list.

      cell = list->car.
      CASE cell->type.
        WHEN type_integer.
          result = cell.

        WHEN type_real.
          TRY.
              _to_real cell lv_num.
              result = lcl_lisp_new=>number( lcl_lisp_real=>gcd( n = lv_num
                                                                 d = 1 ) * lv_num ).
            _catch_arithmetic_error.
          ENDTRY.

        WHEN type_rational.
          lo_rat ?= cell.
          result = lcl_lisp_new=>integer( lo_rat->int ).
*        WHEN type_complex.
        WHEN OTHERS.
          throw( |{ cell->to_string( ) } is not a number in [numerator]| ).
      ENDCASE.
    ENDMETHOD.

    METHOD proc_denominator.
      DATA lo_frac TYPE REF TO lcl_lisp_real.
      _data_local_numeric_cell.

      _validate: list, list->car.
      _assert_last_param list.

      cell = list->car.
      CASE cell->type.
        WHEN type_integer.
          result = lcl_lisp_new=>integer( 1 ).

        WHEN type_real.
          TRY.
              lo_real ?= cell.
              lo_frac = lcl_lisp_new=>real( value = frac( lo_real->real )
                                            exact = abap_true ).
              IF lo_frac->float_eq( 0 ).
                result = lcl_lisp_new=>integer( 1 ).
              ELSE.
                result = lcl_lisp_new=>real( value = 1 / lo_frac->real
                                             exact = abap_false ).
              ENDIF.
            _catch_arithmetic_error.
          ENDTRY.

        WHEN type_rational.
          lo_rat ?= cell.
          result = lcl_lisp_new=>integer( lo_rat->denominator ).
*        WHEN type_complex.
        WHEN OTHERS.
          throw( |{ cell->to_string( ) } is not a number in [denominator]| ).
      ENDCASE.
    ENDMETHOD.

    METHOD proc_remainder.
      DATA numerator TYPE tv_real.
      DATA denominator TYPE tv_real.
      _data_local_numeric_cell.

      result = nil.
      _validate: list, list->cdr.
      _get_number numerator list->car '[remainder]'.
      _get_number denominator list->cdr->car '[remainder]'.
      _assert_last_param list->cdr.
      TRY.
          result = lcl_lisp_new=>number( numerator - denominator * trunc( numerator / denominator ) ).
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_remainder

    METHOD proc_quotient.
      DATA numerator TYPE tv_real.
      DATA denominator TYPE tv_real.
      _data_local_numeric_cell.

      result = nil.
      _validate: list, list->cdr.
      _get_number numerator list->car '[quotient]'.
      _get_number denominator list->cdr->car '[quotient]'.
      _assert_last_param list->cdr.
      TRY.
          result = lcl_lisp_new=>number( trunc( numerator / denominator ) ).
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
      _get_number numerator list->car '[modulo]'.
      _get_number base list->cdr->car '[modulo]'.
      _assert_last_param list->cdr.
      TRY.
          mod = numerator MOD base.
          IF sign( base ) LE 0 AND mod NE 0.
            mod = mod + base.
          ENDIF.
          result = lcl_lisp_new=>number( mod ).
        _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_modulo

    METHOD proc_random.
      DATA lv_high TYPE i.

      result = nil.
      _validate list.
      _validate_integer list->car '[random]'.
      _assert_last_param list.
      TRY.
          DATA(lo_rnd) = cl_abap_random=>create( cl_abap_random=>seed( ) ).
          _to_integer list->car lv_high.
          result = lcl_lisp_new=>number( lo_rnd->intinrange( high = lv_high ) ).
        CATCH cx_dynamic_check INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.                    "proc_random

    METHOD proc_is_exact.
      DATA lo_number TYPE REF TO lcl_lisp_number.
      _validate list.
      _validate_number list->car `exact?`.
      lo_number ?= list->car.
      result = lo_number->is_exact( ).
    ENDMETHOD.

    METHOD proc_is_inexact.
      DATA lo_number TYPE REF TO lcl_lisp_number.
      _validate list.
      _validate_number list->car `inexact?`.
      lo_number ?= list->car.
      result = lo_number->is_inexact( ).
    ENDMETHOD.

    METHOD proc_to_exact.
      DATA lo_number TYPE REF TO lcl_lisp_number.
      DATA lo_real TYPE REF TO lcl_lisp_real.
      DATA lv_denom TYPE tv_int.
      DATA lv_nummer TYPE tv_int.

      _validate list.
      _validate_number list->car `exact`.
      lo_number ?= list->car.
      IF lo_number->exact EQ abap_true.
        result = lo_number.
      ELSE.
        CASE lo_number->type.
          WHEN type_real.
            lo_real ?= lo_number.
            IF abs( lo_real->real ) GT 1.
              lv_denom = trunc( cl_abap_math=>max_int4 / lo_real->real ).
              lv_nummer = round( val = lo_real->real * lv_denom dec = 0 ).
            ELSE.
              lv_nummer = trunc( cl_abap_math=>max_int4 * lo_real->real ).
              lv_denom = round( val = lv_nummer / lo_real->real dec = 0 ).
            ENDIF.

            result = lcl_lisp_new=>rational( nummer = lv_nummer
                                             denom = lv_denom ).
          WHEN OTHERS.
            throw( |no exact representation of { lo_number->to_string( ) }| ).
        ENDCASE.
      ENDIF.
    ENDMETHOD.

    METHOD proc_to_inexact.
      DATA lo_number TYPE REF TO lcl_lisp_number.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lv_real TYPE tv_real.

      _validate list.
      _validate_number list->car `inexact`.
      lo_number ?= list->car.
      IF lo_number->exact EQ abap_true.
        CASE lo_number->type.
          WHEN type_rational.
            lo_rat ?= lo_number.
            lv_real = lo_rat->int / lo_rat->denominator.
            result = lcl_lisp_new=>real( value = lv_real
                                         exact = abap_false ).
          WHEN type_integer.
            lo_int ?= lo_number.
            lv_real = lo_int->int.
            result = lcl_lisp_new=>real( value = lv_real
                                         exact = abap_false ).
          WHEN OTHERS.
            throw( |no inexact representation of { lo_number->to_string( ) }| ).
        ENDCASE.
      ELSE.
        result = lo_number.
      ENDIF.
    ENDMETHOD.

    METHOD proc_num_to_string.
      DATA lv_radix TYPE i VALUE 10.
      DATA lv_radix_error TYPE flag VALUE abap_false.
      DATA lv_text TYPE string.
      DATA lv_int TYPE tv_int.
      DATA lv_real TYPE tv_real.
      DATA lv_digit TYPE i.

      _validate list.
      "_validate_number list->car `number->string`.
      _validate list->car.
*     Optional radix
      _validate list->cdr.
      IF list->cdr NE nil.
        _validate_integer list->cdr->car `number->string`.
        _to_integer list->cdr->car lv_radix.
      ENDIF.

      CASE list->car->type.
        WHEN type_integer.
          _to_integer list->car lv_int.
          CASE lv_radix.
            WHEN 10.
              lv_text = lv_int.
              result = lcl_lisp_new=>string( condense( lv_text ) ).

            WHEN 2 OR 8 OR 16.
              CLEAR lv_text.
              WHILE lv_int GT 0.
                lv_digit = lv_int MOD lv_radix.
                lv_int = lv_int DIV lv_radix.
                lv_text = c_hex_digits+lv_digit(1) && lv_text.
              ENDWHILE.
              result = lcl_lisp_new=>string( lv_text ).

            WHEN OTHERS.
              lv_radix_error = abap_true.
          ENDCASE.

        WHEN type_real.
          lv_radix_error = xsdbool( lv_radix NE 10 ).
          _to_real list->car lv_real.
          lv_text = lv_real.
          result = lcl_lisp_new=>string( condense( lv_text ) ).

        WHEN type_rational.
          lv_radix_error = xsdbool( lv_radix NE 10 ).
          lv_text = list->car->to_string( ).
          result = lcl_lisp_new=>string( lv_text ).

        WHEN OTHERS.
          throw( |{ list->car->to_string( ) } is not a number in number->string| ) ##NO_TEXT.
      ENDCASE.
      IF lv_radix_error EQ abap_true.
        throw( |{ list->car->to_string( ) } radix { lv_radix } not supported in number->string| ) ##NO_TEXT.
      ENDIF.

    ENDMETHOD.

    METHOD string_to_number.
      DATA lv_radix_error TYPE flag VALUE abap_false.

      result = false.
      CHECK iv_text NE space.
      TRY.
          CASE iv_radix.
            WHEN 2.
              result = lcl_lisp_new=>binary( iv_text ).
            WHEN 8.
              result = lcl_lisp_new=>octal( iv_text ).
            WHEN 10.
              result = lcl_lisp_new=>number( iv_text ).
            WHEN 16.
              result = lcl_lisp_new=>hex( iv_text ).
            WHEN OTHERS.
              lv_radix_error = abap_true.
          ENDCASE.
        CATCH lcx_lisp_exception cx_sy_conversion_error ##NO_HANDLER.
      ENDTRY.
      CHECK lv_radix_error EQ abap_true.
      throw( |radix ({ iv_radix }) must be 2, 8, 10 or 16 in string->number| ).
    ENDMETHOD.

    METHOD proc_string_to_num.
      DATA lv_radix TYPE i VALUE 10.

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

    METHOD proc_write_char.
      _validate list.
      _validate_char list->car `write-char`.
      result = write( io_elem = list->car
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

    METHOD proc_read_string.
      result = read_string( io_arg = list ).
    ENDMETHOD.

    METHOD proc_peek_char.
      _optional_port list input `peek-char`.
      result = lcl_lisp_new=>char( li_port->peek_char( ) ).
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
        lv_text = lv_text && lo_ptr->car->value+0(1).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      IF lo_ptr NE nil.
        throw( |{ lo_ptr->to_string( ) } is not a list of char | ).
      ENDIF.

      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_make_string.
      DATA lv_len TYPE tv_index.
      DATA lv_char TYPE char01.
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
      DATA lv_char TYPE c LENGTH 1.
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
      DATA lv_char TYPE c LENGTH 1.
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
      DATA lv_char TYPE c LENGTH 1.
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
      lv_text = lv_text && lv_char && lo_string->value+lv_index.

      lo_string->value = lv_text.
      result = lo_string.
    ENDMETHOD.

    METHOD proc_list_to_string.
      DATA lv_text TYPE string.
      DATA lo_ptr TYPE REF TO lcl_lisp.

      _validate: list, list->car.

      lo_ptr = list->car.
      WHILE lo_ptr->type = type_pair AND lo_ptr->car->type EQ type_char.
        lv_text = lv_text && lo_ptr->car->value+0(1).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      IF lo_ptr NE nil.
        throw( |{ lo_ptr->to_string( ) } is not a list of char| ).
      ENDIF.
      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_symbol_to_string.
      _validate: list, list->car.

      IF list->car->type = type_symbol.
        result = lcl_lisp_new=>string( value = list->car->value
                                       iv_mutable = abap_false ).
      ELSE.
        throw( |{ list->car->to_string( ) } is not a symbol| ).
      ENDIF.
    ENDMETHOD.

    METHOD proc_string_to_symbol.
      _validate: list, list->car.

      IF list->car->type = type_string.
        result = lcl_lisp_new=>symbol( list->car->value ).
      ELSE.
        throw( |{ list->car->to_string( ) } is not a string| ).
      ENDIF.
    ENDMETHOD.

    METHOD proc_string_append.
      DATA lv_text TYPE string.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      _validate list.

      lo_ptr = list.
      WHILE lo_ptr->type = type_pair AND lo_ptr->car->type EQ type_string.
        lv_text = lv_text && lo_ptr->car->value.
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      IF lo_ptr NE nil.
        throw( |{ lo_ptr->car->to_string( ) } is not a string| ).
      ENDIF.
      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_max.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      _data_local_numeric_cell.

      DATA carry TYPE tv_real.
      DATA lv_max TYPE tv_real.

      _validate: list, list->cdr.
      _get_number lv_max list->car '[max]'.

      lo_ptr = list->cdr.
      WHILE lo_ptr->type EQ type_pair.
        _get_number carry lo_ptr->car '[max]'.
        lv_max = nmax( val1 = carry val2 = lv_max ).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      result = lcl_lisp_new=>number( lv_max ).
    ENDMETHOD.

    METHOD proc_min.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      _data_local_numeric_cell.

      DATA carry TYPE tv_real.
      DATA lv_min TYPE tv_real.

      _validate: list, list->cdr.
      _get_number lv_min list->car '[min]'.

      lo_ptr = list->cdr.
      WHILE lo_ptr->type EQ type_pair.
        _get_number carry lo_ptr->car '[min]'.
        lv_min = nmin( val1 = carry val2 = lv_min ).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      result = lcl_lisp_new=>number( lv_min ).
    ENDMETHOD.

    METHOD proc_gcd.
*     non-negative greatest common divisor of the arguments
      DATA lo_ptr TYPE REF TO lcl_lisp.
      _data_local_numeric_cell.

      DATA carry TYPE tv_real.
      DATA lv_gcd TYPE tv_real VALUE 0.

      _validate list.
      IF list NE nil.

        _get_number lv_gcd list->car '[gcd]'.

        lo_ptr = list->cdr.
        WHILE lo_ptr->type EQ type_pair.
          _get_number carry lo_ptr->car '[gcd]'.
          lv_gcd = lcl_lisp_rational=>gcd( n = carry d = lv_gcd ).
          lo_ptr = lo_ptr->cdr.
        ENDWHILE.
      ENDIF.
      result = lcl_lisp_new=>number( lv_gcd ).
    ENDMETHOD.

    METHOD proc_lcm.
*     non-negative least common multiple of the arguments
      DATA lo_ptr TYPE REF TO lcl_lisp.
      _data_local_numeric_cell.

      DATA carry TYPE tv_real.
      DATA lv_lcm TYPE tv_real VALUE 1.

      _validate list.
      IF list NE nil.

        _get_number lv_lcm list->car '[lcm]'.

        lo_ptr = list->cdr.
        WHILE lo_ptr->type EQ type_pair.
          _get_number carry lo_ptr->car '[lcm]'.
          lv_lcm = lv_lcm * carry / lcl_lisp_rational=>gcd( n = carry d = lv_lcm ).
          lo_ptr = lo_ptr->cdr.
        ENDWHILE.
      ENDIF.
      result = lcl_lisp_new=>number( abs( lv_lcm ) ).
    ENDMETHOD.

    METHOD proc_is_textual_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.

      _validate: list, list->car.
      result = false.
      CHECK list->car->type EQ type_port.
      lo_port ?= list->car.
      CHECK lo_port->port_type EQ lcl_lisp_port=>c_port_textual.
      result = true.
    ENDMETHOD.

    METHOD proc_is_binary_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.

      _validate: list, list->car.
      result = false.
      CHECK list->car->type EQ type_port.
      lo_port ?= list->car.
      CHECK lo_port->port_type EQ lcl_lisp_port=>c_port_binary.
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
      result = lcl_lisp_new=>port( iv_port_type = lcl_lisp_port=>c_port_textual
                                   iv_output = abap_true
                                   iv_input = abap_false
                                   iv_error = abap_false
                                   iv_buffered = abap_true ).
    ENDMETHOD.

    METHOD proc_open_input_string.
      DATA lo_port TYPE REF TO lcl_lisp_port.
      _validate list.
      _validate_string list->car `open-input-string`.

      lo_port = lcl_lisp_new=>port( iv_port_type = lcl_lisp_port=>c_port_textual
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
      result = lcl_lisp=>eof_object. " cannot be read using (read), must not be unique
    ENDMETHOD.

    METHOD proc_is_eof_object.
      _validate: list, list->car.
      result = false.
      CHECK list->car EQ lcl_lisp=>eof_object.
      result = true.
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
          throw( |{ lo_converter->to_string( ) } is not a procedure in make-parameter| ).
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
      DATA lv_char TYPE char01.
      _validate list.
      _validate_char list->car `char-alphabetic?`.
      result = false.
      lv_char = list->car->value.
      CHECK lv_char NE space AND to_upper( lv_char ) CO c_abcde.
      result = true.
    ENDMETHOD.

    METHOD proc_is_char_numeric.
      DATA lv_char TYPE char01.

      _validate list.
      _validate_char list->car `char-numeric?`.
      lv_char = list->car->value.

      IF unicode_to_digit( lv_char ) BETWEEN 0 AND 9.
        result = true.
      ELSE.
        result = false.
      ENDIF.
    ENDMETHOD.

    METHOD unicode_to_digit.
      FIELD-SYMBOLS <lv_hex> TYPE x.
      FIELD-SYMBOLS <lv_int> TYPE x.
      DATA ls_digit TYPE ts_digit.
      DATA lv_xdigit TYPE ts_digit-zero.
      DATA lv_zero TYPE i.
      DATA lv_index TYPE sytabix.
      DATA lv_int TYPE tv_int.

      rv_digit = -1.

      ASSIGN iv_char TO <lv_hex> CASTING.
      ASSIGN lv_int TO <lv_int> CASTING.
      <lv_int> = <lv_hex>. " conversion
      lv_xdigit = lv_int.

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
      DATA lv_char TYPE char01.
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
        WHEN lcl_lisp=>char_space OR lcl_lisp=>char_tab OR lcl_lisp=>char_newline.
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

    DEFINE _char01_to_integer.
      FIELD-SYMBOLS <xword> TYPE x.
      FIELD-SYMBOLS <xint> TYPE x.
      DATA lv_int TYPE int2.

      ASSIGN &1 TO <xword> CASTING.
      ASSIGN lv_int TO <xint> CASTING.
      <xint> = <xword>.
      &2 = lv_int.
    END-OF-DEFINITION.

    METHOD char_to_integer.
      DATA lv_char TYPE tv_char.

      lv_char = io_char->value+0(1).
      _char01_to_integer lv_char rv_int.
    ENDMETHOD.

    METHOD char_fold_case_to_integer.
      DATA lv_char TYPE tv_char.

      lv_char = to_upper( element->value+0(1) ).
      _char01_to_integer lv_char rv_int.
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
      DATA lv_char TYPE char01.
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
      _validate list.
      throw( list->to_string( ) ).
    ENDMETHOD.

    METHOD proc_raise.
      _validate list.
      _validate list->car.
      throw( list->car->to_string( ) ).
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
*             call-with-current-continuationinto some kind of Scheme object with unlimited extent
*B) creg-set! - throw procedure - takes such an object and installs it as the continuation
*      for the currently executing procedure overriding the previous implicit continuation.
* f is called an escape procedure, because a call to the escape procedure will allow control
*   to bypass the implicit continuation.

*simplest implementation strategy: allocate storage for each continuation frame (activation record)
*  on a heap and reclaim that storage through garbage collection or reference counting [23].
*  With this strategy, which we call the gc strategy, creg-get can just return the contents of a
*  continuation register (which is often called the dynamic link, stack pointer, or frame pointer),
*  and creg-set! can just store its argument into that register.

      _validate list.
      result = nil.
      throw( `call/cc not implemented yet` ).
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
                       THEN lcl_lisp_new=>number( data )
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

                _get_number lv_real lo_next->car `turtles`.
                lo_init_angle = lcl_lisp_new=>real( value = lv_real
                                                    exact = abap_false ).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lo_init_x IS NOT BOUND.
        lo_init_x = lcl_lisp_new=>integer( lo_width->int div 2 ).
      ENDIF.

      IF lo_init_y IS NOT BOUND.
        lo_init_y = lcl_lisp_new=>integer( lo_height->int div 2 ).
      ENDIF.

      IF lo_init_angle IS NOT BOUND.
        lo_init_angle = lcl_lisp_new=>real( value = 0
                                            exact = abap_true ).
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

      lo_init_x = lcl_lisp_new=>integer( lo_width->int div 2 ).
      lo_init_y = lcl_lisp_new=>integer( lo_height->int div 2 ).
      lo_init_angle = lcl_lisp_new=>real( value = 0
                                          exact = abap_true ).
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
      _get_number theta list->car `turtles turn`.

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
      _get_number theta list->car `turtles turn/radians theta`.

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
                                                                   exact = abap_false ) )  )
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
*     find the environment where the symbol is defined
      env = me.
      WHILE env IS BOUND.
        IF line_exists( env->map[ symbol = symbol ] ).
          RETURN.                      " found
        ENDIF.
        env = env->outer.
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
*     takes a symbol key and uses the find logic to locate the environment with the key,
*     then returns the matching value.
      lo_env = me.
      WHILE lo_env IS BOUND.
        READ TABLE lo_env->map INTO ls_map WITH KEY symbol = symbol.
        IF sy-subrc EQ 0.
          cell = ls_map-value.
          RETURN.
        ENDIF.
        lo_env = lo_env->outer.
      ENDWHILE.
*     raises an "unbound" error if key is not found
      unbound_symbol( symbol ).
    ENDMETHOD.

    METHOD unbound_symbol.
*     symbol not found in the environment chain
      RAISE EXCEPTION TYPE lcx_lisp_exception
        EXPORTING
          message = |Symbol { symbol } is unbound|
          area    = c_area_eval.
    ENDMETHOD.

    METHOD define_value.
      element = lcl_lisp_new=>elem( type = type
                                    value = value ).
      set( symbol = symbol
           element = element ).
    ENDMETHOD.                    "define_cell

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

    METHOD prepare.
      " Create symbols for nil, true and false values
      set( symbol = 'nil' element = lcl_lisp=>nil ).
      set( symbol = '#f' element = lcl_lisp=>true ).
      set( symbol = '#t' element = lcl_lisp=>false ).

      " Add primitive functions to environment
      define_value( symbol = 'define'          type = type_syntax value   = 'define' ).
      define_value( symbol = 'lambda'          type = type_syntax value   = 'lambda' ).
      define_value( symbol = 'if'              type = type_syntax value   = 'if' ).
      define_value( symbol = c_eval_quote      type = type_syntax value   = `'` ).
      define_value( symbol = c_eval_quasiquote type = type_syntax value   = '`' ).
      define_value( symbol = 'set!'            type = type_syntax value   = 'set!' ).

      define_value( symbol = 'define-values'   type = type_syntax value   = 'define-values' ).

      define_value( symbol = 'define-macro'    type = type_syntax value   = 'define-macro' ).
      define_value( symbol = 'define-syntax'   type = type_syntax value   = 'define-syntax' ).
      define_value( symbol = 'macroexpand'     type = type_syntax value   = 'macroexpand' ).
      define_value( symbol = 'gensym'          type = type_syntax value   = 'gensym' ).
      define_value( symbol = 'case-lambda'     type = type_syntax value   = 'case-lambda' ).
      define_value( symbol = 'parameterize'    type = type_syntax value   = 'parameterize' ).

      define_value( symbol = 'and'      type = type_syntax value   = 'and' ).
      define_value( symbol = 'or'       type = type_syntax value   = 'or' ).
      define_value( symbol = 'cond'     type = type_syntax value   = 'cond' ).
      define_value( symbol = 'unless'   type = type_syntax value   = 'unless' ).
      define_value( symbol = 'when'     type = type_syntax value   = 'when' ).
      define_value( symbol = 'begin'    type = type_syntax value   = 'begin' ).
      define_value( symbol = 'let'      type = type_syntax value   = 'let' ).
      define_value( symbol = 'let*'     type = type_syntax value   = 'let*' ).
      define_value( symbol = 'letrec'   type = type_syntax value   = 'letrec' ).
      define_value( symbol = 'letrec*'  type = type_syntax value   = 'letrec*' ).
      define_value( symbol = 'do'       type = type_syntax value   = 'do' ).
      define_value( symbol = 'case'     type = type_syntax value   = 'case' ).

      define_value( symbol = c_eval_unquote          type = type_syntax value   = ',' ).
      define_value( symbol = c_eval_unquote_splicing type = type_syntax value   = ',@' ).

*     Procedures
      define_value( symbol = 'apply'        type = type_primitive value   = 'apply' ).
      define_value( symbol = 'for-each'     type = type_primitive value   = 'for-each' ).
      define_value( symbol = 'map'          type = type_primitive value   = 'map' ).

*     Add native functions to environment
      define_value( symbol = '+'        type = type_native value   = 'PROC_ADD' ).
      define_value( symbol = '-'        type = type_native value   = 'PROC_SUBTRACT' ).
      define_value( symbol = '*'        type = type_native value   = 'PROC_MULTIPLY' ).
      define_value( symbol = '/'        type = type_native value   = 'PROC_DIVIDE' ).
      define_value( symbol = c_eval_append type = type_native value   = 'PROC_APPEND' ).
      define_value( symbol = 'append!'     type = type_native value   = 'PROC_APPEND_UNSAFE' ).
      define_value( symbol = 'list'     type = type_native value   = 'PROC_LIST' ).
      define_value( symbol = 'length'   type = type_native value   = 'PROC_LENGTH' ).
      define_value( symbol = 'reverse'  type = type_native value   = 'PROC_REVERSE' ).
      define_value( symbol = 'not'      type = type_native value   = 'PROC_NOT' ).

      define_value( symbol = 'values'   type = type_native value   = 'PROC_VALUES' ).

      define_value( symbol = 'make-list'    type = type_native value   = 'PROC_MAKE_LIST' ).
      define_value( symbol = 'list-tail'    type = type_native value   = 'PROC_LIST_TAIL' ).
      define_value( symbol = 'list-ref'     type = type_native value   = 'PROC_LIST_REF' ).
      define_value( symbol = 'list-copy'    type = type_native value   = 'PROC_LIST_COPY' ).
      define_value( symbol = 'list->vector' type = type_native value   = 'PROC_LIST_TO_VECTOR' ).
      define_value( symbol = 'iota'         type = type_native value   = 'PROC_IOTA' ).

      define_value( symbol = 'memq'    type = type_native value   = 'PROC_MEMQ' ).
      define_value( symbol = 'memv'    type = type_native value   = 'PROC_MEMV' ).
      define_value( symbol = 'member'  type = type_native value   = 'PROC_MEMBER' ).

      define_value( symbol = 'assq'    type = type_native value   = 'PROC_ASSQ' ).
      define_value( symbol = 'assv'    type = type_native value   = 'PROC_ASSV' ).
      define_value( symbol = 'assoc'   type = type_native value   = 'PROC_ASSOC' ).

      define_value( symbol = 'car'     type = type_native value   = 'PROC_CAR' ).
      define_value( symbol = 'cdr'     type = type_native value   = 'PROC_CDR' ).
      define_value( symbol = c_eval_cons    type = type_native value   = 'PROC_CONS' ).
      define_value( symbol = 'nil?'    type = type_native value   = 'PROC_NILP' ).
      define_value( symbol = 'null?'   type = type_native value   = 'PROC_NILP' ).

      define_value( symbol = '>'       type = type_native value   = 'PROC_GT' ).
      define_value( symbol = '>='      type = type_native value   = 'PROC_GTE' ).
      define_value( symbol = '<'       type = type_native value   = 'PROC_LT' ).
      define_value( symbol = '<='      type = type_native value   = 'PROC_LTE' ).
      define_value( symbol = '='       type = type_native value   = 'PROC_EQL' ). "Math equal
      define_value( symbol = 'eq?'     type = type_native value   = 'PROC_EQ' ).
      define_value( symbol = 'eqv?'    type = type_native value   = 'PROC_EQV' ).
      define_value( symbol = 'equal?'  type = type_native value   = 'PROC_EQUAL' ).

      define_value( symbol = 'set-car!' type = type_native value   = 'PROC_SET_CAR' ).
      define_value( symbol = 'set-cdr!' type = type_native value   = 'PROC_SET_CDR' ).

      define_value( symbol = 'caar'     type = type_native value   = 'PROC_CAAR' ).
      define_value( symbol = 'cadr'     type = type_native value   = 'PROC_CADR' ).
      define_value( symbol = 'cdar'     type = type_native value   = 'PROC_CDAR' ).
      define_value( symbol = 'cddr'     type = type_native value   = 'PROC_CDDR' ).
      define_value( symbol = 'caaar'    type = type_native value   = 'PROC_CAAAR' ).
      define_value( symbol = 'cdaar'    type = type_native value   = 'PROC_CDAAR' ).
      define_value( symbol = 'caadr'    type = type_native value   = 'PROC_CAADR' ).
      define_value( symbol = 'cdadr'    type = type_native value   = 'PROC_CDADR' ).
      define_value( symbol = 'cadar'    type = type_native value   = 'PROC_CADAR' ).
      define_value( symbol = 'cddar'    type = type_native value   = 'PROC_CDDAR' ).
      define_value( symbol = 'caddr'    type = type_native value   = 'PROC_CADDR' ).
      define_value( symbol = 'cdddr'    type = type_native value   = 'PROC_CDDDR' ).

      define_value( symbol = 'caaaar'    type = type_native value   = 'PROC_CAAAAR' ).
      define_value( symbol = 'cdaaar'    type = type_native value   = 'PROC_CDAAAR' ).
      define_value( symbol = 'cadaar'    type = type_native value   = 'PROC_CADAAR' ).
      define_value( symbol = 'cddaar'    type = type_native value   = 'PROC_CDDAAR' ).
      define_value( symbol = 'caaadr'    type = type_native value   = 'PROC_CAAADR' ).
      define_value( symbol = 'cdaadr'    type = type_native value   = 'PROC_CDAADR' ).
      define_value( symbol = 'cadadr'    type = type_native value   = 'PROC_CADADR' ).
      define_value( symbol = 'cddadr'    type = type_native value   = 'PROC_CDDADR' ).
      define_value( symbol = 'caadar'    type = type_native value   = 'PROC_CAADAR' ).
      define_value( symbol = 'cdadar'    type = type_native value   = 'PROC_CDADAR' ).
      define_value( symbol = 'caddar'    type = type_native value   = 'PROC_CADDAR' ).
      define_value( symbol = 'cdddar'    type = type_native value   = 'PROC_CDDDAR' ).
      define_value( symbol = 'caaddr'    type = type_native value   = 'PROC_CAADDR' ).
      define_value( symbol = 'caaddr'    type = type_native value   = 'PROC_CAADDR' ).
      define_value( symbol = 'cadddr'    type = type_native value   = 'PROC_CADDDR' ).
      define_value( symbol = 'cddddr'    type = type_native value   = 'PROC_CDDDDR' ).


      define_value( symbol = 'make-parameter'    type = type_native value = 'PROC_MAKE_PARAMETER' ).

      define_value( symbol = 'current-input-port'  type = type_native value = 'PROC_CURRENT_INPUT_PORT' ).
      define_value( symbol = 'current-output-port' type = type_native value = 'PROC_CURRENT_OUTPUT_PORT' ).
      define_value( symbol = 'current-error-port'  type = type_native value = 'PROC_CURRENT_ERROR_PORT' ).

      define_value( symbol = 'close-input-port'  type = type_native value = 'PROC_CLOSE_INPUT_PORT' ).
      define_value( symbol = 'close-output-port' type = type_native value = 'PROC_CLOSE_OUTPUT_PORT' ).
      define_value( symbol = 'close-port'        type = type_native value = 'PROC_CLOSE_PORT' ).

*     vector-related functions
      define_value( symbol = 'vector'        type = type_native value   = 'PROC_VECTOR' ).
      define_value( symbol = 'vector-length' type = type_native value   = 'PROC_VECTOR_LENGTH' ).
      define_value( symbol = 'vector-set!'   type = type_native value   = 'PROC_VECTOR_SET' ).
      define_value( symbol = 'vector-fill!'  type = type_native value   = 'PROC_VECTOR_FILL' ).
      define_value( symbol = 'vector-ref'    type = type_native value   = 'PROC_VECTOR_REF' ).
      define_value( symbol = 'vector->list'  type = type_native value   = 'PROC_VECTOR_TO_LIST' ).
      define_value( symbol = 'make-vector'   type = type_native value   = 'PROC_MAKE_VECTOR' ).

*     bytevector-related functions
      define_value( symbol = 'bytevector'           type = type_native value   = 'PROC_BYTEVECTOR' ).
      define_value( symbol = 'bytevector-length'    type = type_native value   = 'PROC_BYTEVECTOR_LENGTH' ).
      define_value( symbol = 'bytevector-u8-set!'   type = type_native value   = 'PROC_BYTEVECTOR_U8_SET' ).
      define_value( symbol = 'bytevector-u8-ref'    type = type_native value   = 'PROC_BYTEVECTOR_U8_REF' ).
      define_value( symbol = 'bytevector-append'    type = type_native value   = 'PROC_BYTEVECTOR_APPEND' ).
      define_value( symbol = 'bytevector-copy'      type = type_native value   = 'PROC_BYTEVECTOR_NEW_COPY' ).
      define_value( symbol = 'bytevector-copy!'     type = type_native value   = 'PROC_BYTEVECTOR_COPY' ).
      define_value( symbol = 'utf8->string'         type = type_native value   = 'PROC_UTF8_TO_STRING' ).
      define_value( symbol = 'string->utf8'         type = type_native value   = 'PROC_STRING_TO_UTF8' ).
      define_value( symbol = 'make-bytevector'      type = type_native value   = 'PROC_MAKE_BYTEVECTOR' ).

*     Hash-related functions
      define_value( symbol = 'make-hash'   type = type_native value   = 'PROC_MAKE_HASH' ).
      define_value( symbol = 'hash-get'    type = type_native value   = 'PROC_HASH_GET' ).
      define_value( symbol = 'hash-insert' type = type_native value   = 'PROC_HASH_INSERT' ).
      define_value( symbol = 'hash-remove' type = type_native value   = 'PROC_HASH_REMOVE' ).
      define_value( symbol = 'hash-keys'   type = type_native value   = 'PROC_HASH_KEYS' ).
*     Functions for type:
      define_value( symbol = 'string?'     type = type_native value = 'PROC_IS_STRING' ).
      define_value( symbol = 'char?'       type = type_native value = 'PROC_IS_CHAR' ).
      define_value( symbol = 'hash?'       type = type_native value = 'PROC_IS_HASH' ).
      define_value( symbol = 'number?'     type = type_native value = 'PROC_IS_NUMBER' ).
      define_value( symbol = 'exact-integer?'    type = type_native value = 'PROC_IS_EXACT_INTEGER' ).
      define_value( symbol = 'integer?'    type = type_native value = 'PROC_IS_INTEGER' ).
      define_value( symbol = 'complex?'    type = type_native value = 'PROC_IS_COMPLEX' ).
      define_value( symbol = 'real?'       type = type_native value = 'PROC_IS_REAL' ).
      define_value( symbol = 'rational?'   type = type_native value = 'PROC_IS_RATIONAL' ).
      define_value( symbol = 'list?'       type = type_native value = 'PROC_IS_LIST' ).
      define_value( symbol = 'pair?'       type = type_native value = 'PROC_IS_PAIR' ).
      define_value( symbol = 'vector?'     type = type_native value = 'PROC_IS_VECTOR' ).
      define_value( symbol = 'bytevector?' type = type_native value = 'PROC_IS_BYTEVECTOR' ).
      define_value( symbol = 'boolean?'    type = type_native value = 'PROC_IS_BOOLEAN' ).
      define_value( symbol = 'alist?'      type = type_native value = 'PROC_IS_ALIST' ).
      define_value( symbol = 'procedure?'  type = type_native value = 'PROC_IS_PROCEDURE' ).
      define_value( symbol = 'symbol?'     type = type_native value = 'PROC_IS_SYMBOL' ).
      define_value( symbol = 'port?'       type = type_native value = 'PROC_IS_PORT' ).
      define_value( symbol = 'boolean=?'   type = type_native value = 'PROC_BOOLEAN_LIST_IS_EQUAL' ).
      define_value( symbol = 'exact?'      type = type_native value = 'PROC_IS_EXACT' ).
      define_value( symbol = 'inexact?'    type = type_native value = 'PROC_IS_INEXACT' ).

*     Format
      define_value( symbol = 'newline'     type = type_native value = 'PROC_NEWLINE' ).
      define_value( symbol = 'write'       type = type_native value = 'PROC_WRITE' ).
      define_value( symbol = 'display'     type = type_native value = 'PROC_DISPLAY' ).

      define_value( symbol = 'read'         type = type_native value = 'PROC_READ' ).
      define_value( symbol = 'write-string' type = type_native value = 'PROC_WRITE_STRING' ).
      define_value( symbol = 'write-char'   type = type_native value = 'PROC_WRITE_CHAR' ).
      define_value( symbol = 'read-char'    type = type_native value = 'PROC_READ_CHAR' ).
      define_value( symbol = 'read-string'  type = type_native value = 'PROC_READ_STRING' ).
      define_value( symbol = 'char-ready?'  type = type_native value = 'PROC_IS_CHAR_READY' ).
      define_value( symbol = 'peek-char'    type = type_native value = 'PROC_PEEK_CHAR' ).

      define_value( symbol = 'exact'          type = type_native value = 'PROC_TO_EXACT' ).
      define_value( symbol = 'inexact'        type = type_native value = 'PROC_TO_INEXACT' ).

      define_value( symbol = 'number->string' type = type_native value = 'PROC_NUM_TO_STRING' ).
      define_value( symbol = 'string->number' type = type_native value = 'PROC_STRING_TO_NUM' ).
      define_value( symbol = 'make-string'    type = type_native value = 'PROC_MAKE_STRING' ).
      define_value( symbol = 'string'         type = type_native value = 'PROC_STRING' ).
      define_value( symbol = 'string->list'   type = type_native value = 'PROC_STRING_TO_LIST' ).
      define_value( symbol = 'list->string'   type = type_native value = 'PROC_LIST_TO_STRING' ).
      define_value( symbol = 'symbol->string' type = type_native value = 'PROC_SYMBOL_TO_STRING' ).
      define_value( symbol = 'string->symbol' type = type_native value = 'PROC_STRING_TO_SYMBOL' ).
      define_value( symbol = 'string-append'  type = type_native value = 'PROC_STRING_APPEND' ).
      define_value( symbol = 'string-length'  type = type_native value = 'PROC_STRING_LENGTH' ).
      define_value( symbol = 'string-copy'    type = type_native value = 'PROC_STRING_COPY' ).
      define_value( symbol = 'substring'      type = type_native value = 'PROC_STRING_COPY' ).
      define_value( symbol = 'string-ref'     type = type_native value = 'PROC_STRING_REF' ).
      define_value( symbol = 'string-set!'    type = type_native value = 'PROC_STRING_SET' ).

      define_value( symbol = 'string=?'     type = type_native value   = 'PROC_STRING_LIST_IS_EQ' ).
      define_value( symbol = 'string<?'     type = type_native value   = 'PROC_STRING_LIST_IS_LT' ).
      define_value( symbol = 'string>?'     type = type_native value   = 'PROC_STRING_LIST_IS_GT' ).
      define_value( symbol = 'string<=?'    type = type_native value   = 'PROC_STRING_LIST_IS_LE' ).
      define_value( symbol = 'string>=?'    type = type_native value   = 'PROC_STRING_LIST_IS_GE' ).

      define_value( symbol = 'string-ci=?'     type = type_native value   = 'PROC_STRING_CI_LIST_IS_EQ' ).
      define_value( symbol = 'string-ci<?'     type = type_native value   = 'PROC_STRING_CI_LIST_IS_LT' ).
      define_value( symbol = 'string-ci>?'     type = type_native value   = 'PROC_STRING_CI_LIST_IS_GT' ).
      define_value( symbol = 'string-ci<=?'    type = type_native value   = 'PROC_STRING_CI_LIST_IS_LE' ).
      define_value( symbol = 'string-ci>=?'    type = type_native value   = 'PROC_STRING_CI_LIST_IS_GE' ).

*     Math
      define_value( symbol = 'abs'   type = type_native value = 'PROC_ABS' ).
      define_value( symbol = 'sin'   type = type_native value = 'PROC_SIN' ).
      define_value( symbol = 'cos'   type = type_native value = 'PROC_COS' ).
      define_value( symbol = 'tan'   type = type_native value = 'PROC_TAN' ).
      define_value( symbol = 'asin'  type = type_native value = 'PROC_ASIN' ).
      define_value( symbol = 'acos'  type = type_native value = 'PROC_ACOS' ).
      define_value( symbol = 'atan'  type = type_native value = 'PROC_ATAN' ).
      define_value( symbol = 'sinh'  type = type_native value = 'PROC_SINH' ).
      define_value( symbol = 'cosh'  type = type_native value = 'PROC_COSH' ).
      define_value( symbol = 'tanh'  type = type_native value = 'PROC_TANH' ).
      define_value( symbol = 'asinh' type = type_native value = 'PROC_ASINH' ).
      define_value( symbol = 'acosh' type = type_native value = 'PROC_ACOSH' ).
      define_value( symbol = 'atanh' type = type_native value = 'PROC_ATANH' ).
      define_value( symbol = 'expt'  type = type_native value = 'PROC_EXPT' ).
      define_value( symbol = 'exp'   type = type_native value = 'PROC_EXP' ).
      define_value( symbol = 'log'   type = type_native value = 'PROC_LOG' ).
      define_value( symbol = 'sqrt'  type = type_native value = 'PROC_SQRT' ).

      define_value( symbol = 'square'             type = type_native value = 'PROC_SQUARE' ).
      define_value( symbol = 'exact-integer-sqrt' type = type_native value = 'PROC_INT_SQRT' ).
      define_value( symbol = 'floor-quotient'     type = type_native value = 'PROC_FLOOR_QUOTIENT' ).
      define_value( symbol = 'floor-remainder'    type = type_native value = 'PROC_FLOOR_REMAINDER' ).
      define_value( symbol = 'truncate-remainder' type = type_native value = 'PROC_TRUNC_REMAINDER' ).
      define_value( symbol = 'truncate-quotient'  type = type_native value = 'PROC_TRUNC_QUOTIENT' ).

      " not implemented yet
      define_value( symbol = 'rationalize'        type = type_native value = 'PROC_RATIONALIZE' ).

      define_value( symbol = 'floor'     type = type_native value = 'PROC_FLOOR' ).
      define_value( symbol = 'floor/'    type = type_native value = 'PROC_FLOOR_NEW' ).
      define_value( symbol = 'ceiling'   type = type_native value = 'PROC_CEILING' ).
      define_value( symbol = 'truncate'  type = type_native value = 'PROC_TRUNCATE' ).
      define_value( symbol = 'truncate/' type = type_native value = 'PROC_TRUNCATE_NEW' ).
      define_value( symbol = 'round'     type = type_native value = 'PROC_ROUND' ).

      define_value( symbol = 'numerator'   type = type_native value = 'PROC_NUMERATOR' ).
      define_value( symbol = 'denominator' type = type_native value = 'PROC_DENOMINATOR' ).
      define_value( symbol = 'remainder' type = type_native value = 'PROC_REMAINDER' ).
      define_value( symbol = 'modulo'    type = type_native value = 'PROC_MODULO' ).
      define_value( symbol = 'quotient'  type = type_native value = 'PROC_QUOTIENT' ).
      define_value( symbol = 'random'    type = type_native value = 'PROC_RANDOM' ).
      define_value( symbol = 'max'       type = type_native value = 'PROC_MAX' ).
      define_value( symbol = 'min'       type = type_native value = 'PROC_MIN' ).
      define_value( symbol = 'gcd'       type = type_native value = 'PROC_GCD' ).
      define_value( symbol = 'lcm'       type = type_native value = 'PROC_LCM' ).

      define_value( symbol = 'zero?'     type = type_native value = 'PROC_IS_ZERO' ).
      define_value( symbol = 'positive?' type = type_native value = 'PROC_IS_POSITIVE' ).
      define_value( symbol = 'negative?' type = type_native value = 'PROC_IS_NEGATIVE' ).
      define_value( symbol = 'odd?'      type = type_native value = 'PROC_IS_ODD' ).
      define_value( symbol = 'even?'     type = type_native value = 'PROC_IS_EVEN' ).
*     Continuation
      define_value( symbol = 'call-with-current-continuation' type = type_native value = 'PROC_CALL_CC' ).
      define_value( symbol = 'call/cc'                        type = type_native value = 'PROC_CALL_CC' ).

*     Native functions for ABAP integration
      define_value( symbol = 'ab-data'       type = type_native value   = 'PROC_ABAP_DATA' ).
      define_value( symbol = 'ab-function'   type = type_native value   = 'PROC_ABAP_FUNCTION' ).
      define_value( symbol = 'ab-func-param' type = type_native value   = 'PROC_ABAP_FUNCTION_PARAM' ).
      define_value( symbol = 'ab-table'      type = type_native value   = 'PROC_ABAP_TABLE' ).
      define_value( symbol = 'ab-append-row' type = type_native value   = 'PROC_ABAP_APPEND_ROW' ).
      define_value( symbol = 'ab-delete-row' type = type_native value   = 'PROC_ABAP_DELETE_ROW' ).
      define_value( symbol = 'ab-get-row'    type = type_native value   = 'PROC_ABAP_GET_ROW' ).
      define_value( symbol = 'ab-get-value'  type = type_native value   = 'PROC_ABAP_GET_VALUE' ).
      define_value( symbol = 'ab-set-value'  type = type_native value   = 'PROC_ABAP_SET_VALUE' ).

      define_value( symbol = 'ab-get' type = type_native value = 'PROC_ABAP_GET' ).
      define_value( symbol = 'ab-set' type = type_native value = 'PROC_ABAP_SET' ).

*     Compatibility
      define_value( symbol = 'empty?'  type = type_native value   = 'PROC_NILP' ).
      define_value( symbol = 'first'   type = type_native value   = 'PROC_CAR' ).
      define_value( symbol = 'rest'    type = type_native value   = 'PROC_CDR' ).

*     Errors
      define_value( symbol = 'raise'   type = type_native value   = 'PROC_RAISE' ).
      define_value( symbol = 'error'   type = type_native value   = 'PROC_ERROR' ).

*     Ports
      define_value( symbol = 'input-port?'         type = type_native value   = 'PROC_IS_INPUT_PORT' ).
      define_value( symbol = 'output-port?'        type = type_native value   = 'PROC_IS_OUTPUT_PORT' ).
      define_value( symbol = 'textual-port?'       type = type_native value   = 'PROC_IS_TEXTUAL_PORT' ).
      define_value( symbol = 'binary-port?'        type = type_native value   = 'PROC_IS_BINARY_PORT' ).
      define_value( symbol = 'input-port-open?'    type = type_native value   = 'PROC_IS_OPEN_INPUT_PORT' ).
      define_value( symbol = 'output-port-open?'   type = type_native value   = 'PROC_IS_OPEN_OUTPUT_PORT' ).
      define_value( symbol = 'eof-object?'         type = type_native value   = 'PROC_IS_EOF_OBJECT' ).
      define_value( symbol = 'open-output-string'  type = type_native value   = 'PROC_OPEN_OUTPUT_STRING' ).
      define_value( symbol = 'open-input-string'   type = type_native value   = 'PROC_OPEN_INPUT_STRING' ).
      define_value( symbol = 'get-output-string'   type = type_native value   = 'PROC_GET_OUTPUT_STRING' ).
      define_value( symbol = 'eof-object'          type = type_native value   = 'PROC_EOF_OBJECT' ).

      define_value( symbol = 'char-alphabetic?'  type = type_native value   = 'PROC_IS_CHAR_ALPHABETIC' ).
      define_value( symbol = 'char-numeric?'     type = type_native value   = 'PROC_IS_CHAR_NUMERIC' ).
      define_value( symbol = 'char-whitespace?'  type = type_native value   = 'PROC_IS_CHAR_WHITESPACE' ).
      define_value( symbol = 'char-upper-case?'  type = type_native value   = 'PROC_IS_CHAR_UPPER_CASE' ).
      define_value( symbol = 'char-lower-case?'  type = type_native value   = 'PROC_IS_CHAR_LOWER_CASE' ).

      define_value( symbol = 'digit-value'       type = type_native value   = 'PROC_DIGIT_VALUE' ).
      define_value( symbol = 'char->integer'     type = type_native value   = 'PROC_CHAR_TO_INTEGER' ).
      define_value( symbol = 'integer->char'     type = type_native value   = 'PROC_INTEGER_TO_CHAR' ).
      define_value( symbol = 'char-upcase'       type = type_native value   = 'PROC_CHAR_UPCASE' ).
      define_value( symbol = 'char-downcase'     type = type_native value   = 'PROC_CHAR_DOWNCASE' ).

      define_value( symbol = 'char=?'     type = type_native value   = 'PROC_CHAR_LIST_IS_EQ' ).
      define_value( symbol = 'char<?'     type = type_native value   = 'PROC_CHAR_LIST_IS_LT' ).
      define_value( symbol = 'char>?'     type = type_native value   = 'PROC_CHAR_LIST_IS_GT' ).
      define_value( symbol = 'char<=?'    type = type_native value   = 'PROC_CHAR_LIST_IS_LE' ).
      define_value( symbol = 'char>=?'    type = type_native value   = 'PROC_CHAR_LIST_IS_GE' ).

      define_value( symbol = 'char-ci=?'     type = type_native value   = 'PROC_CHAR_CI_LIST_IS_EQ' ).
      define_value( symbol = 'char-ci<?'     type = type_native value   = 'PROC_CHAR_CI_LIST_IS_LT' ).
      define_value( symbol = 'char-ci>?'     type = type_native value   = 'PROC_CHAR_CI_LIST_IS_GT' ).
      define_value( symbol = 'char-ci<=?'    type = type_native value   = 'PROC_CHAR_CI_LIST_IS_LE' ).
      define_value( symbol = 'char-ci>=?'    type = type_native value   = 'PROC_CHAR_CI_LIST_IS_GE' ).

      define_value( symbol = 'sql-query'         type = type_native value   = 'PROC_SQL_QUERY' ).
      define_value( symbol = 'define-query'      type = type_native value   = 'PROC_SQL_PREPARE' ).

      define_value( symbol = 'turtles'       type = type_native value   = 'PROC_TURTLE_NEW' ).
      define_value( symbol = 'turtles?'      type = type_native value   = 'PROC_TURTLE_EXIST' ).
      define_value( symbol = 'move'          type = type_native value   = 'PROC_TURTLE_MOVE' ).
      define_value( symbol = 'draw'          type = type_native value   = 'PROC_TURTLE_DRAW' ).
      define_value( symbol = 'erase'         type = type_native value   = 'PROC_TURTLE_ERASE' ).
      define_value( symbol = 'move-offset'   type = type_native value   = 'PROC_TURTLE_MOVE_OFFSET' ).
      define_value( symbol = 'draw-offset'   type = type_native value   = 'PROC_TURTLE_DRAW_OFFSET' ).
      define_value( symbol = 'erase-offset'  type = type_native value   = 'PROC_TURTLE_ERASE_OFFSET' ).
      define_value( symbol = 'turn'          type = type_native value   = 'PROC_TURTLE_TURN_DEGREES' ).
      define_value( symbol = 'turn/radians'  type = type_native value   = 'PROC_TURTLE_TURN_RADIANS' ).
      define_value( symbol = 'set-pen-width' type = type_native value   = 'PROC_TURTLE_SET_PEN_WIDTH' ).
      define_value( symbol = 'set-pen-color' type = type_native value   = 'PROC_TURTLE_SET_PEN_COLOR' ).

      define_value( symbol = 'merge'             type = type_native value = 'PROC_TURTLE_MERGE' ).
      define_value( symbol = 'clean'             type = type_native value = 'PROC_TURTLE_CLEAN' ).
      define_value( symbol = 'turtle-state'      type = type_native value = 'PROC_TURTLE_STATE' ).
      define_value( symbol = 'turtles-height'    type = type_native value = 'PROC_TURTLE_HEIGHT' ).
      define_value( symbol = 'turtles-width'     type = type_native value = 'PROC_TURTLE_WIDTH' ).
      define_value( symbol = 'turtles-pen-color' type = type_native value = 'PROC_TURTLE_PEN_COLOR' ).
      define_value( symbol = 'turtles-pen-width' type = type_native value = 'PROC_TURTLE_PEN_WIDTH' ).
      define_value( symbol = 'regular-poly'      type = type_native value = 'PROC_TURTLE_REGULAR_POLY' ).
      define_value( symbol = 'regular-polys'     type = type_native value = 'PROC_TURTLE_REGULAR_POLYS' ).

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

      char_alarm = lcl_lisp_new=>charx( '0007' ).
      char_backspace = lcl_lisp_new=>charx( '0008' ).
      char_delete = lcl_lisp_new=>charx( '007F' ).
      char_escape = lcl_lisp_new=>charx( '001B' ).
      char_newline = lcl_lisp_new=>charx( '000A' ).
      char_null = lcl_lisp_new=>charx( '0000' ).
      char_return = lcl_lisp_new=>charx( '000D' ).
      char_space = lcl_lisp_new=>char( ` ` ).
      char_tab = lcl_lisp_new=>charx( '0009' ).

      new_line = lcl_lisp_new=>string( |\n| ).
      eof_object = lcl_lisp_new=>char( c_lisp_eof ).
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

      result = false.

      DATA(b) = io_elem.
*     Object a and Object b are both #t or both #f or both the empty list.
      IF ( me EQ true AND b EQ true )
        OR ( me EQ false AND b EQ false )
        OR ( me EQ nil AND b EQ nil ).
        result = true.
        RETURN.
      ENDIF.

      CHECK type EQ b->type.

      CASE type.
        WHEN type_integer.
* obj1 and obj2 are both exact numbers and are numerically equal (in the sense of =).
          lo_int ?= b.
          DATA(lo_me_int) = CAST lcl_lisp_integer( me ).
          CHECK lo_me_int->int = lo_int->int
            AND lo_me_int->exact = lo_int->exact.

        WHEN type_rational.
          lo_rat ?= b.
          DATA(lo_me_rat) = CAST lcl_lisp_rational( me ).
          CHECK lo_me_rat->int = lo_rat->int
            AND lo_me_rat->denominator = lo_rat->denominator
            AND lo_me_rat->exact = lo_rat->exact.

        WHEN type_real.
*obj1 and obj2 are both inexact numbers such that they are numerically equal (in the sense of =)
*and they yield the same results (in the sense of eqv?) when passed as arguments to any other
*procedure that can be defined as a finite composition of Scheme’s standard arithmetic procedures,
*provided it does not result in a NaN value.
          lo_real ?= b.
          DATA(lo_me_real) = CAST lcl_lisp_real( me ).
          CHECK lo_me_real->float_eq( lo_real->real )
            AND lo_me_real->exact = lo_real->exact.

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
      result = true.
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

      IF comp NE nil.
        DATA(lo_lambda) = comp->car.
        DATA(lo_env) = lo_lambda->environment.
        IF lo_env IS NOT BOUND.
          lo_env = environment.
        ENDIF.
        DATA(lo_head) = lcl_lisp_new=>list3( io_first = lo_lambda
                                             io_second = lcl_lisp_new=>quote( me )
                                             io_third = lcl_lisp_new=>quote( io_elem ) ).

        result = interpreter->eval( VALUE #( elem = lo_head
                                             env = lo_env ) ).
        RETURN.
      ENDIF.

      result = false.

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
              result = true.
              RETURN.
            ELSEIF lo_a->car->is_equal( lo_b->car ) EQ false.
              RETURN.
            ENDIF.
            lo_slow_a = lo_slow_a->cdr.
            lo_a = lo_a->cdr.
            lo_slow_b = lo_slow_b->cdr.
            lo_b = lo_b->cdr.
            CHECK lo_a->type EQ type_pair AND lo_b->type EQ type_pair.
            IF lo_a->car->is_equal( lo_b->car ) EQ false.
              RETURN.
            ENDIF.
            lo_a = lo_a->cdr.
            lo_b = lo_b->cdr.
            CHECK lo_slow_a EQ lo_a AND lo_slow_b EQ lo_b.
*           Circular list
            result = true.
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
          result = true.

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
            ev_str = ev_str && `'`.
            ev_skip = abap_true.

          WHEN c_eval_quasiquote OR '`'.
            ev_str = ev_str && '`'.
            ev_skip = abap_true.

          WHEN c_eval_unquote OR ','.
            ev_str = ev_str && ','.
            ev_skip = abap_true.

          WHEN c_eval_unquote_splicing OR ',@'.
            ev_str = ev_str && ',@'.
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
          str = str && ` ` && lv_str.
        ENDIF.

        lo_elem = lo_elem->cdr.
      ENDWHILE.

    ENDMETHOD.

    METHOD list_to_string.
      DATA lv_str TYPE string.
      DATA lv_skip TYPE flag.
      DATA lv_parens TYPE flag.
      DATA lv_first TYPE flag VALUE abap_true.
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
              lv_str = |{ lv_str } { lo_elem->car->to_string( ) }|.
              lo_elem = lo_elem->cdr.
            ENDWHILE.
            lv_str = |{ lv_str } { lo_elem->car->to_string( ) } . #{ lv_shared }#|.
            EXIT.
          ENDIF.

        ENDIF.

        IF lv_skip EQ abap_false.
          lv_str = lv_str && COND string( WHEN lo_elem->type NE type_pair     " If item is not a cons cell
                                          THEN | . { lo_elem->to_string( ) }|      " indicate with dot notation:
                                          ELSE | { lo_elem->car->to_string( ) }| ).
        ENDIF.

        lo_elem = lo_elem->cdr.

      ENDWHILE.

      IF lv_parens EQ abap_true.
        str = |{ lcl_parser=>c_open_paren }{ lv_str } { lcl_parser=>c_close_paren }|.
      ELSE.
*       Quasiquoting output
        str = lv_str.
      ENDIF.

      IF lv_shared GE 0.
        str = |#{ lv_shared } = { str }|.
      ENDIF.

    ENDMETHOD.                    "list_to_string

    METHOD to_string.
      CASE type.
        WHEN type_lambda.
          str = |<lambda> { car->list_to_string( ) }|.
        WHEN type_null.
          str = c_lisp_nil.

        WHEN type_syntax
          OR type_primitive
          OR type_symbol
          OR type_boolean.
          str = value.

        WHEN type_not_defined.
          str = c_undefined.

        WHEN type_string.
          IF me EQ lcl_lisp=>new_line.
            str = |\n|.
          ELSE.
            " give back the string as a quoted string
            str = |"{ escape( val = value format = cl_abap_format=>e_html_js ) }"|.
          ENDIF.

        WHEN type_char.
          CASE me.
            WHEN lcl_lisp=>new_line.
              str = |\n|.
            WHEN lcl_lisp=>eof_object.
              str = '<eof>'.
            WHEN OTHERS.
              str = |"{ escape( val = value format = cl_abap_format=>e_html_js ) }"|.
          ENDCASE.

" A numerical constant can be specified to be either exact or inexact by a prefix #e for exact and #i for inexact.
" An exactness prefix can appear before or after any radix prefix that is used.
" If the written representation of a number has no exactness prefix, the constant is inexact if it contains a
" decimal point or an exponent. Otherwise, it is exact.

        WHEN type_integer.
          DATA lv_real TYPE tv_real.
          DATA lo_int TYPE REF TO lcl_lisp_integer.

          lo_int ?= me.
          lv_real = lo_int->int.
          str = lv_real.
          str = condense( str ).

          IF lo_int->exact EQ abap_false AND str CN `.`.
            str = str && `.0`.
          ENDIF.

        WHEN type_real.
          DATA lo_real TYPE REF TO lcl_lisp_real.
          lo_real ?= me.

          DATA(lv_float) = lo_real->real.
          str = condense( CAST lcl_lisp_real( me )->real ).
          "str = condense( CONV #( lo_real->real ) ).

          IF lo_real->exact EQ abap_false AND frac( lv_float ) EQ 0 AND str CN `.e`.
            str = str && `.0`.
          ENDIF.

*        WHEN type_rational.

*        WHEN type_complex.
*          str = ?.

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

        "WHEN type_bytevector.

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
            WHEN lcl_lisp=>new_line.
              str = |\n|.
            WHEN lcl_lisp=>eof_object.
              str = space.
            WHEN OTHERS.
              str = value.
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
          area    = c_area_eval.
    ENDMETHOD.                    "eval_err

    METHOD raise.
      throw( context && to_string( ) && message ).
    ENDMETHOD.

    METHOD raise_NaN.
      throw( to_string( ) && | is not a number in [{ operation }]| ).
    ENDMETHOD.

    METHOD assert_last_param.
      CHECK cdr NE nil.
      throw( to_string( ) && | Parameter mismatch| ).
    ENDMETHOD.

    METHOD error_not_a_pair.
      raise( context = context
             message = ` is not a pair` ).
    ENDMETHOD.

    METHOD is_procedure.
      CASE type.
        WHEN type_lambda
          OR type_native
          OR type_primitive
          OR type_abap_function.  " really?
          result = true.
        WHEN OTHERS.
          result = false.
      ENDCASE.
    ENDMETHOD.

    METHOD is_number.
      CASE type.
        WHEN type_integer
          OR type_rational
          OR type_real
          OR type_complex.
          result = true.
        WHEN OTHERS.
          result = false.
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
          lcl_lisp=>throw( |{ elem->to_string( ) } is not a proper list| ).
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
                          exact = abap_false ).

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
            EXPORTING type = type.
          ro_elem->value = value.

      ENDCASE.
    ENDMETHOD.

    METHOD atom.
      CASE value.
        WHEN space.
          ro_elem = lcl_lisp=>nil.  " or EOF_OBJECT?

        WHEN lcl_lisp=>true->value.
          ro_elem = lcl_lisp=>true.

        WHEN lcl_lisp=>false->value.
          ro_elem = lcl_lisp=>false.

        WHEN OTHERS.
          TRY.
              ro_elem = number( value = value ).
            CATCH cx_sy_conversion_no_number.
*             otherwise treat it as a symbol
              ro_elem = symbol( value ).
          ENDTRY.
      ENDCASE.
    ENDMETHOD.                    "new_atom

    METHOD string.
      ro_elem = NEW lcl_lisp_string( value = value
                                     iv_mutable = iv_mutable ).
    ENDMETHOD.

    METHOD char.
      ro_elem = lcl_lisp_char=>new( value ).
    ENDMETHOD.

    METHOD charx.
      ro_elem = char( hex_to_char( value ) ).
    ENDMETHOD.

    METHOD hex_to_char.
      FIELD-SYMBOLS <xword> TYPE x.
      FIELD-SYMBOLS <xchar> TYPE x.
      DATA xword TYPE tv_xword.
      DATA lv_int TYPE int2.

      xword = value.
      lv_int = xword.
      ASSIGN lv_int TO <xword> CASTING.
      ASSIGN rv_char TO <xchar> CASTING.
      <xchar> = <xword>.
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
                                   exact = exact ).
    ENDMETHOD.

    METHOD number.
      CONSTANTS c_lisp_slash TYPE char1 VALUE '/'.
      DATA lv_nummer_str TYPE string.
      DATA lv_denom_str TYPE string.
      DATA lv_denom TYPE tv_int.
      DATA lv_int TYPE tv_int.
      DATA lv_real TYPE tv_real.

      TRY.
*         Check whether the token can be converted to a float,
*         to cover all manner of number formats, including scientific
          lv_real = value.

          IF iv_exact IS SUPPLIED AND iv_exact EQ abap_false.
            ro_elem = real( value = lv_real
                            exact = iv_exact ).
            RETURN.
          ENDIF.

          TRY.
              lv_nummer_str = value.
              IF NOT contains( val = lv_nummer_str sub = '.' ) OR iv_exact EQ abap_true.
                MOVE EXACT value TO lv_int.
                ro_elem = integer( value = lv_int
                                   iv_exact = abap_true ).
                RETURN.
              ENDIF.
            CATCH cx_sy_conversion_error ##NO_HANDLER.
          ENDTRY.

          IF contains( val = lv_nummer_str sub = '.' ) AND iv_exact EQ abap_true.
            DATA lv_int_str TYPE string.
            DATA lv_dec_str TYPE string.
            SPLIT lv_nummer_str AT lcl_parser=>c_lisp_dot INTO lv_int_str lv_dec_str.
            lv_int_str = lv_int_str && lv_dec_str.
            TRY.
              lv_int = lv_int_str.
              lv_denom = ipow( base = 10 exp = numofchar( lv_dec_str ) ).
              ro_elem = rational( nummer = lv_int
                                  denom = lv_denom
                                  iv_exact = iv_exact ).
            CATCH cx_sy_conversion_overflow.
              ro_elem = real( value = lv_real
                              exact = abap_false ).
            ENDTRY.
            RETURN.
          ENDIF.

          ro_elem = real( value = lv_real
                          exact = iv_exact ).
          RETURN.

        CATCH cx_sy_conversion_error.

          SPLIT value AT c_lisp_slash INTO lv_nummer_str lv_denom_str.
          IF sy-subrc EQ 0 AND lv_denom_str IS NOT INITIAL.
            MOVE EXACT lv_nummer_str TO lv_int.
            MOVE EXACT lv_denom_str TO lv_denom.
            DATA(lv_exact) = iv_exact.
            IF iv_exact IS SUPPLIED.
              lv_exact = iv_exact.
            ELSE.
              lv_exact = abap_true.
            ENDIF.
            ro_elem = rational( nummer = lv_int
                                denom = lv_denom
                                iv_exact = lv_exact ).
            RETURN.
          ENDIF.

      ENDTRY.

      RAISE EXCEPTION TYPE cx_sy_conversion_no_number
        EXPORTING
          value = value.

    ENDMETHOD.

    METHOD numeric.
      CASE record-type.
        WHEN type_integer.
          ro_elem = integer( record-int ).

        WHEN type_rational.
          IF record-denom EQ 1.
            ro_elem = integer( record-nummer ).
          ELSE.
            ro_elem = rational( nummer = record-nummer
                                denom = record-denom ).
          ENDIF.

        WHEN type_real.
          ro_elem = real( value = record-real
                          exact = record-exact ).

        WHEN OTHERS.
          lcl_lisp=>throw( |Error in result of [{ record-operation }]| ).
      ENDCASE.
    ENDMETHOD.

    DEFINE _throw_radix.
      RAISE EXCEPTION TYPE lcx_lisp_exception
        EXPORTING
          message = &1
          area    = c_area_radix.
    END-OF-DEFINITION.

    METHOD hex_integer.
      DATA lv_text TYPE string VALUE '0000000000000000'. " 2x8 = 16
      DATA lv_len TYPE tv_int.
      DATA lv_hex TYPE x LENGTH 8.

      lv_len = 16 - strlen( value ).
      IF lv_len GT 0.
        lv_text = lv_text+0(lv_len) && value.
      ELSE.
        lv_text = value.
      ENDIF.
      IF lv_text CA 'abcdef'.
        lv_text = to_upper( lv_text ).
      ENDIF.
      IF lv_text CO c_hex_digits.
        lv_hex = lv_text.
        rv_int = lv_hex.
      ELSE.
        _throw_radix `Invalid hexadecimal number`.
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
      SPLIT lv_text AT lcl_parser=>c_lisp_dot INTO lv_trunc_str lv_decimal_str.

      lv_int = hex_integer( lv_trunc_str ).
      IF lv_decimal_str IS INITIAL.
        ro_elem = integer( lv_int ).
      ELSE.
        lv_dec = hex_integer( lv_decimal_str ).
        lv_real = lv_int.
        IF lv_dec EQ 0.
          ro_elem = real( value = lv_real
                          exact = abap_true  ).
        ELSE.
          lv_exp = numofchar( lv_decimal_str ).
          ro_elem = real( value = lv_real + lv_dec / ipow( base = 16 exp = lv_exp )
                          exact = abap_false ).
        ENDIF.
      ENDIF.
    ENDMETHOD.

    METHOD octal_integer.
      DATA lv_text TYPE string.
      DATA lv_index TYPE sytabix.
      DATA lv_size TYPE sytabix.
      DATA lv_char TYPE char01.
      DATA lv_radix TYPE i VALUE 1.

      CLEAR rv_int.
      lv_text = value.
      IF lv_text CN '01234567'.
        _throw_radix `Invalid octal number`.
      ENDIF.

      lv_index = lv_size = strlen( lv_text ).
      DO lv_size TIMES.
        SUBTRACT 1 FROM lv_index.
        lv_char = lv_text+lv_index(1).

        rv_int = rv_int + lv_char * lv_radix.
        lv_radix = lv_radix * 8.
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
      SPLIT lv_text AT lcl_parser=>c_lisp_dot INTO lv_trunc_str lv_decimal_str.

      lv_int = octal_integer( lv_trunc_str ).
      IF lv_decimal_str IS INITIAL.
        ro_elem = integer( lv_int ).
      ELSE.
        lv_dec = octal_integer( lv_decimal_str ).
        lv_real = lv_int.
        IF lv_dec EQ 0.
          ro_elem = real( value = lv_real
                          exact = abap_true ).
        ELSE.
          lv_exp = numofchar( lv_decimal_str ).
          ro_elem = real( value = lv_real + lv_dec / ipow( base = 8 exp = lv_exp )
                          exact = abap_false ).
        ENDIF.
      ENDIF.
    ENDMETHOD.

    METHOD binary_integer.
      DATA lv_text TYPE string.
      DATA lv_index TYPE sytabix.
      DATA lv_size TYPE sytabix.
      DATA lv_radix TYPE i VALUE 1.

      lv_text = value.
      IF lv_text CN '01'.
        _throw_radix `Invalid binary number`.
      ENDIF.

      lv_index = lv_size = strlen( lv_text ).
      DO lv_size TIMES.
        SUBTRACT 1 FROM lv_index.

        IF lv_text+lv_index(1) EQ '1'.
          rv_int = rv_int + lv_radix.
        ENDIF.
        lv_radix = lv_radix * 2.
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
      DATA lv_exact TYPE flag.

      lv_text = value.
      SPLIT lv_text AT lcl_parser=>c_lisp_dot INTO lv_trunc_str lv_decimal_str.

      lv_int = binary_integer( lv_trunc_str ).
      IF lv_decimal_str IS INITIAL.
        ro_elem = integer( lv_int ).
      ELSE.
        lv_frac_bin = binary_integer( lv_decimal_str ).

        IF lv_frac_bin EQ 0.
          lv_frac_real = 0.
          lv_exact = abap_true.
        ELSE.
          lv_exp = numofchar( lv_decimal_str ).
          lv_frac_real = lv_frac_bin / ipow( base = 2 exp = lv_exp ).
          lv_exact = abap_false.
        ENDIF.
        ro_elem = real( value = lv_int + lv_frac_real
                        exact = lv_exact ).
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
      ro_vec->vector = it_vector.
      ro_vec->mutable = iv_mutable.
      ro_vec->mo_length = number( lines( it_vector ) ).
    ENDMETHOD.

    METHOD bytevector.
      ro_u8 = NEW lcl_lisp_bytevector( type_bytevector ).
      ro_u8->bytes = it_byte.
      ro_u8->mutable = iv_mutable.
      ro_u8->mo_length = number( lines( it_byte ) ).
    ENDMETHOD.

    METHOD values.
      ro_elem = NEW lcl_lisp_values( io_elem ).
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
      ro_elem->cdr->mutable = abap_false.
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

  ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_hash IMPLEMENTATION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_hash IMPLEMENTATION.

    METHOD eval.
      result = NEW lcl_lisp_hash( type_hash ).

      LOOP AT hash INTO DATA(ls_entry).
        INSERT VALUE #( key = ls_entry-key
                        element = interpreter->eval( VALUE #( elem = ls_entry-element
                                                              env = environment ) ) ) INTO TABLE result->hash.
      ENDLOOP.

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
      ro_elem = nil.
      LOOP AT vector ASSIGNING FIELD-SYMBOL(<vec>).
        AT FIRST.
          ro_elem = lcl_lisp_new=>cons( io_car = <vec> ).
          DATA(lo_ptr) = ro_elem.
          CONTINUE.
        ENDAT.
        lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = <vec> ).
      ENDLOOP.
    ENDMETHOD.

    METHOD get_list.
      DATA lv_end TYPE tv_index.

      DATA(lv_start) = from + 1.         " start is Inclusive

      IF to IS SUPPLIED.
        lv_end = to.                     " end is Exclusive
      ELSE.
        lv_end = lines( vector ).        " End of vector
      ENDIF.

      IF lv_end LT 1 OR lv_start GT lv_end.
        throw( |vector-ref: out-of-bound range| ).
      ENDIF.

      ro_elem = nil.
      CHECK lv_start BETWEEN 1 AND lv_end.

      ro_elem = lcl_lisp_new=>cons( io_car = vector[ lv_start ] ).

      DATA(lo_ptr) = ro_elem.
      LOOP AT vector FROM lv_start + 1 TO lv_end ASSIGNING FIELD-SYMBOL(<vec>).
        lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = <vec> ).
      ENDLOOP.
    ENDMETHOD.

    METHOD get.
      DATA(lv_start) = index + 1.

      IF lv_start BETWEEN 1 AND lines( vector ).
        ro_elem = vector[ lv_start ].
      ELSE.
        throw( |vector-ref: out-of-bound position { index }| ).
      ENDIF.
    ENDMETHOD.

    METHOD set.
      _validate_mutable me `vector`.

      DATA(lv_start) = index + 1.

      IF lv_start BETWEEN 1 AND lines( vector ).
        vector[ lv_start ] = io_elem.
      ELSE.
        throw( |vector-set!: out-of-bound position { index }| ).
      ENDIF.
    ENDMETHOD.

    METHOD fill.
      DATA lv_end TYPE tv_index.
      _validate_mutable me `vector`.
      ro_elem = me.

      DATA(lv_start) = from + 1.         " start is Inclusive
      DATA(lv_size) = lines( vector ).
      IF to IS SUPPLIED.
        lv_end = to.                     " end is Exclusive
      ELSE.
        lv_end = lv_size.        " End of vector
      ENDIF.

      IF lv_end LT 1 OR lv_end GT lv_size OR lv_start GT lv_end.
        throw( |vector-fill!: out-of-bound range| ).
      ENDIF.

      LOOP AT vector FROM lv_start TO lv_end ASSIGNING FIELD-SYMBOL(<vec>).
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

      result = false.
      CHECK io_elem->type EQ type_vector.

      lo_vec ?= io_elem.
      CHECK lines( vector ) = lines( lo_vec->vector ).

      LOOP AT lo_vec->vector INTO DATA(lo_elem).
        lo_ptr = vector[ sy-tabix ].
        CHECK lo_ptr->is_equal( io_elem = lo_elem
                                comp = comp
                                interpreter = interpreter ) EQ false.
        RETURN.
      ENDLOOP.
      result = true.
    ENDMETHOD.

    METHOD eval.
      result = lcl_lisp_new=>vector( it_vector = VALUE tt_lisp( FOR lo_elem IN vector
                                                    ( interpreter->eval( VALUE #( elem = lo_elem
                                                                                  env = environment ) ) ) )
                                     iv_mutable = abap_true ).
    ENDMETHOD.

    METHOD set_shared_structure.
      FIELD-SYMBOLS <lo_elem> TYPE REF TO lcl_lisp.

      CHECK mv_label IS NOT INITIAL.

      CASE type.
        WHEN type_vector.
          LOOP AT vector ASSIGNING <lo_elem> WHERE table_line->type = type_symbol
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
      DATA lv_str TYPE string.
      DATA lv_char2 TYPE c LENGTH 2.
      DATA lv_hex TYPE x.
      FIELD-SYMBOLS <byte> TYPE tv_byte.

      LOOP AT bytes TRANSPORTING NO FIELDS WHERE TABLE_LINE GT c_max_ascii.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
          " non ASCII chars
          LOOP AT bytes ASSIGNING <byte>.
            lv_hex = <byte>.
            WRITE lv_hex TO lv_char2.

            lv_str = lv_str && ` #x` && lv_char2.
          ENDLOOP.
      ELSE.
          LOOP AT bytes ASSIGNING <byte>.
            WRITE <byte> TO lv_char2.
            lv_str = lv_str && ` ` && lv_char2.
          ENDLOOP.
      ENDIF.

      IF lv_str IS NOT INITIAL.
        lv_str = lv_str && ` `.
      ENDIF.

      str = |#u8{ lcl_parser=>c_open_paren }{ lv_str }{ lcl_parser=>c_close_paren }|.

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
          lv_buffer = lv_buffer && lv_hex.
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
          lv_to = lines( bytes ).   " to is Exclusive
        ELSE.
          lv_to = to.               " End of bytevector
        ENDIF.

        LOOP AT bytes INTO lv_u8 FROM lv_from TO lv_to.
          APPEND lv_u8 TO lt_byte.
        ENDLOOP.

        ro_elem = lcl_lisp_new=>bytevector( it_byte = lt_byte
                                            iv_mutable = mutable ).
      ENDMETHOD.

      METHOD copy.
        " ( |bytevector-copy!:| ).
        DATA lv_end TYPE tv_index.

        DATA(lv_start) = start + 1.          " from is Inclusive

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

        result = false.
        CHECK io_elem->type EQ type_bytevector.

        lo_u8 ?= io_elem.
        CHECK bytes = lo_u8->bytes.

        result = true.
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
