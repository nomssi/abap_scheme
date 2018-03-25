*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           YY_LIB_LISP
*& https://github.com/nomssi/abap_scheme
*& https://github.com/mydoghasworms/abap-lisp
*& Lisp interpreter written in ABAP
*& Copy and paste this code into a type I (include) program
*&---------------------------------------------------------------------*
*& MIT License (see below)
*& Martin Ceronio, martin.ceronio@infosize.co.za June 2015
*& Jacques Nomssi Nzali, www.informatik-dv.com Sept. 2015 to Jan. 2018
*&---------------------------------------------------------------------*
*  The MIT License (MIT)
*
*  Copyright (c) 2018 Jacques Nomssi Nzali
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
    c_lisp_input     TYPE string VALUE 'ABAP Lisp Input',
    c_lisp_eof       TYPE x LENGTH 2 VALUE 'FFFF', " we do not expect this in source code
    c_lisp_nil       TYPE string VALUE 'nil',
    c_expr_separator TYPE string VALUE ` `.   " multiple expression output
  CONSTANTS:
    c_error_message         TYPE string VALUE 'Error in processing',
    c_error_incorrect_input TYPE string VALUE 'Incorrect input',
    c_error_unexpected_end  TYPE string VALUE 'Unexpected end',
    c_error_eval            TYPE string VALUE 'EVAL( ) came up empty-handed'.
  CONSTANTS:
    c_area_eval  TYPE string VALUE `Eval`,
    c_area_parse TYPE string VALUE `Parse`.
  CONSTANTS:
    c_lisp_else TYPE string VALUE 'else',
    c_lisp_then TYPE c LENGTH 2 VALUE '=>'.
  CONSTANTS:
    c_eval_append           TYPE string VALUE 'append',
    c_eval_cons             TYPE string VALUE 'cons',
    c_eval_list             TYPE string VALUE 'list',

    c_eval_quote            TYPE string VALUE 'quote',
    c_eval_quasiquote       TYPE string VALUE 'quasiquote',
    c_eval_unquote          TYPE string VALUE 'unquote',
    c_eval_unquote_splicing TYPE string VALUE 'unquote-splicing'.

  TYPES tv_int TYPE i.            " integer data type, use int8 if available
  TYPES tv_real TYPE decfloat34.  " real data type
  TYPES tv_xword TYPE x LENGTH 2.

  DEFINE trace_call.
    IF gv_lisp_trace EQ abap_true.
      cl_demo_output=>write( |call { &1->value } { &1->to_string( ) } param { &2->to_string( ) }| ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE trace_result.
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

  DEFINE assert_is_bound.
    IF &1 IS NOT BOUND.
      lcl_lisp=>throw( &2 ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE validate.
    assert_is_bound &1 c_error_incorrect_input.
  END-OF-DEFINITION.

  DEFINE validate_mutable.
    validate &1.
    IF &1->mutable EQ abap_false.
      throw( |constant { &2 } cannot be changed| ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE validate_type.
    validate &1.
    IF &1->type NE lcl_lisp=>type_&3.
      throw( &1->to_string( ) && ` is not a ` && &4 && ` in ` && &2 ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE validate_integer.
    validate &1.
    IF &1->type NE lcl_lisp=>type_integer.
      throw( &1->to_string( ) && ` is not an integer in ` && &2 ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE validate_index.
    validate_integer &1 &2.
    IF CAST lcl_lisp_integer( &1 )->integer LT 0.
      throw( &1->to_string( ) && ` must be non-negative in ` && &2 ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE validate_char.
    validate_type &1 &2 char `char`.
  END-OF-DEFINITION.

  DEFINE validate_string.
    validate_type &1 &2 string `string`.
  END-OF-DEFINITION.

  DEFINE validate_vector.
    validate_type &1 &2 vector `vector`.
  END-OF-DEFINITION.

  DEFINE validate_port.
    validate_type &1 &2 port `port`.
  END-OF-DEFINITION.

  DEFINE validate_number.
    validate &1.
    CASE &1->type.
      WHEN lcl_lisp=>type_integer
        OR lcl_lisp=>type_real
        OR lcl_lisp=>type_rational
        OR lcl_lisp=>type_complex.
      WHEN OTHERS.
        throw( |{ &1->to_string( ) } is not a number in | && &2 ).
    ENDCASE.
  END-OF-DEFINITION.

  DEFINE error_no_list.
    throw( |{ &2 }: { &1->to_string( ) } is not a proper list| ).
  END-OF-DEFINITION.

  DEFINE validate_tail.
    IF &1 NE nil.
*     if the last element in the list is not a cons cell, we cannot append
      error_no_list &2 &3.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE get_number.
    validate &2.
    cell = &2.
    CASE cell->type.
      WHEN lcl_lisp=>type_integer.
        lo_int ?= cell.
        &1 = lo_int->integer.
      WHEN lcl_lisp=>type_real.
        lo_real ?= cell.
        &1 = lo_real->real.
      WHEN lcl_lisp=>type_rational.
        lo_rat ?= cell.
        &1 = lo_rat->integer / lo_rat->denominator.
*      WHEN lcl_lisp=>type_complex.
      WHEN OTHERS.
        throw( |{ cell->to_string( ) } is not a number in { &3 }| ).
    ENDCASE.
  END-OF-DEFINITION.

* Macro that implements the logic for the comparison native
* procedures, where only the comparison operator differs
  DEFINE _comparison.
    DATA carry TYPE tv_real.
    DATA cell TYPE REF TO lcl_lisp.
    DATA lo_rat TYPE REF TO lcl_lisp_rational.
    DATA lo_int TYPE REF TO lcl_lisp_integer.
    DATA lo_real TYPE REF TO lcl_lisp_real.

    result = false.
    validate list.
    get_number carry list->car &2.

    cell = list->cdr.
    WHILE cell->type EQ lcl_lisp=>type_pair.
    validate cell->car.

    CASE cell->car->type.
    WHEN lcl_lisp=>type_integer.
    lo_int ?= cell->car.
    IF carry &1 lo_int->integer.
    RETURN.
    ENDIF.
    carry = lo_int->integer.

    WHEN lcl_lisp=>type_real.
    lo_real ?= cell->car.
    IF carry &1 lo_real->real.
    RETURN.
    ENDIF.
    carry = lo_real->real.

    WHEN lcl_lisp=>type_rational.
    lo_rat ?= cell->car.
    IF carry * lo_rat->denominator &1 lo_rat->integer.
    RETURN.
    ENDIF.
    carry = lo_rat->integer / lo_rat->denominator.

*            WHEN lcl_lisp=>type_complex.
    WHEN OTHERS.
    throw( |{ cell->car->to_string( ) } is not a number in { &2 }| ).
    ENDCASE.
    cell = cell->cdr.
    ENDWHILE.
    result = true.
  END-OF-DEFINITION.

  DEFINE _sign.
    DATA carry TYPE tv_real.
    DATA cell TYPE REF TO lcl_lisp.
    DATA lo_rat TYPE REF TO lcl_lisp_rational.
    DATA lo_int TYPE REF TO lcl_lisp_integer.
    DATA lo_real TYPE REF TO lcl_lisp_real.

    result = false.
    validate list.
    get_number carry list->car &2.

    IF sign( carry ) NE &1.
    RETURN.
    ENDIF.
    result = true.
  END-OF-DEFINITION.

  DEFINE _is_type. " argument in list->car
    validate list.
    result = false.
    CHECK list->car IS BOUND.
    IF list->car->type EQ lcl_lisp=>type_&1.
      result = true.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _is_last_param.
    IF &1->cdr NE nil.
      throw( |{ &1->to_string( ) } Parameter mismatch| ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE _catch_arithmetic_error.
    CATCH cx_sy_arithmetic_error INTO DATA(lx_error).
      throw( lx_error->get_text( ) ).
  END-OF-DEFINITION.

* Macro that implements the logic for call of ABAP math statements
  DEFINE _math.
    DATA carry TYPE tv_real.
    DATA cell TYPE REF TO lcl_lisp.
    DATA lo_rat TYPE REF TO lcl_lisp_rational.
    DATA lo_int TYPE REF TO lcl_lisp_integer.
    DATA lo_real TYPE REF TO lcl_lisp_real.

    result = nil.
    validate list.
    TRY.
    get_number carry list->car &2.
    _is_last_param list.
    result = lcl_lisp_new=>real( &1( carry ) ).
    _catch_arithmetic_error.
    ENDTRY.
  END-OF-DEFINITION.

  DEFINE _trigonometric.
    DATA carry TYPE f.
    DATA cell TYPE REF TO lcl_lisp.
    DATA lo_rat TYPE REF TO lcl_lisp_rational.
    DATA lo_int TYPE REF TO lcl_lisp_integer.
    DATA lo_real TYPE REF TO lcl_lisp_real.

    result = nil.
    validate list.
    TRY.
    get_number carry list->car &2.
    _is_last_param list.
    result = lcl_lisp_new=>real( &1( carry ) ).
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

  CLASS lcl_sexps DEFINITION.   " Symbolic expression (S-expression)
    PUBLIC SECTION.
      TYPES tv_type TYPE char1.
*      Type definitions for the various elements
      CONSTANTS:
        type_symbol   TYPE tv_type VALUE 'S',
        type_integer  TYPE tv_type VALUE 'N',
        type_real     TYPE tv_type VALUE 'R',
        type_complex  TYPE tv_type VALUE 'z',
        type_rational TYPE tv_type VALUE 'r',
        type_string   TYPE tv_type VALUE '"'.
      CONSTANTS:
        type_boolean   TYPE tv_type VALUE 'b',
        type_char      TYPE tv_type VALUE 'c',
        type_null      TYPE tv_type VALUE '0',
        type_pair      TYPE tv_type VALUE 'C',
        type_lambda    TYPE tv_type VALUE 'λ',
        type_native    TYPE tv_type VALUE 'n',
        type_primitive TYPE tv_type VALUE 'I',
        type_syntax    TYPE tv_type VALUE 'y',
        type_hash      TYPE tv_type VALUE 'h',
        type_vector    TYPE tv_type VALUE 'v',
        type_port      TYPE tv_type VALUE 'o'.
*      Types for ABAP integration:
      CONSTANTS:
        type_abap_data     TYPE tv_type VALUE 'D',
        type_abap_table    TYPE tv_type VALUE 'T',
        type_abap_query    TYPE tv_type VALUE 'q',
        type_abap_sql_set  TYPE tv_type VALUE 's',
        type_abap_function TYPE tv_type VALUE 'F'.
*        type_abap_class    TYPE tv_type VALUE 'a',
*        type_abap_method   TYPE tv_type VALUE 'm'.

      DATA type TYPE char1.
  ENDCLASS.

  CLASS lcl_lisp_iterator DEFINITION DEFERRED.
  CLASS lcl_lisp_new DEFINITION DEFERRED.
  CLASS lcl_lisp_interpreter DEFINITION DEFERRED.

* Single element that will capture cons cells, atoms etc.
*----------------------------------------------------------------------*
*       CLASS lcl_lisp DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_lisp DEFINITION INHERITING FROM lcl_sexps FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
*     Can this be replaced by a mesh? cf. DEMO_RND_PARSER_AST
      DATA mutable TYPE flag VALUE abap_true READ-ONLY.

      DATA macro TYPE flag.
      DATA parameter_object TYPE flag.
      DATA value TYPE string.

      DATA car TYPE REF TO lcl_lisp.
      DATA cdr TYPE REF TO lcl_lisp.

      CLASS-METHODS class_constructor.

      CLASS-DATA nil        TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA false      TYPE REF TO  lcl_lisp READ-ONLY.
      CLASS-DATA true       TYPE REF TO  lcl_lisp READ-ONLY.

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
                       RETURNING VALUE(result) TYPE REF TO lcl_lisp
                       RAISING   lcx_lisp_exception.

      METHODS is_procedure RETURNING VALUE(result) TYPE REF TO lcl_lisp.

      METHODS is_number RETURNING VALUE(result) TYPE REF TO lcl_lisp.

      METHODS error_not_a_pair IMPORTING context TYPE string DEFAULT space
                               RAISING   lcx_lisp_exception.

      CLASS-METHODS throw IMPORTING message TYPE string
                          RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      METHODS list_to_string RETURNING VALUE(str) TYPE string
                             RAISING   lcx_lisp_exception.

      METHODS format_quasiquote IMPORTING io_elem TYPE REF TO lcl_lisp
                                EXPORTING ev_skip TYPE flag
                                          ev_str  TYPE string.
  ENDCLASS.                    "lcl_lisp DEFINITION

  CLASS lcl_lisp_char DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      TYPES: BEGIN OF ts_char,
               char TYPE c LENGTH 1,
               elem TYPE REF TO lcl_lisp_char,
             END OF ts_char.
      CLASS-DATA char_table TYPE HASHED TABLE OF ts_char WITH UNIQUE KEY char READ-ONLY.
    PROTECTED SECTION.
  ENDCLASS.

  CLASS lcl_lisp_null DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
  ENDCLASS.

  CLASS lcl_lisp_boolean DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
  ENDCLASS.

  CLASS lcl_lisp_string DEFINITION INHERITING FROM lcl_lisp
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
    PROTECTED SECTION.
      METHODS constructor IMPORTING value TYPE any
                                    iv_mutable TYPE flag.
  ENDCLASS.

  CLASS lcl_lisp_string IMPLEMENTATION.

    METHOD constructor.
      super->constructor( ).
      type = type_string.
      me->value = value.
      mutable = iv_mutable.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_number DEFINITION INHERITING FROM lcl_lisp ABSTRACT.
    PUBLIC SECTION.
      DATA exact TYPE flag READ-ONLY.
      METHODS is_exact RETURNING VALUE(result) TYPE REF TO lcl_lisp.
  ENDCLASS.

CLASS lcl_lisp_number IMPLEMENTATION.

  METHOD is_exact.
    result = false.
    CHECK exact EQ abap_true.
    result = true.
  ENDMETHOD.

ENDCLASS.

  CLASS lcl_lisp_integer DEFINITION INHERITING FROM lcl_lisp_number
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA integer TYPE tv_int READ-ONLY.
    PROTECTED SECTION.
      METHODS constructor IMPORTING value TYPE any.
  ENDCLASS.

  CLASS lcl_lisp_rational DEFINITION INHERITING FROM lcl_lisp_integer
    CREATE PROTECTED FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA denominator TYPE tv_int READ-ONLY.
      METHODS to_string REDEFINITION.
      CLASS-METHODS gcd IMPORTING n             TYPE numeric
                                  d             TYPE numeric
                        RETURNING VALUE(result) TYPE tv_int
                        RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      METHODS constructor IMPORTING nummer TYPE tv_int
                                    denom  TYPE tv_int.
      METHODS normalize RAISING lcx_lisp_exception.
  ENDCLASS.

  CLASS lcl_lisp_integer IMPLEMENTATION.

    METHOD constructor.
      super->constructor( ).
      type = lcl_lisp=>type_integer.
      integer = value.
      exact = abap_true.
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_rational IMPLEMENTATION.

    METHOD constructor.
      super->constructor( nummer ).
      type = type_rational.
      denominator = denom.
      normalize( ).
    ENDMETHOD.

    METHOD to_string.
      str = |{ integer }/{ denominator }|.
    ENDMETHOD.

    METHOD normalize.
      DATA(g) = gcd( n = integer
                     d = denominator ).
      integer = trunc( integer / g ).
      denominator = trunc( denominator / g ).
      exact = xsdbool( denominator NE 0 ).
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
            den = num - den * trunc( num / den ).
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
    PROTECTED SECTION.
      METHODS constructor IMPORTING value TYPE any.
  ENDCLASS.

CLASS lcl_lisp_real IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    type = lcl_lisp=>type_real.
    real = value.
    exact = abap_false.
  ENDMETHOD.

ENDCLASS.

  CLASS lcl_lisp_pair DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
    PROTECTED SECTION.
  ENDCLASS.

  CLASS lcl_lisp_lambda DEFINITION INHERITING FROM lcl_lisp_pair FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
    PROTECTED SECTION.
  ENDCLASS.

  CLASS lcl_lisp_symbol DEFINITION INHERITING FROM lcl_lisp FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
      DATA index TYPE tv_int READ-ONLY.
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

  TYPES tt_lisp TYPE STANDARD TABLE OF REF TO lcl_lisp WITH EMPTY KEY.
  CLASS lcl_lisp_vector DEFINITION DEFERRED.
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
    METHODS display IMPORTING element TYPE REF TO lcl_lisp.
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
      DATA last_index TYPE sytabix.
      DATA last_len TYPE sytabix.

      METHODS read_block.
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

  CLASS lcl_lisp_new DEFINITION.
    PUBLIC SECTION.
      CLASS-METHODS atom IMPORTING value          TYPE any
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS null RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS symbol IMPORTING value          TYPE any
                                     index          TYPE tv_int OPTIONAL
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_symbol.
      CLASS-METHODS boolean IMPORTING value          TYPE any
                            RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS integer IMPORTING value          TYPE any
                            RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_integer.
      CLASS-METHODS rational IMPORTING nummer         TYPE tv_int
                                       denom          TYPE tv_int
                             RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_integer
                             RAISING   lcx_lisp_exception.

      CLASS-METHODS real IMPORTING value          TYPE any
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_real.
      CLASS-METHODS number IMPORTING value          TYPE any
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS string IMPORTING value          TYPE any
                                     iv_mutable     TYPE flag DEFAULT abap_true
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_string.
      CLASS-METHODS char IMPORTING value          TYPE any
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_char.
      CLASS-METHODS charx IMPORTING value          TYPE any
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_char.
      CLASS-METHODS port IMPORTING iv_port_type   TYPE lcl_lisp_port=>tv_port_type
                                   iv_input       TYPE flag
                                   iv_output      TYPE flag
                                   iv_error       TYPE flag
                                   iv_buffered    TYPE flag
                                   iv_separator   TYPE string OPTIONAL
                                   iv_string      TYPE flag DEFAULT abap_false
                         RETURNING VALUE(ro_port) TYPE REF TO lcl_lisp_port.

      CLASS-METHODS elem IMPORTING type           TYPE lcl_lisp=>tv_type
                                   value          TYPE any OPTIONAL
                                   parameter      TYPE flag DEFAULT abap_false
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS data IMPORTING ref            TYPE REF TO data OPTIONAL
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_data.
      CLASS-METHODS table IMPORTING ref            TYPE REF TO data OPTIONAL
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_table.
      CLASS-METHODS query IMPORTING value          TYPE any OPTIONAL
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
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

      CLASS-METHODS lambda IMPORTING io_car           TYPE REF TO lcl_lisp
                                     io_cdr           TYPE REF TO lcl_lisp
                                     io_env           TYPE REF TO lcl_lisp_environment
                                     iv_macro         TYPE flag DEFAULT abap_false
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
      rv_input = space.
    ENDMETHOD.

  ENDCLASS.                    "lcl_console DEFINITION

  CLASS lcl_lisp_port IMPLEMENTATION.

    METHOD constructor.
      super->constructor( ).
      type = lcl_lisp=>type_port.

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
      IF iv_title IS INITIAL.
        lt_fields = VALUE #( ( tabname = 'ABDBG'     " Text: Input Line
                               fieldname = 'LTEXT'
                               field_obl = 'X' ) ).
      ELSE.
        lt_fields = VALUE #( ( tabname = 'ABDBG'     " Text: Input Line
                               fieldname = 'LTEXT'
                               fieldtext = iv_title
                               field_obl = 'X' ) ).
      ENDIF.

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
    ENDMETHOD.

    METHOD read_block.
      last_input = read_stream( ).
      last_len = strlen( last_input ).
      last_index = 0.
    ENDMETHOD.

    METHOD lif_input_port~peek_char.
      CHECK input EQ abap_true.
      IF last_index < last_len.
        rv_char = last_input+last_index(1).
      ELSE.
        read_block( ).
        rv_char = last_input+0(1).
      ENDIF.
    ENDMETHOD.

    METHOD lif_input_port~read_char.
      CHECK input EQ abap_true.
      IF last_index < last_len.
        rv_char = last_input+last_index(1).
      ELSE.
        read_block( ).
        rv_char = last_input+0(1).
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
      separator = iv_separator.
      string_mode = iv_string.
    ENDMETHOD.

    METHOD lif_log~put.
      write( io_elem ).
    ENDMETHOD.

    METHOD lif_log~get.
      result = flush( ).
    ENDMETHOD.

    METHOD write.
      DATA lv_text TYPE string.

      CHECK element IS BOUND.
      TRY.
          lv_text = element->to_string( ).
        CATCH lcx_lisp_exception INTO DATA(lx_error).
          lv_text = lx_error->get_text( ).
      ENDTRY.
      add( lv_text ).
    ENDMETHOD.

    METHOD display.
      DATA lv_text TYPE string.

      CHECK element IS BOUND.
      TRY.
          lv_text = element->to_text( ).
        CATCH lcx_lisp_exception INTO DATA(lx_error).
          lv_text = lx_error->get_text( ).
      ENDTRY.
      add( lv_text ).
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

      CLASS-METHODS init IMPORTING size             TYPE sytabix
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

      METHODS set IMPORTING index         TYPE sytabix
                            io_elem       TYPE REF TO lcl_lisp
                  RETURNING VALUE(result) TYPE REF TO lcl_lisp_vector
                  RAISING   lcx_lisp_exception.

      METHODS get IMPORTING index          TYPE sytabix
                  RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                  RAISING   lcx_lisp_exception.

      METHODS get_list IMPORTING from           TYPE sytabix DEFAULT 0
                                 to             TYPE sytabix OPTIONAL
                       RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                       RAISING   lcx_lisp_exception.

      METHODS length RETURNING VALUE(ro_length) TYPE REF TO lcl_lisp.

      METHODS to_string REDEFINITION.
      METHODS is_equal REDEFINITION.

      METHODS eval IMPORTING environment   TYPE REF TO lcl_lisp_environment
                             interpreter   TYPE REF TO lcl_lisp_interpreter
                   RETURNING VALUE(result) TYPE REF TO lcl_lisp_vector
                   RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      DATA vector TYPE tt_lisp.
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

  CLASS lcl_lisp_sql_result DEFINITION INHERITING FROM lcl_lisp_data.
    PUBLIC SECTION.
      METHODS constructor IMPORTING io_result TYPE REF TO cl_sql_result_set.
      METHODS clear.
      METHODS close.
    PROTECTED SECTION.
      DATA result_set TYPE REF TO cl_sql_result_set.
  ENDCLASS.

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

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_environment DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_environment DEFINITION CREATE PRIVATE.
    PUBLIC SECTION.

      CLASS-METHODS
        new IMPORTING io_outer      TYPE REF TO lcl_lisp_environment OPTIONAL
            RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment.

      METHODS:
        find IMPORTING symbol     TYPE any
             RETURNING VALUE(env) TYPE REF TO lcl_lisp_environment
             RAISING   lcx_lisp_exception,
        get IMPORTING symbol      TYPE any
            RETURNING VALUE(cell) TYPE REF TO lcl_lisp
            RAISING   lcx_lisp_exception,
        set IMPORTING symbol  TYPE string
                      element TYPE REF TO lcl_lisp,
        set_once IMPORTING symbol  TYPE string
                           element TYPE REF TO lcl_lisp
                 RAISING   lcx_lisp_exception,
*       Convenience method to add a value and create the cell
        define_value IMPORTING symbol         TYPE string
                               type           TYPE lcl_lisp=>tv_type
                               value          TYPE any OPTIONAL
                               parameter      TYPE flag DEFAULT abap_false
                     RETURNING VALUE(element) TYPE REF TO lcl_lisp.

      METHODS parameters_to_symbols IMPORTING io_pars TYPE REF TO lcl_lisp
                                              io_args TYPE REF TO lcl_lisp
                                    RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
*     Reference to outer (parent) environment:
      DATA outer TYPE REF TO lcl_lisp_environment.

      TYPES: BEGIN OF ts_map,
               symbol TYPE string,
               value  TYPE REF TO lcl_lisp,
             END OF ts_map.
      TYPES tt_map TYPE HASHED TABLE OF ts_map WITH UNIQUE KEY symbol.

      DATA map TYPE tt_map.

      METHODS unbound_symbol IMPORTING symbol TYPE any
                             RAISING   lcx_lisp_exception.
  ENDCLASS.                    "lcl_lisp_environment DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_parser DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_parser DEFINITION.
    PUBLIC SECTION.
      TYPES tt_element TYPE STANDARD TABLE OF REF TO lcl_lisp WITH DEFAULT KEY.
      CONSTANTS:
        c_lisp_dot    TYPE char1 VALUE '.',
        c_open_paren  TYPE char1 VALUE '(',
        c_close_paren TYPE char1 VALUE ')',
        c_lisp_equal  TYPE char1 VALUE '=',
        c_lisp_x      TYPE char1 VALUE 'x'.
      CONSTANTS:
        c_escape_char    TYPE char1 VALUE '\',
        c_lisp_slash     TYPE char1 VALUE '/',
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

      METHODS:
        constructor,
        parse IMPORTING iv_code         TYPE clike
              RETURNING VALUE(elements) TYPE tt_element
              RAISING   lcx_lisp_exception,
        read_from IMPORTING ii_port        TYPE REF TO lif_input_port

                  RETURNING VALUE(element) TYPE REF TO lcl_lisp
                  RAISING   lcx_lisp_exception.

    PRIVATE SECTION.
      TYPES: BEGIN OF ts_label,
               datum TYPE string,
               element TYPE REF TO lcl_lisp,
             END OF ts_label.
      TYPES tt_labels TYPE SORTED TABLE OF ts_label WITH UNIQUE KEY datum.

      TYPES tv_text13 TYPE c LENGTH 13.
      DATA code TYPE string.
      DATA length TYPE i.
      DATA index TYPE i.
      DATA char TYPE char1.
      DATA mt_labels TYPE tt_labels.

      DATA mv_eol TYPE char1.
      DATA mv_whitespace TYPE char07. " Case sensitive
      DATA mv_delimiters TYPE tv_text13. " Case sensitive

      METHODS:
        next_char RAISING lcx_lisp_exception,
        peek_char RETURNING VALUE(rv_char) TYPE char1,
        peek_bytevector RETURNING VALUE(rv_flag) TYPE flag,
        peek_label EXPORTING ev_label TYPE string
                   RETURNING VALUE(rv_found) TYPE flag,
        skip_whitespace
          RETURNING VALUE(rv_has_next) TYPE flag
          RAISING   lcx_lisp_exception,
        parse_list IMPORTING delim         TYPE char01 DEFAULT c_open_paren
                   RETURNING VALUE(result) TYPE REF TO lcl_lisp
                   RAISING   lcx_lisp_exception,
        parse_token IMPORTING iv_literal TYPE flag DEFAULT abap_false
                    RETURNING VALUE(element) TYPE REF TO lcl_lisp
                    RAISING   lcx_lisp_exception.
      METHODS match_string CHANGING cv_val TYPE string.
      METHODS match_atom CHANGING cv_val TYPE string.

      METHODS throw IMPORTING message TYPE string
                    RAISING   lcx_lisp_exception.
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
          IMPORTING element       TYPE REF TO lcl_lisp
                    environment   TYPE REF TO lcl_lisp_environment
          RETURNING VALUE(result) TYPE REF TO lcl_lisp
          RAISING   lcx_lisp_exception,
* To enable a REPL, the following convenience method wraps parsing and evaluating
* and stringifies the response/error
        eval_source
          IMPORTING code            TYPE clike
          RETURNING VALUE(response) TYPE string,
        eval_repl
          IMPORTING code            TYPE clike
          RETURNING VALUE(response) TYPE string
          RAISING   lcx_lisp_exception,
        validate_source
          IMPORTING code            TYPE clike
          RETURNING VALUE(response) TYPE string.

* Functions for dealing with lists:
      _proc_meth:
      proc_append          ##called,
      proc_append_unsafe   ##called,
      proc_reverse,         ##called
      proc_set_car,         ##called
      proc_set_cdr,         ##called
      proc_car,             ##called
      proc_cdr,             ##called

      proc_caar,             ##called
      proc_cadr,             ##called
      proc_cdar,             ##called
      proc_cddr,             ##called

      proc_memq,     ##called
      proc_memv,     ##called
      proc_member,   ##called
      proc_assq,     ##called
      proc_assv,     ##called
      proc_assoc,    ##called

*     Constructor
      proc_cons,           ##called
      proc_list            ##called,
      proc_make_list       ##called,
      proc_iota,           ##called
      proc_list_copy       ##called,

      proc_list_tail       ##called,
      proc_list_ref        ##called,
      proc_list_to_vector  ##called,

      proc_length    ##called,
      proc_nilp      ##called.

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

      proc_is_number       ##called,
      proc_is_integer      ##called,
      proc_is_rational     ##called,
      proc_is_real         ##called,
      proc_is_complex      ##called,
      proc_is_string       ##called,
      proc_is_char         ##called,
      proc_is_symbol       ##called,
      proc_is_hash         ##called,
      proc_is_procedure    ##called,
      proc_is_list         ##called,
      proc_is_pair         ##called,
      proc_is_boolean      ##called,
      proc_list_is_boolean ##called,
      proc_is_vector       ##called,
      proc_is_alist        ##called,
      proc_is_exact        ##called,
      proc_is_inexact      ##called,

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

      proc_is_zero,      ##called
      proc_is_positive,  ##called
      proc_is_negative,  ##called
      proc_is_odd,       ##called
      proc_is_even,      ##called

      proc_floor,        ##called
      proc_ceiling,      ##called
      proc_truncate,     ##called
      proc_round,        ##called

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

      proc_string            ##called,
      proc_make_string,      ##called
      proc_num_to_string,    ##called
      proc_list_to_string,   ##called
      proc_symbol_to_string, ##called
      proc_list_is_string,   ##called
      proc_string_length,    ##called
      proc_substring,        ##called
      proc_string_to_num,    ##called
      proc_string_ref,       ##called
      proc_string_set,       ##called
      proc_string_append,    ##called
      proc_string_to_list,   ##called
      proc_string_to_symbol, ##called

* Continuation
      proc_call_cc,          ##called
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
      proc_vector_ref,     ##called
      proc_vector_to_list. ##called

* Functions for dealing with hashes:
      _proc_meth:
      proc_make_hash,    ##called "Create new hash
      proc_hash_get,     ##called "Get an element from a hash
      proc_hash_insert,  ##called "Insert a new element into a hash
      proc_hash_remove,  ##called "Delete an item from a hash
      proc_hash_keys.    ##called "Delete an item from a hash

* Ports
      _proc_meth:
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

    PROTECTED SECTION.
      METHODS define_syntax
        IMPORTING element       TYPE REF TO lcl_lisp
                  environment   TYPE REF TO lcl_lisp_environment
        RETURNING VALUE(result) TYPE  REF TO lcl_lisp
        RAISING   lcx_lisp_exception.

      METHODS assign_symbol
        IMPORTING element       TYPE REF TO lcl_lisp
                  environment   TYPE REF TO lcl_lisp_environment
                  iv_macro      TYPE flag DEFAULT abap_false
        RETURNING VALUE(result) TYPE  REF TO lcl_lisp
        RAISING   lcx_lisp_exception.

      METHODS re_assign_symbol
        IMPORTING element       TYPE REF TO lcl_lisp
                  environment   TYPE REF TO lcl_lisp_environment
        RETURNING VALUE(result) TYPE  REF TO lcl_lisp
        RAISING   lcx_lisp_exception.

      METHODS is_macro_call
        IMPORTING element       TYPE REF TO lcl_lisp
                  environment   TYPE REF TO lcl_lisp_environment
        RETURNING VALUE(result) TYPE flag
        RAISING   lcx_lisp_exception.

      METHODS syntax_expand
        IMPORTING element       TYPE REF TO lcl_lisp
                  environment   TYPE REF TO lcl_lisp_environment
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
      CLASS-DATA: go_input_port  TYPE REF TO lcl_lisp_port,
                  go_output_port TYPE REF TO lcl_lisp_port,
                  go_error_port  TYPE REF TO lcl_lisp_port.
      CLASS-DATA gensym_counter TYPE i.

      METHODS write IMPORTING io_elem       TYPE REF TO lcl_lisp
                              io_arg        TYPE REF TO lcl_lisp DEFAULT lcl_lisp=>nil
                    RETURNING VALUE(result) TYPE REF TO lcl_lisp.

      METHODS display IMPORTING io_elem       TYPE REF TO lcl_lisp
                                io_arg        TYPE REF TO lcl_lisp
                      RETURNING VALUE(result) TYPE REF TO lcl_lisp.

      METHODS read IMPORTING io_arg        TYPE REF TO lcl_lisp
                   RETURNING VALUE(result) TYPE REF TO lcl_lisp
                   RAISING   lcx_lisp_exception.

      METHODS read_char IMPORTING io_arg        TYPE REF TO lcl_lisp
                        RETURNING VALUE(result) TYPE REF TO lcl_lisp
                        RAISING   lcx_lisp_exception.

    PRIVATE SECTION.
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

      METHODS eval_do_step IMPORTING io_command TYPE REF TO lcl_lisp
                                     io_steps   TYPE REF TO lcl_lisp
                                     io_env     TYPE REF TO lcl_lisp_environment
                           RAISING   lcx_lisp_exception.

      METHODS eval_do_init IMPORTING io_head       TYPE REF TO lcl_lisp
                                     VALUE(io_env) TYPE REF TO lcl_lisp_environment
                           EXPORTING eo_step       TYPE REF TO lcl_lisp
                                     eo_env        TYPE REF TO lcl_lisp_environment
                           RAISING   lcx_lisp_exception.

      METHODS eval_ast IMPORTING element       TYPE REF TO lcl_lisp
                                 environment   TYPE REF TO lcl_lisp_environment
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
      METHODS environment_named_let IMPORTING io_env        TYPE REF TO lcl_lisp_environment
                                    CHANGING  co_head       TYPE REF TO lcl_lisp
                                    RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment
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
                                   nesting       TYPE sytabix
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
                                  k             TYPE sytabix
                                  area          TYPE string
                        RETURNING VALUE(result) TYPE REF TO lcl_lisp
                        RAISING   lcx_lisp_exception.

      METHODS eval_hash IMPORTING element       TYPE REF TO lcl_lisp_vector
                                  environment   TYPE REF TO lcl_lisp_environment
                        RETURNING VALUE(result) TYPE REF TO lcl_lisp_vector
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
      mv_whitespace+1(1) = cl_abap_char_utilities=>newline.
      mv_whitespace+2(1) = cl_abap_char_utilities=>cr_lf(1).
      mv_whitespace+3(1) = cl_abap_char_utilities=>cr_lf(2).
      mv_whitespace+4(1) = cl_abap_char_utilities=>horizontal_tab.
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
        " Character constant  #; comment
        next_char( ).       " skip #
        next_char( ).       " skip ;
*       skip optional blanks, max. until end of line
        WHILE char CN mv_eol AND index LT length AND peek_char( ) EQ ` `.
          next_char( ).
        ENDWHILE.
*       skip one datum
        DATA sval TYPE string.
        match_atom( CHANGING cv_val = sval ).
        rv_has_next = skip_whitespace( ).
      ENDIF.

      IF char EQ c_lisp_hash AND rv_has_next EQ abap_true AND peek_char( ) EQ c_block_comment.
        next_char( ).   " skip |
*       match |#
        next_char( ).
*       skip block comment, from #| to |#
        WHILE index LT length AND NOT ( char EQ c_block_comment AND peek_char( ) EQ c_lisp_hash ).
          next_char( ).
        ENDWHILE.

        IF char EQ c_block_comment AND peek_char( ) EQ c_lisp_hash.
          next_char( ).
          next_char( ).
        ENDIF.

        rv_has_next = skip_whitespace( ).
      ENDIF.

    ENDMETHOD.                    "skip_whitespace

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
          lv_token = lv_token && code+lv_idx(1).
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
      CLEAR mt_labels.
      IF length = 0.
        APPEND lcl_lisp=>nil TO elements.
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

          IF lo_peek->type = lcl_lisp=>type_symbol AND lo_peek->value = c_lisp_dot.
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
      DATA pchar TYPE char1.
*     " is included in a string as \"

      next_char( ).                 " Skip past opening quote
      WHILE index < length AND NOT ( char = c_text_quote AND pchar NE c_escape_char ).
*         cv_val = |{ cv_val }{ char }|.
        CONCATENATE cv_val char INTO cv_val RESPECTING BLANKS.
        pchar = char.
        next_char( ).
      ENDWHILE.
      next_char( ).                 "Skip past closing quote
    ENDMETHOD.                    "match_string

    METHOD peek_label.
      rv_found = abap_false.
      CLEAR ev_label.

      DATA(lv_idx) = index.
      WHILE lv_idx < length.
        lv_idx = lv_idx + 1.
        DATA(lv_char) = code+lv_idx(1).
        IF lv_char CO '0123456789'.
          ev_label = |{ ev_label }{ lv_char }|.
        ELSEIF lv_char = c_lisp_equal AND ev_label IS NOT INITIAL.
          rv_found = abap_true.
        ELSE.
          RETURN.
        ENDIF.
      ENDWHILE.
    ENDMETHOD.

    METHOD match_atom.              " run_to_delimiter.
      WHILE index < length.
        cv_val = |{ cv_val }{ char }|.
        next_char( ).
        CHECK char CA mv_delimiters.
        EXIT.
      ENDWHILE.
      CONDENSE cv_val.
      IF cv_val = cl_abap_char_utilities=>newline.
        cv_val = space.
      ENDIF.
    ENDMETHOD.                    "run_to_delimiter

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
          element = lcl_lisp_new=>quote( parse_token( iv_literal = abap_true ) ).
          RETURN.

        WHEN c_lisp_backquote.     " Quasiquote, TO DO
          next_char( ).            " Skip past single quote
          element = lcl_lisp_new=>quasiquote( parse_token( ) ).
          RETURN.

        WHEN c_lisp_unquote.
          CASE peek_char( ).
            WHEN c_lisp_splicing.  " unquote-splicing
              next_char( ).        " Skip past ,@
              next_char( ).
              element = lcl_lisp_new=>splice_unquote( parse_token( ) ).  " token must evaluate to a list, not be a list

            WHEN OTHERS.           " unquote,  TO DO
              next_char( ).        " Skip past single quote
              element = lcl_lisp_new=>unquote( parse_token( ) ).

          ENDCASE.
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
              IF char EQ c_lisp_x.
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

            WHEN OTHERS.
*             Boolean #t #f
              "will be handled in match_atom( )

*              IF peek_bytevector( ).
**               Bytevector constant #u8( ... )
*
*              ENDIF.

*             Notation for numbers #e (exact) #i (inexact) #b (binary) #o (octal) #d (decimal) #x (hexadecimal)
*             further, instead of exp:  s (short), f (single), d (double), l (long)
*             positive infinity, negative infinity -inf / -inf.0, NaN +nan.0, positive zero, negative zero

*             Referencing other literal data #<n>= #<n>#
              DATA lv_label TYPE string.
              IF peek_label( IMPORTING ev_label = lv_label ).

                next_char( ).                 " Skip past hash
                WHILE index < length AND char CO '0123456789'.
                  next_char( ).
                ENDWHILE.
                next_char( ).                 "Skip past closing =

                element = parse_token( ).

*               Now Add: (set! |#{ lv_label }#| element)
                INSERT VALUE #( datum = |#{ lv_label }#|
                                element = element ) INTO TABLE mt_labels.
                RETURN.
              ENDIF.

          ENDCASE.

      ENDCASE.
*     Others
      match_atom( CHANGING cv_val = sval ).
      element = COND #( WHEN sval IS INITIAL
                        THEN lcl_lisp=>nil
                        ELSE lcl_lisp_new=>atom( sval ) ).

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
      go_input_port = go_output_port = go_error_port = io_port.
      gi_log = ii_log.

      env = lcl_lisp_environment=>new( ).

*     Create symbols for nil, true and false values
      nil = lcl_lisp=>nil.
      true = lcl_lisp=>true.
      false = lcl_lisp=>false.
      env->set( symbol = 'nil' element = nil ).
      env->set( symbol = '#f' element = false ).
      env->set( symbol = '#t' element = true ).

*     Add primitive functions to environment
      env->define_value( symbol = 'define'          type = lcl_lisp=>type_syntax value   = 'define' ).
      env->define_value( symbol = 'lambda'          type = lcl_lisp=>type_syntax value   = 'lambda' ).
      env->define_value( symbol = 'if'              type = lcl_lisp=>type_syntax value   = 'if' ).
      env->define_value( symbol = c_eval_quote      type = lcl_lisp=>type_syntax value   = `'` ).
      env->define_value( symbol = c_eval_quasiquote type = lcl_lisp=>type_syntax value   = '`' ).
      env->define_value( symbol = 'set!'            type = lcl_lisp=>type_syntax value   = 'set!' ).

      env->define_value( symbol = 'define-macro'    type = lcl_lisp=>type_syntax value   = 'define-macro' ).
      env->define_value( symbol = 'define-syntax'   type = lcl_lisp=>type_syntax value   = 'define-syntax' ).
      env->define_value( symbol = 'macroexpand'     type = lcl_lisp=>type_syntax value   = 'macroexpand' ).
      env->define_value( symbol = 'gensym'          type = lcl_lisp=>type_syntax value   = 'gensym' ).

      env->define_value( symbol = 'and'      type = lcl_lisp=>type_syntax value   = 'and' ).
      env->define_value( symbol = 'or'       type = lcl_lisp=>type_syntax value   = 'or' ).
      env->define_value( symbol = 'cond'     type = lcl_lisp=>type_syntax value   = 'cond' ).
      env->define_value( symbol = 'unless'   type = lcl_lisp=>type_syntax value   = 'unless' ).
      env->define_value( symbol = 'when'     type = lcl_lisp=>type_syntax value   = 'when' ).
      env->define_value( symbol = 'begin'    type = lcl_lisp=>type_syntax value   = 'begin' ).
      env->define_value( symbol = 'let'      type = lcl_lisp=>type_syntax value   = 'let' ).
      env->define_value( symbol = 'let*'     type = lcl_lisp=>type_syntax value   = 'let*' ).
      env->define_value( symbol = 'letrec'   type = lcl_lisp=>type_syntax value   = 'letrec' ).
      env->define_value( symbol = 'letrec*'  type = lcl_lisp=>type_syntax value   = 'letrec*' ).
      env->define_value( symbol = 'do'       type = lcl_lisp=>type_syntax value   = 'do' ).
      env->define_value( symbol = 'case'     type = lcl_lisp=>type_syntax value   = 'case' ).

      env->define_value( symbol = c_eval_unquote          type = lcl_lisp=>type_syntax value   = ',' ).
      env->define_value( symbol = c_eval_unquote_splicing type = lcl_lisp=>type_syntax value   = ',@' ).

*     Procedures
      env->define_value( symbol = 'apply'        type = lcl_lisp=>type_primitive value   = 'apply' ).
      env->define_value( symbol = 'for-each'     type = lcl_lisp=>type_primitive value   = 'for-each' ).
      env->define_value( symbol = 'map'          type = lcl_lisp=>type_primitive value   = 'map' ).

*     Add native functions to environment
      env->define_value( symbol = '+'        type = lcl_lisp=>type_native value   = 'PROC_ADD' ).
      env->define_value( symbol = '-'        type = lcl_lisp=>type_native value   = 'PROC_SUBTRACT' ).
      env->define_value( symbol = '*'        type = lcl_lisp=>type_native value   = 'PROC_MULTIPLY' ).
      env->define_value( symbol = '/'        type = lcl_lisp=>type_native value   = 'PROC_DIVIDE' ).
      env->define_value( symbol = 'append'   type = lcl_lisp=>type_native value   = 'PROC_APPEND' ).
      env->define_value( symbol = 'append!'  type = lcl_lisp=>type_native value   = 'PROC_APPEND_UNSAFE' ).
      env->define_value( symbol = 'list'     type = lcl_lisp=>type_native value   = 'PROC_LIST' ).
      env->define_value( symbol = 'length'   type = lcl_lisp=>type_native value   = 'PROC_LENGTH' ).
      env->define_value( symbol = 'reverse'  type = lcl_lisp=>type_native value   = 'PROC_REVERSE' ).
      env->define_value( symbol = 'not'      type = lcl_lisp=>type_native value   = 'PROC_NOT' ).

      env->define_value( symbol = 'make-list'    type = lcl_lisp=>type_native value   = 'PROC_MAKE_LIST' ).
      env->define_value( symbol = 'list-tail'    type = lcl_lisp=>type_native value   = 'PROC_LIST_TAIL' ).
      env->define_value( symbol = 'list-ref'     type = lcl_lisp=>type_native value   = 'PROC_LIST_REF' ).
      env->define_value( symbol = 'list-copy'    type = lcl_lisp=>type_native value   = 'PROC_LIST_COPY' ).
      env->define_value( symbol = 'list->vector' type = lcl_lisp=>type_native value   = 'PROC_LIST_TO_VECTOR' ).
      env->define_value( symbol = 'iota'         type = lcl_lisp=>type_native value   = 'PROC_IOTA' ).

      env->define_value( symbol = 'memq'    type = lcl_lisp=>type_native value   = 'PROC_MEMQ' ).
      env->define_value( symbol = 'memv'    type = lcl_lisp=>type_native value   = 'PROC_MEMV' ).
      env->define_value( symbol = 'member'  type = lcl_lisp=>type_native value   = 'PROC_MEMBER' ).

      env->define_value( symbol = 'assq'    type = lcl_lisp=>type_native value   = 'PROC_ASSQ' ).
      env->define_value( symbol = 'assv'    type = lcl_lisp=>type_native value   = 'PROC_ASSV' ).
      env->define_value( symbol = 'assoc'   type = lcl_lisp=>type_native value   = 'PROC_ASSOC' ).

      env->define_value( symbol = 'car'     type = lcl_lisp=>type_native value   = 'PROC_CAR' ).
      env->define_value( symbol = 'cdr'     type = lcl_lisp=>type_native value   = 'PROC_CDR' ).
      env->define_value( symbol = 'cons'    type = lcl_lisp=>type_native value   = 'PROC_CONS' ).
      env->define_value( symbol = 'nil?'    type = lcl_lisp=>type_native value   = 'PROC_NILP' ).
      env->define_value( symbol = 'null?'   type = lcl_lisp=>type_native value   = 'PROC_NILP' ).

      env->define_value( symbol = '>'       type = lcl_lisp=>type_native value   = 'PROC_GT' ).
      env->define_value( symbol = '>='      type = lcl_lisp=>type_native value   = 'PROC_GTE' ).
      env->define_value( symbol = '<'       type = lcl_lisp=>type_native value   = 'PROC_LT' ).
      env->define_value( symbol = '<='      type = lcl_lisp=>type_native value   = 'PROC_LTE' ).
      env->define_value( symbol = '='       type = lcl_lisp=>type_native value   = 'PROC_EQL' ). "Math equal
      env->define_value( symbol = 'eq?'     type = lcl_lisp=>type_native value   = 'PROC_EQ' ).
      env->define_value( symbol = 'eqv?'    type = lcl_lisp=>type_native value   = 'PROC_EQV' ).
      env->define_value( symbol = 'equal?'  type = lcl_lisp=>type_native value   = 'PROC_EQUAL' ).

      env->define_value( symbol = 'set-car!' type = lcl_lisp=>type_native value   = 'PROC_SET_CAR' ).
      env->define_value( symbol = 'set-cdr!' type = lcl_lisp=>type_native value   = 'PROC_SET_CDR' ).
      env->define_value( symbol = 'caar'     type = lcl_lisp=>type_native value   = 'PROC_CAAR' ).
      env->define_value( symbol = 'cadr'     type = lcl_lisp=>type_native value   = 'PROC_CADR' ).
      env->define_value( symbol = 'cdar'     type = lcl_lisp=>type_native value   = 'PROC_CDAR' ).
      env->define_value( symbol = 'cddr'     type = lcl_lisp=>type_native value   = 'PROC_CDDR' ).

      env->define_value( symbol = 'current-input-port'  type = lcl_lisp=>type_native value = 'PROC_CURRENT_INPUT_PORT' ).
      env->define_value( symbol = 'current-output-port' type = lcl_lisp=>type_native value = 'PROC_CURRENT_OUTPUT_PORT' ).
      env->define_value( symbol = 'current-error-port'  type = lcl_lisp=>type_native value = 'PROC_CURRENT_ERROR_PORT' ).

      env->define_value( symbol = 'close-input-port'  type = lcl_lisp=>type_native value = 'PROC_CLOSE_INPUT_PORT' parameter = abap_true ).
      env->define_value( symbol = 'close-output-port' type = lcl_lisp=>type_native value = 'PROC_CLOSE_OUTPUT_PORT' parameter = abap_true ).
      env->define_value( symbol = 'close-port'        type = lcl_lisp=>type_native value = 'PROC_CLOSE_PORT' parameter = abap_true ).

*     vector-related functions
      env->define_value( symbol = 'vector'        type = lcl_lisp=>type_native value   = 'PROC_VECTOR' ).
      env->define_value( symbol = 'vector-length' type = lcl_lisp=>type_native value   = 'PROC_VECTOR_LENGTH' ).
      env->define_value( symbol = 'vector-set!'   type = lcl_lisp=>type_native value   = 'PROC_VECTOR_SET' ).
      env->define_value( symbol = 'vector-ref'    type = lcl_lisp=>type_native value   = 'PROC_VECTOR_REF' ).
      env->define_value( symbol = 'vector->list'  type = lcl_lisp=>type_native value   = 'PROC_VECTOR_TO_LIST' ).
      env->define_value( symbol = 'make-vector'   type = lcl_lisp=>type_native value   = 'PROC_MAKE_VECTOR' ).

*     Hash-related functions
      env->define_value( symbol = 'make-hash'   type = lcl_lisp=>type_native value   = 'PROC_MAKE_HASH' ).
      env->define_value( symbol = 'hash-get'    type = lcl_lisp=>type_native value   = 'PROC_HASH_GET' ).
      env->define_value( symbol = 'hash-insert' type = lcl_lisp=>type_native value   = 'PROC_HASH_INSERT' ).
      env->define_value( symbol = 'hash-remove' type = lcl_lisp=>type_native value   = 'PROC_HASH_REMOVE' ).
      env->define_value( symbol = 'hash-keys'   type = lcl_lisp=>type_native value   = 'PROC_HASH_KEYS' ).
*     Functions for type:
      env->define_value( symbol = 'string?'     type = lcl_lisp=>type_native value = 'PROC_IS_STRING' ).
      env->define_value( symbol = 'char?'       type = lcl_lisp=>type_native value = 'PROC_IS_CHAR' ).
      env->define_value( symbol = 'hash?'       type = lcl_lisp=>type_native value = 'PROC_IS_HASH' ).
      env->define_value( symbol = 'number?'     type = lcl_lisp=>type_native value = 'PROC_IS_NUMBER' ).
      env->define_value( symbol = 'integer?'    type = lcl_lisp=>type_native value = 'PROC_IS_INTEGER' ).
      env->define_value( symbol = 'complex?'    type = lcl_lisp=>type_native value = 'PROC_IS_COMPLEX' ).
      env->define_value( symbol = 'real?'       type = lcl_lisp=>type_native value = 'PROC_IS_REAL' ).
      env->define_value( symbol = 'rational?'   type = lcl_lisp=>type_native value = 'PROC_IS_RATIONAL' ).
      env->define_value( symbol = 'list?'       type = lcl_lisp=>type_native value = 'PROC_IS_LIST' ).
      env->define_value( symbol = 'pair?'       type = lcl_lisp=>type_native value = 'PROC_IS_PAIR' ).
      env->define_value( symbol = 'vector?'     type = lcl_lisp=>type_native value = 'PROC_IS_VECTOR' ).
      env->define_value( symbol = 'boolean?'    type = lcl_lisp=>type_native value = 'PROC_IS_BOOLEAN' ).
      env->define_value( symbol = 'alist?'      type = lcl_lisp=>type_native value = 'PROC_IS_ALIST' ).
      env->define_value( symbol = 'procedure?'  type = lcl_lisp=>type_native value = 'PROC_IS_PROCEDURE' ).
      env->define_value( symbol = 'symbol?'     type = lcl_lisp=>type_native value = 'PROC_IS_SYMBOL' ).
      env->define_value( symbol = 'port?'       type = lcl_lisp=>type_native value = 'PROC_IS_PORT' ).
      env->define_value( symbol = 'boolean=?'   type = lcl_lisp=>type_native value = 'PROC_LIST_IS_BOOLEAN' ).
      env->define_value( symbol = 'string=?'    type = lcl_lisp=>type_native value = 'PROC_LIST_IS_STRING' ).
      env->define_value( symbol = 'exact?'      type = lcl_lisp=>type_native value = 'PROC_IS_EXACT' ).
      env->define_value( symbol = 'inexact?'    type = lcl_lisp=>type_native value = 'PROC_IS_INEXACT' ).

*     Format
      env->define_value( symbol = 'newline'     type = lcl_lisp=>type_native value = 'PROC_NEWLINE' ).
      env->define_value( symbol = 'write'       type = lcl_lisp=>type_native value = 'PROC_WRITE' ).
      env->define_value( symbol = 'display'     type = lcl_lisp=>type_native value = 'PROC_DISPLAY' ).

      env->define_value( symbol = 'read'         type = lcl_lisp=>type_native value = 'PROC_READ' ).
      env->define_value( symbol = 'write-string' type = lcl_lisp=>type_native value = 'PROC_WRITE_STRING' ).
      env->define_value( symbol = 'write-char'   type = lcl_lisp=>type_native value = 'PROC_WRITE_CHAR' ).
      env->define_value( symbol = 'read-char'    type = lcl_lisp=>type_native value = 'PROC_READ_CHAR' ).
      env->define_value( symbol = 'char-ready?'  type = lcl_lisp=>type_native value = 'PROC_IS_CHAR_READY' ).
      env->define_value( symbol = 'peek-char'    type = lcl_lisp=>type_native value = 'PROC_PEEK_CHAR' ).

      env->define_value( symbol = 'string->number' type = lcl_lisp=>type_native value = 'PROC_STRING_TO_NUM' ).
      env->define_value( symbol = 'make-string'    type = lcl_lisp=>type_native value = 'PROC_MAKE_STRING' ).

      env->define_value( symbol = 'number->string' type = lcl_lisp=>type_native value = 'PROC_NUM_TO_STRING' ).
      env->define_value( symbol = 'string->number' type = lcl_lisp=>type_native value = 'PROC_STRING_TO_NUM' ).
      env->define_value( symbol = 'make-string'    type = lcl_lisp=>type_native value = 'PROC_MAKE_STRING' ).
      env->define_value( symbol = 'string'         type = lcl_lisp=>type_native value = 'PROC_STRING' ).
      env->define_value( symbol = 'string->list'   type = lcl_lisp=>type_native value = 'PROC_STRING_TO_LIST' ).
      env->define_value( symbol = 'list->string'   type = lcl_lisp=>type_native value = 'PROC_LIST_TO_STRING' ).
      env->define_value( symbol = 'symbol->string' type = lcl_lisp=>type_native value = 'PROC_SYMBOL_TO_STRING' ).
      env->define_value( symbol = 'string->symbol' type = lcl_lisp=>type_native value = 'PROC_STRING_TO_SYMBOL' ).
      env->define_value( symbol = 'string-append'  type = lcl_lisp=>type_native value = 'PROC_STRING_APPEND' ).
      env->define_value( symbol = 'string-length'  type = lcl_lisp=>type_native value = 'PROC_STRING_LENGTH' ).
      env->define_value( symbol = 'string-ref'     type = lcl_lisp=>type_native value = 'PROC_STRING_REF' ).
      env->define_value( symbol = 'string-set!'    type = lcl_lisp=>type_native value = 'PROC_STRING_SET' ).
*     Math
      env->define_value( symbol = 'abs'   type = lcl_lisp=>type_native value = 'PROC_ABS' ).
      env->define_value( symbol = 'sin'   type = lcl_lisp=>type_native value = 'PROC_SIN' ).
      env->define_value( symbol = 'cos'   type = lcl_lisp=>type_native value = 'PROC_COS' ).
      env->define_value( symbol = 'tan'   type = lcl_lisp=>type_native value = 'PROC_TAN' ).
      env->define_value( symbol = 'asin'  type = lcl_lisp=>type_native value = 'PROC_ASIN' ).
      env->define_value( symbol = 'acos'  type = lcl_lisp=>type_native value = 'PROC_ACOS' ).
      env->define_value( symbol = 'atan'  type = lcl_lisp=>type_native value = 'PROC_ATAN' ).
      env->define_value( symbol = 'sinh'  type = lcl_lisp=>type_native value = 'PROC_SINH' ).
      env->define_value( symbol = 'cosh'  type = lcl_lisp=>type_native value = 'PROC_COSH' ).
      env->define_value( symbol = 'tanh'  type = lcl_lisp=>type_native value = 'PROC_TANH' ).
      env->define_value( symbol = 'asinh' type = lcl_lisp=>type_native value = 'PROC_ASINH' ).
      env->define_value( symbol = 'acosh' type = lcl_lisp=>type_native value = 'PROC_ACOSH' ).
      env->define_value( symbol = 'atanh' type = lcl_lisp=>type_native value = 'PROC_ATANH' ).
      env->define_value( symbol = 'expt'  type = lcl_lisp=>type_native value = 'PROC_EXPT' ).
      env->define_value( symbol = 'exp'   type = lcl_lisp=>type_native value = 'PROC_EXP' ).
      env->define_value( symbol = 'log'   type = lcl_lisp=>type_native value = 'PROC_LOG' ).
      env->define_value( symbol = 'sqrt'  type = lcl_lisp=>type_native value = 'PROC_SQRT' ).

      env->define_value( symbol = 'floor'    type = lcl_lisp=>type_native value = 'PROC_FLOOR' ).
      env->define_value( symbol = 'ceiling'  type = lcl_lisp=>type_native value = 'PROC_CEILING' ).
      env->define_value( symbol = 'truncate' type = lcl_lisp=>type_native value = 'PROC_TRUNCATE' ).
      env->define_value( symbol = 'round'    type = lcl_lisp=>type_native value = 'PROC_ROUND' ).

      env->define_value( symbol = 'remainder' type = lcl_lisp=>type_native value = 'PROC_REMAINDER' ).
      env->define_value( symbol = 'modulo'    type = lcl_lisp=>type_native value = 'PROC_MODULO' ).
      env->define_value( symbol = 'quotient'  type = lcl_lisp=>type_native value = 'PROC_QUOTIENT' ).
      env->define_value( symbol = 'random'    type = lcl_lisp=>type_native value = 'PROC_RANDOM' ).
      env->define_value( symbol = 'max'       type = lcl_lisp=>type_native value = 'PROC_MAX' ).
      env->define_value( symbol = 'min'       type = lcl_lisp=>type_native value = 'PROC_MIN' ).
      env->define_value( symbol = 'gcd'       type = lcl_lisp=>type_native value = 'PROC_GCD' ).
      env->define_value( symbol = 'lcm'       type = lcl_lisp=>type_native value = 'PROC_LCM' ).

      env->define_value( symbol = 'zero?'     type = lcl_lisp=>type_native value = 'PROC_IS_ZERO' ).
      env->define_value( symbol = 'positive?' type = lcl_lisp=>type_native value = 'PROC_IS_POSITIVE' ).
      env->define_value( symbol = 'negative?' type = lcl_lisp=>type_native value = 'PROC_IS_NEGATIVE' ).
      env->define_value( symbol = 'odd?'      type = lcl_lisp=>type_native value = 'PROC_IS_ODD' ).
      env->define_value( symbol = 'even?'     type = lcl_lisp=>type_native value = 'PROC_IS_EVEN' ).
*     Continuation
      env->define_value( symbol = 'call-with-current-continuation' type = lcl_lisp=>type_native value = 'PROC_CALL_CC' ).
      env->define_value( symbol = 'call/cc'                        type = lcl_lisp=>type_native value = 'PROC_CALL_CC' ).

*     Native functions for ABAP integration
      env->define_value( symbol = 'ab-data'       type = lcl_lisp=>type_native value   = 'PROC_ABAP_DATA' ).
      env->define_value( symbol = 'ab-function'   type = lcl_lisp=>type_native value   = 'PROC_ABAP_FUNCTION' ).
      env->define_value( symbol = 'ab-func-param' type = lcl_lisp=>type_native value   = 'PROC_ABAP_FUNCTION_PARAM' ).
      env->define_value( symbol = 'ab-table'      type = lcl_lisp=>type_native value   = 'PROC_ABAP_TABLE' ).
      env->define_value( symbol = 'ab-append-row' type = lcl_lisp=>type_native value   = 'PROC_ABAP_APPEND_ROW' ).
      env->define_value( symbol = 'ab-delete-row' type = lcl_lisp=>type_native value   = 'PROC_ABAP_DELETE_ROW' ).
      env->define_value( symbol = 'ab-get-row'    type = lcl_lisp=>type_native value   = 'PROC_ABAP_GET_ROW' ).
      env->define_value( symbol = 'ab-get-value'  type = lcl_lisp=>type_native value   = 'PROC_ABAP_GET_VALUE' ).
      env->define_value( symbol = 'ab-set-value'  type = lcl_lisp=>type_native value   = 'PROC_ABAP_SET_VALUE' ).

      env->define_value( symbol = 'ab-get' type = lcl_lisp=>type_native value = 'PROC_ABAP_GET' ).
      env->define_value( symbol = 'ab-set' type = lcl_lisp=>type_native value = 'PROC_ABAP_SET' ).

*     Compatibility
      env->define_value( symbol = 'empty?'  type = lcl_lisp=>type_native value   = 'PROC_NILP' ).
      env->define_value( symbol = 'first'   type = lcl_lisp=>type_native value   = 'PROC_CAR' ).
      env->define_value( symbol = 'rest'    type = lcl_lisp=>type_native value   = 'PROC_CDR' ).

*     Ports
      env->define_value( symbol = 'input-port?'         type = lcl_lisp=>type_native value   = 'PROC_IS_INPUT_PORT' ).
      env->define_value( symbol = 'output-port?'        type = lcl_lisp=>type_native value   = 'PROC_IS_OUTPUT_PORT' ).
      env->define_value( symbol = 'textual-port?'       type = lcl_lisp=>type_native value   = 'PROC_IS_TEXTUAL_PORT' ).
      env->define_value( symbol = 'binary-port?'        type = lcl_lisp=>type_native value   = 'PROC_IS_BINARY_PORT' ).
      env->define_value( symbol = 'input-port-open?'    type = lcl_lisp=>type_native value   = 'PROC_IS_OPEN_INPUT_PORT' ).
      env->define_value( symbol = 'output-port-open?'   type = lcl_lisp=>type_native value   = 'PROC_IS_OPEN_OUTPUT_PORT' ).
      env->define_value( symbol = 'eof-object?'         type = lcl_lisp=>type_native value   = 'PROC_IS_EOF_OBJECT' ).
      env->define_value( symbol = 'open-output-string'  type = lcl_lisp=>type_native value   = 'PROC_OPEN_OUTPUT_STRING' ).
      env->define_value( symbol = 'open-input-string'   type = lcl_lisp=>type_native value   = 'PROC_OPEN_INPUT_STRING' ).
      env->define_value( symbol = 'get-output-string'   type = lcl_lisp=>type_native value   = 'PROC_GET_OUTPUT_STRING' ).
      env->define_value( symbol = 'eof-object'          type = lcl_lisp=>type_native value   = 'PROC_EOF_OBJECT' ).

      env->define_value( symbol = 'char-alphabetic?'  type = lcl_lisp=>type_native value   = 'PROC_IS_CHAR_ALPHABETIC' ).
      env->define_value( symbol = 'char-numeric?'     type = lcl_lisp=>type_native value   = 'PROC_IS_CHAR_NUMERIC' ).
      env->define_value( symbol = 'char-whitespace?'  type = lcl_lisp=>type_native value   = 'PROC_IS_CHAR_WHITESPACE' ).
      env->define_value( symbol = 'char-upper-case?'  type = lcl_lisp=>type_native value   = 'PROC_IS_CHAR_UPPER_CASE' ).
      env->define_value( symbol = 'char-lower-case?'  type = lcl_lisp=>type_native value   = 'PROC_IS_CHAR_LOWER_CASE' ).

      env->define_value( symbol = 'digit-value'       type = lcl_lisp=>type_native value   = 'PROC_DIGIT_VALUE' ).
      env->define_value( symbol = 'char->integer'     type = lcl_lisp=>type_native value   = 'PROC_CHAR_TO_INTEGER' ).
      env->define_value( symbol = 'integer->char'     type = lcl_lisp=>type_native value   = 'PROC_INTEGER_TO_CHAR' ).
      env->define_value( symbol = 'char-upcase'       type = lcl_lisp=>type_native value   = 'PROC_CHAR_UPCASE' ).
      env->define_value( symbol = 'char-downcase'     type = lcl_lisp=>type_native value   = 'PROC_CHAR_DOWNCASE' ).

      env->define_value( symbol = 'sql-query'         type = lcl_lisp=>type_native value   = 'PROC_SQL_QUERY' ).
      env->define_value( symbol = 'define-query'      type = lcl_lisp=>type_native value   = 'PROC_SQL_PREPARE' ).

      DATA lr_ref TYPE REF TO data.
*     Define a value in the environment for SYST
      GET REFERENCE OF syst INTO lr_ref.
      env->set( symbol = 'ab-sy' element = lcl_lisp_new=>data( lr_ref ) ).
    ENDMETHOD.                    "constructor

    METHOD throw.
      lcl_lisp=>throw( message ).
    ENDMETHOD.                    "throw

    METHOD assign_symbol.
*     Scheme does not return a value for define; but we are returning the new symbol reference
      DATA(lo_head) = element->car.
      CASE lo_head->type.
        WHEN lcl_lisp=>type_symbol.
*         call the set method of the current environment using the unevaluated first parameter
*         (second list element) as the symbol key and the evaluated second parameter as the value.
          DATA(lo_params) = eval( element = element->cdr->car
                                  environment = environment ).
          lo_params->macro = iv_macro.
          environment->set( symbol  = lo_head->value
                            element = lo_params ).
          result = lcl_lisp_new=>symbol( lo_head->value ).

*       Function shorthand (define (id arg ... ) body ...+)
        WHEN lcl_lisp=>type_pair.
          IF element->cdr EQ nil.
            throw( |{ lo_head->to_string( ) } no expression in body| ).
          ENDIF.
*         define's function shorthand allows us to define a function by specifying a list as the
*         first argument where the first element is a symbol and consecutive elements are arguments
          result = lcl_lisp_new=>lambda( io_car = lo_head->cdr  "List of params following function symbol
                                         io_cdr = element->cdr
                                         io_env = environment
                                         iv_macro = iv_macro ).
*         Add function to the environment with symbol
          environment->set( symbol  = lo_head->car->value
                            element = result ).

          result = lcl_lisp_new=>symbol( lo_head->car->value ).
        WHEN OTHERS.
          throw( |{ lo_head->to_string( ) } cannot be a variable identifier| ).
      ENDCASE.
    ENDMETHOD.                    "assign_symbol

    METHOD define_syntax.
*
      result = assign_symbol( element = element
                              environment = environment
                              iv_macro = abap_true ).
    ENDMETHOD.

    METHOD is_macro_call.
*     returns true if element is a list that contains a symbol as the first element and that symbol refers to a function
*     in the environment and that function has the macro attribute set to true. Otherwise, it returns false.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      validate element.
      result = abap_false.
      CHECK element->type EQ lcl_lisp=>type_pair AND element->car->type = lcl_lisp=>type_symbol.
      TRY.
          lo_ptr = environment->get( element->car->value ).
          CHECK lo_ptr->is_procedure( ) EQ true.
          result = lo_ptr->macro.
        CATCH lcx_lisp_exception.
          RETURN.
      ENDTRY.
    ENDMETHOD.

    METHOD syntax_expand.
      DATA lo_lambda TYPE REF TO lcl_lisp.
      DATA lo_args TYPE REF TO lcl_lisp.
      DATA lo_env TYPE REF TO lcl_lisp_environment.
      validate element.

      result = element.
      WHILE is_macro_call( element = result
                           environment = environment ).
        lo_lambda = environment->get( result->car->value ).
        lo_args = result->cdr.
        lo_env = lo_lambda->environment.

        lo_env->parameters_to_symbols( io_args = lo_args             " Pointer to argument
                                       io_pars = lo_lambda->car ).   " Pointer to formal parameters

        result = eval( element = lcl_lisp_new=>cons( io_car = lo_lambda
                                                     io_cdr = lo_args )
                       environment = lo_env ).
      ENDWHILE.
    ENDMETHOD.

    METHOD generate_symbol.
      DATA lv_index TYPE i.
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

      IF lo_opt IS BOUND AND lo_opt->type EQ lcl_lisp=>type_integer.
        lo_int ?= lo_opt.
        IF lo_int->integer GE 0.
          lv_counter = lo_int->integer.
        ENDIF.
      ENDIF.

      IF lo_opt IS BOUND AND lo_opt->type EQ lcl_lisp=>type_string.
        lo_string ?= lo_opt.
        lv_suffix = lo_string->value.
      ENDIF.

      DATA(lv_name) = |#:{ lv_suffix }{ lv_counter }|.
      result = lcl_lisp_new=>symbol( value = lv_name
                                     index = lv_index ). " uninterned symbols have integer > 0
    ENDMETHOD.

    METHOD re_assign_symbol.
      result = element->car.
      CASE result->type.
        WHEN lcl_lisp=>type_symbol.
*         re-define symbol in the original environment, but evaluate parameters in the current environment
          environment->find( result->value )->set( symbol  = result->value
                                                   element = eval( element = element->cdr->car
                                                                   environment = environment ) ).
        WHEN OTHERS.
          throw( |{ result->to_string( ) } must be a symbol| ).
      ENDCASE.
    ENDMETHOD.                    "re_assign_symbol

    METHOD evaluate_parameters.
*     This routine is called very, very often!
      DATA lo_arg TYPE REF TO lcl_lisp.
      DATA lo_new TYPE REF TO lcl_lisp.
*     Before execution of the procedure or lambda, all parameters must be evaluated
      validate io_list.
      ro_head = nil.
      CHECK io_list NE nil. " AND io_list->car NE nil.

      DATA(elem) = io_list.
*     TO DO: check if circular list are allowed
      WHILE elem->type EQ lcl_lisp=>type_pair.
        lo_new = lcl_lisp_new=>cons( io_car = eval_ast( element = elem->car
                                                        environment = environment ) ).
        IF ro_head = nil.
          lo_arg = ro_head = lo_new.
        ELSE.
          lo_arg = lo_arg->cdr = lo_new.
        ENDIF.
        elem = elem->cdr.
      ENDWHILE.

      validate_tail elem io_list space.

    ENDMETHOD.                    "evaluate_parameters

    METHOD expand_apply.
      validate io_list.
      DATA(lo_proc) = io_list->car.     " proc
      DATA(lo_arg) = io_list->cdr.      " handle arg1 . . . rest-args

      validate lo_arg.
*     (apply proc arg1 . . . argn rest)
*     Parameter io_list is list arg1 ... argn rest

      result = io_list.
      CHECK lo_arg NE nil.

*     At least one argument = rest
      DATA(lo_new) = lcl_lisp_new=>cons( io_car = lo_proc ).
      result = lo_new.

*     Collect arg1 to argn
*     TO DO: check if circular lists are allowed
      WHILE lo_arg->cdr->type EQ lcl_lisp=>type_pair.
*       At least two entries (argn and rest), build (list arg1 . . argn )

        lo_new = lo_new->cdr = lcl_lisp_new=>cons( io_car = lo_arg->car ).
        lo_arg = lo_arg->cdr.
      ENDWHILE.

*     now (append (list arg1 . . argn ) rest )
      DATA(lo_rest) = eval_ast( element = lo_arg->car
                                environment = environment ).

*     TO DO: check if circular lists are allowed
      WHILE lo_rest->type EQ lcl_lisp=>type_pair.  " e.g. NE nil
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
* Proc is always called in the same dynamic environment as map itself. The order in which proc is applied to the elements of the
* list s is unspecified. If multiple returns occur from map, the values returned by earlier returns are not mutated.
      DATA lo_map TYPE REF TO lcl_lisp.
      validate: io_list, io_list->car.

      result = nil.
      DATA(lo_proc) = io_list->car.

      DATA(lt_list) = table_of_lists( io_head = io_list->cdr         " parameter evaluated lists
                                      environment = environment ).

      DATA(lv_has_next) = xsdbool( lines( lt_list ) GT 0 ). " map terminates when the shortest list runs out.

      WHILE lv_has_next EQ abap_true.
        DATA(lo_next) = eval( element = map_next_expr( EXPORTING io_proc = lo_proc
                                                       IMPORTING ev_has_next = lv_has_next
                                                       CHANGING  ct_list = lt_list )
                              environment = environment ).
        DATA(lo_head) = lcl_lisp_new=>cons( io_car = lo_next ).
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
      validate: io_list, io_list->car.

      result = nil.
      DATA(lo_proc) = io_list->car.

      DATA(lt_list) = table_of_lists( io_head = io_list->cdr
                                      environment = environment ).

      DATA(lv_has_next) = xsdbool( lines( lt_list ) GT 0 ).  " for-each terminates when the shortest list runs out.
      WHILE lv_has_next EQ abap_true.
*       evaluate function call (proc list1[k] list2[k]... listn[k])
        DATA(lo_head) = map_next_expr( EXPORTING io_proc = lo_proc
                                       IMPORTING ev_has_next = lv_has_next
                                       CHANGING  ct_list = lt_list ).
        result = eval( element = lo_head
                       environment = environment ).
      ENDWHILE.
    ENDMETHOD.

    METHOD eval_do_init.
*     <init> expressions are evaluated (in unspecified order), the <variable>s are bound to fresh locations,
*     the results of the <init> expressions are stored in the bindings of the <variable>s.
*     A <step> can be omitted, in which case the effect is the same as if (<variable> <init> <variable>)
*     had been written instead of (<variable> <init>).
      validate io_head.

      eo_env = lcl_lisp_environment=>new( io_env ).
      eo_step = nil.

      DATA(lo_loop) = io_head.
      WHILE lo_loop->type EQ lcl_lisp=>type_pair.
        DATA(lo_spec) = lo_loop->car.
*       max. 3 entries
*       <variable>
        DATA(lo_var) = lo_spec->car.

*       <init>

        lo_spec = lo_spec->cdr.
        IF lo_spec NE nil.
          DATA(lo_init) = lo_spec->car.

          eo_env->set_once( symbol = lo_var->value
                            element = eval_ast( element = lo_init    " inits are evaluated in org. environment
                                                environment = io_env ) ).
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
      WHILE lo_command->type EQ lcl_lisp=>type_pair.
        eval( element = lo_command->car
              environment = io_env ).
        lo_command = lo_command->cdr.
      ENDWHILE.

      DATA(lo_local_env) = lcl_lisp_environment=>new( ).
*     the <step> expressions are evaluated in some unspecified order
      DATA(lo_step) = io_steps.
      WHILE lo_step->type EQ lcl_lisp=>type_pair.
        DATA(lo_ptr) = lo_step->car.

*       <variable>s are bound to fresh locations to avoid dependencies in the next step
        lo_local_env->set( symbol = lo_ptr->car->value
                           element = eval_ast( element = lo_ptr->cdr
                                                environment = io_env ) ).
        lo_step = lo_step->cdr.
      ENDWHILE.

      lo_step = io_steps.
      WHILE lo_step->type EQ lcl_lisp=>type_pair.
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
      validate io_head.
      result = nil.
      eo_elem = io_head.

      CHECK io_head NE nil.

      WHILE eo_elem IS BOUND AND eo_elem->type EQ lcl_lisp=>type_pair
        AND eo_elem->cdr NE nil.  " Do not evaluate the last list element

        result = eval_ast( element = eo_elem->car
                           environment = io_environment ).
        eo_elem = eo_elem->cdr.
      ENDWHILE.

      validate_tail eo_elem->cdr io_head space.
    ENDMETHOD.

*    METHOD eval_list.
*      validate io_head.
*      result = nil.
*
*      DATA(elem) = io_head.
*      WHILE elem IS BOUND AND elem->type EQ lcl_lisp=>type_pair.
*        result = eval_ast( element = elem->car
*                           environment = io_environment ).
*        elem = elem->cdr.
*      ENDWHILE.
*
*      validate_tail elem io_head space.
*    ENDMETHOD.

    METHOD lambda_environment.
      DATA lo_args TYPE REF TO lcl_lisp.
*     The function (LAMBDA) receives its own local environment in which to execute,
*     where parameters become symbols that are mapped to the corresponding arguments
      validate io_head.
      ro_env = lcl_lisp_environment=>new( io_head->environment ).

      IF io_head->macro EQ abap_true.
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
      validate io_head.
      eo_args = eo_pars = nil.                "list of parameters

      CHECK io_head->car IS BOUND AND io_head->car NE nil.
      DATA(lo_ptr) = io_head->car.

      validate lo_ptr->car.
      lo_par = lcl_lisp_new=>cons( io_car = lo_ptr->car ).

      IF lo_ptr->cdr IS BOUND AND lo_ptr->cdr NE nil.
        lo_arg = lcl_lisp_new=>cons( io_car = lo_ptr->cdr->car ).
      ENDIF.

      eo_pars = lo_par.
      eo_args = lo_arg.

      lo_ptr = io_head->cdr.
      WHILE lo_ptr->type EQ lcl_lisp=>type_pair. " IS BOUND AND lo_ptr NE nil.
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
      validate: io_args, io_pars.
      DATA(lo_args) = io_args->new_iterator( ).
      DATA(lo_pars) = io_pars->new_iterator( ).

      WHILE lo_args->has_next( ) AND lo_pars->has_next( ).
        DATA(lo_par) = lo_pars->next( ).
        CHECK lo_par NE nil.        " Nil would mean no parameters to map
*       Assign argument to its corresponding symbol in the newly created environment
*       NOTE: element of the argument list is evaluated before being defined in the environment
        io_env->set( symbol = lo_par->value
                     element = eval( element = lo_args->next( )
                                     environment = io_env ) ).
      ENDWHILE.
    ENDMETHOD.

* A letrec expression is equivalent to a let where the bindings are initialized with dummy values,
* and then the initial values are computed and assigned into the bindings.
    METHOD environment_letrec.
      extract_arguments( EXPORTING io_head = io_head
                         IMPORTING eo_pars = DATA(lo_pars)
                                   eo_args = DATA(lo_args) ).

      ro_env = lcl_lisp_environment=>new( io_env ).
*     setup the environment before evaluating the initial value expressions
      DATA(lo_par) = lo_pars.
      DATA(lo_arg) = lo_args.
      WHILE lo_par IS BOUND AND lo_par NE nil   " Nil means no parameters to map
        AND lo_arg IS BOUND AND lo_arg NE nil.  " Nil means no parameters to map

        ro_env->set_once( symbol = lo_par->car->value
                          element = lo_arg->car ).
        lo_par = lo_par->cdr.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

*     the initial value computions execute inside the new environment
      DATA(lo_new_args) = evaluate_parameters( io_list = lo_args           " Pointer to arguments
                                               environment = ro_env ).

      ro_env->parameters_to_symbols( io_args = lo_new_args
                                     io_pars = lo_pars ).   " Pointer to formal parameters
    ENDMETHOD.

    METHOD environment_letrec_star.
      extract_arguments( EXPORTING io_head = io_head
                         IMPORTING eo_pars = DATA(lo_pars)
                                   eo_args = DATA(lo_args) ).

      ro_env = lcl_lisp_environment=>new( io_env ).

      DATA(lo_par) = lo_pars.
      DATA(lo_arg) = lo_args.
      WHILE lo_par IS BOUND AND lo_par NE nil   " Nil means no parameters to map
        AND lo_arg IS BOUND AND lo_arg NE nil.  " Nil means no parameters to map

        ro_env->set_once( symbol = lo_par->car->value
                          element = lo_arg->car ).
        lo_par = lo_par->cdr.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

      evaluate_in_sequence( io_args = lo_args      " Pointer to arguments e.g. (4, (+ x 4)
                            io_pars = lo_pars      " Pointer to formal parameters (x y)
                            io_env = ro_env ).
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
        WHEN lcl_lisp=>type_symbol.
*named let:  (let <variable> (bindings) <body>)
          DATA(lo_var) = co_head->car.
          co_head = co_head->cdr.

        WHEN OTHERS. " lcl_lisp=>type_pair.
*(let ((x 10) (y 5)) (+ x y)) is syntactic sugar for  ( (lambda (x y) (+ x y)) 10 5)
          lo_var = nil.
      ENDCASE.

      extract_arguments( EXPORTING io_head = co_head->car
                         IMPORTING eo_pars = DATA(lo_pars)
                                   eo_args = DATA(lo_args) ).
      ro_env = lcl_lisp_environment=>new( io_env ).

      DATA(lo_new_args) = evaluate_parameters( io_list = lo_args       " Pointer to arguments
                                               environment = io_env ).
      ro_env->parameters_to_symbols( io_args = lo_new_args
                                     io_pars = lo_pars ).              " Pointer to formal parameters

      CHECK lo_var IS BOUND AND lo_var NE nil.
*     named let
      ro_env->set( symbol = lo_var->value
                   element = lcl_lisp_new=>lambda( io_car = lo_pars                " List of parameters
                                                   io_cdr = co_head->cdr           " Body
                                                   io_env = ro_env ) ).
    ENDMETHOD.

    METHOD environment_let_star.
      extract_arguments( EXPORTING io_head = io_head
                         IMPORTING eo_pars = DATA(lo_pars)
                                   eo_args = DATA(lo_args) ).
      ro_env = lcl_lisp_environment=>new( io_env ).

      evaluate_in_sequence( io_args = lo_args      " Pointer to arguments e.g. (4, (+ x 4)
                            io_pars = lo_pars      " Pointer to formal parameters (x y)
                            io_env = ro_env ).
    ENDMETHOD.

    METHOD eval_ast.
*     Evaluate element, Element is not a list
      CASE element->type.
        WHEN lcl_lisp=>type_symbol. "Symbol
*         lookup the symbol in the environment and return the value or raise an error if no value is found
          result = environment->get( element->value ).

        WHEN lcl_lisp=>type_pair. " List
          result = eval( element = element
                         environment = environment ).

        WHEN lcl_lisp=>type_hash. " TEST
          result = CAST lcl_lisp_hash( element )->eval( environment = environment
                                                        interpreter = me ).

        WHEN lcl_lisp=>type_vector. " TEST
          result = CAST lcl_lisp_vector( element )->eval( environment = environment
                                                          interpreter = me ).

        WHEN OTHERS.
*         otherwise just return the original AST value
          result = element.  "Number or string evaluates to itself (also: vector constant)

      ENDCASE.
      assert_is_bound result c_error_eval.

    ENDMETHOD.

    DEFINE tail_expression.
      IF &1 NE nil.
        lo_elem = &1->car.    " Tail context
        CONTINUE.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE tail_sequence.
      IF lo_elem NE nil.
*       result = eval_list( io_head = lo_elem
*                           io_environment = lo_env ).
        result = eval_list_tco( EXPORTING io_head = lo_elem
                                          io_environment = lo_env
                                IMPORTING eo_elem = lo_elem ).
        tail_expression lo_elem.
      ELSE.
        throw( `no expression in body` ).
      ENDIF.
    END-OF-DEFINITION.

    DEFINE validate_quote.
      IF NOT ( &1->cdr->type = lcl_lisp=>type_pair AND &1->cdr->cdr = nil ).
        throw( |invalid form { &1->car->to_string( ) } in { &2 }| ).
      ENDIF.
    END-OF-DEFINITION.

    METHOD is_constant.
*    (define (constant? exp)
*      (if (pair? exp) (eq? (car exp) 'quote) (not (symbol? exp))))
      IF exp->type EQ lcl_lisp=>type_pair.
        rv_flag = xsdbool( exp->car = lcl_lisp=>quote ).
      ELSE.
        rv_flag = xsdbool( exp->type NE lcl_lisp=>type_symbol ).
      ENDIF.
    ENDMETHOD.

    METHOD combine.
      IF is_constant( left ) AND is_constant( right ).
*       (eqv? (eval right) (cdr exp)))
        DATA(eval_left) = eval( element = left
                                environment = environment ).
        DATA(eval_right) = eval( element = right
                                environment = environment ).
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
      ELSEIF right->type = lcl_lisp=>type_pair AND right->car = lcl_lisp=>list.
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
        WHEN lcl_lisp=>type_pair. "non empty list
*         ((and (eq? (car exp) 'unquote) (= (length exp) 2))
          DATA(lo_first) = lo_ptr->car.
          DATA(lo_next) = lo_ptr->cdr.
          validate lo_next.

          IF ( lo_first = lcl_lisp=>unquote
              OR ( lo_first->type EQ lcl_lisp=>type_symbol AND lo_first->value EQ c_eval_unquote ) )
              AND list_length( exp ) EQ 2.
*          ((and (eq? (first = 'unquote) (= (length exp) 2))
            validate_quote lo_ptr c_eval_unquote.

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
              OR ( lo_first->type EQ lcl_lisp=>type_symbol AND lo_first->value EQ c_eval_quasiquote ) )
              AND list_length( exp ) EQ 2.
            validate_quote lo_ptr c_eval_quasiquote.
*           (and (eq? (car exp) 'quasiquote) (= (length exp) 2))

            result = combine( left = lcl_lisp=>quasiquote
                              right = quasiquote( exp = lo_next
                                                  nesting = nesting + 1
                                                  environment = environment )
                              exp = exp
                              environment = environment ).

          ELSEIF lo_first->type EQ lcl_lisp=>type_pair AND
            ( lo_first->car = lcl_lisp=>unquote_splicing OR lo_first->car->value EQ c_eval_unquote_splicing )
            AND list_length( lo_first ) EQ 2.
*           ((and (pair? (car exp))
*          	     (eq? (caar exp) 'unquote-splicing)
*          	     (= (length (car exp)) 2))
            validate_quote lo_first c_eval_unquote_splicing.

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

        WHEN lcl_lisp=>type_vector.
*        	(list 'apply 'vector (expand-quasiquote (vector->list exp) nesting)))
          DATA(lo_vec) = CAST lcl_lisp_vector( exp ).
          result = lcl_lisp_new=>box( io_proc = lcl_lisp_new=>symbol( 'list->vector' )
                                      io_elem = quasiquote( exp = lo_vec->to_list( )
                                                            nesting = nesting
                                                            environment = environment ) ).
        WHEN OTHERS.
*	        (if (constant? exp) exp (list 'quote exp)))
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
      DATA(lo_elem) = element.
      DATA(lo_env) = environment.

      DO.
        validate lo_elem.

        CASE lo_elem.
          WHEN nil OR true OR false.
*           Return predefined symbols as themselves to save having to look them up in the environment
            result = lo_elem.

          WHEN OTHERS.
            IF lo_elem->type EQ lcl_lisp=>type_pair.
              lo_elem = syntax_expand( element = lo_elem
                                       environment = lo_env ).
            ENDIF.

*           Evaluate element
            CASE lo_elem->type.
              WHEN lcl_lisp=>type_pair. " List
*               return a new list that is the result of calling EVAL on each of the members of the list

*               To evaluate list, we must first evaluate head value
*               Evaluate first element of list to determine if it is a native procedure or lambda
                DATA(lr_head) = lo_elem->car.
                DATA(lr_tail) = lo_elem->cdr.

                CASE lr_head->value.

                  WHEN c_eval_quote. " Return the argument to quote unevaluated
                    IF lr_tail->cdr NE nil.
                      throw( |quote can only take a single argument| ).
                    ENDIF.
                    result = lr_tail->car.

                  WHEN c_eval_quasiquote.
                    IF lr_tail->cdr NE nil.
                      throw( |quasiquote can only take a single argument| ).
                    ENDIF.
                    lo_elem = quasiquote( exp = lr_tail->car
                                          nesting = 0
                                          environment = lo_env ).

                    CONTINUE.  "tail_expression lo_elem.

                  WHEN 'and'.
*                   (and <expression>* >tail expression>)
                    result = true.
                    DATA(lo_ptr) = lr_tail.
                    WHILE result NE false AND lo_ptr IS BOUND AND lo_ptr NE nil AND lo_ptr->cdr NE nil.
                      result = eval_ast( element = lo_ptr->car
                                         environment = lo_env ).
                      lo_ptr = lo_ptr->cdr.
                    ENDWHILE.
                    IF result NE false.
                      tail_expression lo_ptr.
                    ENDIF.

                  WHEN 'or'.
*                  (or <expression>* <tail expression>)
                    result = false.
                    lo_ptr = lr_tail.
                    WHILE result EQ false AND lo_ptr IS BOUND AND lo_ptr NE nil AND lo_ptr->cdr NE nil.
                      result = eval_ast( element = lo_ptr->car
                                         environment = lo_env ).
                      lo_ptr = lo_ptr->cdr.
                    ENDWHILE.
                    IF result EQ false.
                      tail_expression lo_ptr.
                    ENDIF.

                  WHEN 'cond'.
                    lo_ptr = lr_tail.
                    lo_elem = nil.
                    WHILE lo_ptr->type EQ lcl_lisp=>type_pair.
                      DATA(lo_clause) = lo_ptr->car.
                      IF lo_clause->car->value EQ c_lisp_else.
                        lo_elem = lo_clause->cdr.
                        EXIT.
                      ENDIF.
                      DATA(lo_test) = eval_ast( element = lo_clause->car
                                                environment = lo_env ).
                      IF lo_test NE false.
                        lo_elem = lo_clause->cdr.
                        EXIT.
                      ENDIF.
                      lo_ptr = lo_ptr->cdr.
                    ENDWHILE.
                    IF lo_elem EQ nil.
                      result = lo_test.
                    ELSEIF lo_elem->car->value = c_lisp_then.
                      lo_elem = lcl_lisp_new=>cons( io_car = lo_elem->cdr->car
                                                    io_cdr = lcl_lisp_new=>box_quote( lo_test ) ).
                      CONTINUE.
                      "tail_expression lo_elem.
                    ELSE.
                      tail_sequence.
                    ENDIF.

                  WHEN 'define'.
*           call the set method of the current environment using the unevaluated first parameter
*           (second list element) as the symbol key and the evaluated second parameter as the value.
                    result = assign_symbol( element = lr_tail
                                            environment = lo_env ).

                  WHEN 'define-macro'.
                    result = assign_symbol( element = lr_tail
                                            environment = lo_env
                                            iv_macro = abap_true ).

                  WHEN 'define-syntax'.
                    result = define_syntax( element = lr_tail
                                            environment = lo_env ).

                  WHEN 'set!'.                        " Re-Assign symbol
                    result = re_assign_symbol( element     = lr_tail
                                               environment = lo_env ).

                  WHEN 'if'.
                    " validate lr_tail->cdr. "I do not have a test case yet where it fails here
                    IF eval( element = lr_tail->car
                             environment = lo_env ) NE false.

                      lo_elem = lr_tail->cdr->car. " Tail context
                      CONTINUE.

                    ELSEIF lr_tail->cdr->cdr = nil.
                      result = false.
                    ELSE.
                      " validate lr_tail->cdr->cdr. " missing test case, comment out
                      lo_elem = lr_tail->cdr->cdr->car. " Tail context
                      CONTINUE.

                    ENDIF.

                  WHEN 'begin'.
                    lo_elem = lr_tail.
                    tail_sequence.

                  WHEN 'let'.
                    lo_env = environment_named_let( EXPORTING io_env = lo_env
                                                    CHANGING co_head = lr_tail ).
                    lo_elem = lr_tail->cdr.
                    tail_sequence.

                  WHEN 'let*'.
                    lo_env = environment_let_star( io_head = lr_tail->car
                                                   io_env = lo_env ).
                    lo_elem = lr_tail->cdr.
                    tail_sequence.

                  WHEN 'letrec'.
*                   (letrec ((a 5) (b (+ a 3)) b)
                    lo_env = environment_letrec( io_head = lr_tail->car
                                                 io_env = lo_env ).
                    lo_elem = lr_tail->cdr.
                    tail_sequence.

                  WHEN 'letrec*'.
                    lo_env = environment_letrec_star( io_head = lr_tail->car
                                                      io_env = lo_env ).
                    lo_elem = lr_tail->cdr.
                    tail_sequence.

                  WHEN 'unless'.
                    result = nil.
                    IF eval( element = lr_tail->car
                             environment = lo_env ) EQ false.
                      "  validate lr_tail->cdr. "I do not have a test case yet where it fails here
                      lo_elem = lr_tail->cdr.
                      tail_sequence.
                    ENDIF.

                  WHEN 'when'.
                    result = nil.
                    IF eval( element = lr_tail->car
                             environment = lo_env  ) NE false.
                      "  validate lr_tail->cdr. "I do not have a test case yet where it fails here
                      lo_elem = lr_tail->cdr.
                      tail_sequence.
                    ENDIF.

                  WHEN 'lambda'.
                    result = lcl_lisp_new=>lambda( io_car = lr_tail->car         " List of parameters
                                                   io_cdr = lr_tail->cdr         " Body
                                                   io_env = lo_env ).

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
                    validate: lo_head, lo_head->cdr, lo_head->cdr->cdr.

*                   Initialization
                    eval_do_init( EXPORTING io_head = lo_head->car
                                            io_env = lo_env
                                  IMPORTING eo_step = DATA(lo_steps)
                                            eo_env = lo_env ).
*                   Iteration
                    lo_test = lo_head->cdr->car.
                    DATA(lo_command) = lo_head->cdr->cdr.

                    DO.
*                     evaluate <test>;
                      CASE eval_ast( element = lo_test->car
                                     environment = lo_env ).
                        WHEN false.
                          eval_do_step( io_command = lo_command
                                        io_steps = lo_steps
                                        io_env = lo_env ).
*                         and the next iteration begins.

                        WHEN OTHERS. " <test> evaluates to a true value
* <expression>s are evaluated from left to right and the values of the last <expression> are returned.
* If no <expression>s are present, then the value of the do expression is unspecified.

                          lo_elem = lo_test->cdr.
                          result = nil.
                          EXIT.
                      ENDCASE.

                    ENDDO.

                    IF lo_elem EQ nil.
                      result = nil.
                    ELSE.
                      tail_sequence.
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
                    validate lr_tail.
                    result = nil.

                    DATA(lo_key) = eval( element = lr_tail->car
                                         environment = lo_env ).

                    lr_tail = lr_tail->cdr.
                    validate: lr_tail, lr_tail->car, lo_key.

                    IF lr_tail EQ nil.
                      throw( `case: no clause` ).
                    ENDIF.

                    lo_elem = nil.
                    DATA(lv_match) = abap_false.
                    WHILE lr_tail->type EQ lcl_lisp=>type_pair AND lv_match EQ abap_false.
                      lo_clause = lr_tail->car.

                      DATA(lo_datum) = lo_clause->car.
                      validate lo_datum.

                      WHILE lo_datum NE nil.

                        IF lo_datum->value EQ c_lisp_else.
                          IF lr_tail->cdr NE nil.
                            throw( `case: else must be the last clause` ).
                          ENDIF.
                          lo_elem = lo_clause->cdr.
                          lv_match = abap_true.
                          EXIT.
                        ENDIF.

                        " eqv? match
                        IF lo_key->is_equivalent( lo_datum->car ) NE false.
                          lo_elem = lo_clause->cdr.
                          lv_match = abap_true.
                          EXIT.
                        ENDIF.

                        lo_datum = lo_datum->cdr.
                      ENDWHILE.

                      lr_tail = lr_tail->cdr.
                    ENDWHILE.

                    IF lo_elem EQ nil.
                      result = nil.

                    ELSEIF lo_elem->car->value = c_lisp_then.

                      lo_elem = lcl_lisp_new=>cons(
                        io_car = lo_elem->cdr->car
                        io_cdr = lcl_lisp_new=>box_quote( lo_key ) ).
                      CONTINUE.
                      "tail_expression lo_elem.

                    ELSE.

                      tail_sequence.

                    ENDIF.

                  WHEN 'for-each'.
                    result = expand_for_each( io_list = lr_tail
                                              environment = lo_env ).

                  WHEN 'map'.
                    result = expand_map( io_list = lr_tail
                                         environment = lo_env ).

                  WHEN 'apply'.
                    " (apply proc arg1 ... argn rest-args)
                    lo_elem = lcl_lisp_new=>cons( io_car = expand_apply( io_list = lr_tail
                                                                         environment = lo_env ) ).
                    tail_expression lo_elem.

                  WHEN 'macroexpand'.
*                   for debugging
                    validate lr_tail->car.
                    result = syntax_expand( element = lr_tail->car
                                            environment = lo_env ).

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
                    DATA(lo_proc) = eval_ast( element = lr_head             " proc
                                              environment = lo_env ).
                    trace_call lo_proc lr_tail.

                    CASE lo_proc->type.

                      WHEN lcl_lisp=>type_lambda.
                        lo_env = lambda_environment( io_head = lo_proc
                                                     io_args = lr_tail
                                                     environment = lo_env ).
                        lo_elem = lo_proc->cdr.
                        tail_sequence.

                      WHEN lcl_lisp=>type_native.
*                       Evaluate native function:
                        CALL METHOD (lo_proc->value)
                          EXPORTING
                            list   = evaluate_parameters( io_list = lr_tail
                                                          environment = lo_env )
                          RECEIVING
                            result = result.

                      WHEN lcl_lisp=>type_primitive OR lcl_lisp=>type_syntax.
                        lo_elem = lcl_lisp_new=>cons( io_car = lo_proc
                                                      io_cdr = lr_tail ).
                        CONTINUE. "tail_expression lo_elem.

                      WHEN lcl_lisp=>type_abap_function.
*              >> TEST: Support evaluation of ABAP function directly
*                       Recompose as if calling a PROC (which we are). This is part of the test. If we make an ABAP function
*                       call first-class, then we would need to revisit evaluating the whole of ELEMENT in one shot
                        result = proc_abap_function_call( lcl_lisp_new=>cons( io_car = lo_proc
                                                                              io_cdr = lr_tail ) ).
*              << TEST
*                      WHEN lcl_lisp=>type_abap_method.
*              >> TEST: Support evaluation of ABAP methods directly
*              << TEST

                      WHEN OTHERS.
                        throw( |Cannot evaluate { lo_proc->to_string( ) } - not a procedure| ).

                    ENDCASE.

                ENDCASE.

              WHEN OTHERS.
                result = eval_ast( element = lo_elem
                                   environment = lo_env ).

            ENDCASE.

        ENDCASE.
        trace_result result.
        RETURN.

      ENDDO.
    ENDMETHOD.

    METHOD eval_hash.
      result = lcl_lisp_vector=>from_list(  eval( element = element
                                                  environment = environment ) ).
    ENDMETHOD.

    DEFINE optional_port.
      DATA li_port TYPE REF TO lif_&2_port.

      IF &1->type EQ lcl_lisp=>type_pair.
      validate_port &1->car &3.
      li_port ?= &1->car.
      ELSE.
      li_port = go_&2_port.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE optional_port_arg.
      optional_port io_arg &1 &2.
    END-OF-DEFINITION.

    METHOD write.
      optional_port_arg output `write`.
      li_port->write( io_elem ).
      result = io_elem.
    ENDMETHOD.

    METHOD display.
      optional_port_arg output `display`.
      li_port->display( io_elem ).
      result = io_elem.
    ENDMETHOD.

    METHOD read.
      optional_port_arg input `read`.
      result = read_from( li_port ).
    ENDMETHOD.

    METHOD read_char.
      optional_port_arg input `read-char`.

      result = lcl_lisp_new=>char( li_port->read( ) ).
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
        DATA(lo_result) = eval( element = lo_element
                                environment = env ).
        gi_log->put( lo_result ).
      ENDLOOP.
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
      validate list.

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

        IF first->type = lcl_lisp=>type_pair.
          result = lcl_lisp_new=>cons( io_car = first->car ).

*         TO DO: Test for circular list! ------------------------------
          DATA(lo_last) = result.
          DATA(lo_arg) = first->cdr.
          WHILE lo_arg->type = lcl_lisp=>type_pair.
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

        validate_tail lo_arg first `append`.

        first = lo_arg = lo_iter->next( ).
        CHECK first NE nil.

*       TO DO: Check for circular list
*       Append lo_arg to result, from last element on
        WHILE lo_arg->type = lcl_lisp=>type_pair.
          lo_last = lo_last->cdr = lcl_lisp_new=>cons( io_car = lo_arg->car ).
          lo_arg = lo_arg->cdr.
        ENDWHILE.

        CHECK lo_arg NE nil.
        lo_last = lo_last->cdr = lo_arg.

      ENDWHILE.

    ENDMETHOD.

    METHOD list_reverse.
      validate io_list.

      result = nil.
      DATA(lo_ptr) = io_list.

*     TO DO: check if circular lists are allowed
      WHILE lo_ptr->type EQ lcl_lisp=>type_pair.
        result = lcl_lisp_new=>cons( io_car = lo_ptr->car
                                     io_cdr = result ).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
    ENDMETHOD.

    METHOD proc_reverse.
      validate list.

      result = list_reverse( list->car ).
    ENDMETHOD.                    "proc_reverse

    METHOD table_of_lists.
      validate io_head.

      CLEAR rt_table.
      CHECK io_head NE nil.

*     build internal table of list interators
      DATA(iter) = io_head->new_iterator( ).
      WHILE iter->has_next( ).
*       Evaluate next list entry
        DATA(lo_next) = eval( element = iter->next( )
                              environment = environment ).
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
      validate: list, list->car, list->cdr.

      IF list->car EQ nil.
        result = list->cdr->car.
      ELSE.
*       Get to last element in list - this can make APPEND expensive, like LENGTH
        DATA(lo_last) = list->car.
        IF lo_last->type NE lcl_lisp=>type_pair.
          error_no_list list `append!`.
        ENDIF.

        WHILE lo_last->cdr IS BOUND AND lo_last->cdr NE nil.
          lo_last = lo_last->cdr.
        ENDWHILE.

        "TO DO - replace with validate_tail lo_last (?) list->car.
        IF lo_last->type NE lcl_lisp=>type_pair.
*         If the last item is not a cons cell, return an error
          error_no_list list->car  `append!`.
        ENDIF.

*       Last item is a cons cell; tack on the new value
        lo_last->cdr = list->cdr->car.
        result = list->car.
      ENDIF.
    ENDMETHOD.                    "proc_append_unsafe

    METHOD proc_car.
      validate: list, list->car.

      DATA(lo_arg) = list->car.
      IF lo_arg->type NE lcl_lisp=>type_pair.
        lo_arg->error_not_a_pair( `car: ` ).
      ENDIF.
      result = lo_arg->car.
    ENDMETHOD.                    "proc_car

    METHOD proc_set_car.
      validate: list, list->cdr.

      DATA(lo_arg) = list->car.
      validate_mutable: lo_arg  `list`.

      IF lo_arg->type NE lcl_lisp=>type_pair.
        lo_arg->error_not_a_pair( `set-car!: ` ).
      ENDIF.

      lo_arg->car = list->cdr->car.
      result = nil.
    ENDMETHOD.                    "proc_car

    METHOD proc_set_cdr.
      validate: list, list->cdr.

      DATA(lo_arg) = list->car.
      validate_mutable: lo_arg `list`.

      IF lo_arg->type NE lcl_lisp=>type_pair.
        lo_arg->error_not_a_pair( `set-cdr!: ` ).
      ENDIF.

      lo_arg->cdr = list->cdr->car.
      result = nil.
    ENDMETHOD.

    METHOD proc_cdr.
      validate list.

      DATA(lo_arg) = list->car.
      IF lo_arg->type NE lcl_lisp=>type_pair.
        lo_arg->error_not_a_pair( `cdr: ` ).
      ENDIF.
      result = lo_arg->cdr.
    ENDMETHOD.                    "proc_cdr

    METHOD proc_cons.
*     Create new cell and prepend it to second parameter
      validate: list, list->car, list->cdr.

      IF list->cdr->cdr NE nil.
        throw( `cons: only 2 arguments allowed` ).
      ENDIF.

      result = lcl_lisp_new=>cons( io_car = list->car
                                   io_cdr = list->cdr->car ).
    ENDMETHOD.                    "proc_cons

    METHOD proc_not.
*     Create new cell and prepend it to second parameter
      validate list.

      IF list->car EQ false.
        result = true.
      ELSE.
        result = false.
      ENDIF.
    ENDMETHOD.                    "proc_cons

    METHOD proc_caar.
      validate list.
      IF list EQ nil.
        list->error_not_a_pair( `caar: ` ).
      ENDIF.

      DATA(lo_arg) = list->car.
      IF lo_arg->type NE lcl_lisp=>type_pair.
        lo_arg->error_not_a_pair( `caar: ` ).
      ENDIF.

      lo_arg = lo_arg->car.
      IF lo_arg->type NE lcl_lisp=>type_pair.
        lo_arg->error_not_a_pair( `caar: ` ).
      ENDIF.

      result = lo_arg->car.
    ENDMETHOD.                    "proc_car

    METHOD proc_cadr.
      validate list.
      IF list EQ nil.
        list->error_not_a_pair( `cadr: ` ).
      ENDIF.

      DATA(lo_arg) = list->car.
      IF lo_arg->type NE lcl_lisp=>type_pair.
        lo_arg->error_not_a_pair( `cadr: ` ).
      ENDIF.

      lo_arg = lo_arg->cdr.
      IF lo_arg->type NE lcl_lisp=>type_pair.
        lo_arg->error_not_a_pair( `cadr: ` ).
      ENDIF.

      result = lo_arg->car.
    ENDMETHOD.                    "proc_cdr

    METHOD proc_cdar.
      validate list.
      IF list EQ nil.
        list->error_not_a_pair( `cdar: ` ).
      ENDIF.

      DATA(lo_arg) = list->car.
      IF lo_arg->type NE lcl_lisp=>type_pair.
        lo_arg->error_not_a_pair( `cdar: ` ).
      ENDIF.

      lo_arg = lo_arg->car.
      IF lo_arg->type NE lcl_lisp=>type_pair.
        lo_arg->error_not_a_pair( `cdar: ` ).
      ENDIF.

      result = lo_arg->cdr.
    ENDMETHOD.                    "proc_car

    METHOD proc_cddr.
      validate list.
      IF list EQ nil.
        list->error_not_a_pair( `cddr: ` ).
      ENDIF.

      DATA(lo_arg) = list->car.
      IF lo_arg->type NE lcl_lisp=>type_pair.
        lo_arg->error_not_a_pair( `cddr: ` ).
      ENDIF.

      lo_arg = lo_arg->cdr.
      IF lo_arg->type NE lcl_lisp=>type_pair.
        lo_arg->error_not_a_pair( `cddr: ` ).
      ENDIF.

      result = lo_arg->cdr.
    ENDMETHOD.                    "proc_cdr


* (defun list-length (x)
*   (do ((n 0 (+ n 2))           ;Counter.
*        (fast x (cddr fast))    ;Fast pointer: leaps by 2.
*        (slow x (cdr slow)))    ;Slow pointer: leaps by 1.
*       (nil)
*     ;; If fast pointer hits the end, return the count.
*     (when (endp fast) (return n))
*     (when (endp (cdr fast)) (return (+ n 1)))
*     ;; If fast pointer eventually equals slow pointer, then we must be stuck in a circular list.
*     ;; (A deeper property is the converse: if we are stuck in a circular list, then eventually
*     ;; the fast pointer will equal the slow pointer. That fact justifies this implementation.
*     (when (and (eq fast slow) (> n 0)) (return nil))))
    METHOD list_length.
      DATA lo_elem TYPE REF TO lcl_lisp.
      DATA lo_slow TYPE REF TO lcl_lisp.
      validate list.

      result = 0.
      lo_slow = lo_elem = list.
*     Iterate over list to count the number of items
      WHILE lo_elem->type EQ lcl_lisp=>type_pair.
        ADD 1 TO result.
        lo_elem = lo_elem->cdr.
        lo_slow = lo_slow->cdr.
        CHECK lo_elem->type EQ lcl_lisp=>type_pair.
        ADD 1 TO result.
        lo_elem = lo_elem->cdr.
        CHECK lo_elem = lo_slow.
*       Circular list
        RETURN.
      ENDWHILE.
      CHECK lo_elem NE nil.
*     If the last item is not a cons cell, return an error
      error_no_list list `list-length`.
    ENDMETHOD.

    METHOD proc_length.
      validate: list, list->car, list->cdr.
      IF list->cdr NE nil.
        throw( |length takes only one argument| ).
      ENDIF.

      result = lcl_lisp_new=>integer( list_length( list->car ) ).
    ENDMETHOD.                    "proc_length

    METHOD proc_list_copy.
      DATA lo_slow TYPE REF TO lcl_lisp.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      DATA lo_new TYPE REF TO lcl_lisp.

      validate: list, list->cdr.
      IF list->cdr NE nil.
        throw( |list-copy takes only one argument| ).
      ENDIF.

      validate list->car.
      result = list->car.
      CHECK result->type EQ lcl_lisp=>type_pair.

      lo_slow = lo_ptr = result->cdr.
      result = lo_new = lcl_lisp_new=>cons( io_car = result->car ).

*     Iterate over list to count the number of items
      WHILE lo_ptr->type EQ lcl_lisp=>type_pair.
        lo_new = lo_new->cdr = lcl_lisp_new=>cons( io_car = lo_ptr->car ).
        lo_ptr = lo_ptr->cdr.
        lo_slow = lo_slow->cdr.
        CHECK lo_ptr->type EQ lcl_lisp=>type_pair.
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
      validate: list, list->car.
      result = COND #( WHEN list->car = nil THEN true ELSE false ).
    ENDMETHOD.                    "proc_nilp

    METHOD proc_make_list.
      DATA k TYPE tv_int.
*     returns a list of length k and every atom is the default fill value supplied or the empty list
      validate list.
      validate_index list->car 'make-list'.

      result = lcl_lisp=>nil.

      k = CAST lcl_lisp_integer( list->car )->integer.
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
      validate: list, list->cdr.
      validate_index list->cdr->car 'list-tail'.

      result = list_tail( list = list->car
                          k    = CAST lcl_lisp_integer( list->cdr->car )->integer
                          area = 'list-tail' ).
    ENDMETHOD.

    METHOD list_tail.
*     (list-tail list k) procedure
*     List should be a list of size at least k.  The list-tail procedure returns the subchain
*     of list obtained by omitting the first k elements:  (list-tail '(a b c d) 2)  => (c d)
*
*     we must check that list is a chain of pairs whose length is at least k.
*     we should not check that it is a list of pairs beyond this length.

      validate: list, list->cdr.

      result = list.
      DO k TIMES.
        IF result->cdr EQ nil.
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

      validate list.

      DATA(lo_count) = list->car.
      validate_index lo_count 'iota count'.
      lv_count = CAST lcl_lisp_integer( lo_count )->integer.

      result = nil.
      CHECK lv_count GT 0.

      validate list->cdr.
      IF list->cdr NE nil.
        DATA(lo_start) = list->cdr->car.
        validate_integer lo_start 'iota start'.
        lv_start = CAST lcl_lisp_integer( lo_start )->integer.

        validate list->cdr->cdr.
        IF list->cdr->cdr NE nil.
          DATA(lo_step) = list->cdr->cdr->car.
          validate_integer lo_step 'iota step'.
          lv_step = CAST lcl_lisp_integer( lo_step )->integer.
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

      validate: list, list->cdr.
      validate_index list->cdr->car 'list-ref'.

      result = list_tail( list = list->car
                          k    = CAST lcl_lisp_integer( list->cdr->car )->integer
                          area = 'list-ref' ).
      result = result->car.
    ENDMETHOD.

    METHOD proc_make_vector.
      validate: list, list->cdr.

      DATA(lo_size) = list->car.

      validate_index lo_size `make-vector`.

      IF list->cdr NE lcl_lisp=>nil.
        DATA(lo_fill) = list->cdr->car.
      ELSE.
        lo_fill = lcl_lisp=>nil.
      ENDIF.

      result = lcl_lisp_vector=>init( size = CAST lcl_lisp_integer( lo_size )->integer
                                      io_fill = lo_fill ).
    ENDMETHOD.

    METHOD proc_vector.
*     The items given to us are already in a list and evaluated; we just need to return the head
      result = lcl_lisp_vector=>from_list( list ).
    ENDMETHOD.

    METHOD proc_vector_length.
      validate list.
      validate_vector list->car 'vector-length'.

      result = CAST lcl_lisp_vector( list->car )->length( ).
    ENDMETHOD.

    METHOD proc_vector_ref.
*    (vector-ref vector k) procedure
      validate list.
      validate_vector list->car 'vector-ref'.
      DATA(lo_vec) = CAST lcl_lisp_vector( list->car ).

      validate list->cdr.
      DATA(lo_idx) = list->cdr->car.
      validate_index lo_idx 'vector-ref'.

      result = lo_vec->get( CAST lcl_lisp_integer( lo_idx )->integer ).

    ENDMETHOD.

    METHOD proc_vector_set.
*    (vector-set! vector k obj) procedure
      validate list.
      validate_vector list->car 'vector-set!'.
      DATA(lo_vec) = CAST lcl_lisp_vector( list->car ).

      validate: list->cdr.
      DATA(lo_idx) = list->cdr->car.
      validate_index lo_idx 'vector-set!'.

      validate: list->cdr->cdr.

      DATA(lo_obj) = list->cdr->cdr.
      IF lo_obj NE nil.
        lo_obj = lo_obj->car.
      ENDIF.

      lo_vec->set( index = CAST lcl_lisp_integer( lo_idx )->integer
                   io_elem = lo_obj ).
*     Result is undefined, but must be valid
      result = lo_obj.
    ENDMETHOD.

    METHOD proc_vector_to_list.
*   (vector->list vector)
*   (vector->list vector start) procedure
*   (vector->list vector start end) procedure
* The vector->list procedure returns a newly allocated list of the objects contained
* in the elements of vector between start and end. Order is preserved.
      validate list.
      validate_vector list->car 'vector->list'.
      DATA(lo_vec) = CAST lcl_lisp_vector( list->car ).

      validate list->cdr.
      IF list->cdr NE nil.
        DATA(lo_start) = list->cdr->car.
        validate_index lo_start 'vector->list start'.

        validate list->cdr->cdr.
        IF list->cdr->cdr NE nil.
          DATA(lo_end) = list->cdr->cdr->car.
          validate_index lo_end 'vector->list end'.

          result = lo_vec->get_list( from = CAST lcl_lisp_integer( lo_start )->integer
                                     to = CAST lcl_lisp_integer( lo_end )->integer ).
        ELSE.
          result = lo_vec->get_list( from = CAST lcl_lisp_integer( lo_start )->integer ).
        ENDIF.

      ELSE.
        result = lo_vec->to_list( ).
      ENDIF.
    ENDMETHOD.

    METHOD proc_list_to_vector.
*   (list->vector list)
* The list->vector procedure returns a newly created vector initialized
* to the elements of the list list. Order is preserved.
      validate: list.

      result = lcl_lisp_vector=>from_list( list->car ).
    ENDMETHOD.

* (memq obj list)  return the first sublist of
* list whose car is obj,  where  the  sublists  of list are the non-empty lists
* returned by (list-tail list  k) for k less than the length of list.
* If obj does not occur in list, then #f (not the empty list) is returned.
* Memq uses eq? to compare obj with the elements  of list
    METHOD proc_memq.
      validate: list, list->car, list->cdr.

      result = false.

      CHECK list->cdr NE nil.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_item) = list->car.
      WHILE lo_sublist NE nil AND lo_sublist->car->type EQ lo_item->type.

        CASE lo_item->type.
          WHEN lcl_lisp=>type_integer.
            IF CAST lcl_lisp_integer( lo_item )->integer = CAST lcl_lisp_integer( lo_sublist->car )->integer.
              result = lo_sublist.
              RETURN.
            ENDIF.

          WHEN lcl_lisp=>type_real.
            IF CAST lcl_lisp_real( lo_item )->real = CAST lcl_lisp_real( lo_sublist->car )->real.
              result = lo_sublist.
              RETURN.
            ENDIF.

          WHEN lcl_lisp=>type_symbol OR lcl_lisp=>type_string.
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
      validate: list, list->car, list->cdr.

      result = false.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_item) = list->car.
      WHILE lo_sublist->type EQ lcl_lisp=>type_pair.
        IF lo_sublist->car->is_equivalent( lo_item ) NE false.
          result = lo_sublist.
          RETURN.
        ENDIF.
        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
*      CHECK lo_sublist NE nil.
*      list->error_not_a_list( ).
    ENDMETHOD.

    METHOD proc_member.
      validate: list, list->car, list->cdr.

      result = false.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_compare) = list->cdr->cdr.
      DATA(lo_item) = list->car.

      WHILE lo_sublist->type EQ lcl_lisp=>type_pair.
        IF lo_item->is_equal( io_elem = lo_sublist->car
                              comp = lo_compare
                              interpreter = me ) NE false.
          result = lo_sublist.
          RETURN.
        ENDIF.
        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
*      CHECK lo_sublist NE nil.
*      list->error_not_a_list( ).
    ENDMETHOD.

    METHOD proc_assoc.
      validate: list, list->car, list->cdr.

      result = false.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_compare) = list->cdr->cdr.
      DATA(lo_key) = list->car.

      WHILE lo_sublist->type EQ lcl_lisp=>type_pair.
        DATA(lo_pair) = lo_sublist->car.
        IF lo_key->is_equal( io_elem = lo_pair->car
                             comp = lo_compare
                             interpreter = me ) NE false.
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
      validate: list, list->car, list->cdr.

      result = false.

      CHECK list->cdr NE nil.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_key) = list->car.
      WHILE lo_sublist->type EQ lcl_lisp=>type_pair.
        DATA(lo_pair) = lo_sublist->car.
        IF lo_pair->car->type EQ lo_key->type.

          CASE lo_key->type.
            WHEN lcl_lisp=>type_integer.
              IF CAST lcl_lisp_integer( lo_key )->integer = CAST lcl_lisp_integer( lo_pair->car )->integer.
                result = lo_pair.
                RETURN.
              ENDIF.

            WHEN lcl_lisp=>type_real.
              IF CAST lcl_lisp_real( lo_key )->real = CAST lcl_lisp_real( lo_pair->car )->real.
                result = lo_pair.
                RETURN.
              ENDIF.

            WHEN lcl_lisp=>type_symbol OR lcl_lisp=>type_string.
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
      validate: list, list->car, list->cdr.

      result = false.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_key) = list->car.

      WHILE lo_sublist->type EQ lcl_lisp=>type_pair.
        DATA(lo_pair) = lo_sublist->car.
        validate lo_pair->car.
        IF lo_pair->car->is_equivalent( lo_key ) NE false.
          result = lo_pair.
          RETURN.
        ENDIF.
        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
    ENDMETHOD.

**********************************************************************
    DEFINE cell_arith_definition.
      DATA lv_type TYPE lcl_sexps=>tv_type.
      DATA res_int TYPE tv_int.
      DATA res_real TYPE tv_real.
      DATA res_nummer TYPE tv_int.
      DATA res_denom TYPE tv_int VALUE 1.
      DATA lv_gcd TYPE tv_int.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_real TYPE REF TO lcl_lisp_real.
    END-OF-DEFINITION.

    DEFINE cell_arith.
      CASE cell->type.
        WHEN lcl_lisp=>type_integer.
          lo_int ?= cell.

          CASE lv_type.
            WHEN lcl_lisp=>type_real.
              res_real = res_real &1 lo_int->integer.

            WHEN lcl_lisp=>type_rational.
              res_nummer = res_nummer &1 ( lo_int->integer * res_denom ).
              lv_gcd = lcl_lisp_rational=>gcd(  n = res_nummer
                                                d = res_denom ).
              res_nummer = res_nummer DIV lv_gcd.
              res_denom = res_denom DIV lv_gcd.

            WHEN lcl_lisp=>type_integer.
              res_int = res_int &1 lo_int->integer.

            WHEN OTHERS.
              lv_type = lcl_lisp=>type_integer.
              res_int = lo_int->integer.
          ENDCASE.

        WHEN lcl_lisp=>type_real.
          lo_real ?= cell.

          CASE lv_type.
            WHEN lcl_lisp=>type_real.
              res_real = res_real &1 lo_real->real.

            WHEN lcl_lisp=>type_rational.
              res_real = res_nummer / res_denom &1 lo_real->real.
              lv_type = lcl_lisp=>type_real.

            WHEN lcl_lisp=>type_integer.
              res_real = res_int &1 lo_real->real.
              lv_type = lcl_lisp=>type_real.

            WHEN OTHERS.
              lv_type = lcl_lisp=>type_real.
              res_real = lo_real->real.
          ENDCASE.

        WHEN lcl_lisp=>type_rational.
          lo_rat ?= cell.

          CASE lv_type.
            WHEN lcl_lisp=>type_real.
              res_real = res_real &1 lo_rat->integer / lo_rat->denominator.
              lv_type = lcl_lisp=>type_real.

            WHEN lcl_lisp=>type_rational.
              res_nummer = res_nummer * lo_rat->denominator &1 ( lo_rat->integer * res_denom ).
              res_denom = res_denom * lo_rat->denominator.
              lv_gcd = lcl_lisp_rational=>gcd(  n = res_nummer
                                                d = res_denom ).
              res_nummer = res_nummer DIV lv_gcd.
              res_denom = res_denom DIV lv_gcd.


            WHEN lcl_lisp=>type_integer.
              res_nummer = ( res_int * lo_rat->denominator ) &1 lo_rat->integer.
              res_denom = lo_rat->denominator.

              lv_gcd = lcl_lisp_rational=>gcd(  n = res_nummer
                                                d = res_denom ).
              res_nummer = res_nummer DIV lv_gcd.
              res_denom = res_denom DIV lv_gcd.
              lv_type = lcl_lisp=>type_rational.

            WHEN OTHERS.
              lv_type = lcl_lisp=>type_rational.
              res_nummer = lo_rat->integer.
              res_denom = lo_rat->denominator.
          ENDCASE.

*        WHEN lcl_lisp=>type_complex.
        WHEN OTHERS.
          throw( |{ cell->to_string( ) } is not a number in { &2 }| ).
      ENDCASE.
    END-OF-DEFINITION.

    DEFINE result_arith.
      CASE lv_type.
        WHEN lcl_lisp=>type_integer.
          result = lcl_lisp_new=>integer( res_int ).

        WHEN lcl_lisp=>type_rational.
          result = lcl_lisp_new=>rational( nummer = res_nummer
                                           denom = res_denom ).
        WHEN lcl_lisp=>type_real.
          result = lcl_lisp_new=>real( res_real ).

        WHEN OTHERS.
          throw( |Error in result of { &1 }| ).
      ENDCASE.
    END-OF-DEFINITION.

    METHOD proc_add.
      cell_arith_definition.

      validate list.
      DATA(iter) = list->new_iterator( ).

      lv_type = lcl_lisp=>type_integer.
      WHILE iter->has_next( ).
        DATA(cell) = iter->next( ).
        cell_arith + `[+]`.
      ENDWHILE.

      result_arith `[+]`.
    ENDMETHOD.                    "proc_add

    METHOD proc_multiply.
      cell_arith_definition.

      validate list.
      DATA(iter) = list->new_iterator( ).

      res_int = 1.
      lv_type = lcl_lisp=>type_integer.
      WHILE iter->has_next( ).
        DATA(cell) = iter->next( ).

        CASE cell->type.
          WHEN lcl_lisp=>type_integer.
            lo_int ?= cell.

            CASE lv_type.
              WHEN lcl_lisp=>type_real.
                res_real = res_real * lo_int->integer.

              WHEN lcl_lisp=>type_rational.
                res_nummer = res_nummer * lo_int->integer.
                lv_gcd = lcl_lisp_rational=>gcd(  n = res_nummer
                                                  d = res_denom ).
                res_nummer = res_nummer DIV lv_gcd.
                res_denom = res_denom DIV lv_gcd.

              WHEN lcl_lisp=>type_integer.
                res_int = res_int * lo_int->integer.

              WHEN OTHERS.
                lv_type = lcl_lisp=>type_integer.
                res_int = lo_int->integer.
            ENDCASE.

          WHEN lcl_lisp=>type_real.
            lo_real ?= cell.

            CASE lv_type.
              WHEN lcl_lisp=>type_real.
                res_real = res_real * lo_real->real.

              WHEN lcl_lisp=>type_rational.
                res_real = res_nummer / res_denom * lo_real->real.
                lv_type = lcl_lisp=>type_real.

              WHEN lcl_lisp=>type_integer.
                res_real = res_int * lo_real->real.
                lv_type = lcl_lisp=>type_real.

              WHEN OTHERS.
                lv_type = lcl_lisp=>type_real.
                res_real = lo_real->real.
            ENDCASE.

          WHEN lcl_lisp=>type_rational.
            lo_rat ?= cell.

            CASE lv_type.
              WHEN lcl_lisp=>type_real.
                res_real = res_real * lo_rat->integer / lo_rat->denominator.
                lv_type = lcl_lisp=>type_real.

              WHEN lcl_lisp=>type_rational.
                res_nummer = res_nummer * lo_rat->integer.
                res_denom = res_denom * lo_rat->denominator.
                lv_gcd = lcl_lisp_rational=>gcd(  n = res_nummer
                                                  d = res_denom ).
                res_nummer = res_nummer DIV lv_gcd.
                res_denom = res_denom DIV lv_gcd.

              WHEN lcl_lisp=>type_integer.
                res_nummer = res_int * lo_rat->integer.
                res_denom = lo_rat->denominator.

                lv_gcd = lcl_lisp_rational=>gcd(  n = res_nummer
                                                  d = res_denom ).
                res_nummer = res_nummer DIV lv_gcd.
                res_denom = res_denom DIV lv_gcd.
                lv_type = lcl_lisp=>type_rational.

              WHEN OTHERS.
                lv_type = lcl_lisp=>type_rational.
                res_nummer = lo_rat->integer.
                res_denom = lo_rat->denominator.
            ENDCASE.

*          WHEN lcl_lisp=>type_complex.
          WHEN OTHERS.
            throw( |{ cell->to_string( ) } is not a number in [*]| ).
        ENDCASE.

      ENDWHILE.

      result_arith `[*]`.
    ENDMETHOD.                    "proc_multiply

    METHOD proc_subtract.
      cell_arith_definition.
      validate list.

      DATA(iter) = list->new_iterator( ).
      IF iter->has_next( ) EQ abap_false.
        throw( |no number in [-]| ).
      ENDIF.

      DATA(cell) = iter->next( ).
      cell_arith - `[-]`.

      IF iter->has_next( ) EQ abap_false.
        res_int = 0 - res_int.
        res_real = 0 - res_real.
        res_nummer = 0 - res_nummer.
      ELSE.
*       Subtract all consecutive numbers from the first
        WHILE iter->has_next( ).
          cell = iter->next( ).
          cell_arith - `[-]`.
        ENDWHILE.
      ENDIF.

      result_arith `[-]`.
    ENDMETHOD.                    "proc_subtract

    METHOD proc_divide.
      cell_arith_definition.

      validate list.
      DATA(iter) = list->new_iterator( ).
      IF iter->has_next( ) EQ abap_false.
        throw( |no number in [/]| ).
      ENDIF.
      DATA(cell) = iter->next( ).
      cell_arith / `[/]`.

      TRY.
          IF iter->has_next( ) EQ abap_false.
            CASE lv_type.
              WHEN lcl_lisp=>type_integer.
                res_denom = res_int.
                res_nummer = 1.
                lv_type = lcl_lisp=>type_rational.

              WHEN lcl_lisp=>type_rational.
                DATA(lv_saved_nummer) = res_nummer.
                res_nummer = res_denom.
                res_denom = lv_saved_nummer.

              WHEN lcl_lisp=>type_real.
                res_real = 1 / res_real.
            ENDCASE.
          ELSE.
            IF lv_type EQ lcl_lisp=>type_integer.
              res_nummer = res_int.
              res_denom = 1.
              lv_type = lcl_lisp=>type_rational.
            ENDIF.

            WHILE iter->has_next( ).
              cell = iter->next( ).

              CASE cell->type.
                WHEN lcl_lisp=>type_integer.
                  lo_int ?= cell.

                  CASE lv_type.
                    WHEN lcl_lisp=>type_real.
                      res_real = res_real / lo_int->integer.

                    WHEN lcl_lisp=>type_rational.
                      res_denom = res_denom * lo_int->integer.
                      lv_gcd = lcl_lisp_rational=>gcd(  n = res_nummer
                                                        d = res_denom ).
                      res_nummer = res_nummer DIV lv_gcd.
                      res_denom = res_denom DIV lv_gcd.

                    WHEN lcl_lisp=>type_integer.
                      res_nummer = res_int.
                      res_denom = lo_int->integer.
                      lv_type = lcl_lisp=>type_rational.

                    WHEN OTHERS.
                      throw( 'internal error proc_divide( )' ).
                      lv_type = lcl_lisp=>type_integer.
                      res_int = lo_int->integer.
                  ENDCASE.

                WHEN lcl_lisp=>type_real.
                  lo_real ?= cell.

                  CASE lv_type.
                    WHEN lcl_lisp=>type_real.
                      res_real = res_real / lo_real->real.

                    WHEN lcl_lisp=>type_rational.
                      res_real = res_nummer / res_denom / lo_real->real.
                      lv_type = lcl_lisp=>type_real.

                    WHEN lcl_lisp=>type_integer.
                      res_real = res_int / lo_real->real.
                      lv_type = lcl_lisp=>type_real.

                    WHEN OTHERS.
                      lv_type = lcl_lisp=>type_real.
                      res_real = lo_real->real.
                  ENDCASE.

                WHEN lcl_lisp=>type_rational.
                  lo_rat ?= cell.

                  CASE lv_type.
                    WHEN lcl_lisp=>type_real.
                      res_real = res_real * lo_rat->denominator / lo_rat->integer.
                      lv_type = lcl_lisp=>type_real.

                    WHEN lcl_lisp=>type_rational.
                      res_nummer = res_nummer * lo_rat->denominator.
                      res_denom = res_denom * lo_rat->integer.
                      lv_gcd = lcl_lisp_rational=>gcd(  n = res_nummer
                                                        d = res_denom ).
                      res_nummer = res_nummer DIV lv_gcd.
                      res_denom = res_denom DIV lv_gcd.

                    WHEN lcl_lisp=>type_integer.
                      res_nummer = res_int * lo_rat->denominator.
                      res_denom = lo_rat->integer.

                      lv_type = lcl_lisp=>type_rational.

                    WHEN OTHERS.
                      lv_type = lcl_lisp=>type_rational.
                      res_nummer = lo_rat->integer.
                      res_denom = lo_rat->denominator.
                  ENDCASE.

*                WHEN lcl_lisp=>type_complex.
                WHEN OTHERS.
                  throw( |{ cell->to_string( ) } is not a number in [/]| ).
              ENDCASE.

            ENDWHILE.
          ENDIF.
          _catch_arithmetic_error.
      ENDTRY.

      result_arith `[/]`.
    ENDMETHOD.                    "proc_divide

**********************************************************************
    METHOD proc_gt.
      _comparison <= '[>]'.
    ENDMETHOD.                    "proc_gt

    METHOD proc_gte.
      _comparison < '[>=]'.
    ENDMETHOD.                    "proc_gte

    METHOD proc_lt.
      _comparison >= '[<]'.
    ENDMETHOD.                    "proc_lt

    METHOD proc_lte.
      _comparison > '[<=]'.
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
      validate: list.
      validate_integer list->car '[odd?]'.
      CHECK CAST lcl_lisp_integer( list->car )->integer MOD 2 NE 0.
      result = true.
    ENDMETHOD.                    "proc_lte

    METHOD proc_is_even.
      result = false.
      validate: list.
      validate_integer list->car '[even?]'.
      CHECK CAST lcl_lisp_integer( list->car )->integer MOD 2 EQ 0.
      result = true.
    ENDMETHOD.                    "proc_lte

**********************************************************************
    METHOD proc_eql.
      DATA lv_real TYPE tv_real.
      DATA lv_int TYPE tv_int.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_rat_2 TYPE REF TO lcl_lisp_rational.

      validate: list, list->car, list->cdr.

      result = nil.
      DATA(lo_ptr) = list.
      WHILE lo_ptr->cdr NE nil.
        validate_number: lo_ptr->car '[=]',
                         lo_ptr->cdr->car '[=]'.
        CASE lo_ptr->car->type.
          WHEN lcl_lisp=>type_integer.
            lv_int = CAST lcl_lisp_integer( lo_ptr->car )->integer.
            CASE lo_ptr->cdr->car->type.
              WHEN lcl_lisp=>type_integer.
                IF lv_int = CAST lcl_lisp_integer( lo_ptr->cdr->car )->integer.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

              WHEN lcl_lisp=>type_real.
                lv_real = CAST lcl_lisp_real( lo_ptr->cdr->car )->real.
                IF lv_int = trunc( lv_real ) AND frac( lv_real ) EQ 0.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

              WHEN lcl_lisp=>type_rational.
                lo_rat ?= lo_ptr->cdr->car.
                IF lv_int = lo_rat->integer AND lo_rat->denominator EQ 1.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

*              WHEN lcl_lisp=>type_complex.
            ENDCASE.

          WHEN lcl_lisp=>type_real.
            lv_real = CAST lcl_lisp_real( lo_ptr->car )->real.
            CASE lo_ptr->cdr->car->type.
              WHEN lcl_lisp=>type_integer.
                IF trunc( lv_real ) = CAST lcl_lisp_integer( lo_ptr->cdr->car )->integer AND frac( lv_real ) EQ 0.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

              WHEN lcl_lisp=>type_real.
                IF lv_real = CAST lcl_lisp_real( lo_ptr->cdr->car )->real.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

              WHEN lcl_lisp=>type_rational.
                lo_rat ?= lo_ptr->cdr->car.
                lv_real = lv_real * lo_rat->denominator.

                IF trunc( lv_real ) = lo_rat->integer AND frac( lv_real ) EQ 0.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

*              WHEN lcl_lisp=>type_complex.
            ENDCASE.

          WHEN lcl_lisp=>type_rational.
            lo_rat = CAST lcl_lisp_rational( lo_ptr->car ).
            CASE lo_ptr->cdr->car->type.
              WHEN lcl_lisp=>type_integer.
                IF lo_rat->integer = CAST lcl_lisp_integer( lo_ptr->cdr->car )->integer
                  AND lo_rat->denominator EQ 1.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

              WHEN lcl_lisp=>type_real.
                lv_real = ( CAST lcl_lisp_real( lo_ptr->cdr->car )->real ) * lo_rat->denominator.

                IF lo_rat->integer = trunc( lv_real ) AND frac( lv_real ) EQ 0.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

              WHEN lcl_lisp=>type_rational.
                lo_rat_2 ?= lo_ptr->cdr->car.
                IF lo_rat->integer = lo_rat_2->integer
                  AND lo_rat->denominator EQ lo_rat_2->denominator.
                  result = true.
                ELSE.
                  result = false.
                  EXIT.
                ENDIF.

*              WHEN lcl_lisp=>type_complex.
            ENDCASE.

*          WHEN lcl_lisp=>type_complex.
        ENDCASE.

        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
    ENDMETHOD.                    "proc_eql

    METHOD proc_eq.
      validate: list, list->car, list->cdr.

      result = nil.
      DATA(lo_ptr) = list.
      DATA(lo_ref) = lo_ptr->car.
      IF lo_ptr->cdr NE nil.
        IF lo_ref->type NE lo_ptr->cdr->car->type.
          result = false.
          EXIT.
        ENDIF.
        CASE lo_ptr->car->type.
          WHEN lcl_lisp=>type_integer.
            IF CAST lcl_lisp_integer( lo_ref )->integer = CAST lcl_lisp_integer( lo_ptr->cdr->car )->integer.
              result = true.
            ELSE.
              result = false.
              EXIT.
            ENDIF.

          WHEN lcl_lisp=>type_real.
            IF CAST lcl_lisp_real( lo_ref )->real = CAST lcl_lisp_real( lo_ptr->cdr->car )->real.
              result = true.
            ELSE.
              result = false.
              EXIT.
            ENDIF.

          WHEN lcl_lisp=>type_string.
            IF CAST lcl_lisp_string( lo_ref )->value = CAST lcl_lisp_string( lo_ptr->cdr->car )->value.
              result = true.
            ELSE.
              result = false.
              EXIT.
            ENDIF.

          WHEN lcl_lisp=>type_symbol.
            DATA(lo_symbol) = CAST lcl_lisp_symbol( lo_ref ).
            DATA(lo_s_car) = CAST lcl_lisp_symbol( lo_ptr->cdr->car ).
            IF lo_symbol->value = lo_s_car->value
              AND lo_symbol->index = lo_s_car->index.  " for uninterned symbols
              result = true.
            ELSE.
              result = false.
              EXIT.
            ENDIF.

          WHEN OTHERS.
            IF lo_ref = lo_ptr->cdr->car.
              result = true.
            ELSE.
              result = false.
              EXIT.
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
      validate: list, list->car.
      result = false.
      DATA(lo_ptr) = list.
      DATA(lo_slow) = list.

      WHILE lo_ptr->cdr NE nil.
        DATA(lo_next) = lo_ptr->cdr->car.
        validate lo_next.

        result = lo_next->is_equal( lo_ptr->car ).
        IF result EQ false.
          RETURN.
        ENDIF.
        lo_ptr = lo_ptr->cdr.
        lo_slow = lo_slow->cdr.

        CHECK lo_ptr->cdr NE nil.
        lo_next = lo_ptr->cdr->car.
        validate lo_next.

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
      validate: list, list->car.
      result = false.

      DATA(lo_ptr) = list.
      WHILE lo_ptr->cdr NE nil.
        DATA(lo_next) = lo_ptr->cdr->car.
        validate lo_next.
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

    METHOD proc_is_integer.
      _is_type integer.
    ENDMETHOD.                    "proc_is_integer

    METHOD proc_is_number. " argument in list->car
      result = false.
      CHECK list IS BOUND AND list->car IS BOUND.
      result = list->car->is_number( ).
    ENDMETHOD.                    "proc_is_integer

    METHOD proc_is_complex.
      _is_type complex.
    ENDMETHOD.

    METHOD proc_is_real.
      _is_type real.
    ENDMETHOD.

    METHOD proc_is_rational.
      _is_type rational.
    ENDMETHOD.

    METHOD proc_is_symbol.
      _is_type symbol.
    ENDMETHOD.

    METHOD proc_is_pair. " argument in list->car
      validate list.
      _is_type pair.
    ENDMETHOD.                    "proc_is_list

    METHOD proc_is_boolean. " argument in list->car
      validate list.
      _is_type boolean.
    ENDMETHOD.

    METHOD proc_is_vector.  " argument in list->car
      validate: list, list->car.
      _is_type vector.
    ENDMETHOD.

    METHOD proc_is_list.  " argument in list->car
      validate: list, list->car, list->cdr.
      IF list->cdr NE nil.
        throw( |list? takes only one argument| ).
      ENDIF.

      result = false.

      DATA(lo_ptr) = list->car.
      DATA(lo_slow) = lo_ptr.
*     Iterate over list
      WHILE lo_ptr->type EQ lcl_lisp=>type_pair.
        lo_ptr = lo_ptr->cdr.
        lo_slow = lo_slow->cdr.
        CHECK lo_ptr->type EQ lcl_lisp=>type_pair.
*       fast pointer takes 2 steps while slow pointer takes one
        lo_ptr = lo_ptr->cdr.
        CHECK lo_ptr = lo_slow.
*       If fast pointer eventually equals slow pointer, then we must be stuck in a circular list.
        result = true.
        RETURN.
      ENDWHILE.

      CHECK lo_ptr EQ nil.
*     the last element of a list must be nil
      result = true.
    ENDMETHOD.                    "proc_is_list

    METHOD proc_list_is_boolean.
      DATA lo_test TYPE REF TO lcl_lisp.
      DATA lo_arg TYPE REF TO lcl_lisp.

      validate list.

      result = false.
      lo_arg = list.

      lo_test = nil.
      IF lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>type_boolean.
        lo_test = lo_arg->car.
        lo_arg = lo_arg->cdr.
      ENDIF.
      IF lo_test EQ nil.
        throw( |boolean=? missing boolean argument in { lo_arg->car->to_string( ) }| ).
      ENDIF.

      WHILE lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>type_boolean.
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
      validate list.

      result = false.

      DATA(lo_arg) = list->car.
      WHILE lo_arg->type = lcl_lisp=>type_pair.
        IF lo_arg->car->type = lcl_lisp=>type_pair.
          RETURN.
        ENDIF.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

      CHECK lo_arg EQ nil.
      result = true.
    ENDMETHOD.                    "proc_is_alist

    METHOD proc_abs.
      _math abs '[abs]'.
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
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      result = nil.
      validate list.
      TRY.
          get_number carry list->car '[asinh]'.
          _is_last_param list.
          result = lcl_lisp_new=>real( log( carry + sqrt( carry ** 2 + 1 ) ) ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_asinh

    METHOD proc_acosh.
      DATA carry TYPE f.
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      result = nil.
      validate list.
      TRY.
          get_number carry list->car '[acosh]'.
          _is_last_param list.
          result = lcl_lisp_new=>real( log( carry + sqrt( carry ** 2 - 1 ) ) ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_acosh

    METHOD proc_atanh.
      DATA carry TYPE f.
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      result = nil.
      validate list.
      TRY.
          get_number carry list->car '[atanh]'.
          _is_last_param list.
          result = lcl_lisp_new=>real( ( log( 1 + carry ) - log( 1 - carry ) ) / 2 ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_atanh

    METHOD proc_expt.
      DATA num1 TYPE tv_real.
      DATA exp1 TYPE tv_real.
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      result = nil.
      validate: list, list->cdr.
      get_number num1 list->car '[expt]'.
      get_number exp1 list->cdr->car '[expt]'.

      _is_last_param list->cdr.
      TRY.
          result = lcl_lisp_new=>real( num1 ** exp1 ).
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

    METHOD proc_floor.
      _math floor '[floor]'.
    ENDMETHOD.                    "proc_floor

    METHOD proc_ceiling.
      _math ceil '[ceil]'.
    ENDMETHOD.                    "proc_ceiling

    METHOD proc_truncate.
      _math trunc '[truncate]'.
    ENDMETHOD.                    "proc_truncate

    METHOD proc_round.
      DATA carry TYPE tv_real.
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      result = nil.
      validate list.
      get_number carry list->car '[round]'.
      _is_last_param list.
      TRY.
          result = lcl_lisp_new=>number( round( val = carry dec = 0 ) ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_round

    METHOD proc_remainder.
      DATA numerator TYPE tv_real.
      DATA denominator TYPE tv_real.
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      result = nil.
      validate: list, list->cdr.
      get_number numerator list->car '[remainder]'.
      get_number denominator list->cdr->car '[remainder]'.
      _is_last_param list->cdr.
      TRY.
          result = lcl_lisp_new=>number( numerator - denominator * trunc( numerator / denominator ) ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_remainder

    METHOD proc_quotient.
      DATA numerator TYPE tv_real.
      DATA denominator TYPE tv_real.
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      result = nil.
      validate: list, list->cdr.
      get_number numerator list->car '[quotient]'.
      get_number denominator list->cdr->car '[quotient]'.
      _is_last_param list->cdr.
      TRY.
          result = lcl_lisp_new=>number( trunc( numerator / denominator ) ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_quotient

    METHOD proc_modulo.
      DATA numerator TYPE tv_real.
      DATA base TYPE tv_real.
      DATA mod TYPE tv_real.
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      result = nil.
      validate: list, list->cdr.
      get_number numerator list->car '[modulo]'.
      get_number base list->cdr->car '[modulo]'.
      _is_last_param list->cdr.
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
      result = nil.
      validate list.
      validate_integer list->car '[random]'.
      _is_last_param list.
      TRY.
          DATA(lo_rnd) = cl_abap_random=>create( cl_abap_random=>seed( ) ).
          result = lcl_lisp_new=>number( lo_rnd->intinrange( high = CAST lcl_lisp_integer( list->car )->integer ) ).
        CATCH cx_dynamic_check INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.                    "proc_random

    METHOD proc_is_exact.
      validate list.
      validate_number list->car `exact?`.
      result = false.
      CHECK list->car->type EQ lcl_lisp=>type_integer.
      result = true.
    ENDMETHOD.

    METHOD proc_is_inexact.
      validate list.
      validate_number list->car `inexact?`.
      result = false.
      CHECK list->car->type EQ lcl_lisp=>type_real.
      result = true.
    ENDMETHOD.

    METHOD proc_num_to_string.
      validate list.
      validate_number list->car `number->string`.
      result = lcl_lisp_new=>string( list->car->to_string( ) ).
    ENDMETHOD.

    METHOD proc_string_to_num.
      validate list.
      validate_string list->car `string->number`.
      result = lcl_lisp_new=>number( list->car->value ).
    ENDMETHOD.

    METHOD proc_newline.
      result = write( io_elem = lcl_lisp=>new_line
                      io_arg = list ).
    ENDMETHOD.

    METHOD proc_write.
      validate: list, list->car.
      result = write( io_elem = list->car
                      io_arg = list->cdr ).
    ENDMETHOD.

    METHOD proc_write_string.
      validate list.
      validate_string list->car `write-string`.
      result = write( io_elem = list->car
                      io_arg = list->cdr ).
    ENDMETHOD.

    METHOD proc_write_char.
      validate list.
      validate_char list->car `write-char`.
      result = write( io_elem = list->car
                      io_arg = list->cdr ).
    ENDMETHOD.

    METHOD proc_display.
      validate: list, list->car.
      result = display( io_elem = list->car
                        io_arg = list->cdr ).
    ENDMETHOD.

    METHOD proc_read.
      result = read( io_arg = list ).
    ENDMETHOD.

    METHOD proc_read_char.
      result = read_char( io_arg = list ).
    ENDMETHOD.

    METHOD proc_peek_char.
      optional_port list input `peek-char`.
      result = lcl_lisp_new=>char( li_port->peek_char( ) ).
    ENDMETHOD.

    METHOD proc_is_char_ready.
      optional_port list input `char-ready?`.

      result = false.
      CHECK li_port->is_char_ready( ).
      result = true.
    ENDMETHOD.

    METHOD proc_string.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      DATA lv_text TYPE string.

      validate: list, list->car.
      lo_ptr = list.
      WHILE lo_ptr->type EQ lcl_lisp=>type_pair AND lo_ptr->car->type EQ lcl_lisp=>type_char.
        lv_text = lv_text && lo_ptr->car->value+0(1).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      IF lo_ptr NE nil.
        throw( |{ lo_ptr->to_string( ) } is not a list of char | ).
      ENDIF.

      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_make_string.
      DATA lv_len TYPE sytabix.
      DATA lv_char TYPE c LENGTH 1.
      DATA lv_text TYPE string.

      validate: list.
      validate_integer list->car 'make-string'.
      lv_len = CAST lcl_lisp_integer( list->car )->integer.

      IF list->cdr NE nil.
        DATA(lo_char) = list->cdr->car.
        validate_char lo_char 'make-string'.
        lv_char = lo_char->value+0(1).
      ENDIF.

      DO lv_len TIMES.
        lv_text = lv_text && lv_char.
      ENDDO.
      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_string_to_list.
      DATA lv_char TYPE c LENGTH 1.
      DATA lv_start TYPE sytabix.
      DATA lv_len TYPE sytabix.
      DATA lv_len_1 TYPE sytabix.
      DATA lv_text TYPE string.
      DATA lo_int TYPE REF TO lcl_lisp_integer.

      validate: list, list->car.

      validate: list->cdr.
      IF list->cdr NE nil.
        validate_integer list->cdr->car 'string->list start'.
        lo_int ?= list->cdr->car.
        lv_start = lo_int->integer.

        validate list->cdr->cdr.
        IF list->cdr->cdr NE nil.
          validate_integer list->cdr->cdr->car 'string->list end'.
          lo_int ?= list->cdr->cdr->car.
          lv_len = lo_int->integer - lv_start.
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

    METHOD proc_list_is_string.
      validate list.

      result = false.

      DATA(lo_arg) = list.

      WHILE lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>type_string.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

      CHECK lo_arg = nil.
      result = true.
    ENDMETHOD.

    METHOD proc_string_length.
      validate: list.
      validate_string list->car 'string-length'.

      result = lcl_lisp_new=>integer( strlen( list->car->value ) ).
    ENDMETHOD.

    METHOD proc_substring.
      DATA lv_start TYPE sytabix.
      DATA lv_len TYPE sytabix.
      DATA lv_end TYPE sytabix.
      DATA lv_text TYPE string.
      DATA lo_int TYPE REF TO lcl_lisp_integer.

      validate: list, list->cdr, list->cdr->cdr.
      validate_string list->car 'substring'.
      validate_index list->cdr->car 'substring'.
      validate_integer list->cdr->cdr->car 'substring'.

      lo_int ?=  list->cdr->car.
      lv_start = lo_int->integer.
      lo_int ?=  list->cdr->cdr->car.
      lv_end = lo_int->integer.
      lv_len = lv_end - lv_start + 1.
      "lv_text = list->car->value+lv_start(lv_len).
      lv_text = substring( val = list->car->value off = lv_start len = lv_len ).

      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_string_ref.
      DATA lv_index TYPE sytabix.
      DATA lv_char TYPE c LENGTH 1.
      DATA lo_int TYPE REF TO lcl_lisp_integer.

      validate: list, list->cdr.
      validate_string list->car 'string-ref'.
      validate_index list->cdr->car 'string-ref'.

      lo_int ?= list->cdr->car.
      lv_index = lo_int->integer.
      lv_char = list->car->value+lv_index(1).

      result = lcl_lisp_new=>char( lv_char ).
    ENDMETHOD.

    METHOD proc_string_set.
      DATA lv_index TYPE sytabix.
      DATA lv_char TYPE c LENGTH 1.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_char TYPE REF TO lcl_lisp_char.
      DATA lo_string TYPE REF TO lcl_lisp_string.

      DATA lv_len TYPE sytabix.
      DATA lv_text TYPE string.

      validate: list, list->cdr, list->cdr->cdr.
      validate_string list->car 'string-set!'.
      validate_index list->cdr->car 'string-set!'.
      validate_char list->cdr->cdr->car 'string-set!'.

      lo_char ?= list->cdr->cdr->car.
      lv_char = lo_char->value(1).

      lo_int ?= list->cdr->car.
      lv_index = lo_int->integer.

      lo_string ?= list->car.
      validate_mutable lo_string `in string-set!`.
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

      validate: list, list->car.

      lo_ptr = list->car.
      WHILE lo_ptr->type = lcl_lisp=>type_pair AND lo_ptr->car->type EQ lcl_lisp=>type_char.
        lv_text = lv_text && lo_ptr->car->value+0(1).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      IF lo_ptr NE nil.
        throw( |{ lo_ptr->to_string( ) } is not a list of char| ).
      ENDIF.
      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_symbol_to_string.
      validate: list, list->car.

      IF list->car->type = lcl_lisp=>type_symbol.
        result = lcl_lisp_new=>string( value = list->car->value
                                       iv_mutable = abap_false ).
      ELSE.
        throw( |{ list->car->to_string( ) } is not a symbol| ).
      ENDIF.
    ENDMETHOD.

    METHOD proc_string_to_symbol.
      validate: list, list->car.

      IF list->car->type = lcl_lisp=>type_string.
        result = lcl_lisp_new=>symbol( list->car->value ).
      ELSE.
        throw( |{ list->car->to_string( ) } is not a string| ).
      ENDIF.
    ENDMETHOD.

    METHOD proc_string_append.
      DATA lv_text TYPE string.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      validate: list, list->car.

      lo_ptr = list.
      WHILE lo_ptr->type = lcl_lisp=>type_pair AND lo_ptr->car->type EQ lcl_lisp=>type_string.
        lv_text = lv_text && lo_ptr->car->value.
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      IF lo_ptr NE nil.
        throw( |{ lo_ptr->car->to_string( ) } is not a string| ).
      ENDIF.
      result = lcl_lisp_new=>string( lv_text ).
    ENDMETHOD.

    METHOD proc_max.
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      DATA carry TYPE tv_real.
      DATA lv_max TYPE tv_real.

      validate: list, list->cdr.
      get_number lv_max list->car '[max]'.

      lo_ptr = list->cdr.
      WHILE lo_ptr->type EQ lcl_lisp=>type_pair.
        get_number carry lo_ptr->car '[max]'.
        lv_max = nmax( val1 = carry val2 = lv_max ).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      result = lcl_lisp_new=>number( lv_max ).
    ENDMETHOD.

    METHOD proc_min.
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      DATA carry TYPE tv_real.
      DATA lv_min TYPE tv_real.

      validate: list, list->cdr.
      get_number lv_min list->car '[min]'.

      lo_ptr = list->cdr.
      WHILE lo_ptr->type EQ lcl_lisp=>type_pair.
        get_number carry lo_ptr->car '[min]'.
        lv_min = nmin( val1 = carry val2 = lv_min ).
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
      result = lcl_lisp_new=>number( lv_min ).
    ENDMETHOD.

    METHOD proc_gcd.
*     non-negative greatest common divisor of the arguments
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      DATA carry TYPE tv_real.
      DATA lv_gcd TYPE tv_real VALUE 0.

      validate list.
      IF list NE nil.

        get_number lv_gcd list->car '[gcd]'.

        lo_ptr = list->cdr.
        WHILE lo_ptr->type EQ lcl_lisp=>type_pair.
          get_number carry lo_ptr->car '[gcd]'.
          lv_gcd = lcl_lisp_rational=>gcd( n = carry d = lv_gcd ).
          lo_ptr = lo_ptr->cdr.
        ENDWHILE.
      ENDIF.
      result = lcl_lisp_new=>number( lv_gcd ).
    ENDMETHOD.

    METHOD proc_lcm.
*     non-negative least common multiple of the arguments
      DATA cell TYPE REF TO lcl_lisp.
      DATA lo_ptr TYPE REF TO lcl_lisp.
      DATA lo_rat TYPE REF TO lcl_lisp_rational.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
      DATA lo_real TYPE REF TO lcl_lisp_real.

      DATA carry TYPE tv_real.
      DATA lv_lcm TYPE tv_real VALUE 1.

      validate list.
      IF list NE nil.

        get_number lv_lcm list->car '[lcm]'.

        lo_ptr = list->cdr.
        WHILE lo_ptr->type EQ lcl_lisp=>type_pair.
          get_number carry lo_ptr->car '[lcm]'.
          lv_lcm = lv_lcm * carry / lcl_lisp_rational=>gcd( n = carry d = lv_lcm ).
          lo_ptr = lo_ptr->cdr.
        ENDWHILE.
      ENDIF.
      result = lcl_lisp_new=>number( abs( lv_lcm ) ).
    ENDMETHOD.

    METHOD proc_is_textual_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.

      validate: list, list->car.
      result = false.
      CHECK list->car->type EQ lcl_lisp=>type_port.
      lo_port ?= list->car.
      CHECK lo_port->port_type EQ lcl_lisp_port=>c_port_textual.
      result = true.
    ENDMETHOD.

    METHOD proc_is_binary_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.

      validate: list, list->car.
      result = false.
      CHECK list->car->type EQ lcl_lisp=>type_port.
      lo_port ?= list->car.
      CHECK lo_port->port_type EQ lcl_lisp_port=>c_port_binary.
      result = true.
    ENDMETHOD.

    METHOD proc_is_port.
      validate: list, list->car.
      result = false.
      CHECK list->car->type EQ lcl_lisp=>type_port.
      result = true.
    ENDMETHOD.

    METHOD proc_is_input_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.

      validate: list, list->car.
      result = false.
      CHECK list->car->type EQ lcl_lisp=>type_port.
      lo_port ?= list->car.
      CHECK lo_port->input EQ abap_true.
      result = true.
    ENDMETHOD.

    METHOD proc_is_output_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.

      validate: list, list->car.
      result = false.
      CHECK list->car->type EQ lcl_lisp=>type_port.
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
      result = go_output_port.
    ENDMETHOD.

    METHOD proc_current_input_port.
      result = go_input_port.
    ENDMETHOD.

    METHOD proc_current_error_port.
      result = go_error_port.
    ENDMETHOD.

    METHOD proc_close_output_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.
      validate list.
      validate_port list->car `close-output-port`.
      lo_port ?= list->car.
      lo_port->close_output( ).
      result = nil.
    ENDMETHOD.

    METHOD proc_close_input_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.
      validate list.
      validate_port list->car `close-input-port`.
      lo_port ?= list->car.
      lo_port->close_input( ).
      result = nil.
    ENDMETHOD.

    METHOD proc_close_port.
      DATA lo_port TYPE REF TO lcl_lisp_port.
      validate list.
      validate_port list->car `close-port`.
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
      validate list.
      validate_string list->car `open-input-string`.

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
      validate list.
      validate_port list->car `get-output-string`.

      lo_port ?= list->car.
      result = lcl_lisp_new=>string( lo_port->flush( ) ).
    ENDMETHOD.

    METHOD proc_eof_object.
      result = lcl_lisp=>eof_object. " cannot be read using (read), must not be unique
    ENDMETHOD.

    METHOD proc_is_eof_object.
      validate: list, list->car.
      result = false.
      CHECK list->car EQ lcl_lisp=>eof_object.
      result = true.
    ENDMETHOD.

    METHOD proc_is_char_alphabetic.
      DATA lv_char TYPE char01.
      validate list.
      validate_char list->car `char-alphabetic?`.
      result = false.
      lv_char = list->car->value.
      CHECK lv_char CN '0123456789'.
      result = true.
    ENDMETHOD.

    METHOD proc_is_char_numeric.
      DATA lv_char TYPE char01.
      validate list.
      validate_char list->car `char-numeric?`.
      result = false.
      lv_char = list->car->value.
      CHECK lv_char CO '0123456789'.
      result = true.
    ENDMETHOD.

    METHOD proc_digit_value.
      DATA lv_char TYPE char01.
      DATA lv_int TYPE i.

      validate list.
      validate_char list->car `digit-value`.

      lv_char = list->car->value.
      IF lv_char CO '0123456789'.
        lv_int = lv_char. " conversion
        result = lcl_lisp_new=>integer( lv_int ).
      ELSE.
        result = false.
      ENDIF.
    ENDMETHOD.

    METHOD proc_is_char_whitespace.
      validate list.
      validate_char list->car `char-whitespace?`.

      CASE list->car.
        WHEN lcl_lisp=>char_space OR lcl_lisp=>char_tab OR lcl_lisp=>char_newline.
          result = true.
        WHEN OTHERS.
          result = false.
      ENDCASE.
    ENDMETHOD.

    METHOD proc_is_char_upper_case.
      validate list.
      validate_char list->car `char-upper-case?`.
      result = false.
      CHECK list->car->value NE to_lower( list->car->value ).
      result = true.
    ENDMETHOD.

    METHOD proc_is_char_lower_case.
      validate list.
      validate_char list->car `char-lower-case?`.
      result = false.
      CHECK list->car->value NE to_upper( list->car->value ).
      result = true.
    ENDMETHOD.

    METHOD proc_char_to_integer.
      DATA lv_char TYPE char01.
      FIELD-SYMBOLS <xword> TYPE x.
      FIELD-SYMBOLS <xint> TYPE x.
      DATA lv_int TYPE int2.

      validate list.
      validate_char list->car `char->integer`.
      lv_char = list->car->value+0(1).
      ASSIGN lv_char TO <xword> CASTING.
      ASSIGN lv_int TO <xint> CASTING.
      <xint> = <xword>.
      result = lcl_lisp_new=>integer( lv_int ).
    ENDMETHOD.

    METHOD proc_integer_to_char.
      DATA lv_char TYPE char01.
      FIELD-SYMBOLS <xchar> TYPE x.
      FIELD-SYMBOLS <xint> TYPE x.
      DATA lv_int TYPE int2.
      DATA lo_int TYPE REF TO lcl_lisp_integer.

      validate list.
      validate_integer list->car `integer->char`.
      lo_int ?= list->car.
      lv_int = lo_int->integer.

      ASSIGN lv_int TO <xint> CASTING.
      ASSIGN lv_char TO <xchar> CASTING.
      <xchar> = <xint>.

      result = lcl_lisp_new=>char( lv_char ).
    ENDMETHOD.

    METHOD proc_char_upcase.
      validate list.
      validate_char list->car `char-upcase`.
      result = lcl_lisp_new=>char( to_upper( list->car->value ) ).
    ENDMETHOD.

    METHOD proc_char_downcase.
      validate list.
      validate_char list->car `char-downcase`.
      result = lcl_lisp_new=>char( to_lower( list->car->value ) ).
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

      validate list.
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
      validate: list, list->car.

      IF list->car = nil OR ( list->car->type NE lcl_lisp=>type_string
                            AND list->car->type NE lcl_lisp=>type_symbol ).
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
      validate: list, list->car.
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
      validate: list, list->car.
      DATA(lo_ref) = list->car.
      IF lo_ref->type NE lcl_lisp=>type_abap_table.
        throw( |ab-append-row requires ABAP table as parameter| ).
      ENDIF.
      throw( `ab-append-row not implemented yet` ).
    ENDMETHOD.                    "proc_abap_append_row

    METHOD proc_abap_delete_row.
      validate: list, list->car.
      DATA(lo_ref) = list->car.
      IF lo_ref->type NE lcl_lisp=>type_abap_table.
        throw( |ab-delete-row requires ABAP table as parameter| ).
      ENDIF.
      throw( `ab-delete-row not implemented yet` ).
    ENDMETHOD.                    "proc_abap_delete_row

    METHOD proc_abap_get_row.
      validate: list, list->car.
      DATA(lo_ref) = list->car.
      IF lo_ref->type NE lcl_lisp=>type_abap_table.
        throw( |ab-get-row requires ABAP table as parameter| ).
      ENDIF.
      throw( `ab-get-row not implemented yet` ).
    ENDMETHOD.                    "proc_abap_get_row

**********************************************************************
    METHOD proc_abap_get_value. "Convert ABAP to Lisp data
      DATA lo_ref TYPE REF TO lcl_lisp_data.
      FIELD-SYMBOLS <data> TYPE any.

      validate: list, list->car.
      IF list->car->type NE lcl_lisp=>type_abap_data AND
         list->car->type NE lcl_lisp=>type_abap_table.
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

      validate: list, list->car.
      IF list->car->type NE lcl_lisp=>type_abap_data AND
         list->car->type NE lcl_lisp=>type_abap_table.
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
      validate: list, list->car.
*     The first parameter must be a function module instance
      IF list->car->type NE lcl_lisp=>type_abap_function.
        throw( |{ list->car->value } is not a function module reference| ).
      ENDIF.

      TRY.
          result = CAST lcl_lisp_abapfunction( list->car )->call( list->car ).
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
      validate: list, list->car.

*     Ensure a valid first parameter is passed
      IF list->car->type NE lcl_lisp=>type_abap_data
        AND list->car->type NE lcl_lisp=>type_abap_function
        AND list->car->type NE lcl_lisp=>type_abap_table.
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

      validate: list, list->car.

*     Ensure a valid first parameter is passed
      IF list->car->type NE lcl_lisp=>type_abap_data
        AND list->car->type NE lcl_lisp=>type_abap_function
        AND list->car->type NE lcl_lisp=>type_abap_table.
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
          WHEN lcl_lisp=>type_string OR lcl_lisp=>type_symbol.
            <target> = lo_source->value.
          WHEN lcl_lisp=>type_integer.
            <target> = CAST lcl_lisp_integer( lo_source )->integer.
          WHEN lcl_lisp=>type_real.
            <target> = CAST lcl_lisp_real( lo_source )->real.
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
          IF element->type NE lcl_lisp=>type_pair.
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
          IF element->type NE lcl_lisp=>type_pair.
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
            WHEN lcl_lisp=>type_integer.
              <field> = CAST lcl_lisp_integer( element )->integer.
            WHEN lcl_lisp=>type_real.
              <field> = CAST lcl_lisp_real( element )->real.
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
        ( identifier->type NE lcl_lisp=>type_string AND identifier->type NE lcl_lisp=>type_symbol ).
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

      validate_integer identifier `ab-get`.
      ASSIGN element->data->* TO <idxtab>.
      TRY.
          lo_int ?= identifier.
          rdata = REF #( <idxtab>[ lo_int->integer ] ).
        CATCH cx_sy_itab_line_not_found.
          throw( |ab-get: No entry at index { lo_int->integer }| ). "Can do AB-TAB-WHERE some other time
      ENDTRY.
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

      IF element->type = lcl_lisp=>type_abap_function.
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
      DATA lo_sql TYPE REF TO lcl_lisp_query.
      DATA lo_string TYPE REF TO lcl_lisp_string.
      validate: list, list->car.
      lo_string ?= list->car.

      result ?= lcl_lisp_new=>query( value = lo_string->value ).
    ENDMETHOD.

    METHOD proc_sql_query.
      DATA lo_sql TYPE REF TO lcl_lisp_query.
      DATA lo_string TYPE REF TO lcl_lisp_string.
*     sql-query
      validate: list, list->cdr.

      TRY.
          lo_string ?= list->car.
          lo_sql ?= lcl_lisp_new=>query( value = lo_string->value ).
          result ?= lo_sql->execute( lo_string->value ).
        CATCH cx_sql_exception INTO DATA(lx_error).
          throw( |SQL query { lx_error->get_text( ) }| ).
      ENDTRY.
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

    METHOD new.
      ro_env = NEW #( ).
      ro_env->outer = io_outer.
    ENDMETHOD.                    "new

    METHOD find.
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
**     Clever but probably expensive TRY / CATCH. This logic is use very often
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
                                    value = value
                                    parameter = parameter ).
      set( symbol = symbol
           element = element ).
    ENDMETHOD.                    "define_cell

    METHOD set.
*     Add a value to the (local) environment
      DATA(ls_map) = VALUE ts_map( symbol = symbol
                                   value = element ).
      INSERT ls_map INTO TABLE map.
      CHECK sy-subrc = 4.                " To comply with Scheme define,
      MODIFY TABLE map FROM ls_map.      " overwrite existing defined values
    ENDMETHOD.                    "define

    METHOD set_once.
*     Add a value to the (local) environment. The value must not exist yet
      DATA(ls_map) = VALUE ts_map( symbol = symbol
                                   value = element ).
      INSERT ls_map INTO TABLE map.
      CHECK sy-subrc = 4.
*     It is an error for a <variable> to appear more than once in the list of variables.
      lcl_lisp=>throw( |variable { symbol } appears more than once| ).
    ENDMETHOD.                    "define

    METHOD parameters_to_symbols.
*     The lambda receives its own local environment in which to execute, where parameters
*     become symbols that are mapped to the corresponding arguments
*     Assign each argument to its corresponding symbol in the newly created environment
      DATA lv_count TYPE i.

      CASE io_pars->type.
        WHEN lcl_lisp=>type_pair.   "Do we have a proper list?

          DATA(lo_var) = io_pars.                " Pointer to formal parameters
          DATA(lo_arg) = io_args.                " Pointer to arguments
*         local environment used to detect duplicate variable usage (invalid)
          DATA(local) = lcl_lisp_environment=>new( me ).

          WHILE lo_var NE lcl_lisp=>nil.         " Nil would mean no parameters to map

            IF lo_var->type EQ lcl_lisp=>type_symbol.
*             dotted pair after fixed number of parameters, to be bound to a variable number of arguments

*             1) Read the next parameter, bind to the (rest) list of arguments
              local->set_once( symbol = lo_var->value
                               element = lcl_lisp=>nil ).
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
            local->set_once( symbol = lo_var->car->value
                             element = lcl_lisp=>nil ).
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

        WHEN lcl_lisp=>type_symbol.
*         args is a symbol to be bound to a variable number of parameters
          set( symbol = io_pars->value
               element = io_args ).

      ENDCASE.

    ENDMETHOD.                    "parameters_to_symbols

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
      response = super->eval_repl( code ).       " Evaluate given code
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

    METHOD is_equivalent. "eqv?
      validate io_elem.
      DATA lo_int TYPE REF TO lcl_lisp_integer.
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
        WHEN lcl_lisp=>type_integer.
* obj1 and obj2 are both exact numbers and are numerically equal (in the sense of =).
          lo_int ?= b.
          CHECK CAST lcl_lisp_integer( me )->integer = lo_int->integer.

        WHEN lcl_lisp=>type_real.
*obj1 and obj2 are both inexact numbers such that they are numerically equal (in the sense of =)
*and they yield the same results (in the sense of eqv?) when passed as arguments to any other
*procedure that can be defined as a finite composition of Scheme’s standard arithmetic procedures,
*provided it does not result in a NaN value.
          lo_real ?= b.
          CHECK CAST lcl_lisp_real( me )->real = lo_real->real.

        WHEN lcl_lisp=>type_symbol.
* obj1 and obj2 are both symbols and are the same symbol according to the symbol=? procedure (section 6.5).
          DATA(lo_symbol) = CAST lcl_lisp_symbol( me ).
          DATA(lo_s_b) = CAST lcl_lisp_symbol( b ).
          CHECK lo_symbol->value = lo_s_b->value
              AND lo_symbol->index = lo_s_b->index.  " for uninterned symbols

        WHEN lcl_lisp=>type_string.
* obj1 and obj2 are both characters and are the same character according to the char=? procedure (section 6.6).
          DATA(lo_string) = CAST lcl_lisp_string( me ).
          DATA(lo_str_b) = CAST lcl_lisp_string( b ).
          CHECK lo_string->value = lo_str_b->value.

        WHEN lcl_lisp=>type_pair.
* obj1 and obj2 are procedures whose location tags are equal (section 4.1.4).
          DATA(lo_pair) = CAST lcl_lisp_pair( me ).
          DATA(lo_p_b) = CAST lcl_lisp_pair( b ).
          CHECK lo_pair->car EQ lo_p_b->car AND lo_pair->cdr EQ lo_p_b->cdr.

        WHEN lcl_lisp=>type_lambda.
* obj1 and obj2 are procedures whose location tags are equal (section 4.1.4).
          DATA(lo_lambda) = CAST lcl_lisp_lambda( me ).
          DATA(lo_l_b) = CAST lcl_lisp_lambda( b ).
          CHECK lo_lambda->car EQ lo_l_b->car AND lo_lambda->cdr EQ lo_l_b->cdr
            AND lo_lambda->macro EQ lo_l_b->macro AND lo_lambda->environment = b->environment.

        WHEN OTHERS.
* obj1 and obj2 are pairs, vectors, bytevectors, records, or strings that denote the same location  in the store (section 3.4).

          CHECK me = b.
      ENDCASE.
      result = true.
    ENDMETHOD.

    METHOD is_equal. "equal?
* equal? returns the same as eqv? when applied to booleans, symbols, numbers, characters, ports,
* procedures, and the empty list. If two objects are eqv?, they must be equal? as well.
* In all other cases, equal? may return either #t or #f.
* Even if its arguments are circular data structures, equal? must always terminate.
      validate: io_elem.

      IF comp NE nil.
        DATA(lo_lambda) = comp->car.
        DATA(lo_head) = lcl_lisp_new=>list3( io_first = lo_lambda
                                             io_second = lcl_lisp_new=>quote( me )
                                             io_third = lcl_lisp_new=>quote( io_elem ) ).

        result = interpreter->eval( element = lo_head
                                    environment = lo_lambda->environment ).
        RETURN.
      ENDIF.

      result = false.

      CHECK type EQ io_elem->type.

      CASE type.

        WHEN lcl_lisp=>type_lambda.
          CHECK io_elem->car EQ io_elem->cdr.
          result = true.

        WHEN lcl_lisp=>type_pair.
          DATA(lo_a) = me.
          DATA(lo_slow) = me.
          DATA(lo_b) = io_elem.

          WHILE lo_a->type EQ lcl_lisp=>type_pair AND lo_b->type EQ lcl_lisp=>type_pair.
            IF lo_a->car EQ lo_a AND lo_b->car EQ lo_b.
*             Circular list
              result = true.
              RETURN.
            ELSEIF lo_a->car->is_equal( lo_b->car ) EQ false.
              RETURN.
            ENDIF.
            lo_slow = lo_slow->cdr.
            lo_a = lo_a->cdr.
            lo_b = lo_b->cdr.
            CHECK lo_a->type EQ lcl_lisp=>type_pair AND lo_b->type EQ lcl_lisp=>type_pair.
            IF lo_a->car->is_equal( lo_b->car ) EQ false.
              RETURN.
            ENDIF.
            lo_a = lo_a->cdr.
            lo_b = lo_b->cdr.
            CHECK lo_slow EQ lo_a.
*           Circular list
            result = true.
            RETURN.
          ENDWHILE.

          result = lo_a->is_equal( lo_b ).

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

        WHEN type_string.
          IF me EQ lcl_lisp=>new_line.
            str = |\n|.
          ELSE.
*           give back the string as a quoted string
            str = |"{ escape( val = value format = cl_abap_format=>e_html_js ) }"|.
          ENDIF.

        WHEN type_char.
          str = |"{ escape( val = value format = cl_abap_format=>e_html_js ) }"|.

        WHEN type_integer.
          DATA lv_real TYPE tv_real.
          DATA lo_int TYPE REF TO lcl_lisp_integer.

          lo_int ?= me.
          lv_real = lo_int->integer.
          str = lv_real.
          str = condense( str ).
        WHEN type_real.
          str = condense( CAST lcl_lisp_real( me )->real ).
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
        WHEN type_port.
          str = '<port>'.
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
      ENDCASE.
    ENDMETHOD.                    "to_string

    METHOD to_text.
      CASE type.
        WHEN type_string
          OR type_char.
          CASE me.
            WHEN lcl_lisp=>new_line.
              str = |\n|.
            WHEN lcl_lisp=>eof_object.
              str = '<eof>'.
            WHEN nil.
              str = space.
            WHEN OTHERS.
              str = value.
          ENDCASE.
        WHEN type_null.
          str = |'()|.
        WHEN OTHERS.
          str = to_string( ).
      ENDCASE.
    ENDMETHOD.

    METHOD error_not_a_pair.
      throw( context && to_string( ) && ` is not a pair` ).
    ENDMETHOD.

    METHOD throw.
      RAISE EXCEPTION TYPE lcx_lisp_exception
        EXPORTING
          message = message
          area    = c_area_eval.
    ENDMETHOD.                    "eval_err

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
        IF elem->cdr->type NE lcl_lisp=>type_pair.
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
        WHEN lcl_lisp=>type_integer.
          ro_elem ?= integer( value ).

        WHEN lcl_lisp=>type_real.
          ro_elem ?= real( value ).

        WHEN lcl_lisp=>type_string.
          ro_elem ?= string( value ).

        WHEN lcl_lisp=>type_char.
          ro_elem ?= char( value ).

        WHEN lcl_lisp=>type_boolean.
          ro_elem ?= boolean( value ).

        WHEN lcl_lisp=>type_abap_data.
          ro_elem ?= data( value ).

        WHEN lcl_lisp=>type_abap_table.
          ro_elem ?= table( value ).

        WHEN OTHERS.
          CREATE OBJECT ro_elem.
          ro_elem->type = type.
          ro_elem->value = value.
      ENDCASE.
      ro_elem->parameter_object = parameter.
    ENDMETHOD.

    METHOD string.
      ro_elem = NEW lcl_lisp_string( value = value
                                     iv_mutable = iv_mutable ).
    ENDMETHOD.

    METHOD char.
      READ TABLE lcl_lisp_char=>char_table INTO DATA(ls_char) WITH TABLE KEY char = value.
      IF sy-subrc EQ 0.
        ro_elem = ls_char-elem.
      ELSE.
        ro_elem = NEW lcl_lisp_char( ).
        ro_elem->type = lcl_lisp=>type_char.
        ro_elem->value = value.
        INSERT VALUE #( char = value
                        elem = ro_elem ) INTO TABLE lcl_lisp_char=>char_table.
      ENDIF.
    ENDMETHOD.

    METHOD charx.
      DATA lv_char TYPE char01.
      FIELD-SYMBOLS <xword> TYPE x.
      FIELD-SYMBOLS <xchar> TYPE x.
      DATA xword TYPE tv_xword.
      DATA lv_int TYPE int2.

      xword = value.
      lv_int = xword.
      ASSIGN lv_int TO <xword> CASTING.
      ASSIGN lv_char TO <xchar> CASTING.
      <xchar> = <xword>.
      ro_elem = char( lv_char ).
    ENDMETHOD.

    METHOD symbol.
      ro_elem = NEW lcl_lisp_symbol( ).
      ro_elem->type = lcl_lisp=>type_symbol.
      ro_elem->index = index.
      ro_elem->value = value.
    ENDMETHOD.

    METHOD boolean.
      ro_elem = NEW lcl_lisp_boolean( ).
      ro_elem->type = lcl_lisp=>type_boolean.
      ro_elem->value = value.
    ENDMETHOD.

    METHOD null.
      ro_elem = NEW lcl_lisp_null( ).
      ro_elem->type = lcl_lisp=>type_null.
      ro_elem->value = 'nil'.
    ENDMETHOD.

    METHOD integer.
      ro_elem = NEW lcl_lisp_integer( value ).
    ENDMETHOD.

    METHOD real.
      ro_elem = NEW lcl_lisp_real( value ).
    ENDMETHOD.

    METHOD number.
      DATA lv_int TYPE tv_int.
      TRY.
          MOVE EXACT value TO lv_int.
          ro_elem = integer( lv_int ).
        CATCH cx_sy_conversion_error.
          ro_elem = real( value ).
      ENDTRY.
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
      ro_elem = NEW lcl_lisp_rational( nummer = nummer
                                       denom = denom ).
    ENDMETHOD.

    METHOD atom.
      CASE value.
        WHEN lcl_lisp=>true->value.
          ro_elem = lcl_lisp=>true.

        WHEN lcl_lisp=>false->value.
          ro_elem = lcl_lisp=>false.

        WHEN OTHERS.
*         Check whether the token can be converted to a float, to cover all manner of number formats,
*         including scientific, otherwise treat it as a symbol (but we still store it as a string to
*         preserve the original value and let the ABAP kernel do the heavy lifting later on)
          DATA lv_nummer TYPE string.
          DATA lv_denom TYPE string.
          SPLIT value AT lcl_parser=>c_lisp_slash INTO lv_nummer lv_denom.
          IF sy-subrc EQ 0 AND lv_denom NE space.
            TRY.
                ro_elem = rational( nummer = CONV tv_int( lv_nummer )
                                    denom = CONV tv_int( lv_denom ) ).
                RETURN.
              CATCH cx_sy_conversion_no_number.
            ENDTRY.
          ENDIF.
          TRY.
              DATA(lv_real) = CONV tv_real( value ).
              ro_elem = number( value ).
            CATCH cx_sy_conversion_no_number.
              ro_elem = symbol( value ).
          ENDTRY.
      ENDCASE.
    ENDMETHOD.                    "new_atom

    METHOD data.
      ro_elem = NEW lcl_lisp_data( ).
      ro_elem->type = lcl_lisp=>type_abap_data.
      ro_elem->data = ref.
    ENDMETHOD.                    "new_data

    METHOD table.
      ro_elem = NEW lcl_lisp_table( ).
      ro_elem->type = lcl_lisp=>type_abap_table.
      ro_elem->data = ref.
    ENDMETHOD.                    "new_table

    METHOD query.
      TRY.
          ro_elem = NEW lcl_lisp_query( ).
        CATCH cx_sql_exception.
          ro_elem = lcl_lisp=>nil.
      ENDTRY.
    ENDMETHOD.

    METHOD cons.
      ro_cons = NEW lcl_lisp_pair( ).
      ro_cons->type = lcl_lisp=>type_pair.
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
      ro_vec = NEW lcl_lisp_vector( ).
      ro_vec->type = lcl_lisp=>type_vector.
      ro_vec->vector = it_vector.
      ro_vec->mutable = iv_mutable.
      ro_vec->mo_length = number( lines( it_vector ) ).
    ENDMETHOD.

    METHOD lambda.
*     The lambda is a special cell that stores a pointer to a list of parameters
*     and a pointer to a list which is the body to be evaluated later on
      DATA(lo_lambda) = NEW lcl_lisp_lambda( ).
      lo_lambda->type = lcl_lisp=>type_lambda.
      lo_lambda->car = io_car.               " List of parameters
      lo_lambda->cdr = io_cdr.               " Body
      lo_lambda->macro = iv_macro.

*     Store the reference to the environment in which the lambda was created
*     (lexical scope) e.g. if the lambda is created inside another lambda
*     we want that environment to be present when we evaluate the new lambda
      lo_lambda->environment = io_env.
      ro_lambda ?= lo_lambda.
    ENDMETHOD.                    "new_lambda

    METHOD hash.
      validate io_list.

      ro_hash = NEW lcl_lisp_hash( ).
      ro_hash->type = lcl_lisp=>type_hash.
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
      ro_elem->cdr->mutable = abap_false.
    ENDMETHOD.

    METHOD quasiquote.
      ro_elem = box( io_proc = lcl_lisp=>quasiquote
                     io_elem = io_elem ).
      ro_elem->cdr->mutable = abap_false.
    ENDMETHOD.

    METHOD function.
      validate: io_list, io_list->car.

      ro_func = NEW lcl_lisp_abapfunction( ).
      ro_func->type = lcl_lisp=>type_abap_function.
*     Determine the parameters of the function module to populate parameter table
      ro_func->value = ro_func->read_interface( io_list->car->value ).
    ENDMETHOD.

  ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_hash IMPLEMENTATION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_hash IMPLEMENTATION.

    METHOD eval.
      result = NEW lcl_lisp_hash( ).
      result->type = lcl_lisp=>type_hash.

      LOOP AT hash INTO DATA(ls_entry).
        INSERT VALUE #( key = ls_entry-key
                        element = interpreter->eval( element = ls_entry-element
                                                     environment = environment ) ) INTO TABLE result->hash.
      ENDLOOP.

    ENDMETHOD.

    METHOD fill.
      validate list.

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
      validate: list, list->car.
      IF list->car = nil.
        throw( |hash-get requires a key to access an element| ).
      ENDIF.

*      TODO: Additional check for key type
      result = VALUE #( hash[ key = list->car->value ]-element DEFAULT nil ).
    ENDMETHOD.                    "get

    METHOD insert.
      validate: list, list->car, list->cdr.

* TODO: Check number and type of parameters
      INSERT VALUE #( key = list->car->value
                      element = list->cdr->car ) INTO TABLE hash.
* TODO: Should we overwrite existing keys?
      result = nil.
    ENDMETHOD.                    "insert

    METHOD delete.
      validate: list, list->car.
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
      validate: list, list->car.
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
      DATA lv_end TYPE sytabix.

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
      validate_mutable me `vector`.

      DATA(lv_start) = index + 1.

      IF lv_start BETWEEN 1 AND lines( vector ).
        vector[ lv_start ] = io_elem.
      ELSE.
        throw( |vector-set!: out-of-bound position { index }| ).
      ENDIF.
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
      result = false.

      CHECK io_elem->type EQ type_vector.

      DATA(vec) = CAST lcl_lisp_vector( io_elem ).
      CHECK lines( vector ) = lines( vec->vector ).

      LOOP AT vec->vector INTO DATA(lo_elem).
        DATA(lo_vec) = vector[ sy-tabix ].
        CHECK lo_vec->is_equal( io_elem = lo_elem
                                comp = comp
                                interpreter = interpreter ) EQ false.
        RETURN.
      ENDLOOP.
      result = true.
    ENDMETHOD.

    METHOD eval.
      DATA lt_vector TYPE tt_lisp.

      LOOP AT vector INTO DATA(lo_list).
        APPEND interpreter->eval( element = lo_list
                                  environment = environment ) TO lt_vector.
      ENDLOOP.
      result = lcl_lisp_new=>vector( it_vector = lt_vector
                                     iv_mutable = abap_true ).
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_query IMPLEMENTATION.

    METHOD constructor.
      super->constructor( ).
      type = type_abap_query.
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
      result = NEW lcl_lisp_sql_result( lo_set ).
    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_lisp_sql_result IMPLEMENTATION.

    METHOD constructor.
      super->constructor( ).
      type = type_abap_sql_set.
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
