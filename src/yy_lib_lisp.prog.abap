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

  CONSTANTS:
    c_error_incorect_input TYPE string VALUE 'Incorrect input',
    c_error_unexpected_end TYPE string VALUE 'Unexpected end',
    c_error_eval           TYPE string VALUE 'EVAL( ) came up empty-handed'.
  CONSTANTS:
    c_area_eval  TYPE string VALUE `Eval`,
    c_area_parse TYPE string VALUE `Parse`.
  CONSTANTS:
    c_lisp_else TYPE string VALUE 'else',
    c_lisp_then TYPE c LENGTH 2 VALUE '=>'.

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
    assert_is_bound &1 c_error_incorect_input.
  END-OF-DEFINITION.

  DEFINE validate_mutable.
    IF &1->mutable EQ abap_false.
      throw( |constant { &2 } cannot be changed| ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE validate_number.
    validate &1.
    IF &1->type NE lcl_lisp=>type_number.
      throw( |{ &1->to_string( ) } is not a number | && &2 ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE validate_vector.
    validate &1.
    IF &1->type NE lcl_lisp=>type_vector.
      throw( |{ &1->to_string( ) } is not a vector | && &2 ).
    ENDIF.
  END-OF-DEFINITION.

  DEFINE validate_integer.
    IF &1->type NE lcl_lisp=>type_number OR frac( &1->number ) NE 0.
      throw( |{ &1->to_string( ) } is not an integer | && &2 ).
    ENDIF.
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

* Macro that implements the logic for the comparison native
* procedures, where only the comparison operator differs
  DEFINE _comparison.
    result = false.
    validate: list.
    validate_number list->car &2.
    DATA(cell) = list->cdr.
    DATA(carry) = list->car->number.  " CONV decfloat34( )
    WHILE cell NE nil.
      validate cell->car.
      validate_number list->car &2.
      IF carry &1 cell->car->number.
        RETURN.
      ENDIF.
      carry = cell->car->number.
      cell = cell->cdr.
    ENDWHILE.
    result = true.
  END-OF-DEFINITION.

  DEFINE _sign.
    result = false.
    validate: list.
    validate_number list->car &2.
    DATA(carry) = list->car->number.  " CONV decfloat34( )
    IF sign( carry ) NE &1.
      RETURN.
    ENDIF.
    result = true.
  END-OF-DEFINITION.

  DEFINE _is_type. " argument in list->car
    result = false.
    CHECK list IS BOUND AND list->car IS BOUND.
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
    result = nil.

    validate list.
    validate_number list->car &2.
    _is_last_param list.
    TRY.
          result = lcl_lisp_new=>number( &1( list->car->number ) ).
      _catch_arithmetic_error.
    ENDTRY.
  END-OF-DEFINITION.

  DEFINE _trigonometric.
    DATA carry TYPE f.

    result = nil.
    validate list.
    validate_number list->car &2.
    _is_last_param list.
    TRY.
    carry = list->car->number.
    result = lcl_lisp_new=>number( &1( carry ) ).
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
        mv_message = |Error in processing|.
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

  CLASS lcl_elem DEFINITION.
    PUBLIC SECTION.
      TYPES tv_type TYPE char1.
*      Type definitions for the various elements
      CONSTANTS:
        type_symbol TYPE tv_type VALUE 'S',
        type_number TYPE tv_type VALUE 'N',
        type_string TYPE tv_type VALUE '"'.
      CONSTANTS:
        type_boolean   TYPE tv_type VALUE 'B',
        type_null      TYPE tv_type VALUE '0',
        type_conscell  TYPE tv_type VALUE 'C',
        type_lambda    TYPE tv_type VALUE 'λ',
        type_native    TYPE tv_type VALUE 'P',
        type_primitive TYPE tv_type VALUE 'I',
        type_hash      TYPE tv_type VALUE 'H',
        type_vector    TYPE tv_type VALUE 'V'.
*      Types for ABAP integration:
      CONSTANTS:
        type_abap_data     TYPE tv_type VALUE 'D',
        type_abap_table    TYPE tv_type VALUE 'T',
        type_abap_function TYPE tv_type VALUE 'F',
        type_abap_class    TYPE tv_type VALUE 'c',
        type_abap_method   TYPE tv_type VALUE 'm'.

      DATA type TYPE char1.

      DATA value TYPE string.
      DATA number TYPE decfloat34.
      DATA data TYPE REF TO data.            " for ABAP integration
      DATA promise TYPE flag VALUE abap_false.
  ENDCLASS.

  CLASS lcl_lisp_iterator DEFINITION DEFERRED.
  CLASS lcl_lisp_new DEFINITION DEFERRED.
  CLASS lcl_lisp_interpreter DEFINITION DEFERRED.

* Single element that will capture cons cells, atoms etc.
*----------------------------------------------------------------------*
*       CLASS lcl_lisp DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_lisp DEFINITION INHERITING FROM lcl_elem FRIENDS lcl_lisp_new.
    PUBLIC SECTION.
*     Can this be replaced by a mesh? cf. DEMO_RND_PARSER_AST
      DATA mutable TYPE flag VALUE abap_true READ-ONLY.

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

      CLASS-DATA new_line   TYPE REF TO  lcl_lisp READ-ONLY.

*     Specifically for lambdas:
      DATA environment TYPE REF TO lcl_lisp_environment.
*     Format
      METHODS to_string RETURNING VALUE(str) TYPE string
                        RAISING   lcx_lisp_exception.
      METHODS write RETURNING VALUE(str) TYPE string
                    RAISING   lcx_lisp_exception.
*     Utilities
      METHODS rest RETURNING VALUE(ro_cdr) TYPE REF TO lcl_lisp.
      METHODS new_iterator RETURNING VALUE(ro_iter) TYPE REF TO lcl_lisp_iterator
                           RAISING   lcx_lisp_exception.

      METHODS is_equal IMPORTING io_elem       TYPE REF TO lcl_lisp
                                 comp          TYPE REF TO lcl_lisp DEFAULT nil
                                 interpreter   TYPE REF TO lcl_lisp_interpreter OPTIONAL
                       RETURNING VALUE(result) TYPE REF TO lcl_lisp
                       RAISING   lcx_lisp_exception.

      METHODS error_not_a_list IMPORTING context TYPE string DEFAULT space
                               RAISING   lcx_lisp_exception.
      METHODS error_not_a_pair IMPORTING context TYPE string DEFAULT space
                               RAISING   lcx_lisp_exception.

      CLASS-METHODS throw IMPORTING message TYPE string
                          RAISING   lcx_lisp_exception.
    PROTECTED SECTION.
      METHODS list_to_string RETURNING VALUE(str) TYPE string
                             RAISING   lcx_lisp_exception.
  ENDCLASS.                    "lcl_lisp DEFINITION

  INTERFACE lif_native.
    METHODS proc IMPORTING list          TYPE REF TO lcl_lisp
                 RETURNING VALUE(result) TYPE REF TO lcl_lisp
                 RAISING   lcx_lisp_exception.
  ENDINTERFACE.

  TYPES tt_lisp TYPE STANDARD TABLE OF REF TO lcl_lisp WITH EMPTY KEY.
  CLASS lcl_lisp_vector DEFINITION DEFERRED.
  CLASS lcl_lisp_abapfunction DEFINITION DEFERRED.
  CLASS lcl_lisp_hash DEFINITION DEFERRED.

  CLASS lcl_lisp_new DEFINITION.
    PUBLIC SECTION.
      CLASS-METHODS atom IMPORTING value          TYPE any
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS null RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS symbol IMPORTING value          TYPE any
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS boolean IMPORTING value          TYPE any
                            RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS number IMPORTING value          TYPE any
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS string IMPORTING value          TYPE any
                           RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS elem IMPORTING type           TYPE lcl_lisp=>tv_type
                                   value          TYPE any OPTIONAL
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS data IMPORTING ref            TYPE REF TO data OPTIONAL
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS table IMPORTING ref            TYPE REF TO data OPTIONAL
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
      CLASS-METHODS cons IMPORTING io_car         TYPE REF TO lcl_lisp DEFAULT lcl_lisp=>nil
                                   io_cdr         TYPE REF TO lcl_lisp DEFAULT lcl_lisp=>nil
                         RETURNING VALUE(ro_cons) TYPE REF TO lcl_lisp.

      CLASS-METHODS vector IMPORTING it_vector     TYPE tt_lisp
                                     iv_mutable    TYPE flag
                           RETURNING VALUE(ro_vec) TYPE REF TO lcl_lisp_vector.

      CLASS-METHODS lambda IMPORTING io_car           TYPE REF TO lcl_lisp
                                     io_cdr           TYPE REF TO lcl_lisp
                                     io_env           TYPE REF TO lcl_lisp_environment
                           RETURNING VALUE(ro_lambda) TYPE REF TO lcl_lisp.

      CLASS-METHODS function IMPORTING io_list        TYPE REF TO lcl_lisp
                             RETURNING VALUE(ro_func) TYPE REF TO lcl_lisp_abapfunction
                             RAISING   lcx_lisp_exception.

      CLASS-METHODS hash IMPORTING io_list        TYPE REF TO lcl_lisp
                         RETURNING VALUE(ro_hash) TYPE REF TO lcl_lisp_hash
                         RAISING   lcx_lisp_exception.

      CLASS-METHODS box_quote IMPORTING io_elem        TYPE REF TO lcl_lisp
                              RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS quote IMPORTING io_elem        TYPE REF TO lcl_lisp
                          RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS quasiquote IMPORTING io_elem        TYPE REF TO lcl_lisp
                               RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS unquote IMPORTING io_elem        TYPE REF TO lcl_lisp
                            RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

      CLASS-METHODS splice_unquote IMPORTING io_elem        TYPE REF TO lcl_lisp
                                   RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

    PRIVATE SECTION.
      CLASS-METHODS node IMPORTING type           TYPE lcl_lisp=>tv_type
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
  ENDCLASS.

  INTERFACE lif_port.
    METHODS write IMPORTING element         TYPE REF TO lcl_lisp
                  RETURNING VALUE(rv_input) TYPE string.
    METHODS read IMPORTING iv_input        TYPE string OPTIONAL
                 RETURNING VALUE(rv_input) TYPE string.
  ENDINTERFACE.

  CLASS lcl_console DEFINITION.
    PUBLIC SECTION.
      INTERFACES lif_port.
      ALIASES: read FOR lif_port~read,
               write FOR lif_port~write.
  ENDCLASS.                    "lcl_console DEFINITION

  CLASS lcl_console IMPLEMENTATION.

    METHOD write.
    ENDMETHOD.

    METHOD read.
      rv_input = iv_input.
    ENDMETHOD.

  ENDCLASS.                    "lcl_console DEFINITION

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
                                   io_fill          TYPE REF TO lcl_lisp DEFAULT lcl_lisp=>nil
                                   mutable          TYPE flag DEFAULT abap_true
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
  CLASS lcl_lisp_abapfunction DEFINITION INHERITING FROM lcl_lisp CREATE PROTECTED
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

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_environment DEFINITION
*----------------------------------------------------------------------*
  CLASS lcl_lisp_environment DEFINITION CREATE PRIVATE.
    PUBLIC SECTION.

*      Reference to outer (parent) environment:
      DATA outer TYPE REF TO lcl_lisp_environment.

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
*       Convenience method to add a value and create the cell
        define_value IMPORTING symbol         TYPE string
                               type           TYPE lcl_lisp=>tv_type
                               value          TYPE any OPTIONAL
                     RETURNING VALUE(element) TYPE REF TO lcl_lisp.

      METHODS parameters_to_symbols IMPORTING io_pars       TYPE REF TO lcl_lisp
                                              io_args       TYPE REF TO lcl_lisp
                                    RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment
                                    RAISING   lcx_lisp_exception.
    PROTECTED SECTION.

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
        c_close_paren TYPE char1 VALUE ')'.

      METHODS:
        constructor,
        parse IMPORTING code            TYPE clike
              RETURNING VALUE(elements) TYPE tt_element
              RAISING   lcx_lisp_exception.
    PRIVATE SECTION.
      CONSTANTS:
        c_escape_char    TYPE char1 VALUE '\',
        c_text_quote     TYPE char1 VALUE '"',
        c_lisp_quote     TYPE char1 VALUE '''', "LISP single quote = QUOTE
        c_lisp_backquote TYPE char1 VALUE '`',  " backquote = quasiquote
        c_lisp_unquote   TYPE char1 VALUE ',',
        c_lisp_splicing  TYPE char1 VALUE '@',
        c_lisp_hash      TYPE char1 VALUE '#',
        c_lisp_comment   TYPE char1 VALUE ';',
        c_open_bracket   TYPE char1 VALUE '[',
        c_close_bracket  TYPE char1 VALUE ']',
        c_peek_dummy     TYPE char1 VALUE 'Ü'.
      DATA code TYPE string.
      DATA length TYPE i.
      DATA index TYPE i.
      DATA char TYPE char1.

      DATA mv_eol TYPE char1.
      DATA mv_whitespace TYPE char04.
      DATA mv_delimiters TYPE char07.

      METHODS:
        next_char RAISING lcx_lisp_exception,
        peek_char RETURNING VALUE(rv_char) TYPE char1,
        skip_whitespace
          RETURNING VALUE(rv_has_next) TYPE flag
          RAISING   lcx_lisp_exception,
        parse_list IMPORTING delim         TYPE char01 DEFAULT c_open_paren
                   RETURNING VALUE(result) TYPE REF TO lcl_lisp
                   RAISING   lcx_lisp_exception,
        parse_token RETURNING VALUE(element) TYPE REF TO lcl_lisp
                    RAISING   lcx_lisp_exception.
      METHODS match_string CHANGING cv_val TYPE string.
      METHODS match_atom CHANGING cv_val TYPE string.
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

      METHODS constructor IMPORTING ii_port TYPE REF TO lif_port OPTIONAL.

*      Methods for evaluation
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
          RAISING   lcx_lisp_exception.

* Functions for dealing with lists:
      _proc_meth:
      proc_append,          ##called
      proc_append_unsafe,   ##called
      proc_reverse,         ##called
      proc_set_car,         ##called
      proc_set_cdr,         ##called
      proc_car,             ##called
      proc_cdr,             ##called
      proc_cons,            ##called

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

      proc_make_list,       ##called
      proc_list_tail,       ##called
      proc_list_ref,        ##called
      proc_list_to_vector,  ##called

      proc_length,   ##called
      proc_list,     ##called
      proc_nilp.     ##called

* Native functions:
      _proc_meth:
      proc_add,      ##called
      proc_subtract, ##called
      proc_multiply, ##called
      proc_divide,   ##called
      proc_gt,       ##called
      proc_gte,      ##called
      proc_lt,       ##called
      proc_lte,      ##called
      proc_eql,      ##called
      proc_eqv,      ##called
      proc_not,      ##called

      proc_is_number,       ##called
      proc_is_integer,      ##called
      proc_is_string,       ##called
      proc_is_symbol,       ##called
      proc_is_hash,         ##called
      proc_is_procedure,    ##called
      proc_is_list,         ##called
      proc_is_pair,         ##called
      proc_is_boolean,      ##called
      proc_list_is_boolean, ##called
      proc_is_vector,       ##called
      proc_is_alist,        ##called

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
* Formating
      proc_num_to_string, ##called
* Not in the spec: Just adding it anyway
      proc_random,       ##called
      proc_eq,           ##called
      proc_equal.        ##called

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
      METHODS assign_symbol
        IMPORTING element       TYPE REF TO lcl_lisp
                  environment   TYPE REF TO lcl_lisp_environment
        RETURNING VALUE(result) TYPE  REF TO lcl_lisp
        RAISING   lcx_lisp_exception.

      METHODS re_assign_symbol
        IMPORTING element       TYPE REF TO lcl_lisp
                  environment   TYPE REF TO lcl_lisp_environment
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

      DATA mi_port TYPE REF TO lif_port.

      METHODS write IMPORTING io_elem       TYPE REF TO lcl_lisp
                    RETURNING VALUE(result) TYPE REF TO lcl_lisp.

      METHODS display IMPORTING io_elem       TYPE REF TO lcl_lisp
                      RETURNING VALUE(result) TYPE REF TO lcl_lisp.

      METHODS read IMPORTING io_elem       TYPE REF TO lcl_lisp
                   RETURNING VALUE(result) TYPE REF TO lcl_lisp
                   RAISING   lcx_lisp_exception.

    PRIVATE SECTION.
      METHODS throw IMPORTING message TYPE string
                    RAISING   lcx_lisp_exception.


      METHODS proc_equivalence IMPORTING a             TYPE REF TO lcl_lisp
                                         b             TYPE REF TO lcl_lisp
                               RETURNING VALUE(result) TYPE REF TO lcl_lisp
                               RAISING   lcx_lisp_exception.

      METHODS create_element_from_data
        IMPORTING ir_data       TYPE REF TO data
        RETURNING VALUE(result) TYPE REF TO lcl_lisp.

      METHODS get_structure_field IMPORTING element           TYPE REF TO lcl_lisp
                                            VALUE(identifier) TYPE REF TO lcl_lisp
                                  RETURNING VALUE(rdata)      TYPE REF TO data
                                  RAISING   lcx_lisp_exception.
      METHODS get_table_row_with_key IMPORTING element           TYPE REF TO lcl_lisp
                                               VALUE(identifier) TYPE REF TO lcl_lisp
                                     RETURNING VALUE(rdata)      TYPE REF TO data
                                     RAISING   lcx_lisp_exception.
      METHODS get_index_table_row IMPORTING element           TYPE REF TO lcl_lisp
                                            VALUE(identifier) TYPE REF TO lcl_lisp
                                  RETURNING VALUE(rdata)      TYPE REF TO data
                                  RAISING   lcx_lisp_exception.

      METHODS evaluate_parameters IMPORTING io_list        TYPE REF TO lcl_lisp
                                            environment    TYPE REF TO lcl_lisp_environment
                                  RETURNING VALUE(ro_head) TYPE REF TO lcl_lisp
                                  RAISING   lcx_lisp_exception.

      METHODS evaluate_apply IMPORTING io_list       TYPE REF TO lcl_lisp
                                       environment   TYPE REF TO lcl_lisp_environment
                             RETURNING VALUE(result) TYPE REF TO lcl_lisp
                             RAISING   lcx_lisp_exception.

      METHODS evaluate_map IMPORTING io_list       TYPE REF TO lcl_lisp
                                     environment   TYPE REF TO lcl_lisp_environment
                           RETURNING VALUE(result) TYPE REF TO lcl_lisp
                           RAISING   lcx_lisp_exception.

      METHODS evaluate_for_each IMPORTING io_list       TYPE REF TO lcl_lisp
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
      METHODS init_letrec IMPORTING io_head       TYPE REF TO lcl_lisp
                                    io_env        TYPE REF TO lcl_lisp_environment
                          RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment
                          RAISING   lcx_lisp_exception.
      METHODS init_letrec_star IMPORTING io_head       TYPE REF TO lcl_lisp
                                         io_env        TYPE REF TO lcl_lisp_environment
                               RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment
                               RAISING   lcx_lisp_exception.

      METHODS init_let_star IMPORTING io_head       TYPE REF TO lcl_lisp
                                      io_env        TYPE REF TO lcl_lisp_environment
                            RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment
                            RAISING   lcx_lisp_exception.
      METHODS init_named_let IMPORTING io_env        TYPE REF TO lcl_lisp_environment
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

      METHODS reverse_list IMPORTING io_list       TYPE REF TO lcl_lisp
                           RETURNING VALUE(result) TYPE REF TO lcl_lisp
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
      mv_whitespace+3(1) = cl_abap_char_utilities=>horizontal_tab.
*     Delimiters value
      mv_delimiters = mv_whitespace.
      mv_delimiters+3(1) = c_close_paren.
      mv_delimiters+4(1) = c_open_paren.
      mv_delimiters+5(1) = c_close_bracket.
      mv_delimiters+6(1) = c_open_bracket.
    ENDMETHOD.                    "constructor

    METHOD skip_whitespace.
      WHILE char CA mv_whitespace AND index LT length.
        next_char( ).
      ENDWHILE.
      rv_has_next = xsdbool( index LT length ).
      CHECK char EQ c_lisp_comment AND rv_has_next EQ abap_true.
*      skip until end of line
      WHILE char CN mv_eol AND index LT length.
        next_char( ).
      ENDWHILE.
      rv_has_next = skip_whitespace( ).
    ENDMETHOD.                    "skip_whitespace

    METHOD next_char.
      index = index + 1.
      IF index < length.
        char = code+index(1).
      ELSEIF index = length.
        char = space.
      ELSEIF index > length.
        RAISE EXCEPTION TYPE lcx_lisp_exception
          EXPORTING
            message = c_error_unexpected_end
            area    = c_area_parse.
      ENDIF.
    ENDMETHOD.                    "next_char

    METHOD peek_char.
      DATA(lv_idx) = index + 1.

      IF lv_idx < length.
        rv_char = code+lv_idx(1).
      ELSE.
        rv_char = c_peek_dummy.
      ENDIF.
    ENDMETHOD.

    METHOD parse.
*     Entry point for parsing code. This is not thread-safe, but as an ABAP
*     process does not have the concept of threads, we are safe :-)
      me->code = code.
      length = strlen( code ).
      IF length = 0.
        APPEND lcl_lisp=>nil TO elements.
        RETURN.
      ENDIF.

      index = 0.
      char = code+index(1).           "Kick off things by reading first char
      WHILE skip_whitespace( ).
        IF char = c_open_paren OR char = c_open_bracket.
          APPEND parse_list( char ) TO elements.
        ELSEIF index < length.
          APPEND parse_token( ) TO elements.
        ENDIF.
      ENDWHILE.
    ENDMETHOD.                    "parse

    METHOD parse_list.
      DATA lo_cell TYPE REF TO lcl_lisp.
      DATA lv_empty_list TYPE boole_d VALUE abap_true.
      DATA lv_proper_list TYPE boole_d VALUE abap_true.

*     Set pointer to start of list
      lo_cell = result = lcl_lisp_new=>cons( ).
      DATA(lv_close_delim) = SWITCH #( delim WHEN c_open_bracket THEN c_close_bracket
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

          WHEN c_close_paren OR c_close_bracket.
            lcl_lisp=>throw( `parse error - no matching closing parens/bracket` ).

          WHEN OTHERS.
        ENDCASE.

        IF lv_proper_list EQ abap_false.
*         inconsistent input
          lcl_lisp=>throw( `parse error - dotted pair` ).
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
            "           match closing parens
            lv_proper_list = abap_false.
          ELSE.

            lo_cell = lo_cell->cdr = lcl_lisp_new=>cons( io_car = lo_peek ).

          ENDIF.

        ENDIF.

      ENDWHILE.
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
        WHEN c_open_paren OR c_open_bracket.
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
          CASE peek_char( ).
            WHEN c_lisp_splicing.  " unquote-splicing
              next_char( ).        " Skip past ,@
              next_char( ).
              element = lcl_lisp_new=>splice_unquote( parse_list( ) ).

            WHEN OTHERS.           " unquote,  TO DO
              next_char( ).        " Skip past single quote
              element = lcl_lisp_new=>unquote( parse_token( ) ).

          ENDCASE.

        WHEN c_text_quote.
          match_string( CHANGING cv_val = sval ).
          element = lcl_lisp_new=>string( sval ).
          RETURN.

        WHEN c_lisp_hash.
          CASE peek_char( ).
            WHEN c_open_paren.   " Vector constant
              next_char( ).
              element = lcl_lisp_vector=>from_list( io_list = parse_list( )
                                                    iv_mutable = abap_false ).
              RETURN.
            WHEN OTHERS.
              " Others
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

    METHOD constructor.
      super->constructor( ).
      mi_port = COND #( WHEN ii_port IS BOUND THEN ii_port ELSE NEW lcl_console( ) ).

      env = lcl_lisp_environment=>new( ).

*     Create symbols for nil, true and false values
      nil = lcl_lisp=>nil.
      true = lcl_lisp=>true.
      false = lcl_lisp=>false.
      env->set( symbol = 'nil' element = nil ).
      env->set( symbol = '#f' element = false ).
      env->set( symbol = '#t' element = true ).

*     Add primitive functions to environment
      env->define_value( symbol = 'define'     type = lcl_lisp=>type_primitive value   = 'define' ).
      env->define_value( symbol = 'lambda'     type = lcl_lisp=>type_primitive value   = 'lambda' ).
      env->define_value( symbol = 'if'         type = lcl_lisp=>type_primitive value   = 'if' ).
      env->define_value( symbol = 'quote'      type = lcl_lisp=>type_primitive value   = 'quote' ).
      env->define_value( symbol = 'quasiquote' type = lcl_lisp=>type_primitive value   = 'quasiquote' ).
      env->define_value( symbol = 'set!'       type = lcl_lisp=>type_primitive value   = 'set!' ).
      env->define_value( symbol = 'apply'      type = lcl_lisp=>type_primitive value   = 'apply' ).

      env->define_value( symbol = 'and'      type = lcl_lisp=>type_primitive value   = 'and' ).
      env->define_value( symbol = 'or'       type = lcl_lisp=>type_primitive value   = 'or' ).
      env->define_value( symbol = 'cond'     type = lcl_lisp=>type_primitive value   = 'cond' ).
      env->define_value( symbol = 'unless'   type = lcl_lisp=>type_primitive value   = 'unless' ).
      env->define_value( symbol = 'when'     type = lcl_lisp=>type_primitive value   = 'when' ).
      env->define_value( symbol = 'begin'    type = lcl_lisp=>type_primitive value   = 'begin' ).
      env->define_value( symbol = 'let'      type = lcl_lisp=>type_primitive value   = 'let' ).
      env->define_value( symbol = 'let*'     type = lcl_lisp=>type_primitive value   = 'let*' ).
      env->define_value( symbol = 'letrec'   type = lcl_lisp=>type_primitive value   = 'letrec' ).
      env->define_value( symbol = 'letrec*'  type = lcl_lisp=>type_primitive value   = 'letrec*' ).
      env->define_value( symbol = 'do'       type = lcl_lisp=>type_primitive value   = 'do' ).
      env->define_value( symbol = 'case'     type = lcl_lisp=>type_primitive value   = 'case' ).
      env->define_value( symbol = 'for-each' type = lcl_lisp=>type_primitive value   = 'for-each' ).
      env->define_value( symbol = 'map'      type = lcl_lisp=>type_primitive value   = 'map' ).

      env->define_value( symbol = 'unquote'          type = lcl_lisp=>type_primitive value   = 'unquote' ).
      env->define_value( symbol = 'unquote-splicing' type = lcl_lisp=>type_primitive value   = 'unquote-splicing' ).
      env->define_value( symbol = 'newline'          type = lcl_lisp=>type_primitive value   = 'newline' ).
      env->define_value( symbol = 'display'          type = lcl_lisp=>type_primitive value   = 'display' ).
      env->define_value( symbol = 'write'            type = lcl_lisp=>type_primitive value   = 'write' ).
      env->define_value( symbol = 'read'             type = lcl_lisp=>type_primitive value   = 'read' ).

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
      env->define_value( symbol = 'list->vector' type = lcl_lisp=>type_native value   = 'PROC_LIST_TO_VECTOR' ).

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
      env->define_value( symbol = 'hash?'       type = lcl_lisp=>type_native value = 'PROC_IS_HASH' ).
      env->define_value( symbol = 'number?'     type = lcl_lisp=>type_native value = 'PROC_IS_NUMBER' ).
      env->define_value( symbol = 'integer?'    type = lcl_lisp=>type_native value = 'PROC_IS_INTEGER' ).
      env->define_value( symbol = 'list?'       type = lcl_lisp=>type_native value = 'PROC_IS_LIST' ).
      env->define_value( symbol = 'pair?'       type = lcl_lisp=>type_native value = 'PROC_IS_PAIR' ).
      env->define_value( symbol = 'vector?'     type = lcl_lisp=>type_native value = 'PROC_IS_VECTOR' ).
      env->define_value( symbol = 'boolean?'    type = lcl_lisp=>type_native value = 'PROC_IS_BOOLEAN' ).
      env->define_value( symbol = 'alist?'      type = lcl_lisp=>type_native value = 'PROC_IS_ALIST' ).
      env->define_value( symbol = 'procedure?'  type = lcl_lisp=>type_native value = 'PROC_IS_PROCEDURE' ).
      env->define_value( symbol = 'symbol?'     type = lcl_lisp=>type_native value = 'PROC_IS_SYMBOL' ).
      env->define_value( symbol = 'boolean=?'   type = lcl_lisp=>type_native value = 'PROC_LIST_IS_BOOLEAN' ).

*     Format
      env->define_value( symbol = 'number->string' type = lcl_lisp=>type_native value = 'PROC_NUM_TO_STRING' ).
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

      env->define_value( symbol = 'zero?'     type = lcl_lisp=>type_native value = 'PROC_IS_ZERO' ).
      env->define_value( symbol = 'positive?' type = lcl_lisp=>type_native value = 'PROC_IS_POSITIVE' ).
      env->define_value( symbol = 'negative?' type = lcl_lisp=>type_native value = 'PROC_IS_NEGATIVE' ).
      env->define_value( symbol = 'odd?'      type = lcl_lisp=>type_native value = 'PROC_IS_ODD' ).
      env->define_value( symbol = 'even?'     type = lcl_lisp=>type_native value = 'PROC_IS_EVEN' ).

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

      DATA lr_ref TYPE REF TO data.
*     Define a value in the environment for SYST
      GET REFERENCE OF syst INTO lr_ref.
      env->set( symbol = 'ab-sy' element = lcl_lisp_new=>data( lr_ref ) ).
    ENDMETHOD.                    "constructor

    METHOD throw.
      RAISE EXCEPTION TYPE lcx_lisp_exception
        EXPORTING
          message = message
          area    = c_area_eval.
    ENDMETHOD.                    "throw

    METHOD assign_symbol.
*     Scheme does not return a value for define; but we are returning the new symbol reference
      DATA(lo_head) = element->car.
      CASE lo_head->type.
        WHEN lcl_lisp=>type_symbol.
*         call the set method of the current environment using the unevaluated first parameter
*         (second list element) as the symbol key and the evaluated second parameter as the value.
          environment->set( symbol  = lo_head->value
                            element = eval( element = element->cdr->car
                                            environment = environment ) ).
          result = lcl_lisp_new=>symbol( lo_head->value ).

*       Function shorthand (define (id arg ... ) body ...+)
        WHEN lcl_lisp=>type_conscell.
*         define's function shorthand allows us to define a function by specifying a list as the
*         first argument where the first element is a symbol and consecutive elements are arguments
          result = lcl_lisp_new=>lambda( io_car = lo_head->cdr  "List of params following function symbol
                                         io_cdr = element->cdr
                                         io_env = environment ).
*         Add function to the environment with symbol
          environment->set( symbol  = lo_head->car->value
                            element = result ).

          result = lcl_lisp_new=>symbol( lo_head->car->value ).
        WHEN OTHERS.
          throw( |{ lo_head->to_string( ) } cannot be a variable identifier| ).
      ENDCASE.
    ENDMETHOD.                    "assign_symbol

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
      DATA lo_arg TYPE REF TO lcl_lisp.
*     Before execution of the procedure or lambda, all parameters must be evaluated
      validate io_list.
      ro_head = nil.
      CHECK io_list NE nil AND io_list->car NE nil.

      DATA(elem) = io_list.
      WHILE elem IS BOUND AND elem->type EQ lcl_lisp=>type_conscell.

        DATA(lo_next) = lcl_lisp_new=>cons( io_car = eval_ast( element = elem->car
                                                               environment = environment ) ).
        elem = elem->cdr.

        IF ro_head EQ nil.
          lo_arg = ro_head = lo_next.
        ELSE.
          lo_arg = lo_arg->cdr = lo_next.
        ENDIF.

      ENDWHILE.

      validate_tail elem io_list ``.

    ENDMETHOD.                    "evaluate_parameters

    METHOD evaluate_apply.
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
      WHILE lo_arg->cdr->type EQ lcl_lisp=>type_conscell.
*       At least two entries (argn and rest), build (list arg1 . . argn )

        lo_new = lo_new->cdr = lcl_lisp_new=>cons( io_car = lo_arg->car ).
        lo_arg = lo_arg->cdr.
      ENDWHILE.

*     now (append (list arg1 . . argn ) rest )
      DATA(lo_rest) = eval_ast( element = lo_arg->car
                                environment = environment ).

      WHILE lo_rest->type EQ lcl_lisp=>type_conscell.  " e.g. NE nil
        lo_new = lo_new->cdr = lcl_lisp_new=>box_quote( lo_rest->car ).
        lo_rest = lo_rest->cdr.
      ENDWHILE.

      lo_new->cdr = lo_rest.

    ENDMETHOD.

*   (define (map f lst)
*     (if (null? lst)
*       '()
*       (cons (f (car lst)) (map f (cdr lst)))))
    METHOD evaluate_map.
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

        DATA(lo_head) = lcl_lisp_new=>cons( io_car = eval(
                              element = map_next_expr( EXPORTING io_proc = lo_proc
                                                       IMPORTING ev_has_next = lv_has_next
                                                       CHANGING  ct_list = lt_list )
                              environment = environment ) ).
        IF result EQ nil.
          " create function call (proc list1-[1] list2-[1]... listn-[1])
          " evaluate, add result as 1st list element of new list
          lo_map = result = lo_head.
        ELSE.
          " evaluate function call (proc list1[k] list2[k]... listn[k]); add result as k-th list element
          lo_map = lo_map->cdr = lo_head.
        ENDIF.
      ENDWHILE.

    ENDMETHOD.

    METHOD evaluate_for_each.
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
        " evaluate function call (proc list1[k] list2[k]... listn[k])
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
      DATA lt_symbols TYPE SORTED TABLE OF string WITH UNIQUE DEFAULT KEY. " to check for duplicate variables

      eo_env = lcl_lisp_environment=>new( io_env ).
      DATA(local_env) = lcl_lisp_environment=>new( ).
      eo_step = nil.


      DATA(lo_loop) = io_head.
      WHILE lo_loop NE nil.
        DATA(lo_spec) = lo_loop->car.
*       max. 3 entries
*       <variable>
        DATA(lo_var) = lo_spec->car.

*       <init>

        lo_spec = lo_spec->cdr.
        IF lo_spec NE nil.
          DATA(lo_init) = lo_spec->car.

*         It is an error for a <variable> to appear more than once in the list of do variables.
          IF line_exists( lt_symbols[ table_line = lo_var->value ] ).
            throw( |do: variable { lo_var->value } appears more than once| ).
          ENDIF.
          INSERT lo_var->value INTO TABLE lt_symbols.


          local_env->set( symbol = lo_var->value
                          element = eval_ast( element = lo_init    " inits are evaluated in org. environment
                                              environment = io_env ) ).
          lo_spec = lo_spec->cdr.
          IF lo_spec NE nil.
*           <step>
            DATA(lo_step) = lcl_lisp_new=>cons( io_car = lo_var
                                                io_cdr = lo_spec->car ).
            IF eo_step EQ nil.  " first
              eo_step = lcl_lisp_new=>cons( io_car = lo_step ).
              DATA(lo_ptr) = eo_step.
            ELSE.
              lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = lo_step ).
            ENDIF.

          ENDIF.
        ENDIF.
*       Next iteration control
        lo_loop = lo_loop->cdr.
      ENDWHILE.

      LOOP AT lt_symbols INTO DATA(lv_symbol).
        io_env->set( symbol = lv_symbol
                     element = local_env->get( lv_symbol ) ).
      ENDLOOP.

    ENDMETHOD.

    METHOD eval_do_step.
*     <command> expressions are evaluated in order for effect
      DATA(lo_command) = io_command.

*     Evaluate in order
      WHILE lo_command NE nil.
        eval( element = lo_command->car
              environment = io_env ).
        lo_command = lo_command->cdr.
      ENDWHILE.

      DATA(lo_local_env) = lcl_lisp_environment=>new( ).
*     the <step> expressions are evaluated in some unspecified order
      DATA(lo_step) = io_steps.
      WHILE lo_step NE nil.
        DATA(lo_ptr) = lo_step->car.

*       the <variable>s are first bound to fresh locations
*       to avoid dependencies in the next step

*       the results of the <step>s are stored in the bindings of the <variable>s
        lo_local_env->set( symbol = lo_ptr->car->value
                           element = eval_ast( element = lo_ptr->cdr
                                                environment = io_env ) ).
        lo_step = lo_step->cdr.
      ENDWHILE.

      lo_step = io_steps.
      WHILE lo_step NE nil.
        lo_ptr = lo_step->car.

*       the results of the <step>s are stored in the bindings of the <variable>s
        io_env->set( symbol = lo_ptr->car->value
                     element = lo_local_env->get( lo_ptr->car->value ) ).
        lo_step = lo_step->cdr.
      ENDWHILE.

    ENDMETHOD.

    METHOD eval_list_tco. " Tail Call Optimization
*     Evaluate all expressions except the last one to be evaluated as a tail call
*     ( eval LOOP for the last evaluation step )
      validate io_head.
      result = nil.
      CHECK io_head NE nil.

      eo_elem = io_head.
      WHILE eo_elem IS BOUND AND eo_elem->type EQ lcl_lisp=>type_conscell
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
*      WHILE elem IS BOUND AND elem->type EQ lcl_lisp=>type_conscell.
*        result = eval_ast( element = elem->car
*                           environment = io_environment ).
*        elem = elem->cdr.
*      ENDWHILE.
*
*      validate_tail elem io_head space.
*    ENDMETHOD.

    METHOD lambda_environment.
*     The function (LAMBDA) receives its own local environment in which to execute,
*     where parameters become symbols that are mapped to the corresponding arguments
      validate io_head.
      ro_env = lcl_lisp_environment=>new( io_head->environment ).

      ro_env->parameters_to_symbols( io_args = evaluate_parameters( io_list = io_args           " Pointer to arguments
                                                                    environment = environment )
                                     io_pars = io_head->car ).   " Pointer to formal parameters
    ENDMETHOD.

    METHOD extract_arguments.
      eo_args = eo_pars = nil.                "list of parameters

      CHECK io_head IS BOUND AND io_head->car IS BOUND AND io_head->car NE nil.
      DATA(lo_ptr) = io_head->car.

      validate lo_ptr->car.
      eo_pars = lcl_lisp_new=>cons( io_car = lo_ptr->car ).
      IF lo_ptr->cdr IS BOUND AND lo_ptr->cdr NE nil.
        eo_args = lcl_lisp_new=>cons( io_car = lo_ptr->cdr->car ).
      ENDIF.
      DATA(lo_par) = eo_pars.
      DATA(lo_arg) = eo_args.
      lo_ptr = io_head.
      WHILE lo_ptr->cdr IS BOUND AND lo_ptr->cdr NE nil.
        lo_ptr = lo_ptr->cdr.
*        Rest of list, pick head
        DATA(lo_pair) = lo_ptr->car.
        IF lo_pair IS BOUND AND lo_pair->car NE nil.
          lo_par = lo_par->cdr = lcl_lisp_new=>cons( io_car = lo_pair->car ).
        ENDIF.
        IF lo_pair->cdr IS BOUND AND lo_pair->cdr NE nil.
          lo_arg = lo_arg->cdr = lcl_lisp_new=>cons( io_car = lo_pair->cdr->car ).
        ENDIF.
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
* letrec lets us create an environment before evaluating the initial value expressions, so that the
* initial value computions execute inside the new environment.
*
*(define (some-procedure...)
*   (letrec ((helper (lambda (x)
*                       ...
*                       (if some-test?
*                           (helper ...))))) ; recursive call
*     ...
*     (helper ...)  ; call to recursive local procedure
*     ...))
* Note the procedure helper can "see its own name," since the lambda expression is evaluated in the
* environment where helper is bound. The above example is equivalent to:
*
*(define (some-procedure ...)
*   (let ((helper '*dummy-value*))
*      (set! helper (lambda (x)
*                      ...
*                      (if some-test?
*                          (helper ...))))) ; recursive call
*     ...
*     (helper ...)  ; call to recursive local procedure
*     ...))
    METHOD  init_letrec.
      ro_env = lcl_lisp_environment=>new( io_env ).

      extract_arguments( EXPORTING io_head = io_head
                         IMPORTING eo_pars = DATA(lo_pars)
                                   eo_args = DATA(lo_args) ).
      DATA(lo_par) = lo_pars.
      DATA(lo_arg) = lo_args.
      WHILE lo_par IS BOUND AND lo_par NE nil   " Nil means no parameters to map
        AND lo_arg IS BOUND AND lo_arg NE nil.  " Nil means no parameters to map

        ro_env->set( symbol = lo_par->car->value
                     element = lo_arg->car ).
        lo_par = lo_par->cdr.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

      DATA(lo_new_args) = evaluate_parameters( io_list = lo_args           " Pointer to arguments
                                               environment = ro_env ).
      ro_env->parameters_to_symbols( io_args = lo_new_args
                                     io_pars = lo_pars ).   " Pointer to formal parameters
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
    METHOD init_named_let.
      CASE co_head->car->type.
        WHEN lcl_lisp=>type_symbol.
*named let:  (let <variable> (bindings) <body>)
          DATA(lo_var) = co_head->car.
          co_head = co_head->cdr.

        WHEN OTHERS. " lcl_lisp=>type_conscell.
*(let ((x 10) (y 5)) (+ x y)) is syntactic sugar for  ( (lambda (x y) (+ x y)) 10 5)
          lo_var = nil.
      ENDCASE.

      ro_env = lcl_lisp_environment=>new( io_env ).
      extract_arguments( EXPORTING io_head = co_head->car
                         IMPORTING eo_pars = DATA(lo_pars)
                                   eo_args = DATA(lo_args) ).

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

    METHOD init_let_star.
      ro_env = lcl_lisp_environment=>new( io_env ).
      extract_arguments( EXPORTING io_head = io_head
                         IMPORTING eo_pars = DATA(lo_pars)
                                   eo_args = DATA(lo_args) ).
      evaluate_in_sequence( io_args = lo_args      " Pointer to arguments e.g. (4, (+ x 4)
                            io_pars = lo_pars      " Pointer to formal parameters (x y)
                            io_env = ro_env ).
    ENDMETHOD.

    METHOD  init_letrec_star.
      ro_env = lcl_lisp_environment=>new( io_env ).

      extract_arguments( EXPORTING io_head = io_head
                         IMPORTING eo_pars = DATA(lo_pars)
                                   eo_args = DATA(lo_args) ).
      DATA(lo_par) = lo_pars.
      DATA(lo_arg) = lo_args.
      WHILE lo_par IS BOUND AND lo_par NE nil   " Nil means no parameters to map
        AND lo_arg IS BOUND AND lo_arg NE nil.  " Nil means no parameters to map

        ro_env->set( symbol = lo_par->car->value
                     element = lo_arg->car ).
        lo_par = lo_par->cdr.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

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

        WHEN lcl_lisp=>type_hash. " TEST
          result = lcl_lisp_new=>hash( eval( element = CAST lcl_lisp_hash( element )->get( element )
                                             environment = environment ) ).

        WHEN lcl_lisp=>type_conscell. " List
          result = eval( element = element
                         environment = environment ).

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
      ENDIF.
    END-OF-DEFINITION.

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
*           Evaluate element
            CASE lo_elem->type.
              WHEN lcl_lisp=>type_conscell. " List
*               return a new list that is the result of calling EVAL on each of the members of the list

*               To evaluate list, we must first evaluate head value
*               Evaluate first element of list to determine if it is a native procedure or lambda
                DATA(lr_head) = lo_elem->car.
                DATA(lr_tail) = lo_elem->cdr.

                CASE lr_head->value.

                  WHEN 'unquote'. " evaluate the argument to unquote
                    IF lr_tail->cdr NE nil.
                      throw( |unquote can only take a single argument| ).
                    ENDIF.
                    lo_elem = lr_tail->car.
                    tail_expression lo_elem.

                  WHEN 'quote'. " Return the argument to quote unevaluated
                    IF lr_tail->cdr NE nil.
                      throw( |quote can only take a single argument| ).
                    ENDIF.
                    result = lr_tail->car.

                  WHEN 'quasiquote'. " Partial quote - TO DO
                    IF lr_tail->cdr NE nil.
                      throw( |quasiquote can only take a single argument| ).
                    ENDIF.
                    result = lr_tail->car.

                  WHEN 'unquote-slicing'.
                    result = nil.
                    throw( `not implemented yet` ).

                  WHEN 'newline'.
                    result = write( lcl_lisp=>new_line ).

                  WHEN 'display'.
                    result = display( eval( element = lr_tail->car
                                            environment = lo_env ) ).

                  WHEN 'write'.
                    result = write( eval( element = lr_tail->car
                                          environment = lo_env )  ).

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
                    WHILE lo_ptr NE nil.
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
                    lo_env = init_named_let( EXPORTING io_env = lo_env
                                             CHANGING co_head = lr_tail ).
                    lo_elem = lr_tail->cdr.
                    tail_sequence.

                  WHEN 'let*'.
                    lo_env = init_let_star( io_head = lr_tail->car
                                            io_env = lo_env ).
                    lo_elem = lr_tail->cdr.
                    tail_sequence.

                  WHEN 'letrec'.
*                   (letrec ((a 5) (b (+ a 3)) b)
                    lo_env = init_letrec( io_head = lr_tail->car
                                          io_env = lo_env ).
                    lo_elem = lr_tail->cdr.
                    tail_sequence.

                  WHEN 'letrec*'.
                    lo_env = init_letrec_star( io_head = lr_tail->car
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

                    tail_sequence.

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
                    validate: lr_tail, lr_tail->car.

                    lo_elem = nil.
                    DATA(lv_match) = abap_false.
                    WHILE lr_tail NE nil AND lv_match EQ abap_false.
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

                        IF proc_equivalence( a = lo_key            " eqv? match
                                             b = lo_datum->car ) NE false.
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

                  WHEN 'read'.
                    result = read( lr_tail ).


                  WHEN 'for-each'.
                    result = evaluate_for_each( io_list = lr_tail
                                                environment = lo_env ).

                  WHEN 'map'.
                    result = evaluate_map( io_list = lr_tail
                                           environment = lo_env ).

                  WHEN 'apply'.
                    " (apply proc arg1 ... argn rest-args)
                    lo_elem = lcl_lisp_new=>cons( io_car = evaluate_apply( io_list = lr_tail
                                                                           environment = lo_env ) ).
                    tail_expression lo_elem.

                  WHEN OTHERS.

*                   EXECUTE PROCEDURE (native or lambda)
*                   Take the first item of the evaluated list and call it as function
*                   using the rest of the evaluated list as its arguments.

*                   The evaluated head must be a native procedure or a lambda or an ABAP function module
                    DATA(lo_proc) = eval_ast( element = lr_head             " proc
                                              environment = lo_env ).
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

                      WHEN lcl_lisp=>type_primitive.
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
        RETURN.

      ENDDO.
    ENDMETHOD.

    METHOD write.
      mi_port->write( io_elem ).
      result = io_elem.
    ENDMETHOD.

    METHOD display.
      mi_port->write( io_elem ).
      result = io_elem.
    ENDMETHOD.

    METHOD read.
      DATA lo_conscell TYPE REF TO lcl_lisp. " Lisp-side  (target)

*      Create list with cell for each row AND Set pointer to start of list
      result = nil.
      IF io_elem IS NOT INITIAL AND io_elem->car IS NOT INITIAL.
        DATA(lv_input) = io_elem->car->value.
      ENDIF.
      LOOP AT parse( mi_port->read( lv_input ) ) INTO DATA(lo_elem).
        IF sy-tabix EQ 1.
          lo_conscell = result = lo_elem.
        ELSE.
          lo_conscell = lo_conscell->cdr = lcl_lisp_new=>cons( io_car = lo_elem ).
        ENDIF.
      ENDLOOP.
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
        DATA(lv_value) = eval( element = lo_element
                               environment = env )->write( ).
        response = COND #( WHEN response IS INITIAL
                           THEN lv_value
                           ELSE |{ response } { lv_value }| ).
      ENDLOOP.
    ENDMETHOD.                    "eval_source

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

        IF first->type = lcl_lisp=>type_conscell.
          result = lcl_lisp_new=>cons( io_car = first->car ).

          DATA(lo_last) = result.
          DATA(lo_arg) = first->cdr.
          WHILE lo_arg NE nil AND lo_arg->type = lcl_lisp=>type_conscell.
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

*       Append lo_arg to result, from last element on
        WHILE lo_arg NE nil AND lo_arg->type = lcl_lisp=>type_conscell.
          lo_last = lo_last->cdr = lcl_lisp_new=>cons( io_car = lo_arg->car ).
          lo_arg = lo_arg->cdr.
        ENDWHILE.

        CHECK lo_arg NE nil.
        lo_last = lo_last->cdr = lo_arg.

      ENDWHILE.

    ENDMETHOD.

    METHOD reverse_list.
      validate io_list.

      result = nil.
      DATA(iter) = io_list->new_iterator( ).
      WHILE iter->has_next( ).
        result = lcl_lisp_new=>cons( io_car = iter->next( )
                                     io_cdr = result ).
      ENDWHILE.
    ENDMETHOD.

    METHOD proc_reverse.
      validate list.

      result = reverse_list( list->car ).
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
          DATA(lo_prev) = lcl_lisp_new=>cons( io_car = lcl_lisp=>quote
                                              io_cdr = lcl_lisp_new=>cons( io_car = <lo_list>->car ) ).

          lo_next = lo_next->cdr = lcl_lisp_new=>cons( io_car = lo_prev ).
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
        IF lo_last->type NE lcl_lisp=>type_conscell.
          lo_last->error_not_a_list( ).
        ENDIF.

        WHILE lo_last->cdr IS BOUND AND lo_last->cdr NE nil.
          lo_last = lo_last->cdr.
        ENDWHILE.

        "TO DO - replace with validate_tail lo_last (?) list->car.
        IF lo_last->type NE lcl_lisp=>type_conscell.
*         If the last item is not a cons cell, return an error
          error_no_list list->car  `append!`.
        ENDIF.

*       Last item is a cons cell; tack on the new value
        lo_last->cdr = list->cdr->car.
        result = list->car.
      ENDIF.
    ENDMETHOD.                    "proc_append_unsafe

    METHOD proc_car.
      validate list.

      DATA(lo_arg) = list->car.
      IF lo_arg EQ nil OR lo_arg->type NE lcl_lisp=>type_conscell.
        lo_arg->error_not_a_pair( `car: ` ).
      ENDIF.
      result = lo_arg->car.
    ENDMETHOD.                    "proc_car

    METHOD proc_set_car.
      validate: list, list->cdr.

      DATA(lo_arg) = list->car.
      validate_mutable: lo_arg  `list`.

      IF lo_arg EQ nil OR lo_arg->type NE lcl_lisp=>type_conscell.
        lo_arg->error_not_a_pair( `set-car!: ` ).
      ENDIF.

      lo_arg->car = list->cdr->car.
      result = nil.
    ENDMETHOD.                    "proc_car

    METHOD proc_set_cdr.
      validate: list, list->cdr.

      DATA(lo_arg) = list->car.
      validate_mutable: lo_arg `list`.

      IF lo_arg EQ nil OR lo_arg->type NE lcl_lisp=>type_conscell.
        lo_arg->error_not_a_pair( `set-cdr!: ` ).
      ENDIF.

      lo_arg->cdr = list->cdr->car.
      result = nil.
    ENDMETHOD.

    METHOD proc_cdr.
      validate list.

      DATA(lo_arg) = list->car.
      IF lo_arg EQ nil OR lo_arg->type NE lcl_lisp=>type_conscell.
        lo_arg->error_not_a_pair( `cdr: ` ).
      ENDIF.
      result = lo_arg->cdr.
    ENDMETHOD.                    "proc_cdr

    METHOD proc_cons.
*     Create new cell and prepend it to second parameter
      validate: list, list->car, list->cdr.

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
      IF lo_arg EQ nil OR lo_arg->type NE lcl_lisp=>type_conscell.
        lo_arg->error_not_a_pair( `caar: ` ).
      ENDIF.

      lo_arg = lo_arg->car.
      IF lo_arg EQ nil OR lo_arg->type NE lcl_lisp=>type_conscell.
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
      IF lo_arg EQ nil OR lo_arg->type NE lcl_lisp=>type_conscell.
        lo_arg->error_not_a_pair( `cadr: ` ).
      ENDIF.

      lo_arg = lo_arg->cdr.
      IF lo_arg EQ nil OR lo_arg->type NE lcl_lisp=>type_conscell.
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
      IF lo_arg EQ nil OR lo_arg->type NE lcl_lisp=>type_conscell.
        lo_arg->error_not_a_pair( `cdar: ` ).
      ENDIF.

      lo_arg = lo_arg->car.
      IF lo_arg EQ nil OR lo_arg->type NE lcl_lisp=>type_conscell.
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
      IF lo_arg EQ nil OR lo_arg->type NE lcl_lisp=>type_conscell.
        lo_arg->error_not_a_pair( `cddr: ` ).
      ENDIF.

      lo_arg = lo_arg->cdr.
      IF lo_arg EQ nil OR lo_arg->type NE lcl_lisp=>type_conscell.
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
    METHOD proc_length.
      validate: list, list->cdr.
      IF list->cdr NE nil.
        throw( |length takes only one argument| ).
      ENDIF.

      result = lcl_lisp_new=>number( 0 ).
      CHECK list NE nil AND ( list->car NE nil OR list->cdr NE nil ).

*      Iterate over list to count the number of items
      result->number = 1.
      DATA(lo_elem) = list->car.
      WHILE lo_elem->cdr IS BOUND AND lo_elem->cdr NE nil.
        lo_elem = lo_elem->cdr.
        ADD 1 TO result->number.
      ENDWHILE.
      CHECK lo_elem->type NE lcl_lisp=>type_conscell
        AND list->car->type NE lcl_lisp=>type_conscell.
*      If the last item is not a cons cell, return an error
      error_no_list list->car `length`.
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
*     returns a list of length n and every atom is an empty list ().
      validate list.
      validate_number list->car 'make-list'.

      result = lcl_lisp=>nil.
      CHECK list->car->number GT 0.

      IF list->cdr EQ nil.
        DATA(lo_default) = nil.
      ELSE.
        lo_default = list->cdr->car.
      ENDIF.

      result = lcl_lisp_new=>cons( io_car = lo_default ).  " first
      DATA(lo_ptr) = result.

      DO list->car->number - 1 TIMES.
        lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = lo_default ).
      ENDDO.
    ENDMETHOD.

*(define list-tail
*  (lambda (x k)
*    (if (zero? k)
*        x
*        (list-tail (cdr x) (- k 1)))))
    METHOD proc_list_tail.
*     (list-tail list k) procedure
*     List should be a list of size at least k.  The list-tail procedure returns
*     the subchain of list obtained by omitting the first k elements
*     (list-tail '(a b c d) 2)  => (c d)
**Implementation responsibilities:
*     The implementation must check that list is a chain of pairs whose length is at least k.
*     It should not check that it is a list of pairs beyond this length.

      validate: list, list->cdr.
      validate_number list->cdr->car 'list-tail'.

      result = list->car.
      DATA(index) = CONV i( list->cdr->car->number ).
      DO index TIMES.
        IF result->cdr EQ nil.
          throw( `list-tail: list too short` ).
        ENDIF.
        result = result->cdr.
        CHECK result IS NOT BOUND.
        throw( |list-tail: index { index } reaches a non-pair| ).
      ENDDO.
    ENDMETHOD.

*(car list-tail list k)
    METHOD proc_list_ref.
*    (list-ref list k) procedure
*    List must be a list whose length is at least k + 1.  The list-ref procedure returns the kth element of list.
*    (list-ref '(a b c d) 2) => c
*
* Implementation responsibilities:
*    The implementation must check that list is a chain of pairs whose length is at least k + 1.
*    It should not check that it is a list of pairs beyond this length.

      validate: list, list->cdr.
      validate_number list->cdr->car 'list-ref'.

      result = list->car.
      DATA(index) = CONV i( list->cdr->car->number ).
      DO index TIMES.
        IF result->cdr EQ nil.
          throw( `list-ref: list too short` ).
        ENDIF.
        result = result->cdr.
        CHECK result IS NOT BOUND.
        throw( |list-ref: index { index } reaches a non-pair| ).
      ENDDO.
      result = result->car.
    ENDMETHOD.

    METHOD proc_make_vector.
      validate: list, list->cdr.

      DATA(lo_size) = list->car.

      "      validate_integer lo_size `make-vector`.
      TRY.
          DATA(lv_length) = EXACT sytabix( lo_size->number ).
        CATCH cx_sy_conversion_error.
          throw( |make-vector: { lo_size->to_string( ) } is not an integer | ).
      ENDTRY.

      IF list->cdr NE lcl_lisp=>nil.
        DATA(lo_fill) = list->cdr->car.
      ELSE.
        lo_fill = lcl_lisp=>nil.
      ENDIF.

      result = lcl_lisp_vector=>init( size = lv_length
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

      validate: list->cdr.
      DATA(lo_idx) = list->cdr->car.
      "      validate_number lo_idx 'vector-ref'.

      TRY.
          DATA(lv_from) = EXACT sytabix( lo_idx->number ).

          result = lo_vec->get( lv_from ).

        CATCH cx_sy_conversion_error.
          throw( |vector-ref: { lo_idx->to_string( ) } is not integer | ).
      ENDTRY.

    ENDMETHOD.

    METHOD proc_vector_set.
*    (vector-set! vector k obj) procedure

      validate list.
      validate_vector list->car 'vector-set!'.
      DATA(lo_vec) = CAST lcl_lisp_vector( list->car ).

      validate: list->cdr.
      DATA(lo_idx) = list->cdr->car.
      validate_number lo_idx 'vector-set!'.

      TRY.
          DATA(lv_index) = EXACT sytabix( lo_idx->number ).
        CATCH cx_sy_conversion_error.
          throw( |vector-set!: { lo_idx->to_string( ) } is not integer | ).
      ENDTRY.

      validate: list->cdr->cdr.

      DATA(lo_obj) = list->cdr->cdr.
      IF lo_obj NE nil.
        lo_obj = lo_obj->car.
      ENDIF.

      lo_vec->set( index = lv_index
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

      DATA(lv_start) = 0.

      validate: list->cdr.
      IF list->cdr NE nil.
        DATA(lo_start) = list->cdr->car.
        "        validate_number lo_start 'vector->list start'.

        TRY.
            lv_start = EXACT sytabix( lo_start->number ).
          CATCH cx_sy_conversion_error.
            throw( |vector->list: start { lo_start->to_string( ) } is not integer | ).
        ENDTRY.

        validate list->cdr->cdr.
        IF list->cdr->cdr NE nil.
          DATA(lo_end) = list->cdr->cdr->car.
          "         validate_number lo_end 'vector->list end'.

          TRY.
              DATA(lv_end) = EXACT sytabix( lo_end->number ).

              result = lo_vec->get_list( from = lv_start
                                         to = lv_end ).

            CATCH cx_sy_conversion_error.
              throw( |vector->list: end { lo_end->to_string( ) } is not integer | ).
          ENDTRY.

        ELSE.
          result = lo_vec->get_list( from = lv_start ).
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

*(define (memq item x)
*  (cond ((null? x) #f)
*        ((eq? item (car x)) x)
*        (else (memq item (cdr x)))))
    METHOD proc_memq.
      validate: list, list->car, list->cdr.

      result = false.

      CHECK list->cdr NE nil.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_item) = list->car.
      WHILE lo_sublist NE nil AND lo_sublist->car->type EQ lo_item->type.

        CASE lo_item->type.
          WHEN lcl_lisp=>type_number.
            IF lo_item->number = lo_sublist->car->number.
              result = lo_sublist.
              RETURN.
            ENDIF.

          WHEN lcl_lisp=>type_symbol OR lcl_lisp=>type_string.
            IF lo_item->value = lo_sublist->car->value.
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
      WHILE lo_sublist NE nil.
        IF proc_equivalence( a = lo_sublist->car
                             b = lo_item ) NE false.
          result = lo_sublist.
          RETURN.
        ENDIF.
        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
    ENDMETHOD.

    METHOD proc_member.
      validate: list, list->car, list->cdr.

      result = false.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_compare) = list->cdr->cdr.
      DATA(lo_item) = list->car.

      WHILE lo_sublist NE nil.
        IF lo_item->is_equal( io_elem = lo_sublist->car
                              comp = lo_compare
                              interpreter = me ) NE false.
          result = lo_sublist.
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
      WHILE lo_sublist NE nil.
        DATA(lo_pair) = lo_sublist->car.
        CHECK lo_pair->car->type EQ lo_key->type.

        CASE lo_key->type.
          WHEN lcl_lisp=>type_number.
            IF lo_key->number = lo_pair->car->number.
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

        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
    ENDMETHOD.

    METHOD proc_assv.
      validate: list, list->car, list->cdr.

      result = false.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_key) = list->car.
      WHILE lo_sublist NE nil.
        DATA(lo_pair) = lo_sublist->car.
        IF proc_equivalence( a = lo_pair->car
                             b = lo_key ) NE false.
          result = lo_pair.
          RETURN.
        ENDIF.
        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
    ENDMETHOD.

    METHOD proc_assoc.
      validate: list, list->car, list->cdr.

      result = false.

      DATA(lo_sublist) = list->cdr->car.
      DATA(lo_key) = list->car.
      WHILE lo_sublist NE nil.
        DATA(lo_pair) = lo_sublist->car.
        IF lo_key->is_equal( lo_pair->car ) NE false.
          result = lo_pair.
          RETURN.
        ENDIF.
        lo_sublist = lo_sublist->cdr.
      ENDWHILE.
    ENDMETHOD.

**********************************************************************

    METHOD proc_add.
      validate list.
      DATA(iter) = list->new_iterator( ).
      result = lcl_lisp_new=>number( 0 ).

      WHILE iter->has_next( ).
        DATA(cell) = iter->next( ).
        validate_number cell '[+]'.
        ADD cell->number TO result->number.
      ENDWHILE.
    ENDMETHOD.                    "proc_add

    METHOD proc_subtract.
      validate list.
      DATA(iter) = list->new_iterator( ).
      DATA(cell) = iter->next( ).
      validate cell.
      result = lcl_lisp_new=>number( cell->number ).

      IF iter->has_next( ) EQ abap_false.
        result->number = 0 - result->number.
      ELSE.
*       Subtract all consecutive numbers from the first
        WHILE iter->has_next( ).
          cell = iter->next( ).
          validate_number cell '[-]'.
          result->number = result->number - cell->number.
        ENDWHILE.
      ENDIF.
    ENDMETHOD.                    "proc_subtract

    METHOD proc_multiply.
      validate list.
      DATA(iter) = list->new_iterator( ).
      DATA(cell) = iter->next( ).
      validate cell.
      result = lcl_lisp_new=>number( cell->number ).

      WHILE iter->has_next( ).
        cell = iter->next( ).
        validate_number cell '[*]'.
        result->number = result->number * cell->number.
      ENDWHILE.
    ENDMETHOD.                    "proc_multiply

    METHOD proc_divide.
      validate list.
      DATA(iter) = list->new_iterator( ).
      DATA(cell) = iter->next( ).
      validate cell.
      result = lcl_lisp_new=>number( cell->number ).

      TRY.
          IF iter->has_next( ) EQ abap_false.
            result->number = 1 / result->number.
          ELSE.
            WHILE iter->has_next( ).
              cell = iter->next( ).
              validate_number cell '[/]'.
              result->number = result->number / cell->number.
            ENDWHILE.
          ENDIF.
          _catch_arithmetic_error.
      ENDTRY.
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
      validate: list, list->car.
      validate_integer list->car '[odd?]'.
      CHECK list->car->number MOD 2 NE 0.
      result = true.
    ENDMETHOD.                    "proc_lte

    METHOD proc_is_even.
      result = false.
      validate: list, list->car.
      validate_integer list->car '[even?]'.
      CHECK list->car->number MOD 2 EQ 0.
      result = true.
    ENDMETHOD.                    "proc_lte

**********************************************************************
    METHOD proc_eql.
      validate: list, list->car, list->cdr.

      result = nil.
      DATA(lo_ptr) = list.
      WHILE lo_ptr->cdr NE nil.
        validate_number: lo_ptr->car '[=]',
                         lo_ptr->cdr->car '[=]'.
        IF lo_ptr->car->number = lo_ptr->cdr->car->number.
          result = true.
        ELSE.
          result = false.
          EXIT.
        ENDIF.
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.
    ENDMETHOD.                    "proc_eql

    METHOD proc_eq.
      validate: list, list->car, list->cdr.

      result = nil.
      DATA(lo_ptr) = list.
      DATA(lo_ref) = lo_ptr->car.
      WHILE lo_ptr->cdr NE nil.
        IF lo_ref->type NE lo_ptr->cdr->car->type.
          result = false.
          EXIT.
        ENDIF.
        CASE lo_ptr->car->type.
          WHEN lcl_lisp=>type_number.
            IF lo_ref->number = lo_ptr->cdr->car->number.
              result = true.
            ELSE.
              result = false.
              EXIT.
            ENDIF.

          WHEN lcl_lisp=>type_symbol OR lcl_lisp=>type_string.
            IF lo_ref->value = lo_ptr->cdr->car->value.
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
      ENDWHILE.
    ENDMETHOD.                    "proc_eq

    METHOD proc_equivalence.
      validate: a, b.

      result = false.

*     Object a and Object b are both #t or both #f or both the empty list.
      IF ( a EQ true AND b EQ true ) OR ( a EQ false AND b EQ false ) OR ( a EQ nil AND b EQ nil ).
        result = true.
        RETURN.
      ENDIF.

      CHECK a->type EQ b->type.

      CASE a->type.
        WHEN lcl_lisp=>type_number.
* obj1 and obj2 are both exact numbers and are numerically equal (in the sense of =).
*obj1 and obj2 are both inexact numbers such that they are numerically equal (in the sense of =)
*and they yield the same results (in the sense of eqv?) when passed as arguments to any other
*procedure that can be defined as a finite composition of Scheme’s standard arithmetic procedures,
*provided it does not result in a NaN value.
          CHECK a->number = b->number.

        WHEN lcl_lisp=>type_symbol OR lcl_lisp=>type_string.
* obj1 and obj2 are both symbols and are the same symbol according to the symbol=? procedure (section 6.5).
* obj1 and obj2 are both characters and are the same character according to the char=? procedure (section 6.6).
          CHECK a->value = b->value.

        WHEN lcl_lisp=>type_conscell OR lcl_lisp=>type_lambda.
* obj1 and obj2 are procedures whose location tags are equal (section 4.1.4).

          CHECK a->car EQ b->car AND a->cdr EQ b->cdr.

        WHEN OTHERS.
* obj1 and obj2 are pairs, vectors, bytevectors, records, or strings that denote the same location  in the store (section 3.4).

          CHECK a = b.
      ENDCASE.
      result = true.
    ENDMETHOD.

    METHOD proc_equal.
      validate: list, list->car.
      result = false.
      DATA(lo_ptr) = list.

      WHILE lo_ptr->cdr NE nil.
        DATA(lo_next) = lo_ptr->cdr->car.
        validate lo_next.

        result = lo_next->is_equal( lo_ptr->car ).
        IF result EQ false.
          RETURN.
        ENDIF.
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.

    ENDMETHOD.                    "proc_equal

    METHOD proc_eqv.
      validate: list, list->car.
      result = false.

      DATA(lo_ptr) = list.
      WHILE lo_ptr->cdr NE nil.
        DATA(lo_next) = lo_ptr->cdr->car.

        result = proc_equivalence( a = lo_next
                                   b = lo_ptr->car ).
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

    METHOD proc_is_hash.
      _is_type hash.
    ENDMETHOD.                    "proc_is_hash

    METHOD proc_is_number.
      _is_type number.
    ENDMETHOD.                    "proc_is_number

    METHOD proc_is_integer. " argument in list->car
      result = false.
      CHECK list IS BOUND AND list->car IS BOUND.
      CHECK list->car->type EQ lcl_lisp=>type_number
        AND list->car->number EQ CONV int8( list->car->number ).
      result = true.
    ENDMETHOD.                    "proc_is_integer

    METHOD proc_is_symbol.
      _is_type symbol.
    ENDMETHOD.

    METHOD proc_is_list.  " argument in list->car
      validate: list, list->car.

      result = false.

      DATA(lo_ptr) = list->car.
      WHILE lo_ptr NE nil AND lo_ptr->type = lcl_lisp=>type_conscell.
        lo_ptr = lo_ptr->cdr.
      ENDWHILE.

      CHECK lo_ptr EQ nil.
      result = true.
    ENDMETHOD.                    "proc_is_list

    METHOD proc_is_pair. " argument in list->car
      validate list.

      result = false.
      DATA(lo_arg) = list->car.
      CHECK lo_arg NE nil AND lo_arg->type = lcl_lisp=>type_conscell.
      result = true.
    ENDMETHOD.                    "proc_is_list

    METHOD proc_is_boolean. " argument in list->car
      validate list.

      result = false.

      validate list->car.
      CHECK list->car->type EQ lcl_lisp=>type_boolean.

      result = true.
    ENDMETHOD.

    METHOD proc_list_is_boolean.
      validate list.

      result = false.

      DATA(lo_arg) = list.

      WHILE lo_arg NE nil.
        IF lo_arg->car->type NE lcl_lisp=>type_boolean.
          RETURN.
        ENDIF.
        lo_arg = lo_arg->cdr.
      ENDWHILE.

      CHECK lo_arg = nil.
      result = true.
    ENDMETHOD.                    "proc_is_boolean

    METHOD proc_is_vector.  " argument in list->car
      validate: list, list->car.

      result = false.

      DATA(lo_arg) = list->car.
      CHECK lo_arg->type = lcl_lisp=>type_vector.
      result = true.
    ENDMETHOD.

    METHOD proc_is_procedure.
      result = false.
      CHECK list IS BOUND        " paramater (car) must not be valid
        AND list->car IS BOUND.  " Body
      CASE list->car->type.
        WHEN lcl_lisp=>type_lambda OR lcl_lisp=>type_native OR lcl_lisp=>type_abap_function.
          result = true.
      ENDCASE.
    ENDMETHOD.

    METHOD proc_is_alist. " not in standard?
      validate list.

      result = false.

      DATA(lo_arg) = list->car.
      WHILE lo_arg NE nil AND lo_arg->type = lcl_lisp=>type_conscell.
        IF lo_arg->car->type = lcl_lisp=>type_conscell.
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

      result = nil.
      validate: list.
      validate_number list->car '[asinh]'.
      _is_last_param list.
      TRY.
          carry = list->car->number.
          result = lcl_lisp_new=>number( log( carry + sqrt( carry ** 2 + 1 ) ) ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_asinh

    METHOD proc_acosh.
      DATA carry TYPE f.

      result = nil.
      validate list.
      validate_number list->car '[acosh]'.
      _is_last_param list.
      TRY.
          carry = list->car->number.
          result = lcl_lisp_new=>number( log( carry + sqrt( carry ** 2 - 1 ) ) ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_acosh

    METHOD proc_atanh.
      DATA carry TYPE f.

      result = nil.
      validate list.
      validate_number list->car '[atanh]'.
      _is_last_param list.
      TRY.
          carry = list->car->number.
          result = lcl_lisp_new=>number( ( log( 1 + carry ) - log( 1 - carry ) ) / 2 ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_atanh

    METHOD proc_expt.
      result = nil.
      validate: list, list->cdr.
      validate_number: list->car '[expt]',
                       list->cdr->car '[expt]'.
      _is_last_param list->cdr.
      TRY.
          result = lcl_lisp_new=>number( list->car->number ** list->cdr->car->number ).
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
      result = nil.
      validate list.
      validate_number list->car '[round]'.
      _is_last_param list.
      TRY.
          result = lcl_lisp_new=>number( round( val = list->car->number dec = 0 ) ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_round

    METHOD proc_remainder.
      result = nil.
      validate: list, list->cdr.
      validate_number: list->car '[remainder]',
                       list->cdr->car '[remainder]'.
      _is_last_param list->cdr.
      TRY.
          result = lcl_lisp_new=>number( list->car->number -
            list->cdr->car->number * trunc( list->car->number / list->cdr->car->number ) ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_remainder

    METHOD proc_modulo.
      result = nil.
      validate: list, list->cdr.
      validate_number: list->car '[modulo]',
                       list->cdr->car '[modulo]'.
      _is_last_param list->cdr.
      TRY.
          result = lcl_lisp_new=>number( list->car->number MOD list->cdr->car->number ).
          IF sign( list->cdr->car->number ) LE 0.
            result->number = result->number + list->cdr->car->number.
          ENDIF.
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_modulo

    METHOD proc_random.
      result = nil.
      validate list.
      validate_integer: list->car '[random]'.
      _is_last_param list.
      TRY.
          DATA(lo_rnd) = cl_abap_random=>create( cl_abap_random=>seed( ) ).
          result = lcl_lisp_new=>number( lo_rnd->intinrange( high = CONV i( list->car->number ) ) ).
        CATCH cx_dynamic_check INTO DATA(lx_error).
          throw( lx_error->get_text( ) ).
      ENDTRY.
    ENDMETHOD.                    "proc_random

    METHOD proc_quotient.
      result = nil.
      validate: list, list->cdr.
      validate_number: list->car '[quotient]',
                       list->cdr->car '[quotient]'.
      _is_last_param list->cdr.
      TRY.
          result = lcl_lisp_new=>number( list->car->number DIV list->cdr->car->number ).
          _catch_arithmetic_error.
      ENDTRY.
    ENDMETHOD.                    "proc_quotient

    METHOD proc_num_to_string.
      result = lcl_lisp_new=>string( list->to_string( ) ).
    ENDMETHOD.

    METHOD proc_max.
      validate list.

      result = nil.
      DATA(iter) = list->new_iterator( ).
      result = iter->next( ).
      validate_number result '[max]'.

      WHILE iter->has_next( ).
        DATA(cell) = iter->next( ).
        validate_number cell '[max]'.
        result->number = nmax( val1 = cell->number val2 = result->number ).
      ENDWHILE.
    ENDMETHOD.

    METHOD proc_min.
      validate list.

      result = nil.
      DATA(iter) = list->new_iterator( ).
      result = iter->next( ).
      validate_number result '[min]'.

      WHILE iter->has_next( ).
        DATA(cell) = iter->next( ).
        validate_number cell '[min]'.
        result->number = nmin( val1 = cell->number val2 = result->number ).
      ENDWHILE.
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
      CREATE DATA result->data TYPE (list->car->value).
*     Set value if supplied as second parameter
      IF list->cdr NE nil.
        element_to_data(
          EXPORTING
            element = list->cdr->car
          CHANGING
            data    = result->data ).
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
      validate: list, list->car.
*     First input: name of data type, second input: value
      result = lcl_lisp_new=>table( ).
      CREATE DATA result->data TYPE TABLE OF (list->car->value).
*     Set value if supplied as second parameter
      IF list->cdr NE nil.
        element_to_data( EXPORTING element = list->cdr->car
                         CHANGING data    = result->data ).
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
      FIELD-SYMBOLS <data> TYPE any.

      validate: list, list->car.
      DATA(lo_ref) = list->car.
      IF lo_ref->type NE lcl_lisp=>type_abap_data AND
         lo_ref->type NE lcl_lisp=>type_abap_table.
        throw( |ab-get-value requires ABAP data or table as parameter| ).
      ENDIF.
      TRY.
          ASSIGN lo_ref->data->* TO <data>.
          result = data_to_element( <data> ).
        CATCH cx_root INTO DATA(lx_root).
          throw( |Mapping error: { lx_root->get_text( ) }| ).
      ENDTRY.
    ENDMETHOD.                    "proc_abap_get_value

    METHOD proc_abap_set_value. "Convert Lisp to ABAP data
      FIELD-SYMBOLS <data> TYPE any.

      validate: list, list->car.
      DATA(lo_ref) = list->car.
      IF lo_ref->type NE lcl_lisp=>type_abap_data AND
         lo_ref->type NE lcl_lisp=>type_abap_table.
        throw( |ab-set-value requires ABAP data or table as first parameter| ).
      ENDIF.
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
      validate: list, list->car.

      DATA(lo_ref) = list->car.
*     Ensure a valid first parameter is passed
      IF lo_ref->type NE lcl_lisp=>type_abap_data
        AND lo_ref->type NE lcl_lisp=>type_abap_function
        AND lo_ref->type NE lcl_lisp=>type_abap_table.
        throw( |ab-get: First parameter must be ABAP data or table or a function| ).
      ENDIF.

*     Determine whether the data is elementary or not to decide if we need to get the element by identifier
      IF lo_ref->data IS NOT INITIAL AND
        cl_abap_typedescr=>describe_by_data_ref( lo_ref->data )->kind = cl_abap_typedescr=>kind_elem.
*       Elementary type; can return the value without mapping
        DATA(lr_data) = lo_ref->data.
      ELSE.
*       Could short-cut here and provide the value right away
        IF list->cdr = nil.
          throw( |ab-get: Complex type requires identifier for lookup| ).
        ELSE.
          lr_data = get_element( list ).
        ENDIF.
      ENDIF.

      result = create_element_from_data( lr_data ).

    ENDMETHOD. "proc_abap_get

    METHOD proc_abap_set.
      FIELD-SYMBOLS <target> TYPE any.
      FIELD-SYMBOLS <source> TYPE any.

      validate: list, list->car.

      DATA(lo_ref) = list->car.
*     Ensure a valid first parameter is passed
      IF lo_ref->type NE lcl_lisp=>type_abap_data
        AND lo_ref->type NE lcl_lisp=>type_abap_function
        AND lo_ref->type NE lcl_lisp=>type_abap_table.
        throw( |ab-set: First parameter must be ABAP data or table or a function| ).
      ENDIF.

*     Determine whether the data is elementary or not to decide if we need to get the element by identifier
      IF lo_ref->data IS NOT INITIAL AND
        cl_abap_typedescr=>describe_by_data_ref( lo_ref->data )->kind = cl_abap_typedescr=>kind_elem.
*       Elementary type; can return the value without mapping
        DATA(lr_target) = lo_ref->data.
        DATA(lo_source) = list->cdr->car.  "Value to set is data ref from second argument
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
          WHEN lcl_lisp=>type_number.
            <target> = lo_source->number.
        ENDCASE.
      ELSE.
*       Complex types will just copy the whole value across
        ASSIGN lo_source->data->* TO <source>.
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
          IF element->type NE lcl_lisp=>type_conscell.
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
          IF element->type NE lcl_lisp=>type_conscell.
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
          <field> = COND #( WHEN element->type = lcl_lisp=>type_number THEN element->number ELSE element->value ).

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

      IF identifier = nil OR identifier->type NE lcl_lisp=>type_number.
        throw( |ab-get: Numeric index required to read index table| ). "Can do AB-TAB-WHERE some other time
      ENDIF.
      ASSIGN element->data->* TO <idxtab>.
      TRY.
          rdata = REF #( <idxtab>[ identifier->number ] ).
        CATCH cx_sy_itab_line_not_found.
          throw( |ab-get: No entry at index { identifier->number }| ). "Can do AB-TAB-WHERE some other time
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
*
*       ASSIGN element->data->* TO <tab>.
*       CREATE DATA line LIKE LINE OF <tab>.
*       ASSIGN line->* TO <wa>.
*
*       READ TABLE <tab> FROM <wa> REFERENCE INTO rdata.
*       CHECK sy-subrc NE 0.
*       throw( |AB-GET: No entry at key| ).
    ENDMETHOD.                    "get_table_row_with_key

    METHOD get_element.
*     RDATA <- Data reference to value pointed to
      DATA(element) = list->car.         " Lisp element containing an ABAP value (data, table or function)
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
                             DEFAULT parameters[ name = lv_parmname ]-value ).
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
*     takes a symbol key and uses the find logic to locate the environment with the key,
*     then returns the matching value.
*     raises an "unbound" error if no key is found up the environment chain
      TRY.
          cell = VALUE #( map[ symbol = symbol ]-value DEFAULT outer->get( symbol ) ).
        CATCH cx_root.
          unbound_symbol( symbol ).
      ENDTRY.
    ENDMETHOD.                    "find

    METHOD unbound_symbol.
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
*      Add a value to the (local) environment
      DATA(ls_map) = VALUE ts_map( symbol = symbol
                                   value = element ).
      INSERT ls_map INTO TABLE map.
      CHECK sy-subrc = 4.                " To comply with Scheme define,
      MODIFY TABLE map FROM ls_map.      " overwrite existing defined values
    ENDMETHOD.                    "define

    METHOD parameters_to_symbols.
*     The lambda receives its own local environment in which to execute,
*     where parameters become symbols that are mapped to the corresponding arguments
*     Assign each argument to its corresponding symbol in the newly created environment
      DATA lv_count TYPE i.

      " they are 3 cases here
      CASE io_pars->type.
        WHEN lcl_lisp=>type_conscell.   "Do we have a proper list?

          DATA(lo_par) = io_pars.                " Pointer to formal parameters
          DATA(lo_arg) = io_args.                " Pointer to arguments

          WHILE lo_par NE lcl_lisp=>nil.         " Nil would mean no parameters to map

            IF lo_par->type EQ lcl_lisp=>type_symbol.
*             dotted pair after fixed number of parameters, to be bound to a variable number of arguments

*             1) Read the next parameter, bind to the (rest) list of arguments
              set( symbol = lo_par->value
                   element = lo_arg ).

*             2) Exit
              RETURN.
            ENDIF.

*           Part of the list with fixed number of parameters

            IF lo_arg = lcl_lisp=>nil.           " Premature end of arguments
              lcl_lisp=>throw( |Missing parameter(s) { lo_par->to_string( ) }| ).
            ENDIF.

            ADD 1 TO lv_count.

*           NOTE: Each element of the argument list is evaluated before being defined in the environment
            set( symbol = lo_par->car->value
                 element = lo_arg->car ).

            lo_par = lo_par->cdr.
            lo_arg = lo_arg->rest( ).
          ENDWHILE.

          IF lo_arg NE lcl_lisp=>nil.  " Excessive number of arguments
            lcl_lisp=>throw( |Expected { lv_count } parameter(s), found { io_args->to_string( ) }| ).
          ENDIF.

        WHEN lcl_lisp=>type_symbol.
          "3) args is a symbol to be bound to a variable number of parameters
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
      quote = lcl_lisp_new=>symbol( 'quote' ).
      quasiquote = lcl_lisp_new=>symbol( 'quasiquote' ).
      unquote = lcl_lisp_new=>symbol( 'unquote' ).
      unquote_splicing = lcl_lisp_new=>symbol( 'unquote-splicing' ).

      new_line = lcl_lisp_new=>string( |\n| ).
    ENDMETHOD.

    METHOD rest.
      ro_cdr = COND #( WHEN cdr IS BOUND THEN cdr ELSE nil ).
    ENDMETHOD.                    "rest

    METHOD is_equal.
      validate: io_elem.

      IF comp NE nil.
        DATA(lo_arg) = lcl_lisp_new=>cons( io_car = me
                                           io_cdr = lcl_lisp_new=>cons( io_car = io_elem ) ).
        DATA(lo_head) = lcl_lisp_new=>cons( io_car = comp
                                            io_cdr = lo_arg ).

        result = interpreter->eval( element = lo_head
                                    environment = comp->environment ).
        RETURN.
      ENDIF.

      result = false.
      CHECK type EQ io_elem->type.

      CASE type.
        WHEN lcl_lisp=>type_number.
          CHECK number = io_elem->number.

        WHEN lcl_lisp=>type_symbol OR lcl_lisp=>type_string.
          CHECK value = io_elem->value.

        WHEN lcl_lisp=>type_conscell OR lcl_lisp=>type_lambda.
          CHECK car->is_equal( io_elem->car ) NE false
            AND cdr->is_equal( io_elem->cdr ) NE false.

        WHEN OTHERS.

      ENDCASE.
      result = true.
    ENDMETHOD.

    METHOD new_iterator.
      ro_iter = NEW lcl_lisp_iterator( me ).
    ENDMETHOD.

    METHOD list_to_string.
      str = lcl_parser=>c_open_paren.
      DATA(lo_elem) = me.
      WHILE lo_elem IS BOUND AND lo_elem NE nil.
        str = str && COND string( WHEN lo_elem->type NE type_conscell     " If item is not a cons cell
                                     THEN | . { lo_elem->write( ) }|      " indicate with dot notation:
                                     ELSE | { lo_elem->car->write( ) }| ).
        lo_elem = lo_elem->cdr.
      ENDWHILE.

      str = |{ str } { lcl_parser=>c_close_paren }|.
    ENDMETHOD.                    "list_to_string

    METHOD to_string.
      CASE type.
        WHEN type_lambda.
          str = |<lambda> { car->list_to_string( ) }|.
        WHEN type_null.
          str = 'nil'.
        WHEN type_symbol.
          str = value.
        WHEN type_boolean.
          str = value.
        WHEN type_string.
          str = value.
        WHEN type_number.
          str = number.
        WHEN type_native.
          str = '<native>'.
        WHEN type_conscell.
          str = list_to_string( ).
        WHEN type_hash.
          str = '<hash>'.
        WHEN type_vector.
          str = '<vector>'.
*--------------------------------------------------------------------*
*        Additions for ABAP Types:
        WHEN type_abap_function.
          str = |<ABAP function module { value }>|.
        WHEN type_abap_class.
          str = |<ABAP class { value }>|.
        WHEN type_abap_method.
*          TODO
*           str = |<ABAP method { car->value }->{ cdr->value }( ) >|.
        WHEN type_abap_data.
          str = |<ABAP Data>|.
        WHEN type_abap_table.
          str = |<ABAP Table>|.
      ENDCASE.
    ENDMETHOD.                    "to_string

    METHOD error_not_a_list.
      throw( context && to_string( ) && ` is not a list` ).
    ENDMETHOD.

    METHOD error_not_a_pair.
      throw( context && to_string( ) && ` is not a pair` ).
    ENDMETHOD.

    METHOD write.
      CASE type.
        WHEN type_string.
*         give back the string as a quoted string
          str = |"{ escape( val = value
                    format = cl_abap_format=>e_html_js ) }"|.
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
        IF elem->cdr->type NE lcl_lisp=>type_conscell.
          lcl_lisp=>throw( |{ elem->to_string( ) } is not a proper list| ).
        ENDIF.
        elem = elem->cdr.
      ENDIF.
      ro_elem = elem->car.
    ENDMETHOD.                    "next

  ENDCLASS.                    "lcl_lisp_iterator IMPLEMENTATION

  CLASS lcl_lisp_new IMPLEMENTATION.

    METHOD node.
      ro_elem = NEW lcl_lisp( ).
      ro_elem->type = type.
    ENDMETHOD.

    METHOD elem.
      ro_elem = node( type ).

      CASE type.
        WHEN lcl_lisp=>type_number.
          ro_elem->number = value.
*         WHEN type_abap_data OR type_abap_table.
*           ro_elem->data = ref.
        WHEN OTHERS.
          ro_elem->value = value.
      ENDCASE.
    ENDMETHOD.                    "new_elem

    METHOD string.
      ro_elem = node( lcl_lisp=>type_string ).
      ro_elem->value = value.
    ENDMETHOD.                    "new_string

    METHOD symbol.
      ro_elem = node( lcl_lisp=>type_symbol ).
      ro_elem->value = value.
    ENDMETHOD.                    "new_symbol

    METHOD boolean.
      ro_elem = node( lcl_lisp=>type_boolean ).
      ro_elem->value = value.
    ENDMETHOD.                    "new_symbol

    METHOD null.
      ro_elem = node( lcl_lisp=>type_null ).
      ro_elem->value = 'nil'.
    ENDMETHOD.                    "new_symbol

    METHOD number.
      ro_elem = node( lcl_lisp=>type_number ).
      ro_elem->number = value.
    ENDMETHOD.                    "new_number

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
          TRY.
              DATA(lv_num) = EXACT decfloat34( value ) ##needed. "If this passes, it's a number
              ro_elem = number( value ).
            CATCH cx_sy_conversion_no_number.
              ro_elem = symbol( value ).
          ENDTRY.
      ENDCASE.
    ENDMETHOD.                    "new_atom

    METHOD data.
      ro_elem = node( lcl_lisp=>type_abap_data ).
      ro_elem->data = ref.
    ENDMETHOD.                    "new_data

    METHOD table.
      ro_elem = node( lcl_lisp=>type_abap_table ).
      ro_elem->data = ref.
    ENDMETHOD.                    "new_table

    METHOD cons.
      ro_cons = node( lcl_lisp=>type_conscell ).
      ro_cons->car = io_car.
      ro_cons->cdr = io_cdr.
    ENDMETHOD.                    "new_cons

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
      ro_lambda = node( lcl_lisp=>type_lambda ).
      ro_lambda->car = io_car.               " List of parameters
      ro_lambda->cdr = io_cdr.               " Body

*     Store the reference to the environment in which the lambda was created
*     (lexical scope) e.g. if the lambda is created inside another lambda
*     we want that environment to be present when we evaluate the new lambda
      ro_lambda->environment = io_env.
    ENDMETHOD.                    "new_lambda

    METHOD hash.
      validate io_list.

      ro_hash = NEW lcl_lisp_hash( ).
      ro_hash->type = lcl_lisp=>type_hash.
      ro_hash->fill( io_list->car ).

    ENDMETHOD.

    METHOD quote.
      ro_elem = cons( io_car = lcl_lisp=>quote
                      io_cdr = cons( io_car = io_elem )  ).
      ro_elem->cdr->mutable = abap_false.
    ENDMETHOD.

    METHOD unquote.
      ro_elem = cons( io_car = lcl_lisp=>unquote
                      io_cdr = cons( io_car = io_elem )  ).
      ro_elem->cdr->mutable = abap_false.
    ENDMETHOD.

    METHOD splice_unquote.
      ro_elem = cons( io_car = lcl_lisp=>unquote_splicing
                      io_cdr = cons( io_car = io_elem )  ).
      ro_elem->cdr->mutable = abap_false.
    ENDMETHOD.

    METHOD box_quote.
*     quote to avoid double eval
      ro_elem = cons( io_car = quote( io_elem ) ).
    ENDMETHOD.

    METHOD quasiquote.
      ro_elem = cons( io_car = lcl_lisp=>quasiquote
                      io_cdr = cons( io_car = io_elem )  ).
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

    METHOD fill.
      validate: list.

      DATA(lo_head) = list.
      CHECK lo_head->type = type_conscell.

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
                                        iv_mutable = mutable ).
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
      ro_elem = lcl_lisp=>nil.
      LOOP AT vector ASSIGNING FIELD-SYMBOL(<vec>).
        AT FIRST.
          ro_elem = lcl_lisp_new=>cons( io_car = <vec> ).
          DATA(lo_ptr) = ro_elem.
          CONTINUE.
        ENDAT.
        lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = <vec> ).
      ENDLOOP.
    ENDMETHOD.

    METHOD get.
      DATA(lv_start) = index + 1.

      IF lv_start BETWEEN 1 AND lines( vector ).
        ro_elem = vector[ lv_start ].
      ELSE.
        throw( |vector-ref: out-of-bound position| ).
      ENDIF.
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
        throw( |vector-ref: out-of-bound position| ).
      ENDIF.

      ro_elem = nil.
      CHECK lv_start BETWEEN 1 AND lv_end.


      ro_elem = lcl_lisp_new=>cons( io_car = vector[ lv_start ] ).

      DATA(lo_ptr) = ro_elem.
      LOOP AT vector FROM lv_start + 1 TO lv_end ASSIGNING FIELD-SYMBOL(<vec>).
        lo_ptr = lo_ptr->cdr = lcl_lisp_new=>cons( io_car = <vec> ).
      ENDLOOP.
    ENDMETHOD.

    METHOD set.
      validate_mutable me `vector`.

      IF index GE lines( vector ) OR index LT 0.
        throw( |vector-set!: out-of-bound position { index }| ).
      ENDIF.

      vector[ index + 1 ] = io_elem.
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
      result = super->is_equal( io_elem ).

      CHECK result EQ true.
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

  ENDCLASS.
