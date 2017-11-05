*&---------------------------------------------------------------------*
*&  Include           YY_LIB_LISP
*& https://github.com/mydoghasworms/abap-lisp
*& Lisp interpreter written in ABAP
*& Copy and paste this code into a type I (include) program
*&---------------------------------------------------------------------*
*& Martin Ceronio, martin.ceronio@infosize.co.za
*& June 2015
*& MIT License (see below)
*& Updated by Jacques Nomssi Nzali, www.informatik-dv.com Sept. 2015
*&---------------------------------------------------------------------*
*  The MIT License (MIT)
*
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
     assert_is_bound &1 'Incorrect input'.
   END-OF-DEFINITION.

   DEFINE validate_number.
     IF &1->type NE lcl_lisp=>type_number.
       throw( |{ &1->to_string( ) } is not a number | && &2 ).
     ENDIF.
   END-OF-DEFINITION.

   DEFINE validate_integer.
     IF &1->type NE lcl_lisp=>type_number OR frac( &1->number ) NE 0.
       throw( |{ &1->to_string( ) } is not an integer | && &2 ).
     ENDIF.
   END-OF-DEFINITION.

*  Macro that implements the logic for the comparison native
*  procedures, where only the comparison operator differs
   DEFINE _comparison.
     DATA carry TYPE decfloat34.

     result = false.
     validate: list, list->car.
     validate_number list->car &2.
     DATA(cell) = list->cdr.
     carry = list->car->number.
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
     DATA carry TYPE decfloat34.

     result = false.
     validate: list, list->car.
     validate_number list->car &2.
     carry = list->car->number.
     IF sign( carry ) NE &1.
       RETURN.
     ENDIF.
     result = true.
   END-OF-DEFINITION.

   DEFINE _is_type.
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
     validate: list, list->car.
     validate_number list->car &2.
     _is_last_param list.
     TRY.
         result = lcl_lisp=>new_number( &1( list->car->number ) ).
       _catch_arithmetic_error.
     ENDTRY.
   END-OF-DEFINITION.

   DEFINE _trigonometric.
     DATA carry TYPE f.

     result = nil.
     validate: list, list->car.
     validate_number list->car &2.
     _is_last_param list.
     TRY.
         carry = list->car->number.
         result = lcl_lisp=>new_number( &1( carry ) ).
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
       CONSTANTS: c_area_eval  TYPE string VALUE `Eval`,
                  c_area_parse TYPE string VALUE `Parse`.
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
         type_conscell TYPE tv_type VALUE 'C',
         type_lambda   TYPE tv_type VALUE '#',
         type_native   TYPE tv_type VALUE 'P',
         type_hash     TYPE tv_type VALUE 'H'.
*      Types for ABAP integration:
       CONSTANTS:
         type_abap_data     TYPE tv_type VALUE 'D',
         type_abap_table    TYPE tv_type VALUE 'T',
         type_abap_function TYPE tv_type VALUE 'F',
         type_abap_class    TYPE tv_type VALUE 'R',
         type_abap_method   TYPE tv_type VALUE 'M'.

       DATA type TYPE char1.

       DATA value TYPE string.
       DATA number TYPE decfloat34.
       DATA data TYPE REF TO data.            " for ABAP integration
   ENDCLASS.

* Single element that will capture cons cells, atoms etc.
*----------------------------------------------------------------------*
*       CLASS lcl_lisp DEFINITION
*----------------------------------------------------------------------*
   CLASS lcl_lisp DEFINITION INHERITING FROM lcl_elem.
     PUBLIC SECTION.
* Can this be replaced by a mesh? cf. DEMO_RND_PARSER_AST
       DATA car TYPE REF TO lcl_lisp.
       DATA cdr TYPE REF TO lcl_lisp.

       CLASS-METHODS class_constructor.

       CLASS-DATA nil TYPE REF TO   lcl_lisp READ-ONLY.
       CLASS-DATA false TYPE REF TO  lcl_lisp READ-ONLY.
       CLASS-DATA true TYPE REF TO  lcl_lisp READ-ONLY.

*      Specifically for lambdas:
       DATA environment TYPE REF TO lcl_lisp_environment.
*      Format
       METHODS to_string RETURNING VALUE(str) TYPE string
                         RAISING   lcx_lisp_exception.
*      Utilities
       METHODS first RETURNING VALUE(ro_car) TYPE REF TO lcl_lisp.
       METHODS rest RETURNING VALUE(ro_cdr) TYPE REF TO lcl_lisp.

*      Factories
       CLASS-METHODS new_atom IMPORTING value          TYPE any
                              RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
       CLASS-METHODS new_symbol IMPORTING value          TYPE any
                                RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
       CLASS-METHODS new_number IMPORTING value          TYPE any
                                RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
       CLASS-METHODS new_string IMPORTING value          TYPE any
                                RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

       CLASS-METHODS new_elem IMPORTING type           TYPE tv_type
                                        value          TYPE any
                              RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
       CLASS-METHODS new_data IMPORTING ref            TYPE REF TO data OPTIONAL
                              RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
       CLASS-METHODS new_table IMPORTING ref            TYPE REF TO data OPTIONAL
                               RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.
       CLASS-METHODS new_cons IMPORTING io_car         TYPE REF TO lcl_lisp DEFAULT nil
                                        io_cdr         TYPE REF TO lcl_lisp DEFAULT nil
                              RETURNING VALUE(ro_cons) TYPE REF TO lcl_lisp.
       CLASS-METHODS new_lambda IMPORTING io_car           TYPE REF TO lcl_lisp
                                          io_cdr           TYPE REF TO lcl_lisp
                                          io_env           TYPE REF TO lcl_lisp_environment
                                RETURNING VALUE(ro_lambda) TYPE REF TO lcl_lisp.

       CLASS-METHODS throw IMPORTING message TYPE string
                           RAISING   lcx_lisp_exception.
     PROTECTED SECTION.
       CLASS-METHODS new IMPORTING type           TYPE tv_type
                                   io_car         TYPE REF TO lcl_lisp DEFAULT nil
                                   io_cdr         TYPE REF TO lcl_lisp DEFAULT nil
                         RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp.

       METHODS list_to_string RETURNING VALUE(str) TYPE string
                              RAISING   lcx_lisp_exception.
   ENDCLASS.                    "lcl_lisp DEFINITION

   INTERFACE lif_port.
     METHODS write IMPORTING element TYPE REF TO lcl_lisp.
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
   CLASS lcl_lisp_iterator DEFINITION CREATE PRIVATE.
     PUBLIC SECTION.
       CLASS-METHODS new IMPORTING io_elem        TYPE REF TO lcl_lisp
                         RETURNING VALUE(ro_iter) TYPE REF TO lcl_lisp_iterator
                         RAISING   lcx_lisp_exception.
       METHODS has_next RETURNING VALUE(rv_flag) TYPE flag.
       METHODS next RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp
                    RAISING   cx_dynamic_check.
     PRIVATE SECTION.
       DATA first TYPE flag VALUE abap_true.
       DATA elem TYPE REF TO lcl_lisp.
   ENDCLASS.                    "lcl_lisp_iterator DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_hash DEFINITION
*----------------------------------------------------------------------*
* Hash is a specialized ABAP Lisp type for quick lookup of elements
* using a symbol or string key (backed by an ABAP hash table)
*----------------------------------------------------------------------*
   CLASS lcl_lisp_hash DEFINITION INHERITING FROM lcl_lisp.
     PUBLIC SECTION.

       CLASS-METHODS new_hash IMPORTING list           TYPE REF TO lcl_lisp
                              RETURNING VALUE(ro_elem) TYPE REF TO lcl_lisp_hash
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
       TYPES: BEGIN OF ts_hash,
                key     TYPE string,
                element TYPE REF TO lcl_lisp,
              END OF ts_hash.
       TYPES tt_hash TYPE HASHED TABLE OF ts_hash WITH UNIQUE KEY key.
       DATA hash TYPE tt_hash.

   ENDCLASS.                    "lcl_lisp_hash DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_abapfunction DEFINITION
*----------------------------------------------------------------------*
* Specialized element representing an ABAP function module that can
* be called
*----------------------------------------------------------------------*
   CLASS lcl_lisp_abapfunction DEFINITION INHERITING FROM lcl_lisp.
     PUBLIC SECTION.
       CLASS-METHODS new_function
         IMPORTING list           TYPE REF TO lcl_lisp
         RETURNING VALUE(ro_func) TYPE REF TO lcl_lisp_abapfunction
         RAISING   lcx_lisp_exception.

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
         lookup IMPORTING symbol      TYPE any
                RETURNING VALUE(cell) TYPE REF TO lcl_lisp
                RAISING   lcx_lisp_exception,
         define IMPORTING symbol  TYPE string
                          element TYPE REF TO lcl_lisp,
*        Convenience method to add a value and create the cell
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
         c_open_paren  TYPE char1 VALUE '(',
         c_close_paren TYPE char1 VALUE ')'.

       METHODS:
         constructor,
         parse IMPORTING code            TYPE clike
               RETURNING VALUE(elements) TYPE tt_element
               RAISING   lcx_lisp_exception.
     PRIVATE SECTION.
       CONSTANTS:
         c_escape_char  TYPE char1 VALUE '\',
         c_text_quote   TYPE char1 VALUE '"',
         c_lisp_quote   TYPE char1 VALUE '''', "LISP single quote = QUOTE
         "c_abap_data    TYPE char1 VALUE '@',
         c_lisp_comment TYPE char1 VALUE ';'.
       DATA code TYPE string.
       DATA length TYPE i.
       DATA index TYPE i.
       DATA char TYPE char1.

       DATA mv_eol TYPE char1.
       DATA mv_whitespace TYPE char04.
       DATA mv_delimiters TYPE char05.

       METHODS:
         next_char RAISING lcx_lisp_exception,
         skip_whitespace
           RETURNING VALUE(rv_has_next) TYPE flag
           RAISING   lcx_lisp_exception,
         parse_list RETURNING VALUE(result) TYPE REF TO lcl_lisp
                    RAISING   lcx_lisp_exception,
         parse_token RETURNING VALUE(element) TYPE REF TO lcl_lisp
                     RAISING   lcx_lisp_exception.
       METHODS match_string CHANGING cv_val TYPE string.
       METHODS run_to_delimiter CHANGING cv_val TYPE string.
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
       proc_append,   ##called
       proc_append_unsafe,   ##called
       proc_reverse,  ##called
       proc_car,      ##called
       proc_cdr,      ##called
       proc_cons,     ##called

       proc_memq,     ##called
       proc_memv,     ##called
       proc_member,   ##called
       proc_assq,     ##called
       proc_assv,     ##called
       proc_assoc,    ##called

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

       proc_is_number,     ##called
       proc_is_string,     ##called
       proc_is_symbol,     ##called
       proc_is_hash,       ##called
       proc_is_type,       ##called
       proc_is_procedure,  ##called
       proc_is_list,       ##called
       proc_is_alist,      ##called

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
* Not in the spec: Just adding it anyway
       proc_random,       ##called
       proc_eq,           ##called
       proc_equal.        ##called

* Functions for dealing with hashes:
       _proc_meth:
       proc_make_hash,    ##called "Create new hash
       proc_hash_get,     ##called "Get an element from a hash
       proc_hash_insert,  ##called "Insert a new element into a hash
       proc_hash_remove,  ##called "Delete an item from a hash
       proc_hash_keys.    ##called "Delete an item from a hash

* Built-in functions for ABAP integration:
       _proc_meth:
       proc_abap_data,          ##called
       proc_abap_function,      ##called
       proc_abap_table,         ##called
       proc_abap_append_row,    ##called
       proc_abap_delete_row,    ##called
       proc_abap_get_row,       ##called
       proc_abap_get_value,     ##called
       proc_abap_set_value,     ##called
       proc_abap_set,           ##called
       proc_abap_get,           ##called
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
*        Convert ABAP data to Lisp element
         data_to_element IMPORTING VALUE(data)    TYPE any
                         RETURNING VALUE(element) TYPE REF TO lcl_lisp
                         RAISING   lcx_lisp_exception,
*        Convert Lisp element to ABAP Data
         element_to_data IMPORTING VALUE(element) TYPE REF TO lcl_lisp
                         CHANGING  VALUE(data)    TYPE any "ref to data
                         RAISING   lcx_lisp_exception,
*        Determine an ABAP data component from an element and an identifier
         get_element IMPORTING list         TYPE REF TO lcl_lisp
                     RETURNING VALUE(rdata) TYPE REF TO data
                     RAISING   lcx_lisp_exception.

       DATA mi_port TYPE REF TO lif_port.

       METHODS write IMPORTING io_elem       TYPE REF TO lcl_lisp
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
       METHODS proc_compare IMPORTING a             TYPE REF TO lcl_lisp
                                      b             TYPE REF TO lcl_lisp
                            RETURNING VALUE(result) TYPE REF TO lcl_lisp
                            RAISING   lcx_lisp_exception.
       METHODS create_element_from_data
         IMPORTING ir_data       TYPE REF TO data
         RETURNING VALUE(result) TYPE REF TO lcl_lisp.

       METHODS structure_to_element IMPORTING VALUE(struct)  TYPE any
                                    RETURNING VALUE(element) TYPE REF TO lcl_lisp
                                    RAISING   lcx_lisp_exception.
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
                                   RETURNING VALUE(ro_args) TYPE REF TO lcl_lisp
                                   RAISING   lcx_lisp_exception.
       METHODS eval_function IMPORTING io_head       TYPE REF TO lcl_lisp
                                       io_args       TYPE REF TO lcl_lisp
                                       environment   TYPE REF TO lcl_lisp_environment
                             RETURNING VALUE(result) TYPE  REF TO lcl_lisp
                             RAISING   lcx_lisp_exception.

       METHODS extract_arguments IMPORTING io_head TYPE REF TO lcl_lisp
                                 EXPORTING eo_pars TYPE REF TO lcl_lisp
                                           eo_args TYPE REF TO lcl_lisp
                                 RAISING   lcx_lisp_exception.
       METHODS evaluate_list IMPORTING io_head       TYPE REF TO lcl_lisp
                                       io_env        TYPE REF TO lcl_lisp_environment
                             RETURNING VALUE(result) TYPE REF TO lcl_lisp
                             RAISING   lcx_lisp_exception.

       METHODS evaluate_in_sequence IMPORTING io_pars TYPE REF TO lcl_lisp
                                              io_args TYPE REF TO lcl_lisp
                                              io_env  TYPE REF TO lcl_lisp_environment
                                    RAISING   lcx_lisp_exception.
       METHODS init_letrec IMPORTING io_head TYPE REF TO lcl_lisp
                                     io_env  TYPE REF TO lcl_lisp_environment
                           RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment
                           RAISING   lcx_lisp_exception.
       METHODS init_let_star IMPORTING io_head TYPE REF TO lcl_lisp
                                       io_env  TYPE REF TO lcl_lisp_environment
                             RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment
                             RAISING   lcx_lisp_exception.
       METHODS init_named_let IMPORTING io_head TYPE REF TO lcl_lisp
                                        io_env  TYPE REF TO lcl_lisp_environment
                              RETURNING VALUE(ro_env) TYPE REF TO lcl_lisp_environment
                              RAISING   lcx_lisp_exception.

   ENDCLASS.                    "lcl_lisp_interpreter DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_parser IMPLEMENTATION
*----------------------------------------------------------------------*
   CLASS lcl_parser IMPLEMENTATION.

     METHOD constructor.
*      End of line value
       mv_eol = cl_abap_char_utilities=>newline.
*      Whitespace values
       CLEAR mv_whitespace.
       mv_whitespace+0(1) = ' '.
       mv_whitespace+1(1) = cl_abap_char_utilities=>newline.
       mv_whitespace+2(1) = cl_abap_char_utilities=>cr_lf(1).
       mv_whitespace+3(1) = cl_abap_char_utilities=>horizontal_tab.
*      Delimiters value
       mv_delimiters = mv_whitespace.
       mv_delimiters+3(1) = c_close_paren.
       mv_delimiters+4(1) = c_open_paren.
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
             message = |Unexpected end|
             area    = lcx_lisp_exception=>c_area_parse.
       ENDIF.
     ENDMETHOD.                    "next_char

     METHOD parse.
*      Entry point for parsing code. This is not thread-safe, but as an ABAP
*      process does not have the concept of threads, we are safe :-)
       me->code = code.
       length = strlen( code ).
       IF length = 0.
         APPEND lcl_lisp=>nil TO elements.
         RETURN.
       ENDIF.

       index = 0.
       char = code+index(1).           "Kick off things by reading first char
       WHILE skip_whitespace( ).
         IF char = c_open_paren.
           APPEND parse_list( ) TO elements.
         ELSEIF index < length.
           APPEND parse_token( ) TO elements.
         ENDIF.
       ENDWHILE.
     ENDMETHOD.                    "parse

     METHOD parse_list.
       DATA lo_cell TYPE REF TO lcl_lisp.
       DATA lv_empty_list TYPE boole_d VALUE abap_true.

*      Set pointer to start of list
       lo_cell = result = lcl_lisp=>new_cons( ).

       next_char( ).                 " Skip past opening paren
       WHILE skip_whitespace( ).
         IF char = c_close_paren.
           IF lv_empty_list = abap_true.
             result = lcl_lisp=>nil.           " Result = empty list
           ELSE.
             lo_cell->cdr = lcl_lisp=>nil.     " Terminate list
           ENDIF.
           next_char( ).              " Skip past closing paren
           RETURN.
         ENDIF.
         IF lv_empty_list = abap_false.
*          On at least the second item; add new cell and move pointer
           lo_cell = lo_cell->cdr = lcl_lisp=>new_cons( ).
         ENDIF.
         lv_empty_list = abap_false. " Next char was not closing paren
         lo_cell->car = parse_token( ).
       ENDWHILE.
     ENDMETHOD.                    "parse_list

     METHOD match_string.
       DATA pchar TYPE char1.

       next_char( ).                 " Skip past opening quote
       WHILE index < length AND NOT ( char = c_text_quote AND pchar NE c_escape_char ).
*         cv_val = |{ cv_val }{ char }|.
         CONCATENATE cv_val char INTO cv_val RESPECTING BLANKS.
         pchar = char.
         next_char( ).
       ENDWHILE.
       next_char( ).                 "Skip past closing quote
     ENDMETHOD.                    "match_string

     METHOD run_to_delimiter.
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
*      create object cell.
       CASE char.
         WHEN c_open_paren.
           element = parse_list( ).

         WHEN c_lisp_quote.
* ' is just a shortcut for QUOTE, so we wrap the consecutive element in a list starting with the quote symbol
* so that when it is evaluated later, it returns the quote elements unmodified
           next_char( ).            " Skip past single quote
           element = lcl_lisp=>new_cons( io_car = lcl_lisp=>new_symbol( 'quote' )
                                         io_cdr = lcl_lisp=>new_cons( io_cdr = lcl_lisp=>nil
                                                                      io_car = parse_token( ) ) ).
         WHEN c_text_quote.
           match_string( CHANGING cv_val = sval ).
           element = lcl_lisp=>new_string( sval ).

         WHEN OTHERS.
           run_to_delimiter( CHANGING cv_val = sval ).
           element = COND #( WHEN sval IS INITIAL
                             THEN lcl_lisp=>nil
                             ELSE lcl_lisp=>new_atom( sval ) ).
       ENDCASE.

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

*      Create symbols for nil, true and false values
       nil = lcl_lisp=>nil.
       true = lcl_lisp=>true.
       false = lcl_lisp=>false.
       env->define( symbol = 'nil'  element = nil ).
       env->define( symbol = '#f' element = false ).
       env->define( symbol = '#t' element = true ).

*      Add native functions to environment
       env->define_value( symbol = '+'       type = lcl_lisp=>type_native value   = 'PROC_ADD' ).
       env->define_value( symbol = '-'       type = lcl_lisp=>type_native value   = 'PROC_SUBTRACT' ).
       env->define_value( symbol = '*'       type = lcl_lisp=>type_native value   = 'PROC_MULTIPLY' ).
       env->define_value( symbol = '/'       type = lcl_lisp=>type_native value   = 'PROC_DIVIDE' ).
       env->define_value( symbol = 'append'  type = lcl_lisp=>type_native value   = 'PROC_APPEND' ).
       env->define_value( symbol = 'append!' type = lcl_lisp=>type_native value   = 'PROC_APPEND_UNSAFE' ).
       env->define_value( symbol = 'list'    type = lcl_lisp=>type_native value   = 'PROC_LIST' ).
       env->define_value( symbol = 'length'  type = lcl_lisp=>type_native value   = 'PROC_LENGTH' ).
       env->define_value( symbol = 'reverse' type = lcl_lisp=>type_native value   = 'PROC_REVERSE' ).

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
*      Hash-related functions
       env->define_value( symbol = 'make-hash'   type = lcl_lisp=>type_native value   = 'PROC_MAKE_HASH' ).
       env->define_value( symbol = 'hash-get'    type = lcl_lisp=>type_native value   = 'PROC_HASH_GET' ).
       env->define_value( symbol = 'hash-insert' type = lcl_lisp=>type_native value   = 'PROC_HASH_INSERT' ).
       env->define_value( symbol = 'hash-remove' type = lcl_lisp=>type_native value   = 'PROC_HASH_REMOVE' ).
       env->define_value( symbol = 'hash-keys'   type = lcl_lisp=>type_native value   = 'PROC_HASH_KEYS' ).
*      Functions for type:
       env->define_value( symbol = 'string?'     type = lcl_lisp=>type_native value = 'PROC_IS_STRING' ).
       env->define_value( symbol = 'hash?'       type = lcl_lisp=>type_native value = 'PROC_IS_HASH' ).
       env->define_value( symbol = 'number?'     type = lcl_lisp=>type_native value = 'PROC_IS_NUMBER' ).
       env->define_value( symbol = 'list?'       type = lcl_lisp=>type_native value = 'PROC_IS_LIST' ).
       env->define_value( symbol = 'alist?'      type = lcl_lisp=>type_native value = 'PROC_IS_ALIST' ).
       env->define_value( symbol = 'procedure?'  type = lcl_lisp=>type_native value = 'PROC_IS_PROCEDURE' ).
       env->define_value( symbol = 'symbol?'     type = lcl_lisp=>type_native value = 'PROC_IS_SYMBOL' ).
       env->define_value( symbol = 'type'        type = lcl_lisp=>type_native value = 'PROC_IS_TYPE' ).

*      Math
       env->define_value( symbol = 'abs' type = lcl_lisp=>type_native value = 'PROC_ABS' ).
       env->define_value( symbol = 'sin' type = lcl_lisp=>type_native value = 'PROC_SIN' ).
       env->define_value( symbol = 'cos' type = lcl_lisp=>type_native value = 'PROC_COS' ).
       env->define_value( symbol = 'tan' type = lcl_lisp=>type_native value = 'PROC_TAN' ).
       env->define_value( symbol = 'asin' type = lcl_lisp=>type_native value = 'PROC_ASIN' ).
       env->define_value( symbol = 'acos' type = lcl_lisp=>type_native value = 'PROC_ACOS' ).
       env->define_value( symbol = 'atan' type = lcl_lisp=>type_native value = 'PROC_ATAN' ).
       env->define_value( symbol = 'sinh' type = lcl_lisp=>type_native value = 'PROC_SINH' ).
       env->define_value( symbol = 'cosh' type = lcl_lisp=>type_native value = 'PROC_COSH' ).
       env->define_value( symbol = 'tanh' type = lcl_lisp=>type_native value = 'PROC_TANH' ).
       env->define_value( symbol = 'asinh' type = lcl_lisp=>type_native value = 'PROC_ASINH' ).
       env->define_value( symbol = 'acosh' type = lcl_lisp=>type_native value = 'PROC_ACOSH' ).
       env->define_value( symbol = 'atanh' type = lcl_lisp=>type_native value = 'PROC_ATANH' ).
       env->define_value( symbol = 'expt' type = lcl_lisp=>type_native value = 'PROC_EXPT' ).
       env->define_value( symbol = 'exp' type = lcl_lisp=>type_native value = 'PROC_EXP' ).
       env->define_value( symbol = 'log' type = lcl_lisp=>type_native value = 'PROC_LOG' ).
       env->define_value( symbol = 'sqrt' type = lcl_lisp=>type_native value = 'PROC_SQRT' ).

       env->define_value( symbol = 'floor'    type = lcl_lisp=>type_native value = 'PROC_FLOOR' ).
       env->define_value( symbol = 'ceiling'  type = lcl_lisp=>type_native value = 'PROC_CEILING' ).
       env->define_value( symbol = 'truncate' type = lcl_lisp=>type_native value = 'PROC_TRUNCATE' ).
       env->define_value( symbol = 'round'    type = lcl_lisp=>type_native value = 'PROC_ROUND' ).

       env->define_value( symbol = 'remainder' type = lcl_lisp=>type_native value = 'PROC_REMAINDER' ).
       env->define_value( symbol = 'modulo'    type = lcl_lisp=>type_native value = 'PROC_MODULO' ).
       env->define_value( symbol = 'quotient'  type = lcl_lisp=>type_native value = 'PROC_QUOTIENT' ).
       env->define_value( symbol = 'random'  type = lcl_lisp=>type_native value   = 'PROC_RANDOM' ).

       env->define_value( symbol = 'zero?'     type = lcl_lisp=>type_native value = 'PROC_IS_ZERO' ).
       env->define_value( symbol = 'positive?' type = lcl_lisp=>type_native value = 'PROC_IS_POSITIVE' ).
       env->define_value( symbol = 'negative?' type = lcl_lisp=>type_native value = 'PROC_IS_NEGATIVE' ).
       env->define_value( symbol = 'odd?'      type = lcl_lisp=>type_native value = 'PROC_IS_ODD' ).
       env->define_value( symbol = 'even?'     type = lcl_lisp=>type_native value = 'PROC_IS_EVEN' ).

*      Native functions for ABAP integration
       env->define_value( symbol = 'ab-data'       type = lcl_lisp=>type_native value   = 'PROC_ABAP_DATA' ).
       env->define_value( symbol = 'ab-function'   type = lcl_lisp=>type_native value   = 'PROC_ABAP_FUNCTION' ).
       env->define_value( symbol = 'ab-table'      type = lcl_lisp=>type_native value   = 'PROC_ABAP_TABLE' ).
       env->define_value( symbol = 'ab-append-row' type = lcl_lisp=>type_native value   = 'PROC_ABAP_APPEND_ROW' ).
       env->define_value( symbol = 'ab-delete-row' type = lcl_lisp=>type_native value   = 'PROC_ABAP_DELETE_ROW' ).
       env->define_value( symbol = 'ab-get-row'    type = lcl_lisp=>type_native value   = 'PROC_ABAP_GET_ROW' ).
       env->define_value( symbol = 'ab-get-value'  type = lcl_lisp=>type_native value   = 'PROC_ABAP_GET_VALUE' ).
       env->define_value( symbol = 'ab-set-value'  type = lcl_lisp=>type_native value   = 'PROC_ABAP_SET_VALUE' ).

       env->define_value( symbol = 'ab-get' type = lcl_lisp=>type_native value = 'PROC_ABAP_GET' ).
       env->define_value( symbol = 'ab-set' type = lcl_lisp=>type_native value = 'PROC_ABAP_SET' ).

       DATA lr_ref TYPE REF TO data.
*      Define a value in the environment for SYST
       GET REFERENCE OF syst INTO lr_ref.
       env->define( symbol = 'ab-sy' element = lcl_lisp=>new_data( lr_ref ) ).
     ENDMETHOD.                    "constructor

     METHOD throw.
       RAISE EXCEPTION TYPE lcx_lisp_exception
         EXPORTING
           message = message
           area    = lcx_lisp_exception=>c_area_eval.
     ENDMETHOD.                    "throw

     METHOD assign_symbol.
       CASE element->car->type.
         WHEN lcl_lisp=>type_symbol.
           environment->define( symbol  = element->car->value
                                element = eval( element = element->cdr->car
                                                environment = environment ) ).
           result = lcl_lisp=>new_symbol( element->car->value ).
*        Function shorthand (define (id arg ... ) body ...+)
         WHEN lcl_lisp=>type_conscell.
*          define's function shorthand allows us to define a function by specifying a list as the
*          first argument where the first element is a symbol and consecutive elements are arguments
           result = lcl_lisp=>new_lambda( io_car = element->car->cdr  "List of params following function symbol
                                          io_cdr = element->cdr
                                          io_env = environment ).
*          Add function to the environment with symbol
           environment->define( symbol  = element->car->car->value
                                element = result ).
*          TODO: Here and above: Scheme does not return a value for define; should we?
           result = lcl_lisp=>new_symbol( element->car->car->value ).
         WHEN OTHERS.
           throw( |{ element->car->to_string( ) } cannot be a variable identifier| ).
       ENDCASE.
     ENDMETHOD.                    "assign_symbol

     METHOD re_assign_symbol.
       result = element->car.
       CASE result->type.
         WHEN lcl_lisp=>type_symbol.
*          re-define symbol in the original environment, but
*          evaluate parameters in the current environment
           environment->find( result->value )->define(
              symbol  = result->value
              element = eval( element = element->cdr->car
                              environment = environment ) ).
         WHEN OTHERS.
           throw( |{ result->to_string( ) } must be a symbol| ).
       ENDCASE.
     ENDMETHOD.                    "re_assign_symbol

     METHOD evaluate_parameters.
*      Before execution of the procedure or lambda, all parameters must be evaluated
       DATA lo_arg TYPE REF TO lcl_lisp.

       ro_args = nil.
       CHECK io_list NE nil AND io_list->car NE nil.

       lo_arg = ro_args = lcl_lisp=>new_cons( ).

       DATA(lo_iter) = lcl_lisp_iterator=>new( io_list ).
       WHILE lo_iter->has_next( ).
         lo_arg->car = eval( element = lo_iter->next( )
                             environment = environment ).
         IF lo_iter->has_next( ) EQ abap_false.
           lo_arg->cdr = nil.
           EXIT.
         ENDIF.
         lo_arg = lo_arg->cdr = lcl_lisp=>new_cons( ).
       ENDWHILE.
     ENDMETHOD.                    "evaluate_parameters

     METHOD eval_function.
*      The function (LAMBDA) receives its own local environment in which to execute,
*      where parameters become symbols that are mapped to the corresponding arguments
       validate io_head.
       DATA(lo_env) = lcl_lisp_environment=>new( io_head->environment ).
       lo_env->parameters_to_symbols( io_args = evaluate_parameters( io_list = io_args           " Pointer to arguments
                                                                     environment = environment )
                                      io_pars = io_head->first( ) ).   " Pointer to formal parameters
       result = evaluate_list( io_head = io_head->rest( )
                               io_env = lo_env ).
     ENDMETHOD.                    "eval_function

     METHOD evaluate_list.
*      Evaluate lambda
       result = nil.
       DATA(lo_iter) = lcl_lisp_iterator=>new( io_head ).
       WHILE lo_iter->has_next( ).
         result = eval( element = lo_iter->next( )
                        environment = io_env ).
       ENDWHILE.
     ENDMETHOD.                    "evaluate_list

     METHOD extract_arguments.
       eo_args = eo_pars = nil.                "list of parameters

       CHECK io_head IS BOUND AND io_head->car IS BOUND AND io_head->car NE nil.
       DATA(lo_ptr) = io_head->car.

       validate lo_ptr->car.
       eo_pars = lcl_lisp=>new_cons( io_car = lo_ptr->car ).
       IF lo_ptr->cdr IS BOUND AND lo_ptr->cdr NE nil.
         eo_args = lcl_lisp=>new_cons( io_car = lo_ptr->cdr->car ).
       ENDIF.
       DATA(lo_par) = eo_pars.
       DATA(lo_arg) = eo_args.
       lo_ptr = io_head.
       WHILE lo_ptr->cdr IS BOUND AND lo_ptr->cdr NE nil.
         lo_ptr = lo_ptr->cdr.
*        Rest of list, pick head
         DATA(lo_pair) = lo_ptr->car.
         IF lo_pair IS BOUND AND lo_pair->car NE nil.
           lo_par = lo_par->cdr = lcl_lisp=>new_cons( io_car = lo_pair->car ).
         ENDIF.
         IF lo_pair->cdr IS BOUND AND lo_pair->cdr NE nil.
           lo_arg = lo_arg->cdr = lcl_lisp=>new_cons( io_car = lo_pair->cdr->car ).
         ENDIF.
       ENDWHILE.
       lo_par->cdr = lo_arg->cdr = nil.

*     Debug help: DATA lv_debug TYPE string.
*      lv_debug = |params { eo_pars->to_string( ) }\n arg { eo_args->to_string( ) }\n|.
     ENDMETHOD.                    "extract_arguments

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
*      Before evaluating the parameter, we create them all with dummy values
       DATA(lo_dummy) = lcl_lisp=>new_string( '*letrec-dummy*' ).

       extract_arguments( EXPORTING io_head = io_head
                          IMPORTING eo_pars = DATA(lo_pars)
                                    eo_args = DATA(lo_args) ).
       DATA(lo_par) = lo_pars.
       DATA(lo_arg) = lo_args.
       WHILE lo_par IS BOUND AND lo_par NE nil   " Nil means no parameters to map
         AND lo_arg IS BOUND AND lo_arg NE nil.  " Nil means no parameters to map

         ro_env->define( symbol = lo_par->car->value
                         element = lo_arg->car ).
         lo_par = lo_par->cdr.
         lo_arg = lo_arg->cdr.
       ENDWHILE.

       DATA(lo_new_args) = evaluate_parameters( io_list = lo_args           " Pointer to arguments
                                                environment = ro_env ).
       ro_env->parameters_to_symbols( io_args = lo_new_args
                                      io_pars = lo_pars ).   " Pointer to formal parameters
     ENDMETHOD.

     METHOD evaluate_in_sequence.
*      Before execution of the procedure or lambda, all parameters must be evaluated
       DATA(lo_args) = lcl_lisp_iterator=>new( io_args ).
       DATA(lo_pars) = lcl_lisp_iterator=>new( io_pars ).

       WHILE lo_args->has_next( ) AND lo_pars->has_next( ).
         DATA(lo_par) = lo_pars->next( ).
         CHECK lo_par NE nil.        " Nil would mean no parameters to map
*        Assign argument to its corresponding symbol in the newly created environment
*        NOTE: element of the argument list is evaluated before being defined in the environment
         io_env->define( symbol = lo_par->value
                         element = eval( element = lo_args->next( )
                                         environment = io_env ) ).
       ENDWHILE.
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
       ro_env = lcl_lisp_environment=>new( io_env ).
       extract_arguments( EXPORTING io_head = io_head
                          IMPORTING eo_pars = DATA(lo_pars)
                                    eo_args = DATA(lo_args) ).

       lo_args = evaluate_parameters( io_list = lo_args           " Pointer to arguments
                                      environment = io_env ).
       ro_env->parameters_to_symbols( io_args = lo_args
                                      io_pars = lo_pars ).   " Pointer to formal parameters
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

**********************************************************************
*
*------------------------------- EVAL( ) ----------------------------
*; eval takes an expression and an environment to a value
*;(define (eval e env) (cond
*;  ((symbol? e)       (cadr (assq e env)))
*;  ((eq? (car e) '#)  (cons e env))
*;  (else              (apply (eval (car e) env) (eval (cadr e) env)))))
**********************************************************************
     METHOD eval.
       validate element.

       CASE element.
*        Return predefined symbols as themselves to save having to look them up in the environment
         WHEN nil OR true OR false.
           result = element.

         WHEN OTHERS.
           CASE element->type.
             WHEN lcl_lisp=>type_number OR lcl_lisp=>type_string.
               result = element.  "Number or string evaluates to itself

             WHEN lcl_lisp=>type_symbol. "Symbol
               result = environment->lookup( element->value ).

*---         ### EVAL LIST
             WHEN lcl_lisp=>type_conscell. "Cons Cell = List
*              To evaluate list, we must first evaluate head value
*              Evaluate first element of list to determine if it is a native procedure or lambda
               DATA(lr_head) = element->car. "Unevaluated value
               DATA(lr_tail) = element->cdr.

               CASE lr_head->value.

                 WHEN 'quote'. " Return the argument to quote unevaluated
                   IF lr_tail->cdr NE nil.
                     throw( |QUOTE can only take a single argument| ).
                   ENDIF.
                   result = lr_tail->car.

                 WHEN 'newline'.
                   result = lcl_lisp=>new_string( |\n| ).

                 WHEN 'display'.
                   result = write( eval( element = lr_tail->car
                                         environment = environment )  ).

                 WHEN 'if'.
                   IF eval( element = lr_tail->car
                            environment = environment  ) NE false.
                     result = eval( element = lr_tail->cdr->car
                                    environment = environment  ).
                   ELSEIF lr_tail->cdr->cdr = nil.
                     result = false.
                   ELSE.
                     result = eval( element = lr_tail->cdr->cdr->car
                                    environment = environment ).
                   ENDIF.

                 WHEN 'and'.
                   result = true.
                   DATA(lo_ptr) = lr_tail.
                   WHILE lo_ptr IS BOUND AND lo_ptr NE nil AND result NE false.
                     result = eval( element = lo_ptr->car
                                    environment = environment ).
                     lo_ptr = lo_ptr->cdr.
                   ENDWHILE.

                 WHEN 'or'.
                   result = false.
                   lo_ptr = lr_tail.
                   WHILE lo_ptr IS BOUND AND lo_ptr NE nil AND result EQ false.
                     result = eval( element = lo_ptr->car
                                    environment = environment ).
                     lo_ptr = lo_ptr->cdr.
                   ENDWHILE.

                 WHEN 'cond'.
                   lo_ptr = lr_tail.
                   WHILE lo_ptr NE nil.
                     DATA(lo_clause) = lo_ptr->car.
                     IF lo_clause->car->value EQ 'else'
                        OR eval( element = lo_clause->car
                                 environment = environment ) NE false.
                       result = evaluate_list( io_head = lo_clause->cdr
                                               io_env = environment ).
                       EXIT.
                     ENDIF.
                     lo_ptr = lo_ptr->cdr.
                   ENDWHILE.

                 WHEN 'define'.
                   result = assign_symbol( element = lr_tail
                                           environment = environment ).

                 WHEN 'set!'.                        " Re-Assign symbol
                   result = re_assign_symbol( element     = lr_tail
                                              environment = environment ).

                 WHEN 'let'.
*                  (let ((x 10) (y 5)) (+ x y)) is syntactic sugar for  ( (lambda (x y) (+ x y)) 10 5)
                   result = evaluate_list( io_head = lr_tail->cdr
                                           io_env = init_named_let( io_head = lr_tail->car
                                                                    io_env = environment ) ).

                 WHEN 'letrec'.
*                  (letrec ((a 5) (b (+ a 3)) b)
                   result = evaluate_list( io_head = lr_tail->cdr
                                           io_env = init_letrec( io_head = lr_tail->car
                                                                 io_env = environment ) ).

                 WHEN 'let*'.
                   result = evaluate_list( io_head = lr_tail->cdr
                                           io_env = init_let_star( io_head = lr_tail->car
                                                                   io_env = environment ) ).

                 WHEN 'lambda'.
                   result = lcl_lisp=>new_lambda( io_car = lr_tail->car         " List of parameters
                                                  io_cdr = lr_tail->cdr         " Body
                                                  io_env = environment ).
                 WHEN 'begin'.
                   result = evaluate_list( io_head = lr_tail
                                           io_env = environment ).

                 WHEN 'read'.
                   result = read( lr_tail ).

                 WHEN OTHERS.
*---           NATIVE PROCEDURES AND LAMBDAS
*              Other symbols at the start of the list must be evaluated first
*              The evaluated head must be either a native procedure or lambda
                   lr_head = eval( element = element->car
                                   environment = environment ).

                   CASE lr_head->type.

                     WHEN lcl_lisp=>type_native.
*---               NATIVE FUNCTION
*                  Evaluate native function:
                       CALL METHOD (lr_head->value)
                         EXPORTING
                           list   = evaluate_parameters( io_list = lr_tail
                                                         environment = environment )
                         RECEIVING
                           result = result.

                     WHEN lcl_lisp=>type_lambda.
                       result = eval_function( io_head = lr_head
                                               io_args = lr_tail
                                               environment = environment ).

                     WHEN lcl_lisp=>type_abap_function.
*>>> TEST:         Support evaluation of ABAP function directly
*                    Recompose as if calling a PROC (which we are). This is part of the test. If we make an ABAP function
*                    call first-class, then we would need to revisit evaluating the whole of ELEMENT in one shot
                       result = proc_abap_function_call( lcl_lisp=>new_cons( io_car = lr_head
                                                                             io_cdr = lr_tail ) ).
*<<< TEST
                     WHEN OTHERS.
                       throw( |Cannot evaluate { lr_head->to_string( ) } - not a function| ).

                   ENDCASE.

               ENDCASE.

           ENDCASE.

       ENDCASE.
       assert_is_bound result `EVAL( ) came up empty-handed` .
     ENDMETHOD.                    "eval

     METHOD write.
       mi_port->write( io_elem ).
       result = lcl_lisp=>new_string( io_elem->to_string( ) ).
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
           lo_conscell = lo_conscell->cdr = lcl_lisp=>new_cons( io_car = lo_elem
                                                                io_cdr = nil ).
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
                                environment = env )->to_string( ).
         response = COND #( WHEN response IS INITIAL
                            THEN lv_value
                            ELSE |{ response } { lv_value }| ).
       ENDLOOP.
     ENDMETHOD.                    "eval_source

**********************************************************************
* NATIVE PROCEDURES
**********************************************************************
     METHOD proc_append.
*      All parameters execpt the last must be lists, the last must be
*      a cons cell. Creates a new list appending all parameters

*      But if the last element in the list is not a cons cell, we cannot append
       result = nil.
       CHECK list IS BOUND.
       validate list->car.
       result = list->car.

       CHECK list->cdr IS BOUND AND list->cdr NE nil.
       validate list->cdr->car.
       result = list->cdr->car.

       CHECK list->car NE nil.
       DATA(lo_iter) = lcl_lisp_iterator=>new( proc_reverse( list ) ).
       WHILE lo_iter->has_next( ).
         result = lcl_lisp=>new_cons( io_car = lo_iter->next( )
                                      io_cdr = result ).
       ENDWHILE.
     ENDMETHOD.                    "proc_append

     METHOD proc_reverse.
*      But if the last element in the list is not a cons cell, we cannot append
       validate: list, list->car.

       result = nil.
       DATA(iter) = lcl_lisp_iterator=>new( list->car ).
       WHILE iter->has_next( ).
         result = lcl_lisp=>new_cons( io_car = iter->next( )
                                      io_cdr = result ).
       ENDWHILE.
     ENDMETHOD.                    "proc_reverse

     METHOD proc_append_unsafe.  " append! (non functional)
*      Takes two parameters: the first must be a list, and the second can
*      be of any type. Appends the second param to the first.

*      But if the last element in the list is not a cons cell, we cannot append
       validate: list, list->car, list->cdr.

       IF list->car EQ nil.
         result = list->cdr->car.
       ELSE.
*        Get to last element in list - this can make APPEND expensive, like LENGTH
         DATA(lo_last) = list->car.
         IF lo_last->type NE lcl_lisp=>type_conscell.
           throw( |{ lo_last->to_string( ) } is not a list| ).
         ENDIF.

         WHILE lo_last->cdr IS BOUND AND lo_last->cdr NE nil.
           lo_last = lo_last->cdr.
         ENDWHILE.

         IF lo_last->type NE lcl_lisp=>type_conscell.
*          If the last item is not a cons cell, return an error
           throw( |{ list->car->to_string( ) } is not a proper list| ).
         ENDIF.

*        Last item is a cons cell; tack on the new value
         lo_last->cdr = list->cdr->car.
         result = list->car.
       ENDIF.
     ENDMETHOD.                    "proc_append_unsafe

     METHOD proc_car.
       validate: list, list->car.
       IF list->car = nil.
         result = nil.
         RETURN.
       ENDIF.
       result = list->car->car.
     ENDMETHOD.                    "proc_car

     METHOD proc_cdr.
       validate: list, list->car, list->cdr.

       IF list->cdr = nil AND list->car = nil.
         result = nil.
         RETURN.
       ENDIF.
       result = list->car->cdr.
     ENDMETHOD.                    "proc_cdr

     METHOD proc_cons.
*      Create new cell and prepend it to second parameter
       validate: list, list->car, list->cdr.

       result = lcl_lisp=>new_cons( io_car = list->car
                                    io_cdr = list->cdr->car ).
     ENDMETHOD.                    "proc_cons

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
         throw( |LIST takes only one argument| ).
       ENDIF.

       result = lcl_lisp=>new_number( 0 ).
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
       throw( |{ list->car->to_string( ) } is not a proper list| ).
     ENDMETHOD.                    "proc_length

     METHOD proc_list.
*      The items given to us are already in a list and evaluated; we just need to return the head
       result = list.
     ENDMETHOD.                    "proc_list

     METHOD proc_nilp.
       validate: list, list->car.
       result = COND #( WHEN list->car = nil THEN true ELSE false ).
     ENDMETHOD.                    "proc_nilp

* TO BE IMPLEMENTED
     METHOD proc_memq.
       validate: list, list->car.

       result = false.
       DATA(lo_ref) = list->car.
       CHECK list->cdr IS BOUND.

       DATA(lo_ptr) = list->cdr->car.
       WHILE lo_ptr IS BOUND AND lo_ptr NE nil AND lo_ptr->car->type EQ lo_ref->type.

         CASE lo_ref->type.
           WHEN lcl_lisp=>type_number.
             IF lo_ref->number = lo_ptr->car->number.
               result = lo_ptr.
               RETURN.
             ENDIF.

           WHEN lcl_lisp=>type_symbol OR lcl_lisp=>type_string.
             IF lo_ref->value = lo_ptr->car->value.
               result = lo_ptr.
               RETURN.
             ENDIF.

           WHEN OTHERS.
             IF lo_ref = lo_ptr->car.
               result = lo_ptr.
               RETURN.
             ENDIF.
         ENDCASE.

         lo_ptr = lo_ptr->cdr.
       ENDWHILE.
     ENDMETHOD.

     METHOD proc_memv.
       validate: list, list->car.

       result = false.
       DATA(lo_ref) = list->car.
       CHECK list->cdr IS BOUND.
       DATA(lo_ptr) = list->cdr->car.
       WHILE lo_ptr IS BOUND AND lo_ptr NE nil.
         IF proc_equivalence( a = lo_ptr->car
                              b = lo_ref ) NE false.
           result = lo_ptr.
           RETURN.
         ENDIF.
         lo_ptr = lo_ptr->cdr.
       ENDWHILE.
     ENDMETHOD.

     METHOD proc_member.
       validate: list, list->car.

       result = false.
       DATA(lo_ref) = list->car.
       CHECK list->cdr IS BOUND.
       DATA(lo_ptr) = list->cdr->car.
       WHILE lo_ptr IS BOUND AND lo_ptr NE nil.
         IF proc_compare( a = lo_ptr->car
                          b = lo_ref ) NE false.
           result = lo_ptr.
           RETURN.
         ENDIF.
         lo_ptr = lo_ptr->cdr.
       ENDWHILE.
     ENDMETHOD.

     METHOD proc_assq.
       throw( |In implementation| ).
       validate: list, list->car, list->cdr.
     ENDMETHOD.

     METHOD proc_assv.
       throw( |In implementation| ).
       validate: list, list->car, list->cdr.
     ENDMETHOD.

     METHOD proc_assoc.
       throw( |In implementation| ).
       validate: list, list->car, list->cdr.
     ENDMETHOD.

**********************************************************************

     METHOD proc_add.
       DATA(iter) = lcl_lisp_iterator=>new( list ).
       result = lcl_lisp=>new_number( 0 ).

       WHILE iter->has_next( ).
         DATA(cell) = iter->next( ).
         validate_number cell '[+]'.
         ADD cell->number TO result->number.
       ENDWHILE.
     ENDMETHOD.                    "proc_add

     METHOD proc_subtract.
       DATA(iter) = lcl_lisp_iterator=>new( list ).
       DATA(cell) = iter->next( ).
       validate cell.
       result = lcl_lisp=>new_number( cell->number ).

       IF iter->has_next( ) EQ abap_false.
         result->number = 0 - result->number.
       ELSE.
*        Subtract all consecutive numbers from the first
         WHILE iter->has_next( ).
           cell = iter->next( ).
           validate_number cell '[-]'.
           result->number = result->number - cell->number.
         ENDWHILE.
       ENDIF.
     ENDMETHOD.                    "proc_subtract

     METHOD proc_multiply.
       DATA(iter) = lcl_lisp_iterator=>new( list ).
       DATA(cell) = iter->next( ).
       validate cell.
       result = lcl_lisp=>new_number( cell->number ).

       WHILE iter->has_next( ).
         cell = iter->next( ).
         validate_number cell '[*]'.
         result->number = result->number * cell->number.
       ENDWHILE.
     ENDMETHOD.                    "proc_multiply

     METHOD proc_divide.
       DATA(iter) = lcl_lisp_iterator=>new( list ).
       DATA(cell) = iter->next( ).
       validate cell.
       result = lcl_lisp=>new_number( cell->number ).

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

       IF ( a EQ true AND b EQ true )
         OR ( a EQ false AND b EQ false )
         OR ( a EQ nil AND b EQ nil ).
         result = true.
         RETURN.
       ENDIF.

       CHECK a->type EQ b->type.

       CASE a->type.
         WHEN lcl_lisp=>type_number.
           CHECK a->number = b->number.
         WHEN lcl_lisp=>type_symbol OR lcl_lisp=>type_string.
           CHECK a->value = b->value.
         WHEN lcl_lisp=>type_conscell OR lcl_lisp=>type_lambda.
           CHECK a->car EQ b->car AND a->cdr EQ b->cdr.
         WHEN OTHERS.
           CHECK a = b.
       ENDCASE.
       result = true.
     ENDMETHOD.

     METHOD proc_compare.
       validate: a, b.

       result = false.
       CHECK a->type EQ b->type.

       CASE a->type.
         WHEN lcl_lisp=>type_number.
           CHECK a->number = b->number.
         WHEN lcl_lisp=>type_symbol OR lcl_lisp=>type_string.
           CHECK a->value = b->value.
         WHEN lcl_lisp=>type_conscell OR lcl_lisp=>type_lambda.
           CHECK proc_compare( a = a->car
                               b = b->car ) NE false
             AND proc_compare( a = a->cdr
                               b = b->cdr ) NE false.
         WHEN OTHERS.
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

         result = proc_compare( a = lo_next
                                b = lo_ptr->car ).
         IF result EQ false.
           EXIT.
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
           EXIT.
         ENDIF.
         lo_ptr = lo_ptr->cdr.
       ENDWHILE.

     ENDMETHOD.

*--------------------------------------------------------------------*
*    Hash-related functions
     METHOD proc_make_hash.
       result = lcl_lisp_hash=>new_hash( list ).
     ENDMETHOD.                    "proc_make_hash

*    Get an element from a hash
     METHOD proc_hash_get.
       result = lcl_lisp_hash=>from_list( list = list
                                          msg = 'HASH-GET' )->get( list->cdr ).
     ENDMETHOD.                    "proc_hash_get

*    Insert an element into a hash
     METHOD proc_hash_insert.
       result = lcl_lisp_hash=>from_list( list = list
                                          msg = 'HASH-INSERT' )->insert( list->cdr ).
     ENDMETHOD.                    "proc_hash_insert

*    Remove an element from a hash
     METHOD proc_hash_remove.
       result = lcl_lisp_hash=>from_list( list = list
                                          msg = 'HASH-REMOVE' )->delete( list->cdr ).
     ENDMETHOD.                    "proc_hash_delete

*    Return the keys of a hash
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

     METHOD proc_is_symbol.
       _is_type symbol.
     ENDMETHOD.

     METHOD proc_is_list.
       result = false.
       CHECK list IS BOUND.

       DATA(lo_ptr) = list->car.
       WHILE lo_ptr->cdr IS BOUND AND lo_ptr->cdr NE nil.
         lo_ptr = lo_ptr->cdr.
       ENDWHILE.
       CHECK ( list EQ nil OR lo_ptr EQ nil )
         OR ( lo_ptr IS BOUND AND lo_ptr->cdr EQ nil AND lo_ptr->type = lcl_lisp=>type_conscell ).
       result = true.
     ENDMETHOD.                    "proc_is_list

     METHOD proc_is_procedure.
       result = false.
       CHECK list IS BOUND        " paramater (car) must not be valid
         AND list->car IS BOUND.  " Body
       CASE list->car->type.
         WHEN lcl_lisp=>type_lambda OR lcl_lisp=>type_native.
           result = true.
       ENDCASE.
     ENDMETHOD.

     METHOD proc_is_alist.
       result = proc_is_list( list ).
       throw( |Not supported yet ALIST?| ).
     ENDMETHOD.                    "proc_is_alist

     METHOD proc_is_type.
       throw( |Not supported yet IS_TYPE?| ).
     ENDMETHOD.                    "proc_is_type

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
       _trigonometric cosh '[acosh]'.
     ENDMETHOD.                    "proc_cosh

     METHOD proc_tanh.
       _trigonometric tanh '[atanh]'.
     ENDMETHOD.                    "proc_tanh

     METHOD proc_asinh.
       DATA carry TYPE f.

       result = nil.
       validate: list, list->car.
       validate_number list->car '[asinh]'.
       _is_last_param list.
       TRY.
           carry = list->car->number.
           result = lcl_lisp=>new_number( log( carry + sqrt( carry ** 2 + 1 ) ) ).
           _catch_arithmetic_error.
       ENDTRY.
     ENDMETHOD.                    "proc_asinh

     METHOD proc_acosh.
       DATA carry TYPE f.

       result = nil.
       validate: list, list->car.
       validate_number list->car '[acosh]'.
       _is_last_param list.
       TRY.
           carry = list->car->number.
           result = lcl_lisp=>new_number( log( carry + sqrt( carry ** 2 - 1 ) ) ).
           _catch_arithmetic_error.
       ENDTRY.
     ENDMETHOD.                    "proc_acosh

     METHOD proc_atanh.
       DATA carry TYPE f.

       result = nil.
       validate: list, list->car.
       validate_number list->car '[atanh]'.
       _is_last_param list.
       TRY.
           carry = list->car->number.
           result = lcl_lisp=>new_number( ( log( 1 + carry ) - log( 1 - carry ) ) / 2 ).
           _catch_arithmetic_error.
       ENDTRY.
     ENDMETHOD.                    "proc_atanh

     METHOD proc_expt.
       result = nil.
       validate: list, list->car, list->cdr.
       validate_number: list->car '[expt]',
                        list->cdr->car '[expt]'.
       _is_last_param list->cdr.
       TRY.
           result = lcl_lisp=>new_number( list->car->number ** list->cdr->car->number ).
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
       validate: list, list->car.
       validate_number list->car '[round]'.
       _is_last_param list.
       TRY.
           result = lcl_lisp=>new_number( round( val = list->car->number dec = 0 ) ).
           _catch_arithmetic_error.
       ENDTRY.
     ENDMETHOD.                    "proc_round

     METHOD proc_remainder.
       result = nil.
       validate: list, list->car, list->cdr.
       validate_number: list->car '[remainder]',
                        list->cdr->car '[remainder]'.
       _is_last_param list->cdr.
       TRY.
           result = lcl_lisp=>new_number( list->car->number -
             list->cdr->car->number * trunc( list->car->number / list->cdr->car->number ) ).
           _catch_arithmetic_error.
       ENDTRY.
     ENDMETHOD.                    "proc_remainder

     METHOD proc_modulo.
       result = nil.
       validate: list, list->car, list->cdr.
       validate_number: list->car '[modulo]',
                        list->cdr->car '[modulo]'.
       _is_last_param list->cdr.
       TRY.
           result = lcl_lisp=>new_number( list->car->number MOD list->cdr->car->number ).
           IF sign( list->cdr->car->number ) LE 0.
             result->number = result->number + list->cdr->car->number.
           ENDIF.
           _catch_arithmetic_error.
       ENDTRY.
     ENDMETHOD.                    "proc_modulo

     METHOD proc_random.
       result = nil.
       validate: list, list->car.
       validate_integer: list->car '[random]'.
       _is_last_param list.
       TRY.
           DATA(lo_rnd) = cl_abap_random=>create( cl_abap_random=>seed( ) ).
           result = lcl_lisp=>new_number( lo_rnd->intinrange( high = CONV i( list->car->number ) ) ).
         CATCH cx_dynamic_check INTO DATA(lx_error).
           throw( lx_error->get_text( ) ).
       ENDTRY.
     ENDMETHOD.                    "proc_random

     METHOD proc_quotient.
       result = nil.
       validate: list, list->car, list->cdr.
       validate_number: list->car '[quotient]',
                        list->cdr->car '[quotient]'.
       _is_last_param list->cdr.
       TRY.
           result = lcl_lisp=>new_number( list->car->number DIV list->cdr->car->number ).
           _catch_arithmetic_error.
       ENDTRY.
     ENDMETHOD.                    "proc_quotient

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
         throw( |AB-DATA: String or symbol required as name of type| ).
       ENDIF.

       cl_abap_typedescr=>describe_by_name( EXPORTING p_name = list->car->value
                                            RECEIVING p_descr_ref = DATA(lr_desc)
                                            EXCEPTIONS OTHERS = 1 ).
       IF sy-subrc NE 0.
         throw( |AB-DATA: Type { list->car->value } not found | ).
       ENDIF.

       CASE lr_desc->kind.
         WHEN cl_abap_typedescr=>kind_table.
           result = lcl_lisp=>new_table( ).
         WHEN cl_abap_typedescr=>kind_elem OR cl_abap_typedescr=>kind_struct.
           result = lcl_lisp=>new_data( ).
         WHEN OTHERS.
           throw( |AB-DATA: Type kind { lr_desc->kind } not supported yet| ).
       ENDCASE.
*      Create data as given type
       CREATE DATA result->data TYPE (list->car->value).
*      Set value if supplied as second parameter
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
       result = lcl_lisp_abapfunction=>new_function( list ).
     ENDMETHOD.                    "proc_abap_function

     METHOD proc_abap_table. "Create a table data
       validate: list, list->car.
*      First input: name of data type, second input: value
       result = lcl_lisp=>new_table( ).
       CREATE DATA result->data TYPE TABLE OF (list->car->value).
*      Set value if supplied as second parameter
       IF list->cdr NE nil.
         element_to_data( EXPORTING element = list->cdr->car
                          CHANGING data    = result->data ).
       ENDIF.
     ENDMETHOD.                    "proc_abap_table

**********************************************************************
     METHOD proc_abap_append_row.
     ENDMETHOD.                    "proc_abap_append_row

     METHOD proc_abap_delete_row.
     ENDMETHOD.                    "proc_abap_delete_row

     METHOD proc_abap_get_row.
     ENDMETHOD.                    "proc_abap_get_row

**********************************************************************
     METHOD proc_abap_get_value. "Convert ABAP to Lisp data
       FIELD-SYMBOLS <data> TYPE any.

       validate: list, list->car.
       DATA(lo_ptr) = list->car.
       IF lo_ptr->type NE lcl_lisp=>type_abap_data AND
          lo_ptr->type NE lcl_lisp=>type_abap_table.
         throw( |AB-GET-VALUE requires ABAP data or table as parameter| ).
       ENDIF.
       TRY.
           ASSIGN lo_ptr->data->* TO <data>.
           result = data_to_element( <data> ).
         CATCH cx_root INTO DATA(lx_root).
           throw( |Mapping error: { lx_root->get_text( ) }| ).
       ENDTRY.
     ENDMETHOD.                    "proc_abap_get_value

     METHOD proc_abap_set_value. "Convert Lisp to ABAP data
       FIELD-SYMBOLS <data> TYPE any.

       validate: list, list->car.
       IF list->car->type NE lcl_lisp=>type_abap_data AND
          list->car->type NE lcl_lisp=>type_abap_table.
         throw( |AB-SET-VALUE requires ABAP data or table as first parameter| ).
       ENDIF.
       TRY.
           ASSIGN list->car->data->* TO <data>.
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

**********************************************************************
     METHOD proc_abap_function_call. "Called internally only for execution of function module
       validate: list, list->car.
*      The first parameter must be a function module instance
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
*      Perform RTTI on determined data and generate appropriate response
       DATA(lv_kind) = cl_abap_typedescr=>describe_by_data_ref( ir_data )->kind.
       CASE lv_kind.
         WHEN cl_abap_typedescr=>kind_table.
           result = lcl_lisp=>new_table( ir_data ).
         WHEN cl_abap_typedescr=>kind_struct.
           result = lcl_lisp=>new_data( ir_data ).
         WHEN cl_abap_typedescr=>kind_elem.
*          Give back immediate value
           FIELD-SYMBOLS <value> TYPE any.

           ASSIGN ir_data->* TO <value>.
           result = data_to_element( <value> ).
         WHEN OTHERS.
           throw( |AB-GET: Type kind { lv_kind } not supported yet| ). "Can do AB-TAB-WHERE some other time
       ENDCASE.
     ENDMETHOD.                    "create_element_from_data

     METHOD proc_abap_get.
*      Ensure a valid first parameter is passed
       IF list->car->type NE lcl_lisp=>type_abap_data
         AND list->car->type NE lcl_lisp=>type_abap_function
         AND list->car->type NE lcl_lisp=>type_abap_table.
         throw( |AB-GET: First parameter must be ABAP data or table or a function| ).
       ENDIF.

*      Determine whether the data is elementary or not to decide if we need to get the element by identifier
       IF list->car->data IS NOT INITIAL AND
         cl_abap_typedescr=>describe_by_data_ref( list->car->data )->kind = cl_abap_typedescr=>kind_elem.
*        Elementary type; can return the value without mapping
         DATA(lr_data) = list->car->data.
       ELSE.
*        Could short-cut here and provide the value right away
         IF list->cdr = nil.
           throw( |AB-GET: Complex type requires identifier for lookup| ).
         ELSE.
           lr_data = get_element( list ).
         ENDIF.
       ENDIF.

       result = create_element_from_data( lr_data ).

     ENDMETHOD. "proc_abap_get

     METHOD proc_abap_set.
       FIELD-SYMBOLS <target> TYPE any.
       FIELD-SYMBOLS <source> TYPE any.

*      Ensure a valid first parameter is passed
       IF list->car->type NE lcl_lisp=>type_abap_data
          AND list->car->type NE lcl_lisp=>type_abap_function
          AND list->car->type NE lcl_lisp=>type_abap_table.
         throw( |AB-SET: First parameter must be ABAP data or table or a function| ).
       ENDIF.

*      Determine whether the data is elementary or not to decide if we need to get the element by identifier
       IF list->car->data IS NOT INITIAL AND cl_abap_typedescr=>describe_by_data_ref( list->car->data )->kind = cl_abap_typedescr=>kind_elem.
*        Elementary type; can return the value without mapping
         DATA(lr_target) = list->car->data.
         DATA(lo_source) = list->cdr->car.
*        lo_sdata = list->cdr->car->data. "Value to set is second argument
       ELSEIF list->cdr = nil.
         throw( |AB-SET: Complex type requires identifier for lookup| ).
       ELSE.
         lr_target = get_element( list ).
*       lr_sdata = list->cdr->cdr->car->data. "Value to set is third argument
         lo_source = list->cdr->cdr->car.
       ENDIF.

* Do we just assign the reference now? Probably should dereference source value
* and copy the value...
*      Perform RTTI on determined data and generate appropriate response
       ASSIGN lr_target->* TO <target>.

*      For elementary types, set value from second parameter, otherwise third
       IF cl_abap_typedescr=>describe_by_data( <target> )->kind = cl_abap_typedescr=>kind_elem.
*        For now, we will support setting data from a number, string or symbol
         CASE lo_source->type.
           WHEN lcl_lisp=>type_string OR lcl_lisp=>type_symbol.
             <target> = lo_source->value.
           WHEN lcl_lisp=>type_number.
             <target> = lo_source->number.
         ENDCASE.
       ELSE.
*        Complex types will just copy the whole value across
         ASSIGN lo_source->data->* TO <source>.
         <target> = <source>.                        "Set the value
       ENDIF.

       result = nil.

     ENDMETHOD. "proc_abap_set

     METHOD structure_to_element.
       DATA lo_conscell TYPE REF TO lcl_lisp. " Lisp-side (target)
       FIELD-SYMBOLS <field> TYPE any.

       lo_conscell = element = lcl_lisp=>new_cons( ).
       DO.
         ASSIGN COMPONENT sy-index OF STRUCTURE struct TO <field>.
         IF sy-subrc NE 0.
           lo_conscell->cdr = nil. "Terminate list
           EXIT.
         ENDIF.
         IF sy-index > 1.          "Move pointer only from second field onward
           lo_conscell = lo_conscell->cdr = lcl_lisp=>new_cons( ).
         ENDIF.
         lo_conscell->car = data_to_element( <field> ).
       ENDDO.
     ENDMETHOD.                    "structure_to_element

     METHOD data_to_element.
*      Map ABAP Data to Lisp element
       FIELD-SYMBOLS <table> TYPE ANY TABLE.          " ABAP-side (source)
       DATA line TYPE REF TO data.

*      Determine type of the ABAP value
       DATA(lr_ddesc) = cl_abap_typedescr=>describe_by_data( data ).
       CASE lr_ddesc->kind.

         WHEN cl_abap_typedescr=>kind_table.
*          Table type
           FIELD-SYMBOLS <line> TYPE any.

           ASSIGN data TO <table>.
           CREATE DATA line LIKE LINE OF <table>.
           ASSIGN line->* TO <line>.

           IF <table> IS INITIAL.
             element = nil.
           ELSE.
             DATA lo_conscell TYPE REF TO lcl_lisp. " Lisp-side  (target)

*            Create list with cell for each row AND Set pointer to start of list
             lo_conscell = element = lcl_lisp=>new_cons( ).
             LOOP AT <table> INTO <line>.
               IF sy-tabix > 1.          "Move pointer only from second line onward
                 lo_conscell = lo_conscell->cdr = lcl_lisp=>new_cons( ).
               ENDIF.
               lo_conscell->car = data_to_element( <line> ).
             ENDLOOP.
             lo_conscell->cdr = nil.     "Terminate list
           ENDIF.

         WHEN cl_abap_typedescr=>kind_struct.
           element = structure_to_element( data ).

         WHEN cl_abap_typedescr=>kind_elem.
*          Elementary type
           element = SWITCH #( lr_ddesc->type_kind
                        WHEN cl_abap_typedescr=>typekind_numeric OR cl_abap_typedescr=>typekind_num
                        THEN lcl_lisp=>new_number( data )
                        ELSE lcl_lisp=>new_string( data ) ).
       ENDCASE.
     ENDMETHOD.                    "data_to_element

*    Map Lisp element to ABAP Data
     METHOD element_to_data.
*      ABAP-side (target) mapping:
       FIELD-SYMBOLS <field> TYPE any.
       FIELD-SYMBOLS <line> TYPE any.
       FIELD-SYMBOLS <table> TYPE ANY TABLE.
       FIELD-SYMBOLS <sotab> TYPE SORTED TABLE.
       FIELD-SYMBOLS <sttab> TYPE STANDARD TABLE.

       DATA line TYPE REF TO data.
       DATA table TYPE REF TO data.

*      Determine type of the ABAP value
       DATA(lr_ddesc) = cl_abap_typedescr=>describe_by_data( data ).
       CASE lr_ddesc->kind.
*        Table type
         WHEN cl_abap_typedescr=>kind_table.
*          For this mapping to happen, the element must be a cons cell
           IF element->type NE lcl_lisp=>type_conscell.
             throw( |Mapping failed: Non-cell to table| ).
           ENDIF.
*          Provide reference to table and line
           table = REF #( data ).
           ASSIGN table->* TO <table>.
           CASE CAST cl_abap_tabledescr( lr_ddesc )->table_kind.
             WHEN cl_abap_tabledescr=>tablekind_sorted OR
                  cl_abap_tabledescr=>tablekind_hashed.
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
*          Structure
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
*            Don't map nil values
             CHECK lr_conscell->car NE nil.

             element_to_data( EXPORTING element = lr_conscell->car
                              CHANGING data    = <field> ).
           ENDDO.

         WHEN cl_abap_typedescr=>kind_elem.
*          Elementary type
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
         throw( `AB-GET: String or symbol required to access structure field` ).
       ENDIF.

       ASSIGN element->data->* TO <struct>.
       ASSIGN COMPONENT identifier->value OF STRUCTURE <struct> TO <value>.
       IF sy-subrc NE 0.
         throw( |AB-GET: Structure has no component { identifier->value }| ).
       ENDIF.
       rdata = REF #( <value> ).
     ENDMETHOD.                    "get_structure_field

     METHOD get_index_table_row.
*      Second input for reading an index table must be a number (row index)
       FIELD-SYMBOLS <idxtab> TYPE INDEX TABLE.

       IF identifier = nil OR identifier->type NE lcl_lisp=>type_number.
         throw( |AB-GET: Numeric index required to read index table| ). "Can do AB-TAB-WHERE some other time
       ENDIF.
       ASSIGN element->data->* TO <idxtab>.
       TRY.
           rdata = REF #( <idxtab>[ identifier->number ] ).
         CATCH cx_sy_itab_line_not_found.
           throw( |AB-GET: No entry at index { identifier->number }| ). "Can do AB-TAB-WHERE some other time
       ENDTRY.
     ENDMETHOD.                    "get_index_table_row

     METHOD get_table_row_with_key.
*      Read with key, which is a bit more effort
       FIELD-SYMBOLS <wa> TYPE any.
       FIELD-SYMBOLS <tab> TYPE table.
       DATA line TYPE REF TO data.

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
*        Get function parameter by name
         rdata = CAST lcl_lisp_abapfunction( element )->get_function_parameter( identifier ).
       ELSE.
*        First parameter is not function, but table or other data; examine the data
         DATA(lo_ddesc) = cl_abap_typedescr=>describe_by_data_ref( element->data ).

         CASE lo_ddesc->kind.
           WHEN cl_abap_typedescr=>kind_struct.
*            Structure: Use second parameter as field name
             rdata = get_structure_field( element = element
                                          identifier = identifier ).
           WHEN cl_abap_typedescr=>kind_elem.
*            Elementary data: No qualifier / second parameter required
             rdata = element->data.

           WHEN cl_abap_typedescr=>kind_table.
*            Table: Second parameter is index (std table) or key (sorted table)
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

*       DATA lv_is_callable TYPE flag.
*       CALL FUNCTION 'CHECK_EXIST_LIMU_FUNC'    " Not released - (Step not needed)
*         EXPORTING
*           name                  = function_name
**          AGGNAME               = ' '
**          MTYPE                 = ' '
*         IMPORTING
*           EXIST                 = lv_is_callable
*         EXCEPTIONS
**          TR_INVALID_TYPE       = 1
*           OTHERS                = 2.
*       IF sy-subrc <> 0.
*         throw( |Function { function_name }: { error_message( ) }| ).
*       ENDIF.


*      Read the function module interface
       CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
         EXPORTING
           funcname           = function_name  " Name of the function module
           with_enhancements  = 'X'            " X = Enhancement Parameters Will Be Provided
*          ignore_switches    = SPACE           " X = Switches Are Ignored
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

     METHOD new_function.
       validate: list, list->car.

       ro_func ?= new( type_abap_function ).
*      Determine the parameters of the function module to populate parameter table
       ro_func->value = ro_func->read_interface( list->car->value ).
*(let (( profiles
*        (let ( (f3 (ab-function "BAPI_USER_GET_DETAIL"))  )
*        ( begin (ab-set f3 "USERNAME" (ab-get ab-sy "UNAME") )
*                  (f3) (ab-get f3 "PROFILES")  ) )
*        ) )
*   (let ((profile (ab-get profiles 1)) )
*             (ab-get profile "BAPIPROF" )  )
     ENDMETHOD.                    "new_function
*(define bapi-userdetail (ab-function "BAPI_USER_GET_DETAIL"))  ;; Assign interface of BAPI_USER_GET_DETAIL to a symbol
*(ab-set bapi-userdetail "USERNAME" (ab-get ab-sy "UNAME"))     ;; Set parameter "USERNAME" to current user

     METHOD call.
       create_parameters( ).
*TODO: Map given list to parameters of function module
*      First parameter: Name of function to call;
*      second parameter: data to pass to interface
       CALL FUNCTION list->value
         PARAMETER-TABLE parameters
         EXCEPTION-TABLE exceptions.

       IF sy-subrc EQ c_error_message.
         throw( |Call { list->value }: { error_message( ) }| ).
       ENDIF.
*    Map output parameters to new list
       ro_elem = list.      "Function reference is updated with values after call
     ENDMETHOD.                    "call

     METHOD error_message.
       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno    "#EC *
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO rv_message.
     ENDMETHOD.

     METHOD get_function_parameter.
*      Get function parameter by name
*      IDENTIFIER -> Lisp element, string or symbol or index, to identify subcomponent of value
       IF identifier = nil OR
         ( identifier->type NE type_string AND identifier->type NE type_symbol ).
         throw( `AB-GET: String or symbol required to access function parameter` ).
       ENDIF.

       create_parameters( ).

       TRY.
           rdata = parameters[ name = identifier->value ]-value.
         CATCH cx_sy_itab_line_not_found.
           throw( |AB-GET: No parameter { identifier->value } in function| ).
       ENDTRY.
     ENDMETHOD.                    "get_function_parameter

     METHOD create_table_params.
*      Create structures in parameter - TABLES
       LOOP AT it_table INTO DATA(ls_table).
         DATA(ls_par) = VALUE abap_func_parmbind( kind = abap_func_tables
                                                  name = ls_table-parameter ).

         DATA(lv_type) = COND rs38l_typ( WHEN ls_table-typ IS INITIAL THEN ls_table-dbstruct ELSE ls_table-typ ).
         CREATE DATA ls_par-value TYPE TABLE OF (lv_type).
         CREATE DATA ls_par-tables_wa TYPE (lv_type).

         INSERT ls_par INTO TABLE parameters.
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
         INSERT ls_par INTO TABLE parameters.
       ENDLOOP.
     ENDMETHOD.

     METHOD create_parameters.
       CHECK parameters_generated EQ abap_false.

       create_exceptions( ).
*      Tables
       create_table_params( interface-tbl ).         "    input TABLES parameter
       create_table_params( interface-enh_tbl ).
*      Import
       create_params( it_table = interface-imp
                      iv_kind = abap_func_exporting ).
       create_params( it_table = interface-enh_imp
                      iv_kind = abap_func_exporting ).
*      Export
       create_params( it_table = interface-exp
                      iv_kind = abap_func_importing ).
       create_params( it_table = interface-enh_exp
                      iv_kind = abap_func_importing ).
*      Changing
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
*      find the environment where the symbol is defined
       IF line_exists( map[ symbol = symbol ] ).
         env = me.            " found in current environment
       ELSEIF outer IS BOUND.
         env = outer->find( symbol ).
       ELSE.
         unbound_symbol( symbol ).
       ENDIF.
     ENDMETHOD.                    "find

     METHOD lookup.
*      Lookup a value in the current environment or in the parent (outer) environment
       TRY.
           cell = VALUE #( map[ symbol = symbol ]-value DEFAULT outer->lookup( symbol ) ).
         CATCH cx_root.
           unbound_symbol( symbol ).
       ENDTRY.
     ENDMETHOD.                    "find

     METHOD unbound_symbol.
       RAISE EXCEPTION TYPE lcx_lisp_exception
         EXPORTING
           message = |Symbol { symbol } is unbound|
           area    = lcx_lisp_exception=>c_area_eval.
     ENDMETHOD.

     METHOD define_value.
       element = lcl_lisp=>new_elem( type = type
                                     value = value ).
       define( symbol = symbol
               element = element ).
     ENDMETHOD.                    "define_cell

     METHOD define.
*      Add a value to the (local) environment
       DATA(ls_map) = VALUE ts_map( symbol = symbol
                                    value = element ).
       INSERT ls_map INTO TABLE map.
       CHECK sy-subrc = 4.                " To comply with Scheme define,
       MODIFY TABLE map FROM ls_map.      " overwrite existing defined values
     ENDMETHOD.                    "define

     METHOD parameters_to_symbols.
*      The lambda receives its own local environment in which to execute,
*      where parameters become symbols that are mapped to the corresponding arguments
*      Assign each argument to its corresponding symbol in the newly created environment
       DATA lv_count TYPE i.

       DATA(lo_par) = io_pars.                " Pointer to formal parameters
       DATA(lo_arg) = io_args.                " Pointer to arguments
       WHILE lo_par NE lcl_lisp=>nil.         " Nil would mean no parameters to map
         IF lo_arg = lcl_lisp=>nil.           " Premature end of arguments
           lcl_lisp=>throw( |Missing parameter(s) { lo_par->to_string( ) }| ).
         ENDIF.

         ADD 1 TO lv_count.
*        NOTE: Each element of the argument list is evaluated before being defined in the environment
         define( symbol = lo_par->car->value
                 element = lo_arg->car ).

         lo_par = lo_par->cdr.
         lo_arg = lo_arg->rest( ).
       ENDWHILE.

       IF lo_arg NE lcl_lisp=>nil.  " Excessive number of arguments
         lcl_lisp=>throw( |Expected { lv_count } parameter(s), found { io_args->to_string( ) }| ).
       ENDIF.
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
       nil = new_symbol( 'nil' ).
       false = new_symbol( 'false' ).
       true = new_symbol( 'true' ).
     ENDMETHOD.                    "class_constructor

     METHOD first.
       ro_car = COND #( WHEN car IS BOUND THEN car ELSE nil ).
     ENDMETHOD.                    "first

     METHOD rest.
       ro_cdr = COND #( WHEN cdr IS BOUND THEN cdr ELSE nil ).
     ENDMETHOD.                    "rest

     METHOD new.
       ro_elem = SWITCH #( type
                   WHEN type_hash THEN NEW lcl_lisp_hash( )
                   WHEN type_abap_function THEN NEW lcl_lisp_abapfunction( )
                   ELSE NEW lcl_lisp( ) ).
       ro_elem->type = type.
       ro_elem->car = io_car.
       ro_elem->cdr = io_cdr.
     ENDMETHOD.                    "new

     METHOD new_elem.
       ro_elem = new( type ).
       CASE type.
         WHEN type_number.
           ro_elem->number = value.
*         WHEN type_abap_data OR type_abap_table.
*           ro_elem->data = ref.
         WHEN OTHERS.
           ro_elem->value = value.
       ENDCASE.
     ENDMETHOD.                    "new_elem

     METHOD new_string.
       ro_elem = new_elem( type = type_string
                           value = value ).
     ENDMETHOD.                    "new_string

     METHOD new_symbol.
       ro_elem = new_elem( type = type_symbol
                           value = value ).
     ENDMETHOD.                    "new_symbol

     METHOD new_number.
       ro_elem = new_elem( type = type_number
                           value = value ).
     ENDMETHOD.                    "new_number

     METHOD new_atom.
       DATA lv_num TYPE decfloat34 ##needed.
*      Check whether the token can be converted to a float, to cover all
*      manner of number formats, including scientific, otherwise treat it
*      as a symbol (but we still store it as a string to preserve the original value
*      and let the ABAP kernel do the heavy lifting later on)
       TRY.
           MOVE EXACT value TO lv_num. "If this passes, it's a number
           ro_elem = new_number( value ).
         CATCH cx_sy_conversion_no_number.
           ro_elem = new_symbol( value ).
       ENDTRY.
     ENDMETHOD.                    "new_atom

     METHOD new_data.
       ro_elem = new( type_abap_data ).
       ro_elem->data = ref.
     ENDMETHOD.                    "new_data

     METHOD new_table.
       ro_elem = new( type_abap_table ).
       ro_elem->data = ref.
     ENDMETHOD.                    "new_table

     METHOD new_cons.
       ro_cons = new( type = type_conscell
                      io_car = io_car
                      io_cdr = io_cdr ).
     ENDMETHOD.                    "new_cons

     METHOD new_lambda.
*      The lambda is a special cell that stores a pointer to a list of parameters
*      and a pointer to a list which is the body to be evaluated later on
       ro_lambda = new( type = type_lambda
                        io_car = io_car                         " List of parameters
                        io_cdr = io_cdr ).                      " Body
*      Store the reference to the environment in which the lambda was created
*      (lexical scope) e.g. if the lambda is created inside another lambda
*      we want that environment to be present when we evaluate the new lambda
       ro_lambda->environment = io_env.
     ENDMETHOD.                    "new_lambda

     METHOD list_to_string.
       str = lcl_parser=>c_open_paren.
       DATA(lo_elem) = me.
       WHILE lo_elem IS BOUND AND lo_elem NE nil.
         str = str && COND string( WHEN lo_elem->type NE type_conscell      " If item is not a cons cell
                                      THEN | . { lo_elem->to_string( ) }|      " indicate with dot notation:
                                      ELSE | { lo_elem->car->to_string( ) }| ).
         lo_elem = lo_elem->cdr.
       ENDWHILE.

       str = |{ str } { lcl_parser=>c_close_paren }|.
     ENDMETHOD.                    "list_to_string

     METHOD to_string.
       CASE type.
         WHEN type_lambda.
           str = |<lambda> { car->list_to_string( ) }|.
         WHEN type_symbol.
           str = value.
         WHEN type_string.
*        TODO: Other Lisp REPLs give back the string as a quoted string
           str = value.
         WHEN type_number.
           str = number.
         WHEN type_native.
           str = '<native>'.
         WHEN type_conscell.
           str = list_to_string( ).
         WHEN type_hash.
           str = '<hash>'.
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

     METHOD throw.
       RAISE EXCEPTION TYPE lcx_lisp_exception
         EXPORTING
           message = message
           area    = lcx_lisp_exception=>c_area_eval.
     ENDMETHOD.                    "eval_err

   ENDCLASS.                    "lcl_lisp IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_iterator IMPLEMENTATION
*----------------------------------------------------------------------*
   CLASS lcl_lisp_iterator IMPLEMENTATION.

     METHOD new.
       assert_is_bound io_elem `Iterator: Invalid head of list`.
       ro_iter = NEW #( ).
       ro_iter->elem = io_elem.
       ro_iter->first = abap_true.
     ENDMETHOD.                    "new

     METHOD has_next.
       rv_flag = xsdbool( first EQ abap_true OR ( elem->cdr IS BOUND AND elem->cdr NE lcl_lisp=>nil ) ).
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

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_hash IMPLEMENTATION
*----------------------------------------------------------------------*
   CLASS lcl_lisp_hash IMPLEMENTATION.

     METHOD new_hash.
       validate: list, list->car.
       ro_elem ?= new( type_hash ).
       CHECK list->car->type = type_conscell.

*      Can accept a parameter which should be a list of alternating symbols/strings and elements
       DATA(lo_iter) = lcl_lisp_iterator=>new( list->car ).
       WHILE lo_iter->has_next( ).
         DATA(lo_ptr) = lo_iter->next( ).
         IF lo_ptr->type NE type_symbol AND lo_ptr->type NE type_string.
           throw( |MAKE-HASH: Use only symbol or string as a key| ).
         ENDIF.
         CHECK lo_iter->has_next( ).
         INSERT VALUE #( key = lo_ptr->value
                         element = lo_iter->next( ) ) INTO TABLE ro_elem->hash.
       ENDWHILE.
     ENDMETHOD.                    "new_hash

     METHOD get.
       validate: list, list->car.
       IF list->car = nil.
         throw( |HASH-GET requires a key to access an element| ).
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
         DATA(lo_last) = new_cons( io_car = new_symbol( ls_entry-key ) ).

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

* Execute Query via secondary DB connection
*   " Sample for OPEN-SQL
*DATA:
*  ls_carrier TYPE scarr,
*  lo_statement TYPE REF TO cl_sql_statement,
*  lo_result_set TYPE REF TO cl_sql_result_set.
*
*SELECT SINGLE * FROM scarr CONNECTION ( 'SECONDARY' ) INTO ls_carrier WHERE carrid = 'LH'.
*
*" Sample for native SQL or ADBC
*TRY.
*  lo_statement = cl_sql_connection=>get_connection( 'SECONDARY' )->create_statement( ).
*  lo_result_set = lo_statement->execute_query( |SELECT SINGLE * FROM scarr WHERE carrid = 'LH' AND mandt = { sy-mandt } | ).
*
*  ...
*
*  CATCH cx_sql_exception.
*  " Error handling
*ENDTRY.
