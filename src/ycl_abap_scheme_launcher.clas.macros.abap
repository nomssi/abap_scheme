***"* use this source file for any macro definitions you need
***"* in the implementation part of the class
*
*
**      DEFINE _to_param_object.
**        &2 = eval( element = &1
**                   environment = io_env ).
**        IF &2->type EQ lcl_lisp=>type_lambda.
**          lo_lambda ?= &2.
**          lv_parameter_object = lo_lambda->parameter_object.
**        ENDIF.
**        IF lv_parameter_object EQ abap_false.
**          throw( |missing parameter object in parameterize| ).
**        ENDIF.
**      END-OF-DEFINITION.
*
**
**  DEFINE _trace_call.
**    IF gv_lisp_trace EQ abap_true.
**      cl_demo_output=>write( |call { &1->value } { &1->to_string( ) } param { &2->to_string( ) }| ).
**    ENDIF.
**  END-OF-DEFINITION.
**
**  DEFINE _trace_result.
**    IF gv_lisp_trace EQ abap_true.
**      cl_demo_output=>write( |=> { &1->to_string( ) }| ).
**    ENDIF.
**  END-OF-DEFINITION.
**
*** Macro to simplify the definition of a native procedure
**  DEFINE _proc_meth.
**    METHODS &1 IMPORTING list TYPE REF TO lcl_lisp
**               RETURNING VALUE(result) TYPE REF TO lcl_lisp
**               RAISING lcx_lisp_exception.
**  END-OF-DEFINITION.
**
**  DEFINE _assert_is_bound.
**    IF &1 IS NOT BOUND.
**      lcl_lisp=>throw( &2 ).
**    ENDIF.
**  END-OF-DEFINITION.
**
*  DEFINE _validate.
*    _assert_is_bound &1 c_error_incorrect_input.
*  END-OF-DEFINITION.
*        IF &1 IS NOT BOUND.
*          lcl_lisp=>throw( c_error_incorrect_input ).
*        ENDIF.
**
*
*  DEFINE _validate_mutable.
*    _validate &1.
*    IF &1->mutable EQ abap_false.
*      throw( |constant { &2 } cannot be changed| ) ##NO_TEXT.
*    ENDIF.
*  END-OF-DEFINITION.
*
*  DEFINE _validate_type.
*    _validate &1.
*    IF &1->type NE lcl_lisp=>type_&3.
*      throw( &1->to_string( ) && ` is not a ` && &4 && ` in ` && &2 ) ##NO_TEXT.
*    ENDIF.
*  END-OF-DEFINITION.
*    _validate_type &1 &2 string `string`.
**
**
***
*  DEFINE _validate_integer.
*    _validate &1.
*    IF &1->type NE lcl_lisp=>type_integer.
*      throw( &1->to_string( ) && ` is not an integer in ` && &2 ) ##NO_TEXT.
*    ENDIF.
*  END-OF-DEFINITION.
**
*  DEFINE _validate_index.
*    _validate_integer &1 &2.
*    IF CAST lcl_lisp_integer( &1 )->integer LT 0.
*      throw( &1->to_string( ) && ` must be non-negative in ` && &2 ) ##NO_TEXT.
*    ENDIF.
*  END-OF-DEFINITION.
***
*  DEFINE _validate_char.
*    _validate_type &1 &2 char `char`.
*  END-OF-DEFINITION.
*
*  DEFINE _validate_string.
*    _validate_type &1 &2 string `string`.
*  END-OF-DEFINITION.
*
*  DEFINE _validate_vector.
*    _validate_type &1 &2 vector `vector`.
*  END-OF-DEFINITION.
*
*  DEFINE _validate_port.
*    _validate_type &1 &2 port `port`.
*  END-OF-DEFINITION.
*    _validate &1.
*    IF &1->type NE lcl_lisp=>type_port.
*      throw( &1->to_string( ) && ` is not a port in ` && &2 ) ##NO_TEXT.
*    ENDIF.
**
***
*  DEFINE _validate_turtle.
*    _validate_type &1 &2 abap_turtle `turtle`.
*  END-OF-DEFINITION.
***
***  DEFINE _validate_number.

*    IF &1 IS NOT BOUND.
*      lcl_lisp=>throw( c_error_incorrect_input ).
*     ENDIF.
*    CASE &1->type.
*      WHEN lcl_lisp=>type_integer
*        OR lcl_lisp=>type_real
*        OR lcl_lisp=>type_rational
*        OR lcl_lisp=>type_complex.
*      WHEN OTHERS.
*        throw( |{ &1->to_string( ) } is not a number in | && &2 ) ##NO_TEXT.
*    ENDCASE.
***  END-OF-DEFINITION.
***
***  DEFINE _error_no_list.
***    throw( |{ &2 }: { &1->to_string( ) } is not a proper list| ) ##NO_TEXT.
***  END-OF-DEFINITION.
***
*  DEFINE _to_integer.
*    &2 = CAST lcl_lisp_integer( &1 )->integer.
*  END-OF-DEFINITION.
*
*  DEFINE _to_real.
*    &2 = CAST lcl_lisp_real( &1 )->real.
*  END-OF-DEFINITION.
***
***  DEFINE _validate_tail.
***    IF &1 NE nil.
****     if the last element in the list is not a cons cell, we cannot append
***      _error_no_list &2 &3.
***    ENDIF.
***  END-OF-DEFINITION.
***
*  DEFINE _get_number.
*    _validate &2.
*    cell = &2.
*    CASE cell->type.
*      WHEN lcl_lisp=>type_integer.
*        _to_integer cell &1.
*      WHEN lcl_lisp=>type_real.
*        _to_real cell &1.
*      WHEN lcl_lisp=>type_rational.
*        lo_rat ?= cell.
*        &1 = lo_rat->integer / lo_rat->denominator.
**      WHEN lcl_lisp=>type_complex.
*      WHEN OTHERS.
*        throw( |{ cell->to_string( ) } is not a number in { &3 }| ).
*    ENDCASE.
*  END-OF-DEFINITION.
***
***  DEFINE _data_local_numeric.
***    DATA lo_rat TYPE REF TO lcl_lisp_rational.
***    DATA lo_int TYPE REF TO lcl_lisp_integer.
***    DATA lo_real TYPE REF TO lcl_lisp_real.
***  END-OF-DEFINITION.
***
***  DEFINE _data_local_numeric_cell.
***    DATA cell TYPE REF TO lcl_lisp.
***    _data_local_numeric.
***  END-OF-DEFINITION.
***
** Macro that implements the logic for the comparison native
** procedures, where only the comparison operator differs
*  DEFINE _comparison.
*    DATA carry TYPE tv_real.
*    DATA carry_int TYPE tv_int.
*    DATA carry_is_int TYPE flag.
*    _data_local_numeric_cell.
*
*
*    result = false.
*    _validate: list, list->car, list->cdr.
*    IF list->cdr->type NE lcl_lisp=>type_pair.
*      throw( c_error_incorrect_input ).
*    ENDIF.
*
*    cell = list->car.
*    carry_is_int = abap_false.
*    CASE cell->type.
*      WHEN lcl_lisp=>type_integer.
*        carry_is_int = abap_true.
*        _to_integer cell carry_int.
*        carry = carry_int.
*      WHEN lcl_lisp=>type_real.
*        _to_real cell carry.
*      WHEN lcl_lisp=>type_rational.
*        lo_rat ?= cell.
*        carry = lo_rat->integer / lo_rat->denominator.
**          WHEN lcl_lisp=>type_complex.
*      WHEN OTHERS.
*        throw( |{ cell->to_string( ) } is not a number in { &2 }| ).
*    ENDCASE.
*
*    cell = list->cdr.
*    WHILE cell->type EQ lcl_lisp=>type_pair.
*      _validate cell->car.
*
*      CASE cell->car->type.
*        WHEN lcl_lisp=>type_integer.
*          lo_int ?= cell->car.
*          IF carry_is_int = abap_true.
*            IF carry_int &1 lo_int->integer.
*              RETURN.
*            ENDIF.
*            carry_int = lo_int->integer.
*          ELSE.
*            IF carry &1 lo_int->integer.
*              RETURN.
*            ENDIF.
*          ENDIF.
*          carry = lo_int->integer.
*
*        WHEN lcl_lisp=>type_real.
*          carry_is_int = abap_false.
*          lo_real ?= cell->car.
*          IF carry &1 lo_real->real.
*            RETURN.
*          ENDIF.
*          carry = lo_real->real.
*
*        WHEN lcl_lisp=>type_rational.
*          carry_is_int = abap_false.
*          lo_rat ?= cell->car.
*          IF carry * lo_rat->denominator &1 lo_rat->integer.
*            RETURN.
*          ENDIF.
*          carry = lo_rat->integer / lo_rat->denominator.
*
**               WHEN lcl_lisp=>type_complex.
*        WHEN OTHERS.
*          throw( |{ cell->car->to_string( ) } is not a number in { &2 }| ).
*      ENDCASE.
*      cell = cell->cdr.
*    ENDWHILE.
*    result = true.
*  END-OF-DEFINITION.
***
**  DEFINE _sign.
**    DATA carry TYPE tv_real.
**    DATA cell TYPE REF TO lcl_lisp.
**    DATA lo_rat TYPE REF TO lcl_lisp_rational.
**    DATA lo_int TYPE REF TO lcl_lisp_integer.
**    DATA lo_real TYPE REF TO lcl_lisp_real.
**
**    result = false.
**    IF list IS NOT BOUND.
**      lcl_lisp=>throw( c_error_incorrect_input ).
**    ENDIF.
**
**    "_get_number carry list->car &2.
**    IF list->car IS NOT BOUND.
**      lcl_lisp=>throw( c_error_incorrect_input ).
**    ENDIF.
**
**    cell = list->car.
**    CASE cell->type.
**      WHEN lcl_lisp=>type_integer.
**        carry = CAST lcl_lisp_integer( cell )->integer.
**      WHEN lcl_lisp=>type_real.
**        carry = CAST lcl_lisp_real( cell )->real.
**      WHEN lcl_lisp=>type_rational.
**        lo_rat ?= cell.
**        carry = lo_rat->integer / lo_rat->denominator.
***      WHEN lcl_lisp=>type_complex.
**      WHEN OTHERS.
**        throw( |{ cell->to_string( ) } is not a number in [&2]| ).
**    ENDCASE.
**
**    IF sign( carry ) NE &1.
**      RETURN.
**    ENDIF.
**    result = true.
**  END-OF-DEFINITION.
***
***  DEFINE _is_type. " argument in list->car
*    _validate list.
*    result = false.
*    CHECK list->car IS BOUND.
*    IF list->car->type EQ lcl_lisp=>type_&1.
*      result = true.
*    ENDIF.
***  END-OF-DEFINITION.
***
***  DEFINE _is_last_param.
*    IF &1->cdr NE nil.
*      throw( |{ &1->to_string( ) } Parameter mismatch| ).
*    ENDIF.
***  END-OF-DEFINITION.
***
*  DEFINE _catch_arithmetic_error.
*    CATCH cx_sy_arithmetic_error cx_sy_conversion_no_number INTO DATA(lx_error).
*      throw( lx_error->get_text( ) ).
*  END-OF-DEFINITION.
***
*** Macro that implements the logic for call of ABAP math statements
**  DEFINE _math.
**    DATA carry TYPE tv_real.
**    DATA cell TYPE REF TO lcl_lisp.
**    DATA lo_rat TYPE REF TO lcl_lisp_rational.
**    DATA lo_int TYPE REF TO lcl_lisp_integer.
**    DATA lo_real TYPE REF TO lcl_lisp_real.
**
**    result = nil.
**    IF list IS NOT BOUND.
**      lcl_lisp=>throw( c_error_incorrect_input ).
**    ENDIF.
**    TRY.
**        "_get_number carry list->car &2.
**        IF &2 IS NOT BOUND.
**          lcl_lisp=>throw( c_error_incorrect_input ).
**        ENDIF.
**        cell = &2.
**        CASE cell->type.
**          WHEN lcl_lisp=>type_integer.
**            &1 = CAST lcl_lisp_integer( cell )->integer.
**          WHEN lcl_lisp=>type_real.
**            &1 = CAST lcl_lisp_real( cell )->real.
**          WHEN lcl_lisp=>type_rational.
**            lo_rat ?= cell.
**            &1 = lo_rat->integer / lo_rat->denominator.
***          WHEN lcl_lisp=>type_complex.
**          WHEN OTHERS.
**            throw( |{ cell->to_string( ) } is not a number in { &3 }| ).
**        ENDCASE.
**        _is_last_param list.
**         IF list->cdr NE nil.
**           throw( |{ list->to_string( ) } Parameter mismatch| ).
**         ENDIF.
**        result = lcl_lisp_new=>&3( &1( carry ) ).
**      _catch_arithmetic_error.
**    ENDTRY.
**  END-OF-DEFINITION.
***
*  DEFINE _trigonometric.
*    DATA carry TYPE f.
*    DATA cell TYPE REF TO lcl_lisp.
*    DATA lo_rat TYPE REF TO lcl_lisp_rational.
*    DATA lo_int TYPE REF TO lcl_lisp_integer.
*    DATA lo_real TYPE REF TO lcl_lisp_real.
*
*    result = nil.
*    _validate list.
*    TRY.
*        _get_number carry list->car &2.
*        _is_last_param list.
*        result = lcl_lisp_new=>real( &1( carry ) ).
*      _catch_arithmetic_error.
*    ENDTRY.
*  END-OF-DEFINITION.

*    DEFINE _char01_to_integer.
*      FIELD-SYMBOLS <xword> TYPE x.
*      FIELD-SYMBOLS <xint> TYPE x.
*      DATA lv_int TYPE int2.
*
*      ASSIGN &1 TO <xword> CASTING.
*      ASSIGN lv_int TO <xint> CASTING.
*      <xint> = <xword>.
*      &2 = lv_int.
*    END-OF-DEFINITION.


*    DEFINE _proc_list_compare.
*      DATA lo_test TYPE REF TO lcl_lisp.
*      DATA lo_arg TYPE REF TO lcl_lisp.
*      DATA lv_ref TYPE &4.
*      DATA lv_test TYPE &4.
*
*      "_validate list.
*      IF list IS NOT BOUND.
*        lcl_lisp=>throw( c_error_incorrect_input ).
*      ENDIF.
*
*      result = false.
*      lo_arg = list.
*
*      lo_test = nil.
*      IF lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>&5.
*        lo_test = lo_arg->car.
*        lv_ref = &3( lo_test ).
*        lo_arg = lo_arg->cdr.
*      ENDIF.
*      IF lo_test EQ nil OR lo_arg EQ nil.
*        throw( |{ &1 } missing argument| ).
*      ENDIF.
*
*      WHILE lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>&5.
*        lv_test = &3( lo_arg->car ).
*        IF lv_ref &2 lv_test.
*          lv_ref = lv_test.
*        ELSE.
*          RETURN.
*        ENDIF.
*        lo_arg = lo_arg->cdr.
*      ENDWHILE.
*
*      IF lo_arg NE nil.
*        throw( |{ &1 } wrong argument in { lo_arg->car->to_string( ) }| ).
*      ENDIF.
*      CHECK lo_arg = nil.
*      result = true.
*    END-OF-DEFINITION.
*
*
*    DEFINE _proc_string_list_compare.
*      "_proc_list_compare &1 &2 char_case_identity string type_string.
*      DATA lo_test TYPE REF TO lcl_lisp.
*      DATA lo_arg TYPE REF TO lcl_lisp.
*      DATA lv_ref TYPE string.
*      DATA lv_test TYPE string.
*
*      "_validate list.
*      IF list IS NOT BOUND.
*        lcl_lisp=>throw( c_error_incorrect_input ).
*      ENDIF.
*
*      result = false.
*      lo_arg = list.
*
*      lo_test = nil.
*      IF lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>type_string.
*        lo_test = lo_arg->car.
*        lv_ref = char_case_identity( lo_test ).
*        lo_arg = lo_arg->cdr.
*      ENDIF.
*      IF lo_test EQ nil OR lo_arg EQ nil.
*        throw( |{ &1 } missing argument| ).
*      ENDIF.
*
*      WHILE lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>type_string.
*        lv_test = char_case_identity( lo_arg->car ).
*        IF lv_ref &2 lv_test.
*          lv_ref = lv_test.
*        ELSE.
*          RETURN.
*        ENDIF.
*        lo_arg = lo_arg->cdr.
*      ENDWHILE.
*
*      IF lo_arg NE nil.
*        throw( |{ &1 } wrong argument in { lo_arg->car->to_string( ) }| ).
*      ENDIF.
*      CHECK lo_arg = nil.
*      result = true.
*    END-OF-DEFINITION.
*
*    DEFINE _proc_string_ci_list_compare.
*      "_proc_list_compare &1 &2 fold_case string type_string.
*      DATA lo_test TYPE REF TO lcl_lisp.
*      DATA lo_arg TYPE REF TO lcl_lisp.
*      DATA lv_ref TYPE string.
*      DATA lv_test TYPE string.
*
*      "_validate list.
*      IF list IS NOT BOUND.
*        lcl_lisp=>throw( c_error_incorrect_input ).
*      ENDIF.
*
*      result = false.
*      lo_arg = list.
*
*      lo_test = nil.
*      IF lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>type_string.
*        lo_test = lo_arg->car.
*        lv_ref = fold_case( lo_test ).
*        lo_arg = lo_arg->cdr.
*      ENDIF.
*      IF lo_test EQ nil OR lo_arg EQ nil.
*        throw( |{ &1 } missing argument| ).
*      ENDIF.
*
*      WHILE lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>type_string.
*        lv_test = fold_case( lo_arg->car ).
*        IF lv_ref &2 lv_test.
*          lv_ref = lv_test.
*        ELSE.
*          RETURN.
*        ENDIF.
*        lo_arg = lo_arg->cdr.
*      ENDWHILE.
*
*      IF lo_arg NE nil.
*        throw( |{ &1 } wrong argument in { lo_arg->car->to_string( ) }| ).
*      ENDIF.
*      CHECK lo_arg = nil.
*      result = true.
*    END-OF-DEFINITION.
*
*    DEFINE _proc_char_list_compare.
*      "_proc_list_compare &1 &2 char_to_integer tv_int type_char.
*      DATA lo_test TYPE REF TO lcl_lisp.
*      DATA lo_arg TYPE REF TO lcl_lisp.
*      DATA lv_ref TYPE tv_int.
*      DATA lv_test TYPE tv_int.
*
*      "_validate list.
*      IF list IS NOT BOUND.
*        lcl_lisp=>throw( c_error_incorrect_input ).
*      ENDIF.
*
*      result = false.
*      lo_arg = list.
*
*      lo_test = nil.
*      IF lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>type_char.
*        lo_test = lo_arg->car.
*        lv_ref = char_to_integer( lo_test ).
*        lo_arg = lo_arg->cdr.
*      ENDIF.
*      IF lo_test EQ nil OR lo_arg EQ nil.
*        throw( |{ &1 } missing argument| ).
*      ENDIF.
*
*      WHILE lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>type_char.
*        lv_test = char_to_integer( lo_arg->car ).
*        IF lv_ref &2 lv_test.
*          lv_ref = lv_test.
*        ELSE.
*          RETURN.
*        ENDIF.
*        lo_arg = lo_arg->cdr.
*      ENDWHILE.
*
*      IF lo_arg NE nil.
*        throw( |{ &1 } wrong argument in { lo_arg->car->to_string( ) }| ).
*      ENDIF.
*      CHECK lo_arg = nil.
*      result = true.
*    END-OF-DEFINITION.
*
*    DEFINE _proc_char_ci_list_compare.
*      "_proc_list_compare &1 &2 char_fold_case_to_integer tv_int type_char.
*      DATA lo_test TYPE REF TO lcl_lisp.
*      DATA lo_arg TYPE REF TO lcl_lisp.
*      DATA lv_ref TYPE tv_int.
*      DATA lv_test TYPE tv_int.
*
*      "_validate list.
*      IF list IS NOT BOUND.
*        lcl_lisp=>throw( c_error_incorrect_input ).
*      ENDIF.
*
*      result = false.
*      lo_arg = list.
*
*      lo_test = nil.
*      IF lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>type_char.
*        lo_test = lo_arg->car.
*        lv_ref = char_fold_case_to_integer( lo_test ).
*        lo_arg = lo_arg->cdr.
*      ENDIF.
*      IF lo_test EQ nil OR lo_arg EQ nil.
*        throw( |{ &1 } missing argument| ).
*      ENDIF.
*
*      WHILE lo_arg->type EQ lcl_lisp=>type_pair AND lo_arg->car->type EQ lcl_lisp=>type_char.
*        lv_test = char_fold_case_to_integer( lo_arg->car ).
*        IF lv_ref &2 lv_test.
*          lv_ref = lv_test.
*        ELSE.
*          RETURN.
*        ENDIF.
*        lo_arg = lo_arg->cdr.
*      ENDWHILE.
*
*      IF lo_arg NE nil.
*        throw( |{ &1 } wrong argument in { lo_arg->car->to_string( ) }| ).
*      ENDIF.
*      CHECK lo_arg = nil.
*      result = true.
*    END-OF-DEFINITION.
*
*    DEFINE _throw_radix.
*      RAISE EXCEPTION TYPE lcx_lisp_exception
*        EXPORTING
*          message = &1
*          area    = 'Radix' ##NO_TEXT.
*    END-OF-DEFINITION.
