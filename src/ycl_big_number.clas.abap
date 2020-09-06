CLASS ycl_big_number DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
     METHODS demo IMPORTING iv_first TYPE string
                            iv_second TYPE string
                            out TYPE REF TO if_oo_adt_classrun_out.
ENDCLASS.

CLASS ycl_big_number  IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
     demo( iv_first = '6584389034291301828999'
           iv_second = '533335555511111'
           out = out ).
  ENDMETHOD.

 METHOD demo.
  DATA:
    lv1    TYPE decfloat34,
    lv2    TYPE decfloat34,
    result TYPE decfloat34.
  DATA lo_int1 TYPE REF TO lcl_lisp_bigintx.
  DATA lo_int2 TYPE REF TO lcl_lisp_bigintx.

  lv1 = iv_first.
  lv2 = iv_second.
  lo_int1 = NEW #( ).  lo_int1->set_str( iv_first ).
  lo_int2 = NEW #( ).  lo_int2->set_str( iv_second ).

  "Add
  result = lv1 + lv2.
  out->WRITE(  |Addition: { result }  with BigInt  { lo_int1->set_float( lv1 )->add( lo_int2->set_float( lv2 ) )->to_str( ) }| ).

  "Subtract
  result = lv1 - lv2.
  out->WRITE(  |Subtraction: { result }  with BigInt  { lo_int1->set_float( lv1 )->sub( lo_int2->set_float( lv2 ) )->to_str( ) }| ).

  "Multiply
  result = lv1 * lv2.
out->WRITE(  |Multiplication: { result }  with BigInt  { lo_int1->set_float( lv1 )->mul( lo_int2->set_float( lv2 ) )->to_str( ) }| ).

  "Divide
  result = lv1 / lv2.
out->WRITE(  |Division: { result }  with BigInt  { lo_int1->set_float( lv1 )->div( lo_int2->set_float( lv2 ) )->to_str( ) }| ).

  "Modulo
  result = lv1 MOD lv2.
out->WRITE(  |Modulo: { result }  with BigInt  { lo_int1->set_float( lv1 )->mod( lo_int2->set_float( lv2 ) )->to_str( ) }| ).
  CLEAR result.
ENDMETHOD.

ENDCLASS.
