CLASS ycl_abap_scheme_launcher DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS repl IMPORTING code TYPE string
                           out TYPE REF TO if_oo_adt_classrun_out.
    "lif_classrun_output
ENDCLASS.



CLASS ycl_abap_scheme_launcher IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
     repl( code = `(round 3.5)`
           out = out ).
     repl( code = `(list->vector '(dididit dah))`
           out = out ).
     repl( code = |(when (= 1 1.0)| &
                         |(display "1")| &
                         |(display "2"))|
           out = out ).
  ENDMETHOD.
  METHOD repl.
    DATA response TYPE string.
    DATA output TYPE string.
    DATA port TYPE REF TO lcl_lisp_buffered_port.
    DATA lo_int TYPE REF TO lcl_lisp_profiler. "The Lisp interpreter.

     out->write( `Welcome to ABAP Scheme in the Cloud!` ).
     lcl_lisp_port=>go_out = out.
    TRY.
        port ?= lcl_lisp_new=>port( iv_port_type = lcl_lisp_port=>c_port_textual
                                   iv_input     = abap_true
                                   iv_output    = abap_true
                                   iv_error     = abap_true
                                   iv_buffered  = abap_true ).

        lo_int = NEW #( io_port = port  " LISP Interpreter
                        ii_log = port ).

        out->get( data = code
                  name = |Enter Scheme code:\n| ).

        response = lo_int->eval_repl( EXPORTING code = code
                                      IMPORTING output = output ).
        response = |[ { lo_int->runtime } µs ] { response }|.

      CATCH cx_root INTO DATA(lx_root).
        response = lx_root->get_text( ).
    ENDTRY.

    out->write( |{ code }\n=> { response }\n| ).
  ENDMETHOD.

ENDCLASS.
