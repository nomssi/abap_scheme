CLASS ycl_abap_scheme_launcher DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_service_extension.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA request  TYPE REF TO if_web_http_request.
    DATA response TYPE REF TO if_web_http_response.

    METHODS repl IMPORTING code TYPE string
                           out  TYPE REF TO if_oo_adt_classrun_out.
    "lif_classrun_output
    METHODS scheme_repl IMPORTING code TYPE string.

    METHODS get_context RETURNING VALUE(context) TYPE string.
    METHODS print_context IMPORTING value TYPE string.
    METHODS print IMPORTING text TYPE string.
    METHODS error IMPORTING text TYPE string DEFAULT 'Bad Request'
                            code TYPE i DEFAULT 400
                              PREFERRED PARAMETER code.
ENDCLASS.



CLASS ycl_abap_scheme_launcher IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    out->write( `Welcome to ABAP Scheme in the Cloud!` ).

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
    DATA lo_interpreter TYPE REF TO lcl_lisp_profiler. "The Lisp interpreter.

    lcl_lisp_port=>go_out = out.
    TRY.
        port ?= lcl_lisp_new=>port( iv_port_type = textual
                                    iv_input     = abap_true
                                    iv_output    = abap_true
                                    iv_error     = abap_true
                                    iv_buffered  = abap_true ).

        lo_interpreter = NEW #( io_port = port
                                ii_log = port ).

        out->get( data = code
                  name = |Enter Scheme code:\n| ).

        response = lo_interpreter->eval_repl( EXPORTING code = code
                                              IMPORTING output = output ).
        response = |[ { lo_interpreter->runtime } µs ] { response }|.

      CATCH cx_root INTO DATA(lx_root).
        response = lx_root->get_text( ).
    ENDTRY.

    out->write( |{ code }\n=> { response }\n| ).
  ENDMETHOD.

  METHOD if_http_service_extension~handle_request.
    me->request = request.
    me->response = response.

    DATA(lt_params) = request->get_form_fields( ).
    DATA(lv_cmd_idx) = line_index(  lt_params[ name = 'cmd' ] ).
    IF lv_cmd_idx EQ 0.
      error( 400 ).
      RETURN.
    ENDIF.

    print_context( lt_params[ lv_cmd_idx ]-value ).

  ENDMETHOD.

  METHOD print.
    response->set_text( text ).
  ENDMETHOD.

  METHOD print_context.
    CASE value.
      WHEN `hello`.
        print( |Hello World!| ).
      WHEN `timestamp`.
        print( |Hello World! application executed by { get_context( ) }| ).
      WHEN `bankdetails`.
        print(  NEW ycl_api_hub_manager_eyjfc( )->get_bank_details( ) ).
      WHEN `scheme`.
*        out->get( data = code
*                  name = |Enter Scheme code:\n| ).
        scheme_repl( `(list->vector '(dididit dah))` ).

      WHEN OTHERS.
        error( ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_context.
    context = cl_abap_context_info=>get_user_technical_name( ) &&
        | on { cl_abap_context_info=>get_system_date( ) DATE = ENVIRONMENT } | &&
        | at { cl_abap_context_info=>get_system_time( ) TIME = ENVIRONMENT } |.
  ENDMETHOD.

  METHOD error.
    response->set_status( i_code = code
                          i_reason = text ).
  ENDMETHOD.

  METHOD scheme_repl.
    DATA response TYPE string.
    DATA output TYPE string.
    DATA port TYPE REF TO lcl_lisp_buffered_port.
    DATA lo_interpreter TYPE REF TO lcl_lisp_profiler. "The Lisp interpreter.

    TRY.
        port ?= lcl_lisp_new=>port( iv_port_type = textual
                                    iv_input     = abap_true
                                    iv_output    = abap_true
                                    iv_error     = abap_true
                                    iv_buffered  = abap_true ).

        lo_interpreter = NEW #( io_port = port
                                ii_log = port ).

        response = lo_interpreter->eval_repl( EXPORTING code = code
                                              IMPORTING output = output ).
        response = |[ { lo_interpreter->runtime } µs ] { response }|.

      CATCH cx_root INTO DATA(lx_root).
        response = lx_root->get_text( ).
    ENDTRY.

    print( |ABAP Scheme { code }\n=> { response }\n| ).
  ENDMETHOD.

ENDCLASS.
