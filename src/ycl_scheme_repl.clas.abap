class YCL_SCHEME_REPL definition
  public
  create public .

public section.

  interfaces IF_HTTP_SERVICE_EXTENSION.
protected section.
private section.
  DATA request  TYPE REF TO if_web_http_request.
  DATA response TYPE REF TO if_web_http_response.

  METHODS get_context RETURNING VALUE(context) TYPE string.
  METHODS print_context IMPORTING value TYPE string.
  METHODS print IMPORTING text TYPE string.
  METHODS error IMPORTING text TYPE string DEFAULT 'Bad Request'
                          code TYPE i DEFAULT 400
                          PREFERRED PARAMETER code.

ENDCLASS.



CLASS YCL_SCHEME_REPL IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
    me->request = request.
    me->response = response.

    DATA(lt_params) = request->get_form_fields( ).
    DATA(lv_cmd_idx) = line_index(  lt_params[ name = 'cmd' ] ).
    IF lv_cmd_idx EQ 0.
      error( 400 ).
      RETURN.
    ENDIF.

    print( |ABAP Scheme| ).
    print_context( lt_params[ lv_cmd_idx ]-value ).

  endmethod.

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

ENDCLASS.
