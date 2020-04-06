CLASS ycl_scheme_eval DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_scheme_eval IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.
    DATA(system_date) = cl_abap_context_info=>get_system_date( ).
    DATA(text) = CONV string( system_date ).
    response->set_text( 'Hello!' && text ).
  ENDMETHOD.
ENDCLASS.
