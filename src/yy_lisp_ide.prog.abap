*&---------------------------------------------------------------------*
*&  Include           YY_LISP_IDE
*&---------------------------------------------------------------------*

CONSTANTS:
  c_lisp_input    TYPE string VALUE 'ABAP Lisp Input',
  c_lisp_untitled TYPE string VALUE 'Untitled'.
CONSTANTS:
  c_lisp_ast_view TYPE cl_abap_browser=>title VALUE 'ABAP Lisp S-Expression Viewer'.

DATA g_ok_code TYPE syucomm.

*----------------------------------------------------------------------*
*       CLASS lcl_stack DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_stack DEFINITION.
  PUBLIC SECTION.
    TYPES tv_data TYPE string.
    METHODS push IMPORTING iv_key TYPE tv_data.
    METHODS pop RETURNING value(rv_data) TYPE tv_data.
    METHODS empty RETURNING value(rv_flag) TYPE xsdboolean.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_node,
             data TYPE tv_data,
             next TYPE REF TO data,
           END OF ts_node.

    DATA mr_top TYPE REF TO ts_node.
ENDCLASS.                    "lcl_stack DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_container DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_container DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS free_controls.
    DATA mo_input TYPE REF TO cl_gui_container READ-ONLY.
    DATA mo_output TYPE REF TO cl_gui_container READ-ONLY.
    DATA mo_log TYPE REF TO cl_gui_container READ-ONLY.
    DATA mo_alv TYPE REF TO cl_gui_container READ-ONLY.
    DATA mo_console TYPE REF TO cl_gui_container READ-ONLY.
  PRIVATE SECTION.
    DATA mo_splitter_h TYPE REF TO cl_gui_splitter_container.
    DATA mo_splitter_v TYPE REF TO cl_gui_splitter_container.
    DATA mo_splitter_v_h TYPE REF TO cl_gui_splitter_container.
    DATA mo_left TYPE REF TO cl_gui_container.
    DATA mo_right TYPE REF TO cl_gui_container.
ENDCLASS.                    "lcl_container DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_editor DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_editor DEFINITION INHERITING FROM cl_gui_textedit.
  PUBLIC SECTION.
    CONSTANTS c_comments_string TYPE char01 VALUE ';'.

    METHODS constructor IMPORTING io_container TYPE REF TO cl_gui_container
                                  iv_read_only TYPE flag DEFAULT abap_true
                                  iv_toolbar   TYPE flag DEFAULT abap_false.
    METHODS append_source IMPORTING iv_text TYPE string.

    METHODS push_text.
    METHODS pop_text RETURNING value(code) TYPE string.
    METHODS to_string RETURNING value(rv_text) TYPE string.
    METHODS update_status IMPORTING iv_string TYPE string.
    METHODS append_string IMPORTING iv_text TYPE string.
  PRIVATE SECTION.
    DATA mv_counter TYPE i.
    DATA mo_stack TYPE REF TO lcl_stack.

    METHODS format_input IMPORTING code           TYPE string
                         RETURNING value(rv_text) TYPE string.
ENDCLASS.                    "lcl_editor DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_stack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_stack IMPLEMENTATION.

  METHOD push.
    DATA lr_node TYPE REF TO ts_node.

    CREATE DATA lr_node.
    lr_node->data = iv_key.
    lr_node->next = mr_top.

    mr_top = lr_node.
  ENDMETHOD.                    "push

  METHOD pop.
    CLEAR rv_data.
    CHECK mr_top IS BOUND.
    rv_data = mr_top->data.
    mr_top ?= mr_top->next.
  ENDMETHOD.                    "pop

  METHOD empty.
    rv_flag = boolc( mr_top IS NOT BOUND ).
  ENDMETHOD.                    "empty

ENDCLASS.                    "lcl_stack IMPLEMENTATION

TYPES tv_scale TYPE perct.
CONSTANTS c_default_scale TYPE tv_scale VALUE '0.9'.
TYPES: BEGIN OF ts_diagram_config,
         local_path     TYPE string,
         java_jar       TYPE string,
         java_appl      TYPE string,
         server_url     TYPE string,
         output_mode    TYPE char01,
         skip_dialog    TYPE flag,
         scale          TYPE tv_scale,
         display_source TYPE flag,
       END OF ts_diagram_config.

*----------------------------------------------------------------------*
*       CLASS lcl_file_name DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_name DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS new IMPORTING iv_mode        TYPE char01
                      RETURNING value(ro_file) TYPE REF TO lcl_file_name.
    METHODS constructor IMPORTING iv_mode TYPE char01.
    METHODS dialog RETURNING value(rv_user_action) TYPE i.
    METHODS get_prefix RETURNING value(rv_name) TYPE string
                       RAISING   cx_dynamic_check.
    METHODS get_fullpath RETURNING value(rv_name) TYPE string.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_fullpath,
             title  TYPE string,
             name   TYPE string,
             ext    TYPE string,
             path   TYPE string,
             filter TYPE string,
           END OF ts_fullpath.
    DATA ms_file TYPE ts_fullpath.
ENDCLASS.                    "lcl_file_name DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_plant_uml DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_plant_uml DEFINITION.
  PUBLIC SECTION.
    CONSTANTS c_plantuml_server TYPE string
      VALUE 'http://www.plantuml.com/plantuml/img/'  ##no_text.

    METHODS constructor IMPORTING iv_diagram TYPE string.
    METHODS to_url IMPORTING iv_base_url   TYPE string DEFAULT c_plantuml_server
                   RETURNING value(rv_url) TYPE string
                   RAISING   cx_dynamic_check.
    METHODS output IMPORTING is_cfg TYPE ts_diagram_config RAISING cx_dynamic_check.
  PROTECTED SECTION.
    TYPES tv_base64 TYPE c LENGTH 65.
    CONSTANTS:
      c_standard TYPE tv_base64 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='  ##no_text,
      c_plantuml TYPE tv_base64 VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_0' ##no_text.
    DATA mv_diagram TYPE string.

    METHODS to_xstring IMPORTING iv_string         TYPE string
                       RETURNING value(rv_xstring) TYPE xstring
                       RAISING   cx_dynamic_check.
    METHODS source IMPORTING iv_display_source TYPE flag
                   RETURNING value(rv_source)  TYPE string.

    METHODS png_file_name IMPORTING io_name        TYPE REF TO lcl_file_name
                                    is_cfg         TYPE ts_diagram_config
                          RETURNING value(rv_name) TYPE string.

    METHODS parameter_string IMPORTING io_name         TYPE REF TO lcl_file_name
                                       is_cfg          TYPE ts_diagram_config
                             RETURNING value(rv_param) TYPE string.
    METHODS show_html IMPORTING iv_html TYPE string
                                iv_size TYPE string DEFAULT cl_abap_browser=>xlarge
                      RAISING   cx_dynamic_check.
    METHODS to_png IMPORTING io_name        TYPE REF TO lcl_file_name
                             is_cfg         TYPE ts_diagram_config
                   RETURNING value(rv_name) TYPE string.

ENDCLASS.                    "lcl_plant_uml DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_file DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CONSTANTS:
      c_mode_txt TYPE char01 VALUE space,
      c_mode_png TYPE char01 VALUE 'P'.

    CLASS-METHODS download
      IMPORTING iv_data         TYPE xstring
                io_name         TYPE REF TO lcl_file_name
      RETURNING value(rv_subrc) TYPE sysubrc.
ENDCLASS.                    "lcl_file DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_file_name_dummy DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_name_dummy DEFINITION INHERITING FROM lcl_file_name.
  PUBLIC SECTION.
    METHODS dialog REDEFINITION.
ENDCLASS.                    "lcl_file_name_dummy DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_configuration DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_configuration DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CONSTANTS:
      c_mode_aut TYPE char01 VALUE 'T',  " for ABAP Unit Test
      c_mode_url TYPE char01 VALUE 'U',
      c_mode_txt TYPE char01 VALUE space,
      c_mode_xmi TYPE char01 VALUE 'X',
      c_mode_exe TYPE char01 VALUE 'E'.

    CLASS-METHODS:
      get RETURNING value(rs_cfg) TYPE ts_diagram_config,
      query,
      class_constructor.
  PRIVATE SECTION.
    CONSTANTS c_registry_java_base_key TYPE string VALUE 'SOFTWARE\JavaSoft\Java Runtime Environment'  ##no_text.
    TYPES: BEGIN OF ts_param,
             local_path     TYPE localfile,
             java_jar       TYPE localfile,
             java_appl      TYPE localfile,
             server_url     TYPE localfile,
             output_mode    TYPE char01,
             skip_dialog    TYPE flag,
             scale          TYPE perct,
             display_source TYPE flag,
           END OF ts_param.
    METHODS get_attributes RETURNING value(rt_attr) TYPE sci_atttab.
    METHODS to_radiobutton.
    METHODS from_radiobutton.
    CLASS-DATA gs_cfg TYPE ts_param.
    DATA: mv_mode_url TYPE flag VALUE 'X',
          mv_mode_exe TYPE flag,
          mv_mode_txt TYPE flag.
    METHODS dialog.
    CLASS-METHODS get_java_path RETURNING value(rv_fullpath) TYPE string.
    CLASS-METHODS get_registry_value IMPORTING iv_key          TYPE string
                                               iv_value        TYPE string
                                     EXPORTING ev_subrc        TYPE sysubrc
                                               ev_value TYPE string.
ENDCLASS.                    "lcl_configuration DEFINITION

*----------------------------------------------------------------------*
*       INTERFACE lif_unit_test IMPLEMENTATION
*----------------------------------------------------------------------*
INTERFACE lif_unit_test.
ENDINTERFACE.                    "lif_unit_test IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_dot_diagram DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_dot_diagram DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS new IMPORTING is_config     TYPE ts_diagram_config
                      RETURNING value(ro_dot) TYPE REF TO lcl_dot_diagram.

    METHODS generate IMPORTING it_elem           TYPE lcl_parser=>tt_element
                     RETURNING value(rv_diagram) TYPE string.
  PRIVATE SECTION.
    CONSTANTS c_start TYPE flag VALUE abap_true.

    DATA mv_diagram TYPE string.
    METHODS header IMPORTING is_cfg TYPE ts_diagram_config.
    METHODS footer.
    METHODS node IMPORTING elem      TYPE REF TO lcl_lisp.
    METHODS add IMPORTING iv_code TYPE string.
    METHODS get RETURNING value(rv_dot) TYPE string.
    METHODS get_object_id IMPORTING io_ref        TYPE REF TO object
                          RETURNING value(rv_oid) TYPE i.
    METHODS print IMPORTING io_elem        TYPE REF TO lcl_lisp
                  RETURNING value(rv_node) TYPE string.
    METHODS detach.

ENDCLASS.                    "lcl_dot_diagram DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_ide DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_ide DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    DATA mv_title TYPE string VALUE c_lisp_untitled READ-ONLY.

    CLASS-METHODS:
      init,
      main,
      free,
      pbo,
      pai IMPORTING iv_code        TYPE syucomm
          RETURNING value(rv_flag) TYPE flag.
    METHODS first_output.

    INTERFACES lif_port.
  PRIVATE SECTION.
    CLASS-DATA go_ide TYPE REF TO lcl_ide.

    DATA mv_first TYPE flag VALUE abap_true.
    DATA mo_cont TYPE REF TO lcl_container.
    DATA mo_int TYPE REF TO lcl_lisp_profiler. "The Lisp interpreter

    DATA mo_source TYPE REF TO lcl_editor.
    DATA mo_output TYPE REF TO lcl_editor.
    DATA mo_log TYPE REF TO lcl_editor.
    DATA mo_console TYPE REF TO lcl_editor.
    DATA mo_alv TYPE REF TO cl_salv_table.

    DATA print_offset TYPE i.
    DATA buffer TYPE string.

    METHODS:
      constructor,
      evaluate,
      trace,
      refresh,
      show_docu,
      graphics,
      graph_config,
      free_controls,
      user_command IMPORTING iv_code        TYPE syucomm
                   RETURNING value(rv_flag) TYPE flag.
    METHODS welcome RETURNING value(text) TYPE string.
    METHODS console_header RETURNING value(text) TYPE string.

    METHODS flush RETURNING value(rv_text) TYPE string.

    METHODS writeln IMPORTING text TYPE string.
    METHODS add IMPORTING text TYPE string.

    METHODS previous.
    METHODS next.

ENDCLASS.                    "lcl_ide DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_ide IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_ide IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT:
      mo_cont,

      mo_source
        EXPORTING
          io_container = mo_cont->mo_input
          iv_read_only = abap_false
          iv_toolbar = abap_true,
      mo_output
        EXPORTING
          io_container = mo_cont->mo_output
          iv_toolbar = abap_true,
      mo_log
        EXPORTING
          io_container = mo_cont->mo_log,
      mo_console
        EXPORTING
          io_container = mo_cont->mo_console.
    refresh( ).
  ENDMETHOD.                    "constructor

  METHOD init.
    RETURN.
  ENDMETHOD.                    "init

  METHOD main.
    CREATE OBJECT go_ide.
    CALL SCREEN 100.
  ENDMETHOD.                    "main

  METHOD refresh.
    mo_source->delete_text( ).
    mo_output->delete_text( ).
    mo_log->delete_text( ).
    CREATE OBJECT mo_int   " LISP Interpreter
      EXPORTING
        ii_port = me.
  ENDMETHOD.                    "refresh

  METHOD show_docu.
    CONSTANTS c_url TYPE string VALUE `https://github.com/nomssi/abap_scheme/wiki`  ##no_text.
    cl_abap_browser=>show_url( title = `ABAP Scheme Wiki`
                               url = c_url
                               modal = abap_false
                               buttons = abap_true
                               context_menu = abap_true
                                "size = ms_cfg-browser_size
                               ).
  ENDMETHOD.                    "show_docu

  METHOD graphics.
    DATA code TYPE string.
    DATA ls_cfg TYPE ts_diagram_config.
    DATA lt_elem TYPE lcl_parser=>tt_element.
    DATA lo_uml TYPE REF TO lcl_plant_uml.
    TRY.
        ls_cfg = lcl_configuration=>get( ).

        code = mo_source->to_string( ).
        CHECK code IS NOT INITIAL.

        lt_elem = mo_int->parse( code ).

        CREATE OBJECT lo_uml
          EXPORTING iv_diagram = lcl_dot_diagram=>new( is_config = ls_cfg )->generate( it_elem = lt_elem ).
        lo_uml->output( ls_cfg ).
      CATCH cx_dynamic_check.                           "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.                    "graphics

  METHOD graph_config.
    lcl_configuration=>query( ).
  ENDMETHOD.                    "graph_config

  METHOD lif_port~read.
    DATA lt_fields TYPE STANDARD TABLE OF sval.
    DATA ls_fields TYPE sval.
    DATA lv_user_response TYPE flag.

    rv_input = iv_input.
    CHECK iv_input IS INITIAL.
    ls_fields-tabname = 'ABDBG'.     " Text: Input Line
    ls_fields-fieldname = 'LTEXT'.
    ls_fields-field_obl = 'X'.
    APPEND ls_fields TO lt_fields.

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

    CHECK sy-subrc EQ 0 AND lv_user_response NE 'A'.
    READ TABLE lt_fields INDEX 1 INTO ls_fields.
    CHECK sy-subrc EQ 0.

    rv_input = ls_fields-value.
    mo_console->append_string( rv_input && |\n| ).
  ENDMETHOD.                    "lif_port~read

  METHOD lif_port~write.
    DATA lx_error TYPE REF TO cx_root.
    DATA lo_elem TYPE REF TO lcl_lisp.

    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    CASE element->type.
      WHEN lcl_lisp=>type_abap_table.
        TRY.
            ASSIGN element->data->* TO <lt_table>.
            cl_salv_table=>factory(
              EXPORTING r_container    = mo_cont->mo_alv
              IMPORTING r_salv_table   = mo_alv
              CHANGING  t_table        = <lt_table> ).

            mo_alv->get_functions( )->set_all( abap_true ).
            mo_alv->display( ).

          CATCH cx_root INTO lx_error.
            lcl_lisp=>throw( lx_error->get_text( ) ).
        ENDTRY.

      WHEN lcl_lisp=>type_pair.
        writeln( `(` ).
        lo_elem = element.
        DO.
          ADD 2 TO print_offset.
          lif_port~write( lo_elem->car ).
          SUBTRACT 2 FROM print_offset.
          IF lo_elem->cdr IS NOT BOUND OR lo_elem->cdr EQ lcl_lisp=>nil.
            EXIT.
          ENDIF.
          lo_elem = lo_elem->cdr.
        ENDDO.
        add( ` )` ).
        mo_console->append_string( flush( ) ).

      WHEN OTHERS.
        TRY.
            add( element->write( ) ).
            mo_console->append_string( flush( ) ).
          CATCH cx_root INTO lx_error.
            mo_console->append_string( lx_error->get_text( ) ).
        ENDTRY.
    ENDCASE.
  ENDMETHOD.                    "lif_console~write

  METHOD flush.
    rv_text = buffer.
    CLEAR buffer.
  ENDMETHOD.                    "get

  METHOD add.
    buffer = buffer && text.
  ENDMETHOD.                    "add

  METHOD writeln.
    add( |\\n{ repeat( val = ` ` occ = print_offset ) }{ text }| ).
  ENDMETHOD.                    "writeln

  METHOD welcome.
    text = |==> Welcome to ABAP List Processing!\n|.
  ENDMETHOD.                    "welcome

  METHOD console_header.
    text = |==> ABAP Lisp -- Console { sy-uname } -- { sy-datlo DATE = ENVIRONMENT } { sy-uzeit TIME = ENVIRONMENT }\n|.
  ENDMETHOD.                    "console_header

  METHOD first_output.
    CHECK mv_first EQ abap_true.
    CLEAR mv_first.
    cl_gui_textedit=>set_focus( mo_source ).
    mo_log->append_string( |{ welcome( ) }\n| ).
    mo_console->append_string( console_header( ) ).
  ENDMETHOD.                    "first_output

  METHOD evaluate.
    DATA code TYPE string.
    DATA response TYPE string.
    DATA lx_root TYPE REF TO cx_root.

    TRY.
        code = mo_source->to_string( ).
        response = mo_int->eval_repl( code ).

        mo_output->append_source( code ).
        mo_console->append_source( response ).

        mo_source->push_text( ).
        mo_source->update_status( |[ { mo_int->runtime } µs ] { response }| ).


      CATCH cx_root INTO lx_root.
        response = lx_root->get_text( ).
        mo_source->update_status( |{ response }| ).
    ENDTRY.

    mo_log->append_string( |{ code }\n=> { response }\n| ).
  ENDMETHOD.                    "evaluate

  METHOD trace.
    TYPES: BEGIN OF ts_header,
             user TYPE syuname,
             time TYPE syuzeit,
           END OF ts_header.
    DATA header TYPE ts_header.

    header-user = sy-uname.
    header-time = sy-uzeit.

    gv_lisp_trace = abap_true.
    "cl_demo_output=>begin_section( `ABAP LISP Workbench` ).
    "cl_demo_output=>write( header ).
    " cl_demo_output=>set_mode( cl_demo_output=>text_mode  ).

    "cl_demo_output=>begin_section( `Scheme Code` ).
    "cl_demo_output=>write( mo_source->to_string( ) ).

    "cl_demo_output=>begin_section( `Trace Output` ).

*   Run
    evaluate( ).

    gv_lisp_trace = abap_false.

    "cl_demo_output=>display( ).

  ENDMETHOD.                    "trace

  METHOD free.
    go_ide->free_controls( ).
  ENDMETHOD.                    "free

  METHOD free_controls.
    FREE mo_alv.
    mo_console->free( ).
    mo_log->free( ).
    mo_output->free( ).
    mo_source->free( ).
    mo_cont->free_controls( ).
  ENDMETHOD.                    "free_controls

  METHOD pbo.
    SET PF-STATUS 'STATUS_100'.
    SET TITLEBAR  'TITLE_100' WITH go_ide->mv_title.
    go_ide->first_output( ).
  ENDMETHOD.                    "pbo

  METHOD pai.
    rv_flag = go_ide->user_command( iv_code ).
  ENDMETHOD.                    "pai

  METHOD user_command.
    rv_flag = abap_false.

    CASE iv_code.
      WHEN 'EXECUTE'.
        evaluate( ).
      WHEN 'TRACE'.
        trace( ).
      WHEN 'CLEAR'.
        refresh( ).
      WHEN 'PREV'.
        previous( ).
      WHEN 'NEXT'.
        next( ).
      WHEN 'WIKI'.
        show_docu( ).
      WHEN 'GRAPH'.
        graphics( ).
      WHEN 'GRPHCFG'.
        graph_config( ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    rv_flag = abap_true.
  ENDMETHOD.                    "user_command

  METHOD previous.
    mo_source->pop_text( ).
  ENDMETHOD.                    "previous

  METHOD next.
    mo_source->push_text( ).
  ENDMETHOD.                    "next

ENDCLASS.                    "lcl_ide IMPLEMENTATION

*----------------------------------------------------------------------*
*  MODULE status_0100 OUTPUT
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  lcl_ide=>pbo( ).
ENDMODULE.                    "status_0100 OUTPUT

*----------------------------------------------------------------------*
*  MODULE cancel_0100 INPUT
*----------------------------------------------------------------------*
MODULE cancel_0100 INPUT.
  lcl_ide=>free( ).
  LEAVE PROGRAM.
ENDMODULE.                    "cancel_0100 INPUT

*----------------------------------------------------------------------*
*  MODULE user_command_0100
*----------------------------------------------------------------------*
MODULE user_command_0100.
  CHECK lcl_ide=>pai( g_ok_code ) EQ abap_true.
  CLEAR g_ok_code.
ENDMODULE.                    "user_command_0100

*----------------------------------------------------------------------*
*       CLASS lcl_container IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_container IMPLEMENTATION.

  METHOD constructor.
*   Splitter Container
    CREATE OBJECT mo_splitter_h
      EXPORTING
        link_dynnr = '0100'
        link_repid = sy-repid
        parent     = cl_gui_container=>screen0
        rows       = 1
        columns    = 2.
    mo_splitter_h->set_border( border = cl_gui_cfw=>false ).

    mo_splitter_h->set_column_mode( mode = mo_splitter_h->mode_absolute ).
    mo_splitter_h->set_column_width( id = 1
                                     width = 750 ).
    mo_left = mo_splitter_h->get_container( row = 1
                                             column = 1 ).
    mo_right = mo_splitter_h->get_container( row = 1
                                              column = 2 ).
    CREATE OBJECT mo_splitter_v_h
      EXPORTING
        parent  = mo_right
        rows    = 2
        columns = 1.
    mo_splitter_v_h->set_border( border = cl_gui_cfw=>false ).
    mo_splitter_v_h->set_row_mode( mode = mo_splitter_v_h->mode_relative ).

    mo_output  = mo_splitter_v_h->get_container( row = 1 column = 1 ).
    mo_console = mo_alv = mo_splitter_v_h->get_container( row = 2 column = 1 ).

    CREATE OBJECT mo_splitter_v
      EXPORTING
        parent  = mo_left
        rows    = 2
        columns = 1.
    mo_splitter_v->set_border( border = cl_gui_cfw=>false ).
    mo_splitter_v->set_row_mode( mode = mo_splitter_v->mode_relative ).

    mo_input  = mo_splitter_v->get_container( row = 1 column = 1 ).
    mo_log = mo_splitter_v->get_container( row = 2 column = 1 ).
  ENDMETHOD.                    "constructor

  METHOD free_controls.
    FREE: mo_input,
          mo_output,
          mo_log,
          mo_alv,
          mo_console.
    FREE mo_left.
    FREE mo_splitter_h.
    FREE mo_splitter_v.
    FREE mo_splitter_v_h.
  ENDMETHOD.                    "free_controls

ENDCLASS.                    "lcl_container IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_editor IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_editor IMPLEMENTATION.

  METHOD constructor.
    DATA mode TYPE i.
    io_container->set_visible( abap_true ).
    super->constructor( io_container ).
    set_comments_string( c_comments_string ).
    set_highlight_comments_mode( ).

    IF iv_toolbar EQ abap_true.
      set_toolbar_mode( 1 ).
    ELSE.
      set_toolbar_mode(  0 ).
    ENDIF.
    cl_gui_cfw=>flush( ).

    IF iv_read_only EQ abap_true.
      set_readonly_mode( cl_gui_textedit=>true ).
      mode = 0.
    ELSE.
      mode = 1.
    ENDIF.
    set_statusbar_mode( mode ).
*   Work around to avoid NO DATA dump on first read
    delete_text( ).
    CREATE OBJECT mo_stack.
  ENDMETHOD.                    "constructor

  METHOD append_string.
    set_textstream( |{ to_string( ) }{ iv_text }| ).
  ENDMETHOD.                    "append_string

  METHOD format_input.
    ADD 1 TO mv_counter.
    rv_text = | ${ mv_counter }> { code }\n|.
  ENDMETHOD.                    "format_input

  METHOD append_source.
    append_string( format_input( iv_text ) ).
  ENDMETHOD.                    "append_string

  METHOD to_string.
    get_textstream( IMPORTING text = rv_text ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.                    "to_string

  METHOD update_status.
    DATA lv_text TYPE char72.

    lv_text = iv_string.
    set_status_text( lv_text ).
  ENDMETHOD.                    "update_status

  METHOD push_text.
    DATA code TYPE string.

    code = to_string( ).
    CHECK code NE space.
    mo_stack->push( code ).
    delete_text( ).
  ENDMETHOD.                    "push_text

  METHOD pop_text.
    CHECK mo_stack->empty( ) EQ abap_false.
    code = mo_stack->pop( ).
    delete_text( ).
    append_string( code ).
  ENDMETHOD.                    "pop_text

ENDCLASS.                    "lcl_editor IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_plant_uml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_plant_uml IMPLEMENTATION.

  METHOD constructor.
    mv_diagram = iv_diagram.
  ENDMETHOD.                    "constructor

  METHOD source.
    CLEAR rv_source.
    CHECK iv_display_source EQ abap_true.
    rv_source = |<p>{ mv_diagram }</p>|.
  ENDMETHOD.                    "source

  METHOD show_html.
    cl_abap_browser=>show_html( title = c_lisp_ast_view
                                html_string = iv_html
                                size = iv_size
                                context_menu = abap_true ).
  ENDMETHOD.                    "show_html

  METHOD output.
    DATA lo_name TYPE REF TO lcl_file_name.

    CASE is_cfg-output_mode.
      WHEN lcl_configuration=>c_mode_url.
        show_html( |<img src="{ to_url( ) }"/>\n{ source( is_cfg-display_source ) }| ).

      WHEN lcl_configuration=>c_mode_exe.
        lo_name = lcl_file_name=>new( lcl_file=>c_mode_txt ).
        IF lcl_file=>download( iv_data = to_xstring( mv_diagram )
                               io_name = lo_name ) IS INITIAL.
          show_html( |<img src="{ to_png( io_name = lo_name
                                          is_cfg = is_cfg ) }"/>\n{ source( is_cfg-display_source ) }| ).
        ENDIF.

      WHEN OTHERS.
*       export data as PlantUML source
        lcl_file=>download( io_name = lcl_file_name=>new( is_cfg-output_mode )
                            iv_data = to_xstring( mv_diagram ) ).
    ENDCASE.
  ENDMETHOD.                    "output

  METHOD to_url.
    DATA lv_bin TYPE xstring.
*   for PlantUML Server: Convert to UTF-8, then deflate, then encode (base64 variant)
    cl_abap_gzip=>compress_binary(
      EXPORTING
        raw_in         = to_xstring( mv_diagram )   " UTF-8
        compress_level = 9
      IMPORTING
        gzip_out       = lv_bin ).

    rv_url = iv_base_url &&
             translate( val = cl_http_utility=>encode_x_base64( lv_bin )
                        from = c_standard
                        to =   c_plantuml ).
  ENDMETHOD.                    "to_url

  METHOD to_xstring.
    DATA lo_conv TYPE REF TO cl_abap_conv_out_ce.

    lo_conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    lo_conv->convert( EXPORTING data = iv_string
                      IMPORTING buffer = rv_xstring ).
  ENDMETHOD.                    "to_xstring

  METHOD parameter_string.
    rv_param = |-jar { is_cfg-java_jar } -o { is_cfg-local_path } "{ io_name->get_fullpath( ) }"|.
  ENDMETHOD.                    "parameter_string

  METHOD png_file_name.
    TRY.
        rv_name = |{ is_cfg-local_path }{ io_name->get_prefix( ) }.png|.
      CATCH cx_dynamic_check.
        CLEAR rv_name.
    ENDTRY.
  ENDMETHOD.                    "png_file_name

  METHOD to_png.
    CLEAR rv_name.
    cl_gui_frontend_services=>execute(
      EXPORTING application = is_cfg-java_appl
                parameter = parameter_string( io_name = io_name
                                              is_cfg = is_cfg )
                synchronous = 'X'
      EXCEPTIONS OTHERS = 1 ).
    CHECK sy-subrc EQ 0.
    rv_name = png_file_name( io_name = io_name
                             is_cfg = is_cfg ).
  ENDMETHOD.                    "to_png

ENDCLASS.                    "lcl_plant_uml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_configuration IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_configuration IMPLEMENTATION.

  METHOD class_constructor.
    gs_cfg-java_appl = get_java_path( ).
    gs_cfg-local_path = `C:\Temp\Dokumente\UML\`.                           " PlantUML jar file and output path
    gs_cfg-java_jar = `C:\Temp\Dokumente\UML\plantuml.jar`.
    gs_cfg-server_url = `http://www.plantuml.com/plantuml/img/` ##no_text.  " PlantUML server URL
    gs_cfg-output_mode = c_mode_url.
    gs_cfg-skip_dialog = space.
    gs_cfg-scale = c_default_scale.
    gs_cfg-display_source = abap_false.
  ENDMETHOD.                    "class_constructor

  METHOD get_registry_value.
    cl_gui_frontend_services=>registry_get_value(
      EXPORTING
        root  = cl_gui_frontend_services=>hkey_local_machine
        key   = iv_key
        value = iv_value
      IMPORTING
        reg_value = ev_value
      EXCEPTIONS
        OTHERS               = 5 ).
    ev_subrc = sy-subrc.
  ENDMETHOD.                    "get_registry_value

  METHOD get_java_path.
*   Windows: Local Java installation
    DATA lv_subrc TYPE sysubrc.
    DATA lv_path TYPE string.

    rv_fullpath = `C:\Windows\System32\java`.   " Default
    get_registry_value( EXPORTING iv_key = c_registry_java_base_key
                                  iv_value = 'CurrentVersion'
                       IMPORTING ev_subrc = lv_subrc
                                 ev_value = lv_path ).
    CHECK lv_subrc EQ 0.
    get_registry_value( EXPORTING iv_key = |{ c_registry_java_base_key }\\{ lv_path }|
                                  iv_value = 'JavaHome'
                        IMPORTING ev_subrc = lv_subrc
                                  ev_value = lv_path ).
    CHECK lv_subrc EQ 0.
    rv_fullpath = |{ lv_path }\\bin\\java|.
  ENDMETHOD.                    "get_java_path

  METHOD get_attributes.
    DATA ls_attr TYPE sci_attent.
* Table Type has type 'T' - patterns SCI_PATTERN
*                     ' ' - ?? private attributes?
*                     'I' - ?? Integer?
    DEFINE _add_attr.
      CLEAR ls_attr.
      GET REFERENCE OF &1 INTO ls_attr-ref.
      ls_attr-text = &2.
      ls_attr-kind = &3.
      ls_attr-button_group = &4.
      INSERT ls_attr INTO TABLE rt_attr.
    END-OF-DEFINITION.

    _add_attr: gs_cfg-skip_dialog      'Remember my settings'(c00)     'C' space,
               SY-INDEX                'PlantUML Execution Mode'(c10)  'G' space,
               mv_mode_url             'PlantUML web service'(c11)     'R' 'MOD',
               mv_mode_txt             'Save text file'(c12)           'R' 'MOD',
               mv_mode_exe             'Local PlantUML '(c13)          'R' 'MOD',
                ''                     'PlantUML Settings'(c20)        'G' space,
               gs_cfg-scale            'Scale '(c21)                   'S' space,
               gs_cfg-server_url       'PlantUML Server'(c25)          'S' space,
               gs_cfg-local_path       'Local PlantUML path'(c26)      'S' space,
               gs_cfg-java_jar         'Local PlantUML jar file'(c27)  ' ' space,
               gs_cfg-java_appl        'Local Java path'(c28)          'S' space, " Select-Options
               gs_cfg-display_source   'Display source '(c32)          'C' space.
  ENDMETHOD.                    "get_attributes

  METHOD to_radiobutton.
    mv_mode_url = boolc( gs_cfg-output_mode EQ c_mode_url ).
    mv_mode_exe = boolc( gs_cfg-output_mode EQ c_mode_exe ).
    mv_mode_txt = boolc( gs_cfg-output_mode EQ c_mode_txt ).
  ENDMETHOD.                    "to_radiobutton

  METHOD from_radiobutton.
    IF mv_mode_url EQ abap_true.
      gs_cfg-output_mode = c_mode_url.
    ELSEIF mv_mode_exe EQ abap_true.
      gs_cfg-output_mode = c_mode_exe.
    ELSEIF mv_mode_txt EQ abap_true.
      gs_cfg-output_mode = c_mode_txt.
    ENDIF.
  ENDMETHOD.                    "from_radiobutton

  METHOD get.
    MOVE-CORRESPONDING gs_cfg TO rs_cfg.
  ENDMETHOD.                    "get

  METHOD query.
    DATA lo_cfg TYPE REF TO lcl_configuration.
    CREATE OBJECT lo_cfg.
    lo_cfg->dialog( ).
  ENDMETHOD.                    "query

  METHOD dialog.
    DATA lv_repid TYPE sychar30.

    lv_repid = sy-repid.
    to_radiobutton( ).
    "CHECK gs_cfg-skip_dialog EQ abap_false.
    CHECK cl_ci_query_attributes=>generic(
        p_name       = lv_repid                              " unique screen ID
        p_title      = 'Class Diagram Parameters'            " Screen title
        p_attributes = get_attributes( )                     " Screen fields
        p_display    = abap_false                            " Edit / Display only
       ) EQ abap_false.   " Do not cancel
    from_radiobutton( ).
  ENDMETHOD.                    "dialog

ENDCLASS.                    "lcl_configuration IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_file IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_file IMPLEMENTATION.

  METHOD download.
    rv_subrc = 1.
    CHECK io_name->dialog( ) NE cl_gui_frontend_services=>action_cancel.

    rv_subrc = cl_uml_utilities=>save_xml_local( xml = iv_data
                                                 filename = io_name->get_fullpath( ) ).
  ENDMETHOD.                    "download

ENDCLASS.                    "lcl_file IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_file_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_name IMPLEMENTATION.

  METHOD new.
    CASE iv_mode.
      WHEN lcl_configuration=>c_mode_aut.
        CREATE OBJECT ro_file TYPE lcl_file_name_dummy
          EXPORTING
            iv_mode = iv_mode.
      WHEN OTHERS.
        CREATE OBJECT ro_file TYPE lcl_file_name
          EXPORTING
            iv_mode = iv_mode.
    ENDCASE.
  ENDMETHOD.                    "new

  METHOD constructor.
    CASE iv_mode.
      WHEN lcl_configuration=>c_mode_txt.
        CLEAR ms_file.
        ms_file-title = |Save UML text source|.
        ms_file-ext = |.txt|.
      WHEN OTHERS.
        CLEAR ms_file.
        ms_file-title = |Save As...|.
        ms_file-ext = |.txt|.
    ENDCASE.
  ENDMETHOD.                    "constructor

  METHOD get_prefix.
    rv_name = shift_right( val = ms_file-name
                           places = strlen( ms_file-ext ) ).
  ENDMETHOD.                    "get_prefix

  METHOD get_fullpath.
    rv_name = ms_file-path.
  ENDMETHOD.                    "get_fullpath

  METHOD dialog.
    DATA lv_path TYPE string ##needed.

    CLEAR rv_user_action.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title      = ms_file-title           " Window Title
        default_extension = ms_file-ext             " Default Extension
        file_filter       = ms_file-filter
      CHANGING
        filename = ms_file-name          " File Name to Save
        path = lv_path                   " Path to File
        fullpath = ms_file-path          " Path + File Name
        user_action = rv_user_action
" User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
*   file_encoding =
      EXCEPTIONS
        OTHERS = 0 ).
  ENDMETHOD.                    "dialog

ENDCLASS.                    "lcl_file_name IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_file_name_dummy IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_name_dummy IMPLEMENTATION.

  METHOD dialog.
    ms_file-path = |test.txt|.
    rv_user_action = cl_gui_frontend_services=>action_cancel.
  ENDMETHOD.                    "dialog

ENDCLASS.                    "lcl_file_name_dummy IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_dot_diagram IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_dot_diagram IMPLEMENTATION.

  METHOD generate.
    DATA lo_elem TYPE REF TO lcl_lisp.

    LOOP AT it_elem INTO lo_elem.
      node( elem = lo_elem ).
    ENDLOOP.
    rv_diagram = get( ).
  ENDMETHOD.                    "generate

  METHOD add.
    mv_diagram = mv_diagram && iv_code.
  ENDMETHOD.                    "add

  METHOD header.
    add( |@startuml\n| ).
    add( |scale { is_cfg-scale }\n| ).
    CHECK c_start EQ abap_true.
    add( |start\n| ).
  ENDMETHOD.                    " header

  METHOD print.
    CASE io_elem->type.
      WHEN lcl_lisp=>type_pair.
        rv_node = get_object_id( io_elem ).
      WHEN lcl_lisp=>type_null.
        rv_node = space.
      WHEN lcl_lisp=>type_number.
        rv_node = |{ io_elem->number }|.
      WHEN OTHERS.
        rv_node = io_elem->value.
    ENDCASE.
  ENDMETHOD.                    "print

  METHOD detach.
    CHECK c_start EQ abap_true.
    add( |detach\n| ).
  ENDMETHOD.                    "detach

  METHOD node.
    CASE elem->type.
      WHEN lcl_lisp=>type_pair.

        IF elem->cdr NE lcl_lisp=>nil.
          add( |if ({ print( elem ) }) then ({ get_object_id( elem->car ) })\n| ).
          node( elem->car ).
          detach( ).

          add( |else ({ get_object_id( elem->cdr ) })\n| ).
          node( elem->cdr ).
          add( |endif\n| ).

          detach( ).
        ELSE.
          add( |  :{ print( elem ) };\n| ).
          node( elem->car ).
          detach( ).
        ENDIF.

      WHEN lcl_lisp=>type_null.
*      do nothing

      WHEN lcl_lisp=>type_symbol.
        add( |  :{ print( elem ) }; \n| ).

      WHEN OTHERS.
        add( |  :{ print( elem ) }] \n| ).

    ENDCASE.
  ENDMETHOD.                    " footer

  METHOD footer.
    "    add( |stop\n| ).
    add( |@enduml\n| ).
  ENDMETHOD.                    " footer

  METHOD new.
    CREATE OBJECT ro_dot.
    ro_dot->header( is_config ).
  ENDMETHOD.                    " new

  METHOD get.
    footer( ).
    rv_dot = mv_diagram.
  ENDMETHOD.                    "get

  METHOD get_object_id.
*   Get object ID - internal call
    CALL 'OBJMGR_GET_INFO' ID 'OPNAME' FIELD 'GET_OBJID'
                           ID 'OBJID'  FIELD rv_oid
                           ID 'OBJ'    FIELD io_ref.
  ENDMETHOD.                    "get_object_id

ENDCLASS.                    "lcl_dot_diagram IMPLEMENTATION
