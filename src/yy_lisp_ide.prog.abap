*&---------------------------------------------------------------------*
*&  Include           YY_LISP_IDE
*&---------------------------------------------------------------------*

CONSTANTS:
  c_lisp_untitled   TYPE programm VALUE 'Untitled',
* enable if you uploaded LISP config files or also change c_source_type to 'LISP'
* check: https://github.com/nomssi/abap_scheme/blob/master/editor/README.md
  c_new_abap_editor TYPE flag VALUE abap_false,
  c_source_type     TYPE string VALUE 'LISP'.

CONSTANTS:
  c_lisp_ast_view TYPE cl_abap_browser=>title VALUE 'ABAP Lisp S-Expression Viewer'.

DATA g_ok_code TYPE syucomm.

TYPES: BEGIN OF ts_settings,
         stack      TYPE string_table,
         new_editor TYPE flag,
       END OF ts_settings.

CLASS lcl_stack DEFINITION.
  PUBLIC SECTION.
    TYPES tv_data TYPE string.

    METHODS previous RETURNING VALUE(rv_data) TYPE tv_data.
    METHODS next RETURNING VALUE(rv_data) TYPE tv_data.

    METHODS push IMPORTING iv_key TYPE tv_data.
    "METHODS pop RETURNING VALUE(rv_data) TYPE tv_data.
    METHODS empty RETURNING VALUE(rv_flag) TYPE xsdboolean.

    METHODS serialize RETURNING VALUE(rt_string) TYPE string_table.
    METHODS deserialize IMPORTING it_string      TYPE string_table.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_node,
             data TYPE tv_data,
             next TYPE REF TO data,
             prev TYPE REF TO data,
           END OF ts_node.

    DATA mr_top TYPE REF TO ts_node.
    DATA mr_last TYPE REF TO ts_node.
    DATA mr_head TYPE REF TO ts_node.
ENDCLASS.

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

INTERFACE lif_source_editor.
  METHODS clear.
  METHODS push_text.
  METHODS previous.
  METHODS next.
  METHODS to_string RETURNING VALUE(rv_text) TYPE string.
  METHODS update_status IMPORTING iv_string TYPE string.
  METHODS setup IMPORTING is_settings TYPE ts_settings.

  METHODS set_focus.
  METHODS free RETURNING VALUE(rt_string) TYPE string_table.
ENDINTERFACE.

CLASS lcl_source DEFINITION INHERITING FROM cl_gui_sourceedit.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_container TYPE REF TO cl_gui_container
                                  iv_read_only TYPE flag DEFAULT abap_false
                                  iv_toolbar   TYPE flag DEFAULT abap_false.
    INTERFACES lif_source_editor.
  PRIVATE SECTION.
    DATA mo_stack TYPE REF TO lcl_stack.
ENDCLASS.

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

    INTERFACES lif_source_editor.

    METHODS append_string IMPORTING iv_text TYPE string.
  PRIVATE SECTION.
    DATA mv_counter TYPE i.
    DATA mo_stack TYPE REF TO lcl_stack.

    METHODS format_input IMPORTING code           TYPE string
                         RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.                    "lcl_editor DEFINITION

CLASS lcl_stack IMPLEMENTATION.

  METHOD push.
    DATA lo_node TYPE REF TO ts_node.
    DATA lo_last TYPE REF TO ts_node.

    IF mr_top IS BOUND.
      lo_last = mr_top.
      mr_top = NEW ts_node( data = iv_key
                            prev = lo_last->prev
                            next = lo_last ).
      IF lo_last->prev IS BOUND.
        lo_node ?= lo_last->prev.
        lo_node->next = mr_top.
      ENDIF.
      lo_last->prev = mr_top.
    ELSE.
      mr_top = NEW ts_node( data = iv_key
                            next = mr_top ).
    ENDIF.
  ENDMETHOD.

*  METHOD pop.
*    CLEAR rv_data.
*    CHECK mr_top IS BOUND.
*    rv_data = mr_top->data.
*    mr_top ?= mr_top->next.
*  ENDMETHOD.

  METHOD previous.
    CLEAR rv_data.
    CHECK mr_top IS BOUND.
    rv_data = mr_top->data.
    CHECK mr_top->next IS BOUND.
    mr_top ?= mr_top->next.
  ENDMETHOD.

  METHOD next.
    CLEAR rv_data.
    CHECK mr_top IS BOUND AND mr_top->prev IS BOUND.
    mr_top ?= mr_top->prev.
    rv_data = mr_top->data.
  ENDMETHOD.

  METHOD empty.
    rv_flag = xsdbool( mr_top IS NOT BOUND ).
  ENDMETHOD.

  METHOD deserialize.
    LOOP AT it_string INTO DATA(lv_string).
      push( lv_string ).
    ENDLOOP.
  ENDMETHOD.

  METHOD serialize.
    DATA lr_node TYPE REF TO ts_node.

    CLEAR rt_string.
    lr_node = mr_top.
    WHILE lr_node IS BOUND.
      APPEND lr_node->data TO rt_string.
      lr_node ?= lr_node->next.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.

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

CLASS lcl_file_name DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS new IMPORTING iv_mode        TYPE char01
                      RETURNING VALUE(ro_file) TYPE REF TO lcl_file_name.
    METHODS constructor IMPORTING iv_mode TYPE char01.
    METHODS dialog RETURNING VALUE(rv_user_action) TYPE i.
    METHODS get_prefix RETURNING VALUE(rv_name) TYPE string
                       RAISING   cx_dynamic_check.
    METHODS get_fullpath RETURNING VALUE(rv_name) TYPE string.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_fullpath,
             title  TYPE string,
             name   TYPE string,
             ext    TYPE string,
             path   TYPE string,
             filter TYPE string,
           END OF ts_fullpath.
    DATA ms_file TYPE ts_fullpath.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_plant_uml DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_plant_uml DEFINITION.
  PUBLIC SECTION.
    CONSTANTS c_plantuml_server TYPE string
      VALUE 'http://www.plantuml.com/plantuml/img/'  ##NO_TEXT.

    METHODS constructor IMPORTING iv_diagram TYPE string.
    METHODS to_url IMPORTING iv_base_url   TYPE string DEFAULT c_plantuml_server
                   RETURNING VALUE(rv_url) TYPE string
                   RAISING   cx_dynamic_check.
    METHODS output IMPORTING is_cfg TYPE ts_diagram_config RAISING cx_dynamic_check.
  PROTECTED SECTION.
    TYPES tv_base64 TYPE c LENGTH 65.
    CONSTANTS:
      c_standard TYPE tv_base64 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='  ##NO_TEXT,
      c_plantuml TYPE tv_base64 VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_0' ##NO_TEXT.
    DATA mv_diagram TYPE string.

    METHODS to_xstring IMPORTING iv_string         TYPE string
                       RETURNING VALUE(rv_xstring) TYPE xstring
                       RAISING   cx_dynamic_check.
    METHODS source IMPORTING iv_display_source TYPE flag
                   RETURNING VALUE(rv_source)  TYPE string.

    METHODS png_file_name IMPORTING io_name        TYPE REF TO lcl_file_name
                                    is_cfg         TYPE ts_diagram_config
                          RETURNING VALUE(rv_name) TYPE string.

    METHODS parameter_string IMPORTING io_name         TYPE REF TO lcl_file_name
                                       is_cfg          TYPE ts_diagram_config
                             RETURNING VALUE(rv_param) TYPE string.
    METHODS show_html IMPORTING iv_html TYPE string
                                iv_size TYPE string DEFAULT cl_abap_browser=>xlarge
                      RAISING   cx_dynamic_check.
    METHODS to_png IMPORTING io_name        TYPE REF TO lcl_file_name
                             is_cfg         TYPE ts_diagram_config
                   RETURNING VALUE(rv_name) TYPE string.

ENDCLASS.                    "lcl_plant_uml DEFINITION

CLASS lcl_file DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CONSTANTS:
      c_mode_txt TYPE char01 VALUE space,
      c_mode_png TYPE char01 VALUE 'P'.

    CLASS-METHODS download
      IMPORTING iv_data         TYPE xstring
                io_name         TYPE REF TO lcl_file_name
      RETURNING VALUE(rv_subrc) TYPE sysubrc.
ENDCLASS.                    "lcl_file DEFINITION

CLASS lcl_file_name_dummy DEFINITION INHERITING FROM lcl_file_name.
  PUBLIC SECTION.
    METHODS dialog REDEFINITION.
ENDCLASS.

CLASS lcl_configuration DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CONSTANTS:
      c_mode_aut TYPE char01 VALUE 'T',  " for ABAP Unit Test
      c_mode_url TYPE char01 VALUE 'U',
      c_mode_txt TYPE char01 VALUE space,
      c_mode_xmi TYPE char01 VALUE 'X',
      c_mode_exe TYPE char01 VALUE 'E'.

    CLASS-METHODS:
      get RETURNING VALUE(rs_cfg) TYPE ts_diagram_config,
      query,
      class_constructor.
  PRIVATE SECTION.
    CONSTANTS c_registry_java_base_key TYPE string VALUE 'SOFTWARE\JavaSoft\Java Runtime Environment'  ##NO_TEXT.
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
    METHODS get_attributes RETURNING VALUE(rt_attr) TYPE sci_atttab.
    METHODS to_radiobutton.
    METHODS from_radiobutton.
    CLASS-DATA gs_cfg TYPE ts_param.
    DATA: mv_mode_url TYPE flag VALUE 'X',
          mv_mode_exe TYPE flag,
          mv_mode_txt TYPE flag.
    METHODS dialog.
    CLASS-METHODS get_java_path RETURNING VALUE(rv_fullpath) TYPE string.
    CLASS-METHODS get_registry_value IMPORTING iv_key          TYPE string
                                               iv_value        TYPE string
                                     EXPORTING ev_subrc        TYPE sysubrc
                                     RETURNING VALUE(rv_value) TYPE string.
ENDCLASS.

*----------------------------------------------------------------------*
*       INTERFACE lif_unit_test IMPLEMENTATION
*----------------------------------------------------------------------*
INTERFACE lif_unit_test.
ENDINTERFACE.                    "lif_unit_test IMPLEMENTATION

CLASS lcl_dot_diagram DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS new IMPORTING is_config     TYPE ts_diagram_config
                      RETURNING VALUE(ro_dot) TYPE REF TO lcl_dot_diagram.

    METHODS generate IMPORTING it_elem           TYPE lcl_parser=>tt_element
                     RETURNING VALUE(rv_diagram) TYPE string.
  PRIVATE SECTION.
    CONSTANTS c_start TYPE flag VALUE abap_true.

    DATA mv_diagram TYPE string.
    METHODS header IMPORTING is_cfg TYPE ts_diagram_config.
    METHODS footer.
    METHODS node IMPORTING elem      TYPE REF TO lcl_lisp.
    METHODS add IMPORTING iv_code TYPE string.
    METHODS get RETURNING VALUE(rv_dot) TYPE string.
    METHODS get_object_id IMPORTING io_ref        TYPE REF TO object
                          RETURNING VALUE(rv_oid) TYPE i.
    METHODS print IMPORTING io_elem        TYPE REF TO lcl_lisp
                  RETURNING VALUE(rv_node) TYPE string.
    METHODS detach.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_ide DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_ide DEFINITION INHERITING FROM lcl_lisp_buffered_port CREATE PRIVATE.
  PUBLIC SECTION.
    DATA mv_title TYPE string VALUE c_lisp_untitled READ-ONLY.

    CLASS-METHODS:
      init,
      main,
      free,
      pbo,
      pai IMPORTING iv_code        TYPE syucomm
          RETURNING VALUE(rv_flag) TYPE flag.
    METHODS first_output.
    METHODS read REDEFINITION.
    METHODS write REDEFINITION.
  PRIVATE SECTION.
    CLASS-DATA go_ide TYPE REF TO lcl_ide.

    DATA ms_settings TYPE ts_settings.
    DATA mv_first TYPE flag VALUE abap_true.
    DATA mo_cont TYPE REF TO lcl_container.
    DATA mo_int TYPE REF TO lcl_lisp_profiler. "The Lisp interpreter

    DATA mi_source TYPE REF TO lif_source_editor.
    DATA mo_output TYPE REF TO lcl_editor.
    DATA mo_log TYPE REF TO lcl_editor.
    DATA mo_console TYPE REF TO lcl_editor.
    DATA mo_alv TYPE REF TO cl_salv_table.

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
                   RETURNING VALUE(rv_flag) TYPE flag.
    METHODS welcome RETURNING VALUE(text) TYPE string.
    METHODS console_header RETURNING VALUE(text) TYPE string.

    METHODS new_source_editor IMPORTING io_cont          TYPE REF TO cl_gui_container
                              RETURNING VALUE(ri_source) TYPE REF TO lif_source_editor.
    METHODS previous.
    METHODS next.

    METHODS read_settings.
    METHODS save_settings.
    METHODS post_settings IMPORTING handle TYPE REF TO zcl_lisp_area
                          RAISING   cx_shm_error cx_dynamic_check.

ENDCLASS.                    "lcl_ide DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_ide IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_ide IMPLEMENTATION.

  METHOD constructor.
    super->constructor( iv_input = abap_true
                        iv_output = abap_true ).
    read_settings( ).
    "VALUE #( stack = serialize( )
    CREATE OBJECT:
      mo_cont,

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

    mi_source = new_source_editor( mo_cont->mo_input ).
    refresh( ).
  ENDMETHOD.                    "constructor

  METHOD read_settings.
    DATA params TYPE REF TO zcl_lisp_shm_root.
    DATA: handle TYPE REF TO zcl_lisp_area,
          exc    TYPE REF TO cx_root.

    TRY.

        TRY.
           handle = zcl_lisp_area=>attach_for_read( ).
        CATCH cx_shm_no_active_version.
          WAIT UP TO 1 SECONDS.
          handle = zcl_lisp_area=>attach_for_read( ).
        ENDTRY.

        params = handle->root.
        ms_settings = params->load( ).
        handle->detach( ).
      CATCH cx_shm_attach_error INTO exc.

        TRY.
            post_settings( zcl_lisp_area=>attach_for_write( ) ).
          CATCH cx_shm_error cx_dynamic_check INTO exc.
            MESSAGE exc TYPE 'S'.
        ENDTRY.

    ENDTRY.

  ENDMETHOD.

  METHOD save_settings.
    DATA handle TYPE REF TO zcl_lisp_area.
    DATA params TYPE REF TO zcl_lisp_shm_root.
    DATA exc    TYPE REF TO cx_root.

    TRY.
        TRY.
            handle = zcl_lisp_area=>attach_for_update( ).
          CATCH cx_shm_no_active_version.
            WAIT UP TO 1 SECONDS.
            handle = zcl_lisp_area=>attach_for_update( ).
        ENDTRY.

        params ?= handle->get_root( ).
        IF params IS NOT BOUND.
          post_settings( handle ).
        ELSE.
          params->save( ms_settings ).
          handle->detach_commit( ).
        ENDIF.
      CATCH cx_shm_error cx_dynamic_check INTO exc.
        MESSAGE exc TYPE 'S'.
    ENDTRY.
  ENDMETHOD.

  METHOD post_settings.
    DATA params TYPE REF TO zcl_lisp_shm_root.

    CREATE OBJECT params AREA HANDLE handle.
    params->save( ms_settings ).

    handle->set_root( params ).
    handle->detach_commit( ).
  ENDMETHOD.

  METHOD new_source_editor.
    DATA gui_support TYPE boolean.
*   Check for frontend support for the new ABAP Editor
    cl_gui_frontend_services=>check_gui_support(
      EXPORTING
        component            = 'abapeditor'                 "#EC NOTEXT
        feature_name         = 'ab4'                        "#EC NOTEXT
      RECEIVING
        result               = gui_support
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        unknown_error        = 5
        OTHERS               = 6 ).
    IF sy-subrc NE 0 OR gui_support NE abap_true OR c_new_abap_editor NE abap_true.
      CREATE OBJECT ri_source TYPE lcl_editor
        EXPORTING
          io_container = io_cont
          iv_read_only = abap_false
          iv_toolbar   = abap_true.

    ELSE.
      CREATE OBJECT ri_source TYPE lcl_source
        EXPORTING
          io_container = io_cont
          iv_read_only = abap_false
          iv_toolbar   = abap_true.
    ENDIF.
    ri_source->setup( ms_settings ).

  ENDMETHOD.

  METHOD init.
    RETURN.
  ENDMETHOD.

  METHOD main.
    CREATE OBJECT go_ide.
    CALL SCREEN 100.
  ENDMETHOD.                    "main

  METHOD refresh.
    mi_source->clear( ).
    mo_output->delete_text( ).
    mo_log->delete_text( ).
    CREATE OBJECT mo_int
      EXPORTING io_port = me  " LISP Interpreter
                ii_log = me.
  ENDMETHOD.                    "refresh

  METHOD show_docu.
    CONSTANTS c_url TYPE string VALUE `https://github.com/nomssi/abap_scheme/wiki`  ##NO_TEXT.
    cl_abap_browser=>show_url( title = `ABAP Scheme Wiki`
                               url = c_url
                               modal = abap_false
                               buttons = abap_true
                               context_menu = abap_true
                                "size = ms_cfg-browser_size
                               ).
  ENDMETHOD.

  METHOD graphics.
    DATA lx_root TYPE REF TO cx_root.
    TRY.
        DATA(ls_cfg) = lcl_configuration=>get( ).

        DATA(code) = mi_source->to_string( ).
        CHECK code IS NOT INITIAL.

        DATA(lt_elem) = mo_int->parse( code ).

        NEW lcl_plant_uml( lcl_dot_diagram=>new( is_config = ls_cfg
                              )->generate( it_elem = lt_elem ) )->output( ls_cfg ).
      CATCH cx_root INTO lx_root.
        mi_source->update_status( lx_root->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD graph_config.
    lcl_configuration=>query( ).
  ENDMETHOD.

  METHOD read.
    rv_input = super->read( ).
    mo_console->append_string( rv_input && |\n| ).
  ENDMETHOD.

  METHOD write.
    DATA lx_error TYPE REF TO cx_root.
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

      WHEN OTHERS.
        super->write( element ).

    ENDCASE.
  ENDMETHOD.                    "lif_console~write

  METHOD welcome.
    text = |==> Welcome to ABAP List Processing!\n|.
  ENDMETHOD.                    "welcome

  METHOD console_header.
    text = |==> ABAP Lisp -- Console { sy-uname } -- { sy-datlo DATE = ENVIRONMENT } { sy-uzeit TIME = ENVIRONMENT }\n|.
  ENDMETHOD.

  METHOD first_output.
    CHECK mv_first EQ abap_true.
    CLEAR mv_first.
    mi_source->set_focus( ).
    mo_log->append_string( |{ welcome( ) }\n| ).
    mo_console->append_string( console_header( ) ).
  ENDMETHOD.                    "first_output

  METHOD evaluate.
    TRY.
        DATA(code) = mi_source->to_string( ).
        DATA(response) = mo_int->eval_repl( code ).

        mo_output->append_source( code ).
        mo_console->append_source( response ).

        mi_source->push_text( ).
        mi_source->update_status( |[ { mo_int->runtime } µs ] { response }| ).

      CATCH cx_root INTO DATA(lx_root).
        response = lx_root->get_text( ).
        mi_source->update_status( response ).
    ENDTRY.

    mo_log->append_string( |{ code }\n=> { response }\n| ).
  ENDMETHOD.                    "evaluate

  METHOD trace.
    TYPES: BEGIN OF ts_header,
             user TYPE syuname,
             time TYPE syuzeit,
           END OF ts_header.
    DATA(header) = VALUE ts_header( user = sy-uname
                                    time = sy-uzeit ).
    gv_lisp_trace = abap_true.
    cl_demo_output=>begin_section( `ABAP LISP Workbench` ).
    cl_demo_output=>write( header ).
    " cl_demo_output=>set_mode( cl_demo_output=>text_mode  ).

    cl_demo_output=>begin_section( `Scheme Code` ).
    cl_demo_output=>write( mi_source->to_string( ) ).

    cl_demo_output=>begin_section( `Trace Output` ).

*   Run
    evaluate( ).

    gv_lisp_trace = abap_false.

    cl_demo_output=>display( ).

  ENDMETHOD.

  METHOD free.
    go_ide->free_controls( ).
  ENDMETHOD.                    "free

  METHOD free_controls.
    FREE mo_alv.
    mo_console->free( ).
    mo_log->free( ).
    mo_output->free( ).
    ms_settings-stack = mi_source->free( ).
    mo_cont->free_controls( ).
    save_settings( ).
  ENDMETHOD.                    "free_controls

  METHOD pbo.
    SET PF-STATUS 'STATUS_100'.
    SET TITLEBAR 'TITLE_100' WITH go_ide->mv_title.
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
    mi_source->previous( ).
  ENDMETHOD.

  METHOD next.
    mi_source->next( ).
  ENDMETHOD.

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
    mo_splitter_h = NEW #( link_dynnr = '0100'
                            link_repid = sy-repid
                            parent     = cl_gui_container=>screen0
                            rows       = 1
                            columns    = 2 ).
    mo_splitter_h->set_border( border = cl_gui_cfw=>false ).

    mo_splitter_h->set_column_mode( mode = mo_splitter_h->mode_absolute ).
    mo_splitter_h->set_column_width( id = 1
                                     width = 750 ).
    mo_left = mo_splitter_h->get_container( row = 1
                                             column = 1 ).
    mo_right = mo_splitter_h->get_container( row = 1
                                              column = 2 ).
    mo_splitter_v_h = NEW #( parent  = mo_right
                              rows    = 2
                              columns = 1 ).
    mo_splitter_v_h->set_border( border = cl_gui_cfw=>false ).
    mo_splitter_v_h->set_row_mode( mode = mo_splitter_v_h->mode_relative ).

    mo_output  = mo_splitter_v_h->get_container( row = 1 column = 1 ).
    mo_console = mo_alv = mo_splitter_v_h->get_container( row = 2 column = 1 ).

    mo_splitter_v = NEW #( parent  = mo_left
                           rows    = 2
                           columns = 1 ).
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
    set_toolbar_mode( SWITCH #( iv_toolbar WHEN abap_true THEN 1 ELSE 0 ) ).
    cl_gui_cfw=>flush( ).

    IF iv_read_only EQ abap_true.
      set_readonly_mode( cl_gui_textedit=>true ).
      mode = 0.
    ELSE.
      mode = 1.
    ENDIF.
    set_statusbar_mode( mode ).
*   Work around to avoid NO DATA dump on first read
    lif_source_editor~clear( ).
    mo_stack = NEW #( ).
  ENDMETHOD.                    "constructor

  METHOD lif_source_editor~clear.
    delete_text( ).
  ENDMETHOD.                    "append_string

  METHOD append_string.
    set_textstream( |{ lif_source_editor~to_string( ) }{ iv_text }| ).
  ENDMETHOD.

  METHOD format_input.
    ADD 1 TO mv_counter.
    rv_text = | ${ mv_counter }> { code }\n|.
  ENDMETHOD.                    "format_input

  METHOD append_source.
    append_string( format_input( iv_text ) ).
  ENDMETHOD.                    "append_string

  METHOD lif_source_editor~to_string.
    get_textstream( IMPORTING text = rv_text
                    EXCEPTIONS OTHERS = 1 ).
    CHECK sy-subrc EQ 0.
    cl_gui_cfw=>flush( ).
  ENDMETHOD.                    "to_string

  METHOD lif_source_editor~update_status.
    set_status_text( CONV char72( iv_string ) ).
  ENDMETHOD.                    "update_status

  METHOD lif_source_editor~push_text.
    DATA(code) = lif_source_editor~to_string( ).
    CHECK code NE space.
    mo_stack->push( code ).
    lif_source_editor~clear( ).
  ENDMETHOD.

*  METHOD lif_source_editor~pop_text.
*    CHECK mo_stack->empty( ) EQ abap_false.
*    code = mo_stack->pop( ).
*    lif_source_editor~clear( ).
*    append_string( code ).
*  ENDMETHOD.

  METHOD lif_source_editor~previous.
    lif_source_editor~clear( ).
    append_string( mo_stack->previous( ) ).
  ENDMETHOD.

  METHOD lif_source_editor~next.
    lif_source_editor~clear( ).
    append_string( mo_stack->next( ) ).
  ENDMETHOD.

  METHOD lif_source_editor~set_focus.
    set_focus( EXPORTING control = me
               EXCEPTIONS OTHERS = 0 ).
  ENDMETHOD.

  METHOD lif_source_editor~free.
    free( ).
    rt_string = mo_stack->serialize( ).
  ENDMETHOD.

  METHOD lif_source_editor~setup.
    mo_stack->deserialize( is_settings-stack ).
  ENDMETHOD.

ENDCLASS.                    "lcl_editor IMPLEMENTATION

CLASS lcl_source IMPLEMENTATION.

  METHOD constructor.
    DATA mode TYPE i.
    DATA exception_name TYPE string.

    io_container->set_visible( abap_true ).
    super->constructor(
      EXPORTING
        parent = io_container
        max_number_chars = '255'
      EXCEPTIONS
        error_cntl_create      = 1
        error_dp_create        = 2
        gui_type_not_supported = 3
        error_cntl_init        = 4 ).

    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1.
          exception_name = 'ERROR_CNTL_CREATE'.
        WHEN 2.
          exception_name = 'ERROR_DP_CREATE'.
        WHEN 3.
          exception_name = 'GUI_TYPE_NOT_SUPPORTED'.
        WHEN 4.
          exception_name = 'ERROR_CNTL_INIT'.
      ENDCASE.
      RAISE EXCEPTION TYPE cx_coverage_editor_problem
        EXPORTING
          exception_name = exception_name.
    ENDIF.

    set_source_type( c_source_type ).
    set_toolbar_mode( SWITCH #( iv_toolbar WHEN abap_true THEN 1 ELSE 0 ) ).
    cl_gui_cfw=>flush( ).

    IF iv_read_only EQ abap_true.
      set_readonly_mode( cl_gui_textedit=>true ).
      mode = 0.
    ELSE.
      mode = 1.
    ENDIF.
    set_statusbar_mode( mode ).
    set_actual_name( c_lisp_untitled ).
    upload_properties( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
*      MESSAGE e215(ed).
    ENDIF.
    create_document( ).

*    register_event_context_menu( register      = 1
*                                 local_entries = 1 ).

*    SET HANDLER on_context_menu FOR me.
*    SET HANDLER on_context_menu_selected FOR me.

*   Work around to avoid NO DATA dump on first read
    lif_source_editor~clear( ).

    mo_stack = NEW #( ).
  ENDMETHOD.                    "constructor

  METHOD lif_source_editor~set_focus.
    set_focus( EXPORTING control = me
               EXCEPTIONS OTHERS = 0 ).
  ENDMETHOD.

  METHOD lif_source_editor~free.
    free( ).
    rt_string = mo_stack->serialize( ).
  ENDMETHOD.

  METHOD lif_source_editor~setup.
    mo_stack->deserialize( is_settings-stack ).
  ENDMETHOD.

  METHOD lif_source_editor~to_string.
    DATA lt_text TYPE STANDARD TABLE OF string.

    get_text( IMPORTING table = lt_text
              EXCEPTIONS OTHERS = 0 ).
    "cl_gui_cfw=>flush( ).
    rv_text = concat_lines_of( table = lt_text sep = |\n| ).
  ENDMETHOD.                    "to_string

  METHOD lif_source_editor~update_status.
    MESSAGE iv_string TYPE 'S'.
  ENDMETHOD.                    "update_status

  METHOD lif_source_editor~clear.
    DATA lt_text TYPE STANDARD TABLE OF string.

    set_text( EXPORTING table = lt_text
              EXCEPTIONS OTHERS = 0 ).
  ENDMETHOD.

  METHOD lif_source_editor~push_text.
    DATA(code) = lif_source_editor~to_string( ).
    CHECK code NE space.
    mo_stack->push( code ).
    lif_source_editor~clear( ).
  ENDMETHOD.

  METHOD lif_source_editor~previous.
    DATA lt_text TYPE STANDARD TABLE OF string.

    APPEND mo_stack->previous( ) TO lt_text.
    set_text( EXPORTING table = lt_text
              EXCEPTIONS OTHERS = 0 ).
  ENDMETHOD.

  METHOD lif_source_editor~next.
    DATA lt_text TYPE STANDARD TABLE OF string.

    APPEND mo_stack->next( ) TO lt_text.
    set_text( EXPORTING table = lt_text
              EXCEPTIONS OTHERS = 0 ).
  ENDMETHOD.

*  METHOD lif_source_editor~pop_text.
*    DATA lt_text TYPE STANDARD TABLE OF string.
*
*    CHECK mo_stack->empty( ) EQ abap_false.
*    code = mo_stack->pop( ).
**    clear( ).
*    APPEND code TO lt_text.
*    set_text( EXPORTING table = lt_text
*              EXCEPTIONS OTHERS = 0 ).
*  ENDMETHOD.

ENDCLASS.

CLASS lcl_plant_uml IMPLEMENTATION.

  METHOD constructor.
    mv_diagram = iv_diagram.
  ENDMETHOD.                    "constructor

  METHOD source.
    CLEAR rv_source.
    CHECK iv_display_source EQ abap_true.
    rv_source = |<p>{ mv_diagram }</p>|.
  ENDMETHOD.

  METHOD show_html.
    cl_abap_browser=>show_html( title = c_lisp_ast_view
                                html_string = iv_html
                                size = iv_size
                                context_menu = abap_true ).
  ENDMETHOD.

  METHOD output.
    CASE is_cfg-output_mode.
      WHEN lcl_configuration=>c_mode_url.
        show_html( |<img src="{ to_url( ) }"/>\n{ source( is_cfg-display_source ) }| ).

      WHEN lcl_configuration=>c_mode_exe.
        DATA(lo_name) = lcl_file_name=>new( lcl_file=>c_mode_txt ).
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
    cl_abap_conv_out_ce=>create( encoding = 'UTF-8' )->convert( EXPORTING data = iv_string
                                                                IMPORTING buffer = rv_xstring ).
  ENDMETHOD.                    "to_xstring

  METHOD parameter_string.
    rv_param = |-jar { is_cfg-java_jar } -o { is_cfg-local_path } "{ io_name->get_fullpath( ) }"|.
  ENDMETHOD.

  METHOD png_file_name.
    TRY.
        rv_name = |{ is_cfg-local_path }{ io_name->get_prefix( ) }.png|.
      CATCH cx_dynamic_check.
        CLEAR rv_name.
    ENDTRY.
  ENDMETHOD.

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
  ENDMETHOD.

ENDCLASS.

CLASS lcl_configuration IMPLEMENTATION.

  METHOD class_constructor.
    gs_cfg = VALUE #( java_appl = get_java_path( )
                      local_path = `C:\Temp\Dokumente\UML\`                           " PlantUML jar file and output path
                      java_jar = `C:\Temp\Dokumente\UML\plantuml.jar`
                      server_url = `http://www.plantuml.com/plantuml/img/` ##NO_TEXT  " PlantUML server URL
                      output_mode = c_mode_url
                      skip_dialog = space
                      scale = c_default_scale
                      display_source = abap_false ).
  ENDMETHOD.

  METHOD get_registry_value.
    cl_gui_frontend_services=>registry_get_value(
      EXPORTING
        root  = cl_gui_frontend_services=>hkey_local_machine
        key   = iv_key
        value = iv_value
      IMPORTING
        reg_value = rv_value
      EXCEPTIONS
        OTHERS               = 5 ).
    ev_subrc = sy-subrc.
  ENDMETHOD.

  METHOD get_java_path.
*   Windows: Local Java installation
    DATA lv_subrc TYPE sysubrc.

    rv_fullpath = `C:\Windows\System32\java`.   " Default
    DATA(lv_path) = get_registry_value( EXPORTING iv_key = c_registry_java_base_key
                                                  iv_value = 'CurrentVersion'
                                        IMPORTING ev_subrc = lv_subrc ).
    CHECK lv_subrc EQ 0.
    lv_path = get_registry_value( EXPORTING iv_key = |{ c_registry_java_base_key }\\{ lv_path }|
                                            iv_value = 'JavaHome'
                                  IMPORTING ev_subrc = lv_subrc ).
    CHECK lv_subrc EQ 0.
    rv_fullpath = |{ lv_path }\\bin\\java|.
  ENDMETHOD.

  METHOD get_attributes.
* Table Type has type 'T' - patterns SCI_PATTERN
*                     ' ' - ?? private attributes?
*                     'I' - ?? Integer?
    rt_attr = VALUE #(
     ( ref = REF #( gs_cfg-skip_dialog )    text = 'Remember my settings'(c00)    kind = 'C' )
     ( ref = REF #( sy-index )              text = 'PlantUML Execution Mode'(c10) kind = 'G' ) " Group
     ( ref = REF #( mv_mode_url )           text = 'PlantUML web service'(c11)    kind = 'R' button_group = 'MOD' )
     ( ref = REF #( mv_mode_txt )           text = 'Save text file'(c12)          kind = 'R' button_group = 'MOD' )
     ( ref = REF #( mv_mode_exe )           text = 'Local PlantUML '(c13)         kind = 'R' button_group = 'MOD' )
     ( ref = REF #( '' )                    text = 'PlantUML Settings'(c20)       kind = 'G' )
     ( ref = REF #( gs_cfg-scale )          text = 'Scale '(c21)                  kind = 'S' )
     ( ref = REF #( gs_cfg-server_url )     text = 'PlantUML Server'(c25)         kind = 'S' )
     ( ref = REF #( gs_cfg-local_path )     text = 'Local PlantUML path'(c26)     kind = 'S' )
     ( ref = REF #( gs_cfg-java_jar )       text = 'Local PlantUML jar file'(c27) kind = ' ' )
     ( ref = REF #( gs_cfg-java_appl )      text = 'Local Java path'(c28)         kind = 'S' ) " Select-Options
     ( ref = REF #( gs_cfg-display_source ) text = 'Display source '(c32)         kind = 'C' )    ).
  ENDMETHOD.

  METHOD to_radiobutton.
    mv_mode_url = xsdbool( gs_cfg-output_mode EQ c_mode_url ).
    mv_mode_exe = xsdbool( gs_cfg-output_mode EQ c_mode_exe ).
    mv_mode_txt = xsdbool( gs_cfg-output_mode EQ c_mode_txt ).
  ENDMETHOD.

  METHOD from_radiobutton.
    IF mv_mode_url EQ abap_true.
      gs_cfg-output_mode = c_mode_url.
    ELSEIF mv_mode_exe EQ abap_true.
      gs_cfg-output_mode = c_mode_exe.
    ELSEIF mv_mode_txt EQ abap_true.
      gs_cfg-output_mode = c_mode_txt.
    ENDIF.
  ENDMETHOD.

  METHOD get.
    rs_cfg = CORRESPONDING #( gs_cfg ).
  ENDMETHOD.

  METHOD query.
    NEW lcl_configuration( )->dialog( ).
  ENDMETHOD.

  METHOD dialog.
    to_radiobutton( ).
    "CHECK gs_cfg-skip_dialog EQ abap_false.
    CHECK cl_ci_query_attributes=>generic(
        p_name       = CONV #( sy-repid )                    " unique screen ID
        p_title      = 'Class Diagram Parameters'            " Screen title
        p_attributes = get_attributes( )                     " Screen fields
        p_display    = abap_false                            " Edit / Display only
       ) EQ abap_false.   " Do not cancel
    from_radiobutton( ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_file IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_file IMPLEMENTATION.

  METHOD download.
    rv_subrc = 1.
    CHECK io_name->dialog( ) NE cl_gui_frontend_services=>action_cancel.

    rv_subrc = cl_uml_utilities=>save_xml_local( xml = iv_data
                                                 filename = io_name->get_fullpath( ) ).
  ENDMETHOD.

ENDCLASS.                    "lcl_file IMPLEMENTATION

CLASS lcl_file_name IMPLEMENTATION.

  METHOD new.
    CASE iv_mode.
      WHEN lcl_configuration=>c_mode_aut.
        ro_file = NEW lcl_file_name_dummy( iv_mode ).
      WHEN OTHERS.
        ro_file = NEW lcl_file_name( iv_mode ).
    ENDCASE.
  ENDMETHOD.

  METHOD constructor.
    CASE iv_mode.
      WHEN lcl_configuration=>c_mode_txt.
        ms_file = VALUE #( title = |Save UML text source|
                           ext = |.txt| ).
      WHEN OTHERS.
        ms_file = VALUE #( title = |Save As...|
                           ext = |.txt| ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_prefix.
    rv_name = shift_right( val = ms_file-name
                           places = strlen( ms_file-ext ) ).
  ENDMETHOD.

  METHOD get_fullpath.
    rv_name = ms_file-path.
  ENDMETHOD.

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
  ENDMETHOD.

ENDCLASS.

CLASS lcl_file_name_dummy IMPLEMENTATION.

  METHOD dialog.
    ms_file-path = |test.txt|.
    rv_user_action = cl_gui_frontend_services=>action_cancel.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_dot_diagram IMPLEMENTATION.

  METHOD generate.
    LOOP AT it_elem INTO DATA(lo_elem).
      node( elem = lo_elem ).
    ENDLOOP.
    rv_diagram = get( ).
  ENDMETHOD.

  METHOD add.
    mv_diagram = mv_diagram && iv_code.
  ENDMETHOD.

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
  ENDMETHOD.

  METHOD detach.
    CHECK c_start EQ abap_true.
    add( |detach\n| ).
  ENDMETHOD.

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
        add( |  :; \n| ).

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
    ro_dot = NEW #( ).
    ro_dot->header( is_config ).
  ENDMETHOD.                    " new

  METHOD get.
    footer( ).
    rv_dot = mv_diagram.
  ENDMETHOD.

  METHOD get_object_id.
*   Get object ID - internal call
    CALL 'OBJMGR_GET_INFO' ID 'OPNAME' FIELD 'GET_OBJID'  "#EC CI_CCALL
                           ID 'OBJID'  FIELD rv_oid
                           ID 'OBJ'    FIELD io_ref.
  ENDMETHOD.

ENDCLASS.
