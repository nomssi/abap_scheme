*&---------------------------------------------------------------------*
*&  Include           YY_LISP_IDE
*&---------------------------------------------------------------------*

DATA g_ok_code TYPE syucomm.

CLASS lcl_stack DEFINITION.
  PUBLIC SECTION.
    TYPES tv_data TYPE string.
    METHODS push IMPORTING iv_key TYPE tv_data.
    METHODS pop RETURNING VALUE(rv_data) TYPE tv_data.
    METHODS empty RETURNING VALUE(rv_flag) TYPE xsdboolean.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_node,
             data  TYPE tv_data,
             next TYPE REF TO data,
           END OF ts_node.

    DATA mr_top TYPE REF TO ts_node.
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
    METHODS append_to IMPORTING io_editor TYPE REF TO lcl_editor.
    METHODS push_text.
    METHODS pop_text RETURNING VALUE(code) TYPE string.
    METHODS to_string RETURNING VALUE(rv_text) TYPE string.
    METHODS update_status IMPORTING iv_string TYPE string.
    METHODS append_string IMPORTING iv_text TYPE string.
  PRIVATE SECTION.
    DATA mv_counter TYPE i.
    DATA mo_stack TYPE REF TO lcl_stack.

    METHODS format_input IMPORTING code           TYPE string
                         RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.                    "lcl_editor DEFINITION

CLASS lcl_stack IMPLEMENTATION.

  METHOD push.
    mr_top = NEW ts_node( data = iv_key
                          next = mr_top ).
  ENDMETHOD.

  METHOD pop.
    CLEAR rv_data.
    CHECK mr_top IS BOUND.
    rv_data = mr_top->data.
    mr_top ?= mr_top->next.
  ENDMETHOD.

  METHOD empty.
    rv_flag = xsdbool( mr_top IS NOT BOUND ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       INTERFACE lif_unit_test IMPLEMENTATION
*----------------------------------------------------------------------*
INTERFACE lif_unit_test.
ENDINTERFACE.                    "lif_unit_test IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_ide DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_ide DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS: main,
      free,
      pbo,
      pai IMPORTING iv_code        TYPE syucomm
          RETURNING VALUE(rv_flag) TYPE flag.
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
      refresh,
      show_docu,
      free_controls,
      user_command IMPORTING iv_code        TYPE syucomm
                   RETURNING VALUE(rv_flag) TYPE flag.
    METHODS welcome RETURNING VALUE(text) TYPE string.
    METHODS console_header RETURNING VALUE(text) TYPE string.

    METHODS flush RETURNING VALUE(rv_text) TYPE string.

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
    CONSTANTS c_url TYPE string VALUE `https://github.com/nomssi/abap_scheme/wiki`.
    cl_abap_browser=>show_url( title = `ABAP Scheme Wiki`
                               url = c_url
                               modal = abap_false
                                "size = ms_cfg-browser_size
                               ).
  ENDMETHOD.

  METHOD lif_port~read.
    DATA lt_fields TYPE STANDARD TABLE OF sval.
    DATA lv_user_response TYPE flag.

    rv_input = iv_input.
    CHECK iv_input IS INITIAL.
    lt_fields = VALUE #( ( tabname = 'ABDBG'     " Text: Input Line
                           fieldname = 'LTEXT'
                           field_obl = 'X' ) ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title  = 'ABAP Lisp Input'
        start_column = '45'
        start_row    = '11'
      IMPORTING
        returncode   = lv_user_response
      TABLES
        fields       = lt_fields
      EXCEPTIONS
        OTHERS       = 2.

    CHECK sy-subrc EQ 0
      AND lv_user_response NE 'A'
      AND line_exists( lt_fields[ 1 ] ).

    rv_input = lt_fields[ 1 ]-value.
    mo_console->append_string( rv_input && |\n| ).
  ENDMETHOD.

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

      WHEN lcl_lisp=>type_conscell.
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
  ENDMETHOD.

  METHOD welcome.
    text = |==> Welcome to ABAP List Processing!\n|.
  ENDMETHOD.                    "welcome

  METHOD console_header.
    text = |==> ABAP Lisp -- Console { sy-uname } -- { sy-datlo DATE = ENVIRONMENT } { sy-uzeit TIME = ENVIRONMENT }\n|.
  ENDMETHOD.

  METHOD first_output.
    CHECK mv_first EQ abap_true.
    CLEAR mv_first.
    cl_gui_textedit=>set_focus( mo_source ).
    mo_log->append_string( |{ welcome( ) }\n| ).
    mo_console->append_string( console_header( ) ).
  ENDMETHOD.                    "first_output

  METHOD evaluate.
    TRY.
        DATA(code) = mo_source->to_string( ).
        DATA(response) = mo_int->eval_repl( code ).

        mo_output->append_source( code ).
        mo_console->append_source( response ).

        mo_source->push_text( ).
        mo_source->update_status( |[ { mo_int->runtime } µs ] { response }| ).

      CATCH cx_root INTO DATA(lx_root).
        response = lx_root->get_text( ).
        mo_source->update_status( |{ response }| ).
    ENDTRY.
    mo_log->append_string( |{ code }\n=> { response }\n| ).
  ENDMETHOD.                    "evaluate

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
    SET TITLEBAR  'TITLE_100'.
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
      WHEN 'CLEAR'.
        refresh( ).
      WHEN 'PREV'.
        previous( ).
      WHEN 'NEXT'.
        next( ).
      WHEN 'HELP'.
        show_docu( ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    rv_flag = abap_true.
  ENDMETHOD.                    "user_command

  METHOD previous.
    mo_source->pop_text( ).
  ENDMETHOD.

  METHOD next.
    mo_source->push_text( ).
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
    delete_text( ).
    mo_stack = NEW #( ).
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

  METHOD append_to.
    io_editor->append_string( to_string( ) ).
  ENDMETHOD.                    "append_to

  METHOD to_string.
    get_textstream( IMPORTING text = rv_text ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.                    "to_string

  METHOD update_status.
    set_status_text( CONV char72( iv_string ) ).
  ENDMETHOD.                    "update_status

  METHOD push_text.
    DATA(code) = to_string( ).
    CHECK code NE space.
    mo_stack->push( code ).
    delete_text( ).
  ENDMETHOD.

  METHOD pop_text.
    CHECK mo_stack->empty( ) EQ abap_false.
    code = mo_stack->pop( ).
    delete_text( ).
    append_string( code ).
  ENDMETHOD.

ENDCLASS.                    "lcl_editor IMPLEMENTATION
