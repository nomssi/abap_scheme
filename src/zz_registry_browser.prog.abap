*&---------------------------------------------------------------------*
*& Report ZZ_REGISTRY_BROWSER
*&---------------------------------------------------------------------*
*& was Report  ZUSR_REGISTRY_BROWSER
*&---------------------------------------------------------------------*
*& Viewer and editor for registry stored in INDX
*& (requires ZLIB_REGISTRY)
*&---------------------------------------------------------------------*
* Author: Martin Ceronio (2015), http://ceronio.net
* Released under MIT License: http://opensource.org/licenses/MIT
REPORT zz_registry_browser.

INCLUDE yy_lib_registry.

* For tree control:
DATA: gr_tree TYPE REF TO cl_gui_alv_tree.
DATA: gr_tree_toolbar TYPE REF TO cl_gui_toolbar.
DATA: gs_node_layout TYPE lvc_s_layn. "Layout for new nodes
* Table for registry entries on tree
TYPES: BEGIN OF ts_tab,
         key       TYPE string,
         reg_entry TYPE REF TO lcl_registry_entry,
       END OF ts_tab.
* Container for ALV tree data:
DATA: gt_tab TYPE TABLE OF ts_tab.

* For maintaining registry values in an an entry (ALV control):
DATA: gr_table   TYPE REF TO cl_gui_alv_grid.
DATA: gt_value   TYPE STANDARD TABLE OF lcl_registry_entry=>ts_keyval.
DATA: gt_value_ori TYPE STANDARD TABLE OF lcl_registry_entry=>ts_keyval. "Original data

* For splitter container
DATA: gr_splitter TYPE REF TO cl_gui_easy_splitter_container.

* For registry access:
DATA: gr_reg_root TYPE REF TO lcl_registry_entry.

DATA: gr_sel_reg_entry TYPE REF TO lcl_registry_entry. "Selected reg. entry
DATA: gv_sel_node_key TYPE lvc_nkey. "Tree node key of currently selected node

* Single statement to generate a selection screen
PARAMETERS: dummy.

*----------------------------------------------------------------------*
*       CLASS event_handler DEFINITION
*----------------------------------------------------------------------*
CLASS event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      handle_node_expand FOR EVENT expand_nc OF cl_gui_alv_tree
        IMPORTING node_key sender,
      handle_table_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive sender,
      handle_table_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      handle_node_selected FOR EVENT selection_changed OF cl_gui_alv_tree
        IMPORTING node_key,
      handle_tree_command FOR EVENT function_selected OF cl_gui_toolbar
        IMPORTING fcode.
*      handle_values_changed for event DATA_CHANGE of CL_GUI_ALV_GRID
*        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

ENDCLASS.                    "event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS event_handler IMPLEMENTATION.

* Handle commands to the tree toolbar
  METHOD handle_tree_command.
    DATA: lv_new_key TYPE string.
    DATA: lr_reg_entry TYPE REF TO lcl_registry_entry.
    DATA: lv_node_key TYPE lvc_nkey.
    DATA: lv_rc TYPE char1.
    DATA: lv_ntext TYPE lvc_value.
    DATA: ls_tab TYPE ts_tab.

    IF gr_sel_reg_entry IS NOT BOUND.
      MESSAGE 'Select a node from the tree first' TYPE 'I'.
      RETURN.
    ENDIF.

* Create a new node under selected node, or copy a registry node on the same level
    IF fcode = 'INSE'.

* Dialog to capture name of new node
      PERFORM value_input_dialog USING 'New registry entry key'(007)
            CHANGING lv_new_key lv_rc.

* Add the new key to the current registry entry if the user accepts
      IF lv_rc = space.
        TRY.
* Update the tree by adding the new node
            gr_sel_reg_entry->add_subentry( lv_new_key ).
            PERFORM refresh_subnodes USING gv_sel_node_key.

          CATCH lcx_registry_entry_exists.
            MESSAGE 'The registry entry already exists'(015) TYPE 'I'.
            RETURN.
        ENDTRY.
      ENDIF.

* Copy the selected node at the same level
    ELSEIF fcode = 'COPY'.

* Dialog to capture name of new node
      PERFORM value_input_dialog USING 'Target registry entry key'(006)
            CHANGING lv_new_key lv_rc.

* Perform deep copy of source to target node
      IF lv_rc = space.
        DATA: lr_parent TYPE REF TO lcl_registry_entry.
        TRY.
            lr_parent = gr_sel_reg_entry->get_parent( ).
            lr_parent->copy_subentry( source_key = gr_sel_reg_entry->entry_id target_key = lv_new_key ).

* Get the parent node in the tree to refresh it
            CALL METHOD gr_tree->get_parent
              EXPORTING
                i_node_key        = gv_sel_node_key
              IMPORTING
                e_parent_node_key = lv_node_key.

* Refresh the parent node
            PERFORM refresh_subnodes USING lv_node_key.

          CATCH lcx_registry_entry_exists.
            MESSAGE 'The registry entry already exists'(015) TYPE 'I'.
            RETURN.
          CATCH lcx_registry_err.
            MESSAGE 'Error updating registry'(017) TYPE 'I'.
            RETURN.
        ENDTRY.
      ENDIF.

* Delete the selected node from the registry
    ELSEIF fcode = 'DELE'.

* Prevent deleting of the root entity, which would fail anyway when we try get its parent
      IF gr_sel_reg_entry->internal_key = lcl_registry_entry=>registry_root.
        MESSAGE 'Root node cannot be deleted' TYPE 'I'.
        RETURN.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirm deletion'(009)
          text_question         = 'Are you sure you want to delete the selected entry?'(010)
          display_cancel_button = abap_false
        IMPORTING
          answer                = lv_rc
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
* Won't happen
      ENDIF.

* Check that the user selected OK on the confirmation
      CHECK lv_rc = '1'.

      lr_reg_entry = gr_sel_reg_entry->get_parent( ).
      CHECK lr_reg_entry IS BOUND.
      lr_reg_entry->remove_subentry( gr_sel_reg_entry->entry_id ).

* Get the parent node in the tree to refresh it
      CALL METHOD gr_tree->get_parent
        EXPORTING
          i_node_key        = gv_sel_node_key
        IMPORTING
          e_parent_node_key = lv_node_key.

* Refresh the parent node
      PERFORM refresh_subnodes USING lv_node_key.

    ENDIF.

  ENDMETHOD.                    "handle_tree_command

* Handle commands on the values table
  METHOD handle_table_command.
    DATA: lv_node_key TYPE lvc_nkey.
    DATA: ls_tab TYPE ts_tab.
    DATA: lt_value TYPE lcl_registry_entry=>tt_keyval.
    DATA: ls_value TYPE lcl_registry_entry=>ts_keyval.

* Save current values
    IF e_ucomm = 'SAVE'.
      PERFORM save_values.
    ENDIF.
  ENDMETHOD.                    "handle_table_command

* Handle selection of a node in the tree
  METHOD handle_node_selected.
    DATA: ls_tab TYPE ts_tab.
    DATA: lt_val TYPE lcl_registry_entry=>tt_keyval.

* Check whether data has changed before
    DATA: lv_answer TYPE char01.
    DATA: lv_refresh TYPE char01.
* Check for changed data. The CHECK_CHANGED_DATA() method and
* neither the DATA_CHANGED or DATA_CHANGED_FINISHED
* events of CL_GUI_ALV_GRID seem to fit the bill, so we keep our own copy
* of the original data and compare it
    IF gr_table IS BOUND.
* Refresh data in local table (GT_VALUE)
      CALL METHOD gr_table->check_changed_data.

      IF gt_value NE gt_value_ori.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirm data loss'(017)
            text_question         = 'Data has changed. Save first?'(018)
            display_cancel_button = abap_false
          IMPORTING
            answer                = lv_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
* Not going to happen; not using a text
        ENDIF.

        IF lv_answer = '1'. "Save data before moving on
          PERFORM save_values.
        ENDIF.

      ENDIF.

    ENDIF.

* Set up the table
    IF gr_table IS NOT BOUND.
      PERFORM create_table.
    ENDIF.

    CALL METHOD gr_tree->get_outtab_line
      EXPORTING
        i_node_key     = node_key
      IMPORTING
        e_outtab_line  = ls_tab    " Line of Outtab
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* Read the values of the selected registry entry
    gt_value =  ls_tab-reg_entry->get_values( ).
    gt_value_ori = gt_value. "Store last values
* Ensure column widths are correct on every update
* Settings on table
*    data: ls_layout type lvc_s_layo.
*    ls_layout-cwidth_opt = abap_true.
*    gr_table->set_frontend_layout( ls_layout ).
    gr_table->refresh_table_display( ).
* Keep track of selected reg. entry for update
    gr_sel_reg_entry = ls_tab-reg_entry.
    gv_sel_node_key  = node_key.

  ENDMETHOD.                    "handle_node_selected

* Expand nodes of the registry tree to add sub-entries
  METHOD handle_node_expand.
    DATA: lr_reg_entry TYPE REF TO lcl_registry_entry.
    DATA: ls_tab TYPE ts_tab.

    CALL METHOD sender->get_outtab_line
      EXPORTING
        i_node_key     = node_key
      IMPORTING
        e_outtab_line  = ls_tab    " Line of Outtab
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA: lt_sub_entries TYPE lcl_registry_entry=>tt_keyobj.
    DATA: ls_sub_entry TYPE lcl_registry_entry=>ts_keyobj.
    DATA: lv_expander TYPE abap_bool.
    DATA: lv_node_text TYPE lvc_value.

* Add sub-entries to selected node in tree
    lt_sub_entries = ls_tab-reg_entry->get_subentries( ).
    LOOP AT lt_sub_entries INTO ls_sub_entry.

      lr_reg_entry = ls_sub_entry-value.
      PERFORM add_node USING node_key lr_reg_entry.

    ENDLOOP.

  ENDMETHOD.                    "EXPAND_EMPTY_FOLDER

* Modify toolbar entries for table/grid
  METHOD handle_table_toolbar.
    DATA: ls_tbe TYPE stb_button.
* Keep only the local editing features
    LOOP AT e_object->mt_toolbar INTO ls_tbe.
      IF ls_tbe-function(7) NE '&LOCAL&'.
        DELETE e_object->mt_toolbar.
      ENDIF.
    ENDLOOP.
* Add a function for saving the values
    ls_tbe-function = 'SAVE'.
    ls_tbe-icon = '@2L@'.
    ls_tbe-butn_type = '0'.
    ls_tbe-quickinfo = 'Save'.
    APPEND ls_tbe TO e_object->mt_toolbar.
  ENDMETHOD.                    "handle_table_toolbar

ENDCLASS.                    "event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  refresh_subnodes
*&---------------------------------------------------------------------*
*       Delete and refresh subnodes of a node
*----------------------------------------------------------------------*
FORM refresh_subnodes USING pv_nkey TYPE lvc_nkey.
  DATA: ls_tab TYPE ts_tab.
  DATA: ls_subentry TYPE lcl_registry_entry=>ts_keyval.
  DATA: lr_reg_entry TYPE REF TO lcl_registry_entry.
  DATA: lt_children TYPE lvc_t_nkey.
  DATA: lv_nkey TYPE lvc_nkey.

* Delete subnodes of node. This means: getting all children and deleting
* them individually!
  CALL METHOD gr_tree->get_children
    EXPORTING
      i_node_key         = pv_nkey
    IMPORTING
      et_children        = lt_children
    EXCEPTIONS
      historic_error     = 1
      node_key_not_found = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE 'Error building tree'(014) TYPE 'E'.
  ENDIF.

  LOOP AT lt_children INTO lv_nkey.

    CALL METHOD gr_tree->delete_subtree
      EXPORTING
        i_node_key                = lv_nkey
        i_update_parents_expander = abap_true
      EXCEPTIONS
        node_key_not_in_model     = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Error building tree'(014) TYPE 'E'.
    ENDIF.

  ENDLOOP.

* With the children deleted, proceed to re-add registry entries

* Get the registry entry on the node
  CALL METHOD gr_tree->get_outtab_line
    EXPORTING
      i_node_key     = pv_nkey
    IMPORTING
      e_outtab_line  = ls_tab
    EXCEPTIONS
      node_not_found = 1
      OTHERS         = 2.
  IF sy-subrc NE 0.
    MESSAGE 'Error building tree'(014) TYPE 'E'.
  ENDIF.
* Add a subnode for each sub-entry
  LOOP AT ls_tab-reg_entry->sub_entries INTO ls_subentry.
    lr_reg_entry = ls_tab-reg_entry->get_subentry( ls_subentry-key ).
    PERFORM add_node USING pv_nkey lr_reg_entry.
  ENDLOOP.
* Expand parent node
  CALL METHOD gr_tree->expand_node
    EXPORTING
      i_node_key          = pv_nkey
    EXCEPTIONS
      failed              = 1
      illegal_level_count = 2
      cntl_system_error   = 3
      node_not_found      = 4
      cannot_expand_leaf  = 5
      OTHERS              = 6.
  IF sy-subrc <> 0.
    MESSAGE 'Error building tree'(014) TYPE 'E'.
  ENDIF.
* Update tree display
  gr_table->refresh_table_display( ).
ENDFORM.                    "refresh_subnodes


*&---------------------------------------------------------------------*
*&      Form  save_values
*&---------------------------------------------------------------------*
*       Save current values in table to currently selected reg. node
*----------------------------------------------------------------------*
FORM save_values.
  IF gr_table IS BOUND AND gr_sel_reg_entry IS BOUND.
    DATA: lt_value TYPE lcl_registry_entry=>tt_keyval.
    DATA: ls_value TYPE lcl_registry_entry=>ts_keyval.
* Normalize the values; duplicate keys are overwritten, with possible loss of data!
    LOOP AT gt_value INTO ls_value.
      INSERT ls_value INTO TABLE lt_value.
    ENDLOOP.
    gr_sel_reg_entry->set_values( lt_value ).
    TRY.
        gr_sel_reg_entry->save( ).
      CATCH lcx_registry_lock.
        MESSAGE 'Values have been overwritten since last change and are refreshed'(004) TYPE 'I'.
        gr_sel_reg_entry->reload( ).
        gt_value = gr_sel_reg_entry->get_values( ).
    ENDTRY.
    gr_table->refresh_table_display( ).
  ENDIF.

ENDFORM.                    "save_values

*&---------------------------------------------------------------------*
*&      Form  add_node
*&---------------------------------------------------------------------*
* Add single node to tree
*----------------------------------------------------------------------*
*      -->PV_NKEY    Node of tree to which to add node
*      -->PS_TAB     Table entry (with reg. entry) to add as child
*----------------------------------------------------------------------*
FORM add_node
  USING pv_nkey TYPE lvc_nkey pr_regentry TYPE REF TO lcl_registry_entry.

  DATA: lv_node_text TYPE lvc_value.
  DATA: ls_node_layout TYPE lvc_s_layn. "Layout for new nodes
  DATA: ls_tab TYPE ts_tab.

  IF pr_regentry IS NOT BOUND.
    MESSAGE 'Error building tree'(014) TYPE 'E'.
  ENDIF.

  ls_tab-reg_entry = pr_regentry.

* Add node as folder always
  ls_node_layout-isfolder = abap_true.
* Add expander only if there are more sub-entries
  IF lines( pr_regentry->get_subentry_keys( ) ) > 0.
    ls_node_layout-expander = abap_true.
  ELSE.
    ls_node_layout-expander = abap_false.
  ENDIF.

  lv_node_text = pr_regentry->entry_id.

  CALL METHOD gr_tree->add_node
    EXPORTING
      i_relat_node_key     = pv_nkey
      i_relationship       = cl_gui_column_tree=>relat_last_child
      is_outtab_line       = ls_tab
      i_node_text          = lv_node_text
      is_node_layout       = ls_node_layout
    EXCEPTIONS
      relat_node_not_found = 1
      node_not_found       = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
    MESSAGE 'Error building tree'(014) TYPE 'E'.
  ENDIF.

ENDFORM.                    "add_node

*&---------------------------------------------------------------------*
*&      Form  create_table
*&---------------------------------------------------------------------*
*       Initialize table for showing values in a registry entry
*----------------------------------------------------------------------*
FORM create_table RAISING cx_salv_msg.
  DATA: lr_func TYPE REF TO cl_salv_functions_list.
  DATA: lr_cols TYPE REF TO cl_salv_columns_table.

  DATA: lt_fcat TYPE lvc_t_fcat.
  DATA: ls_fcat TYPE lvc_s_fcat.

  CREATE OBJECT gr_table
    EXPORTING
      i_parent          = gr_splitter->bottom_right_container
      i_appl_events     = abap_true    " Register Events as Application Events
*     i_fcat_complete   = SPACE    " Boolean Variable (X=True, Space=False)
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Add fields to catalog
  ls_fcat-fieldname = 'KEY'.
  ls_fcat-edit      = abap_true.
  ls_fcat-key       = abap_true.
  ls_fcat-scrtext_s = 'Key'(001).
  ls_fcat-outputlen = 35. "Because colwidth opt is not always great
  APPEND ls_fcat TO lt_fcat.
  ls_fcat-fieldname = 'VALUE'.
  ls_fcat-edit      = abap_true.
  ls_fcat-key       = abap_false.
  ls_fcat-scrtext_s = 'Value'(002).
  ls_fcat-outputlen = 35. "Because colwidth opt is not always great
  APPEND ls_fcat TO lt_fcat.

  gr_table->set_table_for_first_display(
    CHANGING
      it_outtab                     = gt_value[]
      it_fieldcatalog               = lt_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Toolbar to hold only functions for editing
  SET HANDLER event_handler=>handle_table_toolbar FOR gr_table.
  SET HANDLER event_handler=>handle_table_command FOR gr_table.
  gr_table->set_toolbar_interactive( ).

** Settings on table
*  data: ls_layout type lvc_s_layo.
*  ls_layout-cwidth_opt = abap_true.
*  gr_table->set_frontend_layout( ls_layout ).

ENDFORM.                    "create_table

*&---------------------------------------------------------------------*
*&      Form  create_tree
*&---------------------------------------------------------------------*
*       Initialize tree showing the registry hierarchy
*----------------------------------------------------------------------*
FORM create_tree.

  DATA: lt_fcat TYPE lvc_t_fcat.
  DATA: ls_fcat TYPE lvc_s_fcat.
  DATA: lt_event TYPE cntl_simple_events,
        ls_event TYPE cntl_simple_event.

  gr_reg_root = lcl_registry_entry=>get_root( ).

* Create tree
  CREATE OBJECT gr_tree
    EXPORTING
      parent                      = gr_splitter->top_left_container
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
      item_selection              = abap_false
      no_toolbar                  = abap_false
      no_html_header              = abap_true
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7
      OTHERS                      = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Add key so that there is *something* in the field catalog
  ls_fcat-fieldname = 'KEY'.
  ls_fcat-no_out    = abap_true.
  APPEND ls_fcat TO lt_fcat.

  CALL METHOD gr_tree->set_table_for_first_display
    CHANGING
      it_outtab       = gt_tab
      it_fieldcatalog = lt_fcat.

* Get handle on tree toolbar
  DATA: lt_ttb TYPE ttb_button.
  DATA: ls_ttb TYPE stb_button.
  gr_tree->get_toolbar_object( IMPORTING er_toolbar = gr_tree_toolbar ).
  gr_tree_toolbar->delete_all_buttons( ).
* Add custom buttons for registry entry operations
  ls_ttb-function = 'INSE'. "Insert entry
  ls_ttb-icon     = '@17@'. "ICON_INSERT_ROW
  APPEND ls_ttb TO lt_ttb.
  ls_ttb-function = 'DELE'. "Delete entry
  ls_ttb-icon     = '@18@'. "ICON_DELETE_ROW
  APPEND ls_ttb TO lt_ttb.
  ls_ttb-function = 'COPY'. "Copy Entry
  ls_ttb-icon     = '@14@'. "ICON_COPY_OBJECT
  APPEND ls_ttb TO lt_ttb.
  CALL METHOD gr_tree_toolbar->add_button_group
    EXPORTING
      data_table       = lt_ttb
    EXCEPTIONS
      dp_error         = 1
      cntb_error_fcode = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
    MESSAGE 'Error when setting up registry toolbar'(005) TYPE 'E'.
  ENDIF.

* Add root node
  PERFORM add_node USING '' gr_reg_root.

* Register events and set handlers
*  ls_event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
  ls_event-eventid = cl_gui_simple_tree=>eventid_selection_changed.
  ls_event-appl_event = 'X'.
  APPEND ls_event TO lt_event.
  ls_event-eventid = cl_gui_simple_tree=>eventid_expand_no_children.
  ls_event-appl_event = 'X'.
  APPEND ls_event TO lt_event.
  CALL METHOD gr_tree->set_registered_events
    EXPORTING
      events                    = lt_event
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SET HANDLER event_handler=>handle_node_expand FOR gr_tree.
  SET HANDLER event_handler=>handle_node_selected FOR gr_tree.
  SET HANDLER event_handler=>handle_tree_command FOR gr_tree_toolbar.

  CALL METHOD gr_tree->frontend_update.

ENDFORM.                    "create_tree

*&---------------------------------------------------------------------*
*&      Form  value_input_dialog
*&---------------------------------------------------------------------*
*       Get single value from user
*----------------------------------------------------------------------*
FORM value_input_dialog USING title CHANGING value returncode.
  DATA: lt_fld TYPE TABLE OF sval.
  DATA: ls_fld TYPE sval.

  ls_fld-tabname = 'OJFIELDS'.
  ls_fld-fieldname = 'INPUT'.
  APPEND ls_fld TO lt_fld.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      no_value_check  = abap_true
      popup_title     = title
    IMPORTING
      returncode      = returncode
    TABLES
      fields          = lt_fld
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Error during request for value'(008) TYPE 'E'.
  ENDIF.

  READ TABLE lt_fld INTO ls_fld INDEX 1.
  value = ls_fld-value.

ENDFORM.                    "value_input_dialog

START-OF-SELECTION.


AT SELECTION-SCREEN OUTPUT.
* Disable Execute and Save functions on report selection screen
  PERFORM insert_into_excl(rsdbrunt) USING 'ONLI'.
  PERFORM insert_into_excl(rsdbrunt) USING 'SPOS'.

* Initialize the display on the first dynpro roundtrip
  IF gr_splitter IS NOT BOUND.
    DATA: gv_dynnr TYPE sydynnr.
    DATA: gv_repid TYPE syrepid.
    gv_dynnr = sy-dynnr.
    gv_repid = sy-repid.
    CREATE OBJECT gr_splitter
      EXPORTING
        link_dynnr        = gv_dynnr
        link_repid        = gv_repid
        parent            = cl_gui_easy_splitter_container=>default_screen
        orientation       = 1    " Orientation: 0 = Vertical, 1 = Horizontal
        sash_position     = 30    " Position of Splitter Bar (in Percent)
        with_border       = 0    " With Border = 1; Without Border = 0
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    PERFORM create_tree.

* Table creation is deferred until the first node is selected

  ENDIF.
