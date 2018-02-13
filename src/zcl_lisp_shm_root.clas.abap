class ZCL_LISP_SHM_ROOT definition
  public
  final
  create public
  shared memory enabled .

public section.

  interfaces IF_SHM_BUILD_INSTANCE .

  types:
    BEGIN OF ts_settings,
             stack      TYPE string_table,
             new_editor TYPE flag,
           END OF ts_settings .
  types:
    BEGIN OF ts_param.
      INCLUDE TYPE ts_settings as settings.
    TYPES:   uname      TYPE syuname,
             datum      TYPE aedat,
             uzeit      TYPE uzeit,
           END OF ts_param .

  methods LOAD
    importing
      !IV_UNAME type SYUNAME default SY-UNAME
    returning
      value(RS_SETTINGS) type TS_SETTINGS .
  methods SAVE
    importing
      !IV_UNAME type SYUNAME default SY-UNAME
      !IS_SETTINGS type TS_SETTINGS .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ms_param TYPE ts_param.
ENDCLASS.



CLASS ZCL_LISP_SHM_ROOT IMPLEMENTATION.


  METHOD if_shm_build_instance~build.
    DATA params TYPE REF TO zcl_lisp_shm_root.
    DATA handle TYPE REF TO zcl_lisp_area.

    handle = zcl_lisp_area=>attach_for_write( ).
    CREATE OBJECT params AREA HANDLE handle.
    handle->set_root( params ).
*   Initial values are saved
    handle->detach_commit( ).

  ENDMETHOD.


  METHOD LOAD.
    rs_settings = ms_param-settings.
  ENDMETHOD.


  METHOD SAVE.
    ms_param = VALUE #( settings = is_settings
                        uname    = iv_uname
                        datum    = sy-datum
                        uzeit    = sy-uzeit ).
  ENDMETHOD.
ENDCLASS.
