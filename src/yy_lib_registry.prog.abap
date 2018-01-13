*&---------------------------------------------------------------------*
*& Include          YY_LIB_REGISTRY
*&---------------------------------------------------------------------*
*&  Was: Include ZLIBREGISTRY
* Implementation of a registry for storing arbitrary values (similar to the MS Windows registry)
* Author: Martin Ceronio (2015), http://ceronio.net
* Released under MIT License: http://opensource.org/licenses/MIT

CLASS lcx_registry_err DEFINITION INHERITING FROM cx_dynamic_check.
ENDCLASS.                    "lcx_registry_err DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcx_registry_lock DEFINITION
*----------------------------------------------------------------------*
CLASS lcx_registry_lock DEFINITION INHERITING FROM lcx_registry_err.
ENDCLASS.                    "lcx_registry_lock DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcx_registry_noentry DEFINITION
*----------------------------------------------------------------------*
CLASS lcx_registry_noentry DEFINITION INHERITING FROM lcx_registry_err.
ENDCLASS.                    "lcx_registry_noentry DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcx_registry_entry_exists DEFINITION
*----------------------------------------------------------------------*
CLASS lcx_registry_entry_exists DEFINITION INHERITING FROM lcx_registry_err.
ENDCLASS.                    "lcx_registry_entry_exists DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcx_registry_entry_deleted DEFINITION
*----------------------------------------------------------------------*
CLASS lcx_registry_entry_deleted DEFINITION INHERITING FROM lcx_registry_err.
ENDCLASS.                    "lcx_registry_entry_deleted DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcx_registry_invalid_char DEFINITION
*----------------------------------------------------------------------*
CLASS lcx_registry_invalid_char DEFINITION INHERITING FROM lcx_registry_err.
ENDCLASS.                    "lcx_registry_invalid_char DEFINITION

CLASS lcl_registry_lock DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING key TYPE indx_srtfd,
      set_optimistic
        RAISING lcx_registry_lock,
      promote
        RAISING lcx_registry_lock,
      release.

    METHODS
      get_uuid RETURNING VALUE(rv_uuid) TYPE sysuuid_c22
               RAISING   lcx_registry_err.
  PRIVATE SECTION.
    DATA internal_key TYPE indx_srtfd.
ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS lcl_registry_entry DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_registry_entry DEFINITION CREATE PROTECTED.

  PUBLIC SECTION.
*   Predefined key for the registry root:
    CLASS-DATA registry_root TYPE indx_srtfd READ-ONLY VALUE 'REGISTRY_ROOT'.

    TYPES: BEGIN OF ts_keyval,
             key   TYPE string,
             value TYPE string,
           END OF ts_keyval.
    TYPES: tt_keyval TYPE SORTED TABLE OF ts_keyval WITH UNIQUE KEY key.

* For keeping track of references to sub-entries, we maintain a shadow
* table with the same keys
    TYPES: BEGIN OF ts_keyobj,
             key   TYPE string,
             value TYPE REF TO lcl_registry_entry,
           END OF ts_keyobj.
    TYPES: tt_keyobj TYPE SORTED TABLE OF ts_keyobj WITH UNIQUE KEY key.

    DATA sub_entries    TYPE tt_keyval READ-ONLY.
    DATA values         TYPE tt_keyval READ-ONLY.
    DATA internal_key   TYPE indx_srtfd READ-ONLY.
    DATA parent_key     TYPE indx_srtfd READ-ONLY.
    DATA entry_id       TYPE string READ-ONLY. "User-friendly ID of the subnode

    METHODS:
      constructor
        IMPORTING internal_key TYPE any,
      reload
        RAISING lcx_registry_noentry,
*      lock raising lcx_registry_err,
* Saves entry and all dirty sub-entries
      save RAISING lcx_registry_err,

      get_parent
        RETURNING VALUE(parent) TYPE REF TO lcl_registry_entry,

      create_by_path
        IMPORTING path         TYPE string
        RETURNING VALUE(entry) TYPE REF TO lcl_registry_entry
        RAISING   lcx_registry_err,

*--------------------------------------------------------------------*
* Methods dealing with sub-entries of the registry entry
      get_subentry
        IMPORTING key          TYPE clike
        RETURNING VALUE(entry) TYPE REF TO lcl_registry_entry,
      add_subentry
        IMPORTING key          TYPE clike
        RETURNING VALUE(entry) TYPE REF TO lcl_registry_entry
        RAISING   lcx_registry_entry_exists,
* Removes sub-entry and all entries underneath
      remove_subentry
        IMPORTING key TYPE clike
        RAISING   lcx_registry_err,
      remove_subentries
        RAISING lcx_registry_err,
      copy_subentry
        IMPORTING source_key          TYPE clike
                  target_key          TYPE clike
        RETURNING VALUE(target_entry) TYPE REF TO lcl_registry_entry
        RAISING   lcx_registry_err,

      get_subentry_keys
        RETURNING VALUE(keys) TYPE string_table,

      get_subentries
        RETURNING VALUE(sub_entries) TYPE tt_keyobj,

* Methods for dealing with values in the registry entry:

*     Get keys of all values
      get_value_keys
        RETURNING VALUE(keys) TYPE string_table,
*     Get all values
      get_values
        RETURNING VALUE(values) TYPE tt_keyval,
*     Set all values in one go:
      set_values
        IMPORTING values TYPE tt_keyval,
*     Get single value by key
      get_value
        IMPORTING key          TYPE clike
        RETURNING VALUE(value) TYPE string
        RAISING   lcx_registry_noentry,
*     Set/overwrite single value
      set_value
        IMPORTING key   TYPE clike
                  value TYPE any,
*     Delete single value by key
      delete_value
        IMPORTING key TYPE clike.


    CLASS-METHODS:
      get_entry_by_internal_key
        IMPORTING key          TYPE any
        RETURNING VALUE(entry) TYPE REF TO lcl_registry_entry,
      get_root
        RETURNING VALUE(root) TYPE REF TO lcl_registry_entry.

  PROTECTED SECTION.

    METHODS:
      copy_subentry_deep
        IMPORTING source TYPE REF TO lcl_registry_entry
                  target TYPE REF TO lcl_registry_entry,
* Remove the registry entry from the database:
* The DELETE method is protected because you must always delete an entry
* as the sub-entry of its parent so that the link is removed from the
* parent
      delete
        RAISING lcx_registry_err.

    DATA deleted TYPE abap_bool.

    CLASS-DATA lock TYPE REF TO lcl_registry_lock.

*   Class-wide buffer of instances of registry entries
    CLASS-DATA registry_entries TYPE tt_keyobj.


ENDCLASS.                    "lcl_registry_entry DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_registry_entry IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_registry_entry IMPLEMENTATION.

*--------------------------------------------------------------------*
* CONSTRUCTOR - new instance of registry key
*--------------------------------------------------------------------*
  METHOD constructor.
    me->internal_key = internal_key.

    lock = NEW lcl_registry_lock( me->internal_key ).

*   Load the entry from the database
    reload( ).

*   Object inserts itself into registry of entries
    INSERT VALUE ts_keyobj( key = me->internal_key
                            value = me ) INTO TABLE registry_entries.
  ENDMETHOD.                    "constructor

*--------------------------------------------------------------------*
* RELOAD - reload values and sub-entries from database, set new lock
*--------------------------------------------------------------------*
  METHOD reload.
*   Reload the values and sub-entries from the database
    IMPORT values = me->values
           sub_entries = me->sub_entries
           parent = parent_key
           entry_id = entry_id FROM DATABASE indx(zr) ID internal_key.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE lcx_registry_noentry.
    ENDIF.

    lock->set_optimistic( ).
  ENDMETHOD.                    "reload

*--------------------------------------------------------------------*
* GET_ROOT - retrieve root entry of registry
*--------------------------------------------------------------------*
  METHOD get_root.
*   If the root doesn't exist yet, create it
    DATA values TYPE tt_keyval.
    DATA sub_entries TYPE tt_keyval.
    DATA parent_key TYPE indx_srtfd VALUE space.
    DATA entry_id TYPE string.

    IMPORT values = values
           sub_entries = sub_entries FROM DATABASE indx(zr) ID registry_root.
    IF sy-subrc NE 0.
      entry_id = registry_root.
      EXPORT values = values
             sub_entries = sub_entries
             parent = parent_key
             entry_id = entry_id TO DATABASE indx(zr) ID registry_root.
    ENDIF.

*   Retrieve the root entry of the registry
    root = get_entry_by_internal_key( registry_root ).

  ENDMETHOD.                    "get_root

*--------------------------------------------------------------------*
* GET_ENTRY_BY_INTERNAL_KEY - retrieve reg. entry by internal ID
*--------------------------------------------------------------------*
  METHOD get_entry_by_internal_key.
*   Search global index of registry entry instances

    entry = VALUE #( registry_entries[ key = key ]-value DEFAULT
                 NEW #( internal_key = key ) ). " will insert itself in registry

  ENDMETHOD.                    "get_entry_by_internal_key

*--------------------------------------------------------------------*
* CREATE_BY_PATH - convenience method, analogous to mkdir -p that
* allows you to create a path of registry entries if they do not yet
* exist; paths must be separated by forward slash ('/')
* Sub-entries are created from the current registry entry
*--------------------------------------------------------------------*
  METHOD create_by_path.
    DATA sub_entry TYPE REF TO lcl_registry_entry.

    SPLIT path AT '/' INTO TABLE DATA(keys).

    entry = me.
    LOOP AT keys INTO DATA(key) WHERE table_line IS NOT INITIAL.
      sub_entry = entry->get_subentry( key ).
      IF sub_entry IS NOT BOUND.
        sub_entry = entry->add_subentry( key ).
      ENDIF.
      entry = sub_entry.
    ENDLOOP.
* After successful processing of chain, ENTRY will contain the last-created node

  ENDMETHOD.                    "create_by_path

*--------------------------------------------------------------------*
* GET_PARENT - retrieve parent entry of this entry
*--------------------------------------------------------------------*
  METHOD get_parent.
*   Return the parent of the current key
    parent = get_entry_by_internal_key( parent_key ).
  ENDMETHOD.                    "get_parent

*--------------------------------------------------------------------*
* GET_SUBENTRY - return single child entry by key
*--------------------------------------------------------------------*
  METHOD get_subentry.

    CLEAR entry.
*   Read internal store of sub-entries
    CHECK line_exists( sub_entries[ key = key ] ).

    DATA(lv_value) = sub_entries[ key = key ]-value.

*   Search global index of registry entry instances
*   read table sub_entrobj into ko with key key = kv-value.

    entry = VALUE #( registry_entries[ key =  lv_value ]-value
               DEFAULT NEW #( internal_key = lv_value  ) ).
*   Create new reference to sub-entry, Will insert itself into registry entries

  ENDMETHOD.                    "get_subentry

*--------------------------------------------------------------------*
* GET_SUBENTRIES - return immediate children registry entries
*--------------------------------------------------------------------*
  METHOD get_subentries.
    LOOP AT get_subentry_keys( ) INTO DATA(subkey).
      INSERT VALUE #( key = subkey
                      value = get_subentry( subkey ) ) INTO TABLE sub_entries.
    ENDLOOP.
  ENDMETHOD.                    "get_subentries

  DEFINE validate.
*   Prevent any changes if this entry is marked as deleted
    IF deleted = abap_true.
      RAISE EXCEPTION TYPE lcx_registry_entry_deleted.
    ENDIF.
  END-OF-DEFINITION.

*--------------------------------------------------------------------*
* ADD_SUBENTRY - add a child entry with new key and save
*--------------------------------------------------------------------*
  METHOD add_subentry.
    validate.

*   Check that only allowed characters are used. Will help for making
*   sensible paths and string handling in other applications
*   Most of all, we want to avoid spaces and slashes (although those
*   square and curly brackets could cause problems for JSON...)
    IF NOT key CO 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890@#$%^_+-(){}[]'.
      RAISE EXCEPTION TYPE lcx_registry_invalid_char.
    ENDIF.

*   Read internal store of sub-entries
    IF line_exists( sub_entries[ key = key ] ).
      RAISE EXCEPTION TYPE lcx_registry_entry_exists.
    ENDIF.

    DATA(lv_value) = lock->get_uuid( ).
    INSERT VALUE #( key = key
                    value = lv_value ) INTO TABLE sub_entries.

*     Create an entry on the database for the new entry
    DATA lt_empty_vals TYPE tt_keyval.
    DATA lv_srtfd TYPE indx_srtfd.
    lv_srtfd = lv_value.

    EXPORT values = lt_empty_vals
           sub_entries = lt_empty_vals
           parent = internal_key
           entry_id = key  TO DATABASE indx(zr) ID lv_srtfd.

    entry = NEW #( internal_key = lv_value ).
*   Will insert itself into registry entries

** Set current entry as the parent of the new entry
*    entry->parent_key = internal_key.
** Set short ID on the new entry
*    entry->entry_id = key.
** Save the new entry
*    entry->save( ).

*     Save the current entry to update the list of sub-keys
    save( ).

  ENDMETHOD.                    "add_subentry

*--------------------------------------------------------------------*
* COPY_SUBENTRY - copy a child registry entry at the same level,
*  including all values, by passing a source and target key
*--------------------------------------------------------------------*
  METHOD copy_subentry.
    validate.

    DATA(source_entry) = get_subentry( source_key ).
    IF source_entry IS NOT BOUND.
      RAISE EXCEPTION TYPE lcx_registry_noentry.
    ENDIF.

*     Using the source and the new target, do a deep copy
*     that includes copies of sub-entries and values
    copy_subentry_deep( source = source_entry
                        target = add_subentry( target_key ) ).

  ENDMETHOD.                    "copy_subentry

*--------------------------------------------------------------------*
* COPY_SUBENTRY_DEEP - (protected) - copy a branch of the registry
*         at the same level, including all values
*--------------------------------------------------------------------*
  METHOD copy_subentry_deep.

*     Copy values from source to target
    target->values = source->values.

*     Copy sub-entries from source to target
    LOOP AT source->sub_entries INTO DATA(ls_subentry).
      copy_subentry_deep( source = source->get_subentry( ls_subentry-key )
                          target = target->add_subentry( ls_subentry-key ) ).
    ENDLOOP.

*     Ensure that values are also saved
    save( ).

  ENDMETHOD.                    "copy_subentry_deep

*--------------------------------------------------------------------*
* REMOVE_SUBENTRIES - remove all child entries of this entry
*--------------------------------------------------------------------*
  METHOD remove_subentries.
    validate.

    LOOP AT sub_entries INTO DATA(kv).
      remove_subentry( kv-key ).
    ENDLOOP.
  ENDMETHOD.                    "remove_subentries

*--------------------------------------------------------------------*
* DELETE - delete the current entry from the database and mark it,
*          preventing any further operations on this entry
*--------------------------------------------------------------------*
  METHOD delete.
    validate.

*   Delete all sub-entries before deleting this entry
    LOOP AT sub_entries INTO DATA(sub_entry).
      get_subentry( sub_entry-key )->delete( ).
      DELETE sub_entries.
    ENDLOOP.

*   Remove DB entry for the current entry
    lock->promote( ).
    DELETE FROM DATABASE indx(zr) ID internal_key.
*   Object removes itself from the global table too so that that reference no longer exists
    DELETE registry_entries WHERE key = internal_key.
*   Set the object to deleted to prevent any operations on any remaining references to the object
    deleted = abap_true.

*   Release lock held on this key
    lock->release( ).

  ENDMETHOD.                    "delete

*--------------------------------------------------------------------*
* REMOVE_SUBENTRY - remove a single child registry entry by key
*--------------------------------------------------------------------*
  METHOD remove_subentry.
    validate.

*   Read internal store of sub-entries
    IF NOT line_exists( sub_entries[ key = key ] ).
*     Entry does not exist; exit with error
      RAISE EXCEPTION TYPE lcx_registry_noentry.
    ENDIF.

*     Remove all sub-entries of the sub-entry before removing the sub-entry
    DATA(sub_entry) = get_subentry( key ).
    CHECK sub_entry IS BOUND.

*   Delete the sub_entry (which deletes its sub-entries)
    sub_entry->delete( ).
*   Remove entry from sub-entry table and shadow table
    DELETE sub_entries WHERE key = key.

    save( ). "Save current entry to remove subentry that has been removed

  ENDMETHOD.                    "remove_subentry

*--------------------------------------------------------------------*
* SAVE - save the current entry, with concurrency control
*--------------------------------------------------------------------*
  METHOD save.
    validate.

    lock->promote( ).
    EXPORT values = me->values
           sub_entries = me->sub_entries
           parent = parent_key
           entry_id = entry_id  TO DATABASE indx(zr) ID internal_key.
    lock->set_optimistic( ).
  ENDMETHOD.                    "save

*--------------------------------------------------------------------*
* GET_SUBENTRY_KEYS - retrieve keys of all child registry entries
*--------------------------------------------------------------------*
  METHOD get_subentry_keys.
    keys = VALUE #( FOR kv IN sub_entries ( kv-key ) ).
  ENDMETHOD.                    "get_subentry_keys

*--------------------------------------------------------------------*
* GET_VALUE_KEYS - retrieve keys of all values
*--------------------------------------------------------------------*
  METHOD get_value_keys.
    keys = VALUE #( FOR kv IN values ( kv-key ) ).
  ENDMETHOD.                    "get_value_keys

*--------------------------------------------------------------------*
* GET_VALUES - retrieve all values at once in key+value table
*--------------------------------------------------------------------*
  METHOD get_values.
    values = me->values.
  ENDMETHOD.                    "get_values

*--------------------------------------------------------------------*
* SET_VALUES - set all values at once with key+value table
*--------------------------------------------------------------------*
  METHOD set_values.
    validate.

    me->values = values.
  ENDMETHOD.                    "set_values

*--------------------------------------------------------------------*
* GET_VALUE - return a single value by key
*--------------------------------------------------------------------*
  METHOD get_value.
    TRY.
        value = values[ key = key ]-value.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE lcx_registry_noentry.
    ENDTRY.
  ENDMETHOD.                    "get_value

  METHOD set_value.
    validate.

*   Add the value to set of values if not existing or change if it does exist
    READ TABLE values INTO DATA(kv) WITH KEY key = key.
    IF sy-subrc NE 0.
      INSERT VALUE #( key = key
                      value = value ) INTO TABLE values.
    ELSE.
      kv-value = value.
      MODIFY TABLE values FROM kv.
    ENDIF.
  ENDMETHOD.                    "set_value

  METHOD delete_value.
    validate.

    DELETE values WHERE key = key.
  ENDMETHOD.                    "delete_value

ENDCLASS.                    "lcl_registry_entry IMPLEMENTATION

CLASS lcl_registry_lock IMPLEMENTATION.

**********************************************************************
* CONCURRENCY HELPER METHODS
**********************************************************************
  METHOD constructor.
    super->constructor( ).
    internal_key = key.
  ENDMETHOD.

*--------------------------------------------------------------------*
* SET_OPTIMISTIC_LOCK - always set when (re-)reading an entry
*--------------------------------------------------------------------*
  METHOD set_optimistic.
* Existing lock must be released before acquiring a new one
    release( ).
    CALL FUNCTION 'ENQUEUE_ESINDX'
      EXPORTING
        mode_indx      = 'O'
        relid          = 'ZR'
        srtfd          = internal_key
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_registry_lock.
    ENDIF.
  ENDMETHOD.                    "set_optimistic_lock

*--------------------------------------------------------------------*
* PROMOTE_LOCK - Get exclusive lock just before saving
*--------------------------------------------------------------------*
  METHOD promote.
    CALL FUNCTION 'ENQUEUE_ESINDX'
      EXPORTING
        mode_indx      = 'R'
        relid          = 'ZR'
        srtfd          = internal_key
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_registry_lock.
    ENDIF.
  ENDMETHOD.                    "promote_lock

*--------------------------------------------------------------------*
* RELEASE_LOCK - called after deleting or before re-acquiring
*--------------------------------------------------------------------*
  METHOD release.
    CALL FUNCTION 'DEQUEUE_ESINDX'
      EXPORTING
        relid = 'ZR'
        srtfd = internal_key.
  ENDMETHOD.                    "release_lock

  METHOD get_uuid.
*   Create unique ID for key in INDX for the new entry
    TRY.
        rv_uuid = cl_system_uuid=>create_uuid_c22_static( ).
      CATCH cx_uuid_error.
        RAISE EXCEPTION TYPE lcx_registry_err.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
