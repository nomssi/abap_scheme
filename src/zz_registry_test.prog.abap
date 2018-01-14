*&---------------------------------------------------------------------*
*& Report ZZ_REGISTRY_TEST
*&---------------------------------------------------------------------*
*&   Author: Martin Ceronio (2015), http://ceronio.net
*& Released under MIT License: http://opensource.org/licenses/MIT
*& All modifications by JNN
*&---------------------------------------------------------------------*
REPORT zz_registry_test.
* Make the registry API available to our program

INCLUDE yy_lib_registry.

DATA reg_entry TYPE REF TO lcl_registry_entry.
DATA lv_customer TYPE kunnr.
DATA lv_run_date TYPE d.
DATA lv_timestamp TYPE timestamp.

START-OF-SELECTION.
* Get the root entry of the registry
  DATA(reg_root) = lcl_registry_entry=>get_root( ).

* If we want to ensure, on startup, that a certain entry exists, we
* could do the following (e.g. in LOAD-OF-PROGRAM):
  reg_root->create_by_path( 'Sales/Enhancements/Process_XYZ' ).

* Retrieval of a specific entry. If we did not have the above line,
* we would have to check that the result of each call to GET_SUBENTRY( )
* to ensure it is bound.
  reg_entry = reg_root->get_subentry( 'Sales' )->get_subentry( 'Enhancements' )->get_subentry( 'Process_XYZ' ).

* Getting a specific value from the entry:
  lv_customer = reg_entry->get_value( 'Process_XYZ' ).
  "lv_customer = reg_entry->get_value( 'Process_XYZCustomer' ).

* Writing values to the entry:
  lv_run_date = sy-datum.
  reg_entry->set_value( key = 'LastRunDate' value = lv_run_date ).
  GET TIME STAMP FIELD lv_timestamp.
  reg_entry->set_value( key = 'LastRunDateTime' value = lv_timestamp ).

* Saving the entry
  reg_entry->save( ).
