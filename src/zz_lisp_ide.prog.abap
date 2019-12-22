REPORT zz_lisp_ide.

INCLUDE yy_lib_lisp.

INCLUDE yy_lisp_turtle.
INCLUDE yy_lisp_aunit.
INCLUDE yy_lisp_ide.

INITIALIZATION.
  lcl_ide=>init( ).

START-OF-SELECTION.
  lcl_ide=>main( ).
