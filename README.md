# abap_scheme
# LISP interpreter in ABAP with Editor

[![Language: ABAP](https://img.shields.io/badge/Language-ABAP-blue.svg?style=flat)](https://www.sap.com/developer/topics/abap-platform.html)
[![License: MIT](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat)](https://opensource.org/licenses/MIT)

## Overview 

After reading Conrad Barski's <a href="http://landoflisp.com/">Land of Lisp</a> I was curious to understand Martin Ceronio's <a href="https://blogs.sap.com/2015/06/24/a-lisp-interpreter-in-abap/">LISP interpreter in ABAP</a>.

This is my [Make a Lisp](https://github.com/kanaka/mal/blob/master/process/guide.md) learning project that started as a fork of https://github.com/mydoghasworms/abap-lisp that was inspired by [Peter Norvig](http://norvig.com/lispy2.html).

I converted the regression tests to ABAP Unit, added validations, an editor and many [Scheme](http://www.r7rs.org/) features but I won't call this LISP interpreter a _Scheme_ until issue #1 is resolved.

### Requirements

The current version was tested on ABAP Netweaver 7.50 and can be cloned with <a href="http://docs.abapgit.org/">ABAP GIT</a>. For an older version, check the <a href="https://wiki.scn.sap.com/wiki/display/Snippets/Lisp+Interpreter+in+ABAP">SCN Code Gallery</a>.

### Architecture

- Report ZZ_LISP_IDE - Main report
- Include YY_LIB_LISP - Custom version of ABAP LISP
- Include YY_LISP_AUNIT - ABAP Unit tests
- Include YY_LISP_IDE 

![abap_scheme](https://github.com/nomssi/abap_scheme/blob/master/img/abap_lisp_workbench.png)
The ABAP LISP Workbench has two commands:
- Evaluate (F8) executes the Scheme code 
- Refresh (Shift-F2) will delete the source and console and reset the environment

### ABAP Integration
#### Interpreter
Class `lcl_lisp_interpreter` evaluates your Scheme code in a string `code`, using either method `eval_repl( code )` which throws an exception on errors, or method `eval_source( code )` catches exception:

      DATA(response) = NEW lcl_lisp_interpreter( )->eval_source( code ).

#### Access to ABAP Fields
For a [dynamic IF statement](https://blogs.sap.com/2016/02/29/dynamic-if-condition/)
     `( PLAAB = '02' ) and ( DELKZ = 'BB') and ( LIFNR > '' ) and ( PLUMI = '-')` 
we concatenate the following Scheme expression in a string variable `code` and evaluate. 

    (let 
    ; Define local fields
         (PLAAB (ab-data "GS_MDPS-PLAAB" ))
         (DELKZ (ab-data "GS_MDPS-DELKZ" ))
         (LIFNR (ab-data "GS_MDPS-LIFNR" ))
         (PLUMI (ab-data "GS_MDPS-PLUMI" ))
      (and (= PLAAB '02') (= DELKZ 'BB') (> LIFNR '') (= PLUMI '-')) )

The result on the expression either `#t` or `#f`.

#### Function Module Call
    (let (( profiles
      (let ( (f3 (ab-function "BAPI_USER_GET_DETAIL"))  )
        ( begin (ab-set f3 "USERNAME" (ab-get ab-sy "UNAME") )
                  (f3) (ab-get f3 "PROFILES")  ) )
        ) )
     (let ((profile (ab-get profiles 1)) )
                (ab-get profile "BAPIPROF" )  )

#### Optional: Console Interface

      INTERFACE lif_port.
        METHODS write IMPORTING element TYPE REF TO lcl_lisp
                      RETURNING VALUE(rv_input) TYPE string.

        METHODS read IMPORTING iv_input        TYPE string OPTIONAL
                     RETURNING VALUE(rv_input) TYPE string.
      ENDINTERFACE.

## FAQ
For questions/comments/bugs/feature requests/wishes please create an [issue](https://github.com/nomssi/abap_scheme/issues)
