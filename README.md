# abap_scheme
# LISP interpreter in ABAP with Workbench

[![Language: ABAP](https://img.shields.io/badge/Language-ABAP-blue.svg?style=flat)](https://www.sap.com/developer/topics/abap-platform.html)
[![License: MIT](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat)](https://opensource.org/licenses/MIT)

## Overview 

After reading Conrad Barski's <a href="http://landoflisp.com/">Land of Lisp</a> I was curious to understand Martin Ceronio's <a href="https://blogs.sap.com/2015/06/24/a-lisp-interpreter-in-abap/">LISP interpreter in ABAP</a> inspired by [Peter Norvig](http://norvig.com/lispy2.html).
So I started my [Make your own Lisp](https://github.com/kanaka/mal/blob/master/process/guide.md) project based on [https://github.com/mydoghasworms/abap-lisp](https://github.com/mydoghasworms/abap-lisp). 

[![LISP Inside](https://github.com/nomssi/abap_scheme/blob/master/img/lisplogo_256.png)](http://lisperati.com/logo.html)

My contributions:
- target the _Revised <sup>7</sup> Report on the Algorithmic Language Scheme_, aka [R7RS](http://www.r7rs.org/) including proper treatment of tail calls
- a programming environment to make it fun to use, featuring the editor and console views, a trace view, a graphical S-Expression viewer

![abap_scheme](https://github.com/nomssi/abap_scheme/blob/master/img/abap_scheme_workbench.png)
![abap_trace](https://github.com/nomssi/abap_scheme/blob/master/img/abap_lisp_trace.png)
![abap_expression](https://github.com/nomssi/abap_scheme/blob/master/img/SExpressionViewer.png)

Check the [wiki pages](https://github.com/nomssi/abap_scheme/wiki) for documentation.

Capturing our 15 minutes of fame: The ABAP Scheme [announcement](https://blogs.sap.com/2018/02/01/announcing-the-abap-scheme-workbench/) was popular in the SAP Community ![SAP Community](https://github.com/nomssi/abap_scheme/blob/master/img/popular_blogs.png)

### Architecture

- Report ZZ_LISP_IDE - Main report for the workbench
- Include YY_LIB_LISP - Complete ABAP LISP library
- Include YY_LISP_AUNIT - a large _ABAP Unit_ regression test suite
- Include YY_LISP_IDE - Editor/Tools

The code can be cloned with [ABAP GIT](http://docs.abapgit.org/) 

- The main version is developed on Netweaver 7.5 and should work on ABAP Netweaver 7.4
- Milestones are downported to ABAP Netweaver 7.02 - use the 7.02 branch 
      Note the Eval. with Trace is not supported in this version
- A version is available that should work on older releases, but is not maintained anymore
      Note: check the [SCN Code Gallery](https://wiki.scn.sap.com/wiki/display/Snippets/Lisp+Interpreter+in+ABAP).

![class_diagram](https://github.com/nomssi/abap_scheme/blob/master/img/zz_lisp_uml.png)

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
        ((PLAAB (ab-data "GS_MDPS-PLAAB" ))
         (DELKZ (ab-data "GS_MDPS-DELKZ" ))
         (LIFNR (ab-data "GS_MDPS-LIFNR" ))
         (PLUMI (ab-data "GS_MDPS-PLUMI" )))
      (and (= PLAAB '02') (= DELKZ 'BB') (> LIFNR '') (= PLUMI '-')) ))

The result on the expression either `#t` or `#f`.

#### Function Module Call
    (let (( profiles
      (let ( (f3 (ab-function "BAPI_USER_GET_DETAIL"))  )  
        ( begin (ab-set f3 "USERNAME" (ab-get ab-sy "UNAME") )  ; param USERNAME = sy-uname
                  (f3)                                          ; function module call
                  (ab-get f3 "PROFILES")  ) )                   ; return table PROFILES
        ) )
     (let ((profile (ab-get profiles 1)) )         ; read table PROFILES index 1 INTO profile 
                (ab-get profile "BAPIPROF" )  ) )  ; read field profile-bapiprof

#### Optional: Console Interface

      INTERFACE lif_input_port.
        METHODS read IMPORTING iv_title        TYPE string OPTIONAL
                     RETURNING VALUE(rv_input) TYPE string.
        METHODS peek_char RETURNING VALUE(rv_char) TYPE char01.
        METHODS is_char_ready RETURNING VALUE(rv_flag) TYPE flag.
        METHODS read_char RETURNING VALUE(rv_char) TYPE char01.
        METHODS put IMPORTING iv_text TYPE string.
      ENDINTERFACE.
    
      INTERFACE lif_output_port.
        METHODS write IMPORTING element TYPE REF TO lcl_lisp.
        METHODS display IMPORTING element TYPE REF TO lcl_lisp
                        RAISING   lcx_lisp_exception.
      ENDINTERFACE.

## FAQ
* How to [enable the new editor](/editor)
* For questions/comments/bugs/feature requests/wishes please create an [issue](https://github.com/nomssi/abap_scheme/issues)
