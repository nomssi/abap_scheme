# abap_scheme
[![Language: ABAP](https://img.shields.io/badge/Language-ABAP-blue.svg?style=flat)](https://www.sap.com/developer/topics/abap-platform.html)
[![License: MIT](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat)](https://opensource.org/licenses/MIT)
# LISP interpreter in ABAP with Workbench

## Installation
The code can be cloned with [ABAP GIT](http://docs.abapgit.org/). Check for known issues #28 if you experience trouble.

- The main version is developed on Netweaver 7.5 and should work on ABAP Netweaver 7.4. Milestones are downported to other branches (7.02). The legacy code on [SCN Code Gallery](https://wiki.scn.sap.com/wiki/display/Snippets/Lisp+Interpreter+in+ABAP) should work on older releases.

- After installation, the [wiki pages](https://github.com/nomssi/abap_scheme/wiki) are a good place to start. Try this <i>guess my number</i> game

      (begin (display "Please enter a number between 1 - 99: ")
          (do ((quit #f)
               (guess 0)   
               (answer (+ 1 (random 100))) )
          (quit)
          (set! guess (read))
          (cond ((and (number? guess) (< guess answer)) (display "Too low. Please guess again: ") )
                ((and (number? guess) (> guess answer)) (display "Too high. Please guess again: ") )
                (else (set! quit #t) (if (number? guess) (display "Correct!")
                                                         (display "Good bye...") ) ) ) ) )

* For questions/comments/bugs/feature requests/wishes please create an [issue](https://github.com/nomssi/abap_scheme/issues)
* How to [enable the new editor](/editor)

## What is Scheme?

- from Wikipedia: [Scheme](https://en.wikipedia.org/wiki/Scheme_%28programming_language%29) is a programming language that supports multiple paradigms, including functional and imperative programming. It is one of the three main dialects of Lisp, alongside Common Lisp and Clojure.

- any valid Scheme source code or data is represented using a tree structure (s-expression). e.g. 

S-expression for (* 2 (+ 3 4)) | workbench view
--- | ---
[![as tree](https://upload.wikimedia.org/wikipedia/commons/thumb/e/e3/Corrected_S-expression_tree_2.png/220px-Corrected_S-expression_tree_2.png)] |  [![workbench view](https://github.com/nomssi/abap_scheme/blob/master/img/sample_sexp.png)]

that can be passed to a logic (functional programming). Code is data, data is code. It is a common pattern to use the quote command to say just don’t execute this code yet, treat it as data. 

You can [Make your own Lisp](https://github.com/kanaka/mal/blob/master/process/guide.md) to really understand how an interpreter works.

Read the _Revised <sup>7</sup> Report on the Algorithmic Language Scheme_, aka [R7RS](http://www.r7rs.org/) or Conrad Barski's <a href="http://landoflisp.com/">Land of Lisp</a> to understand the power of lisp.
[![LISP Inside](https://github.com/nomssi/abap_scheme/blob/master/img/lisplogo_256.png)](http://lisperati.com/logo.html)

### Features
After reading Conrad Barski's <a href="http://landoflisp.com/">Land of Lisp</a> I was curious to understand Martin Ceronio's <a href="https://blogs.sap.com/2015/06/24/a-lisp-interpreter-in-abap/">LISP interpreter in ABAP</a> inspired by [Peter Norvig](http://norvig.com/lispy2.html). So I started my project based on [https://github.com/mydoghasworms/abap-lisp](https://github.com/mydoghasworms/abap-lisp). 

- target [R7RS](http://www.r7rs.org/) including proper treatment of tail calls. In constrast to ABAP, Scheme is lexically scoped with proper tail call optimization.
- a programming environment to make it fun to use, featuring the editor and console views, a trace view, a graphical S-Expression viewer

### Architecture

- Report ZZ_LISP_IDE - Main report for the workbench
- Include YY_LIB_LISP - Complete ABAP LISP library
- Include YY_LISP_AUNIT - a large _ABAP Unit_ regression test suite
- Include YY_LISP_IDE - Editor/Tools

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

![abap_scheme](https://github.com/nomssi/abap_scheme/blob/master/img/abap_scheme_workbench.png)
![abap_trace](https://github.com/nomssi/abap_scheme/blob/master/img/abap_lisp_trace.png)
![abap_expression](https://github.com/nomssi/abap_scheme/blob/master/img/SExpressionViewer.png)


Capturing our 15 minutes of fame: The ABAP Scheme [announcement](https://blogs.sap.com/2018/02/01/announcing-the-abap-scheme-workbench/) was popular in the SAP Community ![SAP Community](https://github.com/nomssi/abap_scheme/blob/master/img/popular_blogs.png)
