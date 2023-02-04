# ABAP Scheme
[![Language: ABAP](https://img.shields.io/badge/Language-ABAP-blue.svg?style=flat)](https://www.sap.com/developer/topics/abap-platform.html)
[![License: MIT](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat)](https://opensource.org/licenses/MIT)

- is an interpreter for Scheme, a Lisp dialect with exceptionally clear and concise semantics and a focus on functional programming
- provides a SAP GUI based workbench for Scheme
- is written in, and can be called from ABAP

## Getting Started
The code can be cloned with [ABAP GIT](http://docs.abapgit.org/).

- The main version is developed on Netweaver 7.5 and should work on ABAP Netweaver 7.4. 
- Milestones are downported to other branches (7.02). 
- The legacy code on [SCN Code Gallery](https://wiki.scn.sap.com/wiki/display/Snippets/Lisp+Interpreter+in+ABAP) should work on older releases.

To check your installation, execute this <i>guess my number</i> game...

```Scheme
      (begin (display "Please enter a number between 1 - 100: ")
          (do ((quit #f)
               (guess 0)   
               (answer (+ 1 (random 100))) )
          (quit)
          (begin (set! guess (read)) (display guess) )
          (cond ((and (number? guess) (< guess answer)) (display "\nToo low. Please guess again: ") )
                ((and (number? guess) (> guess answer)) (display "\nToo high. Please guess again: ") )
                (else (set! quit #t) (if (number? guess) (display "\nCorrect!")
                                                         (display "\nGood bye...") ) ) ) ) )
```

Scheme syntax is based upon nested parenthesization. The [wiki pages](https://github.com/nomssi/abap_scheme/wiki) are a good place to start.

* I suggest you check the [syntax](https://github.com/nomssi/abap_scheme/wiki/Learn-Try-Scheme) and understand [Lists](https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Lists.html#Lists).
* For questions/comments/bugs/feature requests/wishes please create an [issue](https://github.com/nomssi/abap_scheme/issues)
* How to [enable the new editor](/editor)

## Why Scheme?
- [Scheme](https://en.wikipedia.org/wiki/Scheme_%28programming_language%29) is one of the main Lisp dialects, alongside *Common Lisp* and *Clojure*. Conrad Barski's [Land of Lisp](http://landoflisp.com), Martin Ceronio's [LISP interpreter in ABAP](https://blogs.sap.com/2015/06/24/a-lisp-interpreter-in-abap/) and [Peter Norvig](http://norvig.com/lispy2.html) inspired me to learn Lisp. It is common to [Make your own Lisp](https://github.com/kanaka/mal/blob/master/process/guide.md) to really understand Lisp's core concepts.

[![LISP Inside](https://github.com/nomssi/abap_scheme/blob/master/img/lisplogo_256.png)](http://lisperati.com/logo.html)

- Scheme's uses *symbolic expressions* (S-exps) to represent code *and* data. Expressions are then [evaluated](https://docs.racket-lang.org/reference/eval-model.html). Those  concepts cannot be expressed in ABAP, except by first implementing a Lisp interpreter in ABAP ([Greenspun 10th rule](http://www.paulgraham.com/quotes.html) ).

- My initial plan was to write a Lisp workbench for [Martin's Lisp interpreter](https://github.com/mydoghasworms/abap-lisp). I changed the target language after reading the *Revised <sup>7</sup> Report on the Algorithmic Language Scheme* aka [R7RS small](http://www.r7rs.org/) that offers a lot of examples to verify the interpreter. With this I can aim at compatibility with open source Scheme code.

- In constrast to ABAP, Scheme has a very small number of rules for forming expressions that can be composed without restrictions. Scheme is lexically scoped and requires proper tail call optimization. Scheme is apt at [symbolic processing](https://github.com/nomssi/abap_scheme/wiki/Learn-Try-Symbolic-Derivation).

### Features
- ABAP Scheme supports a subset of [R7RS](http://www.r7rs.org/) with some [Racket](https://docs.racket-lang.org/) extensions. Check the current [list of features](https://github.com/nomssi/abap_scheme/wiki/Features)
- This documentation is the source of many of the 500+ tests implemented in the ABAP unit test suite. 
- Access to ABAP global fields and function modules
- a programming environment to make it fun to use, featuring the editor and console views, a trace view, a graphical S-Expression viewer

S-expression for (* 2 (+ 3 4)) | workbench view
--- | ---
![s-exp](https://upload.wikimedia.org/wikipedia/commons/thumb/e/e3/Corrected_S-expression_tree_2.png/220px-Corrected_S-expression_tree_2.png) |  ![workbench view](https://github.com/nomssi/abap_scheme/blob/master/img/sexpr_new.PNG)

- R7RS alignment makes it easier to run open source Scheme code. This is however limited, as *first class continuations* (call cc) and *hygienic macros* (define-syntax) are missing

### Architecture

- Report ZZ_LISP_IDE - Main report for the workbench
- Include YY_LIB_LISP - Complete ABAP LISP library
- Include YY_LISP_AUNIT - a large _ABAP Unit_ regression test suite
- Include YY_LISP_IDE - Editor/Tools

### ABAP Integration
#### Interpreter
Class `lcl_lisp_interpreter` evaluates your Scheme code in a string `code`, using either method `eval_repl( code )` which throws an exception on errors, or method `eval_source( code )` catches exception:

```ABAP
      DATA(response) = NEW lcl_lisp_interpreter( io_port = port 
                                                 ii_log = log )->eval_source( code ).
```
`port` is a buffered port that can allow input or output. `log` implements a simple logging interface with 2 methods, put( ) and get( ).
#### Access to ABAP Fields
For a [dynamic IF statement](https://blogs.sap.com/2016/02/29/dynamic-if-condition/)
     `( PLAAB = '02' ) and ( DELKZ = 'BB') and ( LIFNR > '' ) and ( PLUMI = '-')` 
we concatenate the following Scheme expression in a string variable `code` and evaluate. 

```Scheme
    (let 
    ; Define local fields
        ((PLAAB (ab-data "GS_MDPS-PLAAB" ))
         (DELKZ (ab-data "GS_MDPS-DELKZ" ))
         (LIFNR (ab-data "GS_MDPS-LIFNR" ))
         (PLUMI (ab-data "GS_MDPS-PLUMI" )))
      (and (= PLAAB '02') (= DELKZ 'BB') (> LIFNR '') (= PLUMI '-')) )
```

The result on the expression either `#t` or `#f`.

#### Function Module Call

```Scheme
    (let (( profiles
      (let ( (f3 (ab-function "BAPI_USER_GET_DETAIL"))  )  
        ( begin (ab-set f3 "USERNAME" (ab-get ab-sy "UNAME") )  ; param USERNAME = sy-uname
                  (f3)                                          ; function module call
                  (ab-get f3 "PROFILES")  ) )                   ; return table PROFILES
        ) )
     (let ((profile (ab-get profiles 1)) )         ; read table PROFILES index 1 INTO profile 
                (ab-get profile "BAPIPROF" )  ) )  ; read field profile-bapiprof
```

#### Optional: Console Interface

```ABAP
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
```

### Workbench
![abap_scheme](https://github.com/nomssi/abap_scheme/blob/master/img/abap_scheme_workbench.png)
![abap_trace](https://github.com/nomssi/abap_scheme/blob/master/img/abap_lisp_trace.png)
![abap_expression](https://github.com/nomssi/abap_scheme/blob/master/img/SExpressionViewer.png)


Read the ABAP Scheme [announcement](https://blogs.sap.com/2018/02/01/announcing-the-abap-scheme-workbench/) blog
