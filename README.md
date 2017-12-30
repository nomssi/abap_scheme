<<<<<<< HEAD
# abap_scheme
LISP interpreter in ABAP with Editor
=======
# Scheme interpreter in ABAP

[![Language: ABAP](https://img.shields.io/badge/Language-ABAP-blue.svg?style=flat)](https://www.sap.com/developer/topics/abap-platform.html)
[![License: MIT](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat)](https://opensource.org/licenses/MIT)

## Overview 

After reading Conrad Barski's <a href="http://landoflisp.com/">Land of Lisp</a> I was curious to understand Martin Ceronio's 
<a href="https://blogs.sap.com/2015/06/24/a-lisp-interpreter-in-abap/">LISP interpreter in ABAP</a>.

### Features

This project started as a fork of https://github.com/mydoghasworms/abap-lisp. I converted the regression tests to ABAP Unit and added validations, an editor and more <a href="https://mitpress.mit.edu/sicp/">Scheme</a> features, so this is now a simple Scheme interpreter with ABAP integration. 

- more validation to avoid dumps from incorrect syntax
- a workbench that makes it easier to play with the interpreter
- Comments using the ; delimiter
- native operations: set!, let, let*, letrec, cond, and, or, reverse, append!
- predicates: number? string? symbol? hash? procedure? list? zero? positive? negative? odd? even? eq?
- trigonometric functions: sin cos tan sinh cosh tanh asin acos atan asinh acosh atanh
- math: quotient remainder modulo abs floor ceiling truncate round expt exp sqrt
- display / newline - are not working properly yet

### Architecture

- Report ZZ_LISP_IDE - Main report
- Include YY_LIB_LISP - Custom version of ABAP LISP
- Include YY_LISP_AUNIT - ABAP Unit tests
- Include YY_LISP_IDE 

### Requirements

The current version was tested on ABAP Netweaver 7.50 and can be cloned with <a href="http://docs.abapgit.org/">ABAP GIT</a>. For an older version, check the <a href="https://wiki.scn.sap.com/wiki/display/Snippets/Lisp+Interpreter+in+ABAP">SCN Code Gallery</a>.

## FAQ
For questions/comments/bugs/feature requests/wishes please create an [issue](https://github.com/nomssi/abap_scheme/issues)

![abap_scheme](https://github.com/nomssi/abap_scheme/blob/master/img/abap_lisp_workbench.png)
>>>>>>> parent of d0a35d0... apply and other changes
