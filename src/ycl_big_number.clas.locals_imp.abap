*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

INTERFACE lif_unit_test.
ENDINTERFACE.

  TYPES: BEGIN OF ENUM tv_order BASE TYPE i,
           smaller VALUE -1,
           equal   VALUE IS INITIAL,
           larger  VALUE +1,
         END OF ENUM tv_order.

CLASS lcx_div_zero DEFINITION INHERITING FROM cx_sy_arithmetic_error.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !previous        LIKE previous OPTIONAL
        VALUE(operation) TYPE string OPTIONAL.
ENDCLASS.

CLASS lcx_div_zero IMPLEMENTATION.

  METHOD constructor.
    super->constructor( textid   = '155FB739985CE619E10000000A11447B'
                        previous = previous
                        operation = operation ).
  ENDMETHOD.

ENDCLASS.

CLASS lcx_too_big DEFINITION INHERITING FROM cx_sy_arithmetic_error.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !previous        LIKE previous OPTIONAL
        VALUE(operation) TYPE string OPTIONAL.
ENDCLASS.

CLASS lcx_too_big IMPLEMENTATION.

  METHOD constructor.
    super->constructor( textid   = '4B5DB739AB5CE919E10000000A11447B'
                        previous = previous
                        operation = operation ).
  ENDMETHOD.

ENDCLASS.

CLASS lcx_negative_exponent DEFINITION INHERITING FROM cx_sy_arithmetic_error.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !previous        LIKE previous OPTIONAL
        VALUE(operation) TYPE string OPTIONAL.
ENDCLASS.

CLASS lcx_negative_exponent IMPLEMENTATION.

  METHOD constructor.
    super->constructor( textid   = '4B5DB739AB5CE919E10000000A11447B'
                        previous = previous
                        operation = operation ).
  ENDMETHOD.

ENDCLASS.

  TYPES tv_sign_char TYPE c LENGTH 1.
  TYPES: BEGIN OF ENUM tv_sign BASE TYPE c,
           positive  VALUE IS INITIAL,
           negative  VALUE '-',
         END OF ENUM tv_sign.

CLASS lcl_lisp_bigintx DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    TYPES tv_int TYPE int8.            " integer data type, use int8 if available
    TYPES tv_real TYPE decfloat34.     " real data type

    TYPES tr_bigint TYPE REF TO lcl_lisp_bigintx.
    TYPES tt_bigint TYPE STANDARD TABLE OF tr_bigint.

    CLASS-DATA zero TYPE tr_bigint READ-ONLY.
    CLASS-DATA one TYPE tr_bigint READ-ONLY.
    CLASS-DATA minus_one TYPE tr_bigint READ-ONLY.
    CLASS-DATA two TYPE tr_bigint READ-ONLY.
    CLASS-DATA maxint TYPE tr_bigint READ-ONLY.

    CLASS-DATA fmantsize TYPE i READ-ONLY VALUE 15.       "#EC NOTEXT .
    CLASS-DATA dfmantsize TYPE i READ-ONLY VALUE 34.      "#EC NOTEXT .

    METHODS constructor.
    CLASS-METHODS clone IMPORTING VALUE(src)  TYPE tr_bigint
                        RETURNING VALUE(this) TYPE tr_bigint.

    METHODS set IMPORTING VALUE(src)  TYPE tr_bigint
                RETURNING VALUE(this) TYPE tr_bigint.

    METHODS set_dec IMPORTING VALUE(src)  TYPE f
                              VALUE(exp)  TYPE i DEFAULT 0
                    RETURNING VALUE(this) TYPE tr_bigint.
    METHODS set_float IMPORTING VALUE(src)  TYPE tv_real
                                VALUE(exp)  TYPE tv_int OPTIONAL
                      RETURNING VALUE(this) TYPE tr_bigint.
    METHODS set_int IMPORTING VALUE(src)  TYPE tv_int
                    RETURNING VALUE(this) TYPE tr_bigint.
    METHODS set_str IMPORTING VALUE(src)  TYPE string
                    RETURNING VALUE(this) TYPE tr_bigint.

    METHODS to_str IMPORTING VALUE(with_sign) TYPE abap_bool DEFAULT abap_true
                             VALUE(base) TYPE i DEFAULT 10
                             PREFERRED PARAMETER with_sign
                   RETURNING VALUE(s)         TYPE string.
    METHODS to_dec EXPORTING VALUE(mnt) TYPE f
                             VALUE(exp) TYPE tv_int.
    METHODS to_float EXPORTING VALUE(mnt) TYPE tv_real
                               VALUE(exp) TYPE tv_int.
    METHODS get_sign RETURNING VALUE(s) TYPE tv_sign_char.
    METHODS number_of_digits RETURNING VALUE(r) TYPE tv_int.

    METHODS abs RETURNING VALUE(this) TYPE tr_bigint.
    METHODS neg RETURNING VALUE(this) TYPE tr_bigint.
    METHODS is_zero RETURNING VALUE(zero) TYPE abap_bool.
    METHODS abscmp IMPORTING !x       TYPE tr_bigint
                   RETURNING VALUE(r) TYPE tv_order.
    METHODS cmp IMPORTING !x       TYPE tr_bigint
                RETURNING VALUE(r) TYPE tv_order.
    METHODS add IMPORTING !x          TYPE tr_bigint
                RETURNING VALUE(this) TYPE tr_bigint.

    METHODS sub IMPORTING !x          TYPE tr_bigint
                RETURNING VALUE(this) TYPE tr_bigint .
    METHODS mul IMPORTING !x          TYPE tr_bigint
                RETURNING VALUE(this) TYPE tr_bigint.
    METHODS divx IMPORTING !x       TYPE tr_bigint
                 RETURNING VALUE(r) TYPE tr_bigint
                 RAISING   lcx_div_zero. " EXCEPTIONS div_zero.
    METHODS div IMPORTING !x          TYPE tr_bigint
                RETURNING VALUE(this) TYPE tr_bigint
                RAISING   lcx_div_zero.
    METHODS mod    IMPORTING !x          TYPE tr_bigint
                   RETURNING VALUE(this) TYPE tr_bigint
                   RAISING   lcx_div_zero.
    METHODS pow IMPORTING !x          TYPE tr_bigint
                RETURNING VALUE(this) TYPE tr_bigint
                RAISING   lcx_negative_exponent lcx_too_big. " EXCEPTIONS negative_exponent too_big.
    METHODS powmod IMPORTING !x          TYPE tr_bigint
                             !m          TYPE tr_bigint
                   RETURNING VALUE(this) TYPE tr_bigint
                   RAISING   lcx_negative_exponent lcx_too_big. " EXCEPTIONS negative_exponent too_big.

    METHODS inc RETURNING VALUE(this) TYPE tr_bigint.
    METHODS dec RETURNING VALUE(this) TYPE tr_bigint.

    METHODS sqroot IMPORTING !x          TYPE tr_bigint
                   RETURNING VALUE(this) TYPE tr_bigint.
    METHODS div_by_2 IMPORTING !x          TYPE tr_bigint
                     RETURNING VALUE(this) TYPE tr_bigint.

    METHODS root IMPORTING !n          TYPE tr_bigint
                 RETURNING VALUE(this) TYPE tr_bigint.

    METHODS shiftLeft IMPORTING !n          TYPE tr_bigint
                      RETURNING VALUE(this) TYPE tr_bigint.

    METHODS gcd IMPORTING !n          TYPE tr_bigint
                RETURNING VALUE(this) TYPE tr_bigint.

    CLASS-METHODS class_constructor.
    METHODS debug IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    CLASS-METHODS strpad IMPORTING VALUE(n) TYPE tv_int
                                   VALUE(c) TYPE c
                                   VALUE(s) TYPE string
                         RETURNING VALUE(r) TYPE string .
  PROTECTED SECTION.
    TYPES tv_chunk TYPE tv_real.
    TYPES tt_chunk TYPE STANDARD TABLE OF tv_chunk.

    CONSTANTS fzero TYPE tv_chunk VALUE 0.
    CONSTANTS fhalf TYPE tv_chunk VALUE '0.5'.

    DATA sign TYPE tv_sign.
    DATA dat TYPE tt_chunk.

    CLASS-DATA chunksize TYPE tv_int VALUE 16.            "#EC NOTEXT .
    CLASS-DATA dchunksize TYPE tv_int VALUE 32.           "#EC NOTEXT .
    CLASS-DATA chunkmod TYPE tv_chunk VALUE `1e16`.       "#EC NOTEXT .
    CLASS-DATA dchunkmod TYPE tv_chunk VALUE `1e32`.      "#EC NOTEXT .

    CLASS-DATA o_denom TYPE tr_bigint.
    CLASS-DATA o_nummer TYPE tr_bigint.
    CLASS-DATA o_quotient TYPE tr_bigint.
    CLASS-DATA o_rest TYPE tr_bigint.

    METHODS free_bigint_table IMPORTING it_bigint TYPE tt_bigint.
ENDCLASS.

CLASS lcl_lisp_bigintx IMPLEMENTATION.

  METHOD abs.
    sign = positive.
    this = me.
  ENDMETHOD.

  METHOD abscmp.
    DATA(n1) = lines( dat ).
    DATA(n2) = lines( x->dat ).

    IF n1 > n2.
      r = larger.
    ELSEIF n1 < n2.
      r = smaller.
    ELSE.
      DATA i TYPE sy-tabix.

      DO n1 TIMES.
        i = n1 - sy-index + 1.
        READ TABLE dat INTO DATA(v1) INDEX i.
        READ TABLE x->dat INTO DATA(v2) INDEX i.
        IF v1 > v2.
          r = larger.
          RETURN.
        ELSEIF v1 < v2.
          r = smaller.
          RETURN.
        ENDIF.
      ENDDO.

      r = equal.

    ENDIF.

  ENDMETHOD.

*void BigInteger::add(const BigInteger &a, const BigInteger &b) {
*   DTRT_ALIASED(this == &a || this == &b, add(a, b));
*   // If one argument is zero, copy the other.
*   if (a.sign == zero)
*   operator =(b);
*   else if (b.sign == zero)
*   operator =(a);
*   // If the arguments have the same sign, take the
*   // common sign and add their magnitudes.
*   else if (a.sign == b.sign) {
*   sign = a.sign;
*   mag.add(a.mag, b.mag);
*   } else {
*   // Otherwise, their magnitudes must be compared.
*   switch (a.mag.compareTo(b.mag)) {
*   case equal:
*     // If their magnitudes are the same, copy zero.
*     mag = 0;
*     sign = zero;
*     break;
*     // Otherwise, take the sign of the greater, and subtract
*     // the lesser magnitude from the greater magnitude.
*   case greater:
*     sign = a.sign;
*     mag.subtract(a.mag, b.mag);
*     break;
*   case less:
*     sign = b.sign;
*     mag.subtract(b.mag, a.mag);
*     break;
*   }
*   }
*}

  METHOD add.
    " bei Unterschied im Vorzeichen auf Subtraktion wechseln;
    " dazu zwischendurch das Vorzeichen des Subtrahenden wechseln...
    IF sign NE x->sign.
      x->neg( ).
      sub( x ).
      x->neg( ).
      this = me.
      RETURN.
    ENDIF.

    DATA: result TYPE tt_chunk,
          i      TYPE tv_int,
          v1     TYPE tv_chunk,
          v2     TYPE tv_chunk,
          v3     TYPE tv_chunk,
          ov     TYPE tv_chunk.  " Carry, overflow

    ov = 0.   " carry <-- false
    DO nmax( val1 = lines( dat )
             val2 = lines( x->dat ) ) TIMES.
      i = sy-index.
      " Das Lesen aus den Tabellen ist tolerant gegenüber Wertebereichsüberschreitung des Index.
      " Aber in so einem Fall wird das Datenziel nicht genullt.
      CLEAR: v1, v2.  " Nullsetzung auf kürzestmögliche Weise
      READ TABLE    dat INTO v1 INDEX i.
      READ TABLE x->dat INTO v2 INDEX i.
      v3 = v1 + v2 + ov.   " a[i] <-- a[i] + b[i] + carry
      " Aufbereitungs-Variante mit div/mod:
      " => signifikant langsamer als if/sub!
      " -> Diese Operationen werden offenbar in Software durchgeführt.
      " -> Sie sind deutlich langsamer als elementare ABAP-Operationen.
      " ov = v3 div chunkmod.
      " v3 = v3 mod chunkmod.
      " Aufbereitungs-Variante mit Vergleich/sub:
      IF v3 GE chunkmod.
        ov = 1.            " carry <-- true
        v3 = v3 - chunkmod.
      ELSE.
        ov = 0.            " carry <-- false
      ENDIF.
      APPEND v3 TO result.
    ENDDO.

    IF ov NE 0.
      APPEND ov TO result.
    ENDIF.

    CLEAR dat.
    dat = result.

    this = me.
  ENDMETHOD.

  METHOD class_constructor.
    CREATE OBJECT: minus_one, zero, one, two, maxint, o_denom, o_nummer, o_rest, o_quotient.
    minus_one->set_int( -1 ).
    zero->set_int( 0 ).
    one->set_int( 1 ).
    two->set_int( 2 ).
    maxint->set_int( 2 ** 31 - 1 ).
  ENDMETHOD.

  METHOD cmp.
    " Hier wird eine sophistische Vorzeichenbehandlung angesetzt,
    " die auch vorzeichenbehaftete Nullen einbezieht.
    " Wenn eine negative Zahl zu Null verkleinert wurde - etwa durch Division -,
    " wird diese als kleiner als eine positive zu Null verkleinerte Zahl betrachtet.

    IF is_zero( ) AND x->is_zero( ).
      r = equal.
      RETURN.
    ENDIF.
    IF sign = positive AND x->sign = negative.
      r = larger.
      RETURN.
    ENDIF.
    IF sign = negative AND x->sign = positive.
      r = smaller.
      RETURN.
    ENDIF.

    r = abscmp( x ).
    IF sign = negative.
      CASE r.
        WHEN larger.
          r = smaller.
        WHEN smaller.
          r = larger.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD constructor.
    sign = positive.
    dat = VALUE #( ( fzero ) ).
  ENDMETHOD.

  METHOD debug.
    out->write( get_sign( ) ).
    LOOP AT dat INTO DATA(t).
      out->write( t ).
    ENDLOOP.
  ENDMETHOD.

  METHOD div.
    IF o_denom->cmp( me ) = equal AND o_nummer->cmp( x ) = equal.
      me->set( o_quotient ).
      this = me.
      RETURN.
    ENDIF.

    o_denom->set( me ).
    o_nummer->set( x ).
    o_rest = divx( x ).
    o_quotient->set( me ).

    this = me.
  ENDMETHOD.

*/*
* * DIVISION WITH REMAINDER
* * Please read the comments before the definition of
* * `BigUnsigned::divideWithRemainder' in `BigUnsigned.cc' for lots of
* * information you should know before reading this function.
* *
* * Following Knuth, I decree that x / y is to be
* * 0 if y==0 and floor(real-number x / y) if y!=0.
* * Then x % y shall be x - y*(integer x / y).
* *
* * Note that x = y * (x / y) + (x % y) always holds.
* * In addition, (x % y) is from 0 to y - 1 if y > 0,
* * and from -(|y| - 1) to 0 if y < 0.  (x % y) = x if y = 0.
* *
* * Examples: (q = a / b, r = a % b)
* * a   b   q   r
* * === === === ===
* * 4   3   1   1
* * -4  3 -2  2
* * 4 -3  -2  -2
* * -4  -3  1 -1
* */
*void BigInteger::divideWithRemainder(const BigInteger &b, BigInteger &q) {
*   // Defend against aliased calls;
*   // same idea as in BigUnsigned::divideWithRemainder .
*   if (this == &q)
*   throw "BigInteger::divideWithRemainder: Cannot write quotient and remainder into the same variable";
*   if (this == &b || &q == &b) {
*   BigInteger tmpB(b);
*   divideWithRemainder(tmpB, q);
*   return;
*   }
*
*   // Division by zero gives quotient 0 and remainder *this
*   if (b.sign == zero) {
*   q.mag = 0;
*   q.sign = zero;
*   return;
*   }
*   // 0 / b gives quotient 0 and remainder 0
*   if (sign == zero) {
*   q.mag = 0;
*   q.sign = zero;
*   return;
*   }
*
*   // Here *this != 0, b != 0.
*
*   // Do the operands have the same sign?
*   if (sign == b.sign) {
*   // Yes: easy case.  Quotient is zero or positive.
*   q.sign = positive;
*   } else {
*   // No: harder case.  Quotient is negative.
*   q.sign = negative;
*   // Decrease the magnitude of the dividend by one.
*   mag--;
*   /*
*    * We tinker with the dividend before and with the
*    * quotient and remainder after so that the result
*    * comes out right.  To see why it works, consider the following
*    * list of examples, where A is the magnitude-decreased
*    * a, Q and R are the results of BigUnsigned division
*    * with remainder on A and |b|, and q and r are the
*    * final results we want:
*    *
*    *  a A b Q R q r
*    *  -3  -2  3 0 2 -1  0
*    *  -4  -3  3 1 0 -2  2
*    *  -5  -4  3 1 1 -2  1
*    *  -6  -5  3 1 2 -2  0
*    *
*    * It appears that we need a total of 3 corrections:
*    * Decrease the magnitude of a to get A.  Increase the
*    * magnitude of Q to get q (and make it negative).
*    * Find r = (b - 1) - R and give it the desired sign.
*    */
*   }
*
*   // Divide the magnitudes.
*   mag.divideWithRemainder(b.mag, q.mag);
*
*   if (sign != b.sign) {
*   // More for the harder case (as described):
*   // Increase the magnitude of the quotient by one.
*   q.mag++;
*   // Modify the remainder.
*   mag.subtract(b.mag, mag);
*   mag--;
*   }
*
*   // Sign of the remainder is always the sign of the divisor b.
*   sign = b.sign;
*
*   // Set signs to zero as necessary.  (Thanks David Allen!)
*   if (mag.isZero())
*   sign = zero;
*   if (q.mag.isZero())
*   q.sign = zero;
*
*   // WHEW!!!
*}

  METHOD divx.
    DATA z TYPE tr_bigint.     " Zwischenergebnis eines Näherungsschritts
    DATA: mr TYPE tv_chunk,       " Mantisse Rest
          er TYPE tv_int,         " Exponent Rest
          mx TYPE tv_chunk,       " Mantisse Nenner (X)
          ex TYPE tv_int,         " Exponent Nenner (X)
          mv TYPE tv_chunk,       " Mantisse Verhältnis
          ev TYPE tv_int.     " Exponent Verhältnis
    "eva TYPE tv_int,     " Verhältnis-Exponent, der direkt als Nullen eingesetzt wird
    "evb TYPE tv_int.     " Verhältnis-Exponent, der über set_dec eingesetzt wird

    z = NEW #( ).
    r = clone( me ).
    me->set( zero ).

    x->to_float( IMPORTING mnt = mx
                           exp = ex ).

    WHILE r->abscmp( x ) NE smaller.

      r->to_float( IMPORTING mnt = mr
                             exp = er ).
      " Das im folgenden ermittelte Verhältnis ist eine Schätzung auf etwa 15 Stellen Genauigkeit.
      " Die kann etwas zu klein, aber auch etwas zu groß ausfallen.
      " Dies ist kein Problem: Der Rest darf zwischendurch ruhig das Vorzeichen wechseln,
      " er wird trotzdem immer in Richtung kleiner werdender Beträge abgebaut.
      " Wenn wir am Punkt angekommen sind, dass der Rest-Betrag kleiner als x geworden ist,
      " müssen wir nochmal testen, ob der Rest das selbe Vorzeichen hat wie zu Anfang
      " (was identisch ist zum Produkt der Vorzeichen von Quotient und Nenner).
      " Falls nicht, muss der Quotient um ein 1 und der Rest um x korrigiert werden.

      mv = mr / mx.
      ev = er - ex.
      z->set_float( src = mv
                    exp = ev ).
      me->add( z ).
      r->sub( z->mul( x ) ).

    ENDWHILE.

    " Rest-Underflow?
    " -> Korrektur des Quotienten um -1 relativ zu seinem Vorzeichen
    "     und des Rests um jene Quotientenkorrektur mal -x
    " (Um die Faktoren besser lesbar auseinanderzuhalten, wird das aktuelle Objekt,
    " welches den Quotienten darstellt, explizit mit "me->" gekennzeichnet.)

    IF xsdbool( r->sign EQ positive ) NE xsdbool( me->sign EQ x->sign ).  "IF r->sign NE me->sgn * x->sgn.
      IF me->sign EQ positive.
        z->set( one ).
      ELSE.
        z->set( minus_one ).
      ENDIF.
      me->sub( z ).
      r->add( z->mul( x ) ).
    ENDIF.

    CLEAR z.

  ENDMETHOD.

  METHOD number_of_digits.
    DATA(n) = lines( dat ).
    r = ( n - 1 ) * chunksize.
    READ TABLE dat INTO DATA(v) INDEX n.
    "r = r + ceil( log10( v + fhalf ) ).
    " Die Funktion log10 ist für decfloats EXTREM aufwendig implementiert
    " (rund 500 mikrosec -> das mehr als 1000-fache des Wertes für float).
    " div (und powmod als abhängige Funktion) werden dadurch extrem ausgebremst.
    r = r + cl_abap_math=>get_number_of_digits( floor( v + fhalf ) ).
  ENDMETHOD.

  METHOD to_float.
    DATA imin TYPE sy-tabix.

    mnt = 0.
    exp = 0.
    DATA(index) = lines( dat ).
    imin = nmax( val1 = 1
                 val2 = index - 2 ).
    WHILE index >= imin.
      READ TABLE dat INTO DATA(v) INDEX index.
      mnt = mnt * chunkmod + v.
      index = index - 1.
    ENDWHILE.
    IF imin > 1.
      exp = index * chunksize.
    ENDIF.
    IF sign EQ negative.
      mnt = - mnt.
    ENDIF.
  ENDMETHOD.

  METHOD to_dec. " to decimal type f
    DATA imin TYPE sy-tabix.

    mnt = 0.
    exp = 0.
    DATA(i) = lines( dat ).
    imin = nmax( val1 = 1
                 val2 = i - 1 ).
    WHILE i >= imin.
      READ TABLE dat INTO DATA(v) INDEX i.
      mnt = mnt * chunkmod + v.
      i = i - 1.
    ENDWHILE.
    IF imin > 1.
      exp = i * chunksize.
    ENDIF.
    IF sign EQ negative.
      mnt = - mnt.
    ENDIF.
  ENDMETHOD.

  METHOD get_sign.
    s = SWITCH #( sign WHEN positive THEN '+' ELSE '-' ).
  ENDMETHOD.

  METHOD to_str.
    DATA st TYPE string.

    DATA(n) = lines( dat ).
    LOOP AT dat INTO DATA(v).
      st = |{ v }|.
      "write: / ' to_str:',st.
      IF sy-tabix < n.
        s = strpad( s = st
                    n = chunksize
                    c = '0' ) && s.
      ELSE.
        s = st && s.
      ENDIF.
    ENDLOOP.

    IF with_sign = abap_true.
      s = get_sign( ) && s.
    ENDIF.
  ENDMETHOD.

  METHOD is_zero.
    zero = abap_false.
    IF lines( dat ) LE 1.
      READ TABLE dat INTO DATA(d) INDEX 1.
      zero = xsdbool( d LE 0 ).
    ENDIF.
  ENDMETHOD.

  METHOD mod.
    IF o_denom->cmp( me ) = equal AND o_nummer->cmp( x ) = equal.
      this = me->set( o_rest ).
      RETURN.
    ENDIF.

    o_denom->set( me ).
    o_nummer->set( x ).
    o_rest = divx( x ).
    o_quotient->set( me ).
    this = me->set( o_rest ).
  ENDMETHOD.

  METHOD inc. " Increment
    DATA: i      TYPE tv_int,
          v1     TYPE tv_chunk,
          v3     TYPE tv_chunk,
          ov     TYPE tv_chunk.  " Carry, overflow

    IF sign EQ negative.
      this = me->add( one ).
*   mag--;
*   if (mag == 0)
*     sign = zero;
    ELSE.
      " mag++;
        this = me.
        sign = positive. " if not already

        READ TABLE dat ASSIGNING FIELD-SYMBOL(<v1>) INDEX 1.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE lcx_negative_exponent. " ???
        ENDIF.

        v3 = <v1> + 1.

        " Aufbereitungs-Variante mit Vergleich/sub:
        IF v3 GE chunkmod.
          ov = 1.            " carry <-- true
          v3 = v3 - chunkmod.
          <v1> = v3.
        ELSE.
          <v1> = v3.         " carry <-- false

          sign = positive.   " if not already
          RETURN.
        ENDIF.

        DO lines( dat ) - 1 TIMES.
          i = sy-index + 1.

          READ TABLE dat ASSIGNING <v1> INDEX i.
          CHECK sy-subrc EQ 0.
          v3 = <v1> + ov.
          " Aufbereitungs-Variante mit Vergleich/sub:
          IF v3 GE chunkmod.
            ov = 1.            " carry <-- true
            <v1> = v3 - chunkmod.
          ELSE.
            ov = 0.            " carry <-- false
            <v1> = v3.
            EXIT.
          ENDIF.
        ENDDO.

        IF ov NE 0.
          APPEND ov TO dat.
        ENDIF.

      ENDIF.
    ENDMETHOD.

    METHOD dec. " Decrement
      this = me->sub( one ).

*      RAISE EXCEPTION TYPE lcx_negative_exponent. " not implemented yet
*      IF positive EQ abap_true.
**   mag--;
**   if (mag == 0)
**     sign = zero;
*      ELSE.
**   mag++;
**   sign = negative;
*      ENDIF.
    ENDMETHOD.

    METHOD sqroot.
      " Return a new bigint that is the square root. This truncates.
      RAISE EXCEPTION TYPE lcx_negative_exponent. " not implemented yet
    ENDMETHOD.

    METHOD div_by_2.
      RAISE EXCEPTION TYPE lcx_negative_exponent. " not implemented yet
    ENDMETHOD.

    METHOD root. " param n
      " Return a new bigint that is the nth root. This truncates.
      RAISE EXCEPTION TYPE lcx_negative_exponent. " not implemented yet
    ENDMETHOD.

    METHOD shiftLeft. " param n
      RAISE EXCEPTION TYPE lcx_negative_exponent. " not implemented yet
    ENDMETHOD.

    METHOD gcd. " param n
      " Return the greatest common divisor of the current bigint with n as a new bigint.
      RAISE EXCEPTION TYPE lcx_negative_exponent. " not implemented yet
    ENDMETHOD.

    METHOD mul.
      IF sign EQ x->sign.
        sign = positive.
      ELSE.
        sign = negative.
      ENDIF.

      IF is_zero( ) OR x->is_zero( ).
        dat = VALUE #( ( fzero ) ).
        this = me.
        RETURN.
      ENDIF.

      DATA: result TYPE tt_chunk,
            i1     TYPE tv_int,
            i2     TYPE tv_int,
            i3     TYPE tv_int,
            i1min  TYPE tv_int,
            i3max  TYPE tv_int,
            v1     TYPE tv_chunk,
            v2     TYPE tv_chunk,
            v3     TYPE tv_chunk,
            ov     TYPE tv_chunk,
            vr     TYPE tv_chunk.

      DATA(n1) = lines( dat ).
      DATA(n2) = lines( x->dat ).

      " Addition nach Zielstellen...
      " i3 gibt die Zielstelle an.
      " Es sind alle i1 + i2 = i3 zu durchlaufen!
      " Nullbasiert wäre das:
      "   i1 = i1min...i3   i1min = max( 0, i3 - n2 )
      "   i2 = i3 - i1
      " Das bei SAP 1-basiert indiziert wird,
      " muss an den richtigen Stellen noch +1 ergänzt werden!
      "
      " Es ist zu bedenken, dass die Menge der bearbeiteten chunks SEHR groß sein kann!
      " Das resultiert in großem Wertebereich für den Overflow!
      " Der ist halt nicht bloß eine eventuelle "1", sondern nimmt maximal den Wert
      " der Menge der beteiligten chunks an!
      " Wir haben bei den Zwischen-Additionen also große Zahlen
      " aus drei chunks zu verarbeiten.
      " Die chunksize ist so bemessen, dass das Ergebnis einer Multiplikation
      " plus eine Überlaufstelle im zugrundeliegenden chunk-Datentyp aufgenommen werden kann.
      i3max = n1 + n2 - 1.
      i3 = 1.
      DO i3max TIMES.

        i1min = nmax( val1 = 1 val2 = i3 - n2 + 1 ).
        i1 = i1min.
        i2 = i3 - i1 + 1.

        WHILE i1 <= n1 AND i2 > 0.
          " Da wir hier explizit nur Elemente durchgehen, die tatsächlich existieren,
          " werden alle read tables erfolgreich durchgehen.
          " Das Nullen der Zielwerte können wir uns deshalb ersparen.
          READ TABLE    dat INTO v1 INDEX i1.
          READ TABLE x->dat INTO v2 INDEX i2.

          "data s  type string.
          "s = `  1: ` && v1 && `[` && i1 && `]  2: ` && v2 && `[` && i2 && `]`.
          "write: / s.

          v3 = v3 + v1 * v2.

          "ov = ov + v3 div dchunkmod.
          "v3 =      v3 mod dchunkmod.

          IF v3 >= dchunkmod.
            ov = ov + 1.
            v3 = v3 - dchunkmod.
          ENDIF.

          i1 = i1 + 1.
          i2 = i2 - 1.

        ENDWHILE.

        "vr = v3 mod chunkmod.
        "v3 = v3 div chunkmod + ov * chunkmod.

        DATA vt TYPE tv_chunk.

        vt = v3 DIV chunkmod.
        vr = v3 - vt * chunkmod.
        v3 = vt + ov * chunkmod.

        ov = fzero.
        APPEND vr TO result.

        i3 = i3 + 1.
      ENDDO.
      " Im Ergebnis wird es nie mehr Stellen geben als in den letzten v3 gepasst haben.
      " v3 wird im letzten Schleifendurchlauf innerhalb chunkmod bleiben.

      IF v3 NE 0.
        APPEND v3 TO result.
      ENDIF.

      CLEAR dat.
      dat = result.

      this = me.
    ENDMETHOD.

    METHOD neg. " negate
      IF sign EQ negative.
        sign = positive.
      ELSE.
        sign = negative.
      ENDIF.
      this = me.
    ENDMETHOD.

    METHOD pow.
      IF x->cmp( zero ) = smaller.
        RAISE EXCEPTION TYPE lcx_negative_exponent.
      ENDIF.

      " Wir haben einen Ziel-Speicherbedarf von: Originalgröße * x.
      " Zwischendurch haben wir denselben Speicherbedarf für Zwischenwerte nochmal.
      " Wobei nicht dokumentiert ist, wieviele Elemente ein Array alias interne Tabelle
      " halten kann. Denkbar ist ohne weiteres, dass dies auf 64-bit-Systemen ausschließlich
      " durch die Adressierbarkeit der Array-Blöcke und deren Begrenzung durch den
      " Integer-Wertebereich der Indizes gegeben ist.
      " Außerdem ist nicht dokumentiert, wieviel RAM eine Session verwalten kann.
      " Ich setze hier also erstmal an, dass ich - falls nicht ein Speicherüberlauf
      " vom Betriebssystem kommt - bis zu maxint Blöcke adressieren können sollte.
      "
      " Letztlich können wir hier eh nur eine Abschätzung geben.
      " Ob und wann eine Exception kommt, entscheidet die Laufzeitumgebung und die
      " konkrete Füllung der Argumente.
      " => Die folgende sehr grobe Schätzung wurde wieder deaktiviert.
      "
      "if ( x->cmp( maxint_2 ) > 0 ).
      "  raise TOO_BIG.
      "endif.

      DATA: "cifcount   TYPE tv_int,
        exptmp TYPE tv_int,
        expmax TYPE tv_int,
        exp    TYPE tv_int,
        facs   TYPE tt_bigint,
        factmp TYPE tr_bigint.

      TRY.
          "------------------------
          " Potenz-Faktoren anlegen
          "------------------------
          factmp = clone( me ).

          exptmp = 1.
          exp = x->to_str( ).
          expmax = exp DIV 2.
          "write: / '  exp:',exp,', expmax:',expmax.

          APPEND clone( factmp ) TO facs.

          WHILE exptmp <= expmax.
            DATA(fac) = clone( factmp )->mul( factmp ).
            exptmp += exptmp.
            APPEND fac TO facs.
            factmp->set( fac ).

            "write: / '  faktor:', fac->to_str( ).

          ENDWHILE.

          "loop at facs into fac.
          "  write: / |  facs[{ sy-tabix }]:{ fac->to_str( ) }|.
          "endloop.

          "------------------------
          " Potenz bilden
          "------------------------
          factmp->set_int( 1 ).
          "exptmp wird aus der Init-Schleife übernommen
          DATA(ei) = lines( facs ).
          WHILE exp > 0.
            READ TABLE facs INTO fac INDEX ei.
            IF exptmp <= exp.
              factmp->mul( fac ).

              "write: / |  fac:{ fac->to_str( ) }, exptmp:{ exptmp }, exp:{ exp }|.

              exp -= exptmp.
            ENDIF.
            exptmp = exptmp DIV 2.
            ei = ei - 1.
          ENDWHILE.

          set( factmp ).
          free_bigint_table( facs ).
          CLEAR factmp.

        CATCH cx_root.
          IF factmp IS NOT INITIAL.
            free_bigint_table( facs ).
            CLEAR factmp.
          ENDIF.
          RAISE EXCEPTION TYPE lcx_too_big.
      ENDTRY.

      this = me.
    ENDMETHOD.

    METHOD powmod.
      " Restklassen-Rechnung erlaubt, den numerischen Aufwand
      " bei Multiplikation und Potenzierung krass zu reduzieren.
      "   .
      " - Multiplikation einer Zahl mit einem beliebigen Faktor modulo m:
      "       z mod m = (    x * m +     r) mod m =     r
      "   k * z mod m = (k * x * m + k * r) mod m = k * r
      "   .
      " - auch der Faktor kann modulo m reduziert werden:
      "       a = xa * m + ra
      "       b = xb * m + rb
      "   a * b mod m = (xa*xb * m^2 + (xa*rb + xb*ra) * m + ra*rb) mod m = ra*rb
      "   .
      " -> Da in einer Restklassenbetrachtung das k*x*m völlig wurscht ist,
      " kann man sich, solange man nur mit ganzen Zahlen k rechnet, das k*x*m klemmen.
      " In jedem Multiplikations-Rechenschritt. Indem man stets auf die Restklasse reduziert.
      " .
      " Das machen wir hier.
      " Dadurch fallen alle Beschränkungen des Exponenten.
      " Die Prozedur ist kopiert vom normalen pow.

      DATA(cmp) = x->cmp( zero ).
      CASE cmp.
        WHEN smaller.
          RAISE EXCEPTION TYPE lcx_negative_exponent.

      " Extrabehandlung des Falls x = 0 (passt nicht in die Logik weiter unten)
        WHEN equal.
            this = me->set( one ).
            RETURN.
      ENDCASE.

      DATA: "cifcount   TYPE tv_int,
        exptmp TYPE tr_bigint,
        facs   TYPE tt_bigint,
        exps   TYPE tt_bigint.

      TRY.
          "------------------------
          " Potenz-Faktoren anlegen
          "------------------------
          " Die Zweierpotenzreihe zu speichern ist effektiver als sie
          " beim Abwärtswandern der Faktorenliste auszudividieren.
          exptmp = clone( one ). " me = original me ^ 1
          o_rest = me->divx( m ).
          me->set( o_rest ). " Reduzierung auf Restklasse

          WHILE exptmp->cmp( x ) NE larger.
            APPEND clone( me ) TO facs.
            APPEND clone( exptmp ) TO exps.
            me->mul( me ).          " me = me^2 = original me ^ exp
            exptmp->add( exptmp ).  " exp = 2 ^ schleifenindex
            o_rest = me->divx( m ).
            me->set( o_rest ). " Reduzierung auf Restklasse
          ENDWHILE.

          " debugging...
          "ei = lines( facs ).
          "if ( ei > 0 ).
          "  write: / 'potenz-faktoren:'.
          "  while ( ei > 0 ).
          "    read table facs into fac index ei.
          "    read table exps into exp index ei.
          "    write: / |{ exp->to_str( ) } -> { fac->to_str( ) }|.
          "    ei = ei - 1.
          "  endwhile.
          "endif.

          "------------------------
          " Potenz bilden
          "------------------------
          me->set( one ).
          exptmp->set( x ).
          DATA(en) = lines( facs ).
          DATA(ei) = en.
          DO en TIMES.
            IF exptmp->cmp( zero ) NE larger.
              EXIT.
            ENDIF.
            READ TABLE exps INTO DATA(exp) INDEX ei.
            cmp = exp->cmp( exptmp ).
            IF cmp NE larger.
              READ TABLE facs INTO DATA(fac) INDEX ei.
              me->mul( fac ).
              exptmp->sub( exp ).
              o_rest = me->divx( m ).
              me->set( o_rest ). " Reduzierung auf Restklasse
              CHECK cmp = equal.
              EXIT.
            ENDIF.
            ei = ei - 1.
          ENDDO.

          free_bigint_table( facs ).
          free_bigint_table( exps ).
          CLEAR exptmp.

        CATCH cx_root.
          IF exptmp IS NOT INITIAL.
            CLEAR exptmp.
          ENDIF.
          free_bigint_table( facs ).
          free_bigint_table( exps ).
          RAISE EXCEPTION TYPE lcx_too_big.
      ENDTRY.

      this = me.
    ENDMETHOD.

    METHOD free_bigint_table.
      LOOP AT it_bigint INTO DATA(lr_bigit).
        CLEAR lr_bigit.
      ENDLOOP.
    ENDMETHOD.

    METHOD set_float.
      DATA: val  TYPE tv_real,
            rest TYPE tv_real,
            t    TYPE tv_real,
            xexp TYPE tv_int, " Zehnerpotenz des Quellwertes
            texp TYPE tv_int, " Zehnerpotenz-Übertrag in/aus Nullenkette
            mexp TYPE tv_int,
            nzch TYPE tv_int, " Anzahl der abschließend zu ergänzenden Null-Chunks
            izch TYPE tv_int.

      IF src GE 0.
        sign = positive.
        val =   src.
      ELSE.
        sign = negative.
        val = - src.
      ENDIF.

      " Der reinkommende float wird dezimal so zurechtgerückt,
      " dass er nach Möglichkeit gerade genau dfmantsize Ziffern enthält.
      " wobei die kleinste Ziffer den Stellenwert 1 haben soll.
      " Dazu wird die Zehnerpotenz korrigiert, indem überflüssige Zehnerpotenzen
      " nach "exp" verschoben werden bzw. fehlende aus diesem abgezogen.
      " Nebenbei wird dafür gesorgt, dass die in "exp" verbleibenden Zehnerpotenzen
      " ein ganzzahliges Vielfaches der chunksize sind, so dass diese Nullen
      " geradlinig schlicht als Null-Chunks angehängt werden können.
      "-----------------
      "xexp = ceil( log10( val ) ).
      " Die Funktion log10 ist für decfloats EXTREM aufwendig implementiert
      " (rund 500 mikrosec -> das mehr als 1000-fache des Wertes für float).
      " div (und powmod als abhängige Funktion) werden dadurch extrem ausgebremst.
      xexp = cl_abap_math=>get_number_of_digits( floor( val ) ).

      texp = xexp - dfmantsize. " durch 10 ** texp wird val nachher dividiert
      exp += texp.              " ...und diese Nullen später direkt angehängt
      IF exp < 0.
        texp -= exp.
        exp = 0.
      ENDIF.
      mexp = exp MOD chunksize.
      nzch = exp DIV chunksize.
      texp -= mexp.

      "write: / '  aus Mantisse entfernt:',texp,', Anzahl Nullchunks:',nzch.

      " Zehnerpotenz-Korrektur und Rundung (auf höchstens runter bis Stellenwert 1)
      val = round( val = val / ipow( base = 10 exp = texp )
                   dec = nmin( val1 = 0
                               val2 = - ( xexp - texp - dfmantsize ) ) ) .
      CLEAR dat.
      IF nzch > 0.
        APPEND fzero TO dat.
        izch = 1.
        nzch -= izch.
        WHILE nzch >= izch.
          APPEND LINES OF dat FROM 1 TO izch TO dat.
          nzch -= izch.
          izch += izch.
        ENDWHILE.
        IF nzch > 0.
          APPEND LINES OF dat FROM 1 TO nzch TO dat.
        ENDIF.
      ENDIF.

      WHILE val >= chunkmod.
        "rest = val mod chunkmod.
        "val  = val div chunkmod.
        t = val DIV chunkmod.
        rest = val - t * chunkmod.
        val = t.

        APPEND rest TO dat.
      ENDWHILE.
      APPEND val TO dat.

      this = me.
    ENDMETHOD.

    METHOD set_dec.
      DATA: val  TYPE f,
            rest TYPE f,
            xexp TYPE tv_int, " Zehnerpotenz des Quellwertes
            texp TYPE tv_int, " Zehnerpotenz-Übertrag in/aus Nullenkette
            mexp TYPE tv_int,
            nzch TYPE tv_int, " Anzahl der abschließend zu ergänzenden Null-Chunks
            izch TYPE tv_int.

      IF src GE 0.
        sign = positive.
        val =   src.
      ELSE.
        sign = negative.
        val = - src.
      ENDIF.

      " Der reinkommende float wird dezimal so zurechtgerückt,
      " dass er nach Möglichkeit gerade genau fmantsize Ziffern enthält.
      " wobei die kleinste Ziffer den Stellenwert 1 haben soll.
      " Dazu wird die Zehnerpotenz korrigiert, indem überflüssige Zehnerpotenzen
      " nach "exp" verschoben werden bzw. fehlende aus diesem abgezogen.
      " Nebenbei wird dafür gesorgt, dass die in "exp" verbleibenden Zehnerpotenzen
      " ein ganzzahliges Vielfaches der chunksize sind, so dass diese Nullen
      " geradlinig schlicht als Null-Chunks angehängt werden können.
      xexp = ceil( log10( val ) ).
      texp = xexp - fmantsize. " durch 10 ** texp wird val nachher dividiert
      exp += texp.        " ...und diese Nullen später direkt angehängt
      IF exp < 0.
        texp = texp - exp.
        exp = 0.
      ENDIF.
      mexp = exp MOD chunksize.
      nzch = exp DIV chunksize.
      texp = texp - mexp.

      "write: / '  aus Mantisse entfernt:',texp,', Anzahl Nullchunks:',nzch.

      " Zehnerpotenz-Korrektur und Rundung (auf höchstens runter bis Stellenwert 1)
      val = round( val = val / ipow( base = 10 exp = texp )
                   dec = nmin( val1 = 0
                               val2 = - ( xexp - texp - fmantsize ) ) ) .
      CLEAR dat.
      IF nzch > 0.
        APPEND fzero TO dat.
        izch = 1.
        nzch -=  izch.
        WHILE nzch >= izch.
          APPEND LINES OF dat FROM 1 TO izch TO dat.
          nzch -= izch.
          izch += izch.
        ENDWHILE.
        IF nzch > 0.
          APPEND LINES OF dat FROM 1 TO nzch TO dat.
        ENDIF.
      ENDIF.

      WHILE val >= chunkmod.
        rest = val MOD chunkmod.
        "val  = val div chunkmod.
        " Die Operation "div" ist EIGENTLICH das zu "mod" passende Komplement.
        " Sie liefert allerdings Nachkommastellen, wenn die Zahl ursprünglich Nachkommastellen enthält
        " (so, dass (x div y)*y+(x mod y) == x).
        " Eigentlich kommt val hier gerundet rein, aber beim Dividieren durch chunkmod
        " können Rundungsfehler auftreten.
        " mod arbeitet aber nur wie gebraucht, wenn eben KEINE Nachkommastellen vorhanden sind.
        " Deshalb wird hier die aufwendigere Form mit floor gewählt.
        val = floor( val / chunkmod ).
        APPEND rest TO dat.
      ENDWHILE.
      APPEND val TO dat.

      this = me.
    ENDMETHOD.

    METHOD set_int.
      CLEAR dat.

      DATA val TYPE tv_real.

      IF src GE 0.
        sign = positive.
        val = src.
      ELSE.
        sign = negative.
        val = - src.
      ENDIF.

      APPEND val TO dat.

      this = me.
    ENDMETHOD.

    METHOD set.
      dat = src->dat.
      sign = src->sign.
      this = me.
    ENDMETHOD.

    METHOD clone.
      this = NEW lcl_lisp_bigintx( )->set( src ).
    ENDMETHOD.

    METHOD set_str.
      CLEAR dat.

      DATA: "ofs   TYPE tv_int,
        mpart TYPE string,
        xpart TYPE string,
        ipart TYPE string,
        fpart TYPE string,
        chunk TYPE tv_chunk,
        ilen  TYPE tv_int,
        flen  TYPE tv_int,
        sc    TYPE c,
        x     TYPE tv_int.

      "write: / '  originale src:', src.

      " Entschrottung, Vereinfachung und Zerlegung
      "-----------------------
      CONDENSE src.
      TRANSLATE src TO LOWER CASE.
      " to do: Prüfunmit regex
      SPLIT src   AT 'e' INTO mpart xpart.
      SPLIT mpart AT '.' INTO ipart fpart.
      ilen = strlen( ipart ).
      flen = strlen( fpart ).

      "write: / '  parts:', ipart,',', fpart,' * 10^ ', xpart.

      " Vorzeichen-Erkennung
      "-----------------------
      sign = positive.
      IF ilen > 0.
        CASE ipart(1).
          WHEN '+'.
            ipart = ipart+1.
            ilen -= 1.

          WHEN '-'.
            sign = negative.
            ipart = ipart+1.
            ilen -= 1.
        ENDCASE.
      ENDIF.
      "write: / '  ipart nach Vorzeichenerkennung:', ipart.

      " Zehnerpotenz-Erkennung
      "-----------------------
      x = xpart.
      "write: / '  Zehnerpotenz / originale Länge: ',x,ilen.
      ilen += x.
      "write: / '  Zehnerpotenz / korrigierte Länge: ',ilen.
      IF x < 0.
        IF ilen < 1.
          ilen = 1.
          ipart = '0'.
        ENDIF.
        "write: / '  negative Zehnerpotenz -> verbleibende Länge: ',ilen.
        ipart = ipart(ilen).
      ELSEIF x > 0.
        IF x < flen.
          ipart &&= fpart(x).
        ELSE.
          x -= flen.
          ipart &&= fpart && repeat( val = '0'
                                     occ = x ).
        ENDIF.
      ENDIF.
      "write: / '  ipart nach Zehnerpotenz-Erkennung:', ipart.

      " Integer-Zerlegung
      "-----------------------
      DATA(size) = chunksize.
      WHILE ilen > 0.
        ilen -= chunksize.
        IF ilen < 0.
          size = ilen + chunksize.
          ilen = 0.
        ENDIF.
        chunk = ipart+ilen(size).
        APPEND chunk TO dat.
      ENDWHILE.

      this = me.

    ENDMETHOD.

    METHOD strpad.
      DATA d TYPE tv_int.
      d = n - strlen( s ).
      IF d < 0.
        r = s.
      ELSE.
        " repeat ist 10% schneller als das Ausschneiden aus einem vorbereiteten String...
        r = repeat( val = '0'
                    occ = d ) && s.
      ENDIF.
    ENDMETHOD.


    METHOD sub.
      " bei Unterschied im Vorzeichen auf Addition wechseln;
      " dazu zwischendurch das Vorzeichen des Subtrahenden wechseln...
      IF sign NE x->sign.
        x->neg( ).
        add( x ).
        x->neg( ).
        this = me.
        RETURN.
      ENDIF.

      " Den größeren Wert vom kleineren Wert abziehen
      " (Alternative wäre blind modulo zu subtrahieren und zum Schluss
      " bei verbleibendem Überlauf das Ergebnis nochmal komplett zu negieren.
      " Das ist allerdings mit mehr Aufwand verbunden als beim Tausch vorweg.)
      FIELD-SYMBOLS:
        <d1> TYPE tt_chunk,
        <d2> TYPE tt_chunk.

      " ...Dazu den Fall Null extra behandeln, weil der recht häufig vorkommen kann
      "   und besonders fix über die Bühne zu bringen ist...
      CASE abscmp( x ).
        WHEN equal.
          dat = VALUE #( ( fzero ) ).
          this = me.
          RETURN.

        WHEN smaller.
          ASSIGN dat    TO <d2>.
          ASSIGN x->dat TO <d1>.
          neg( ).

        WHEN larger.
          ASSIGN dat    TO <d1>.
          ASSIGN x->dat TO <d2>.
      ENDCASE..

      " Letztlich die eigentliche Subtraktion durchführen
      DATA: result TYPE tt_chunk,
            i      TYPE tv_int,
            v1     TYPE tv_chunk,
            v2     TYPE tv_chunk,
            v3     TYPE tv_chunk,
            ov     TYPE tv_chunk.

      " ov = 0. " überflüssig
      DO lines( <d1> ) TIMES.
        i = sy-index.
        " Das Lesen aus den Tabellen ist tolerant gegenüber Wertebereichsüberschreitung des Index.
        " Aber in so einem Fall wird das Datenziel nicht genullt.
        CLEAR: v1, v2.  " Nullsetzung auf kürzestmögliche Weise
        READ TABLE <d1> INTO v1 INDEX i.
        READ TABLE <d2> INTO v2 INDEX i.
        v3 = v1 - v2 + ov.

        " das folgende ist signifikant effektiver als ein if/sub
        "ov = v3 div chunkmod. " Überlauf ist hier -1
        "v3 = v3 mod chunkmod.
        IF v3 < 0.
          ov = -1.
          v3 = v3 + chunkmod.
        ELSE.
          ov = 0.
        ENDIF.

        APPEND v3 TO result.
      ENDDO.
      " ...Ein Überlauf nach Abschluss kann wegen vorherigem Sortieren nicht auftreten.

      " Führende Nullen löschen
      DATA(n) = lines( result ).
      DO n TIMES.
        i = n - sy-index + 1.
        READ TABLE result INTO v1 INDEX i.
        CHECK v1 <> fzero.
        EXIT.
      ENDDO.
      " -> alles von i bis n löschen! Aber nur, wenn i > 1 ist!
      DELETE result FROM i + 1." to n.

      CLEAR dat.
      dat = result.

      this = me.
    ENDMETHOD.

ENDCLASS.
