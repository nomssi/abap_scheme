*&---------------------------------------------------------------------*
*&  Include           YY_LISP_TURTLE
*&---------------------------------------------------------------------*
* Ported from https://github.com/FreHu/abap-turtle-graphics

CLASS lcl_turtle_examples DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS polygon_flower
      IMPORTING polygons      TYPE tv_int
                polygon_sides TYPE tv_int
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    CLASS-METHODS filled_square
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    CLASS-METHODS polygon_using_lines
      IMPORTING num_sides     TYPE tv_int
                side_length   TYPE tv_int
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

  PRIVATE SECTION.
    CLASS-METHODS demo IMPORTING title TYPE string OPTIONAL
                       RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.
ENDCLASS.

CLASS lcl_turtle_examples IMPLEMENTATION.

  METHOD demo.
    turtle = lcl_turtle=>new( height = 800 width = 800 title = title ).
    turtle->goto( x = 200 y = 200 ).

    turtle->set_pen( VALUE #(
            fill_color = `#FF0000`
            stroke_color = `#FF00FF`
            stroke_width = 2 ) ).
  ENDMETHOD.

  METHOD filled_square.
    turtle = demo( )->filled_square( side_length = 100
                                     start = VALUE lcl_turtle=>t_point( x = 100 y = 100 ) ).
  ENDMETHOD.

  METHOD polygon_flower.
    turtle = demo( title = |Polygons:{ polygons } Sides: { polygon_sides }|
         )->polygon_flower( number_of_polygons = polygons
                            polygon_sides = polygon_sides
                            side_length = 50 ).
  ENDMETHOD.

  METHOD polygon_using_lines.
    turtle = demo( )->regular_polygon( num_sides = num_sides
                                       side_length = side_length ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_turtle_lsystem_examples DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS koch_curve.
    CLASS-METHODS pattern.
    CLASS-METHODS plant.
    CLASS-METHODS plant_2.
  PRIVATE SECTION.
    CLASS-METHODS execute IMPORTING title TYPE string OPTIONAL
                                    x     TYPE tv_int DEFAULT 200
                                    y     TYPE tv_int DEFAULT 200
                                    angle TYPE tv_real OPTIONAL
                                    parameters TYPE lcl_turtle_lsystem=>params.
ENDCLASS.

CLASS lcl_turtle_lsystem_examples IMPLEMENTATION.

  METHOD execute.
    DATA(turtle) = lcl_turtle=>new( height = 800 width = 600 title = title ).
    turtle->goto( x = x
                  y = y ).
    turtle->set_angle( angle ).

    DATA(lsystem) = lcl_turtle_lsystem=>new( turtle = turtle
                                             parameters = parameters ).
    lsystem->execute( ).
    lsystem->show( ).
  ENDMETHOD.

  METHOD koch_curve.
    execute( title = |Koch curve|
             parameters = lcl_turtle_lsystem=>koch_curve_params( ) ).
  ENDMETHOD.


  METHOD pattern.
    execute( parameters = lcl_turtle_lsystem=>pattern_params( ) ).
  ENDMETHOD.

  METHOD plant.
    execute( x = 300
             y = 600
             angle = -90
             parameters = lcl_turtle_lsystem=>plant_params( ) ).
  ENDMETHOD.

  METHOD plant_2.
    execute( x = 300
             y = 600
             angle = -90
             parameters = lcl_turtle_lsystem=>plant_2_params( ) ).
  ENDMETHOD.

ENDCLASS.
