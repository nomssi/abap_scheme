*&---------------------------------------------------------------------*
*&  Include           YY_LISP_TURTLE
*&---------------------------------------------------------------------*
* Ported from https://github.com/FreHu/abap-turtle-graphics

CLASS lcl_turtle_examples DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create IMPORTING title TYPE string OPTIONAL
                         RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    CLASS-METHODS polygon_flower
      IMPORTING polygons      TYPE i
                polygon_sides TYPE i
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    CLASS-METHODS filled_square
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    CLASS-METHODS polygon_using_lines
      IMPORTING num_sides     TYPE i
                side_length   TYPE i
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.
ENDCLASS.

CLASS lcl_turtle_examples IMPLEMENTATION.

  METHOD create.
    turtle = lcl_turtle=>new( height = 800 width = 800 title = title ).
    turtle->goto( x = 200 y = 200 ).

    turtle->set_pen( VALUE #(
            fill_color = `#FF0000`
            stroke_color = `#FF00FF`
            stroke_width = 2 ) ).
  ENDMETHOD.

  METHOD filled_square.
    turtle = create( ).

    turtle->filled_square( side_length = 100
                           start = VALUE lcl_turtle=>t_point( x = 100 y = 100 ) ).
  ENDMETHOD.

  METHOD polygon_flower.
    turtle = create( title = |Polygons:{ polygons } Sides: { polygon_sides }| ).

    turtle->polygon_flower( number_of_polygons = polygons
                            polygon_sides = polygon_sides
                            side_length = 50 ).
  ENDMETHOD.

  METHOD polygon_using_lines.
    turtle = create( ).

    turtle->regular_polygon( num_sides = num_sides
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
    CLASS-METHODS execute IMPORTING turtle TYPE REF TO lcl_turtle
                                    parameters TYPE lcl_turtle_lsystem=>params.
ENDCLASS.

CLASS lcl_turtle_lsystem_examples IMPLEMENTATION.

  METHOD execute.
    DATA(lsystem) = lcl_turtle_lsystem=>new( turtle = turtle
                                             parameters = parameters ).
    lsystem->execute( ).
    lsystem->show( ).
  ENDMETHOD.

  METHOD koch_curve.
    DATA(turtle) = lcl_turtle=>new( height = 800 width = 600 title = |Koch curve| ).
    turtle->goto( x = 200 y = 200 ).
    DATA(parameters) = VALUE lcl_turtle_lsystem=>params(
      initial_state = `F`
      " Move distance 10, Rotate right by 90, Rotate left by 90
      instructions = VALUE #(
        ( symbol = 'F' kind = lcl_turtle_lsystem=>instruction_kind-forward amount = 10 )
        ( symbol = '+' kind = lcl_turtle_lsystem=>instruction_kind-right amount = 90 )
        ( symbol = '-' kind = lcl_turtle_lsystem=>instruction_kind-left amount = 90 ) )
      num_iterations = 3
      rewrite_rules = VALUE #( ( from = `F` to = `F+F-F-F+F` ) ) ).

     execute( turtle = turtle
              parameters = parameters ).
  ENDMETHOD.


  METHOD pattern.
    DATA(turtle) = lcl_turtle=>new( height = 800 width = 600 ).
    turtle->goto( x = 200 y = 200 ).

    DATA(parameters) = VALUE lcl_turtle_lsystem=>params(
      initial_state = `F-F-F-F`
      instructions = VALUE #(
        ( symbol = 'F' kind = lcl_turtle_lsystem=>instruction_kind-forward amount = 10 )
        ( symbol = '+' kind = lcl_turtle_lsystem=>instruction_kind-right amount = 90 )
        ( symbol = '-' kind = lcl_turtle_lsystem=>instruction_kind-left amount = 90 ) )
      num_iterations = 3
      rewrite_rules = VALUE #( ( from = `F` to = `FF-F+F-F-FF` ) ) ).

    execute( turtle = turtle
             parameters = parameters ).
  ENDMETHOD.

  METHOD plant.
    DATA(turtle) = lcl_turtle=>new( height = 800 width = 600 ).
    turtle->goto( x = 300 y = 600 ).
    turtle->set_angle( -90 ).

    DATA(parameters) = VALUE lcl_turtle_lsystem=>params(
      LET distance = 10
          rotation = 25 IN
      initial_state = `F`
      instructions = VALUE #(
        ( symbol = `F` kind = lcl_turtle_lsystem=>instruction_kind-forward amount = distance )
        ( symbol = `+` kind = lcl_turtle_lsystem=>instruction_kind-right amount = rotation )
        ( symbol = `-` kind = lcl_turtle_lsystem=>instruction_kind-left amount = rotation )
        ( symbol = `[` kind = lcl_turtle_lsystem=>instruction_kind-stack_push )
        ( symbol = `]` kind = lcl_turtle_lsystem=>instruction_kind-stack_pop ) )
      num_iterations = 5
      rewrite_rules = VALUE #( ( from = `F` to = `F[+F]F[-F][F]` ) ) ).

    execute( turtle = turtle
             parameters = parameters ).
  ENDMETHOD.

  METHOD plant_2.
    DATA(turtle) = lcl_turtle=>new( height = 800 width = 600 ).
    turtle->goto( x = 300 y = 600 ).
    turtle->set_angle( -90 ).

    DATA(parameters) = VALUE lcl_turtle_lsystem=>params(
      initial_state = `F`
      instructions = VALUE #(
        ( symbol = `F` kind = lcl_turtle_lsystem=>instruction_kind-forward amount = 10 )
        ( symbol = `+` kind = lcl_turtle_lsystem=>instruction_kind-right amount = 21 )
        ( symbol = `-` kind = lcl_turtle_lsystem=>instruction_kind-left amount = 21 )
        ( symbol = `[` kind = lcl_turtle_lsystem=>instruction_kind-stack_push )
        ( symbol = `]` kind = lcl_turtle_lsystem=>instruction_kind-stack_pop ) )
      num_iterations = 4
      rewrite_rules = VALUE #( ( from = `F` to = `FF-[+F+F+F]+[-F-F+F]` ) ) ).

    DATA(lsystem) = lcl_turtle_lsystem=>new( turtle = turtle
                                             parameters = parameters ).
    lsystem->execute( ).
    lsystem->show( ).
  ENDMETHOD.

ENDCLASS.
