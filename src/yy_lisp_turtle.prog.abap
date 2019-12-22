*&---------------------------------------------------------------------*
*&  Include           YY_LISP_TURTLE
*&---------------------------------------------------------------------*

CLASS lcl_turtle_examples DEFINITION.
  PUBLIC SECTION.
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

  METHOD filled_square.
    turtle = lcl_turtle=>new( height = 800 width = 800 ).
    turtle->goto( x = 200 y = 200 ).

    turtle->set_pen( VALUE #(
            fill_color = `#FF0000`
            stroke_color = `#FF00FF`
            stroke_width = 2 ) ).

    DATA(start) = VALUE lcl_turtle=>t_point( x = 100 y = 100 ).
    DATA(side_length) = 100.

    DATA(points) = VALUE lcl_turtle=>t_points(
      ( start )
      ( x = start-x + side_length y = start-y )
      ( x = start-x + side_length y = start-y + side_length )
      ( x = start-x y = start-y + side_length )
    ).

    turtle->polyline( points ).
  ENDMETHOD.

  METHOD polygon_flower.
    turtle = lcl_turtle=>new( height = 800 width = 800 ).
    turtle->text( |Polygons:{ polygons } Sides: { polygon_sides }| ).

    turtle->goto( x = 200 y = 200 ).
    turtle->set_pen( VALUE #(
            stroke_color = `#FF00FF`
            stroke_width = 2 ) ).

    DATA(current_polygon) = 0.
    WHILE current_polygon < polygons.

      " draw a regular polygon
      DATA(current_polygon_side) = 0.
      DATA(side_length) = 50.
      WHILE current_polygon_side < polygon_sides.
        turtle->forward( side_length ).
        turtle->right( 360 / polygon_sides ).
        current_polygon_side = current_polygon_side + 1.
      ENDWHILE.

      " rotate before painting next polygon
      turtle->right( 360 / polygons ).

      current_polygon = current_polygon + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD polygon_using_lines.

    turtle = lcl_turtle=>new( height = 800 width = 800 ).
    turtle->goto( x = 200 y = 200 ).

    turtle->set_pen( VALUE #(
            stroke_color = `#FF00FF`
            stroke_width = 2 ) ).

    DATA(i) = 0.
    WHILE i < num_sides.
      turtle->forward( side_length ).
      turtle->right( 360 / num_sides ).

      i = i + 1.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_turtle_lsystem_examples DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS koch_curve.
    CLASS-METHODS pattern.
    CLASS-METHODS plant.
    CLASS-METHODS plant_2.
ENDCLASS.

CLASS lcl_turtle_lsystem_examples IMPLEMENTATION.

  METHOD koch_curve.
    DATA(turtle) = lcl_turtle=>new( height = 800 width = 600 ).
    turtle->goto( x = 200 y = 200 ).
    DATA(parameters) = VALUE lcl_turtle_lsystem=>params(
      initial_state = `F`
      move_distance = 10
      rotate_by = 90
      num_iterations = 3
      rewrite_rules = VALUE #(
        ( from = `F` to = `F+F-F-F+F` ) ) ).

    DATA(lsystem) = lcl_turtle_lsystem=>new(
      turtle = turtle
      parameters = parameters ).

    lsystem->execute( ).
    lsystem->show( ).

  ENDMETHOD.


  METHOD pattern.
    DATA(turtle) = lcl_turtle=>new( height = 800 width = 600 ).
    turtle->goto( x = 200 y = 200 ).

    DATA(parameters) = VALUE lcl_turtle_lsystem=>params(
      initial_state = `F-F-F-F`
      move_distance = 10
      rotate_by = 90
      num_iterations = 3
      rewrite_rules = VALUE #( ( from = `F`
                                 to = `FF-F+F-F-FF` ) ) ).

    DATA(lsystem) = lcl_turtle_lsystem=>new(
      turtle = turtle
      parameters = parameters ).

    lsystem->execute( ).
    lsystem->show( ).
  ENDMETHOD.

  METHOD plant.
    DATA(turtle) = lcl_turtle=>new( height = 800 width = 600 ).
    turtle->goto( x = 300 y = 600 ).
    turtle->set_angle( -90 ).

    DATA(parameters) = VALUE lcl_turtle_lsystem=>params(
      initial_state = `F`
      move_distance = 10
      rotate_by = 25
      num_iterations = 5
      rewrite_rules = VALUE #( ( from = `F`
                                 to = `F[+F]F[-F][F]` ) ) ).

    DATA(lsystem) = lcl_turtle_lsystem=>new( turtle = turtle
                                             parameters = parameters ).
    lsystem->execute( ).
    lsystem->show( ).
  ENDMETHOD.

  METHOD plant_2.
    DATA(turtle) = lcl_turtle=>new( height = 800 width = 600 ).
    turtle->goto( x = 300 y = 600 ).
    turtle->set_angle( -90 ).

    DATA(parameters) = VALUE lcl_turtle_lsystem=>params(
      initial_state = `F`
      move_distance = 10
      rotate_by = 21
      num_iterations = 4
      rewrite_rules = VALUE #( ( from = `F`
                                 to = `FF-[+F+F+F]+[-F-F+F]` ) ) ).

    DATA(lsystem) = lcl_turtle_lsystem=>new(
      turtle = turtle
      parameters = parameters ).

    lsystem->execute( ).
    lsystem->show( ).
  ENDMETHOD.

ENDCLASS.
