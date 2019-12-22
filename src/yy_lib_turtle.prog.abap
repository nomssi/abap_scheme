*&---------------------------------------------------------------------*
*&  Include           YY_LIB_TURTLE
*&---------------------------------------------------------------------*

"TYPES tv_real TYPE decfloat34.  " real data type

CLASS lcl_turtle_convert DEFINITION.
  PUBLIC SECTION.
    CONSTANTS pi TYPE tv_real
      VALUE '3.1415926535897932384626433832795'.

    CLASS-METHODS degrees_to_radians
      IMPORTING degrees        TYPE tv_real
      RETURNING VALUE(radians) TYPE tv_real.

    CLASS-METHODS radians_to_degrees
      IMPORTING radians        TYPE tv_real
      RETURNING VALUE(degrees) TYPE tv_real.
ENDCLASS.

CLASS lcl_turtle_convert IMPLEMENTATION.

  METHOD degrees_to_radians.
    radians = degrees * pi / 180.
  ENDMETHOD.

  METHOD radians_to_degrees.
    degrees = radians * 180 / pi.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_number_range DEFINITION.
  PUBLIC SECTION.
    TYPES number_range TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    "! Returns the list of numbers &lt;min, max).
    "! This method repeats the mistake of Python 2.x and will consume a lot of memory if used with large ranges
    CLASS-METHODS get
      IMPORTING min           TYPE i
                max           TYPE i
      RETURNING VALUE(result) TYPE number_range.
ENDCLASS.

CLASS lcl_number_range IMPLEMENTATION.

  METHOD get.
    DATA(i) = min.
    WHILE i < max.
      APPEND i TO result.
      i = i + 1.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_turtle_colors DEFINITION.
  PUBLIC SECTION.
    TYPES: rgb_hex_color  TYPE string,
           rgb_hex_colors TYPE STANDARD TABLE OF rgb_hex_color WITH EMPTY KEY.

    CLASS-METHODS class_constructor.
    CLASS-METHODS get_random_color
      IMPORTING colors       TYPE rgb_hex_colors
      RETURNING VALUE(color) TYPE rgb_hex_color.

    CLASS-DATA default_color_scheme TYPE rgb_hex_colors.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA random TYPE REF TO cl_abap_random.
ENDCLASS.


CLASS lcl_turtle_colors IMPLEMENTATION.

  METHOD class_constructor.
    default_color_scheme = VALUE #(
      ( `#8a295c` )
      ( `#5bbc6d` )
      ( `#cb72d3` )
      ( `#a8b03f` )
      ( `#6973d8` )
      ( `#c38138` )
      ( `#543788` )
      ( `#768a3c` )
      ( `#ac4595` )
      ( `#47bf9c` )
      ( `#db6697` )
      ( `#5f8dd3` )
      ( `#b64e37` )
      ( `#c287d1` )
      ( `#ba4758` )  ).

    random = cl_abap_random=>create( seed = 42 ).
  ENDMETHOD.

  METHOD get_random_color.
    DATA(random_index) = random->intinrange( low = 1 high = lines( colors ) ).
    color = colors[ random_index ].
  ENDMETHOD.
ENDCLASS.

CLASS lcl_turtle DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_pen,
        stroke_color TYPE lcl_turtle_colors=>rgb_hex_color,
        stroke_width TYPE i,
        fill_color   TYPE lcl_turtle_colors=>rgb_hex_color,
        is_up        TYPE abap_bool,
      END OF t_pen.

    TYPES:
      BEGIN OF t_point,
        x TYPE i,
        y TYPE i,
      END OF t_point,
      t_points TYPE STANDARD TABLE OF t_point WITH DEFAULT KEY.

    TYPES:
      BEGIN OF turtle_position,
        x     TYPE i,
        y     TYPE i,
        angle TYPE tv_real,
      END OF turtle_position.

    CLASS-METHODS new
      IMPORTING height        TYPE i
                width         TYPE i
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS constructor
      IMPORTING height TYPE i
                width  TYPE i.

    METHODS right
      IMPORTING degrees       TYPE tv_real
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS left
      IMPORTING degrees       TYPE tv_real
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS set_pen
      IMPORTING pen           TYPE t_pen
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS goto
      IMPORTING x             TYPE i
                y             TYPE i
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS set_angle
      IMPORTING angle TYPE tv_real.

    METHODS forward
      IMPORTING how_far       TYPE i
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS back
      IMPORTING how_far       TYPE i
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS pen_up
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS pen_down
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS line
      IMPORTING x_from        TYPE i
                y_from        TYPE i
                x_to          TYPE i
                y_to          TYPE i
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS circle
      IMPORTING center_x      TYPE i
                center_y      TYPE i
                radius        TYPE i
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS polygon
      IMPORTING points        TYPE t_points
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS polyline
      IMPORTING points        TYPE t_points
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS text
      IMPORTING text TYPE string.

    METHODS show
      IMPORTING size          TYPE string DEFAULT cl_abap_browser=>xlarge
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS download
      IMPORTING filename TYPE string DEFAULT `abap-turtle.html`.

    METHODS enable_random_colors.
    METHODS disable_random_colors.

    METHODS get_svg RETURNING VALUE(svg) TYPE string.
    METHODS:
      get_position RETURNING VALUE(result) TYPE turtle_position,
      set_position IMPORTING position TYPE turtle_position,
      set_color_scheme IMPORTING color_scheme TYPE lcl_turtle_colors=>rgb_hex_colors.

    DATA: svg          TYPE string READ-ONLY,
          width        TYPE i READ-ONLY,
          height       TYPE i READ-ONLY,
          position     TYPE turtle_position READ-ONLY,
          pen          TYPE t_pen READ-ONLY,
          color_scheme TYPE lcl_turtle_colors=>rgb_hex_colors READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA use_random_colors TYPE abap_bool.
    METHODS get_html
      RETURNING VALUE(html) TYPE string.

ENDCLASS.

CLASS lcl_turtle IMPLEMENTATION.

  METHOD back.
    right( degrees = 180 ).
    forward( how_far ).
    right( degrees = 180 ).
  ENDMETHOD.


  METHOD circle.
    svg = svg && |<circle cx="{ center_x }" cy="{ center_y }" r="{ radius }" |
        && |stroke="{ pen-stroke_color }" stroke-width="{ pen-stroke_width }" fill="{ pen-fill_color }"/>|.
    turtle = me.
  ENDMETHOD.


  METHOD constructor.
    me->width = width.
    me->height = height.
    me->pen = VALUE #(
     stroke_width = 1
     stroke_color = `#FF0000`
     is_up = abap_false ).
    me->color_scheme = lcl_turtle_colors=>default_color_scheme.
    me->use_random_colors = abap_true.
  ENDMETHOD.

  METHOD disable_random_colors.
    me->use_random_colors = abap_false.
  ENDMETHOD.


  METHOD download.

    DATA(file_name) = filename.
    DATA(path) = ``.
    DATA(full_path) = ``.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension = `html`
        default_file_name = filename
        initial_directory = ``
      CHANGING
        filename = file_name
        path = path
        fullpath = full_path
      EXCEPTIONS
        OTHERS = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SPLIT me->get_html( ) AT |\r\n| INTO TABLE DATA(lines).
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename = file_name
      CHANGING
        data_tab = lines
      EXCEPTIONS OTHERS = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD enable_random_colors.
    me->use_random_colors = abap_true.
  ENDMETHOD.


  METHOD forward.

    DATA(old_position) = position.
    DATA(new_x) = how_far * cos( CONV f( lcl_turtle_convert=>degrees_to_radians( old_position-angle ) ) ).
    DATA(new_y) = how_far * sin( CONV f( lcl_turtle_convert=>degrees_to_radians( old_position-angle ) ) ).

    DATA(new_position) = VALUE turtle_position(
      x = old_position-x + new_x
      y = old_position-y + new_y
      angle = old_position-angle ).

    IF pen-is_up = abap_false.
      me->line(
        x_from = old_position-x
        y_from = old_position-y
        x_to = new_position-x
        y_to = new_position-y ).
    ENDIF.

    me->set_position( new_position ).

    turtle = me.
  ENDMETHOD.

  METHOD get_html.
    html = |<html><body><h1>abapTurtle</h1><svg width="{ width }" height="{ height }">{ svg }</svg></body></html>|.
  ENDMETHOD.

  METHOD get_position.
    result = me->position.
  ENDMETHOD.

  METHOD get_svg.
    svg = me->svg.
  ENDMETHOD.

  METHOD goto.
    position-x = x.
    position-y = y.
    turtle = me.
  ENDMETHOD.

  METHOD left.
    position-angle = position-angle - degrees.
    position-angle = position-angle MOD 360.
    turtle = me.
  ENDMETHOD.

  METHOD line.

    IF use_random_colors = abap_true.
      pen-stroke_color = lcl_turtle_colors=>get_random_color( me->color_scheme ).
    ENDIF.

    svg = svg && |<line x1="{ x_from }" y1="{ y_from }" x2="{ x_to }" y2="{ y_to }"|
        && |stroke="{ pen-stroke_color }" stroke-width="{ pen-stroke_width }"/>|.

    turtle = me.
  ENDMETHOD.

  METHOD new.
    turtle = NEW lcl_turtle( width = width height = height ).
  ENDMETHOD.

  METHOD pen_down.
    me->pen-is_up = abap_false.
    turtle = me.
  ENDMETHOD.

  METHOD pen_up.
    me->pen-is_up = abap_true.
    turtle = me.
  ENDMETHOD.

  METHOD polygon.
    DATA(point_data) = REDUCE string(
    INIT res = ||
    FOR point IN points
    NEXT res = res && |{ point-x },{ point-y } | ).

    svg = svg && |<polygon points="{ point_data }"|
                  && | stroke="{ pen-stroke_color }" stroke-width="{ pen-stroke_width }" fill="{ pen-fill_color }" />|.

    turtle = me.
  ENDMETHOD.

  METHOD polyline.
    DATA(point_data) = REDUCE string(
      INIT res = ||
      FOR point IN points
      NEXT res = res && |{ point-x },{ point-y } | ).

    svg = svg && |<polyline points="{ point_data }"|
                  && | stroke="{ pen-stroke_color }" stroke-width="{ pen-stroke_width }" fill="{ pen-fill_color }" />|.

    turtle = me.
  ENDMETHOD.

  METHOD right.
    position-angle = position-angle + degrees.
    position-angle = position-angle MOD 360.
    turtle = me.
  ENDMETHOD.

  METHOD set_angle.
    me->position-angle = angle.
  ENDMETHOD.

  METHOD set_color_scheme.
    me->color_scheme = color_scheme.
  ENDMETHOD.

  METHOD set_pen.
    me->pen = pen.
    turtle = me.
  ENDMETHOD.

  METHOD set_position.
    me->position = position.
  ENDMETHOD.

  METHOD show.
    cl_abap_browser=>show_html(
      size = size
      html_string = get_html( ) ).

    turtle = me.
  ENDMETHOD.

  METHOD text.
    me->svg = svg && |<text x="{ position-x }" y="{ position-y }">{ text }</text>|.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_turtle_lsystem DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF lsystem_rewrite_rule,
        from TYPE string,
        to   TYPE string,
      END OF lsystem_rewrite_rule,
      lsystem_rewrite_rules TYPE STANDARD TABLE OF lsystem_rewrite_rule WITH DEFAULT KEY.

    TYPES:
      BEGIN OF params,
        "! Starting symbols
        initial_state  TYPE string,
        "! How many times the rewrite rules will be applied
        num_iterations TYPE i,
        "! For move instructions, how many pixels to move by
        move_distance  TYPE i,
        "! For rotate instructions, how many degrees to rotate by
        rotate_by      TYPE tv_real,
        "! A list of rewrite patterns which will be applied each iteration in order.
        "! E.g. initial state F with rule F -> FG and 3 iterations
        "! will produce FG, FGG, FGGG in each iteration respectively.
        "! Currently allows up to 3 variables F,G,H
        rewrite_rules  TYPE lsystem_rewrite_rules,
      END OF params.

    CLASS-METHODS new
      IMPORTING turtle        TYPE REF TO lcl_turtle
                parameters    TYPE params
      RETURNING VALUE(result) TYPE REF TO lcl_turtle_lsystem.

    METHODS execute.
    METHODS show IMPORTING size TYPE string DEFAULT cl_abap_browser=>large.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_final_value
      RETURNING VALUE(result) TYPE string.

    TYPES t_position_stack TYPE STANDARD TABLE OF lcl_turtle=>turtle_position WITH EMPTY KEY.
    METHODS:
      push_stack IMPORTING position TYPE lcl_turtle=>turtle_position,
      pop_stack RETURNING VALUE(position) TYPE lcl_turtle=>turtle_position.

    DATA turtle TYPE REF TO lcl_turtle.
    DATA parameters TYPE params.
    DATA position_stack TYPE t_position_stack.
ENDCLASS.

CLASS lcl_turtle_lsystem IMPLEMENTATION.

  METHOD execute.
    DATA(final_value) = get_final_value( ).

    DATA(index) = 0.
    WHILE index < strlen( final_value ).
      DATA(char) = final_value+index(1).

      CASE char.
        WHEN `F` OR `G` OR `H`.
          turtle->forward( parameters-move_distance ).
        WHEN `+`.
          turtle->right( parameters-rotate_by ).
        WHEN `-`.
          turtle->left( parameters-rotate_by ).
        WHEN `[`.
          push_stack( turtle->position ).
        WHEN `]`.
          DATA(position) = pop_stack( ).
          turtle->goto( x = position-x y = position-y ).
          turtle->set_angle( position-angle ).
      ENDCASE.

      index = index + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD get_final_value.
    DATA(instructions) = parameters-initial_state.
    DO parameters-num_iterations TIMES.
      LOOP AT parameters-rewrite_rules ASSIGNING FIELD-SYMBOL(<rule>).
        REPLACE ALL OCCURRENCES OF <rule>-from IN instructions
          WITH <rule>-to.
      ENDLOOP.
    ENDDO.

    result = instructions.
  ENDMETHOD.

  METHOD new.
    result = NEW #( ).
    result->turtle = turtle.
    result->parameters = parameters.
  ENDMETHOD.

  METHOD pop_stack.
    position = position_stack[ lines( position_stack ) ].
    DELETE position_stack INDEX lines( position_stack ).
  ENDMETHOD.

  METHOD push_stack.
    APPEND position TO position_stack.
  ENDMETHOD.

  METHOD show.
    turtle->show( size ).
  ENDMETHOD.
ENDCLASS.
