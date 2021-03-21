*&---------------------------------------------------------------------*
*&  Include           YY_LIB_TURTLE
*&---------------------------------------------------------------------*
* Ported from https://github.com/FreHu/abap-turtle-graphics

"TYPES tv_real TYPE decfloat34.  " real data type

CLASS lcx_turtle_problem DEFINITION CREATE PRIVATE
  INHERITING FROM cx_no_check.

  PUBLIC SECTION.
    CLASS-METHODS raise IMPORTING text TYPE string.
    METHODS constructor IMPORTING text     TYPE string
                                  previous TYPE REF TO cx_root OPTIONAL.
  PRIVATE SECTION.
    DATA text TYPE string.
ENDCLASS.

CLASS lcx_turtle_problem IMPLEMENTATION.

  METHOD raise.
    RAISE EXCEPTION TYPE lcx_turtle_problem EXPORTING text = text.
  ENDMETHOD.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).
    me->text = text.
    me->previous = previous.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_turtle_math DEFINITION.
  PUBLIC SECTION.
    TYPES numbers_i TYPE STANDARD TABLE OF tv_int WITH DEFAULT KEY.

    CLASS-METHODS find_max_int
      IMPORTING numbers       TYPE numbers_i
      RETURNING VALUE(result) TYPE tv_int.
ENDCLASS.

CLASS lcl_turtle_math IMPLEMENTATION.

  METHOD find_max_int.
    DATA(max) = numbers[ 1 ].
    LOOP AT numbers ASSIGNING FIELD-SYMBOL(<num>) FROM 2.
      CHECK <num> > max.
      max = <num>.
    ENDLOOP.

    result = max.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_turtle_convert DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS degrees_to_radians
      IMPORTING degrees        TYPE tv_real
      RETURNING VALUE(radians) TYPE tv_real.

    CLASS-METHODS radians_to_degrees
      IMPORTING radians        TYPE tv_real
      RETURNING VALUE(degrees) TYPE tv_real.
ENDCLASS.

CLASS lcl_turtle_convert IMPLEMENTATION.

  METHOD degrees_to_radians.
    radians = degrees * c_pi / 180.
  ENDMETHOD.

  METHOD radians_to_degrees.
    degrees = radians * 180 / c_pi.
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

    TYPES: BEGIN OF t_pen,
             stroke_color TYPE rgb_hex_color,
             stroke_width TYPE i,
             fill_color   TYPE rgb_hex_color,
             is_up        TYPE abap_bool,
           END OF t_pen.

    CLASS-METHODS class_constructor.
    CLASS-METHODS get_random_color
      IMPORTING colors       TYPE rgb_hex_colors
      RETURNING VALUE(color) TYPE rgb_hex_color.

    CLASS-DATA default_color_scheme TYPE rgb_hex_colors.
    CLASS-DATA default_pen TYPE t_pen.
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

    default_pen = VALUE t_pen( stroke_width = 1
                               stroke_color = `#FF0000`
                               is_up = abap_false ).
  ENDMETHOD.

  METHOD get_random_color.
    DATA(random_index) = random->intinrange( low = 1 high = lines( colors ) ).
    color = colors[ random_index ].
  ENDMETHOD.
ENDCLASS.

CLASS lcl_turtle DEFINITION DEFERRED.

CLASS lcl_turtle_svg DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_point,
        x TYPE i,
        y TYPE i,
      END OF t_point,
      t_points TYPE STANDARD TABLE OF t_point WITH DEFAULT KEY.

    TYPES:
      BEGIN OF line_params,
        x_from TYPE i,
        y_from TYPE i,
        x_to   TYPE i,
        y_to   TYPE i,
      END OF line_params,


      BEGIN OF polygon_params,
        points TYPE t_points,
      END OF polygon_params,
      polyline_params TYPE polygon_params,

      BEGIN OF text_params,
        x    TYPE i,
        y    TYPE i,
        text TYPE string,
      END OF text_params,

      BEGIN OF circle_params,
        center_x TYPE i,
        center_y TYPE i,
        radius   TYPE i,
      END OF circle_params.

    CLASS-METHODS new IMPORTING turtle TYPE REF TO lcl_turtle
                      RETURNING VALUE(result) TYPE REF TO lcl_turtle_svg.

    METHODS: line IMPORTING params          TYPE line_params
                  RETURNING VALUE(svg_line) TYPE string,

      polygon IMPORTING params             TYPE polygon_params
              RETURNING VALUE(svg_polygon) TYPE string,

      polyline IMPORTING params              TYPE polyline_params
               RETURNING VALUE(svg_polyline) TYPE string,

      text IMPORTING params          TYPE text_params
           RETURNING VALUE(svg_text) TYPE string,

      circle IMPORTING params            TYPE circle_params
             RETURNING VALUE(svg_circle) TYPE string.

  PROTECTED SECTION.
    DATA turtle TYPE REF TO lcl_turtle.

    METHODS constructor IMPORTING turtle TYPE REF TO lcl_turtle.
ENDCLASS.

CLASS lcl_turtle DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF defaults,
        height TYPE tv_int VALUE 800,
        width  TYPE tv_int VALUE 600,
        title  TYPE string VALUE `abapTurtle`,
      END OF defaults.

    TYPES: t_point  TYPE lcl_turtle_svg=>t_point,
           t_points TYPE lcl_turtle_svg=>t_points.

    TYPES t_pen TYPE lcl_turtle_colors=>t_pen.
    TYPES rgb_hex_color TYPE lcl_turtle_colors=>rgb_hex_color.
    TYPES rgb_hex_colors TYPE lcl_turtle_colors=>rgb_hex_colors.

    TYPES:
      BEGIN OF turtle_position,
        x     TYPE tv_int,
        y     TYPE tv_int,
        angle TYPE tv_real,
      END OF turtle_position.

    TYPES multiple_turtles TYPE STANDARD TABLE OF REF TO lcl_turtle.

    CLASS-METHODS new
      IMPORTING height           TYPE tv_int DEFAULT defaults-height
                width            TYPE tv_int DEFAULT defaults-width
                background_color TYPE rgb_hex_color OPTIONAL
                title            TYPE string DEFAULT defaults-title
      RETURNING VALUE(turtle)    TYPE REF TO lcl_turtle.

    "! Creates a new turtle based on an existing instance. The position, angle and pen are preserved.
    "! Does not preserve content.
    METHODS clone RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    "! Merges drawings of multiple turtles into one.
    CLASS-METHODS compose
      IMPORTING turtles       TYPE multiple_turtles
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS constructor
      IMPORTING height           TYPE tv_int
                width            TYPE tv_int
                background_color TYPE rgb_hex_color OPTIONAL
                title            TYPE string.

    METHODS right
      IMPORTING degrees       TYPE tv_real
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS left IMPORTING degrees       TYPE tv_real
                 RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS set_pen IMPORTING pen           TYPE t_pen
                    RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS goto
      IMPORTING x             TYPE tv_int
                y             TYPE tv_int
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS set_angle IMPORTING angle TYPE tv_real.

    METHODS forward IMPORTING how_far       TYPE tv_int
                    RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS to_offset
      IMPORTING delta_x       TYPE numeric
                delta_y       TYPE numeric
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS back IMPORTING how_far       TYPE tv_int
                 RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS pen_up RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS pen_down RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS show
      IMPORTING size          TYPE string DEFAULT cl_abap_browser=>xlarge
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS polygon_flower IMPORTING number_of_polygons TYPE tv_int
                                     polygon_sides      TYPE tv_int
                                     side_length        TYPE tv_int
                           RETURNING VALUE(turtle)      TYPE REF TO lcl_turtle.

    METHODS filled_square IMPORTING side_length TYPE tv_int
                                    start TYPE t_point
                          RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS regular_polygon IMPORTING num_sides     TYPE tv_int
                                      side_length   TYPE tv_int
                            RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.

    METHODS download
      IMPORTING filename TYPE string DEFAULT `abap-turtle.html`.

    METHODS enable_random_colors.
    METHODS disable_random_colors.

    METHODS get_svg RETURNING VALUE(svg) TYPE string.
    METHODS append_svg IMPORTING svg_to_append TYPE string.

    METHODS:
      get_position RETURNING VALUE(result) TYPE turtle_position,
      set_position IMPORTING position TYPE turtle_position,
      set_color_scheme IMPORTING color_scheme TYPE rgb_hex_colors,
      set_width IMPORTING width TYPE tv_int,
      set_height IMPORTING height TYPE tv_int,
      set_svg IMPORTING svg TYPE string.

    DATA: title        TYPE string READ-ONLY,
          svg          TYPE string READ-ONLY,
          width        TYPE tv_int READ-ONLY,
          height       TYPE tv_int READ-ONLY,
          position     TYPE turtle_position READ-ONLY,
          pen          TYPE t_pen READ-ONLY,
          color_scheme TYPE rgb_hex_colors READ-ONLY,
          svg_builder  TYPE REF TO lcl_turtle_svg READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA use_random_colors TYPE abap_bool.

    METHODS get_html RETURNING VALUE(html) TYPE string.

    METHODS line
      IMPORTING x_from        TYPE tv_int
                y_from        TYPE tv_int
                x_to          TYPE tv_int
                y_to          TYPE tv_int
      RETURNING VALUE(turtle) TYPE REF TO lcl_turtle.
ENDCLASS.

CLASS lcl_turtle IMPLEMENTATION.

  METHOD back.
    right( degrees = 180 ).
    forward( how_far ).
    right( degrees = 180 ).
  ENDMETHOD.

  METHOD constructor.
    me->width = width.
    me->height = height.
    me->pen = lcl_turtle_colors=>default_pen.
    me->color_scheme = lcl_turtle_colors=>default_color_scheme.
    me->use_random_colors = abap_true.
    me->title = title.
    me->svg_builder = lcl_turtle_svg=>new( me ).

    IF background_color IS NOT INITIAL.
      me->set_pen( VALUE #( fill_color = background_color ) ).
      DATA(side_length) = 100.

      DATA(points) = VALUE t_points( ( x = 0         y = 0 )
                                     ( x = 0 + width y = 0 )
                                     ( x = 0 + width y = 0 + height )
                                     ( x = 0         y = 0 + height )  ).

      me->append_svg( me->svg_builder->polyline( VALUE #( points = points ) )  ).
    ENDIF.

    me->pen = VALUE #( stroke_width = 1
                       stroke_color = `#FF0000`
                       is_up = abap_false ).
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

  METHOD to_offset.
    DATA(old_position) = position.
    DATA(new_position) = VALUE turtle_position( x = round( val = old_position-x + delta_x dec = 0 )
                                                y = round( val = old_position-y + delta_y dec = 0 )
                                            angle = old_position-angle ).
    IF pen-is_up = abap_false.
      me->line( x_from = position-x
                y_from = position-y
                x_to = new_position-x
                y_to = new_position-y ).
    ENDIF.

    set_position( new_position ).

    turtle = me.
  ENDMETHOD.

  METHOD forward.
    DATA(angle) = CONV f( lcl_turtle_convert=>degrees_to_radians( position-angle ) ).

    turtle = to_offset( delta_x = how_far * cos( angle )
                        delta_y = how_far * sin( angle ) ).
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

  METHOD append_svg.
    svg = svg && svg_to_append.
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
    turtle = NEW lcl_turtle( width = width
                             height = height
                             background_color = background_color
                             title = title ).
  ENDMETHOD.

  METHOD clone.
    turtle = NEW #( width = width
                    height = height
                    title = title ).

    turtle->set_pen( pen ).
    turtle->set_color_scheme( color_scheme ).
    turtle->set_position( position ).
    turtle->set_angle( position-angle ).
  ENDMETHOD.

  METHOD compose.

    IF lines( turtles ) < 1.
      lcx_turtle_problem=>raise( `Not enough turtles to compose anything.` ).
    ENDIF.

    " start where the last one left off
    DATA(lo_turtle) = turtles[ lines( turtles ) ].
    turtle = lo_turtle->clone( ).

    " new image size is the largest of composed turtles
    DATA(new_width) = lcl_turtle_math=>find_max_int(
      VALUE #( FOR <x> IN turtles ( <x>->width ) ) ).

    DATA(new_height) = lcl_turtle_math=>find_max_int(
      VALUE #( FOR <x> IN turtles ( <x>->height ) ) ).

    turtle->set_height( new_height ).
    turtle->set_width( new_width ).

    DATA(composed_svg) = REDUCE string( INIT result = ``
        FOR <svg> IN VALUE stringtab( FOR <x> IN turtles ( <x>->svg ) )
          NEXT result = result && <svg> ).

    turtle->append_svg( composed_svg ).

  ENDMETHOD.

  METHOD pen_down.
    me->pen-is_up = abap_false.
    turtle = me.
  ENDMETHOD.

  METHOD pen_up.
    me->pen-is_up = abap_true.
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

  METHOD set_width.
    me->width = width.
  ENDMETHOD.

  METHOD set_height.
    me->height = height.
  ENDMETHOD.

  METHOD set_svg.
    me->svg = svg.
  ENDMETHOD.

  METHOD set_pen.
    me->pen = pen.
    turtle = me.
  ENDMETHOD.

  METHOD set_position.
    me->position = position.
  ENDMETHOD.

  METHOD show.
    cl_abap_browser=>show_html( size = size
                                html_string = get_html( ) ).
    turtle = me.
  ENDMETHOD.

  METHOD regular_polygon.
    DATA(angle) = CONV tv_real( 360 / num_sides ).
    DATA(n) = nmax( val1 = 0 val2 = num_sides ).
    DO n TIMES.
      forward( side_length ).
      right( angle ).
    ENDDO.

    turtle = me.
  ENDMETHOD.

  METHOD polygon_flower.
    DATA(current_polygon) = 0.
    WHILE current_polygon < number_of_polygons.

      regular_polygon( num_sides   = polygon_sides
                       side_length = side_length ).

      " rotate before painting next polygon
      right( 360 / number_of_polygons ).

      current_polygon = current_polygon + 1.
    ENDWHILE.

    turtle = me.
  ENDMETHOD.

  METHOD filled_square.
    DATA(points) = VALUE t_points( ( start )
                                   ( x = start-x + side_length y = start-y )
                                   ( x = start-x + side_length y = start-y + side_length )
                                   ( x = start-x y = start-y + side_length ) ).

    append_svg( svg_builder->polyline( VALUE #( points = points ) )  ).

    turtle = me.
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

    TYPES lsystem_instruction_kind TYPE string.

    CONSTANTS:
      BEGIN OF instruction_kind,
        "! Doesn't do anything. Can be used for helper symbols.
        noop       TYPE lsystem_instruction_kind VALUE `noop`,
        "! Go forward by 'amount' pixels
        forward    TYPE lsystem_instruction_kind VALUE `forwrad`,
        "! Go back by 'amount' pixels
        back       TYPE lsystem_instruction_kind VALUE `back`,
        "! Turn left by 'amount' degrees
        left       TYPE lsystem_instruction_kind VALUE `left`,
        "! Turn right by 'amount' degrees
        right      TYPE lsystem_instruction_kind VALUE `right`,
        "! Push position on the stack
        stack_push TYPE lsystem_instruction_kind VALUE `stack_push`,
        "! Pop position from the stack
        stack_pop  TYPE lsystem_instruction_kind VALUE `stack_pop`,
      END OF instruction_kind.

    TYPES:
      BEGIN OF lsystem_instruction,
        symbol TYPE char01,
        kind   TYPE lsystem_instruction_kind,
        "! Distance or angle (if the operation requires it)
        amount TYPE tv_int,
*        move_distance  TYPE tv_int,  "! For move instructions, how many pixels to move by
*        rotate_by      TYPE tv_real, "! For rotate instructions, how many degrees to rotate by
      END OF lsystem_instruction,
      lsystem_instructions TYPE HASHED TABLE OF lsystem_instruction WITH UNIQUE KEY symbol.

    TYPES:
      BEGIN OF params,
        "! Starting symbols
        initial_state  TYPE string,
        "! How many times the rewrite rules will be applied
        num_iterations TYPE i,
        instructions   TYPE lsystem_instructions,
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

    CLASS-METHODS koch_curve_params RETURNING VALUE(params) TYPE params.
    CLASS-METHODS pattern_params RETURNING VALUE(params) TYPE params.
    CLASS-METHODS plant_params RETURNING VALUE(params) TYPE params.
    CLASS-METHODS plant_2_params RETURNING VALUE(params) TYPE params.

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
      DATA(symbol) = final_value+index(1).  " char
      DATA(rule) = VALUE #( parameters-instructions[ symbol = symbol ] OPTIONAL ).
      CASE rule-kind.
        WHEN instruction_kind-noop.
          CONTINUE.
        WHEN instruction_kind-forward.      " WHEN `F` OR `G` OR `H`.
          turtle->forward( rule-amount ).   " turtle->forward( parameters-move_distance ).
        WHEN instruction_kind-back.
          turtle->back( rule-amount ).
        WHEN instruction_kind-left.                      " WHEN `+`.
          turtle->right( CONV tv_real( rule-amount ) ).  "   turtle->right( parameters-rotate_by ).
        WHEN instruction_kind-right.                     " WHEN `-`.
          turtle->left( CONV tv_real( rule-amount ) ).   "   turtle->left( parameters-rotate_by ).
        WHEN instruction_kind-stack_push.                " WHEN `[`.
          push_stack( turtle->position ).                "   push_stack( turtle->position ).
        WHEN instruction_kind-stack_pop.                 " WHEN `]`.
          DATA(position) = pop_stack( ).
          turtle->goto( x = position-x y = position-y ).
          turtle->set_angle( position-angle ).
        WHEN OTHERS.
          lcx_turtle_problem=>raise( |Lsystem - unconfigured symbol { symbol }.| ).
      ENDCASE.

      index = index + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD get_final_value.
    DATA(instructions) = parameters-initial_state.
    DO parameters-num_iterations TIMES.
      LOOP AT parameters-rewrite_rules ASSIGNING FIELD-SYMBOL(<rule>).
        REPLACE ALL OCCURRENCES OF <rule>-from IN instructions WITH <rule>-to.
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

  METHOD koch_curve_params.
    params = VALUE #(
      initial_state = `F`
      " Move distance 10, Rotate right by 90, Rotate left by 90
      instructions = VALUE #(
        ( symbol = 'F' kind = instruction_kind-forward amount = 10 )
        ( symbol = '+' kind = instruction_kind-right amount = 90 )
        ( symbol = '-' kind = instruction_kind-left amount = 90 ) )
      num_iterations = 3
      rewrite_rules = VALUE #( ( from = `F` to = `F+F-F-F+F` ) ) ).

  ENDMETHOD.

  METHOD pattern_params.
    params = VALUE #( initial_state = `F-F-F-F`
      instructions = VALUE #(
        ( symbol = 'F' kind = instruction_kind-forward amount = 10 )
        ( symbol = '+' kind = instruction_kind-right amount = 90 )
        ( symbol = '-' kind = instruction_kind-left amount = 90 ) )
      num_iterations = 3
      rewrite_rules = VALUE #( ( from = `F` to = `FF-F+F-F-FF` ) ) ).
  ENDMETHOD.

  METHOD plant_params.
    params = VALUE #( LET distance = 10 rotation = 25 IN
      initial_state = `F`
      instructions = VALUE #(
        ( symbol = `F` kind = instruction_kind-forward amount = distance )
        ( symbol = `+` kind = instruction_kind-right amount = rotation )
        ( symbol = `-` kind = instruction_kind-left amount = rotation )
        ( symbol = `[` kind = instruction_kind-stack_push )
        ( symbol = `]` kind = instruction_kind-stack_pop ) )
      num_iterations = 5
      rewrite_rules = VALUE #( ( from = `F` to = `F[+F]F[-F][F]` ) ) ).
  ENDMETHOD.

  METHOD plant_2_params.
    params = VALUE #( initial_state = `F`
      instructions = VALUE #(
        ( symbol = `F` kind = instruction_kind-forward amount = 10 )
        ( symbol = `+` kind = instruction_kind-right amount = 21 )
        ( symbol = `-` kind = instruction_kind-left amount = 21 )
        ( symbol = `[` kind = instruction_kind-stack_push )
        ( symbol = `]` kind = instruction_kind-stack_pop ) )
      num_iterations = 4
      rewrite_rules = VALUE #( ( from = `F` to = `FF-[+F+F+F]+[-F-F+F]` ) ) ).
  ENDMETHOD.


ENDCLASS.

CLASS lcl_turtle_svg IMPLEMENTATION.

  METHOD circle.
    svg_circle = |<circle cx="{ params-center_x }" cy="{ params-center_y }" r="{ params-radius }" |
        && |stroke="{ turtle->pen-stroke_color }" |
        && |stroke-width="{ turtle->pen-stroke_width }" fill="{ turtle->pen-fill_color }"/>|.
  ENDMETHOD.

  METHOD line.
    svg_line = |<line x1="{ params-x_from }" y1="{ params-y_from }" x2="{ params-x_to }" y2="{ params-y_to }" |
      && |stroke="{ turtle->pen-stroke_color }" stroke-width="{ turtle->pen-stroke_width }"/>|.
  ENDMETHOD.

  METHOD constructor.
    me->turtle = turtle.
  ENDMETHOD.

  METHOD new.
    result = NEW #( turtle ).
  ENDMETHOD.

  METHOD polygon.
    DATA(point_data) = REDUCE string(
      INIT res = ||
      FOR point IN params-points
      NEXT res = res && |{ point-x },{ point-y } | ).

    svg_polygon = |<polygon points="{ point_data }" |
      && | stroke="{ turtle->pen-stroke_color }"|
      && | stroke-width="{ turtle->pen-stroke_width }" fill="{ turtle->pen-fill_color }" />|.
  ENDMETHOD.

  METHOD polyline.
    DATA(point_data) = REDUCE string(
      INIT res = ||
      FOR point IN params-points
      NEXT res = res && |{ point-x },{ point-y } | ).

    svg_polyline = |<polyline points="{ point_data }" |
      && |stroke="{ turtle->pen-stroke_color }" |
      && |stroke-width="{ turtle->pen-stroke_width }" fill="{ turtle->pen-fill_color }" />|.
  ENDMETHOD.

  METHOD text.
    svg_text = |<text x="{ params-x }" y="{ params-y }">{ params-text }</text>|.
  ENDMETHOD.
ENDCLASS.
