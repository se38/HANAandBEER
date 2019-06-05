*&---------------------------------------------------------------------*
*& Report YSTSTF00
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_obdb_next_alt.

TABLES addr1_data.
DATA : container    TYPE REF TO cl_gui_custom_container,
       html_control TYPE REF TO cl_gui_html_viewer.

CLASS lcl_app DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS main.
    METHODS status_9000.
    METHODS user_command_9000.
    METHODS exit_command_9000.

  PRIVATE SECTION.

    TYPES ty_distance TYPE p LENGTH 13 DECIMALS 5.

    TYPES: BEGIN OF ty_coordinate,
             name      TYPE string,
             address   TYPE string,
             city      TYPE string,
             country   TYPE string,
             longitude TYPE p LENGTH 16 DECIMALS 13,
             latitude  TYPE p LENGTH 16 DECIMALS 13,
             distance  TYPE ty_distance,
           END OF ty_coordinate.

    TYPES tt_brewery_ids TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    TYPES tt_html_table TYPE STANDARD TABLE OF char1024 WITH EMPTY KEY.

    DATA sql TYPE REF TO cl_sql_statement.

    METHODS string_to_table
      IMPORTING i_html_string   TYPE string
      RETURNING VALUE(r_result) TYPE tt_html_table.
    METHODS display_breweries
      IMPORTING i_city TYPE ad_city1.
    METHODS get_geojson
      IMPORTING
        i_city          TYPE ty_coordinate
        i_brewery       TYPE ty_coordinate
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS get_city_coordinate
      IMPORTING
        i_city          TYPE ad_city1
      RETURNING
        VALUE(r_result) TYPE ty_coordinate.
    METHODS get_nearest_brewery
      IMPORTING
        i_city_coordinate TYPE ty_coordinate
      RETURNING
        VALUE(r_result)   TYPE ty_coordinate.
    METHODS get_brewery_ids
      RETURNING
        VALUE(r_result) TYPE tt_brewery_ids.
    METHODS get_brewery_coordinate
      IMPORTING
        i_brewery_id    TYPE REF TO i
      RETURNING
        VALUE(r_result) TYPE ty_coordinate.
    METHODS get_distance
      IMPORTING
        i_city          TYPE ty_coordinate
        i_brewery       TYPE ty_coordinate
      RETURNING
        VALUE(r_result) TYPE ty_distance.

ENDCLASS.

DATA(app) = NEW lcl_app( ).
app->main( ).

CLASS lcl_app IMPLEMENTATION.

  METHOD main.

    CALL SCREEN 9000.

  ENDMETHOD.


  METHOD status_9000.

    IF sy-pfkey <> '9000'.
      SET PF-STATUS '9000'.
      SET TITLEBAR '9000'.

      container = NEW #( 'CCCONTAINER' ).
      html_control = NEW #( container ).
    ENDIF.

  ENDMETHOD.


  METHOD user_command_9000.

    DATA(ucomm) = sy-ucomm.
    CLEAR sy-ucomm.

    CASE ucomm.
      WHEN 'BACK' OR 'EXIT'.
        LEAVE PROGRAM.
      WHEN 'ENTER'.
        display_breweries( addr1_data-city1 ).
    ENDCASE.

  ENDMETHOD.

  METHOD exit_command_9000.

    LEAVE PROGRAM.

  ENDMETHOD.

  METHOD string_to_table.

    DATA: split_table TYPE TABLE OF string,
          line        TYPE char1024.

    SPLIT i_html_string AT space INTO TABLE split_table.

    LOOP AT split_table REFERENCE INTO DATA(split).
      DATA(len) = strlen( line ) + strlen( split->* ).
      IF len LT 1024.
        CONCATENATE line split->* INTO line SEPARATED BY space.
      ELSE.
        INSERT line INTO TABLE r_result.
        line = split->*.
      ENDIF.
    ENDLOOP.
    INSERT line INTO TABLE r_result.

  ENDMETHOD.


  METHOD display_breweries.

    DATA(city_coordinate) = get_city_coordinate( i_city ).
    CHECK city_coordinate IS NOT INITIAL.

    DATA(brewery_coordinate) = get_nearest_brewery( city_coordinate ).

    DATA(html_string) = NEW zcl_geojson_leafletjs(  )->get_html(
        i_json          = get_geojson(
                            i_city = city_coordinate
                            i_brewery = brewery_coordinate )
        i_width_x_in_px = 1024
        i_width_y_in_px = 580 ).

    DATA(html_table) = string_to_table( html_string ).

    DATA url TYPE c LENGTH 1024.

    html_control->load_data(
      IMPORTING
        assigned_url           = url    " URL
      CHANGING
        data_table             = html_table    " data table
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5
    ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    html_control->show_url( url ).

  ENDMETHOD.


  METHOD get_geojson.

    DATA(geojson) = NEW zcl_geojson( ).

    DATA(point) = geojson->get_new_point(
      i_latitude = i_city-latitude
      i_longitude = i_city-longitude
    ).

    point->set_properties(
        i_popup_content = i_city-city
    ).
    geojson->add_feature( point ).

    point = geojson->get_new_point(
      i_latitude = i_brewery-latitude
      i_longitude = i_brewery-longitude
    ).

    point->set_properties(
        i_popup_content = i_brewery-name
    ).
    geojson->add_feature( point ).

    DATA(line) = geojson->get_new_linestring( ).

    line->add_coordinate(
      EXPORTING
        i_latitude = i_city-latitude
        i_longitude = i_city-longitude
    ).
    line->add_coordinate(
      EXPORTING
        i_latitude = i_brewery-latitude
        i_longitude = i_brewery-longitude
    ).

    line->set_properties(
        i_popup_content = |{ i_brewery-distance } km|
    ).

    geojson->add_feature( line ).

    r_result = geojson->get_json( ).

  ENDMETHOD.


  METHOD get_city_coordinate.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = |http://open.mapquestapi.com/geocoding/v1/address?key=dN6Tj5IVcSIf5kEm8PrWMZ5PKjAyanh7&location={ cl_http_utility=>escape_url( CONV #( i_city ) ) }|    " URL
      IMPORTING
        client             = DATA(client)    " HTTP Client Abstraction
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4
    ).

    CHECK sy-subrc = 0.

    client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).

    CHECK sy-subrc = 0.

    client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4
    ).

    CHECK sy-subrc = 0.

    TYPES: BEGIN OF ty_latlng,
             lat TYPE zcl_geojson=>ty_coordinate_value,
             lng TYPE zcl_geojson=>ty_coordinate_value,
           END OF ty_latlng.

    TYPES: BEGIN OF ty_location,
             latlng TYPE ty_latlng,
           END OF ty_location.

    TYPES: tt_locations TYPE STANDARD TABLE OF ty_location.

    DATA: BEGIN OF result,
            locations TYPE tt_locations,
          END OF result.

    DATA: results LIKE STANDARD TABLE OF result.
    DATA: BEGIN OF geocode,
            results LIKE results,
          END OF geocode.

    DATA(json) = zcl_json_document=>create_with_json( client->response->get_cdata( ) ).

    TRY.
        json->get_data(
          EXPORTING
            json              = client->response->get_cdata( )
          IMPORTING
            data              = geocode
        ).


        DATA(latlng) = geocode-results[ 1 ]-locations[ 1 ]-latlng.
        r_result-city = i_city.
        r_result-latitude = latlng-lat.
        r_result-longitude = latlng-lng.

      CATCH zcx_json_document
            cx_sy_itab_line_not_found ##no_handler.    "

    ENDTRY.

  ENDMETHOD.


  METHOD get_nearest_brewery.

    DATA(brewery_ids) = get_brewery_ids( ).
    CHECK brewery_ids IS NOT INITIAL.

    r_result-distance = '999999'.

    LOOP AT brewery_ids REFERENCE INTO DATA(brewery_id).

      DATA(brewery_coordinate) = get_brewery_coordinate( brewery_id ).
      CHECK brewery_coordinate IS NOT INITIAL.

      brewery_coordinate-distance = get_distance( i_city = i_city_coordinate i_brewery = brewery_coordinate ).

      IF brewery_coordinate-distance < r_result-distance.
        r_result = brewery_coordinate.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_brewery_ids.

    TRY.
        DATA(statement) = |select distinct "brewery_id"from cba_beer."beers" where "name" = 'Altbier' or "name" = 'Alt';|.

        DATA(result) = sql->execute_query( statement ).

        result->set_param_table( REF #( r_result ) ).
        result->next_package( ).

      CATCH cx_sql_exception INTO DATA(lcx).
        cl_demo_output=>display( lcx->get_text( ) ).

      CATCH cx_parameter_invalid INTO DATA(lcx_parameter).    "
        cl_demo_output=>display( lcx_parameter->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD constructor.

    TRY.
        sql = NEW cl_sql_statement( con_ref = cl_sql_connection=>get_connection( 'HANACBA' ) ).
      CATCH  cx_sql_exception INTO DATA(lcx).
        MESSAGE lcx TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD get_brewery_coordinate.

    DATA results TYPE STANDARD TABLE OF ty_coordinate WITH EMPTY KEY.

    TRY.
        DATA(statement) = |select brewery."name", '' as address, '' as city, '' as country, | &&
                          |  geocode."longitude" as longitude, geocode."latitude" as latitude, 0 as distance | &&
                          |  from cba_beer."breweries" as brewery | &&
                          |  inner join cba_beer."breweries_geocode" as geocode | &&
                          |  on geocode."brewery_id" = brewery."id" | &&
                          |  where brewery."id" = { i_brewery_id->* } | &&
                          |  and   geocode."brewery_id" = brewery."id" |.

        DATA(result) = sql->execute_query( statement ).

        result->set_param_table( REF #( results ) ).
        result->next_package( ).

        r_result = results[ 1 ].

      CATCH cx_sql_exception INTO DATA(lcx).
        cl_demo_output=>write( lcx->get_text( ) ).

      CATCH cx_parameter_invalid INTO DATA(lcx_parameter).    "
        cl_demo_output=>write( lcx_parameter->get_text( ) ).

      CATCH cx_sy_itab_line_not_found INTO DATA(lcx_line).
        cl_demo_output=>write( lcx_line->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD get_distance.

    DATA results TYPE STANDARD TABLE OF ty_distance WITH EMPTY KEY.

    TRY.
        DATA(statement) = |SELECT NEW ST_Point('POINT ({ i_city-longitude } { i_city-latitude })', 4326)| &&
                          |.ST_Distance(NEW ST_Point('POINT ({ i_brewery-longitude } { i_brewery-latitude })', 4326), 'kilometer') | &&
                          |FROM dummy;|.

        DATA(result) = sql->execute_query( statement ).

        result->set_param_table( REF #( results ) ).
        result->next_package( ).

        r_result = results[ 1 ].

      CATCH cx_sql_exception INTO DATA(lcx).
        cl_demo_output=>write( lcx->get_text( ) ).

      CATCH cx_parameter_invalid INTO DATA(lcx_parameter).    "
        cl_demo_output=>write( lcx_parameter->get_text( ) ).

      CATCH cx_sy_itab_line_not_found INTO DATA(lcx_line).
        cl_demo_output=>write( lcx_line->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  app->status_9000( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  app->user_command_9000( ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_9000 INPUT.
  app->exit_command_9000( ).
ENDMODULE.
