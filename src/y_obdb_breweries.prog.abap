*&---------------------------------------------------------------------*
*& Report YSTSTF00
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_obdb_breweries.

TABLES addr1_data.
DATA : container    TYPE REF TO cl_gui_custom_container,
       html_control TYPE REF TO cl_gui_html_viewer.

CLASS lcl_app DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS main.
    METHODS status_9000.
    METHODS user_command_9000.
    METHODS exit_command_9000.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_coordinate,
             name      TYPE string,
             address   TYPE string,
             city      TYPE string,
             country   TYPE string,
             longitude TYPE p LENGTH 16 DECIMALS 13,
             latitude  TYPE p LENGTH 16 DECIMALS 13,
           END OF ty_coordinate.
    TYPES: tt_breweries TYPE STANDARD TABLE OF ty_coordinate WITH EMPTY KEY.
    TYPES tt_html_table TYPE STANDARD TABLE OF char1024 WITH EMPTY KEY.

    METHODS string_to_table
      IMPORTING i_html_string   TYPE string
      RETURNING VALUE(r_result) TYPE tt_html_table.
    METHODS display_breweries
      IMPORTING i_city TYPE ad_city1.
    METHODS get_breweries
      IMPORTING
        i_city          TYPE ad_city1
      RETURNING
        VALUE(r_result) TYPE tt_breweries.
    METHODS get_geojson
      IMPORTING
        i_breweries     TYPE tt_breweries
      RETURNING
        VALUE(r_result) TYPE string.

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

    DATA(breweries) = get_breweries( i_city ).
    CHECK breweries IS NOT INITIAL.

    DATA(html_string) = NEW zcl_geojson_leafletjs(  )->get_html(
        i_json          = get_geojson( breweries )
        i_width_x_in_px = 1024
        i_width_y_in_px = 580
    ).

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


  METHOD get_breweries.

    TRY.
        DATA(sql) = NEW cl_sql_statement( con_ref = cl_sql_connection=>get_connection( 'HANACBA' ) ).

        DATA(statement) = |select "breweries"."name", | &&
                          |"breweries"."address1", | &&
                          |"breweries"."city", | &&
                          |"breweries"."country", | &&
                          |"breweries_geocode"."point".ST_X(), | &&
                          |"breweries_geocode"."point".ST_Y() | &&
                          |from "CBA_BEER"."breweries" | &&
*                          |inner join "CBA_BEER"."beers" | &&
*                          |on "beers"."brewery_id" = "breweries"."id" | &&
                          |inner join "CBA_BEER"."breweries_geocode" | &&
                          |on "breweries_geocode"."brewery_id" = "breweries"."id" | &&
                          |where "breweries"."city" = '{ i_city }' |.
**                          |where "beers"."name" = 'Kölsch' |.
*                          |where "beers"."name" = 'Alt' | &&
*                          |or    "beers"."name" = 'Altbier' | &&
*                          |or    "beers"."name" = 'Alt Bier' |.

        DATA(result) = sql->execute_query( statement ).
        result->set_param_table( REF #( r_result ) ).
        result->next_package( ).

        IF r_result IS INITIAL.
          MESSAGE |No brewery in { i_city } found| TYPE 'I'.
          RETURN.
        ENDIF.

      CATCH cx_sql_exception INTO DATA(lcx2).
        cl_demo_output=>display( lcx2->get_text( ) ).

      CATCH cx_parameter_invalid INTO DATA(lcx_parameter).    "
        cl_demo_output=>display( lcx_parameter->get_text( ) ).

    ENDTRY.

  ENDMETHOD.


  METHOD get_geojson.

    DATA(geojson) = NEW zcl_geojson( ).

    LOOP AT i_breweries REFERENCE INTO DATA(brewery).

      DATA(point) = geojson->get_new_point(
        i_latitude = brewery->latitude
        i_longitude = brewery->longitude
      ).

      point->set_properties(
          i_popup_content = |{ brewery->name }<br />| &&
                            |{ brewery->address }<br />| &&
                            |{ brewery->city }<br />| &&
                            |{ brewery->country }<br />|
      ).
      geojson->add_feature( point ).
    ENDLOOP.

    r_result = geojson->get_json( ).

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
