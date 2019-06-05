*&---------------------------------------------------------------------*
*& Report y_obdb_vertex_edge
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_obdb_vertex_edge.

CLASS app DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

NEW app( )->main( ).

CLASS app IMPLEMENTATION.

  METHOD main.

    TYPES: BEGIN OF ty_brewery,
             id   TYPE i,
             city TYPE string,
             country TYPE string,
           END OF TY_BREWERY.

    DATA brewieries TYPE STANDARD TABLE OF ty_brewery.

    TYPES: BEGIN OF ty_city,
             id      TYPE i,
             city    TYPE string,
             country TYPE string,
           END OF ty_city.

    DATA cities TYPE STANDARD TABLE OF ty_city WITH EMPTY KEY.

    TRY.
        DATA(sql) = NEW cl_sql_statement( con_ref = cl_sql_connection=>get_connection( 'HANACBA' ) ).

        DATA(statement) = |select id, city, country from "CBA_BEER"."vertex" | &&
                          |  where type = 'BREWERY' | &&
                          |  and city <> '' and city is not NULL | &&
                          |  and country <> '' and country is not NULL;|.

        DATA(result) = sql->execute_query( statement ).

        result->set_param_table( REF #( brewieries ) ).
        result->next_package( ).

        statement = |select id, city, country from "CBA_BEER"."vertex" | &&
                    |  where type = 'CITY';|.

        result = sql->execute_query( statement ).

        result->set_param_table( REF #( cities ) ).
        result->next_package( ).

        DATA id TYPE i VALUE 21335.

        LOOP AT brewieries REFERENCE INTO DATA(brewery).

          id = id + 1.

          TRY.
              statement = |INSERT INTO "CBA_BEER"."edge" VALUES( | &&
                          |{ id }, | &&
                          |{ brewery->id },| &&
                          |{ cities[ city = brewery->city country = brewery->country ]-id },| &&
                          |'IN_CITY' );|.
            CATCH cx_sy_itab_line_not_found.
              WRITE:/ 'City not found', brewery->city, brewery->country.
          ENDTRY.

          DATA(i) = sql->execute_update( statement ).
*          cl_demo_output=>write( statement ).
        ENDLOOP.

*        cl_demo_output=>display( ).
        WRITE:/ 'fertig'.
      CATCH cx_sql_exception INTO DATA(lcx).
        cl_demo_output=>display( 'SQL Fehler' && statement ).

      CATCH cx_parameter_invalid INTO DATA(lcx_parameter).    "
        cl_demo_output=>display( lcx_parameter->get_text( ) ).

    ENDTRY.


  ENDMETHOD.

ENDCLASS.
