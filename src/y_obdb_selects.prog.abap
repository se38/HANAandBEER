*&---------------------------------------------------------------------*
*& Report ys_openbeer_selects
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_obdb_selects.

CLASS app DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

NEW app( )->main( ).

CLASS app IMPLEMENTATION.

  METHOD main.

    TYPES: BEGIN OF ty_beer,
             beer     TYPE string,
             brewery  TYPE string,
             category TYPE string,
             style    TYPE string,
             abv      TYPE string,
           END OF ty_beer.

    DATA beers TYPE STANDARD TABLE OF ty_beer.

    TRY.
        DATA(sql) = NEW cl_sql_statement( con_ref = cl_sql_connection=>get_connection( 'HANACBA' ) ).

        DATA(statement) = |select top 100 "beers"."name", "breweries"."name", "categories"."cat_name", "styles"."style_name", "beers"."abv"| &&
                          |  from "CBA_BEER"."beers" as "beers"| &&
                          |  left join "CBA_BEER"."breweries" as "breweries"| &&
                          |  on "breweries"."id" = "beers"."brewery_id"| &&
                          |  left join "CBA_BEER"."categories" as "categories"| &&
                          |  on "categories"."id" = "beers"."cat_id"| &&
                          |  left join "CBA_BEER"."styles" as "styles"| &&
                          |  on "styles"."id" = "beers"."style_id"| &&
                          |  where "beers"."abv" >= '4.0' and "beers"."abv" <= '5.0'| &&
                          |  order by "beers"."abv"|.

        DATA(result) = sql->execute_query( statement ).
        result->set_param_table( REF #( beers ) ).
        result->next_package( ).

        cl_demo_output=>display( beers ).

      CATCH cx_sql_exception INTO DATA(lcx2).
        cl_demo_output=>display( lcx2->get_text( ) ).

      CATCH cx_parameter_invalid INTO DATA(lcx_parameter).    "
        cl_demo_output=>display( lcx_parameter->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
