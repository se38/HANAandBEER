*&---------------------------------------------------------------------*
*& Report ys_openbeer_selects
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_obdb_selects_cypher.

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
             brewery  TYPE string,
             beer     TYPE string,
           END OF ty_beer.

    DATA beers TYPE STANDARD TABLE OF ty_beer.

    TRY.
        DATA(sql) = NEW cl_sql_statement( con_ref = cl_sql_connection=>get_connection( 'HANACBA' ) ).

        DATA(statement) = |SELECT * FROM OPENCYPHER_TABLE( GRAPH WORKSPACE "CBA_BEER"."beer_graph" QUERY '| &&
                          |  match (beer)-[e]->(brewery) | &&
                          |  where beer.NAME starts with ''Alt'' | &&
                          |  and e.TYPE = ''BREWED_AT'' | &&
                          |  return brewery.NAME as brewery, beer.NAME as beer | &&
                          |  order by brewery.NAME | &&
                          |')|.


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