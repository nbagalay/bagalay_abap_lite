************************************************************************
*                              Documentation                           *
*                                                                      *
* Title              : ABAP Sample - IDA Report for CDS Views          *
* Original Creator   : Nick Bagalay                                    *
* Creation Month/Year: November 2022                                   *
* Purpose/Detail     : Sample program illustrating the use of IDA ALV
*                                                                      *
* SPECIAL NOTE: You will need to run SAPBC_DATA_GENERATOR to populate
*     the SFLIGHT model tables. Without it, the program won't have data
*     to pull back.
************************************************************************
*                         History of Revisions                         *
************************************************************************
*  Mod  |   Date   |   SAP   | Transport | Specify changes made        *
*       |          | USER ID |           |                             *
*-------|----------|---------|-----------|-----------------------------*
************************************************************************
REPORT znnb_abap_ida_flight_rpt.

************************************************************************
*******                   Types Specifications                   *******
************************************************************************
CONSTANTS: BEGIN OF gc_range_tbl,
             BEGIN OF sign,
               include TYPE char1 VALUE 'I',
               exclude TYPE char1 VALUE 'E',
             END OF sign,

             BEGIN OF option,
               equal_to TYPE char2 VALUE 'EQ',
             END OF option,
           END OF gc_range_tbl.

************************************************************************
*******                   Working Storage Area                   *******
************************************************************************
DATA: lv_flight_date TYPE s_date.

************************************************************************
*******                     Selection-Screen                     *******
************************************************************************

PARAMETERS: p_carrid    TYPE s_carr_id.

SELECT-OPTIONS: s_fldate  FOR lv_flight_date.


************************************************************************
*******                  Local Class Definition                  *******
************************************************************************
CLASS lcl_ida_event_handle DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor    IMPORTING io_flight_ida    TYPE REF TO if_salv_gui_table_ida,
      handle_dbclick FOR EVENT double_click     OF if_salv_gui_table_display_opt.

  PRIVATE SECTION.
    DATA: lo_flight_ida   TYPE REF TO if_salv_gui_table_ida.
ENDCLASS.

CLASS lcl_flight_ida DEFINITION.

  PUBLIC SECTION.

    DATA: ltr_carrier_id    TYPE RANGE OF s_carr_id.

    METHODS constructor.

    METHODS display_sflight_info.


  PRIVATE SECTION.
    METHODS set_filter_for_flight
      CHANGING co_flight_ida TYPE REF TO if_salv_gui_table_ida.

ENDCLASS.

************************************************************************
*******                Local Class Implementation                *******
************************************************************************
CLASS lcl_ida_event_handle IMPLEMENTATION.
  METHOD constructor.
    lo_flight_ida = io_flight_ida.
  ENDMETHOD.

  METHOD handle_dbclick.
    DATA: ls_flight   TYPE sflight.


    " NOTE: Here on the double click event we will be opening the SBOOK to get the details
    "       of the customers on the flight.
    IF lo_flight_ida IS BOUND AND
       lo_flight_ida->selection( )->is_row_selected( ).

      lo_flight_ida->selection( )->get_selected_row( IMPORTING es_row = ls_flight ).

      DATA(lo_sbook_ida) = cl_salv_gui_table_ida=>create( iv_table_name = 'SBOOK' ).
      IF lo_sbook_ida IS BOUND.
        lo_sbook_ida->set_select_options( it_ranges = VALUE #( ( name     = 'CARRID'
                                                                 sign     = gc_range_tbl-sign-include
                                                                 option   = gc_range_tbl-option-equal_to
                                                                 low      = ls_flight-carrid )
                                                               ( name     = 'CONNID'
                                                                 sign     = gc_range_tbl-sign-include
                                                                 option   = gc_range_tbl-option-equal_to
                                                                 low      = ls_flight-connid )
                                                               ( name     = 'FLDATE'
                                                                 sign     = gc_range_tbl-sign-include
                                                                 option   = gc_range_tbl-option-equal_to
                                                                 low      = ls_flight-fldate ) ) ).
        lo_sbook_ida->fullscreen( )->display( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_flight_ida IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD display_sflight_info.

    CHECK cl_salv_gui_table_ida=>db_capabilities( )->is_table_supported( iv_ddic_table_name = 'SFLIGHT' ).

    DATA(lo_flight_ida) = cl_salv_gui_table_ida=>create( iv_table_name = 'SFLIGHT' ).

    "--Additional Criteria--
    "Filtering ability
    me->set_filter_for_flight( CHANGING co_flight_ida = lo_flight_ida ).

    "Field Catalog
    lo_flight_ida->field_catalog( )->set_available_fields( EXPORTING its_field_names = VALUE #( ( CONV fieldname( 'CARRID' ) )
                                                                                                ( CONV fieldname( 'FLDATE' ) )
                                                                                                ( CONV fieldname( 'PRICE' ) )
                                                                                                ( CONV fieldname( 'PLANETYPE' ) )
                                                                                                ( CONV fieldname( 'SEATSMAX' ) )
                                                                                                ( CONV fieldname( 'SEATSOCC' ) ) ) ).

    "Sort Ordering
    lo_flight_ida->default_layout( )->set_sort_order( EXPORTING it_sort_order = VALUE #( ( field_name = 'CARRID'
                                                                                           is_grouped = abap_true )
                                                                                         ( field_name = 'PLANETYPE'
                                                                                           is_grouped = abap_true )
                                                                                         ( field_name = 'SEATSOCC'
                                                                                           descending = abap_true ) ) ).
    "Event Handler
    lo_flight_ida->display_options( )->enable_double_click( ).
    lo_flight_ida->selection( )->set_selection_mode( EXPORTING iv_mode = 'SINGLE' ).
    DATA(lo_flight_handler) = NEW lcl_ida_event_handle( io_flight_ida = lo_flight_ida ).
    SET HANDLER lo_flight_handler->handle_dbclick FOR lo_flight_ida->display_options( ).

    lo_flight_ida->fullscreen( )->display( ).

  ENDMETHOD.

  "----PRIVATE----
  METHOD set_filter_for_flight.

    IF p_carrid IS NOT INITIAL.
      ltr_carrier_id = VALUE #( BASE ltr_carrier_id
                                sign   = gc_range_tbl-sign-include
                                option = gc_range_tbl-option-equal_to
                              ( low    = p_carrid ) ).
    ENDIF.

    DATA(lo_flight_selection) = NEW cl_salv_range_tab_collector( ).
    lo_flight_selection->add_ranges_for_name( iv_name = 'CARRID' it_ranges = ltr_carrier_id[] ).
    lo_flight_selection->add_ranges_for_name( iv_name = 'FLDATE' it_ranges = s_fldate[] ).

    lo_flight_selection->get_collected_ranges( IMPORTING et_named_ranges = DATA(lt_named_ranges) ).

    co_flight_ida->set_select_options( it_ranges = lt_named_ranges ).

  ENDMETHOD.

ENDCLASS.

************************************************************************
*******                  Start-Of-Selection Area                 *******
************************************************************************
START-OF-SELECTION.

  NEW lcl_flight_ida( )->display_sflight_info( ).