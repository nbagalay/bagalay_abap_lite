class ZCL_PROJECT_TRACKER_DPC definition
  public
  inheriting from /IWBEP/CL_V4_ABS_DATA_PROVIDER
  final
  create public .

public section.

  interfaces ZIF_PROJECT_TRACKER_TYPES .
  interfaces ZIF_ACCOUNT_DETAILS_TYPES .

  class-methods OBTAIN_NEW_GUID
    returning
      value(RV_NEW_GUID) type STRING
    exceptions
      EX_GUID_EXCEPTION .
  class-methods FIELDMAP_TO_DB_CONVERSION
    importing
      !IT_FIELD_MAP type ZIF_PROJECT_TRACKER_TYPES~GTT_EDM_FIELD_MAP
      !IS_BUSI_DATA type ANY
      !IV_DB_TABLE_NAME type CHAR16
    exporting
      !ES_DB_TBL_ROW type ANY .

  methods /IWBEP/IF_V4_DP_BASIC~CREATE_ENTITY
    redefinition .
  methods /IWBEP/IF_V4_DP_BASIC~DELETE_ENTITY
    redefinition .
  methods /IWBEP/IF_V4_DP_BASIC~READ_ENTITY
    redefinition .
  methods /IWBEP/IF_V4_DP_BASIC~READ_ENTITY_LIST
    redefinition .
  methods /IWBEP/IF_V4_DP_BASIC~READ_REF_TARGET_KEY_DATA
    redefinition .
  methods /IWBEP/IF_V4_DP_BASIC~READ_REF_TARGET_KEY_DATA_LIST
    redefinition .
  methods /IWBEP/IF_V4_DP_BASIC~UPDATE_ENTITY
    redefinition .
  methods /IWBEP/IF_V4_DP_INTERMEDIATE~PATCH_ENTITY
    redefinition .
protected section.

  methods READ_ENTITY_WORKITEMS
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_READ
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_READ
    raising
      /IWBEP/CX_GATEWAY .
  methods READ_LIST_WORKITEMS
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_LIST
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_LIST
      !IS_DONE_LIST type /IWBEP/IF_V4_REQU_BASIC_LIST=>TY_S_TODO_PROCESS_LIST
      !IV_ORDERBY_STRING type STRING
      !IV_SELECT_STRING type STRING
      !IV_SKIP type I
      !IV_TOP type I
      !IV_WHERE_CLAUSE type STRING
    raising
      /IWBEP/CX_GATEWAY .
  methods CREATE_WORKITEMS
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_CREATE
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_CREATE
    raising
      /IWBEP/CX_GATEWAY .
  methods OBTAIN_CURRENT_TIMESTAMP
    returning
      value(RV_CURRENT_TS) type TIMESTAMP .
  methods UPDATE_WORKITEMS
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_UPDATE
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_UPDATE
    raising
      /IWBEP/CX_GATEWAY .
  methods DELETE_WORKITEMS
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_DELETE
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_DELETE
    raising
      /IWBEP/CX_GATEWAY .
  methods READ_REF_KEY_LIST_WORKITEM
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_REF_L
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_REF_L
    raising
      /IWBEP/CX_GATEWAY .
  methods READ_ENTITY_TIMEENTRY
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_READ
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_READ
    raising
      /IWBEP/CX_GATEWAY .
  methods READ_LIST_TIMEENTRY
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_LIST
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_LIST
      !IS_DONE_LIST type /IWBEP/IF_V4_REQU_BASIC_LIST=>TY_S_TODO_PROCESS_LIST
      !IV_ORDERBY_STRING type STRING
      !IV_SELECT_STRING type STRING
      !IV_SKIP type I
      !IV_TOP type I
      !IV_WHERE_CLAUSE type STRING
    raising
      /IWBEP/CX_GATEWAY .
  methods CREATE_TIMEENTRY
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_CREATE
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_CREATE
    raising
      /IWBEP/CX_GATEWAY .
  methods UPDATE_TIMEENTRY
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_UPDATE
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_UPDATE
    raising
      /IWBEP/CX_GATEWAY .
  methods DELETE_TIMEENTRY
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_DELETE
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_DELETE
    raising
      /IWBEP/CX_GATEWAY .
  methods READ_REF_KEY_TIMEENTRY
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_REF_R
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_REF_R
    raising
      /IWBEP/CX_GATEWAY .
  methods PATCH_TIMEENTRY
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_INTM_PATCH
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_INTM_PATCH
    raising
      /IWBEP/CX_GATEWAY .
  PRIVATE SECTION.

    METHODS validation_standard
      IMPORTING
        !it_field_map          TYPE zif_project_tracker_types~gtt_edm_field_map
        !is_busi_data          TYPE any
        !is_original_busi_data TYPE any
        !iv_update_entity      TYPE abap_bool
      RETURNING
        VALUE(rt_return_msg)   TYPE bapiret2_t .
ENDCLASS.



CLASS ZCL_PROJECT_TRACKER_DPC IMPLEMENTATION.


  METHOD /iwbep/if_v4_dp_basic~create_entity.
    DATA: lv_entity_type_name TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name.

    " DEVELOPER NOTE: At the BASIC~CREATE_ENTITY this handles non-complex structure. However, if
    "   you wan tto support DEEP INSERT like true OData, go to the ADVANCE~CREATE_ENTITY to handle that
    "   level of detail

    io_request->get_entity_type( IMPORTING ev_entity_type_name = lv_entity_type_name ).

    CASE lv_entity_type_name.
      WHEN zif_project_tracker_types~gc_entity_type_names-internal-workitem.
        create_workitems( io_request  = io_request
                          io_response = io_response ).

      WHEN zif_project_tracker_types~gc_entity_type_names-internal-timentry.
        create_timeentry( io_request  = io_request
                          io_response = io_response ).

      WHEN OTHERS.
        super->/iwbep/if_v4_dp_basic~create_entity( EXPORTING io_request  = io_request
                                                              io_response = io_response ).
    ENDCASE.
  ENDMETHOD.


  METHOD /iwbep/if_v4_dp_basic~delete_entity.
    DATA: lv_entity_type_name TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name.

    io_request->get_entity_type( IMPORTING ev_entity_type_name = lv_entity_type_name ).

    CASE lv_entity_type_name.
      WHEN zif_project_tracker_types~gc_entity_type_names-internal-workitem.
        delete_workitems( io_request  = io_request
                          io_response = io_response ).

      WHEN zif_project_tracker_types~gc_entity_type_names-internal-timentry.
        delete_timeentry( io_request  = io_request
                          io_response = io_response ).

      WHEN OTHERS.
        super->/iwbep/if_v4_dp_basic~delete_entity( EXPORTING io_request  = io_request
                                                              io_response = io_response ).
    ENDCASE.
  ENDMETHOD.


  METHOD /iwbep/if_v4_dp_basic~read_entity.
    DATA: lv_entityset_name TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name.

    io_request->get_entity_set( IMPORTING ev_entity_set_name = lv_entityset_name ).

    CASE lv_entityset_name.
      WHEN zif_project_tracker_types~gc_entity_set_names-internal-workitemset.
        read_entity_workitems( EXPORTING io_request        = io_request
                                         io_response       = io_response
        ).
      WHEN zif_project_tracker_types~gc_entity_set_names-internal-timeentryset.
        read_entity_timeentry( EXPORTING io_request        = io_request
                                         io_response       = io_response
        ).
      WHEN OTHERS.
        super->/iwbep/if_v4_dp_basic~read_entity( EXPORTING io_request  = io_request
                                                            io_response = io_response ).
    ENDCASE.

  ENDMETHOD.


  METHOD /iwbep/if_v4_dp_basic~read_entity_list.
    DATA: ls_todo_list         TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_list,
          ls_done_list         TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_process_list,
          lt_selected_property TYPE /iwbep/if_v4_runtime_types=>ty_t_property_path,
          lv_entityset_name    TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name,
          lv_orderby_string    TYPE string,
          lt_orderby           TYPE abap_sortorder_tab,
          lv_skip              TYPE i VALUE 0,
          lv_top               TYPE i VALUE 0,
          lv_where_clause      TYPE string,
          lv_skiptoken_str     TYPE string,
          lv_select_string     TYPE string,
          lv_skiptoken_i       TYPE i.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).
    io_request->get_entity_set( IMPORTING ev_entity_set_name = lv_entityset_name ).

*   /** DEAL WITH ORDER BY **\
    IF ls_todo_list-process-orderby = abap_true.
      ls_done_list-orderby = abap_true.
      io_request->get_osql_orderby_clause( IMPORTING ev_osql_orderby_clause = lv_orderby_string ).

*     Alternate - If you are on an older system, you might have to use this statement instead
*       and manually build the statement
****      io_request->get_orderby( IMPORTING et_orderby_property = lt_orderby    " Sort properties / property paths provided by ($orderby)
****      ).
    ELSE.
      lv_orderby_string = zif_project_tracker_types~gc_orderby_primary_key.
    ENDIF.

*   /** CLIENT SIDE PAGING - SKIP/TOP **\
    IF ls_todo_list-process-skip = abap_true.
      ls_done_list-skip = abap_true.
      io_request->get_skip( IMPORTING ev_skip = lv_skip ).
    ENDIF.

*   /** SERVER SIDE PAGING - SKIP/TOP **\
    IF ls_todo_list-process-skip_token = abap_true.
      io_request->get_skip_token( IMPORTING ev_skip_token = lv_skiptoken_str ).
      ls_done_list-skip_token = abap_true.

      IF lv_skiptoken_str IS NOT INITIAL.

        TRY .
            lv_skiptoken_i = CONV i( lv_skiptoken_str ).
          CATCH cx_root.
            RAISE EXCEPTION TYPE zcx_project_tracker
              EXPORTING
                textid           = zcx_project_tracker=>bad_skiptoken
                http_status_code = /iwbep/cx_gateway=>gcs_http_status_codes-bad_request
                gv_skiptoken     = lv_skiptoken_str.
        ENDTRY.

        IF lv_skiptoken_i > 0.
          lv_skip = lv_skiptoken_i.
        ENDIF.
      ENDIF.
    ENDIF.

    lv_top = zif_project_tracker_types~gc_max_responses.
    IF ls_todo_list-process-top = abap_true.
      ls_done_list-top = abap_true.
      io_request->get_top( IMPORTING ev_top = lv_top ).

      "Invalid Request - we only support up to gc_max_responses.
      IF lv_top > zif_project_tracker_types~gc_max_responses.
        RAISE EXCEPTION TYPE zcx_project_tracker
          EXPORTING
            textid             = zcx_project_tracker=>max_responses_exceeded
            http_status_code   = /iwbep/cx_gateway=>gcs_http_status_codes-bad_request
            entity_set_name    = lv_entityset_name.
      ENDIF.
    ENDIF.

*   /** SELECT LIST **\
    IF ls_todo_list-process-select = abap_true.
      ls_done_list-select = abap_true.
      io_request->get_selected_properties(  IMPORTING et_selected_property = lt_selected_property ).

      lv_select_string = concat_lines_of( table = lt_selected_property
                                          sep   = ',' ).

    ELSE.
      lv_select_string = '*'.
    ENDIF.

*   /** FILTER **\
    IF ls_todo_list-process-filter = abap_true.
      ls_done_list-filter = abap_true.
      io_request->get_filter_osql_where_clause( IMPORTING ev_osql_where_clause = lv_where_clause ).
    ENDIF.

    CASE lv_entityset_name.
      WHEN zif_project_tracker_types~gc_entity_set_names-internal-workitemset.
        read_list_workitems( EXPORTING io_request        = io_request
                                       io_response       = io_response
                                       iv_orderby_string = lv_orderby_string
                                       iv_select_string  = lv_select_string
                                       iv_where_clause   = lv_where_clause
                                       iv_skip           = lv_skip
                                       iv_top            = lv_top
                                       is_done_list      = ls_done_list ).
      WHEN zif_project_tracker_types~gc_entity_set_names-internal-timeentryset.
        read_list_timeentry( EXPORTING io_request        = io_request
                                       io_response       = io_response
                                       iv_orderby_string = lv_orderby_string
                                       iv_select_string  = lv_select_string
                                       iv_where_clause   = lv_where_clause
                                       iv_skip           = lv_skip
                                       iv_top            = lv_top
                                       is_done_list      = ls_done_list ).
      WHEN OTHERS.
        super->/iwbep/if_v4_dp_basic~read_entity_list( io_request  = io_request
                                                       io_response = io_response ).
    ENDCASE.
  ENDMETHOD.


  METHOD /iwbep/if_v4_dp_basic~read_ref_target_key_data.
    DATA: lv_source_entity_name TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name.

    io_request->get_source_entity_type( IMPORTING ev_source_entity_type_name = lv_source_entity_name ).

    CASE lv_source_entity_name.
      WHEN zif_project_tracker_types~gc_entity_type_names-internal-timentry.
        read_ref_key_timeentry( EXPORTING io_request  = io_request
                                          io_response = io_response ).

      WHEN OTHERS.
        super->/iwbep/if_v4_dp_basic~read_ref_target_key_data( EXPORTING io_request  = io_request   " Basic Request Info Create
                                                                         io_response = io_response  " Basic Response Info Create
        ).
    ENDCASE.
  ENDMETHOD.


  METHOD /iwbep/if_v4_dp_basic~read_ref_target_key_data_list.
    DATA: lv_source_entity_name TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name.

    io_request->get_source_entity_type( IMPORTING ev_source_entity_type_name = lv_source_entity_name ).

    CASE lv_source_entity_name.
      WHEN zif_project_tracker_types~gc_entity_type_names-internal-workitem.
        read_ref_key_list_workitem( EXPORTING io_request  = io_request
                                              io_response = io_response ).

      WHEN OTHERS.
        super->/iwbep/if_v4_dp_basic~read_ref_target_key_data_list(
          EXPORTING
            io_request  = io_request
            io_response = io_response ).
    ENDCASE.
  ENDMETHOD.


  METHOD /iwbep/if_v4_dp_basic~update_entity.
    DATA: lv_entityset_name TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name.
    io_request->get_entity_set( IMPORTING ev_entity_set_name = lv_entityset_name ).

    CASE lv_entityset_name.
      WHEN zif_project_tracker_types~gc_entity_set_names-internal-workitemset.
        update_workitems( EXPORTING io_request        = io_request
                                    io_response       = io_response
        ).

      WHEN zif_project_tracker_types~gc_entity_set_names-internal-timeentryset.

        "SAMPLE: 'TIME ENTRY' ONLY SUPPORTS PATCH CALLS, NO PUT
        " In this example, we only support PATCH calls. Sometimes there are developments that has CDS views but
        " not all the of the fields have a mapping to a direct table (say like a Sales Order Update where you
        " want to use the BAPI). Anyways, BASIC~UPDATE is basically where the PUT call is handled.

        " We handle PATCH at the INTERMEDIATE level so by this level, this would get called ONLY
        " if the user specified PUT. We want to let then know we have PATCH only support
        RAISE EXCEPTION TYPE zcx_project_tracker
          EXPORTING
            textid           = zcx_project_tracker=>put_support_not_allowed
            http_status_code = /iwbep/cx_gateway=>gcs_http_status_codes-method_not_allowed.


**        update_timeentry( EXPORTING io_request        = io_request
**                                    io_response       = io_response
**        ).

      WHEN OTHERS.
        super->/iwbep/if_v4_dp_basic~update_entity( EXPORTING io_request  = io_request
                                                              io_response = io_response ).
    ENDCASE.
  ENDMETHOD.


  METHOD /iwbep/if_v4_dp_intermediate~patch_entity.
    DATA: lo_request_pro    TYPE REF TO /iwbep/cl_v4_request_info_pro,
          lo_request_base   TYPE REF TO /iwbep/if_v4_request_info,
          lt_req_headers    TYPE tihttpnvp,
          lv_str_length     TYPE i,
          lv_entityset_name TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name,
          lv_if_match_etag  TYPE string.

    CONSTANTS: BEGIN OF lc_header,
                 if_match TYPE string VALUE 'if-match',
               END OF lc_header.

    " Obtain the Request Headers
    " --NOT used in this project BUT this is how the headers are retrieved in case it is needed--
    lo_request_pro  ?= io_request.
    lo_request_base =  lo_request_pro->get_base_request_info( ).

    lo_request_base->get_http_headers( IMPORTING et_http_header = lt_req_headers                 " HTTP Framework (iHTTP) Table Name/Value Pairs
    ).

    " ---SPECIAL NOTE: The code below will grad header details (like IF tag). Uncomment for example
***    " SAP already took care of the If-Match checks for us so by this point this is valid. We
***    " just need the IF-Match value to store in the request processor
***    lv_if_match_etag = VALUE #( lt_req_headers[ name = lc_header-if_match ]-value OPTIONAL ).
***
***    IF lv_if_match_etag IS INITIAL.
***      RAISE EXCEPTION TYPE zcx_project_tracker
***        EXPORTING
***          textid           = zcx_project_tracker=>if_match_not_found
***          http_status_code = /iwbep/cx_gateway=>gcs_http_status_codes-conflict.
***    ENDIF.
***
***    IF lv_if_match_etag CS 'W/"'.
***      lv_str_length = strlen( lv_if_match_etag ).
***
***      lv_str_length    = lv_str_length - 4.
***      lv_if_match_etag = lv_if_match_etag+3(lv_str_length).
***    ENDIF.

    io_request->get_entity_type( IMPORTING ev_entity_type_name = lv_entityset_name ).

    CASE lv_entityset_name.
      WHEN zif_project_tracker_types~gc_entity_type_names-internal-workitem.
        patch_timeentry( EXPORTING io_request  = io_request            " V4 Request - intermediate entity patch
                                   io_response = io_response           " V4 Response - Intermediate Entity Patch
        ).

      WHEN OTHERS.
        super->/iwbep/if_v4_dp_intermediate~patch_entity( EXPORTING io_request  = io_request
                                                                    io_response = io_response ).
    ENDCASE.

  ENDMETHOD.


  METHOD create_timeentry.
    "entity type specific data types
    DATA: ls_timeentry    TYPE zif_project_tracker_types~gty_cds_views-timeentry,
          ls_db_timeentry TYPE zprjt_timeentry,
          lt_field_map    TYPE zif_project_tracker_types~gtt_edm_field_map.

    "generic data types
    DATA: ls_todo_list              TYPE /iwbep/if_v4_requ_basic_create=>ty_s_todo_list,
          ls_done_list              TYPE /iwbep/if_v4_requ_basic_create=>ty_s_todo_process_list,
          lo_message_container      TYPE REF TO /iwbep/if_v4_message_container,
          lv_names_of_missing_props TYPE string.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

    IF ls_todo_list-process-busi_data = abap_true.
      io_request->get_busi_data( IMPORTING es_busi_data = ls_timeentry ).

      DATA(lv_new_guid) = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ). "me->obtain_new_guid( ).
      ls_timeentry-guid = lv_new_guid.

*     --INSERT INTO DATABASE--
      zcl_project_tracker_mpc=>fieldmap_timeentry( IMPORTING et_field_map = lt_field_map ).
      fieldmap_to_db_conversion(
        EXPORTING
          it_field_map     = lt_field_map
          is_busi_data     = ls_timeentry
          iv_db_table_name = zif_project_tracker_types~gc_cds_to_db_tbl-timeentry
        IMPORTING
          es_db_tbl_row = ls_db_timeentry
      ).

      INSERT INTO zprjt_timeentry VALUES ls_db_timeentry.

      io_response->set_busi_data( is_busi_data = ls_timeentry ).
      ls_done_list-busi_data = abap_true. "business data processed
      ls_done_list-partial_busi_data = abap_true.
    ENDIF.

*   Report list of request options handled by application
    io_response->set_is_done( ls_done_list ).
  ENDMETHOD.


  METHOD create_workitems.
    "entity type specific data types
    DATA: ls_workitem    TYPE zif_project_tracker_types~gty_cds_views-workitem,
          ls_db_workitem TYPE zprjt_workitem,
          lt_field_map   TYPE zif_project_tracker_types~gtt_edm_field_map.

    "generic data types
    DATA: ls_todo_list              TYPE /iwbep/if_v4_requ_basic_create=>ty_s_todo_list,
          ls_done_list              TYPE /iwbep/if_v4_requ_basic_create=>ty_s_todo_process_list,
          lo_message_container      TYPE REF TO /iwbep/if_v4_message_container,
          lv_names_of_missing_props TYPE string.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

    IF ls_todo_list-process-busi_data = abap_true.
      io_request->get_busi_data( IMPORTING es_busi_data = ls_workitem ).

      DATA(lv_new_guid) = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ). "me->obtain_new_guid( ).
      ls_workitem-guid = lv_new_guid.

*     Set default fields
      ls_workitem-createddate = ls_workitem-modifieddate = me->obtain_current_timestamp( ).
      ls_workitem-createdby   = ls_workitem-modifiedby   = sy-uname.

*     --INSERT INTO DATABASE--
      zcl_project_tracker_mpc=>fieldmap_workitem( IMPORTING et_field_map = lt_field_map ).
      fieldmap_to_db_conversion(
        EXPORTING
          it_field_map     = lt_field_map
          is_busi_data     = ls_workitem
          iv_db_table_name = zif_project_tracker_types~gc_cds_to_db_tbl-workitem
        IMPORTING
          es_db_tbl_row = ls_db_workitem
      ).

      INSERT INTO zprjt_workitem VALUES ls_db_workitem.

      io_response->set_busi_data( is_busi_data = ls_workitem ).
      ls_done_list-busi_data = abap_true. "business data processed
      ls_done_list-partial_busi_data = abap_true.
    ENDIF.

*   Report list of request options handled by application
    io_response->set_is_done( ls_done_list ).
  ENDMETHOD.


  METHOD delete_timeentry.
    DATA: ls_key_timeentry TYPE zif_project_tracker_types~gty_cds_views-timeentry,
          ls_db_reqproc    TYPE zprjt_timeentry,
          lt_field_map     TYPE zif_project_tracker_types~gtt_edm_field_map.

    "generic data types
    DATA: ls_todo_list              TYPE /iwbep/if_v4_requ_basic_delete=>ty_s_todo_list,
          ls_done_list              TYPE /iwbep/if_v4_requ_basic_delete=>ty_s_todo_process_list,
          lo_message_container      TYPE REF TO /iwbep/if_v4_message_container,
          lv_names_of_missing_props TYPE string.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

    " read the key data
    io_request->get_key_data( IMPORTING es_key_data = ls_key_timeentry ).
    ls_done_list-key_data = abap_true.

*   TODO: Set and Ensure Locking is done!

*   Delete the record
    DELETE FROM zprjt_timeentry WHERE guid = ls_key_timeentry-guid.

    io_response->set_is_done( ls_done_list ).
  ENDMETHOD.


  METHOD delete_workitems.
    DATA: ls_key_workitem TYPE zif_project_tracker_types~gty_cds_views-workitem,
          ls_db_reqproc   TYPE zprjt_workitem,
          lt_field_map    TYPE zif_project_tracker_types~gtt_edm_field_map.

    "generic data types
    DATA: ls_todo_list              TYPE /iwbep/if_v4_requ_basic_delete=>ty_s_todo_list,
          ls_done_list              TYPE /iwbep/if_v4_requ_basic_delete=>ty_s_todo_process_list,
          lo_message_container      TYPE REF TO /iwbep/if_v4_message_container,
          lv_names_of_missing_props TYPE string.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

    " read the key data
    io_request->get_key_data( IMPORTING es_key_data = ls_key_workitem ).
    ls_done_list-key_data = abap_true.

*   TODO: Set and Ensure Locking is done!

*   Delete the record
    DELETE FROM zprjt_workitem WHERE guid = ls_key_workitem-guid.

    io_response->set_is_done( ls_done_list ).
  ENDMETHOD.


  METHOD fieldmap_to_db_conversion.
    DATA: lo_table         TYPE REF TO data,
          lv_busi_data_col TYPE string.
    FIELD-SYMBOLS: <ls_db_field>      TYPE any,
                   <ls_table_str>     TYPE any, "table,
                   <lv_busi_data_col> TYPE any,
                   <lv_db_col>        TYPE any.

    CREATE DATA lo_table TYPE (iv_db_table_name).
    ASSIGN lo_table->* TO <ls_table_str>.

    LOOP AT it_field_map ASSIGNING FIELD-SYMBOL(<ls_field_map>) WHERE db_field IS NOT INITIAL.

*     We need to read the busi data (the CDS) value
      lv_busi_data_col = |is_busi_data-{ <ls_field_map>-edm_name }|.
      ASSIGN (lv_busi_data_col) TO <lv_busi_data_col>.

*     Now lets set the column of the DB structure
      lv_busi_data_col = |<ls_table_str>-{ <ls_field_map>-db_field }|.
      ASSIGN (lv_busi_data_col) TO <lv_db_col>.

*     To Conclude, we will set the value from the EDM to the corresponding DB Column
      <lv_db_col> = <lv_busi_data_col>.

      UNASSIGN: <lv_busi_data_col>, <lv_db_col>.
      CLEAR: lv_busi_data_col.
    ENDLOOP.

    es_db_tbl_row = <ls_table_str>.
  ENDMETHOD.


  METHOD obtain_current_timestamp.
    GET TIME STAMP FIELD rv_current_ts.
  ENDMETHOD.


  METHOD obtain_new_guid.
    DATA: lv_random        TYPE xstring,
          lv_access_guid_x TYPE xstring.

    DATA(lv_new_uuid) = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).

*   We now want to generate a new random number
    CALL FUNCTION 'GENERATE_SEC_RANDOM'
      EXPORTING
        length         = 8
      IMPORTING
        random         = lv_random
      EXCEPTIONS
        invalid_length = 1
        no_memory      = 2
        internal_error = 3
        OTHERS         = 4.

    IF sy-subrc <> 0.
      RAISE ex_guid_exception.
    ENDIF.

    CONCATENATE lv_new_uuid lv_random INTO lv_access_guid_x IN BYTE MODE.

    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING
        bindata                  = lv_access_guid_x
      IMPORTING
        b64data                  = rv_new_guid
      EXCEPTIONS
        ssf_krn_error            = 1
        ssf_krn_noop             = 2
        ssf_krn_nomemory         = 3
        ssf_krn_opinv            = 4
        ssf_krn_input_data_error = 5
        ssf_krn_invalid_par      = 6
        ssf_krn_invalid_parlen   = 7
        OTHERS                   = 8.
    IF sy-subrc <> 0.
      RAISE ex_guid_exception.
    ENDIF.

    REPLACE ALL OCCURRENCES OF '+' IN rv_new_guid WITH '-'.
    REPLACE ALL OCCURRENCES OF '/' IN rv_new_guid WITH '_'.
  ENDMETHOD.


  METHOD patch_timeentry.
    DATA: ls_todo_list           TYPE /iwbep/if_v4_requ_basic_update=>ty_s_todo_list,
          ls_done_list           TYPE /iwbep/if_v4_requ_basic_update=>ty_s_todo_process_list,
          lt_provided_properties TYPE STANDARD TABLE OF /iwbep/if_v4_runtime_types=>ty_property_path  WITH NON-UNIQUE KEY table_line,
          lt_field_map           TYPE zif_project_tracker_types~gtt_edm_field_map,
          lt_return_msg          TYPE bapiret2_t,
          lo_message_container   TYPE REF TO /iwbep/if_v4_message_container.

    DATA: ls_db_timeentry  TYPE zprjt_timeentry,
          ls_timeentry     TYPE zif_project_tracker_types~gty_cds_views-timeentry,
          ls_timeentry_out TYPE zif_project_tracker_types~gty_cds_views-timeentry,
          ls_timeentry_key TYPE zif_project_tracker_types~gty_cds_views-timeentry.

    "Log object
    lo_message_container = io_response->get_message_container( ).

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

    IF ls_todo_list-process-busi_data = abap_true.
      io_request->get_busi_data( IMPORTING es_busi_data = ls_timeentry ).
      ls_done_list-busi_data = abap_true.
    ENDIF.

    io_request->get_key_data( IMPORTING es_key_data = ls_timeentry_key ).
    ls_done_list-key_data = abap_true. "key data processed

    "don't update the keys - Safety check to ensure the entity didn't add additional key changes
    ls_timeentry-guid = ls_timeentry_key-guid.


    "Read the parameter list provided by the request body
    IF ( ls_todo_list-process-partial_busi_data = abap_true ).
      ls_done_list-partial_busi_data = abap_true. "Partial Busi data considered
    ELSE.
      lo_message_container->add_t100( EXPORTING iv_msg_type   = zif_project_tracker_types=>gc_message_types-error_e       " Message Type
                                                iv_msg_id     = zif_project_tracker_types=>gc_message_class               " Message Class
                                                iv_msg_number = zcx_project_tracker=>cannot_update_all_flds-msgno         " Message Number
      ).
      RAISE EXCEPTION TYPE zcx_project_tracker
        EXPORTING
          message_container = lo_message_container
          http_status_code  = /iwbep/cx_gateway=>gcs_http_status_codes-bad_request.

    ENDIF.


    "--------------------------------------------------------------------------------------------
    " OData 304 Check - No Change Check
    "--------------------------------------------------------------------------------------------
    io_request->get_provided_properties( IMPORTING et_provided_property_path = lt_provided_properties      " Provided properties (path representation)
    ).

    SELECT SINGLE *
           FROM zcds_prjtrk_timeentry
          WHERE guid     = @ls_timeentry-guid
           INTO @ls_timeentry_out.
    IF sy-subrc <> 0.

      lo_message_container->add_t100( EXPORTING iv_msg_type    = zif_project_tracker_types=>gc_message_types-error_e       " Message Type
                                                iv_msg_id      = zcx_project_tracker=>record_missing-msgid                 " Message Class
                                                iv_msg_number  = zcx_project_tracker=>record_missing-msgno                 " Message Number
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_gateway
        EXPORTING
          message_container = lo_message_container
          http_status_code  = /iwbep/cx_gateway=>gcs_http_status_codes-sv_internal_server_error.
    ENDIF.

    zcl_bagalay_utility=>check_and_confirm_changes( EXPORTING is_payload_data        = ls_timeentry               " What the User requested
                                                              is_ecc_data            = ls_timeentry_out           " Data currently in ECC
                                                              it_selected_properties = lt_provided_properties     " List of properties / property paths
                                                    IMPORTING ev_no_change_detected  = DATA(lv_no_changes_detected)
    ).

    IF lv_no_changes_detected = abap_true.
      " **SPECIAL NOTE: HTTP STATUS 304 THROWS A HARD ERROR BACK TO POSTMAN (NO BODY)
      "     Someone with more time will need to investigate why
      RAISE EXCEPTION TYPE zcx_project_tracker
        EXPORTING
          textid           = zcx_project_tracker=>no_change_detected
          http_status_code = /iwbep/cx_gateway=>gcs_http_status_codes-conflict.
    ENDIF.

    "--------------------------------------------------------------------------------------------
    " VALIDATE
    "--------------------------------------------------------------------------------------------
    "Defaults
    lt_return_msg = validation_standard( EXPORTING it_field_map          = lt_field_map
                                                   is_busi_data          = ls_timeentry
                                                   is_original_busi_data = ls_timeentry_out
                                                   iv_update_entity      = abap_true ).

    IF line_exists( lt_return_msg[ type   = zif_project_tracker_types~gc_message_types-error_e
                                   number = zif_project_tracker_types~gc_message_nbr_ref-cannot_modify_key ] ).
      RAISE EXCEPTION TYPE zcx_project_tracker
        EXPORTING
          textid              = zcx_project_tracker=>cannot_modify_key
          http_status_code    = zcx_project_tracker=>gcs_http_status_codes-forbidden
          edm_entity_set_name = zif_project_tracker_types~gc_entity_set_names-edm-timeentryset
          entity_key          = | { ls_timeentry_key-guid } |.
    ENDIF.

    IF line_exists( lt_return_msg[ type   = zif_project_tracker_types~gc_message_types-error_e
                                   number = zif_project_tracker_types~gc_message_nbr_ref-change_date_forbidden ] ).
      RAISE EXCEPTION TYPE zcx_project_tracker
        EXPORTING
          textid           = zcx_project_tracker=>change_date_forbidden
          http_status_code = zcx_project_tracker=>gcs_http_status_codes-forbidden.
    ENDIF.

    "--------------------------------------------------------------------------------------------
    " CONVERSION  AND  COMMIT
    "--------------------------------------------------------------------------------------------
    zcl_project_tracker_mpc=>fieldmap_timeentry( IMPORTING et_field_map = lt_field_map ).

    fieldmap_to_db_conversion( EXPORTING it_field_map     = lt_field_map
                                         is_busi_data     = ls_timeentry
                                         iv_db_table_name = zif_project_tracker_types~gc_cds_to_db_tbl-timeentry
                               IMPORTING es_db_tbl_row    = ls_db_timeentry ).

    MODIFY zprjt_timeentry FROM ls_db_timeentry.

    io_response->set_busi_data( ls_timeentry ).
    io_response->set_is_done( ls_done_list ).
  ENDMETHOD.


  method READ_ENTITY_TIMEENTRY.
    DATA: ls_timeentry         TYPE zif_project_tracker_types=>gty_cds_views-timeentry,
          ls_todo_list         TYPE /iwbep/if_v4_requ_basic_read=>ty_s_todo_list,
          ls_done_list         TYPE /iwbep/if_v4_requ_basic_read=>ty_s_todo_process_list,
          ls_key_timeentry     TYPE zif_project_tracker_types=>gty_cds_views-timeentry,
          lt_selected_property TYPE /iwbep/if_v4_runtime_types=>ty_t_property_path,
          lv_select_string     TYPE string.

*   Get the request options the application should/must handle
    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

*   READ the key data
    io_request->get_key_data( IMPORTING es_key_data = ls_key_timeentry ).
    ls_done_list-key_data = abap_true.

*   SELECT list
    IF ls_todo_list-process-select = abap_true.
      ls_done_list-select = abap_true.
      io_request->get_selected_properties(  IMPORTING et_selected_property = lt_selected_property ).
      CONCATENATE LINES OF lt_selected_property INTO lv_select_string  SEPARATED BY ','.
    ELSE.
      lv_select_string = '*'.
    ENDIF.

*   READ
    SELECT SINGLE (lv_select_string)
      FROM zcds_prjtrk_timeentry
      INTO @ls_timeentry
     WHERE guid = @ls_key_timeentry-guid.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_project_tracker
        EXPORTING
          textid              = zcx_project_tracker=>entity_not_found
          http_status_code    = zcx_project_tracker=>gcs_http_status_codes-not_found
          edm_entity_set_name = zif_project_tracker_types~gc_entity_set_names-edm-timeentryset
          entity_key          = | { ls_key_timeentry-guid } |.
    ENDIF.

    io_response->set_busi_data( is_busi_data = ls_timeentry ).
    io_response->set_is_done( ls_done_list ).
  endmethod.


  METHOD read_entity_workitems.
    DATA: ls_workitem          TYPE zif_project_tracker_types=>gty_cds_views-workitem,
          ls_todo_list         TYPE /iwbep/if_v4_requ_basic_read=>ty_s_todo_list,
          ls_done_list         TYPE /iwbep/if_v4_requ_basic_read=>ty_s_todo_process_list,
          ls_key_workitem      TYPE zif_project_tracker_types=>gty_cds_views-workitem,
          lt_selected_property TYPE /iwbep/if_v4_runtime_types=>ty_t_property_path,
          lv_select_string     TYPE string.

*   Get the request options the application should/must handle
    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

*   READ the key data
    io_request->get_key_data( IMPORTING es_key_data = ls_key_workitem ).
    ls_done_list-key_data = abap_true.

*   SELECT list
    IF ls_todo_list-process-select = abap_true.
      ls_done_list-select = abap_true.
      io_request->get_selected_properties(  IMPORTING et_selected_property = lt_selected_property ).
      CONCATENATE LINES OF lt_selected_property INTO lv_select_string  SEPARATED BY ','.
    ELSE.
      lv_select_string = '*'.
    ENDIF.

*   READ
    SELECT SINGLE (lv_select_string)
      FROM zcds_prjtrk_workitem
      INTO @ls_workitem
     WHERE guid = @ls_key_workitem-guid.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_project_tracker
        EXPORTING
          textid              = zcx_project_tracker=>entity_not_found
          http_status_code    = zcx_project_tracker=>gcs_http_status_codes-not_found
          edm_entity_set_name = zif_project_tracker_types~gc_entity_set_names-edm-workitemset
          entity_key          = | { ls_key_workitem-guid } |.
    ENDIF.

    io_response->set_busi_data( is_busi_data = ls_workitem ).
    io_response->set_is_done( ls_done_list ).

  ENDMETHOD.


  METHOD read_list_timeentry.
    DATA: lt_timeentry           TYPE STANDARD TABLE OF zif_project_tracker_types~gty_cds_views-timeentry,
          ls_todo_list           TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_list,
          ls_done_list           TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_process_list,
          lt_key_timeentry       TYPE STANDARD TABLE OF zif_project_tracker_types=>gty_cds_views-timeentry,
          lt_key_range_timeentry TYPE RANGE OF zif_project_tracker_types=>gty_cds_views-timeentry-guid,
          lt_selected_property   TYPE /iwbep/if_v4_runtime_types=>ty_t_property_path,
          lv_select_string       TYPE string,
          lv_skip_token          TYPE string,
          lv_count               TYPE i.

*   Get the request options the application should/must handle
    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).
    ls_done_list = is_done_list.

*   READ the key data
    IF ls_todo_list-process-key_data = abap_true.
      io_request->get_key_data( IMPORTING et_key_data = lt_key_timeentry ).

      lt_key_range_timeentry = VALUE #( FOR <ls_key_timeentry> IN lt_key_timeentry
                                     ( sign   = zif_project_tracker_types~gc_range_tbl-sign-include
                                       option = zif_project_tracker_types~gc_range_tbl-option-equal_to
                                       low    = <ls_key_timeentry>-guid ) ).

      ls_done_list-key_data = abap_true.
    ENDIF.

*   Business Data
    IF ls_todo_list-return-busi_data = abap_true.

      SELECT (iv_select_string)
        FROM zcds_prjtrk_timeentry
       WHERE (iv_where_clause)
        AND guid IN @lt_key_range_timeentry
        ORDER BY (iv_orderby_string)
       INTO CORRESPONDING FIELDS OF TABLE @lt_timeentry
        OFFSET @iv_skip
        UP TO @iv_top ROWS.

*     SERVER SIDE PAGING
      IF lines( lt_timeentry )      = zif_project_tracker_types~gc_max_responses OR
       ( lines( lt_timeentry )     >= iv_top AND
         ls_todo_list-process-skip_token = abap_true ).
        lv_skip_token = |{ ( iv_skip + iv_top ) }|.
        io_response->set_skip_token( iv_skip_token = lv_skip_token ).
      ENDIF.

      io_response->set_busi_data( it_busi_data = lt_timeentry ).
    ENDIF.

*   **COUNT: The count should be based on the $filter condition, not the amount
*       returned. Ex: If a filter condition had 72 records but the user asked to
*       show the top 5, when you use $count, it should be 72, NOT 5. This practice
*       is already done by Microsoft Web Services
    IF ls_todo_list-return-count = abap_true.

      SELECT COUNT( * )
        FROM zcds_prjtrk_timeentry
          WHERE (iv_where_clause)
          AND guid IN @lt_key_range_timeentry
            INTO @lv_count.

      io_response->set_count( lv_count ).
    ENDIF.


*   Report list of request options handled by application
    io_response->set_is_done( ls_done_list ).
  ENDMETHOD.


  METHOD read_list_workitems.
    DATA: lt_workitem           TYPE STANDARD TABLE OF zif_project_tracker_types~gty_cds_views-workitem,
          ls_todo_list          TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_list,
          ls_done_list          TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_process_list,
          lt_key_worktem        TYPE STANDARD TABLE OF zif_project_tracker_types=>gty_cds_views-workitem,
          lt_key_range_workitem TYPE RANGE OF zif_project_tracker_types=>gty_cds_views-workitem-guid,
          lt_selected_property  TYPE /iwbep/if_v4_runtime_types=>ty_t_property_path,
          lv_select_string      TYPE string,
          lv_skip_token         TYPE string,
          lv_count              TYPE i.

*   Get the request options the application should/must handle
    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).
    ls_done_list = is_done_list.

*   READ the key data
    IF ls_todo_list-process-key_data = abap_true.
      io_request->get_key_data( IMPORTING et_key_data = lt_key_worktem ).

      lt_key_range_workitem = VALUE #( FOR <ls_key_workitem> IN lt_key_worktem
                                     ( sign   = zif_project_tracker_types~gc_range_tbl-sign-include
                                       option = zif_project_tracker_types~gc_range_tbl-option-equal_to
                                       low    = <ls_key_workitem>-guid ) ).

      ls_done_list-key_data = abap_true.
    ENDIF.

*   Business Data
    IF ls_todo_list-return-busi_data = abap_true.

      SELECT (iv_select_string)
        FROM zcds_prjtrk_workitem
       WHERE (iv_where_clause)
        AND guid IN @lt_key_range_workitem
        ORDER BY (iv_orderby_string)
       INTO CORRESPONDING FIELDS OF TABLE @lt_workitem
        OFFSET @iv_skip
        UP TO @iv_top ROWS.

*     SERVER SIDE PAGING
      IF lines( lt_workitem )      = zif_project_tracker_types~gc_max_responses OR
       ( lines( lt_workitem )     >= iv_top AND
         ls_todo_list-process-skip_token = abap_true ).
        lv_skip_token = |{ ( iv_skip + iv_top ) }|.
        io_response->set_skip_token( iv_skip_token = lv_skip_token ).
      ENDIF.

      io_response->set_busi_data( it_busi_data = lt_workitem ).
    ENDIF.

*   **COUNT: The count should be based on the $filter condition, not the amount
*       returned. Ex: If a filter condition had 72 records but the user asked to
*       show the top 5, when you use $count, it should be 72, NOT 5. This practice
*       is already done by Microsoft Web Services
    IF ls_todo_list-return-count = abap_true.

      SELECT COUNT( * )
        FROM zcds_prjtrk_workitem
          WHERE (iv_where_clause)
          AND guid IN @lt_key_range_workitem
            INTO @lv_count.

      io_response->set_count( lv_count ).
    ENDIF.

*   Report list of request options handled by application
    io_response->set_is_done( ls_done_list ).

  ENDMETHOD.


  METHOD read_ref_key_list_workitem.
    DATA: ls_done_list         TYPE /iwbep/if_v4_requ_basic_ref_l=>ty_s_todo_process_list,
          ls_todo_list         TYPE /iwbep/if_v4_requ_basic_ref_l=>ty_s_todo_list,
          lv_nav_property_name TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name,
          lt_accounts          TYPE STANDARD TABLE OF zif_account_details_types=>gty_cds_views-account,
          lt_timeentry         TYPE STANDARD TABLE OF zif_project_tracker_types~gty_cds_views-timeentry,
          ls_workitem_key      TYPE zif_project_tracker_types~gty_cds_views-workitem.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

    IF ls_todo_list-process-source_key_data = abap_true.
      io_request->get_source_key_data( IMPORTING es_source_key_data =  ls_workitem_key ).
      ls_done_list-source_key_data = abap_true.
    ENDIF.

    io_request->get_navigation_prop( IMPORTING ev_navigation_prop_name = lv_nav_property_name ).

    CASE lv_nav_property_name.
      WHEN zif_project_tracker_types~gc_nav_prop_names-internal-workitem_to_account.

        SELECT accountcontact AS accountguid
          FROM zcds_prjtrk_workitem
          INTO CORRESPONDING FIELDS OF TABLE @lt_accounts
          WHERE guid     = @ls_workitem_key-guid.

        DELETE lt_accounts WHERE accountguid IS INITIAL.
        io_response->set_target_key_data( lt_accounts ).

      WHEN zif_project_tracker_types~gc_nav_prop_names-internal-workitem_to_timeentry.
        SELECT guid
          FROM zcds_prjtrk_timeentry
          INTO CORRESPONDING FIELDS OF TABLE @lt_timeentry
          WHERE workitemguid = @ls_workitem_key-guid.

        io_response->set_target_key_data( lt_timeentry ).

    ENDCASE.

    io_response->set_is_done( ls_done_list ).
  ENDMETHOD.


  METHOD read_ref_key_timeentry.
    DATA: ls_done_list         TYPE /iwbep/if_v4_requ_basic_ref_l=>ty_s_todo_process_list,
          ls_todo_list         TYPE /iwbep/if_v4_requ_basic_ref_l=>ty_s_todo_list,
          lv_nav_property_name TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name,
          ls_timeentry_key     TYPE zif_project_tracker_types~gty_cds_views-timeentry,
          ls_workitem          TYPE zif_project_tracker_types~gty_cds_views-workitem.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).

    IF ls_todo_list-process-source_key_data = abap_true.
      io_request->get_source_key_data( IMPORTING es_source_key_data =  ls_timeentry_key ).
      ls_done_list-source_key_data = abap_true.
    ENDIF.

    io_request->get_navigation_prop( IMPORTING ev_navigation_prop_name = lv_nav_property_name ).

    CASE lv_nav_property_name.
      WHEN zif_project_tracker_types~gc_nav_prop_names-internal-timeentry_to_workitem.

        SELECT SINGLE workitemguid AS guid
          FROM zcds_prjtrk_timeentry
          INTO CORRESPONDING FIELDS OF @ls_workitem
          WHERE guid = @ls_timeentry_key-guid.

        io_response->set_target_key_data( ls_workitem ).
    ENDCASE.

    io_response->set_is_done( ls_done_list ).
  ENDMETHOD.


  method UPDATE_TIMEENTRY.
    "entity type specific data types
    DATA: ls_timeentry      TYPE zif_project_tracker_types~gty_cds_views-timeentry,
          ls_orig_timeentry TYPE zif_project_tracker_types~gty_cds_views-timeentry,
          ls_db_timeentry   TYPE zprjt_timeentry,
          lt_field_map     TYPE zif_project_tracker_types~gtt_edm_field_map,
          lt_return_msg    TYPE bapiret2_t,
          ls_key_timeentry  TYPE zif_project_tracker_types~gty_cds_views-timeentry,
          lv_helper_int    TYPE i.

    "generic data types
    DATA: ls_todo_list         TYPE /iwbep/if_v4_requ_basic_update=>ty_s_todo_list,
          ls_done_list         TYPE /iwbep/if_v4_requ_basic_update=>ty_s_todo_process_list,
          lo_message_container TYPE REF TO /iwbep/if_v4_message_container.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).
    zcl_project_tracker_mpc=>fieldmap_timeentry( IMPORTING et_field_map = lt_field_map ).

*   Obtain Key Information
    io_request->get_key_data( IMPORTING es_key_data = ls_key_timeentry ).   " Structure with the entity's key fields filled

*   Get original table Record
    SELECT SINGLE *
            FROM zprjt_timeentry
            INTO CORRESPONDING FIELDS OF @ls_orig_timeentry
            WHERE guid = @ls_key_timeentry-guid.

    IF ls_todo_list-process-busi_data = abap_true.
      io_request->get_busi_data( IMPORTING es_busi_data = ls_timeentry ).

*     --VALIDATION CHECK--
*     Defaults
      lt_return_msg = validation_standard( EXPORTING it_field_map          = lt_field_map
                                                     is_busi_data          = ls_timeentry
                                                     is_original_busi_data = ls_orig_timeentry
                                                     iv_update_entity      = abap_true ).

      IF line_exists( lt_return_msg[ type   = zif_project_tracker_types~gc_message_types-error_e
                                     number = zif_project_tracker_types~gc_message_nbr_ref-cannot_modify_key ] ).
        RAISE EXCEPTION TYPE zcx_project_tracker
          EXPORTING
            textid              = zcx_project_tracker=>cannot_modify_key
            http_status_code    = zcx_project_tracker=>gcs_http_status_codes-forbidden
            edm_entity_set_name = zif_project_tracker_types~gc_entity_set_names-edm-timeentryset
            entity_key          = | { ls_key_timeentry-guid } |.
      ENDIF.

      IF line_exists( lt_return_msg[ type   = zif_project_tracker_types~gc_message_types-error_e
                                     number = zif_project_tracker_types~gc_message_nbr_ref-change_date_forbidden ] ).
        RAISE EXCEPTION TYPE zcx_project_tracker
          EXPORTING
            textid           = zcx_project_tracker=>change_date_forbidden
            http_status_code = zcx_project_tracker=>gcs_http_status_codes-forbidden.
      ENDIF.

*     --UPDATE DATABASE--
      fieldmap_to_db_conversion( EXPORTING it_field_map     = lt_field_map
                                           is_busi_data     = ls_timeentry
                                           iv_db_table_name = zif_project_tracker_types~gc_cds_to_db_tbl-timeentry
                                 IMPORTING es_db_tbl_row    = ls_db_timeentry ).

      MODIFY zprjt_timeentry FROM ls_db_timeentry.

      ls_done_list-busi_data = abap_true. "business data processed
      ls_done_list-key_data  = abap_true.
    ENDIF.

    IF ls_todo_list-return-busi_data = abap_true.
      SELECT SINGLE *
        FROM zprjt_timeentry
        INTO CORRESPONDING FIELDS OF @ls_timeentry
        WHERE guid = @ls_key_timeentry-guid.
      io_response->set_busi_data( is_busi_data = ls_timeentry ).

    ENDIF.

    io_response->set_is_done( ls_done_list ).
  endmethod.


  METHOD update_workitems.
    "entity type specific data types
    DATA: ls_workitem      TYPE zif_project_tracker_types~gty_cds_views-workitem,
          ls_orig_workitem TYPE zif_project_tracker_types~gty_cds_views-workitem,
          ls_db_workitem   TYPE zprjt_workitem,
          lt_field_map     TYPE zif_project_tracker_types~gtt_edm_field_map,
          lt_return_msg    TYPE bapiret2_t,
          ls_key_workitem  TYPE zif_project_tracker_types~gty_cds_views-workitem,
          lv_helper_int    TYPE i.

    "generic data types
    DATA: ls_todo_list         TYPE /iwbep/if_v4_requ_basic_update=>ty_s_todo_list,
          ls_done_list         TYPE /iwbep/if_v4_requ_basic_update=>ty_s_todo_process_list,
          lo_message_container TYPE REF TO /iwbep/if_v4_message_container.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).
    zcl_project_tracker_mpc=>fieldmap_workitem( IMPORTING et_field_map = lt_field_map ).

*   Obtain Key Information
    io_request->get_key_data( IMPORTING es_key_data = ls_key_workitem ).   " Structure with the entity's key fields filled

*   Get original table Record
    SELECT SINGLE *
            FROM zprjt_workitem
            INTO CORRESPONDING FIELDS OF @ls_orig_workitem
            WHERE guid = @ls_key_workitem-guid.

    IF ls_todo_list-process-busi_data = abap_true.
      io_request->get_busi_data( IMPORTING es_busi_data = ls_workitem ).

*     --VALIDATION CHECK--
*     Defaults
      lt_return_msg = validation_standard( EXPORTING it_field_map          = lt_field_map
                                                     is_busi_data          = ls_workitem
                                                     is_original_busi_data = ls_orig_workitem
                                                     iv_update_entity      = abap_true ).

      IF line_exists( lt_return_msg[ type   = zif_project_tracker_types~gc_message_types-error_e
                                     number = zif_project_tracker_types~gc_message_nbr_ref-cannot_modify_key ] ).
        RAISE EXCEPTION TYPE zcx_project_tracker
          EXPORTING
            textid              = zcx_project_tracker=>cannot_modify_key
            http_status_code    = zcx_project_tracker=>gcs_http_status_codes-forbidden
            edm_entity_set_name = zif_project_tracker_types~gc_entity_set_names-edm-workitemset
            entity_key          = | { ls_key_workitem-guid } |.
      ENDIF.

      IF line_exists( lt_return_msg[ type   = zif_project_tracker_types~gc_message_types-error_e
                                     number = zif_project_tracker_types~gc_message_nbr_ref-change_date_forbidden ] ).
        RAISE EXCEPTION TYPE zcx_project_tracker
          EXPORTING
            textid           = zcx_project_tracker=>change_date_forbidden
            http_status_code = zcx_project_tracker=>gcs_http_status_codes-forbidden.
      ENDIF.

*     --UPDATE DATABASE--
*     Set default fields
      ls_workitem-modifieddate = me->obtain_current_timestamp( ).
      ls_workitem-modifiedby   = sy-uname.

      fieldmap_to_db_conversion( EXPORTING it_field_map     = lt_field_map
                                           is_busi_data     = ls_workitem
                                           iv_db_table_name = zif_project_tracker_types~gc_cds_to_db_tbl-workitem
                                 IMPORTING es_db_tbl_row    = ls_db_workitem ).

      MODIFY zprjt_workitem FROM ls_db_workitem.

      ls_done_list-busi_data = abap_true. "business data processed
      ls_done_list-key_data  = abap_true.
    ENDIF.

    IF ls_todo_list-return-busi_data = abap_true.
      SELECT SINGLE *
        FROM zprjt_workitem
        INTO CORRESPONDING FIELDS OF @ls_workitem
        WHERE guid = @ls_key_workitem-guid.
      io_response->set_busi_data( is_busi_data = ls_workitem ).

    ENDIF.

    io_response->set_is_done( ls_done_list ).
  ENDMETHOD.


  METHOD validation_standard.
    DATA : lo_abap_struc  TYPE REF TO cl_abap_structdescr,
           lt_components  TYPE abap_compdescr_tab,
           lt_return_msg  TYPE bapiret2_t,
           lv_column_name TYPE string.
    FIELD-SYMBOLS: <lv_orig_column> TYPE any,
                   <lv_column>      TYPE any.

**    lo_abap_struc ?= cl_abap_typedescr=>describe_by_data( is_busi_data ).
**    lt_components = lo_abap_struc->components.

    LOOP AT it_field_map ASSIGNING FIELD-SYMBOL(<ls_field_map>).

      lv_column_name = |IS_BUSI_DATA-{ <ls_field_map>-edm_name }|.
      ASSIGN (lv_column_name) TO <lv_column>.

      lv_column_name = |IS_ORIGINAL_BUSI_DATA-{ <ls_field_map>-edm_name }|.
      ASSIGN (lv_column_name) TO <lv_orig_column>.

*       --VALIDATION CHECKS--
*       Check for Key Change
      IF <ls_field_map>-key = abap_true AND
         <lv_column> <> <lv_orig_column>.
        lt_return_msg = VALUE #( BASE lt_return_msg
                               ( type    = zif_project_tracker_types~gc_message_types-error_e
                                 id      = zif_project_tracker_types~gc_message_class
                                 number  = zif_project_tracker_types~gc_message_nbr_ref-cannot_modify_key ) ).
      ENDIF.

*       Check for Date Fields (UPDATE ONLY CHECK)
      IF ( <ls_field_map>-db_field = |CREATED_DATE| ) AND
         <lv_column> <> <lv_orig_column> AND
         iv_update_entity = abap_true.
        lt_return_msg = VALUE #( BASE lt_return_msg
                               ( type    = zif_project_tracker_types~gc_message_types-error_e
                                 id      = zif_project_tracker_types~gc_message_class
                                 number  = zif_project_tracker_types~gc_message_nbr_ref-change_date_forbidden ) ).
      ENDIF.

    ENDLOOP.

    rt_return_msg = lt_return_msg.
  ENDMETHOD.
ENDCLASS.