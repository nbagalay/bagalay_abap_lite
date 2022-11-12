class ZCL_PROJECT_TRACKER_MPC definition
  public
  inheriting from /IWBEP/CL_V4_ABS_MODEL_PROV
  final
  create public .

public section.

  interfaces ZIF_PROJECT_TRACKER_TYPES .

  class-methods FIELDMAP_WORKITEM
    exporting
      !ET_FIELD_MAP type ZIF_PROJECT_TRACKER_TYPES~GTT_EDM_FIELD_MAP .
  class-methods FIELDMAP_TIMEENTRY
    exporting
      !ET_FIELD_MAP type ZIF_PROJECT_TRACKER_TYPES~GTT_EDM_FIELD_MAP .

  methods /IWBEP/IF_V4_MP_BASIC~DEFINE
    redefinition .
protected section.
private section.

  aliases GC_ENTITY_SET_NAMES
    for ZIF_PROJECT_TRACKER_TYPES~GC_ENTITY_SET_NAMES .
  aliases GC_ENTITY_TYPE_NAMES
    for ZIF_PROJECT_TRACKER_TYPES~GC_ENTITY_TYPE_NAMES .
  aliases GC_NAV_PROP_NAMES
    for ZIF_PROJECT_TRACKER_TYPES~GC_NAV_PROP_NAMES .
  aliases GC_SERVICE_REFERENCE
    for ZIF_PROJECT_TRACKER_TYPES~GC_SERVICE_REFERENCE .

  methods DEFINE_WORKITEM
    importing
      !IO_MODEL type ref to /IWBEP/IF_V4_MED_MODEL
      !IO_MODEL_INFO type ref to /IWBEP/IF_V4_MODEL_INFO
    raising
      /IWBEP/CX_GATEWAY .
  methods DEFINE_SERVICE_REFERENCES
    importing
      !IO_MODEL type ref to /IWBEP/IF_V4_MED_MODEL
    raising
      /IWBEP/CX_V4_MED .
  methods DEFINE_TIMEENTRY
    importing
      !IO_MODEL type ref to /IWBEP/IF_V4_MED_MODEL
      !IO_MODEL_INFO type ref to /IWBEP/IF_V4_MODEL_INFO
    raising
      /IWBEP/CX_GATEWAY .
ENDCLASS.



CLASS ZCL_PROJECT_TRACKER_MPC IMPLEMENTATION.


  METHOD /iwbep/if_v4_mp_basic~define.
    define_service_references( io_model = io_model ).

    define_workitem(  io_model = io_model io_model_info = io_model_info ).
    define_timeentry( io_model = io_model io_model_info = io_model_info ).

**TRY.
*CALL METHOD SUPER->/IWBEP/IF_V4_MP_BASIC~DEFINE
*  EXPORTING
*    IO_MODEL      =
*    IO_MODEL_INFO =
*    .
** CATCH /iwbep/cx_gateway .
**ENDTRY.
  ENDMETHOD.


  method DEFINE_SERVICE_REFERENCES.
*   External service used


**  ---IMPORTANT NOTE FOR ECC 7.51 SP02 USERS (aka current CAL instance users)-----
*   Cross service ref does work if you do a ENTITY call by key with expand (Ex: ./WorkItemSet('{your key here'})?$expand=_ACCOUNTSET
*   HOWEVER, if you do a wide open LIST call (so not by key) you'll get an error:
*   "message": "Not implemented - The request cannot be processed; Implement '/IWBEP/IF_V4_DP_BASIC~READ_ENTITY_LIST'",
*   This is an error with the current SP we are on. I verified on a system that was on 7.50 SP16 and worked just fine.


    io_model->create_service_reference( iv_service_ref_name = gc_service_reference-account_details-service_ref_name
                                        is_service_key      = VALUE #( service_id      = gc_service_reference-account_details-service_id
                                                                       service_version = 0001
                                                                       repository_id   = gc_service_reference-account_details-repository_id )
                                        iv_schema_alias     = gc_service_reference-account_details-schema_alias ).
  endmethod.


  METHOD define_timeentry.
    DATA: lt_primitive_properties TYPE /iwbep/if_v4_med_element=>ty_t_med_prim_property,
          lt_field_map            TYPE zif_project_tracker_types~gtt_edm_field_map,
          lo_prim_prop            TYPE REF TO /iwbep/if_v4_med_prim_prop,
          lo_entity_set           TYPE REF TO /iwbep/if_v4_med_entity_set,
          lo_nav_prop             TYPE REF TO /iwbep/if_v4_med_nav_prop,
          lo_entity_type          TYPE REF TO /iwbep/if_v4_med_entity_type,
          lo_annotation_model     TYPE REF TO /iwbep/if_v4_anno_model,
          ls_referenced_cds_view  TYPE zif_project_tracker_types~gty_cds_views-timeentry.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "   Create entity field map
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    zcl_project_tracker_mpc=>fieldmap_timeentry( IMPORTING et_field_map = lt_field_map ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "   Create entity type
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    lo_entity_type = io_model->create_entity_type_by_struct(
      EXPORTING
        iv_entity_type_name          = gc_entity_type_names-internal-timentry  " Internal entity type name
        is_structure                 = ls_referenced_cds_view       " Structure
        iv_gen_prim_props            = abap_true                    " Generate primitive properties automatically
        iv_add_annos_to_prim_props   = abap_true                    " Automatically generate annotations for  primitive properties
        iv_add_conv_to_prim_props    = abap_true                    " Enable conversion for primitive properties
        iv_add_f4_help_to_prim_props = abap_true                    " Add F4 help for primitive properties
    ).

    lo_entity_type->set_edm_name( gc_entity_type_names-edm-timeentry ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "   Primative Properties - Keys, nullable and EDM Type checks
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    lo_entity_type->get_primitive_properties( IMPORTING et_property = lt_primitive_properties ).
    LOOP AT lt_primitive_properties ASSIGNING FIELD-SYMBOL(<lo_primitive_property>).
      READ TABLE lt_field_map WITH TABLE KEY by_upper
                              COMPONENTS
                                upper = <lo_primitive_property>->get_internal_name( )
                              ASSIGNING FIELD-SYMBOL(<ls_field_map>).
      IF sy-subrc = 0.
        <lo_primitive_property>->set_edm_name( <ls_field_map>-edm_name ).
        IF <ls_field_map>-nullable = abap_true.
          <lo_primitive_property>->set_is_nullable( ).
        ENDIF.
        IF <ls_field_map>-key = abap_true.
          <lo_primitive_property>->set_is_key( ).
        ENDIF.

*       There are cases with OData V4 where the conversion routine will NOT properly
*       handle null/blank currency and will throw an error
        IF <ls_field_map>-no_conversion = abap_true.
          <lo_primitive_property>->set_add_conversion( abap_false ).
        ENDIF.

**        IF <ls_field_map>-upper = |STARTTIME| OR <ls_field_map>-upper = |ENDTIME|.
**          <lo_primitive_property>->set_edm_type( /iwbep/if_v4_med_element=>gcs_edm_data_types-datetimeoffset ).
**        ENDIF.

        "Mark all dates as nullable as framework dumps when mandatory dates aren't populated.
        IF <lo_primitive_property>->get_edm_type( ) = /iwbep/if_v4_med_element=>gcs_edm_data_types-date OR
           <lo_primitive_property>->get_edm_type( ) = /iwbep/if_v4_med_element=>gcs_edm_data_types-datetimeoffset.
          <lo_primitive_property>->set_is_nullable( ).
        ENDIF.
      ENDIF.

    ENDLOOP.

    "Define Entity Set
    lo_entity_set = lo_entity_type->create_entity_set( gc_entity_set_names-internal-timeentryset ).
    lo_entity_set->set_edm_name( gc_entity_set_names-edm-timeentryset ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "   Navigation Properties
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*   --TIME ENTRY ---> TO ---> WORKITEM (1:1)
    lo_nav_prop = lo_entity_type->create_navigation_property( iv_property_name = gc_nav_prop_names-internal-timeentry_to_workitem ).
    lo_nav_prop->set_edm_name( gc_nav_prop_names-edm-timeentry_to_workitem ).

    lo_nav_prop->set_target_entity_type_name( iv_entity_type_name = gc_entity_type_names-internal-workitem ).
    lo_nav_prop->set_target_multiplicity( /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_one ).  "ALWAYS a Work Item for a given time entry

    "Special Actions
    lo_nav_prop->set_on_delete_action( iv_on_delete_action = /iwbep/if_v4_med_element=>gcs_med_on_delete_action-none ).

    lo_entity_set->add_navigation_prop_binding( iv_navigation_property_path = gc_nav_prop_names-edm-timeentry_to_workitem && ``
                                                iv_target_entity_set        = gc_entity_set_names-internal-workitemset ).


  ENDMETHOD.


  METHOD define_workitem.
    DATA: lt_primitive_properties TYPE /iwbep/if_v4_med_element=>ty_t_med_prim_property,
          lt_field_map            TYPE zif_project_tracker_types~gtt_edm_field_map,
          lo_prim_prop            TYPE REF TO /iwbep/if_v4_med_prim_prop,
          lo_entity_set           TYPE REF TO /iwbep/if_v4_med_entity_set,
          lo_nav_prop             TYPE REF TO /iwbep/if_v4_med_nav_prop,
          lo_entity_type          TYPE REF TO /iwbep/if_v4_med_entity_type,
          lo_annotation_model     TYPE REF TO /iwbep/if_v4_anno_model,
          ls_referenced_cds_view  TYPE zif_project_tracker_types~gty_cds_views-workitem.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "   Create entity field map
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    zcl_project_tracker_mpc=>fieldmap_workitem( IMPORTING et_field_map = lt_field_map ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "   Create entity type
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    lo_entity_type = io_model->create_entity_type_by_struct(
      EXPORTING
        iv_entity_type_name          = gc_entity_type_names-internal-workitem  " Internal entity type name
        is_structure                 = ls_referenced_cds_view       " Structure
        iv_gen_prim_props            = abap_true                    " Generate primitive properties automatically
        iv_add_annos_to_prim_props   = abap_true                    " Automatically generate annotations for  primitive properties
        iv_add_conv_to_prim_props    = abap_true                    " Enable conversion for primitive properties
        iv_add_f4_help_to_prim_props = abap_true                    " Add F4 help for primitive properties
    ).

    lo_entity_type->set_edm_name( gc_entity_type_names-edm-workitem ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "   Primative Properties - Keys, nullable and EDM Type checks
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    lo_entity_type->get_primitive_properties( IMPORTING et_property = lt_primitive_properties ).
    LOOP AT lt_primitive_properties ASSIGNING FIELD-SYMBOL(<lo_primitive_property>).
      READ TABLE lt_field_map WITH TABLE KEY by_upper
                              COMPONENTS
                                upper = <lo_primitive_property>->get_internal_name( )
                              ASSIGNING FIELD-SYMBOL(<ls_field_map>).
      IF sy-subrc = 0.
        <lo_primitive_property>->set_edm_name( <ls_field_map>-edm_name ).
        IF <ls_field_map>-nullable = abap_true.
          <lo_primitive_property>->set_is_nullable( ).
        ENDIF.
        IF <ls_field_map>-key = abap_true.
          <lo_primitive_property>->set_is_key( ).
        ENDIF.

        IF <ls_field_map>-internal = 'MODIFIEDDATE'.
          <lo_primitive_property>->use_as_etag( ).
        ENDIF.

*       There are cases with OData V4 where the conversion routine will NOT properly
*       handle null/blank currency and will throw an error
        IF <ls_field_map>-no_conversion = abap_true.
          <lo_primitive_property>->set_add_conversion( abap_false ).
        ENDIF.

        "Mark all dates as nullable as framework dumps when mandatory dates aren't populated.
        IF <lo_primitive_property>->get_edm_type( ) = /iwbep/if_v4_med_element=>gcs_edm_data_types-date.
          <lo_primitive_property>->set_is_nullable( ).
        ENDIF.
      ENDIF.

      "Mark all dates as nullable as framework dumps when mandatory dates aren't populated.
      IF <lo_primitive_property>->get_edm_type( ) = /iwbep/if_v4_med_element=>gcs_edm_data_types-date.
        <lo_primitive_property>->set_is_nullable( ).
      ENDIF.
    ENDLOOP.

    "Define Entity Set
    lo_entity_set = lo_entity_type->create_entity_set( gc_entity_set_names-internal-workitemset ).
    lo_entity_set->set_edm_name( gc_entity_set_names-edm-workitemset ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "   Navigation Properties
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*   --WORK ITEM---> TO ---> TIME ENTRY  (0:N)
    lo_nav_prop = lo_entity_type->create_navigation_property( iv_property_name = gc_nav_prop_names-internal-workitem_to_timeentry ).
    lo_nav_prop->set_edm_name( gc_nav_prop_names-edm-workitem_to_timeentry ).

    lo_nav_prop->set_target_entity_type_name( iv_entity_type_name = gc_entity_type_names-internal-timentry ).
    lo_nav_prop->set_target_multiplicity( /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_many_optional ).  "ALWAYS a Work Item for a given time entry

    "Special Actions
    lo_nav_prop->set_on_delete_action( iv_on_delete_action = /iwbep/if_v4_med_element=>gcs_med_on_delete_action-cascade ).

    lo_entity_set->add_navigation_prop_binding( iv_navigation_property_path = gc_nav_prop_names-edm-workitem_to_timeentry && ``
                                                iv_target_entity_set        = gc_entity_set_names-internal-timeentryset ).

*   --WORK ITEM ---> TO ---> (ACCOUNT DETAILS SERVICE) ACCOUNTS (0:1)
    lo_nav_prop = lo_entity_type->create_navigation_property( iv_property_name = gc_nav_prop_names-internal-workitem_to_account ).
    lo_nav_prop->set_edm_name( gc_nav_prop_names-edm-workitem_to_account ).

    "Set Target entity name and then Set Source to Target entityset cardinality
    lo_nav_prop->set_target_entity_type_name( iv_service_ref_name = gc_service_reference-account_details-service_ref_name
                                              iv_entity_type_name = zif_account_details_types=>gc_entity_type_names-internal-account ).
    lo_nav_prop->set_target_multiplicity( /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_one_optional ).

    "Special Actions
    lo_nav_prop->set_on_delete_action( iv_on_delete_action = /iwbep/if_v4_med_element=>gcs_med_on_delete_action-none ).

    "Bind Navigation to target entity set
    lo_entity_set->add_navigation_prop_binding( iv_navigation_property_path = gc_nav_prop_names-edm-workitem_to_account && ``
                                                iv_target_entity_set        = zif_account_details_types=>gc_entity_set_names-internal-accountset ).

  ENDMETHOD.


  METHOD fieldmap_timeentry.
    et_field_map = VALUE #( ( internal = 'GUID' edm_name = 'Guid' db_field = 'GUID' key = abap_true )
                            ( internal = 'WORKITEMGUID' edm_name = 'WorkItemGuid' db_field = 'WORKITEM_GUID' )
                            ( internal = 'STARTTIME' edm_name = 'StartTime' db_field = 'STARTTIME' )
                            ( internal = 'ENDTIME' edm_name = 'EndTime' db_field = 'ENDTIME' )
                            ( internal = 'NOTES' edm_name = 'Notes' db_field = 'NOTES' nullable = abap_true )
                            ).

    LOOP AT et_field_map ASSIGNING FIELD-SYMBOL(<ls_field_map>).
      <ls_field_map>-upper = to_upper( <ls_field_map>-edm_name ).
    ENDLOOP.
  ENDMETHOD.


  method FIELDMAP_WORKITEM.
    et_field_map = VALUE #( ( internal = 'GUID' edm_name = 'Guid' db_field = 'GUID' key = abap_true )
                            ( internal = 'ITEMNAME' edm_name = 'ItemName' db_field = 'ITEMNAME' )
                            ( internal = 'ACCOUNTCONTACT' edm_name = 'AccountContact' db_field = 'ACCOUNT_CONTACT' nullable = abap_true )
                            ( internal = 'STATUS' edm_name = 'Status' db_field = 'STATUS' )
                            ( internal = 'DETAILS' edm_name = 'Details' db_field = 'DETAILS' )
                            ( internal = 'CREATEDDATE' edm_name = 'CreatedDate' db_field = 'CREATED_DATE' nullable = abap_true )
                            ( internal = 'CREATEDBY' edm_name = 'CreatedBy' db_field = 'CREATED_BY' )
                            ( internal = 'MODIFIEDDATE' edm_name = 'ModifiedDate' db_field = 'MODIFIED_DATE' nullable = abap_true )
                            ( internal = 'MODIFIEDBY' edm_name = 'ModifiedBy' db_field = 'MODIFIED_BY' )
                            ).

    LOOP AT et_field_map ASSIGNING FIELD-SYMBOL(<ls_field_map>).
      <ls_field_map>-upper = to_upper( <ls_field_map>-edm_name ).
    ENDLOOP.
  endmethod.
ENDCLASS.