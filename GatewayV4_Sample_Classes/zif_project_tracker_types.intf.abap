INTERFACE zif_project_tracker_types
  PUBLIC .

  TYPES: BEGIN OF gty_cds_views,
           workitem  TYPE zcds_prjtrk_workitem,
           timeentry TYPE zcds_prjtrk_timeentry,
         END OF gty_cds_views.

  CONSTANTS: gc_max_responses       TYPE i VALUE 5000,
             gc_message_class       TYPE symsgid VALUE 'ZAPI_PRJTRACKER',
             gc_orderby_primary_key TYPE string VALUE 'PRIMARY KEY'.

* https://help.sap.com/viewer/68bf513362174d54b58cddec28794093/7.51.6/en-US/54a326519eff236ee10000000a445394.html
  CONSTANTS: BEGIN OF ge_edm_data_types,
               edm_date TYPE string VALUE 'Edm.Date',
             END OF ge_edm_data_types.

  CONSTANTS: BEGIN OF gc_message_nbr_ref,
               cannot_modify_key     TYPE symsgno VALUE '004',
               change_date_forbidden TYPE symsgno VALUE '005',
             END OF gc_message_nbr_ref.

  TYPES: BEGIN OF gty_edm_field_map,
           internal      TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name,
           db_field      TYPE char30,
           upper         TYPE /iwbep/if_v4_med_element=>ty_e_med_edm_name,
           edm_name      TYPE /iwbep/if_v4_med_element=>ty_e_med_edm_name,
           nullable      TYPE abap_bool,
           no_conversion TYPE abap_bool,
           key           TYPE abap_bool,
           createable    TYPE abap_bool,
           updateable    TYPE abap_bool,
           deleatable    TYPE abap_bool,
         END OF gty_edm_field_map,
         gtt_edm_field_map TYPE STANDARD TABLE OF gty_edm_field_map WITH NON-UNIQUE SORTED KEY by_upper COMPONENTS upper.

  CONSTANTS: BEGIN OF gc_message_types,
               error_e TYPE bapi_mtype VALUE 'E',
             END OF gc_message_types.

  CONSTANTS: BEGIN OF gc_range_tbl,
               BEGIN OF sign,
                 include TYPE char1 VALUE 'I',
                 exclude TYPE char1 VALUE 'E',
               END OF sign,

               BEGIN OF option,
                 equal_to TYPE char2 VALUE 'EQ',
               END OF option,
             END OF gc_range_tbl.

  CONSTANTS: BEGIN OF gc_entity_type_names,
*            /*** ENTITY TYPE NAMES (Internal and EDM) ***\
               BEGIN OF internal,
                 workitem TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name VALUE 'WORKITEM',
                 timentry TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name VALUE 'TIMEENTRY',
               END OF internal,

               BEGIN OF edm,
                 workitem  TYPE /iwbep/if_v4_med_element=>ty_e_med_edm_name VALUE 'WorkItem',
                 timeentry TYPE /iwbep/if_v4_med_element=>ty_e_med_edm_name VALUE 'TimeEntry',
               END OF edm,
             END OF gc_entity_type_names,

*            /*** ENTITY SET NAMES (Internal and EDM) ***\
             BEGIN OF gc_entity_set_names,
               BEGIN OF internal,
                 workitemset  TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name VALUE 'WORKITEMSET',
                 timeentryset TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name VALUE 'TIMEENTRYSET',
               END OF internal,
               BEGIN OF edm,
                 workitemset  TYPE /iwbep/if_v4_med_element=>ty_e_med_edm_name VALUE 'WorkItemSet',
                 timeentryset TYPE /iwbep/if_v4_med_element=>ty_e_med_edm_name VALUE 'TimeEntrySet',
               END OF edm,
             END OF gc_entity_set_names,

*            /*** ENTITY NAVIGATION(Internal and EDM) ***\
             BEGIN OF gc_nav_prop_names,
               BEGIN OF internal,
                 workitem_to_timeentry TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name VALUE '_TIMEENTRYSET',
                 workitem_to_account   TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name VALUE '_ACCOUNTSET',
                 timeentry_to_workitem TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name VALUE '_WORKITEMSET',
               END OF internal,

               BEGIN OF edm,
                 workitem_to_timeentry TYPE /iwbep/if_v4_med_element=>ty_e_med_edm_name VALUE '_TIMEENTRYSET',
                 workitem_to_account   TYPE /iwbep/if_v4_med_element=>ty_e_med_edm_name VALUE '_ACCOUNTSET',
                 timeentry_to_workitem TYPE /iwbep/if_v4_med_element=>ty_e_med_edm_name VALUE '_WORKITEMSET',
               END OF edm,
             END OF gc_nav_prop_names,

*   /*** ENTITY CDS TO ORIGINAL TABLE ***/
             BEGIN OF gc_cds_to_db_tbl,
               workitem  TYPE char16 VALUE 'ZPRJT_WORKITEM',
               timeentry TYPE char16 VALUE 'ZPRJT_TIMEENTRY',
             END OF gc_cds_to_db_tbl,

*   /*** CROSS SERVICE NAVIGATION SERVICE REFERENCE ***/

**  ---IMPORTANT NOTE FOR ECC 7.51 SP02 USERS (aka current CAL instance users)-----
*   Cross service ref does work if you do a ENTITY call by key with expand (Ex: ./WorkItemSet('{your key here'})?$expand=_ACCOUNTSET
*   HOWEVER, if you do a wide open LIST call (so not by key) you'll get an error:
*   "message": "Not implemented - The request cannot be processed; Implement '/IWBEP/IF_V4_DP_BASIC~READ_ENTITY_LIST'",
*   This is an error with the current SP we are on. I verified on a system that was on 7.50 SP16 and worked just fine.

             BEGIN OF gc_service_reference,
               BEGIN OF account_details,
                 service_ref_name TYPE  /iwbep/if_v4_med_element=>ty_e_med_internal_name VALUE 'REF_TO_ACCOUNTDETAILS',
                 schema_alias     TYPE  string                                           VALUE 'PRJTRKRACCTDET',
                 service_id       TYPE  /iwbep/v4_med_service_id                         VALUE 'ZACCOUNT_DETAILS',
                 repository_id    TYPE  /iwbep/v4_med_repository_id                      VALUE 'DEFAULT',
               END OF account_details,
             END OF gc_service_reference .

ENDINTERFACE.