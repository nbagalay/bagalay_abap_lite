# SAP OData V4 - Major Component Shell
Attached are a few of the core classes for a Project Tracking Service. This service has 2 entities: a Work Item (details of the task) and a Time Entry (time spent against the Work Item). Below we'll walk through the key files in this Repository.

To help traverse through the file, I added comments as needed. Please don't hesitate to post any questions about the content.

## Sample ABAP Programs
In the _ABAP Program_ folder is a small program. This just shows some calls that represent the newer syntax presented in 7.40. Althrough this is now second hand, I wanted a program to reflect several examples using the syntax. Note the comment structure of the ABAP program and how I section it off. It helps break it apart. For local classes, I like to use INCLUDE programs but in this case it is such a small program it didn't warrant a need.

## Gateway OData V4 Sample
This is a small sample of a full CRUD service called Project Tracker. There are 3 major components to an OData v4 service, the DPC, MPC and Interface. Such files are here for your reference. **Please Note these files alone will not work as the data elements, structures, tables etc were not included**. This is used as an aid on the development style I use for my ABAP projects.

### Project Tracker MPC File
This is the Model Provider Class (MPC). In short it defines the structure and the various entites that will be in the class. We use CDS views to define the query and the structure of the primary calls. The FIELD_MAP method defines special attributes we want to call out for each column of the entity. Ex: How we want the EDM name to be displayed, if we want to have F4 conversions on ETC.

### Project Tracker DPC File
This is the Data Provider Class (DPC). Here we define our CRUD operations of the service. Operations include:

* _UPDATE 304 Check_ : In the PATCH call, I shared a utility method I build (not provided here) of comparing the change structure against what the DB record has. If it matches, then no changes were found.
* _Fieldmap to DB Conversion_ : In the Change Operations, we provided a conversion from the Entity EDM Structure to the DB Fields. We want to displayn _human readable_ fields on our entities but some of the names do not match between fields. This allows us to move the related fields using a Dynamic structure mapping.

### Project Tracker Types Interface File
This is more of a tie for all of the classes. We used an Interface file to hold our constants, naming definitions etc for the service. As you can see in the DPC and MPC, most of our constants are found here.