# Bagalay Development Samples
This is a small collection of developments sharing different aspects of design and program development. These programs were extracted from my private ABAPGit collection which is why this didn't following the Git structure.

To help traverse through the file, I added comments as needed. Please don't hesitate to post any questions about the content.

## Sample ABAP Programs
In the _ABAP Program_ folder are a few small programs. Please do not hesitate to send me any questions or comments you have.

### 740 Syntax Samples
This just shows some calls that represent the newer syntax presented in 7.40. Althrough this is now becoming standard for everyone, I wanted a program to reflect several examples using the syntax. Note the comment structure of the ABAP program and how I section it off. It helps break it apart. For local classes, I like to use INCLUDE programs but in this case it is such a small program it didn't warrant a need.

### IDA Sflight Sample
This is a full operating program showing the use of local classes and the use of the IDA ALV grid using the SFLIGHT model. You will have to populate your system first with the model data.

## Gateway OData V4 Sample
This is a small sample of a full CRUD service called Project Tracker. There are 3 major components to an OData v4 service, the DPC, MPC and Interface. Such files are here for your reference. **Please Note these files alone will not work as the data elements, structures, tables etc were not included**. This is used as an aid on the development style I use for my ABAP projects.

Attached are a few of the core classes for a Project Tracking Service. This service has 2 entities: a Work Item (details of the task) and a Time Entry (time spent against the Work Item). Below we'll walk through the key files in this Repository.

### Project Tracker MPC File
This is the Model Provider Class (MPC). In short it defines the structure and the various entites that will be in the class. I used CDS views to define the query and the structure of the primary calls. The FIELD_MAP method defines special attributes I wanted to call out for each column of the entity. Ex: How we want the EDM name to be displayed, if we want to have F4 conversions on ETC.

### Project Tracker DPC File
This is the Data Provider Class (DPC). Here is where I defined our CRUD operations of the service. Operations include:

* _UPDATE 304 Check_ : In the PATCH call, I shared a utility method I build (not provided here) of comparing the change structure against what the DB record has. If it matches, then no changes were found.
* _Fieldmap to DB Conversion_ : In the Change Operations, I provided a conversion from the Entity EDM Structure to the DB Fields. I want to displayn _human readable_ fields on our entities but some of the names do not match between fields. This allows us to move the related fields using a Dynamic structure mapping.
* _Full Query Support_: All the entities support full OData query support like $select, $filter, $skiptoken

### Project Tracker Types Interface File
This is more of a tie for all of the classes. I used an Interface file to hold our constants, naming definitions etc for the service. As you can see in the DPC and MPC, most of our constants are found here.