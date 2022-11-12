************************************************************************
*                              Documentation                           *
*                                                                      *
* Title              : ABAP 740 Training                               *
* Original Creator   : Nick Bagalay                                    *
* Creation Month/Year: January 2019                                    *
* Purpose/Detail     : With Release 740, a number of new ABAP language *
*           enhancements were introduced. The developer can execute    *
*           this in debug mode and watch how the new logic works
*                                                                      *
************************************************************************
*                         History of Revisions                         *
************************************************************************
*  Mod  |   Date   |   SAP   | Transport | Specify changes made        *
*       |          | USER ID |           |                             *
*-------|----------|---------|-----------|-----------------------------*
************************************************************************
REPORT ZNNB_ABAP_740_TRAINING.

************************************************************************
*******                   Database Tables Area                   *******
************************************************************************
TYPES: BEGIN OF ty_alternate_prices,
         carrid    TYPE s_carr_id,
         connid    TYPE s_conn_id,
         fldate    TYPE s_date,
         bookid    TYPE s_book_id,
         customid  TYPE s_customer,
         loccuram  TYPE s_price,
         loccurkey TYPE s_currcode,
         condflag  TYPE abap_bool,
       END OF ty_alternate_prices.

TYPES: tt_alternate_prices  TYPE TABLE OF ty_alternate_prices WITH KEY carrid connid fldate bookid.


************************************************************************
*******                   Types Specifications                   *******
************************************************************************

************************************************************************
*******                  Local Class Definition                  *******
************************************************************************
CLASS lcl_training DEFINITION.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_output_message TYPE string.

    METHODS get_welcome_message
      RETURNING VALUE(rv_return_message) TYPE string.
  PRIVATE SECTION.
    DATA: gv_output_message TYPE string.

ENDCLASS.
************************************************************************
*******                Local Class Implementation                *******
************************************************************************
CLASS lcl_training IMPLEMENTATION.
  METHOD constructor.
    gv_output_message = iv_output_message.
  ENDMETHOD.

  METHOD get_welcome_message.
    rv_return_message = gv_output_message.
  ENDMETHOD.

ENDCLASS.
************************************************************************
*******                   Internal Tables Area                   *******
************************************************************************
DATA: lt_alternate_prices   TYPE tt_alternate_prices,
      lt_aa_alt_prices      TYPE tt_alternate_prices,
      lt_corresp_alt_prices TYPE tt_alternate_prices,
      lv_total_price        TYPE s_price,
      lv_count              TYPE i,
      lv_customer           TYPE s_customer,
      lv_found              TYPE abap_bool,
      lv_string             TYPE string,
      lv_material           TYPE matnr.

DATA: lr_customer   TYPE RANGE OF s_customer.

************************************************************************
*******                          Objects                         *******
************************************************************************

************************************************************************
*******                   Working Storage Area                   *******
************************************************************************

************************************************************************
*******                     Selection-Screen                     *******
************************************************************************

************************************************************************
*******               Initialization-Statement Area              *******
************************************************************************
INITIALIZATION.

************************************************************************
*******                     Top-Of-Page Area                     *******
************************************************************************
TOP-OF-PAGE.

************************************************************************
*******                  Start-Of-Selection Area                 *******
************************************************************************
START-OF-SELECTION.

************************************************************************
*******                   Logical DB Macro Area                  *******
************************************************************************

************************************************************************
*******               Initial Processing Logic Area              *******
************************************************************************
  lv_material = '1100944'.

* One of the new features introduced was the use of the VALUE statement.
* Throughout this program, we will use this a LOT because it can do a lot.

* --VALUE: Build our alternate pricing table - What is happening here is that
*     we are filling an internal table with records. This is very useful in testing
*     where you may need to build/create records on the fly. The first set of '('
*     after the VALUE statement is the braces for VALUE. The second '(' is each
*     single record you are adding/modifying. We'll discuss modification later. Here,
*     we are inserting a new record
  lt_alternate_prices = VALUE #( ( carrid = 'AA'
                                   connid = 0017
                                   fldate = '20170322'
                                   bookid = 61
                                   customid = 3798
                                   loccuram = '477.16'
                                   loccurkey = 'USD' )
                                 ( carrid = 'AA'
                                   connid = 0017
                                   fldate = '20170322'
                                   bookid = 65
                                   customid = 2702
                                   loccuram = '50.89'
                                   loccurkey = 'USD' )
                                 ( carrid = 'AA'
                                   connid = 0017
                                   fldate = '20170322'
                                   bookid = 66
                                   customid = 1426
                                   loccuram = '100.12'
                                   loccurkey = 'USD' )
                                 ( carrid = 'AA'
                                   connid = 0017
                                   fldate = '20170419'
                                   bookid = 487
                                   customid = 2684
                                   loccuram = '150.12'
                                   loccurkey = 'USD' )
                                 ( carrid = 'AA'
                                   connid = 0017
                                   fldate = '20170419'
                                   bookid = 490
                                   customid = 4192
                                   loccuram = '210.22'
                                   loccurkey = 'USD' ) ).

* --VALUE: Build our alternate pricing table (reduced) - In our example above, you may have noticed that some of the fields
*     were repeated like CARRID, CONNID and LOCCURKEY. When you are building an IT (internal table), static fields can
*     be delared once. See the following example of the SAME records but the cosntants are grouped
  CLEAR: lt_alternate_prices.
  lt_alternate_prices = VALUE #( carrid = 'AA'
                                 connid = 0017
                                 fldate = '20170322'
                                 loccurkey = 'USD'
                               ( bookid   = 61
                                 customid = 3798
                                 loccuram = '477.16' )
                               ( bookid   = 65
                                 customid = 2702
                                 loccuram = '50.89' )
                               ( bookid   = 66
                                 customid = 1426
                                 loccuram = '100.12' )

                                 fldate = '20170419'        "NOTE: Noticed how we switched our constant? Moving from this point, we will use this date

                               ( bookid = 487
                                 customid = 2684
                                 loccuram = '150.12' )
                               ( bookid = 490
                                 customid = 4192
                                 loccuram = '210.22' ) ).

* Can't think of a real life case of this? I bet you can :). Remember our awesome range tables??? Remember, when you fill a
* range table (as in it is delared as a range), it always contains SIGN, OPTION, LOW, HIGH. In our example below, again we
* know the SIGN and OPTION are always the same and are adding each new row using those constants with the variable of LOW
* Note: if you EVER forget what values can exist for a given IT, CTRL+SPACE is your friend
  lr_customer = VALUE #( sign = 'I'
                         option = 'EQ'
                       ( low    = 3798 )
                       ( low    = 2702 ) ).

* --VALUE: Adding a record to Existing Table - We may need to insert a new record for an existing table. The BASE
*     statement will allow the user to insert a new record into an EXISTING IT. In the above examples, tenchnically I didn't
*     need the clear because when I do a new insert, it wipes out the table and restarts. Below is an example of us inserting a
*     different flight number to our already existing alternate prices table
  lt_alternate_prices = VALUE #( BASE lt_alternate_prices
                                 carrid = 'AA'
                                 connid = 0064
                                 fldate = '20170324'
                                 loccurkey = 'USD'
                               ( bookid = 4385
                                 customid = 3513
                                 loccuram = '910.22' )
                               ( bookid = 4389
                                 customid = 2830
                                 loccuram = '1210.22' ) ).

* --VALUE: FOR LOOPs in VALUE statements - So we have an IT of records, awesome. Now we realized in our giant table of
*     records we need to bring back values that meet a set of conditions
  lt_aa_alt_prices = VALUE #( FOR ls_aa_prices IN lt_alternate_prices
                              WHERE ( fldate = '20170322' )
                            ( CORRESPONDING #( ls_aa_prices ) ) ).

* --REDUCE: Getting the sum of some values - You may have a table of records and need to get the count/sum of a column or records
  lv_total_price = REDUCE #( INIT x TYPE s_price
                             FOR ls_altern_price IN lt_alternate_prices
                             WHERE ( connid = 17 )
                             NEXT x = x + ls_altern_price-loccuram ).

* --REDUCE: Getting the Count of some values - Counting is not much different than SUM
  lv_count = REDUCE #( INIT xcount TYPE i
                        FOR ls_altern_price IN lt_alternate_prices
                      WHERE ( connid = 17 )
                       NEXT xcount = xcount + 1 ).

* --VALUE: Condition (COND) - The condition is a new keyword that can be used to help do some light IF checks.
  lt_aa_alt_prices = VALUE #( FOR ls_cond_example IN lt_alternate_prices
                            ( carrid = ls_cond_example-carrid
                              connid = ls_cond_example-connid
                              fldate = ls_cond_example-fldate
                              bookid = ls_cond_example-bookid
                              customid = ls_cond_example-customid
                              loccuram = ls_cond_example-loccuram
                              loccurkey = ls_cond_example-loccurkey
                              condflag  = COND #( WHEN ls_cond_example-fldate = '20170322' AND
                                                       ls_cond_example-connid = 17 THEN |X|
                                                  ELSE abap_false ) ) ).

* --READ TABLE: Generic read of a table - Moving forward, we are going to show some different types
*     of reads on an IT. NOTE: If a value is NOT found, an exception CX_SY_ITAB_LINE_NOT_FOUND is
*     Thrown. Using the VALUE and OPTIONAL keywords will eliminate that. See below.

* You can get more table read examples (such as secondary keys etc) at:
* https://blogs.sap.com/2013/05/29/abap-news-for-release-740-table-expressions/

* This statement is the same as: READ TABLE lt_alternative_prices WITH KEY carrid    = 'AA'
*                                                                          connid    = 17
*                                                                          fldate    = '20170322'
*                                                                          bookid    = 65.
  lv_customer = VALUE #( lt_alternate_prices[ carrid    = 'AA'
                                              connid    = 17
                                              fldate    = '20170322'
                                              bookid    = 65 ]-customid OPTIONAL ).

* You can read based on an index. Here we are reading the INDEX of 1.
* Similar to: READ TABLE lt_alternative_prices INDEX 1 INTO lv_customer
  lv_customer = VALUE #( lt_alternate_prices[ 1 ]-customid OPTIONAL ).

* --PREDICATE FUNCTIONS - LINES and LINE_EXISTS - Two new statements were introduced. CTRL+SPACE won't recognize
*     this however they work. LINES returns the number of lines while LINE_EXISTS checks if an IT
*     contains the records you are looking for
* More predicate functions at: https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenpredicate_functions.htm
  IF lines( lt_alternate_prices ) > 2.
    lv_found = abap_true.
  ENDIF.

  CLEAR: lv_found.

* Note: when using LINE_EXISTS, if you are looking for a condition where it doesn't exist,
* just add NOT before LINE_EXISTS. Ex: IF NOT LINE_EXISTS( *****.....
  IF line_exists( lt_alternate_prices[ carrid    = 'AA'
                                       connid    = 17
                                       fldate    = '20170322' ] ).
    lv_found = abap_true.
  ENDIF.

* --CORRESPONDING: Using the new corresponding keyword
  lt_corresp_alt_prices = CORRESPONDING #( lt_corresp_alt_prices ).

* You can also use this in the VALUE statement.
  lt_corresp_alt_prices = VALUE #( FOR ls_corr_example IN lt_alternate_prices
                                   WHERE ( connid = 17 )
                                 ( CORRESPONDING #( ls_corr_example ) ) ).

* --NEW Statement - You can now instantiate a new class using the NEW keyword
  DATA(lo_new_class) = NEW lcl_training( 'Welcome!' ).
  DATA(lv_ret_msg_method_call) = lo_new_class->get_welcome_message( ).

* We can also chain a call
  DATA(lv_return_msg_from_welcome) = NEW lcl_training( 'Welcome to my class!' )->get_welcome_message( ).

* --EXPRESSIONS: Using the new STRING - Below are some new embedded expressions that were
*     introduced in 740. The || are used to enter your comment. The {} inside lets you call
*     variables/conditions inside
* See for more examples: https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abapcompute_string_format_options.htm
* Here is a light example of using the extended expressions calling a variable.
  lv_string = |Our output message from our class was '{ lv_return_msg_from_welcome }'|.

* Here we can get back a value from a table read into our string
  lv_string = |Welcome Customer '{ VALUE #( lt_alternate_prices[ carrid    = 'AA'
                                                                 connid    = 17
                                                                 fldate    = '20170322'
                                                                 bookid    = 65 ]-customid OPTIONAL ) }'|.

* We can also format date types given to how we want it to display such as the format the suer specified
  lv_string = |The flight Date { VALUE #( lt_alternate_prices[ carrid    = 'AA'
                                          connid    = 17
                                          fldate    = '20170322'
                                          bookid    = 65 ]-fldate OPTIONAL ) DATE = USER }|.

* The ALPHA will convert to internal/external values. This is useful for items like
* material numbers. The IN condition will pad the 0's, just like how you would want
* it done INternally. Out will format it how it should be on OUTput (ex 110-0944)
  lv_string = |{ lv_material ALPHA = IN }|.

************************************************************************
*******                   End-Of-Selection Area                  *******
************************************************************************
END-OF-SELECTION.

************************************************************************
*******                      End-Of-Page Area                    *******
************************************************************************
END-OF-PAGE.

************************************************************************
*******                         Forms Area                       *******
************************************************************************
