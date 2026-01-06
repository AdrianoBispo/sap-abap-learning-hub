# Complex Internal Tables

## Properties of Complex Internal Tables

![](https://learning.sap.com/service/media/topic/eefe0ef7-411b-4889-b06f-a8959ad3d58f/S4D400_24_en-US_media/S4D400_24_en-US_images/01-PropertiesOfComplexITABs_001.png)

The internal tables we have used so far had a scalar data type as their row type. In the example shown in the figures here, the row type of internal table numbers is the ABAP built-in type I.

We refer to these kinds of internal tables as simple internal tables.

![](https://learning.sap.com/service/media/topic/eefe0ef7-411b-4889-b06f-a8959ad3d58f/S4D400_24_en-US_media/S4D400_24_en-US_images/01-PropertiesOfComplexITABs_002.png)

We speak of a complex internal table if the row type is a structured data type.

While a simple internal table has only one nameless column, a complex internal table consists of several columns, each of them with the name and type of the corresponding component of the structured row type. In the example, the row type of internal table connection is a structured type with five components: *carrier_id*, *connection_id*, *airport_from_id*, *airport_to_id*, and *carrier_name*. Consequently, internal table connections has five columns with those names.

> **NOTE:** The columns in the examples in the figures, Reminder: Simple Internal Tables and Internal Tables with Structured Row Type, all have scalar types. More generally, a column of an internal table could also be of structured type or even have a table type. In the latter case, we talk of a nested internal table.

![](https://learning.sap.com/service/media/topic/eefe0ef7-411b-4889-b06f-a8959ad3d58f/S4D400_24_en-US_media/S4D400_24_en-US_images/01-PropertiesOfComplexITABs_003.png)

Up to now, we have addressed the rows of an internal table by their position. This is called an index access.

With the named columns of a complex internal table, key access becomes more important. Key access means addressing a row of the internal table by looking for particular values in particular columns. The columns in which you search can be any columns of the internal table.

Index access to an internal table is always very fast, even if the internal table contains many rows. Key access, however, can become very slow if the table contains a lot of rows. Choosing the right access type for the internal table can improve the performance of a key access.

![](https://learning.sap.com/service/media/topic/eefe0ef7-411b-4889-b06f-a8959ad3d58f/S4D400_24_en-US_media/S4D400_24_en-US_images/01-PropertiesOfComplexITABs_004.png)

Every internal table has one of three access types. The access type determines how data is stored in the table and, based on that, how the system reads the table to retrieve the data.

The different types of tables are as follows:

- **Standard Table:** In a standard table, the contents are not stored in a particular sort order. By default, new records are appended to the end of the table. In order to retrieve data by key, the system must read it sequentially, which can lead to long retrieval times if the table is very large. The simple internal tables we used so far were standard tables.

- **Sorted Table:** In a sorted table, the contents of the table are always sorted according to the key fields in ascending order. When you insert a new record into the table, the system ensures that it is placed at the correct position. Since the data is always sorted, the system can retrieve records more efficiently than from a standard table (as long as you follow particular rules).

- **Hashed Table:** Hashed tables are managed using a special hash algorithm. This ensures that the system can retrieve records very quickly even if the table is extremely large. However, this performance gain only works in very particular cases.

![](https://learning.sap.com/service/media/topic/eefe0ef7-411b-4889-b06f-a8959ad3d58f/S4D400_24_en-US_media/S4D400_24_en-US_images/01-PropertiesOfComplexITABs_005.png)

Every internal table has a key. In standard tables, the key does not play a particularly significant role. For sorted and hashed tables, the key is very important as it determines the way in which the data will be managed in the table. Crucially, sorted and hashed tables are only faster for key access that addresses all or at least a subset of the key fields.

A further attribute of the table key is its uniqueness. You will sometimes want to allow duplicate entries in an internal table, and sometimes you will want to ensure that the key is unique. Here, the following rules apply:

- Duplicates are always allowed in standard tables
- Duplicates are never allowed in hashed tables
- For a sorted table, you choose in the definition whether the key is to be unique or non-unique.

> **NOTE:** Internal tables may also have secondary keys. Secondary keys are a way of improving the performance of key accesses to internal tables that use different combinations of fields. You will find more information about secondary keys in the ABAP syntax documentation.

## Complex Table Types

![](https://learning.sap.com/service/media/topic/e13a9f09-5d74-4b3c-966d-17aa779c4d14/S4D400_24_en-US_media/S4D400_24_en-US_images/02-ComplexTableTypes_002.png)

Furthermore, it is good programming style to define the data type first and then to create a variable that refers to the type.

Instead of specifying the access type and key of an internal table in the DATA statement, you should use a named table type. If you need the table type only locally in a method or in connection with a given class, you can define it using the TYPES statement.

The example defines a structured type st_connection, first. With this structured type as row type it then defines table type tt_connections. Finally, the declaration of internal table connections_5 refers to the table type.

If you need the table type globally, you can use a global table type.

A global table type is a repository object that can be used as data type anywhere in the system. ADT provides a dedicated editor for this kind of repository object. The tool consists of the following frames:

## Try It Out: Complex Table Types

1. Like in the first exercise of this course, create a new global class that implements interface _IF_OO_ADT_CLASSRUN_.

2. Copy the following code snippet to the implementation part of method, _if\_oo\_adt\_classrun~main( )_:

    ``` ABAP

      TYPES: BEGIN OF st_connection,
               carrier_id      TYPE /dmo/carrier_id,
               connection_id   TYPE /dmo/connection_id,
               airport_from_id TYPE /dmo/airport_from_id,
               airport_to_id   TYPE /dmo/airport_to_id,
               carrier_name    TYPE /dmo/carrier_name,
             END OF st_connection.
    
    
    * Example 1 : Simple and Complex Internal Table
    *********************************************************************
    
      " simple table (scalar row type)
      DATA numbers TYPE TABLE OF i.
      " complex table (structured row type)
      DATA connections TYPE TABLE OF st_connection.
    
      out->write(  `--------------------------------------------` ).
      out->write(  `Example 1: Simple and Complex Internal Table` ).
      out->write( data = numbers
                  name = `Simple Table NUMBERS:`).
      out->write( data = connections
                  name = `Complex Table CONNECTIONS:`).
    
    * Example 2 : Complex Internal Tables
    **********************************************************************
    
      " standard table with non-unique standard key (short form)
      DATA connections_1 TYPE TABLE OF st_connection.
    
      " standard table with non-unique standard key (explicit form)
      DATA connections_2 TYPE STANDARD TABLE OF st_connection
                              WITH NON-UNIQUE DEFAULT KEY.
    
      " sorted table with non-unique explicit key
      DATA connections_3  TYPE SORTED TABLE OF st_connection
                                WITH NON-UNIQUE KEY airport_from_id
                                                    airport_to_id.
    
      " sorted hashed with unique explicit key
      DATA connections_4  TYPE HASHED TABLE OF st_connection
                                WITH UNIQUE KEY carrier_id
                                                connection_id.
    
    *  Example 3 : Local Table Type
    **********************************************************************
    
      TYPES tt_connections TYPE SORTED TABLE OF st_connection
                                  WITH UNIQUE KEY carrier_id
                                                  connection_id.
    
      DATA connections_5 TYPE tt_connections.
    
    * Example 4 : Global Table Type
    *********************************************************************
    
      DATA flights  TYPE /dmo/t_flight.
    
      out->write(  `------------------------------------------` ).
      out->write(  `Example 4: Global Table TYpe /DMO/T_FLIGHT` ).
      out->write(  data = flights
                   name = `Internal Table FLIGHTS:` ).
    ```
    
3.  Press ``CTRL + F3`` on your keyboard to activate the class and ``F9`` to execute the console app.

4.  Analyze the console output. Debug the program, play around with the source code to get familiar with the concepts.
  
## Complex Internal Table Filling

![](https://learning.sap.com/service/media/topic/ac111635-a9dc-4107-8841-119603c2e2fa/S4D400_24_en-US_media/S4D400_24_en-US_images/03-FillComplexITABs_001.png)

As you already learned, the simplest way to add a new row to an internal table is the APPEND statement with a data object whose type corresponds to the row type of the internal table. This data object is sometimes referred to as work area.

For simple internal tables the work area used in APPEND can be a scalar variable, constant, or a literal. For complex internal tables, the work area has to be structured.

In the example, structured variable connection is used to fill internal table connections.

In principle, there are two ways to declare work area connection:

- Reference the row type *st_connection* directly
- Reference the row type indirectly using ``LIKE LINE OF <internal_table>``.

Defining work areas with ``LIKE LINE OF`` has two advantages:

- It reveals the purpose of the structured variable as work area for the internal table
- It ensures that the work area fits to the internal table, even if the definition of the internal table changes

![](https://learning.sap.com/service/media/topic/ac111635-a9dc-4107-8841-119603c2e2fa/S4D400_24_en-US_media/S4D400_24_en-US_images/03-FillComplexITABs_002.png)

If you do not fill the work area before the APPEND statement, the new row of the internal table will be filled with type-specific initial values.

> **Hint:** You get the same result with the special variant ``APPEND INITIAL LINE TO <internal_table>``. This variant does not even require a work area.

To fill the structured work area, you can either fill the individual components or, as you can see in the example, use a VALUE #( ) expression.

![](https://learning.sap.com/service/media/topic/ac111635-a9dc-4107-8841-119603c2e2fa/S4D400_24_en-US_media/S4D400_24_en-US_images/03-FillComplexITABs_003.png)

As you can see in the example, you can also use a ``VALUE #( )`` expression directly in the *APPEND* statement. In this case, you do not need a work area.

> **NOTE:** This can have a positive effect on the overall memory consumption of your program.

![](https://learning.sap.com/service/media/topic/ac111635-a9dc-4107-8841-119603c2e2fa/S4D400_24_en-US_media/S4D400_24_en-US_images/03-FillComplexITABs_004.png)

There is a variant of the ``VALUE #( )`` expression that you can assign directly to an internal table. In this variant of ``VALUE #( )`` additional pairs of brackets are used to separate the table rows from each other.

The code example fills internal table carriers with three rows, each with a different value for *carrier_id* and *carrier_name*. As a result of this, column *currency_code* is not mentioned, it is filled with the type specific initial value.

> **NOTE:** With the assignment above, all existing table rows are removed before the table is filled with the new rows.

![](https://learning.sap.com/service/media/topic/ac111635-a9dc-4107-8841-119603c2e2fa/S4D400_24_en-US_media/S4D400_24_en-US_images/03-FillComplexITABs_005.png)

To copy data between identically-named fields of two internal tables, use the CORRESPONDING operator. This works similarly to CORRESPONDING for structures: for each row of the source internal table, the system creates a new row in the target internal table and copies data between identically-named fields. Source fields for which there is no identically named field in the target are not copied. Target fields for which there is no identically named field in the source are filled with type-specific initial values.

In the example, the source internal table carriers contains three rows. Therefore, after the value assignment, the target internal table connections also contains three rows.

Fields *carrier_id* and *carrier_name* exist in both internal tables. They are copied from source to target. Field *currency_code* only exists in the source. It is not copied. Fields *connection_id*, *airport_from_id*, and *airport_to_id* exist only in the target. They are filled with initial values.

> **NOTE:** If the target internal table contains data before the assignment, the system deletes it.

> **Tutorial:** [How To Debug Complex Internal Tables](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_9171F14114573EA8:demo)

## Try It Out: Filling Complex Internal Tables

1. As it is in the first exercise of this course, create a new global class that implements interface *IF_OO_ADT_CLASSRUN*.
2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:
    
    ``` ABAP

        TYPES: BEGIN OF st_connection,
                  carrier_id      TYPE /dmo/carrier_id,
                  connection_id   TYPE /dmo/connection_id,
                  airport_from_id TYPE /dmo/airport_from_id,
                  airport_to_id   TYPE /dmo/airport_to_id,
                  carrier_name    TYPE /dmo/carrier_name,
               END OF st_connection.
    
        TYPES tt_connections TYPE STANDARD TABLE OF   st_connection
                                  WITH NON-UNIQUE KEY carrier_id
                                                      connection_id.
    
        DATA connections TYPE tt_connections.
    
        TYPES:  BEGIN OF st_carrier,
                  carrier_id    TYPE /dmo/carrier_id,
                  carrier_name  TYPE /dmo/carrier_name,
                  currency_code TYPE /dmo/currency_code,
                END OF st_carrier.
    
        TYPES tt_carriers TYPE STANDARD TABLE OF st_carrier
                              WITH NON-UNIQUE KEY carrier_id.
    
        DATA carriers TYPE tt_carriers.
    
    * Example 1: APPEND with structured data object (work area)
    *********************************************************************
    
    *    DATA connection  TYPE st_connection.
        " Declare the work area with LIKE LINE OF
        DATA connection LIKE LINE OF connections.
    
    
    *    connection-carrier_id       = 'NN'.
    *    connection-connection_id    = '1234'.
    *    connection-airport_from_id  = 'ABC'.
    *    connection-airport_to_id    = 'XYZ'.
    *    connection-carrier_name     = 'My Airline'.
    
        " Use VALUE #( ) instead assignment to individual components
        connection = VALUE #( carrier_id       = 'NN'
                              connection_id    = '1234'
                              airport_from_id  = 'ABC'
                              airport_to_id    = 'XYZ'
                              carrier_name     = 'My Airline' ).
    
        APPEND connection TO connections.
    
        out->write(  `--------------------------------` ).
        out->write(  `Example 1: APPEND with Work Area` ).
        out->write(  connections ).
    
    * Example 2: APPEND with VALUE #( ) expression
    *********************************************************************
    
        APPEND VALUE #( carrier_id       = 'NN'
                        connection_id    = '1234'
                        airport_from_id  = 'ABC'
                        airport_to_id    = 'XYZ'
                        carrier_name     = 'My Airline'
                      )
           TO connections.
    
        out->write(  `----------------------------` ).
        out->write(  `Example 2: Append with VALUE` ).
        out->write(  connections ).
    
    * Example 3: Filling an Internal Table with Several Rows
    *********************************************************************
    
        carriers = VALUE #(  (  carrier_id = 'AA' carrier_name = 'American Airlines' )
                             (  carrier_id = 'JL' carrier_name = 'Japan Airlines'    )
                             (  carrier_id = 'SQ' carrier_name = 'Singapore Airlines')
                          ).
    
        out->write(  `-----------------------------------------` ).
        out->write(  `Example 3: Fill Internal Table with VALUE` ).
        out->write(  carriers ).
    
    * Example 4: Filling one Internal Table from Another
    *********************************************************************
    
        connections = CORRESPONDING #( carriers ).
    
        out->write(  `--------------------------------------------` ).
        out->write(  `Example 4: CORRESPONDING for Internal Tables` ).
        out->write(  data = carriers
                     name = `Source Table CARRIERS:`).
        out->write(  data = connections
                     name = `Target Table CONNECTIONS:`).

    ```
    
3.  Press ``CTRL + F3`` on your keyboard to activate the class and ``F9`` to execute the console app.
4.  Analyze the console output. Debug the program, play around with the source code to get familiar with the concepts.

## Access to Complex Internal Tables

![](https://learning.sap.com/service/media/topic/d8977bd8-72d3-4e17-b362-9521c3b3a4f6/S4D400_24_en-US_media/S4D400_24_en-US_images/04-AccessComplexITABs_001.png)

Earlier in this course, you learned how to retrieve a single row from a simple internal table using an internal table expression. Back then we used an index access, that is, we identified the row through its position in the internal table. This index access works for complex internal tables in just the same way. For complex internal tables, however, internal table expressions with key access become important, where you identify the row through its content.

> **NOTE:** Even though this is called a key access, you can use any fields for the selection, not only key fields of the internal table. If more than one row fulfills the requirement, the first row is returned, that is, the row with the lowest index.

The example reads a single row from internal table connections. The key of this internal table consists of fields, *carrier_id* and *connection_id*, but the key access uses *airport_from_id* and *airport_to_id* to identify the row. The Internal table contains two connections from airport *SFO* to *SIN*, so the first of them is returned.

Remember that the ABAP runtime raises exception ``CX_SY_ITAB_LINE_NOT_FOUND`` if none of the rows fulfills the requirement. Handle this exception in a ``TRY … CATCH … ENDTRY`` structure to avoid runtime errors

![](https://learning.sap.com/service/media/topic/d8977bd8-72d3-4e17-b362-9521c3b3a4f6/S4D400_24_en-US_media/S4D400_24_en-US_images/04-AccessComplexITABs_002.png)

To process multiple lines of an internal table by specifying fields, you use ``LOOP AT <internal table> INTO <target> WHERE <condition>``. The ``WHERE`` condition can contain any number of constituent expressions joined using ``{AND}`` and ``OR``. Within the expressions, you can use not just the equals operator (``=``) but also operators ``>``, ``>=`` ``<``, ``<=``, ``<>`` and ``BETWEEN``.

![](https://learning.sap.com/service/media/topic/d8977bd8-72d3-4e17-b362-9521c3b3a4f6/S4D400_24_en-US_media/S4D400_24_en-US_images/04-AccessComplexITABs_003.png)

After reading the content of a table row into a work area, you sometimes want to write changes from the work area back into the internal table. One way to do this is the ``MODIFY TABLE`` statement.

This statement is a key access because the system uses the content of the key fields in the work area to identify the table row that needs to be modified. It then overwrites this table row with the contents of the work area.

In the example, the work area carrier contains value *'JL'* in key field, *carrier_id*. Based on this value, the system identifies the second row to be updated. This row is then updated with the values from the work area.

> **NOTE:** You can only change non-key fields with ``MODIFY TABLE``. The statement does not support changes to key fields.

![](https://learning.sap.com/service/media/topic/d8977bd8-72d3-4e17-b362-9521c3b3a4f6/S4D400_24_en-US_media/S4D400_24_en-US_images/04-AccessComplexITABs_004.png)

The ``MODIFY`` statement (without keyword TABLE!) does not distinguish between key fields and non-key fields. It overwrites the entire table row with new values from the work area. This statement is considered an index access because the row to be updated is identified by its position in the internal table. Usually, the index is specified explicitly using addition ``INDEX`` followed by an integer argument (literal, constant, variable, expression).

> **NOTE:** There is also a special variant without addition INDEX. We will discuss this variant next.

In the example, the ``MODIFY`` statement uses the ``INDEX`` addition to address the first table row. In this row, all fields are overwritten with the values from the work area, even key field *carrier_id*.

![](https://learning.sap.com/service/media/topic/d8977bd8-72d3-4e17-b362-9521c3b3a4f6/S4D400_24_en-US_media/S4D400_24_en-US_images/04-AccessComplexITABs_005.png)

There will often be times when you need to modify the contents of multiple rows of an internal table, or maybe even all of them. To do this, you implement a loop over the table, which places each row you need to change successively into a work area. Within the loop, you first change the contents of the work area and then write the changes back into the internal table using the ``MODIFY`` statement.

> **NOTE:** If you do not write your changes back into the table, the changes will be lost when the work area is filled with the data from next row.

In the example, the loop reads all rows of internal table carriers for which field *currency_code* is not yet filled. This is the case for the last two rows. For each of these rows the program replaces the initial value in field *currency_code* with the new value *'USD'*. Finally, it uses the ``MODIFY`` statement to overwrite the current row with the updated values.

Instead of specifying the index explicitly, the code example uses a short form of the ``MODIFY`` statement where the ``INDEX`` addition is missing. This short form is only allowed between ``LOOP … ENDLOOP``. Only there the system can implicitly update the row it is currently working on.

If you use ``MODIFY`` without ``INDEX`` outside of LOOP…ENDLOOP, the system does not know which row to modify and triggers a non-catchable runtime error. To avoid such runtime errors, make sure not to ignore the related warning from the syntax check!

## Try It Out: Access to Complex Internal Tables

1. As with the first exercise of this course, create a new global class that implements the interface, ``IF_OO_ADT_CLASSRUN``.
2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:
    
    ``` ABAP
    
      TYPES: BEGIN OF st_connection,
                 carrier_id      TYPE /dmo/carrier_id,
                 connection_id   TYPE /dmo/connection_id,
                 airport_from_id TYPE /dmo/airport_from_id,
                 airport_to_id   TYPE /dmo/airport_to_id,
                 carrier_name    TYPE /dmo/carrier_name,
               END OF st_connection.
    
        TYPES tt_connections TYPE SORTED TABLE OF   st_connection
                                  WITH NON-UNIQUE KEY carrier_id
                                                      connection_id.
    
        DATA connections TYPE tt_connections.
        DATA connection  LIKE LINE OF connections.
    
        TYPES: BEGIN OF st_carrier,
                 carrier_id    TYPE /dmo/carrier_id,
                 currency_code TYPE /dmo/currency_code,
               END OF st_carrier.
    
        DATA carriers TYPE STANDARD TABLE OF st_carrier
                           WITH NON-UNIQUE KEY carrier_id.
    
        DATA carrier LIKE LINE OF carriers.
    
    * Preparation: Fill internal tables with data
    *********************************************************************
        connections = VALUE #(  ( carrier_id      = 'JL'
                                  connection_id   = '0408'
                                  airport_from_id = 'FRA'
                                  airport_to_id   = 'NRT'
                                  carrier_name    = 'Japan Airlines'
                                )
                                ( carrier_id      = 'AA'
                                  connection_id   = '0017'
                                  airport_from_id = 'MIA'
                                  airport_to_id   = 'HAV'
                                  carrier_name    = 'American Airlines'
                                )
                                ( carrier_id      = 'SQ'
                                  connection_id   = '0001'
                                  airport_from_id = 'SFO'
                                  airport_to_id   = 'SIN'
                                  carrier_name    = 'Singapore Airlines'
                                )
                                ( carrier_id      = 'UA'
                                  connection_id   = '0078'
                                  airport_from_id = 'SFO'
                                  airport_to_id   = 'SIN'
                                  carrier_name    = 'United Airlines'
                                )
                               ).
    
        carriers = VALUE #(  (  carrier_id    = 'SQ'
                                currency_code = ' '
                             )
                             (  carrier_id    = 'JL'
                                currency_code = ' '
                             )
                             (  carrier_id    = 'AA'
                                currency_code = ' '
                             )
                             (  carrier_id    = 'UA'
                                currency_code = ' '
                             )
                          ).
    
    * Example 1: Table Expression with Key Access
    *********************************************************************
        out->write(  `--------------------------------------------` ).
        out->write(  `Example 1: Table Expressions with Key Access` ).
    
        out->write(  data = connections
                     name = `Internal Table CONNECTIONS: ` ).
    
        " with key fields
        connection = connections[ carrier_id    = 'SQ'
                                  connection_id = '0001' ].
    
        out->write(  data = connection
                     name = `CARRIER_ID = 'SQ' AND CONNECTION_ID = '001':` ).
    
        " with non-key fields
        connection = connections[ airport_from_id = 'SFO'
                                  airport_to_id   = 'SIN' ].
        out->write(  data = connection
                     name = `AIRPORT_FROM_ID = 'SFO' AND AIRPORT_TO_ID = 'SIN':` ).
    
    * Example 2: LOOP with key access
    *********************************************************************
    
        out->write(  `-------------------------------` ).
        out->write(  `Example 2: LOOP with Key Access` ).
    
        LOOP AT connections INTO connection
                           WHERE airport_from_id <> 'MIA'.
    
          "do something with the content of connection
          out->write( data = connection
                      name = |This is row number { sy-tabix }: | ).
    
        ENDLOOP.
    
    * Example 3: MODIFY TABLE (key access)
    *********************************************************************
        out->write(  `-----------------------------------` ).
        out->write(  `Example 3: MODIFY TABLE (key access` ).
    
        out->write(  data = carriers
                     name = `Table CARRRIERS before MODIFY TABLE:`).
    
        carrier = carriers[  carrier_id = 'JL' ].
        carrier-currency_code = 'JPY'.
        MODIFY TABLE carriers FROM carrier.
    
        out->write(  data = carriers
                     name = `Table CARRRIERS after MODIFY TABLE:`).
    
    * Example 4: MODIFY (index access)
    *********************************************************************
        out->write(  `--------------------------------` ).
        out->write(  `Example 4: MODIFY (index access)` ).
    
        carrier-carrier_id    = 'LH'.
        carrier-currency_code = 'EUR'.
        MODIFY carriers FROM carrier INDEX 1.
    
        out->write(  data = carriers
                     name = `Table CARRRIERS after MODIFY:`).
    
    * Example 5: MODIFY in a LOOP
    *********************************************************************
        out->write(  `----------------------------` ).
        out->write(  `Example 5: MODIFY  in a LOOP` ).
    
        LOOP AT carriers INTO carrier
                        WHERE currency_code IS INITIAL.
    
          carrier-currency_code = 'USD'.
          MODIFY carriers FROM carrier.
    
        ENDLOOP.
    
        out->write(  data = carriers
                     name = `Table CARRRIERS after the LOOP:`).

    ```
    
3. Press ``CTRL + F3`` on your keyboard to activate the class and ``F9`` to execute the console app.
4. Analyze the console output. Debug the program, play around with the source code to get familiar with the concepts.

## Complex Internal Tables in ABAP SQL

![](https://learning.sap.com/service/media/topic/bd50f310-d058-4fda-8431-9b7e9a4824c5/S4D400_24_en-US_media/S4D400_24_en-US_images/05-ComplexITABsInABAPSQL_001.png)

The *ABAP SQL* statement, ``SELECT``, reads data from a database table or a CDS View. When you use the ``SINGLE`` option, exactly one record is read from the database, even if more data exist that meets the conditions in the ``WHERE`` clause.

As you learned earlier, one way to receive this single record result is structured variable after keyword ``INTO``.

![](https://learning.sap.com/service/media/topic/bd50f310-d058-4fda-8431-9b7e9a4824c5/S4D400_24_en-US_media/S4D400_24_en-US_images/05-ComplexITABsInABAPSQL_002.png)

If you use ``SELECT`` without ``SINGLE``, you indicate that you are interested in all records that match the conditions in the ``WHERE`` clause. You then have to make sure that you can actually receive and store multiple records. The obvious way to do this is the usage of a complex internal table as target of the ``SELECT`` statement. This is possible but it requires addition ``TABLE`` between keyword ``INTO`` and the name of the internal table.

In the example, we want to read all three airports related to London and not just a single one of them. Therefore, we leave out the keyword ``SINGLE`` after ``SELECT``, add keyword ``TABLE`` after ``INTO`` and use internal table *airports_full* as target of the ``SELECT`` statement.

The example uses an explicit field list after ``FIELDS`` that matches the columns of internal table *airports_full*. Of course, you can also use ``FIELDS *``, ``INTO CORRESPONDING FIELDS OF TABLE``, and alias names in the field list.

![](https://learning.sap.com/service/media/topic/bd50f310-d058-4fda-8431-9b7e9a4824c5/S4D400_24_en-US_media/S4D400_24_en-US_images/05-ComplexITABsInABAPSQL_003.png)

This example uses ``FIELDS *`` instead of an explicit field list and ``INTO CORRESPONDING FIELDS OF TABLE`` instead of ``INTO TABLE``.

As the row type of internal table airports contains only two components *AirportID* and *Name*, only the fields with the same name are read from the database.

![](https://learning.sap.com/service/media/topic/bd50f310-d058-4fda-8431-9b7e9a4824c5/S4D400_24_en-US_media/S4D400_24_en-US_images/05-ComplexITABsInABAPSQL_004.png)

If you use ``DATA( )`` in a ``SELECT`` statement after addition ``INTO TABLE``, you inline declare an internal table. The row type of this internal table is derived from the ``FIELDS`` clause. For table fields and view elements an alias name is optional. For expressions in the ``FIELDS`` clause, an alias name is mandatory if the ``INTO`` clause contains an inline declaration.

> **NOTE:** Inline declarations of internal tables are only supported after ``INTO TABLE``. You cannot use inline declarations after ``INTO CORRESPONDING FIELDS OF TABLE``.

Inline-declared internal tables are always standard tables without a key. You cannot declare sorted or hashed tables using inline declarations. This can cause performance problems if you fill the internal table with many rows and use key access a lot.

![](https://learning.sap.com/service/media/topic/bd50f310-d058-4fda-8431-9b7e9a4824c5/S4D400_24_en-US_media/S4D400_24_en-US_images/05-ComplexITABsInABAPSQL_005.png)

When you are reading multiple records from the database, some special SQL techniques become particularly interesting. One of these techniques is the ``UNION`` directive to combine the results of several ``SELECT`` statements.

The figure illustrates the combination of two ``SELECT`` results:

The first ``SELECT`` result reads ID and NAME of all carriers with ``CURRENCY_CODE = 'GBP'``. The second ``SELECT`` reads *ID* and *NAME* of all airports with ``CITY = 'London'``. The first ``SELECT`` returns one record, the second ``SELECT`` returns three records. Instead of retrieving these results separately, they are combined into one result with four records. It is important to point out that this happens inside the database.

A prerequisite for this technique is, of course, that the two results are compatible with each other, that is, that they have the same number of fields, the same field names. It is beneficial, though not necessary, that the types of the fields are also the same.

![](https://learning.sap.com/service/media/topic/bd50f310-d058-4fda-8431-9b7e9a4824c5/S4D400_24_en-US_media/S4D400_24_en-US_images/05-ComplexITABsInABAPSQL_006.png)

The ABAP SQL syntax for this example consists of two SELECT statements. Each SELECT statement has its own FROM clause, FIELDS clause, and WHERE clause, but there is only one INTO clause at the very end. The two SELECT statements are connected by keywords UNION ALL.

> **NOTE:** With UNION instead of UNION ALL, the database would look for and eliminate duplicates before returning the result. We use UNION ALL to avoid this unnecessary additional load on the database.

Both field lists consist of three elements, the first and second element have identical alias names in both FIELDS clauses. The third field does not need an alias because the field name is the same in both CDS Views.

> **NOTE:** The first element in FIELDS is a literal text that allows us to distinguish between Airlines and Airports in the combined result.

## Try It Out: Internal Tables in ABAP SQL

1. As in the first exercise of this course, create a new global class that implements interface *IF_OO_ADT_CLASSRUN*.

2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:

    ``` ABAP
    
        TYPES: BEGIN OF st_airport,
                 airportid TYPE /dmo/airport_id,
                 name      TYPE /dmo/airport_name,
               END OF st_airport.
    
        TYPES tt_airports TYPE STANDARD TABLE OF st_airport
                              WITH NON-UNIQUE KEY airportid.
    
        DATA airports TYPE tt_airports.
    
    
    * Example 1: Structured Variables in SELECT SINGLE ... INTO ...
    *********************************************************************
    
        DATA airport_full TYPE /DMO/I_Airport.
    
        SELECT SINGLE
          FROM /DMO/I_Airport
        FIELDS AirportID, Name, City, CountryCode
         WHERE City = 'Zurich'
          INTO @airport_full.
    
        out->write(  `-------------------------------------` ).
        out->write(  `Example 1: SELECT SINGLE ... INTO ...` ).
        out->write(  data = airport_full
                     name = `One of the airports in Zurich (Structure):` ).
    
    * Example 2: Internal Tables in SELECT ... INTO TABLE ...
    *********************************************************************
    
        DATA airports_full TYPE STANDARD TABLE OF /DMO/I_Airport
                                WITH NON-UNIQUE KEY AirportID.
    
        SELECT
          FROM /DMO/I_Airport
        FIELDS airportid, Name, City, CountryCode
         WHERE City = 'London'
          INTO TABLE @airports_full.
    
        out->write(  `------------------------------------` ).
        out->write(  `Example 2: SELECT ... INTO TABLE ...` ).
        out->write(  data = airports_full
                     name = `All airports in London (Internal Table):` ).
    
    * Example 3: FIELDS * and INTO CORRESPONDING FIELDS OF TABLE
    **********************************************************************
    
        SELECT
          FROM /DMO/I_Airport
        FIELDS *
         WHERE City = 'London'
          INTO CORRESPONDING FIELDS OF TABLE @airports.
    
        out->write(  `----------------------------------------------------------` ).
        out->write(  `Example 3: FIELDS * and INTO CORRESPONDING FIELDS OF TABLE` ).
        out->write(  data = airports
                     name = `Internal Table AIRPORTS:` ).
    
    * Example 4: Inline Declaration
    **********************************************************************
    
        SELECT
          FROM /DMO/I_airport
        FIELDS AirportID, Name AS AirportName
         WHERE City = 'London'
         INTO TABLE @DATA(airports_inline).
    
        out->write(  `----------------------------------------------------------` ).
        out->write(  `Example 4: Inline Declaration after INTO TABLE` ).
        out->write(  data = airports_inline
                     name = `Internal Table AIRPORTS_INLINE:` ).
    
    * Example 4: ORDER BY and DISTINCT
    **********************************************************************
    *
    *     SELECT
    *       FROM /DMO/I_Airport
    *     FIELDS DISTINCT CountryCode
    *      ORDER BY CountryCode
    *      INTO TABLE @DATA(countryCodes).
    *
    *    out->write(  countryCodes ).
    
    * Example 5: UNION (ALL)
    **********************************************************************
    
        SELECT FROM /DMO/I_Carrier
               FIELDS 'Airline' AS type, AirlineID AS Id, Name
               WHERE CurrencyCode = 'GBP'
    
        UNION ALL
    
        SELECT FROM /DMO/I_Airport
               FIELDS 'Airport' AS type, AirportID AS Id,  Name
               WHERE City = 'London'
    *     ORDER BY type, Id
        INTO TABLE @DATA(names).
    
        out->write(  `----------------------------------------------` ).
        out->write(  `Example 5: UNION ALL of Airlines and Airports ` ).
        out->write(  data = names
                     name = `ID and Name of Airlines and Airports:` ).
    
    ```

3. Press ``CTRL + F3`` on your keyboard to activate the class and ``F9`` to execute the console app.

4. Analyze the console output. Debug the program, play around with the source code to get familiar with the concepts.

## Exercise - Use a Complex Internal Table

In this exercise, you declare a table-like attribute, fill it using a ``SELECT`` statement and access the content.

- **Template:** ``/LRN/CL_S4D400_STS_STRUCTURE`` (global Class)
- **Solution:** ``LRN/CL_S4D400_ITS_ITAB`` (global Class)

### Task 1: Copy Template

Copy the template class. Alternatively, copy your solution of the previous exercise.

#### Steps

1. Copy the class **/LRN/CL_S4D400_STS_STRUCTURE** to a class in your own package (suggested name: **ZCL_##_ITAB**, where ## stands for your group number).
    
    a. Open the source code of the global class **/LRN/CL_S4D400_STS_STRUCTURE**.
          
    b. Link the _Project Explorer_ view with the editor.
          
    c. In the _Project Explorer_ view, right-click the class **/LRN/CL_S4D400_STS_STRUCTURE** to open the context menu.
          
    d. From the context menu, choose _Duplicate ..._.
          
    e. Enter the name of your package in the _Package_ field. In the _Name_ field, enter the name **ZCL_##_ITAB**, where ## stands for your group number.
          
    f. Adjust the description and choose _Next_.
          
    g. Confirm the transport request and choose _Finish_.

### Task 2: Declare an Internal Table

In the local class, declare a static, table-like attribute **airports** to buffer detail information on all available airports. Begin by defining a private structure type **st_airport** and a table type **tt_airports** inside the local class.

#### Steps

1. Switch to the local class **lcl_connection**.
    
    a. In the global class, choose _Local Types_.
    
2. Define a **private** structure type **st_airport** with the following components:
    
    | Component Name | Data Type |
    | --- | --- |
    | **AirportID** | ``/dmo/airport_id`` |
    | **Name** | ``/dmo/airport_name`` |
    
    a. Adjust the code as follows:

      ``` ABAP

             TYPES:
                BEGIN OF st_details,
                  DepartureAirport   TYPE /dmo/airport_from_id,
                  DestinationAirport TYPE /dmo/airport_to_id,
                  AirlineName        TYPE /dmo/carrier_name,
                END OF st_details.
          
              TYPES:
                BEGIN OF st_airport,
                  AirportId TYPE /dmo/airport_id,
                  Name      TYPE /dmo/airport_name,
                END OF st_airport.

      ```
    
3. Define a **private** table type **tt_airports** with the following properties:
    
    | Property | Value |
    | --- | --- |
    | **Line type** | *st_airport* |
    | **Table kind** | *STANDARD TABLE* |
    | **Key Definition** | *NON-UNIQUE DEFAULT KEY* |
    
    a. Adjust the code as follows:

      ``` ABAP

           TYPES:
                BEGIN OF st_airport,
                  AirportId TYPE /dmo/airport_id,
                  Name      TYPE /dmo/airport_name,
                END OF st_airport.
          
              TYPES tt_airports TYPE STANDARD TABLE OF st_airport
                                     WITH NON-UNIQUE DEFAULT KEY.

      ```
    
4. Declare a new private static attribute **airports** and type it with table type **tt_airports.**.
    
    a. At the end of the class definition, add the following code:

      ``` ABAP
          CLASS-DATA airports TYPE tt_airports.
      ```

### Task 3: Fill the Static Attribute in a Class Constructor

Define a class constructor and implement a ``SELECT`` statement that reads all available airports from the CDS view entity **/DMO/I_Airports** into the static attribute **Airports**.

#### Steps

1. Add a class constructor to the local class **lcl_connection** using a quick fix.
    
    a.  Place the cursor on the name of the class and press **Ctrl + 1**.
          
    b.  Double-click on _Generate class constructor_.
    
2. In the class constructor, implement a ``SELECT`` statement that reads all data sets from the CDS view entity **/DMO/I_Airports** into the static attribute **Airports** .
    
    a. Navigate to the implementation of the method **class_constructor**.

    b. Inside the method implementation, add the following code:

      ``` ABAP

              SELECT FROM /DMO/I_Airport
                  FIELDS AirportID, Name
                    INTO TABLE @airports.

      ```

### Task 4: Access the Content of the Internal Table

Use the content of internal table **Airports** in the method **get_output** to add the airport names to the output.

#### Steps

1. Navigate to the implementation of the method **get_output**.
    
    a. Proceed as you have done in previous exercises.
    
2. At the beginning of the method, read the details of the departure airport into a structured data object **departure**.
    
    > **Hint:** Use a table expression ``Airports[ ... ]`` and an inline declaration for the data object **departure**.
    
    a. At the beginning of the method, add the following code:

      ``` ABAP

          DATA(departure)   = airports[ airportID = details-departureairport ].

      ```
    
3. Similarly, read the details of the destination airport into a structured data object **destination**.
    
    a. Adjust the code as follows:

      ``` ABAP

            DATA(departure)   = airports[ airportID = details-departureairport   ].
            DATA(destination) = airports[ airportID = details-destinationairport ].

      ```
    
4. Use the component **name** of the two structures to add the airport names to the output.
    
    a.  Adjust the code as follows:

      ``` ABAP

          APPEND |Departure:   { details-departureairport   } { departure-name   }| TO r_output.
          APPEND |Destination: { details-destinationairport } { destination-name }| TO r_output.

      ```
    
5. **Optional:** Omit the structured data object and use the table expressions directly in the string templates.
    
    a. Comment the code lines where you fill the structures **departure** and **destination**.
          
    b. In the string template replace ``departure-name`` with ``airports[ airportid = details-departureairport ]-name``.
          
    c. Similarly replace ``destination-name`` with ``airports[ airportid = details-destination ]-name``.
    
6. Activate the class. Execute it and analyze the console output. Check that the output displays data for all attributes.
    
    a. Press **Ctrl + F3** to activate the class.
          
    b. Press **F9** to run the class.

