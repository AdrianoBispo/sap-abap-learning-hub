# Structured Data Object

## Declaring a Structured Data Object

![](https://learning.sap.com/service/media/topic/a8e17c59-d436-4fb4-b762-b1d67590d186/S4D400_24_en-US_media/S4D400_24_en-US_images/01-StructuredDataObjects_001.png)

Up until now, you have been using simple variables, each of which can store a single piece of information. Here, for example, there are two variables, one for the departure airport, the other for the arrival airport.

When you read a record from the database you need to hold all of this information together. The two variables in the example are completely independent of one another, and are therefore not suitable for storing different pieces of information that belong together.

In ABAP, the solution is to use a structured variable, or a structure, for short.

![](https://learning.sap.com/service/media/topic/a8e17c59-d436-4fb4-b762-b1d67590d186/S4D400_24_en-US_media/S4D400_24_en-US_images/01-StructuredDataObjects_002.png)

A structure is a variable ABAP data object with a name and a structured type.

In the example, data object connection_full is such a structure. It is subdivided into eight components, each of which also has a name and a type. You can address both the structure as a whole and the individual components. Importantly, you can use each component in exactly the same way that you would use a standalone simple variable.

There are various possibilities to declare a structure. You can define structured types with keyword TYPES or use a repository object of type Structure. The definitions of views and database tables can also serve as structured types. The example uses CDS View /DMO/I_Connection as a structured type.

![](https://learning.sap.com/service/media/topic/a8e17c59-d436-4fb4-b762-b1d67590d186/S4D400_24_en-US_media/S4D400_24_en-US_images/01-StructuredDataObjects_003.png)

In the debugger perspective, there are two ways to analyze the structure and content of a structured variable:

- **Variable Preview (Mouse Over):** Set the focus on the Source Code Editor and place the pointer on a variable name. After a moment a dialog window opens with details on the structure and content of the data object.

- **Display in the Variables View (Double-Click):** To display the variable in the Variables view, either enter the variable name under <enter variable> or double-click the variable name somewhere in the source code editor. Expand the node with the structure name to see the list of components

> **Tutorial:** [How To Analyze Structured Data Objects Using The Debug Perspective](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_3A36CFBA3590F685:demo)

## Structured Data Types

![](https://learning.sap.com/service/media/topic/fdf47c2b-b79a-4904-8852-982eea57a839/S4D400_24_en-US_media/S4D400_24_en-US_images/02-StructuredDataTypes_005.png)

A global structure type is a repository object that can be used as data type anywhere in the system. In the example, structured type SYMSG is used to declare variable message.

When you press the ``F2`` key to display the details of this data type you can see that this is a structure type consisting of seven components. You can also see the names, technical types and descriptions of the components.

When you press the ``F3`` key to navigate to the definition of the type, a new view opens with the ADT editor for global structured types.

The definition of a global structure type is very similar to the definition of a database table. The main part of the definition consists of a DEFINE STRUCTURE statement with the name of the structure type. This is followed by the list of structure components in a pair of curly brackets ( { , } ); each component with a component type. Component types are often described by data elements, but it is also possible to use structure types as component types. Structures with structured components are referred to as Nested Structures.

Additional code lines before the DEFINE STRUCTURE statement specify additional properties of the structure type, among them a label.

![](https://learning.sap.com/service/media/topic/fdf47c2b-b79a-4904-8852-982eea57a839/S4D400_24_en-US_media/S4D400_24_en-US_images/02-StructuredDataTypes_006.png)

You can also define structured types in an ABAP program using the TYPES statement. The structure definition begins with the statement TYPES BEGIN OF <structure type name> and ends with TYPES END OF <structure type name>. In between, you name each component and specify its type in an additional TYPES statement.

A compact form uses keyword TYPES only once, followed by a colon( : ). The BEGIN OF addition, the END OF addition and the component definitions in between are then separated by commas.

This is referred to as chain statement.

> **Note:** In the past, chain statements were used a lot in ABAP. Nowadays they are only recommended to combine statements that belong closely together.

![](https://learning.sap.com/service/media/topic/fdf47c2b-b79a-4904-8852-982eea57a839/S4D400_24_en-US_media/S4D400_24_en-US_images/02-StructuredDataTypes_007.png)

In this example, a chain statement TYPES: is used to define local structured type st_connection which consists of the three components airport_from_id, airport_to_id, and carrier_name. Each component is typed with a data element beginning with /dmo/.

Local structured type st_connection is then used in a DATA statement to type structured variable connection.

![](https://learning.sap.com/service/media/topic/fdf47c2b-b79a-4904-8852-982eea57a839/S4D400_24_en-US_media/S4D400_24_en-US_images/02-StructuredDataTypes_008.png)

In this example, variable connection has a nested structured type. Type st_nested defines 4 components. The first three are typed with data elements, therefore they are simple components. The fourth component message, however, is typed with a structured type. Therefore it is a structured component. This makes variable connection nested structure.

![](https://learning.sap.com/service/media/topic/fdf47c2b-b79a-4904-8852-982eea57a839/S4D400_24_en-US_media/S4D400_24_en-US_images/02-StructuredDataTypes_009.png)

ABAP supports not only structured variables but also structured constants. To define a structured constant, use BEGIN OF and END OF as part of a CONSTANTS statement. The example shows a structured constant that is defined in the public section of global class CL_ABAP_BEHV. All four components are typed with data element SECKEYNAME. Remember that addition VALUE is mandatory when defining constants.

## Try It Out: Structured Data Types

1. Like in the first exercise of this course, create a new global class that implements interface ``IF_OO_ADT_CLASSRUN``.
2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:

  ``` ABAP

  * Example 1 : Motivation for Structured Variables
  **********************************************************************

      DATA connection_full TYPE /DMO/I_Connection.

      SELECT SINGLE
      FROM /dmo/I_Connection
    FIELDS AirlineID, ConnectionID, DepartureAirport, DestinationAirport,
           DepartureTime, ArrivalTime, Distance, DistanceUnit
     WHERE AirlineId    = 'LH'
       AND ConnectionId = '0400'
      INTO @connection_full.

    out->write(  `--------------------------------------` ).
    out->write(  `Example 1: CDS View as Structured Type` ).
    out->write( connection_full ).

  * Example 2: Global Structured Type
  **********************************************************************

    DATA message TYPE symsg.

    out->write(  `---------------------------------` ).
    out->write(  `Example 2: Global Structured Type` ).
    out->write( message ).

  * Example 3 : Local Structured Type
  **********************************************************************

    TYPES: BEGIN OF st_connection,
             airport_from_id TYPE /dmo/airport_from_id,
             airport_to_id   TYPE /dmo/airport_to_id,
             carrier_name    TYPE /dmo/carrier_name,
           END OF st_connection.

    DATA connection TYPE st_connection.

    SELECT SINGLE
      FROM /DMO/I_Connection
    FIELDS DepartureAirport, DestinationAirport, \_Airline-Name
     WHERE AirlineID = 'LH'
       AND ConnectionID = '0400'
      INTO @connection.

    out->write(  `---------------------------------------` ).
    out->write(  `Example 3: Local Structured Type` ).
    out->write( connection ).

  * Example 4 : Nested Structured Type
  **********************************************************************

    TYPES: BEGIN OF st_nested,
             airport_from_id TYPE /dmo/airport_from_id,
             airport_to_id   TYPE /dmo/airport_to_id,
             message         TYPE symsg,
             carrier_name    TYPE /dmo/carrier_name,
           END OF st_nested.

    DATA connection_nested TYPE st_nested.

    out->write(  `---------------------------------` ).
    out->write(  `Example 4: Nested Structured Type` ).
    out->write( connection_nested ).

  ```

3. Press ``CTRL + F3`` to activate the class and ``F9`` to execute the console app.
4. Analyze the console output. Debug the program, play around with the source code to get familiar with the concepts.

## Working with Structured Data Objects

![](https://learning.sap.com/service/media/topic/e77e41c5-992e-491b-95f0-e1978835bad7/S4D400_24_en-US_media/S4D400_24_en-US_images/03-WorkWithStructDataObj_001.png)

To access a component of a structure, you have to place a minus-sign (-) between the structure name and the component name.

> **NOTE:** No blanks are allowed before or after the component selector.

Accessing a structure component that way, you can use it in any operand position in which you can use a variable of the same type. Component airport_from_id of structure connection in the example above is of type /DMO/AIRPORT_FROM_ID. In consequence, you can use this component in any operand position in which you could use a simple variable of type /DMO/AIRPORT_FROM_ID; not only on the left-hand side of a value assignment as in the example, but also on the right-hand side, in the parameter passing of a method call, in the INTO clause or WHERE clause of a SELECT statement, and so on.

![](https://learning.sap.com/service/media/topic/e77e41c5-992e-491b-95f0-e1978835bad7/S4D400_24_en-US_media/S4D400_24_en-US_images/03-WorkWithStructDataObj_002.png)

If the component of a structure is itself a structure you access the sub-components by using the component selector again after the name of the main component. The first value assignment in the example accesses component MSGTY of MESSAGE, which itself is a component of nested structure CONNECTION_NESTED.

> **Hint:** You can use code-completion to implement access to structure components. Place the cursor immediately after the structure component selector and press **CTRL + SPACE** to see a list of all available structure components.

![](https://learning.sap.com/service/media/topic/e77e41c5-992e-491b-95f0-e1978835bad7/S4D400_24_en-US_media/S4D400_24_en-US_images/03-WorkWithStructDataObj_003.png)

The VALUE #( ) expression is an elegant way to assign values to a structured data object.

If you want to fill a whole structure, you can address each component individually as you saw in the previous example.

However, you can also use a VALUE #( ) expression to fill the structure. The expression constructs a structure, fills it with value and assigns the filled structure to a variable, in this case connection. The pound sign (#) tells the ABAP runtime environment to construct a structure with the same type as the target variable connection. In the brackets, you list the components of the structure that you want to fill (it does not have to be all of them) and assign a value to them. The value can be either a literal or the contents of a variable.

When you fill a structure in this way, the runtime system deletes all existing values from the structure before refilling it with the values from your expression.

> **NOTE:** An assignment in the form connection = VALUE #( ). with just a blank between the brackets, fills all components of the structure with the type-specific initial value. This has the same effect as statement CLEAR connection.

![](https://learning.sap.com/service/media/topic/e77e41c5-992e-491b-95f0-e1978835bad7/S4D400_24_en-US_media/S4D400_24_en-US_images/03-WorkWithStructDataObj_004.png)

In ABAP, you may only copy the contents of one structure directly into another structure using the notation <target structure> = <source structure> if the two structure types are compatible. This is generally only the case if both structures have the same type. If the structures have different types, two things can happen:

- If one of the structures has a non-char-like component at a position where the other structure has a char-like component, direct assignment leads to a syntax error.
- If both structures are char-like, or, in other words, both structures consist of char-like components, only, direct assignment is technically possible. But usually, the result will be wrong.

In the example, source structure and target structure are char-like. Therefore, direct assignment is technically possible. But because they are not compatible the result is wrong: The content of component carrier_name is copied to component message in the target structure.

Because there is no syntax error, you have to be extra careful when working with non-compatible char-like structures.

![](https://learning.sap.com/service/media/topic/e77e41c5-992e-491b-95f0-e1978835bad7/S4D400_24_en-US_media/S4D400_24_en-US_images/03-WorkWithStructDataObj_005.png)

When you copy data between structures, you usually want to copy information from one field into the corresponding field of the target structure - airport_from_id to airport_from_id, airport_to_id to airport_to_id, and so on. To achieve this in ABAP, use the CORRESPONDING expression. This assigns values from <source_structure> to the corresponding, that is, identically-named components of . <target_structure>. You must remember the following points:

- The fields must have identical names.
- The components do not have to be in the same position or sequence in the two structures.
- If the fields have different types, ABAP attempts a type conversion according to the predefined set of rules.

> **NOTE:** The target structure is initialized before being re-filled with the result of the expression.

## Try It Out: Access to Structured Data Objects

1. Like in the first exercise of this course, create a new global class that implements interface *IF_OO_ADT_CLASSRUN*.
2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:

  ``` ABAP

      TYPES: BEGIN OF st_connection,
               airport_from_id TYPE /dmo/airport_from_id,
               airport_to_id   TYPE /dmo/airport_to_id,
               carrier_name    TYPE /dmo/carrier_name,
            END OF st_connection.
    
      TYPES: BEGIN OF st_connection_nested,
               airport_from_id TYPE /dmo/airport_from_id,
               airport_to_id   TYPE /dmo/airport_to_id,
               message         TYPE symsg,
               carrier_name    TYPE /dmo/carrier_name,
            END OF st_connection_nested.
    
      DATA connection TYPE st_connection.
      DATA connection_nested TYPE st_Connection_nested.
    
  * Example 1: Access to structure components
  *********************************************************************
    
      connection-airport_from_id = 'ABC'.
      connection-airport_to_id   = 'XYZ'.
      connection-carrier_name    = 'My Airline'.
    
      "Access to sub-components of nested structure
      connection_nested-message-msgty = 'E'.
      connection_nested-message-msgid = 'ABC'.
      connection_nested-message-msgno = '123'.
    
  * Example 2: Filling a structure with VALUE #(    ).
  *********************************************************************
      
    CLEAR connection.
    
      connection = VALUE #( airport_from_id = 'ABC'
                            airport_to_id   = 'XYZ'
                            carrier_name    = 'My Airline'
                          ).
    
      " Nested VALUE to fill nested structure
      connection_nested = VALUE #( airport_from_id = 'ABC'
                                    airport_to_id   = 'XYZ'
                                    message         = VALUE #( msgty = 'E'
                                                                msgid = 'ABC'
                                                                msgno = '123' )
                                    carrier_name    = 'My Airline'
                                ).
    
  * Example 3: Wrong result after direct assignment
  *********************************************************************
    
      connection_nested = connection.
    
      out->write(  `-------------------------------------------------------------` ).
      out->write(  `Example 3: Wrong Result after direct assignment` ).
    
      out->write( data = connection
                    name = `Source Structure:`).
    
      out->write( |Component connection_nested-message-msgid: { connection_nested-message-msgid }| ).
      out->write( |Component connection_nested-carrier_name : { connection_nested-carrier_name  }| ).
    
  * Example 4: Assigning Structures using CORRESPONDING #( )
  *********************************************************************
      CLEAR connection_nested.
      connection_nested = CORRESPONDING #(  connection ).  "
    
      out->write(  `-------------------------------------------------------------` ).
      out->write(  `Example 4: Correct Result after assignment with CORRESPONDING` ).
    
      out->write( data = connection
                  name = `Source Structure:`).
    
      out->write( |Component connection_nested-message-msgid: { connection_nested-message-msgid }| ).
      out->write( |Component connection_nested-carrier_name : { connection_nested-carrier_name  }| ). 

    ```
    
3.  Press ``CTRL + F3`` to activate the class and ``F9`` to execute the console app.
4.  Analyze the console output. Debug the program, play around with the source code to get familiar with the concepts.

## Structured Data Objects in ABAP SQL

![](https://learning.sap.com/service/media/topic/b1d3006e-c73e-488c-96da-79f96e0c5895/S4D400_24_en-US_media/S4D400_24_en-US_images/04-UseStructDataObjInABAPSQL_001.png)

The INTO clause of the SELECT statement will only work correctly if the number and types of the components of the structure correspond to the number and types of the columns specified in the FIELDS clause. In the above example, the statement can only work if the target structure connection has three components with the same type and length as the columns DepartureAirport, DestinationAirport, and \_Airline-Name listed in the FIELDS clause. Note that, in this case, the names do not have to be identical - the system fills the target structure from left to right.

If the field list in the FIELDS clause does not match the structure or table line type in the INTO clause, a runtime error will occur.

![](https://learning.sap.com/service/media/topic/b1d3006e-c73e-488c-96da-79f96e0c5895/S4D400_24_en-US_media/S4D400_24_en-US_images/04-UseStructDataObjInABAPSQL_002.png)

The example shows an easy technique to ensure that the target structure matches the field selection:

- The target structure is typed with CDS view */DMO/I_Connection*, which is the data source in the FROM clause.
- The asterisk sign (*) after keyword FIELDS is a short notation to makes sure, that all fields of the view are part of the field selection. Exposed associations are ignored.

> **NOTE:** This technique is also available when you read directly from a database table. Just like CDS view definitions, database table definitions can also serve as global structure types in ABAP.

The main advantage of this technique is, that the SELECT statement stays syntactically intact even if you or someone else makes changes to CDS view or database table. The most important drawback is, that you always read all fields from the database, whether you actually need them or not.

> **NOTE:** Only use this technique for views and tables with a small number of fields and if you actually need all the fields. Unnecessary reading of data from the database is a major cause for performance problems.

![](https://learning.sap.com/service/media/topic/b1d3006e-c73e-488c-96da-79f96e0c5895/S4D400_24_en-US_media/S4D400_24_en-US_images/04-UseStructDataObjInABAPSQL_003.png)

Another way to avoid syntax errors is variant INTO CORRESPONDING FIELDS. This variant has the same effect as the CORRESPONDING #( ) operator that you learned about earlier. It ensures that data is copied between identically-named components. By defining the structure type according to your needs you can ensure that only the required data is read.

Once again, only the names must be identical. But to avoid problems you should make sure that identically-named components have compatible types. Otherwise the system attempts to convert the contents of the source field into the type of the target field. This can lead to data loss or (catchable) runtime errors.

![](https://learning.sap.com/service/media/topic/b1d3006e-c73e-488c-96da-79f96e0c5895/S4D400_24_en-US_media/S4D400_24_en-US_images/04-UseStructDataObjInABAPSQL_004.png)

If the field names in the data source and the component names in the target structure do not match, the combination of ``FIELDS *`` and ``INTO CORRESPONDING FIELDS OF`` does not work.

If you want to keep variant ``INTO CORRESPONDING FIELDS OF``, you can define alias names for the selected fields in the field list. For this, add addition ``AS`` after the field name, followed by the alias name. In the example, the alias name for view field DepartureAirport is airport_from_id and the alias name for path expression \_Airline-Name is carrier_name. Based on this alias names, ``INTO CORRESPONDING FIELDS OF`` correctly identifies the structure component in which to store the retrieved data.

![](https://learning.sap.com/service/media/topic/b1d3006e-c73e-488c-96da-79f96e0c5895/S4D400_24_en-US_media/S4D400_24_en-US_images/04-UseStructDataObjInABAPSQL_005.png)

The simplest technique to avoid conflicts between the field selection and the target structure is an inline declaration in the INTO clause. The sequence, type and name of the inline declared structure is derived from the FIELD clause. Therefore the target structure always fits the field selection.

> **NOTE:** Inline declarations are only supported after INTO. You cannot use inline declarations after INTO CORRESPONDING FIELDS OF.

If you use an inline declaration in the INTO clause, you have to provide a name for each element in the FIELDS clause. For fields of the data source, this can be the field name itself or, optionally an alias name. For expressions, the alias name becomes mandatory.

In the example, there is no alias for field DepartureAirport. The name of the field is used as component name in structure connection_inline. Field DestinationAirport has an optional alias ArrivalAirport. In this case the alias is used as component name. The alias for path expression \_Airline-Name is mandatory.

![](https://learning.sap.com/service/media/topic/b1d3006e-c73e-488c-96da-79f96e0c5895/S4D400_24_en-US_media/S4D400_24_en-US_images/04-UseStructDataObjInABAPSQL_006.png)

When working with a relational database you often face the problem that you have to read related data from different database tables. We already learned that associations in CDS views are an elegant way to perform this task.

If no CDS View with suitable associations exist you can implement SQL joins, instead. The example above illustrates the principle of joins:

We are interested in flight connections and the airports they connect with each other. We find the 3-letter IDs of the airports in database table /DMO/CONNECTION. The full names of the airports are stored in database table /DMO/AIRPORT.

To retrieve a connection with the departure airport name, in one SELECT statement, we read connection data from DB table /DMO/CONNECTION and join it with DB table /DMO/AIRPORT.

A join consists of the following building blocks:

**Data Sources:** The Database tables and views to join with each other. A single join always combines a left-hand data source with a right-hand data source. In the example above, table /DMO/CONNECTION is the left-hand data source and table /DMO/AIRPORT the right-hand data source. ABAP SQL also supports joins of joins (nested joins)

**Join Condition:** The join condition specifies which records of the right-hand data source belong to a record from the left-hand data source. In the example above, the related departure airport is identified by the value in columns CLIENT and AIRPORT_ID. The join condition reads:

  /DMO/CONNECTION~CLIENT = /DMO/AIRPORT~CLIENT
  AND
  /DMO/CONNECTION~AIRPORT_FROM_ID = /DMO/AIRPORT~AIRPORT_ID

**Join Type:** The join type has an influence on the result if one of the data sources does not contain a matching records. ABAP SQL currently supports INNER JOIN, LEFT OUTER JOIN, and RIGHT OUTER JOIN. The most common join type is a LEFT OUTER JOIN.

![](https://learning.sap.com/service/media/topic/b1d3006e-c73e-488c-96da-79f96e0c5895/S4D400_24_en-US_media/S4D400_24_en-US_images/04-UseStructDataObjInABAPSQL_007.png)

The figure shows the ABAP SQL syntax for a join. In the FROM clause, the join type is specified by keywords LEFT OUTER JOIN between the left-hand data source /dmo/connection and the right-hand data source /dmo/airport. The syntax introduces alias names c and f for the data sources. Alias names for data sources are optional, unless a data source appears more than once in the join.

The join condition follows keyword ON. The separator between the data source or its alias and the field is the tilde sign (~).

> **NOTE:** In ABAP SQL, it is not necessary to mention the client fields. They are added to the join condition by the database interface before the statement is sent to the database. If the FROM clause defines a join, you can use fields from both data sources in the FIELDS and WHERE clauses.

![](https://learning.sap.com/service/media/topic/b1d3006e-c73e-488c-96da-79f96e0c5895/S4D400_24_en-US_media/S4D400_24_en-US_images/04-UseStructDataObjInABAPSQL_008.png)

In this example, the SELECT statement not only read the departure airport name but also the destination airport name. To do so, the FROM clause defines a nested join:

The first join is a left outer join of tables /dmo/connection and /dmo/airport, introducing alias "f" (like "from") for the right-hand data source. This first join is then used as left-hand data source for a second left outer join that has table /dmo/airport as right-hand data source. Note that in this case alias "t" (for "to") is crucial to distinguish this appearance of table /dmo/airport from the previous one.

The FIELDS clause, lists the airport names from both data sources, introducing aliases airport_from_name and airport_to_name to distinguish them from each other.

> **Hint:** The brackets around the first join are optional. If they are omitted, the joins in the from clause are evaluated from left to right.

## Try It Out: Structured Data Objects in ABAP SQL

1. Like in the first exercise of this course, create a new global class that implements interface ``IF_OO_ADT_CLASSRUN``.
2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:

  ``` ABAP

      TYPES: BEGIN OF st_connection,
                airport_from_id TYPE /dmo/airport_from_id,
                airport_to_id   TYPE /dmo/airport_to_id,
                carrier_name    TYPE /dmo/carrier_name,
             END OF st_connection.
    
      TYPES: BEGIN OF st_connection_short,
                DepartureAirport   TYPE /dmo/airport_from_id,
                DestinationAirport TYPE /dmo/airport_to_id,
             END OF st_connection_short.
    
    
      DATA connection TYPE st_connection.
    
      DATA connection_short TYPE st_connection_short.
    
      DATA connection_full TYPE /DMO/I_Connection.
    
  * Example 1: Correspondence between FIELDS and INTO
  *********************************************************************
    
      SELECT SINGLE
          FROM /DMO/I_Connection
        FIELDS DepartureAirport, DestinationAirport, \_Airline-Name
          WHERE AirlineID = 'LH'
            AND ConnectionID = '0400'
           INTO @connection.
    
      out->write(  `------------------------------` ).
      out->write(  `Example 1: Field List and INTO` ).
      out->write( connection ).
    
  * Example 2: FIELDS *
  *********************************************************************
    
      SELECT SINGLE
          FROM /DMO/I_Connection
        FIELDS *
          WHERE AirlineID = 'LH'
            AND ConnectionID = '0400'
           INTO @connection_full.
    
      out->write(  `----------------------------` ).
      out->write(  `Example 2: FIELDS * and INTO` ).
      out->write( connection_full ).
    
  * Example 3: INTO CORRESPONDING FIELDS
  *********************************************************************
    
      SELECT SINGLE
          FROM /DMO/I_Connection
      FIELDS *
        WHERE AirlineID    = 'LH'
          AND ConnectionID = '0400'
         INTO CORRESPONDING FIELDS OF @connection_short.
    
      out->write(  `----------------------------------------------------` ).
      out->write(  `Example 3: FIELDS * and INTO CORRESPONDING FIELDS OF` ).
      out->write( connection_short ).
    
  * Example 4: Alias Names for Fields
  *********************************************************************
    
      CLEAR connection.
    
      SELECT SINGLE
          FROM /DMO/I_Connection
      FIELDS DepartureAirport AS airport_from_id,
             \_Airline-Name   AS carrier_name
        WHERE AirlineID    = 'LH'
          AND ConnectionID = '0400'
         INTO CORRESPONDING FIELDS OF @connection.
    
      out->write(  `---------------------------------------------------` ).
      out->write(  `Example 4: Aliases and INTO CORRESPONDING FIELDS OF` ).
      out->write( connection ).
    
  * Example 5: Inline Declaration
  *********************************************************************
    
      SELECT SINGLE
          FROM /DMO/I_Connection
        FIELDS DepartureAirport,
               DestinationAirport AS ArrivalAirport,
               \_Airline-Name     AS carrier_name
        WHERE AirlineID    = 'LH'
          AND ConnectionID = '0400'
         INTO @DATA(connection_inline).
    
      out->write(  `-----------------------------------------` ).
      out->write(  `Example 5: Aliases and Inline Declaration` ).
      out->write( connection_inline ).
    
  * Example 6: Joins
  *********************************************************************
    
      SELECT SINGLE
        FROM (  /dmo/connection AS c
        LEFT OUTER JOIN /dmo/airport AS f
          ON c~airport_from_id = f~airport_id )
        LEFT OUTER JOIN /dmo/airport AS t
          ON c~airport_to_id = t~airport_id
      FIELDS c~airport_from_id, c~airport_to_id,
             f~name AS airport_from_name, t~name AS airport_to_name
       WHERE c~carrier_id    = 'LH'
        AND c~connection_id = '0400'
       INTO @DATA(connection_join).
    
      out->write(  `------------------------------------------` ).
      out->write(  `Example 6: Join of Connection and Airports` ).
      out->write( connection_join ).

  ```

3. Press ``CTRL + F3`` to activate the class and ``F9`` to execute the console app.
4. Analyze the console output. Debug the program, play around with the source code to get familiar with the concepts.

## Use a Structured Data Object

In this exercise, you declare a structured attribute, fill it using a SELECT statement and access the structure components.

- **Template:** ``/LRN/CL_S4D400_DBS_CDS`` (global Class)

- **Solution:** ``/LRN/CL_S4D400_STS_STRUCTURE`` (global Class)

### Task 1: Copy Template

Copy the template class. Alternatively, copy your solution of the previous exercise.

#### Steps

1. Copy the class **/LRN/CL_S4D400_DBS_CDS** to a class in your own package (suggested name: **ZCL_##_STRUCTURE**, where ## stands for your group number).
    
    a. Open the source code of the global class **/LRN/CL_S4D400_DBS_CDS**.
          
    b. Link the _Project Explorer_ view with the editor.
          
    c. In the _Project Explorer_ view, right-click the class **/LRN/CL_S4D400_DBS_CDS** to open the context menu.
          
    d. From the context menu, choose _Duplicate ..._.
          
    e. Enter the name of your package in the _Package_ field. In the _Name_ field, enter the name **ZCL_##_STRUCTURE**, where ## stands for your group number.
          
    f. Adjust the description and choose _Next_.
          
    g. Confirm the transport request and choose _Finish_.

### Task 2: Declare a Structured Data Object

In the local class, declare a structured attribute **details** to replace the scalar attributes **airport_from_id**, **airport_to_id**, and **carrier_name**. Begin by defining a private structure type **st_details** inside the local class.

#### Steps

1. Switch to the local class **lcl_connection**.
    
    a. In the global class, choose _Local Types_.
    
2. Define a **private** structure type **st_details** with the following components:
    
    | Component Name | Data Type |
    | --- | --- |
    | **DepartureAirport** | ``/dmo/airport_from_id`` |
    | **DestinationAirport** | ``/dmo/airport_to_id`` |
    | **AirlineName** | ``/dmo/carrier_name`` |
    
    a. After line ``PRIVATE SECTION.``, add the following code:
          
      ``` ABAP

          TYPES: 
            BEGIN OF st_details,
                  DepartureAirport   TYPE /dmo/airport_from_id,
                  DestinationAirport TYPE   /dmo/airport_to_id,
                  AirlineName        TYPE   /dmo/carrier_name,
            END OF st_details.

      ```

3. Comment or remove the declaration of attributes **airport_from_id**, **airport_to_id**, and **carrier_name**.
    
    a. Select the lines with the three DATA statements.
          
    b. Press **Ctrl + <** to add a star sign (*) in front of each selected line.
    
4. Declare a new private instance attribute **details** and type it with structure type **st\_details.**.
    
    a. Adjust the code as follows:

      ``` ABAP

      *    DATA airport_from_id TYPE /dmo/airport_from_id.
      *    DATA airport_to_id   TYPE /dmo/airport_to_id.
      *
      *    DATA carrier_name    TYPE /dmo/carrier_name.
          
          DATA details TYPE st_details.
  
      ```

### Task 3: Access Structure Components

Use the components of the structured attribute **details** in the method **get_output**.

#### Steps

1. Adjust the implementation of the method **get_output**. Replace any access to the attributes **airport_from_id**, **airport_to_id**, and **carrier_name** with the corresponding component of attribute **details**.
    
    > **Hint:** Do not type in the component names manually! After typing the structure component selector (-), press **Ctrl + Space** to get a list of all components.
    
    a. Navigate to the implementation of the method **get_output**.
          
    b. Adjust the APPEND statements as follows:
          
      ``` ABAP

      *     APPEND |--------------------------------|             TO r_output.
      *     APPEND |Carrier:     { carrier_id } { carrier_name }| TO r_output.
      *     APPEND |Connection:  { connection_id   }|             TO r_output.
      *     APPEND |Departure:   { airport_from_id }|             TO r_output.
      *     APPEND |Destination: { airport_to_id   }|             TO r_output.
          
          APPEND |--------------------------------|                    TO r_output.
          APPEND |Carrier:     { carrier_id } { details-airlinename }| TO r_output.
          APPEND |Connection:  { connection_id   }|                    TO r_output.
          APPEND |Departure:   { details-departureairport     }|       TO r_output.
          APPEND |Destination: { details-destinationairport   }|       TO r_output.

      ```

### Task 4: Fill the Structured Attribute in the SELECT Statement

Use the structured attribute as the target of the SELECT statement in the **constructor** method.

#### Steps

1. Adjust the SELECT statement in the implementation of the **constructor** method. Replace the list of data objects in the INTO clause with the structured attribute **details**.
    
    a. Navigate to the implementation of method **constructor**.
          
    b. Adjust the SELECT statement as follows:

      ``` ABAP

      SELECT SINGLE
            FROM /DMO/I_Connection
          FIELDS DepartureAirport, DestinationAirport, \_Airline-Name 
            WHERE AirlineID    = @i_carrier_id
              AND ConnectionID = @i_connection_id
      *        INTO ( @airport_from_id, @airport_to_id, @carrier_name  ).
             INTO @details.

      ```

2. Optional: Use syntax variant ``INTO CORRESPONDING FIELDS OF @details.``.
    
    a. In the SELECT statement, replace ``INTO @details.`` with the following code:
          
      ``` ABAP

          INTO CORRESPONDING FIELDS OF @details.

      ```
          
    b. Add alias name **AirlineName** for the path expression.
          
    c. The SELECT statement should now look like this:

      ``` ABAP

      SELECT SINGLE
            FROM /DMO/I_Connection
          FIELDS DepartureAirport, DestinationAirport, \_Airline-Name as AirlineName
            WHERE AirlineID    = @i_carrier_id
              AND ConnectionID = @i_connection_id
             INTO CORRESPONDING FIELDS OF @details.
 
      ```

3. Activate the class. Execute it and analyze the console output. Check that the output displays data for all attributes.

      1. Press **Ctrl + F3** to activate the class.

      2. Press **F9** to run the class.

 