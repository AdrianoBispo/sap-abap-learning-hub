# Reading Data from the Database

## Database Table Definitions

Every ABAP system runs on a relational database management system (DBMS). Originally, ABAP supported different database management systems from different vendors. Therefore the definition of database objects like tables and views is done in a database independent way.

> **NOTE:** Newer ABAP releases only support SAP HANA as database. The ABAP Environment on SAP BTP always runs on SAP HANA.

![](https://learning.sap.com/service/media/topic/e2a52f9f-2937-4bbf-983b-9808b1390bd1/S4D400_24_en-US_media/S4D400_24_en-US_images/11_analyze_table_001.png)

In a relational database, information is stored in two-dimensional tables, in which each row represents one data record whose contents are split up into columns. The database is called a relational database, as there are also relations between tables. For example, the table in the figure contains an airline code. In the database table, there would be a relationship to a further table containing the information that AA stands for American Airlines, JL for Japanese Airline, LH for Lufthansa, and so on.

A sequence of columns at the beginning of each database table forms its key. The key is a combination of values that ensures that each row in the table can be identified uniquely.

In SAP systems, database table definitions are development objects, and as such, are cross-client. However, the vast majority of tables contain business data, which is client-specific. To keep the data separate, client-specific tables have a client field (often named CLIENT or MANDT) as their first key field. The database of SAP accesses statements using ABAP SQL to ensure that a statement only manipulates data from the current client.

> **Tutorial:** [How to Analyze a Database Table](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_44B67792CC5AACB1:demo)

## Basic ABAP SQL Syntax

### Structured Query Language (SQL)

![](https://learning.sap.com/service/media/topic/df026ee6-8769-4a24-a451-3d6621e6922b/S4D400_24_en-US_media/S4D400_24_en-US_images/12_-Basic_ABAP_SQL_001.png)

All relational database systems use a variant of Structured Query Language (SQL) to allow you to work with them. Standard SQL consists of three main components:

- **Data Manipulation Language (DML):** DML comprises of the statements that you need to work with information in database tables. These are INSERT, for adding new records, SELECT, for reading data, UPDATE, to change existing records, and DELETE, to delete table contents. These statements are reflected in ABAP in the form of ABAP SQL.

- **Data Definition Language (DDL):** Data Definition Language provides you with commands that enable you to create and delete database tables and other database artifacts such as schemata, table indexes, and views. In an SAP system, you perform these tasks using dedicated editors of ADT, not with ABAP programs. One example is the editor for database table definitions we saw earlier.

- **Data Control Language (DCL):** Data Control Language is used in SQL to restrict access to data in the database for a particular user. It is not used in its classic form in ABAP, since the users at database level do not correspond one-to-one with the end users. Consequently, ABAP has its own authorization concept.

### ABAP SQL Architecture

![](https://learning.sap.com/service/media/topic/df026ee6-8769-4a24-a451-3d6621e6922b/S4D400_24_en-US_media/S4D400_24_en-US_images/12_-Basic_ABAP_SQL_002.png)

In the past, SAP systems had to support a range of database platforms, and each platform had a slightly different implementation of the SQL standard. This meant that each platform needed slightly different commands to achieve a particular task. To avoid the ABAP code being database-specific, SAP invented ABAP SQL - or Open SQL as it was called originally.

> **NOTE:** The name change from Open SQL and ABAP SQL also illustrates that as off release 7.53 ABAP only supports SAP HANA as DBMS.

ABAP SQL is an abstract set of SQL commands implemented at ABAP level and integrated into the ABAP language. At runtime, ABAP SQL is translated into a variant of SQL that the database understands. This variant is called Native SQL to distinguish it from ABAP SQL, the SQL variant that is integrated into ABAP. The translation from ABAP SQL to Native SQL takes place in the database interface, a component of the ABAP system that consists of a general part and a database-specific library.

Even though newer ABAP releases only support SAP HANA as DBMS, SAP has still retained the concept of ABAP SQL and the database interface. This is because of the following reasons:

- **Architecture compatibility:** ABAP SQL and the database interface are an integral part of the system architecture.

- **Code compatibility:** ABAP SQL coding from previous SAP products (including customer-specific development) should run free of side-effects in the modern, SAP-HANA-only ABAP environments.

- **Tasks of the Database Interface:** The database interface does not just translate statements; it is also responsible for ABAP specific tasks like, for example, automatic client-handling.

## Single Record Reading from the Database

### The SELECT Statement in ABAP

![](https://learning.sap.com/service/media/topic/ce9b94bd-ee88-44ee-bc3a-311fd36d4734/S4D400_24_en-US_media/S4D400_24_en-US_images/13_-_Read_single_record_001.png)

To read data from the database, you use the SELECT statement.

When you write a SELECT statement in ABAP SQL, the syntax check compares what you have written with the definition of the tables and views. If you try to address tables, views, or fields that do not exist, a syntax error occurs.

The basic syntax of the SELECT statement contains several sections, called clauses, and always follows the pattern in the figure, The SELECT Statement in ABAP. The most important clauses of a SELECT statement are as follows:

**FROM:** In the FROM clause of the SELECT statement, you specify the data source from which you want to read. This can be either be a database table or a view. Special SQL techniques allow you to combine data from multiple sources in the same SELECT statement.

**FIELDS:** In the FIELDS clause of the SELECT statement, you list the columns of the database table that you want to read. The columns in the list must be separated by commas. If you want to read the entire table line, you can specify FIELDS * instead of a column list. Be aware, however, that this can cause the database considerably more work than just reading the columns you need.

**WHERE:** In the WHERE clause, you can specify a condition that describes which rows of the table will be read. For example, the condition WHERE carrier_id = 'LH' means that only those rows will be read (in which the column CARRIER_D contains the value LH).

  The WHERE clause can contain multiple conditions linked with the AND and OR operators. For example, WHERE carrier_id = 'LH' and connection_id = '0400' would return the data of flight connection LH 0400. You can also negate conditions using NOT. The WHERE clause is the only clause that is optional. Be aware, however, that without a WHERE clause you read all data from the table, or if the table has a client field - all data that belongs to the logon client of the user. SELECTs without a WHERE clause can cause serious performance problems and should be avoided.

**INTO:** The INTO clause specifies the variable or variables in the ABAP program into which the data should be placed. This is normally a structure, or an internal table, and should ideally have the same sequence of components as the column list in the FIELDS clause.

> **NOTE:** You will see other forms of SQL syntax in ABAP. These are older and have been retained to ensure compatibility. You should get used to using the modern syntax, as it provides far more functions and features than the old form.

![](https://learning.sap.com/service/media/topic/ce9b94bd-ee88-44ee-bc3a-311fd36d4734/S4D400_24_en-US_media/S4D400_24_en-US_images/13_-_Read_single_record_002.png)

The figure, Example 1: Reading Single Field of Single Record, illustrates a SELECT statement that reads a single value from the database. The FROM clause tells us that the statement reads from database table /DMO/CONNECTION. The option, SINGLE, after the keyword, SELECT, indicates that only one row (a single record) is read. This row is identified in the WHERE clause by providing key filter values for key fields carrier_id and connection_id. Keep in mind, that the database interface will add a filter on the remaining key field client.

The FIELDS clause lists only one column of the table: column AIRPORT_FROM_ID.

The INTO clause has to match the rest of the statement. In our example, this specifies the variable airport_from_id as the target object, a scalar data object of identical type as table field, airport_from_id.

> **Note:** The at sign (@) identifies airport_from_id as the name of an ABAP data object. It is mandatory for all variables and constants that you use in an ABAP SQL statement. It is needed to avoid ambiguities if, for example, a data object and table field have the same name.

![](https://learning.sap.com/service/media/topic/ce9b94bd-ee88-44ee-bc3a-311fd36d4734/S4D400_24_en-US_media/S4D400_24_en-US_images/13_-_Read_single_record_003.png)

The figure, Example 2: Reading Several Fields of Single Record, illustrates a SELECT statement that reads two values from the same record of the database.

This time, the FIELDS clause lists two columns of the table; column AIRPORT_FROM_ID and column AIRPORT_TO_ID.

To match this, the INTO clause specifies the variables airport_from_id and airport_to_id as the target objects. They are separated by a comma and surrounded by pair of brackets to make it clear that together they form the target of the SELECT statement.

![](https://learning.sap.com/service/media/topic/ce9b94bd-ee88-44ee-bc3a-311fd36d4734/S4D400_24_en-US_media/S4D400_24_en-US_images/13_-_Read_single_record_004.png)

When you implement a SELECT statement you always have to take into account that there could be no result, either because the database table does not contain any data at all or because it does not contain any rows that fulfill the conditions in the WHERE clause. In the example from the figure above, the database table does not contain a row with carrier_id = 'XX' and connection_id = '1234'.

ABAP SQL use system field SY-SUBRC to indicate a successful or unsuccessful execution of a statement. System field SY-SUBRC is of type integer. Initial value 0 always indicates a successful execution. If, after a SELECT statement, SY-SUBRC contains the value 4, this indicates that the database returned an empty result.

If the database returns an empty result, ABAP SQL does NOT touch the target variable after INTO!

In particular, the target variable is not initialized in case of an error.

![](https://learning.sap.com/service/media/topic/ce9b94bd-ee88-44ee-bc3a-311fd36d4734/S4D400_24_en-US_media/S4D400_24_en-US_images/13_-_Read_single_record_005.png)

It is recommended that you evaluate the content of system field sy-subrc immediately after each SELECT statement.

## Try It Out: SELECT Examples

1. Like in the first exercise of this course, create a new global class that implements interface ``IF_OO_ADT_CLASSRUN``.
2. Copy the following code snippet to the implementation part of method ``if\_oo\_adt\_classrun~main( )``:

  ``` ABAP
      DATA airport_from_id TYPE /DMO/airport_from_id.
      DATA airport_to_id   TYPE /DMO/airport_to_id.
    
      DATA airports TYPE TABLE OF /DMO/airport_from_id.
    
  * Example 1: Single field from Single Record
  *********************************************************************
      SELECT SINGLE
        FROM /dmo/connection
        FIELDS airport_from_id
        WHERE carrier_id    = 'LH'
          AND connection_id = '0400'
          INTO @airport_from_id.
    
      out->write( `----------`  ).
      out->write( `Example 1:`  ).
    
      out->write( |Flight LH 400 departs from {  airport_from_id }.| ).
    
  * Example 2: Multiple Fields from Single Record
  *********************************************************************
      SELECT SINGLE
        FROM /dmo/connection
        FIELDS airport_from_id, airport_to_id
        WHERE carrier_id    = 'LH'
          AND connection_id = '0400'
          INTO (  @airport_from_id, @airport_to_id ).
    
      out->write( `----------`  ).
      out->write( `Example 2:`  ).
    
      out->write( |Flight LH 400 flies from {  airport_from_id } to { airport_to_id  }| ).
    
  * Example 3: Empty Result and sy-subrc
  *********************************************************************
      SELECT SINGLE
        FROM /dmo/connection
        FIELDS airport_from_id
        WHERE carrier_id    = 'XX'
          AND connection_id = '1234'
          INTO @airport_from_id.
    
      IF sy-subrc = 0.
    
        out->write( `----------`  ).
        out->write( `Example 3:`  ).
        out->write( |Flight XX 1234 departs from {  airport_from_id }.| ).
    
      ELSE.
    
        out->write( `----------`  ).
        out->write( `Example 3:`  ).
        out->write( |There is no flight XX 1234, but still airport_from_id = {  airport_from_id }!| ).
    
      ENDIF.
  ```

3. Press ``CTRL + F3`` to activate the class and ``F9`` to execute it as a console app.
4. Analyze the console output. Debug the program, play around with the source code to get familiar with the concepts.

## Exercise - Read Data from a Database Table

In this exercise, you extend your local class with attributes for the departure airport and the destination airport, and read the values for these attributes from a database table.

- **Template:** ``/LRN/CL_S4D400_CLS_CONSTRUCTOR`` (global Class)

- **Solution:** ``/LRN/CL_S4D400_DBS_SELECT`` (global Class)

### Task 1: Copy Template

Copy the template class. Alternatively, copy your solution of the previous exercise.

#### Steps

1. Copy the class **/LRN/CL_S4D400_CLS_CONSTRUCTOR** to a class in your own package (suggested name: **ZCL_##_SELECT**, where ## stands for your group number).
    
    a. Open the source code of the global class **/LRN/CL_S4D400_CLS_CONSTRUCTOR**.
          
    b. Link the _Project Explorer_ view with the editor.
          
    c. In the _Project Explorer_ view, right-click class **/LRN/CL_S4D400_CLS_CONSTRUCTOR** to open the context menu.
          
    d. From the context menu, choose _Duplicate ..._.
          
    e. Enter the name of your package in the _Package_ field. In the _Name_ field, enter the name **ZCL_##_SELECT**, where ## stands for your group number.
          
    f. Adjust the description and choose _Next_.
          
    g. Confirm the transport request and choose _Finish_.

### Task 2: Declare Additional Attributes

Extend the local class **lcl_connection** with the private instance attributes **airport_from_id**and **airport_to_id**. Add some output for the new attributes to the implementation of the method **get_output**.

> **NOTE:** The new attributes are not filled yet. In order to fill them we will add a SELECT statement to the **constructor** in one of the next tasks of this exercise.

#### Steps

1. Switch to the local class **lcl_connection**.

    a. In the global class, choose _Local Types_.

2. Add the following **private** attributes to the class definition:

    | Attribute Name | Scope | Data Type |
    | --- | --- | --- |
     **airport_from_id** | instance | ``/DMO/AIRPORT_FROM_ID`` |
     **airport_to_id** | instance | ``/DMO/AIRPORT_TO_ID`` |

    a. Adjust the code as follows:

      ``` ABAP
        PRIVATE SECTION.
          DATA carrier_id    TYPE /dmo/carrier_id.
          DATA connection_id TYPE /dmo/connection_id.
          
          DATA airport_from_id TYPE /dmo/airport_from_id.
          DATA airport_to_id   TYPE /dmo/airport_to_id.
          
      ENDCLASS. 
      ```

3. Extend the implementation of the method **get_output**. Append more string templates to the returning parameter **r_output**. Embed the new attributes as expressions into the string templates.

    a.  Navigate to the implementation of the method **get_output**.

    b.  Adjust the code as follows:

      ``` ABAP
            APPEND |--------------------------------|             TO r_output.
            APPEND |Carrier:     { carrier_id      }|             TO r_output.
            APPEND |Connection:  { connection_id   }|             TO r_output.
            APPEND |Departure:   { airport_from_id }|             TO r_output.
            APPEND |Destination: { airport_to_id   }|             TO r_output.
      ```
    
4. Activate the class. Execute it and analyze the console output.
    
    a. Press **Ctrl + F3** to activate the class.
          
    b. Press **F9** to run the class.

### Task 3: Analyze the Database Table

Analyze the definition of the database table **/DMO/CONNECTION**.

#### Steps

1. Open the development object that contains the definition of the database table **/DMO/CONNECTION**.

    a. From the eclipse toolbar, choose _Open ABAP Development Object_ or press **Ctrl + Shift + A**.
          
    b. In the input field, enter **/dmo/con** as a search string.
          
    c. In the list of matching items, click on _/DMO/CONNECTION (Database Table)_ and choose _OK_.

2. Open the _Tooltip Description_ for the database table ``/DMO/CONNECTION``.
    
    a. Click on _/dmo/connection_ after the keyword define table and press **F2** to show the tooltip description.

### Task 4: Read Data from the Database

In the method **constructor** of the local class **lcl_connection**, implement a SELECT statement that reads values for the new attributes from the database table **/DMO/CONNECTION**.

#### Steps

1. Return to the local class **lcl_connection** in your global class.
    
    a. In the editor view of Eclipse, open tab ``ZCL_##_SELECT``.
          
    b. In the global class, choose _Local Types_.
    
2. Navigate to the implementation of the method **constructor**.
    
    a. Search for the code line METHOD constructor..
    
3. After the ENDIF. statement, add a SELECT statement that reads a single record from database table **/DMO/CONNECTION**.
    
    a. Adjust the code as follows:

      ``` ABAP
          
          IF i_carrier_id IS INITIAL OR i_connection_id IS INITIAL.
            RAISE EXCEPTION TYPE cx_abap_invalid_value.
          ENDIF.
          
          SELECT SINGLE
            FROM /dmo/connection

      ```

4. Implement the FIELDS clause. Read the table fields **airport_from_id** and **airport_to_id**.
    
    > **Hint:** Use auto-completion (**Ctrl + Space**) to enter the field names.
    
    a. After ``FROM /DMO/CONNECTION`` enter FIELDS.
          
    b. After a blank, press **Ctrl + Space** and choose *airport_from_id*.
          
    c. After a comma and a blank press **Ctrl + Space** again and choose *airport_to_id*.
          
    d. The complete FIELDS clause should look like this:
          
      ``` ABAP

        FIELDS DepartureAirport, DestinationAirport
   
      ```

5. Implement the WHERE condition. Restrict all key fields of the database table (except for the client field) with the values of importing parameters **i_carrier_id** and **i_connection_id**. Do not forget to escape the parameters with prefix @.
    
    > **Hint:** Use auto-completion (**Ctrl + Space**) to enter the element names and parameter names.
    
    a. Add the following code after the FIELDS clause:

    ``` ABAP
        WHERE carrier_id    = @i_carrier_id
          AND connection_id = @i_connection_id
    ```

6. Implement the INTO clause. Store the SELECT result in the attributes **airport_from_id**, and **airport_to_id**. Do not forget to escape the attributes with prefix @.
    
    > **Hint:** Use auto-completion (**Ctrl + Space**) to enter the attribute names.
    
    a. Adjust the code as follows:

      ``` ABAP
            WHERE carrier_id    = @i_carrier_id
              AND connection_id = @i_connection_id   
            INTO ( @airport_from_id, @airport_to_id ).
      ```
          
    b. The complete SELECT statement should look like this:

      ``` ABAP
      SELECT SINGLE
            FROM /dmo/connection
          FIELDS airport_from_id, airport_to_id
            WHERE carrier_id    = @i_carrier_id
                AND connection_id = @i_connection_id
              INTO ( @airport_from_id, @airport_to_id ). 
      ```

7. Implement error handling after the SELECT statement. Check the content of the system field **sy-subrc**. If it does not equal zero, raise exception **CX_ABAP_INVALID_VALUE**.
    
    1. Add the following code after the SELECT statement:
  
      ``` ABAP
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_abap_invalid_value.
          ENDIF.
      ```

8. Activate the class. Execute it and analyze the console output. Check that the output for the new attributes displays data.

    a.  Press **Ctrl + F3** to activate the class.   

    b.  Press **F9** to run the class.

## CDS Views

### View Definitions in ABAP Core Data Services (ABAP CDS)

ABAP Core Data Services, or ABAP CDS for short, is an infrastructure for defining and consuming semantically rich data models in ABAP. They use a CDS DDL to define CDS entities that implement a semantic data model. The most important CDS modeling entities are CDS View entities.

![](https://learning.sap.com/service/media/topic/a73b8342-8269-4e4e-a40d-ab81b7f5e106/S4D400_24_en-US_media/S4D400_24_en-US_images/14_-_Analyze_CDS_001.png)

The CDS View definition contains re-usable SQL logic; sometimes as simple as a projection of table fields and sometimes more sophisticated with calculations, aggregations, joins, unions, and so on.

A CDS View definition can contain associations to reflect the relations of the data model. Consumers of the view can use the associations to retrieve related data.

Finally, annotations are used to semantically enrich the view definition. This metadata is evaluated by frameworks that build on top of CDS View definitions. One such framework is the ABAP RESTful application programming model , for which we will discuss an example later in this course.

Let's watch examples of CDS view definitions.

![](https://learning.sap.com/service/media/topic/a73b8342-8269-4e4e-a40d-ab81b7f5e106/S4D400_24_en-US_media/S4D400_24_en-US_images/14_-_Analyze_CDS_002.png)

CDS View definitions are contained in repository objects of type Data Definition. Let us now have a look at the source code of data definition ``/DMO/I_CONNECTION``.

The main part is the ``DEFINE VIEW ENTITY`` statement. It contains the name of the CDS View entity and, after keyword FROM, the data source. In our example, the name of the view entity is */DMO/I_Connection* and the data source is database table _/dmo/connection_. Optional addition AS, defines an alias name Connection to address the data source inside the View definition.

> **NOTE:** The source of a CDS View entity could also be another CDS View.

A pair of curly brackets contains the list of view elements. In our example, the view elements are fields of database table /dmo/connection. Keyword Key in front of the first two elements defines them as key fields of the CDS View entity. Optional addition AS defines an alias name for each view element.

![](https://learning.sap.com/service/media/topic/a73b8342-8269-4e4e-a40d-ab81b7f5e106/S4D400_24_en-US_media/S4D400_24_en-US_images/14_-_Analyze_CDS_003.png)

Addition association defines a relation to another CDS view entity. In our example, the related is CDS view entity */DMO/I_Carrier* and the name of the association is *_Airline*.

This association becomes available for consumers of the view by adding it to the element list. This is referred to as exposing the association.

![](https://learning.sap.com/service/media/topic/a73b8342-8269-4e4e-a40d-ab81b7f5e106/S4D400_24_en-US_media/S4D400_24_en-US_images/14_-_Analyze_CDS_004.png)

Annotations start with the at sign (@) and they are used to semantically enrich the view definition for consumers. Annotations before the view definition are called entity annotations. Entity annotations are used to define metadata for the view entity as a whole. Annotations between the curly brackets are called element annotations. Element annotations are used to define metadata for the different elements of the view.

![](https://learning.sap.com/service/media/topic/a73b8342-8269-4e4e-a40d-ab81b7f5e106/S4D400_24_en-US_media/S4D400_24_en-US_images/14_-_Analyze_CDS_005.png)

You already learned how to use the _Data Preview_ tool to display and analyze the content of a database table. This tool is also available for CDS view entities.

To open the _Data Preview_ for a given CDS entity, right-click anywhere in the data definition and choose ``Open With > Data Preview``. Alternatively, place the cursor anywhere in the database table definition and press ``Ctrl + F8``.

The tool displays the data returned by the CDS entity. The same functions are available to sort or filter the data and adjust the display.

If the view definition contains one or more associations, you can use them to display related data. To do so, proceed in the following manner:

1. Right-click on a row in the display.
2. From the context menu, choose _Follow Association_.
3. From the list of available associations, choose the one in which you are interested.

![](https://learning.sap.com/service/media/topic/a73b8342-8269-4e4e-a40d-ab81b7f5e106/S4D400_24_en-US_media/S4D400_24_en-US_images/14_-_Analyze_CDS_006.png)

If you want to find all CDS Views with a certain database table as a source, you can utilize the Where-used List tool of ADT. To use this tool, proceed as follows:

1.  Open the definition of the database table.
2.  Right-click anywhere in the source code and choose _Get Where-used List_ from the context menu. Alternatively, you can press ``Ctrl + Shift + G``, or choose the button from the toolbar with the same symbol.
3.  The _Search_ view displays a list of all development objects that directly use the database table.

![](https://learning.sap.com/service/media/topic/a73b8342-8269-4e4e-a40d-ab81b7f5e106/S4D400_24_en-US_media/S4D400_24_en-US_images/14_-_Analyze_CDS_007.png)

You can apply filters to the Where-used List if, for example, you are only interested in objects from certain packages or objects of the specific object type. The example illustrates how to filter for CDS views that use the table.

> **Tutorial:** [How to Analyze a CDS View](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_3E120CA182A03BAE:demo)

## CDS Views in ABAP SQL

![](https://learning.sap.com/service/media/topic/b1d725bd-c1f6-4c8f-959b-db97d178d895/S4D400_24_en-US_media/S4D400_24_en-US_images/15_-_Read_using_CDS_001.png)

When you implement a SELECT statement in ABAP, you can use a CDS view entity as the data source instead of reading from the database table directly. This has several advantages.

- Re-use of the SQL logic contained in the CDS view
- Concise, easy-to-read reading of related data using the associations
- Sometimes the names of views and view elements are better readable than the more technical names of database tables and table fields

The SELECT statement in the example uses CDS view entity */DMO/I_Connection* as a data source.

> **NOTE:** Note that the names used in the FIELDS clause and WHERE conditions are the alias names of the view elements.

The third element in the FIELDS clause makes use of exposed association *_Airline*. It reads element name from the associated CDS view entity */DMO/I_Airline*. This kind of element is called a path expression. The backslash (\) is a mandatory prefix for association names.

## Analyze and Use a CDS View Entity

In this exercise, you read from a CDS view entity rather than from the database table directly. This provides you with comfortable access to the name of the airline as well.

- **Template:** ``/LRN/CL_S4D400_DBS_SELECT`` (global Class)

- **Solution:** ``/LRN/CL_S4D400_DBS_CDS`` (global Class)

### Task 1: Copy Template (Optional)

Copy the template class. If you finished the previous exercise, you can skip this task and continue editing your class **ZCL_##_SELECT**.

#### Steps

1. Copy the class **/LRN/CL_S4D400_DBS_SELECT** to a class in your own package (suggested name: **ZCL_##_CDS**, where ## stands for your group number).
    
    a. Open the source code of the global class **/LRN/CL_S4D400_DBS_SELECT**.
          
    b. Link the _Project Explorer_ view with the editor.
          
    c. In the _Project Explorer_ view, right-click the class **/LRN/CL_S4D400_DBS_SELECT** to open the context menu.
          
    d. From the context menu, choose _Duplicate ..._.
          
    e. Enter the name of your package in the _Package_ field. In the _Name_ field, enter the name **ZCL_##_CDS**, where ## stands for your group number.
          
    f. Adjust the description and choose _Next_.
          
    g. Confirm the transport request and choose _Finish_.

### Task 2: Analyze CDS View Entity

Analyze the definition of the CDS view entity **/DMO/I_Connection**.

#### Steps

1. Open the development object that contains the definition of the CDS view entity **/DMO/I\_Connection**.
    
    a.  From the eclipse toolbar, choose _Open ABAP Development Object_ or press **Ctrl + Shift + A**.
          
    b.  In the input field, enter **/DMO/I_Con** as a search string.
          
    c.  In the list of matching items, click on */DMO/I_CONNECTION* (Data Definition)_ and choose _OK_.
    
2. Analyze the element list of the CDS view entity.
    
    a. Navigate to the comma-separated list between the pair of curly brackets ( { } ).
          
    b. You find the alias names after keyword AS.
    
3. Open the _Tooltip Description_ for the target of association **_Airline**.
    
    a.  Click on */DMO/I_Carrier* beforeas *_Airline* and press **F2** to show the tooltip description.

### Task 3: Declare Additional Attribute

Extend the local class **lcl_connection** with a private instance attribute **carrier_name**. Add some output for the new attribute to the implementation of method **get_output( )**.

> **NOTE:** The new attribute is not filled, yet. In order to fill it we will adjust the SELECT statement in the next task of this exercise.

#### Steps

1. Return to the local class **lcl_connection** in your global class.
    
    a. In the global class, choose _Local Types_.
    
2. Add the following **private** attribute to the class definition:

    | Attribute Name | Scope | Data Type |
    | --- | --- | --- |
    **carrier_name** | instance | ``/DMO/CARRIER_NAME`` |
    
    a. Adjust the code as follows:

      ``` ABAP
          PRIVATE SECTION.
            DATA carrier_id      TYPE /dmo/carrier_id.
            DATA connection_id   TYPE /dmo/connection_id.
          
            DATA airport_from_id TYPE /dmo/airport_from_id.
            DATA airport_to_id   TYPE /dmo/airport_to_id.
          
            DATA carrier_name    TYPE /dmo/carrier_name.
      ENDCLASS.
      ```
    
3. Extend the implementation of method **get_output**. Add the carrier name to the string template with the carrier identification.
    
    a. Navigate to the implementation of method **get_output**.
          
    b. Replace statement ``APPEND |Carrier:     { carrier_id      }| TO r_output.`` with the following statement:
          
      ``` ABAP
      
            APPEND |Carrier:     { carrier_id } { carrier_name }| TO r_output.
          
      ```
    
4. Activate the class. Execute it and analyze the console output.
    
    a. Press **Ctrl + F3** to activate the class.
          
    b. Press **F9** to run the class.

### Task 4: Use CDS View Entity

In the implementation of method **constructor**, replace the SELECT statement that reads from database table **/dmo/connection** with a SELECT statement that reads from CDS view entity **/DMO/I_Connection**.

#### Steps

1. In the implementation of method **constructor**, comment the SELECT statement.
    
    a. Select all lines that belong to the ``SELECT SINGLE ...`` ..
          
    b. Press **Ctrl + <** to add a star sign (*) in front of each selected line.

2. After the commented code, add a SELECT statement that reads a single record from database table **/DMO/I_Conncetion**.
    
    a. Adjust the code as follows:

      ``` ABAP
          
      *     SELECT SINGLE
      *       FROM /dmo/connection
      *     FIELDS airport_from_id, airport_to_id
      *      WHERE carrier_id    = @i_carrier_id
      *        AND connection_id = @i_connection_id
      *       INTO ( @airport_from_id, @airport_to_id ).
          
        SELECT SINGLE
          FROM /DMO/I_Connection

      ```

3. Implement the FIELDS clause. Read view elements **DepartureAirport** and **DestinationAirport**. In addition, read view element **Name** from the target of association **_Airline**.
    
    > **Hint:** Use auto-completion (**Ctrl + Space**) to enter the names.
    
    a. After **FROM /DMO/I_Connection** enter FIELDS.
          
    b. After a blank, press **Ctrl + Space** and choose _DepartureAirport_.
          
    c. After a comma and a blank press **Ctrl + Space** again and choose **DestinationAirport**.
          
    d. After a comma and a blank, type in a backslash (\) , press **Ctrl + Space** and choose *_Airline*.
          
    e. Immediately after *_Airline*, type in a dash sign (-) , press **Ctrl + Space** and choose _Name_.
          
    f. The complete FIELDS clause should look like this:

      ``` ABAP
              FIELDS DepartureAirport, DestinationAirport, \_Airline-Name
      ```

4. Implement the WHERE condition. Restrict the key elements of CDS view entity with the values of importing parameters **i_carrier_id** and **i_connection_id**. Do not forget to escape the parameters with prefix @.
    
    > **Hint:** Use auto-completion (**Ctrl + Space**) to enter the element names and parameter names.
    
    a. Add the following code after the FIELDS clause:

      ``` ABAP

            WHERE AirlineID    = @i_carrier_id
              AND ConnectionID = @i_connection_id
         
      ```

5. Implement the INTO clause. Store the SELECT result in attributes **airport_from_id**, **airport_to_id**, and **airline_name**. Do not forget to escape the attributes with prefix @.
    
    > **Hint:** Use auto-completion (**Ctrl + Space**) to enter the attribute names.
    
    a. Adjust the code as follows:

      ``` ABAP
               WHERE AirlineID    = @i_carrier_id
                 AND ConnectionID = @i_connection_id
                INTO ( @airport_from_id, @airport_to_id, @carrier_name ).
      ```
          
    b. The complete SELECT statement should look like this:
          
      ``` ABAP

        SELECT SINGLE
              FROM /DMO/I_Connection
            FIELDS DepartureAirport, DestinationAirport, \_Airline-Name
              WHERE AirlineID    = @i_carrier_id
                  AND ConnectionID = @i_connection_id
                INTO ( @airport_from_id, @airport_to_id, @carrier_name  ).
        
      ```

6. Activate the class. Execute it and analyze the console output. Check that the output for the new attributes displays data.

    a. Press **Ctrl + F3** to activate the class.
  
    b. Press **F9** to run the class.
