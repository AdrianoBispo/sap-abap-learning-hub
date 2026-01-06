# Implementing Database Updates Using Business Objects

## Business Objects

In the **ABAP RESTful application programming model**, a business object defines a particular entity, such as a travel agency. Its definition has two parts; one or more CDS views, which define the structure of the object or, in other words, the fields that it contains, and a behavior definition, which describes what you can do with the business object.

![](https://learning.sap.com/service/media/topic/d8918226-ee78-4230-a18b-fa619dafe9b5/S4D400_24_en-US_media/S4D400_24_en-US_images/21_Analyzing_BO_001.png)

The **behavior definition** specifies which of the standard operations, create, update, delete are allowed. It can also contain the definition of validations, determinations, and actions. Validations check that the data is correct when you create or update a record. Determinations modify instances of business objects based on trigger conditions. Actions are non-standard operations which you use to provide customized, business-logic-specific behavior. Approving a purchase order or canceling a flight are activities that you would implement as an action.

The **behavior implementation** consists of one or more ABAP classes. Here the validations, determinations, and actions are implemented. When it comes to the standard operations, the model distinguishes two implementation scenarios: In the **unmanaged** implementation scenario, create, update, and delete are implemented in the behavior implementation. In the **managed** implementation scenario the runtime takes care of them.

Business objects are commonly used to provide the transactional logic for Fiori Elements apps or Web APIs. However, you can also access them from ABAP coding using the **Entity Manipulation Language (EML)**. This is a set of ABAP statements that allows you to create, read, update, and delete data using business objects.

> **NOTE:** *EML* is also able to access the application data from inside the behavior implementation of a business object.

![](https://learning.sap.com/service/media/topic/d8918226-ee78-4230-a18b-fa619dafe9b5/S4D400_24_en-US_media/S4D400_24_en-US_images/21_Analyzing_BO_002.png)

In this unit, you will create a class that uses a business object to modify travel agency data. The view entity contains the key field AgencyID and various other fields containing information about the travel agency.

### Behavior Definition and Implementation

![](https://learning.sap.com/service/media/topic/d8918226-ee78-4230-a18b-fa619dafe9b5/S4D400_24_en-US_media/S4D400_24_en-US_images/21_Analyzing_BO_003.png)

There are two parts to the behavior of a business object: the behavior definition and the behavior implementation. The behavior definition contains information about what the business object can do, while the behavior implementation contains the actual coding that the system executes.

The behavior implementation is an ABAP class. You declare the class in the behavior definition in the statement ``managed implementation in class <class> unique``. The actual coding of the behavior implementation is contained in a local class within the global class that you specify.

![](https://learning.sap.com/service/media/topic/d8918226-ee78-4230-a18b-fa619dafe9b5/S4D400_24_en-US_media/S4D400_24_en-US_images/21_Analyzing_BO_004.png)

A behavior definition is an essential component of a business object. It describes which of the standard operations are allowed, for example *create*, *update*, *delete*. It also defines checks (validations) that are performed when you create or change data.

The example here is the behavior definition for the travel agency. At the beginning of the definition, you can see the name of the CDS view we just looked at, and that there is an alias defined for it. This is important, since it is the alias name that you use to address the entity using *EML*.

The behavior definition also links the CDS entity to the database table in which the data is stored. In this case, the business object uses two tables: one for active data and one for drafts (data that is incomplete and has not been checked). There is also information that is relevant for locking the data, authorization checks, and concurrency control. We are not going to look at this information in depth - you just need to know that the runtime can take care of these issues. You can also generate CDS entities and behavior definitions based on the definition of a database table and, in this case, locking, authorization checks, and concurrency controls are dealt with automatically.

![](https://learning.sap.com/service/media/topic/d8918226-ee78-4230-a18b-fa619dafe9b5/S4D400_24_en-US_media/S4D400_24_en-US_images/21_Analyzing_BO_005.png)

The global class of the behavior implementation (also known as a behavior pool) is just an empty class definition with the special addition ``FOR BEHAVIOR OF`` followed by the name of the behavior definition. The actual implementation of the behavior definition is a local class within the global class definition. You access the class by clicking the _Local Types_ tab.

The behavior implementation contains code that is specific to the business object, for example, the implementation for validations, determinations, and actions. Whether it also contains code for the standard operations (*create*, *update*, *delete*, and *lock*) depends on the details of the behavior definition. The behavior implementation for our business object does not contain code for the standard operations. This is because the business object uses the managed implementation type in which the runtime deals with the standard operations.

![](https://learning.sap.com/service/media/topic/d8918226-ee78-4230-a18b-fa619dafe9b5/S4D400_24_en-US_media/S4D400_24_en-US_images/21_Analyzing_BO_006.png)

A validation is a check that the runtime performs when data is changed. Here, the validation is always performed when a new record is created (trigger ``create;``) If an existing record is changed, the validation is only performed if the _Name_ field has been changed (trigger ``field Name;``).

Validations are defined in the behavior definition. For each validation, there is a corresponding method in the behavior implementation.

> **Tutorial:** [How To Analyze A Business Object](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?show=project!PR_6DC14C585665F186:demo)

## BO Projections and BO Interfaces

![](https://learning.sap.com/service/media/topic/e352d1b8-2718-457f-ad91-b5c10cf17886/S4D400_24_en-US_media/S4D400_24_en-US_images/02BOProjectionsAndInterfaces_001.png)

In the ABAP RESTful application programming model, there are two important ways to consume a Business Object:

- Through a **Business Service**, for example an OData UI Service for an SAP Fiori Application.
- From inside ABAP Code, using the **Entity Manipulation Language (EML)**.

Although technically possible, a Business Object should not be consumed directly. Instead, consumers should access **Business Object Projections** (BO Projections) and **Business Object Interfaces** (BO Interfaces) as follows:

- **BO Projection:** Business Services should always define a service-specific projection of the Business Object. The BO projection specifies which subset of the business object's data and operations are available through this service. A BO Projection can also contain the definition and implementation of service-specific data and behavior.

  > **NOTE:** A BO Projection is either based on the Business Object directly or on a BO Interface.

- **BO Interface:** A Business Object Interface provides stable access to a business object's data and operations. BO Interfaces are typically released for use in other software components. If a BO Interface exists for a given Business Object, ABAP code using EML should always be used to access the BO Interface.

  ![](https://learning.sap.com/service/media/topic/e352d1b8-2718-457f-ad91-b5c10cf17886/S4D400_24_en-US_media/S4D400_24_en-US_images/02BOProjectionsAndInterfaces_002.png)

Like a Business Object, a BO Projection consists of two parts: one or more CDS view entities (defined in Data Definitions), and a Behavior Definition. The same is true for BO Interfaces. They also consist of one or more CDS views and a Behavior Definition.

The easiest way to identify projections and interfaces is by looking at their behavior definitions: Behavior definitions for projections start with the keyword ``projection``, behavior definitions for interfaces start with the keyword ``interface``.

The CDS views of BO projections and the CDS views of BO interfaces are always **CDS Projection Views**. This means that their definitions contain the addition ``as projection on`` where the definitions of ordinary CDS views use ``as select from``. In newer releases, the use case of a projection view is specified using the addition ``provider contract`` after the view entity name as follows:

- ``provider contract transactional_interface`` for BO Interfaces
- ``provider contract transactional_query`` for BO Projections

> **Hint:** For SAP developments, the following naming convention applies:
> - ``<namespace>C_<…>`` for BO Projections
> - ``<namespace>I_<…>`` for BO Interfaces
> - ``<namespace>R_<…>`` for BO Definitions

## Analyze a Business Object

In this exercise, you analyze the business object interface ``/DMO/I_AgencyTP`` to find out about its structure and the data manipulation operations it offers.

### Task 1: Analyze the Interface Behavior

Analyze the behavior definition of the business object interface **/DMO/I_AGENCYTP**.

#### Steps

1. Open the behavior definition **/DMO/I_AGENCYTP** in the editor.
    
2. Analyze the source code. Open the ABAP language help to find more information.

### Task 2: Analyze the Data Structure

Analyze the CDS view entity that defines the data structure of the business object interface. Navigate to its data source until you reach a CDS view entity that is not a projection.

#### Steps

1. Navigate to the CDS view entity ``/DMO/I_AgencyTP`` and analyze the source code.
    
    a. Go to the statement ``define behavior for /DMO/I_AgencyTP``.
          
    b. Hold down the ``Ctrl`` key and left-click on ``/DMO/I_AgencyTP``. Alternatively, place the cursor on ``/DMO/I_AgencyTP`` and press ``F3``.
    
2. Navigate to the data source of the CDS view entity ``/DMO/I_AgencyTP`` and analyze the source code.
    
    a. Go to the code fragment ``as projection on /DMO/R_AgencyTP``.
          
    b. Hold down the ``Ctrl`` key and left-click on ``/DMO/R_AgencyTP``. Alternatively, place the cursor on ``/DMO/R_AgencyTP`` and press ``F3``.

### Task 3: Analyze the Business Object Behavior

Analyze the behavior definition of the business object ``/DMO/R_AGENCYTP`` which lies under business object interface ``/DMO/I_AGENCYTP``.

#### Steps

1. Open the behavior definition ``/DMO/R_AGENCYTP`` in the editor.
    
2. Analyze the source code. Open the ABAP language help to find more information.

### Task 4: Analyze the Behavior Implementation

Analyze the behavior implementation of the business object ``/DMO/R_AGENCYTP``.

#### Steps

1. Navigate to the ABAP class ``/DMO/BP_R_AGENCYTP`` that contains the behavior implementation of the business object ``/DMO/R_AGENCYTP``.
    
    a. Go to the statement ``managed implementation in class /dmo/bp_r_agencytp unique;``.
          
    b. Hold down the ``Ctrl`` key and left-click on ``/dmo/bp_r_agencytp``. Alternatively, place the cursor on ``/dmo/bp_r_agencytp`` and press ``F3``.
    
2. Go to the local class **lhc_agency** and analyze the definition.
    
    a.  Go to the tab _Local Types_. Alternatively, expand root node ``/DMO/BP_R_AGENCYTP`` in the _Outline_ view and choose ``LHC_AGENCY``.

## Entity Manipulation Language (EML)

![](https://learning.sap.com/service/media/topic/e8f44333-1046-4cc6-9baf-a494a9b2358c/S4D400_24_en-US_media/S4D400_24_en-US_images/22_Using_EML_001.png)

*EML* consists of statements that you can use to manipulate the data of a business object. You use the ``READ ENTITIES`` statement to read data; for all other operations, you use the ``MODIFY ENTITIES`` statement with the corresponding addition ``UPDATE``, ``CREATE``, or ``DELETE``.

> **NOTE:** You can only use the ``CREATE``, ``UPDATE``, and ``DELETE`` operations if the behavior definition of the business object interface contains the corresponding ``use create``, ``use update``, or ``use delete`` directive. Trying to use a prohibited operation causes a syntax error.

![](https://learning.sap.com/service/media/topic/e8f44333-1046-4cc6-9baf-a494a9b2358c/S4D400_24_en-US_media/S4D400_24_en-US_images/22_Using_EML_002.png)

To read data from a business object, you use the ``READ ENTITIES`` statement. The statement has two important parameters: one internal table containing the keys of the data that you want to read and another containing the results of the query.

These internal tables have special data types called derived behavior definition types. The system creates them automatically when a developer creates a behavior definition and they contain some or all of the fields of the business object along with further fields that control how the system processes a particular request. You declare the internal tables using the new addition ``TYPE TABLE FOR <operation>`` in the ``DATA``statement.

![](https://learning.sap.com/service/media/topic/e8f44333-1046-4cc6-9baf-a494a9b2358c/S4D400_24_en-US_media/S4D400_24_en-US_images/22_Using_EML_003.png)

The type ``TABLE FOR READ IMPORT`` contains the key field or fields of the business object. The _%control_ structure is a generated structure that indicates which fields of the business object are actually used in the current operation. In our example, the system fills the structure automatically and you do not have to worry about it.

The type ``TABLE FOR READ RESULT`` contains all of the fields of the business object. This table contains the result set after the read statement has been executed.

![](https://learning.sap.com/service/media/topic/e8f44333-1046-4cc6-9baf-a494a9b2358c/S4D400_24_en-US_media/S4D400_24_en-US_images/22_Using_EML_004.png)

The read import table contains a column for each key field of the business object, in this case, _agencyID_. To read a particular agency, you add a row to the internal table containing its key. As well as the key field or fields, the table contains the columns *%is_draft* and _%control_. With *%is_draft* you can specify whether you want to read draft data or active data. The _%control_ structure is used to specify which fields are to be read.

> **NOTE:** In our example, we only read active data and the _%control_ structure is filled by the framework, based on the field list after addition FIELDS.

![](https://learning.sap.com/service/media/topic/e8f44333-1046-4cc6-9baf-a494a9b2358c/S4D400_24_en-US_media/S4D400_24_en-US_images/22_Using_EML_005.png)

When you process a business object, using the ``READ ENTITIES OF`` or ``MODIFY ENTITIES OF`` statement, you must first specify the name of the behavior definition. This is followed by keyword ENTITY and the name of the entity with which you want to work. If the entity has an alias name, you should use it, here.

> **NOTE:** You cannot use the alias name after addition OF. This is because technically speaking, you specify the name of the behavior definition at this point, not the name of the entity.

![](https://learning.sap.com/service/media/topic/e8f44333-1046-4cc6-9baf-a494a9b2358c/S4D400_24_en-US_media/S4D400_24_en-US_images/22_Using_EML_006.png)

The ``READ ENTITIES`` statement reads business object data according to the keys that you pass in the ``WITH`` addition. It returns the result in the internal table in the ``RESULT`` addition. In the statement, you can also specify which fields of the business object you need. In this example, we have used the ``ALL FIELDS`` addition to return all of the fields. However, if you only require a subset of the fields, you can use the variant ``FIELDS`` ( _field1, field2_ … ) to restrict the amount of data that is read.

> **NOTE:** Unlike in a SELECT statement, the field list is not comma-separated.

![](https://learning.sap.com/service/media/topic/e8f44333-1046-4cc6-9baf-a494a9b2358c/S4D400_24_en-US_media/S4D400_24_en-US_images/22_Using_EML_007.png)

The result table contains all of the fields of the business object, along with the control field *%is_draft*. If your ``READ ENTITIES`` statement contains the ``ALL FIELDS`` variant, the system provides the values of all of the fields. If you use the FIELDS ( _f1_ … _fn_ ) variant, only the fields that you requested will be filled.

> **NOTE:** Key fields are always filled, even if they are not specified in the FIELDS ( _f1_ … _fn_ ) variant.

![](https://learning.sap.com/service/media/topic/e8f44333-1046-4cc6-9baf-a494a9b2358c/S4D400_24_en-US_media/S4D400_24_en-US_images/22_Using_EML_008.png)

If you want to update data, you declare an internal table with ``TYPE TABLE FOR UPDATE``. This contains all of the fields of the business object and also the _%control_ structure. In our variant of the ``MODIFY ENTITIES`` statement, the system fills the _%control_ structure automatically.

![](https://learning.sap.com/service/media/topic/e8f44333-1046-4cc6-9baf-a494a9b2358c/S4D400_24_en-US_media/S4D400_24_en-US_images/22_Using_EML_009.png)

![](https://learning.sap.com/service/media/topic/e8f44333-1046-4cc6-9baf-a494a9b2358c/S4D400_24_en-US_media/S4D400_24_en-US_images/22_Using_EML_010.png)

The ``MODIFY ENTITIES`` statement updates data in the transactional buffer. In the ``FIELDS`` addition, you specify which fields should be changed. In the ``WITH`` addition, you pass the internal table containing the data that you want to update.

When you use *EML* outside the business object, you must use the ``COMMIT ENTITIES`` statement to trigger the save sequence and persist the data in the database.

> NOTE: Later in this course we will use *EML* inside the behavior implementation of the business object. Inside the behavior implementation it is neither necessary nor allowed to trigger the commit with ``COMMIT ENTITIES``.

[How to Implement an EML Statement](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_3AD00A7871CEE7A8:demo)

## Exercise - Modify Data Using EML

In this exercise, you use *EML* to perform the standard operation _update_ for the root entity of the business object **/DMO/R_AGENCYTP**. To avoid later inconsistencies, you do not access the business object directly but via its released stable interface **/DMO/_I_AGENCYTP.**

- **Template:** *none*
- **Solution:** ``/LRN/CL_S4D400_BOS_EML`` (global Class)

### Task 1: Analyze Current Data

Use the _Data Preview_ tool to check the current name of the travel agency with ID "0700##", where ## stands for your group number.)

#### Steps

1. Open the definition of the CDS view entity **/DMO/I_AgencyTP**.
    
    a. If you still have the data definition **/DMO/I_AGENCYTP** open from the previous exercise, switch to the corresponding tab. Otherwise, press **Ctrl + Shift + A** to open the ABAP development object.
    
2. Open the _Data Preview_ tool.
    
    a. Right-click anywhere in the source code to open the context menu.
          
    b. From the context menu, choose _Open With_→_Data Preview_.
    
3. Check the current name of the travel agency with ``ID "0700##"``, where ## stands for your group number.
    
    a. Scroll down to the entry with the value "0700##" in the column **AgencyID** and take a note of the value in the column **Name**.

### Task 2: Create a Global Class

In your own package, create a new ABAP class.

#### Steps

1. Create a new ABAP class called **ZCL_##_EML**, where **##** stands for your group number. Ensure that the class implements the interface **IF_OO_ADT_CLASSRUN**.
    
    a. Choose _File_→_New_→_ABAP Class_.
          
    b. Enter the name of your package in the _Package_ field. In the _Name_ field, enter the name **ZCL_##_EML**, where **##** is your group number. Enter a description.
          
    c. In the _Interfaces_ group box, choose _Add_.
          
    d. Enter **IF_OO_ADT_CLASSRUN**. When the interface appears in the hit list, double-click it to add it to the class definition.
          
    e. Choose _Next_.
          
    f. Select your transport request and choose _Finish_.

### Task 3: Update Data

Implement an *EML* statement to change the name of the travel agency with ``ID "0700##"``. ( **##** stands for your group number.)

#### Steps

1. In the method **IF_OO_ADT_CLASSRUN~MAIN**, declare an internal table with the correct derived type for an *EML* update of entity **/DMO/I_AgencyTP** (suggested name: **agencies_upd)**.
    
    a.  Add the following code:
          
      ``` ABAP
      DATA agencies_upd TYPE TABLE FOR UPDATE /DMO/I_AgencyTP.
      ```
    
2. Fill the internal table with a single line containing **"0700##"** as the value for the column **AGENCYID** (where **##** stands for your group number) and any new value for the column **NAME**.
    
    a. After the data declaration, add some code similar to the following code:

      ``` ABAP
      agencies_upd = VALUE #( ( agencyid = '0700##' name = 'Some fancy new name' ) ).
      ```
    
3. Implement an *EML* statement ``MODIFY ENTITIES`` for the business object interface **/DMO/I_AgencyTP**. Update the data of the root entity, ensuring that only the field **NAME** is changed.
    
    > **Hint:** Remember that the interface behavior defines alias name **/DMO/Agency** for the root entity **/DMO/I_AgencyTP**.
    
    a. At the end of the method, add the following code:

      ``` ABAP

          MODIFY ENTITIES OF /dmo/i_agencytp
            ENTITY /dmo/agency
            UPDATE FIELDS ( name )
              WITH agencies_upd.

      ```
    
4. In order to get some visible result from the application, write a text literal to the console using the method **out->write**.
    
    a. After the ``MODIFY ENTITIES`` statement, add the following code:

      ``` ABAP
        out->write( `Method execution finished!`  ).
      ```
    
5. Activate and test the class.
    
    a. Press **Ctrl + F3** to activate the class.
          
    b. Press **F9** to run the class.
    
6. Check whether the new name has been written to the database.
    
    a. Return to the data preview for CDS view entity **/DMO/I_AgencyTP**.
          
    b. From the toolbar of the _Data Preview_ tab, choose _Refresh_.
    
7. Add the ``COMMIT ENTITIES`` statement to your code to commit the changes.
    
    a. Return to the method **IF_OO_ADT_CLASSRUN~MAIN** in your global class **ZCL_##_EML**.
          
    b. Adjust the code as follows:

      ``` ABAP

      METHOD if_oo_adt_classrun~main.
          
        DATA agencies_upd TYPE TABLE FOR UPDATE /dmo/i_agencytp.
          
        agencies_upd = VALUE #( ( agencyid = '0700##' name = 'Some fancy new name' ) ).
          
        MODIFY ENTITIES OF /dmo/i_agencytp
          ENTITY /dmo/agency
          UPDATE FIELDS ( name )
            WITH agencies_upd.
          
        COMMIT ENTITIES.
          
        out->write( `Method execution finished!`  ).
          
      ENDMETHOD.

      ```
    
8. Activate and test the class again. Confirm that the changes are now in the database.
    
    a.  Press **Ctrl + F3** to activate the class.
          
    b.  Press **F9** to run the class.
          
    c.  Return to the data preview for the CDS view entity **/DMO/I_AgencyTP**.
          
    d.  From the toolbar of the _Data Preview_ tab, choose _Refresh_.