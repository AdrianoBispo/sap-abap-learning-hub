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

