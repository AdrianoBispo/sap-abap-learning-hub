# Applying Basic Techniques and Concepts

## Understanding the Basics of ABAP

### Origin and Evolution of ABAP

ABAP is a programming language developed by SAP for the development of business applications in the SAP environment.

![Ilustration - ABAP Language Versions](./images/01_-_ABAP_then_and_now_002.png)

Each ABAP program has the program attribute "ABAP language version", which is defined internally by a version ID. The version of a program determines which language elements and which repository objects can be used in the program and which syntax rules apply. The following versions are currently available:

**Standard ABAP:** This version of ABAP is the universal basic version. It is an unrestricted ABAP language version that covers the entire language scope of ABAP that can be used in Unicode systems and, apart from the static package concept, access to all other repository objects is allowed. The syntax check for Standard ABAP is performed as Unicode check, which is the minimum requirement for a Unicode system.

**ABAP for Key Users:** This version of ABAP is intended for the secure implementation of enhancements by key users within the scope of enhancement options provided by SAP. ABAP for Key Users is a restricted ABAP language version where the general rules for Standard ABAP apply but only a very restricted set of language elements is supported and access to repository objects is restricted.

**ABAP for Cloud Development:** This version of ABAP is intended for developments in the SAP BTP ABAP environment and in the S/4HANA Cloud ABAP environment. ABAP for Cloud Development is a restricted ABAP language version where the general rules for Standard ABAP apply but only a restricted set of language elements is supported and access to repository objects is restricted.

> **NOTE:** This course gives a universal introduction to ABAP development. It restricts itself to syntax elements and language features available in all three language versions. An exception is the introduction to the ABAP RESTful Application Programming Model which is not part of ABAP for key users.

### ABAP Cloud

![Ilustration - ABAP Cloud](./images/02-ABAP_Cloud.png)

The ABAP for Cloud Development language version is a key element of **ABAP Cloud**. The technological core of ABAP Cloud defines the design-time and runtime architecture of all extensions, services, and applications. The main ABAP Cloud elements are:

- **ABAP for Cloud Development** as the cloud-optimized ABAP language for the business logic;
    
- **ABAP Development Tools** for Eclipse as the ABAP integrated development environment;
    
- **ABAP Core Data Services (CDS)** for the data model and for embedded analytics;
    
- The **ABAP RESTful Application Programming Model** for building apps and services;
    
- **Mandatory public SAP APIs and extension points** to allow automated cloud operations and lifecycle stable extensibility.

![Ilustration - SAP S/4HANA](./images/02%20-%20ABAP%20Cloud%20Availability.png)

Using ABAP Cloud is mandatory on the _SAP Business Technology Platform_ and on _SAP S/4HANA Cloud, public edition_. It is not mandatory but strongly recommended to follow the principles of ABAP Cloud on _SAP S/4HANA, private edition_ and in on-premise systems. By implementing your custom development with ABAP Cloud, your developments are cloud ready by default and you are not changing the composition of the core system, which is an important principle for system maintenance in the cloud environment (**Clean Core** approach).

## The Basics of the ABAP Syntax

### Some Basic Features of the ABAP Language

Let's look at some basic features of the ABAP language.

![Ilustration - Basics of ABAP Syntax](./images/02_-_Basics_of_ABAP_syntax_001.png)

**The ABAP keyword documentation documents use the following syntax conventions:**

1. ABAP words are displayed in UPPERCASE letters (Ex: ``APPEND``, ``NON-UNIQUE``, ``INTO``);

2. Operands are displayed in lowercase letters (Ex: ``dobj``, ``dbtab``);

3. Parts of statements whose syntax diagrams are displayed elsewhere, are shown as pseudo syntax (Ex: ``selscreen_options``, ``list_options``);

4. Periods, commas, colons, and parentheses are displayed as normal. They are part of the ABAP syntax (Ex: ``.`` ``:``, ``,``, ``()`` );

5. Operators are displayed as normal. They are part of the ABAP syntax (Ex: ``+``, ``-``, ``*``, ``/`` );

6. Parts of statements that are optional are displayed in italic square brackets _[ ]_. A list of statement parts in such brackets means that some or all of the parts can be used. If at least one part has to be used, this is noted in the text. Square brackets `[ ]` that are part of the ABAP syntax are shown in the same color as all other language elements and are not shown in italics (Ex: _[NO-GROUPING]_,  _[NO-SIGN]_,  _[NO-ZERO]_ );

7. Italic vertical bars (_|_) between parts of statements mean that only one of the listed parts can be used within a statement. A vertical bar like this always binds immediate neighbors. Vertical bars `|` that are part of the ABAP syntax are shown in the same color as all other language elements (Ex: DDMMYY _|_ MMDDYY _|_ YYMMDD);

8. Italic curly brackets (_{ }_) group together related parts of statements, for example those on the right or left of vertical bars. Curly brackets `{ }` that are part of the ABAP syntax are shown in the same color as all other language elements and are not shown in italics (Ex: _{_ NO EDIT MASK _}_ _|_ _{_ EDIT MASK mask _}_);

9. A string of dots `...` means that other parts of the statement can be included at this point (Ex **`...`** `AS CHECKBOX`).

### Comments in ABAP

A comment is a description inserted in the source code of a program that helps the user better understand the program and that is ignored when the program is generated by the ABAP Compiler. From a technical perspective, a comment can have any content. Two language elements are available for creating comments:

- The `*` character in the first position of a line in a program defines the entire line as a comment.
- The `"` character at any position defines that all of the remaining content in the line is a comment. This rule does not apply to the `"` character in [character literals](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABENCHARACTER_LITERAL_GLOSRY.html) and [pseudo comments](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABENPSEUDO_COMMENT_GLOSRY.html).

These language elements make it possible to create two types of comments:

- **Comment lines:** A comment line contains nothing but a comment. It can be defined either by the * character in the first position in the program line or by the " character at any position of an otherwise blank program line.
- **Line end comments:** A line end comment is a comment started with the " character and that stands behind an ABAP statement or parts thereof.

#### Example

The following example demonstrates the recommended use of comments. Comment lines started by * are used to structure the program. Line end comments occur after declarations and statements at the end of blocks. All other comments stand in front of the described statements and are indented accordingly.

```abap

*----------------------------------------------------------* 
* Class implementations                                    * 
*----------------------------------------------------------* 
CLASS application IMPLEMENTATION. 
  METHOD main. 
    DATA: items    TYPE STANDARD TABLE 
                   OF REF TO item,     "Item table 
          item_ref LIKE LINE OF items. "Item reference 
    DATA: amount       TYPE i, "Amount per item 
          total_amount TYPE i. "Total amount of items 
    ... 
    "Loop over all items to compute total amount 
    LOOP AT items INTO item_ref. 
      IF item_ref IS BOUND AND 
        item_ref->is_valid( ) = abap_true. 
        "Compute total amount for valid items 
        amount = item_ref->get_amount( ). 
        ADD amount TO total_amount. 
        ... 
      ELSE. 
        ... 
      ENDIF. "item_ref IS BOUND AND... 
    ENDLOOP. 
    ... 
  ENDMETHOD. "main 
ENDCLASS. "application

```

## Working With Basic Data Objects and Data Type

### Data Objects in ABAP

A data object in an ABAP program represents a reserved section of the program memory.

ABAP knows three types of data objects: _Variables_, _Constants_, and _Literals_.

![Ilustration - Variables, Constants and Literals](https://learning.sap.com/service/media/topic/bd745e90-2cf2-49af-8d95-2d8b9a59610e/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Data_Objects_and_Types_001.png)

- **Variables:** A variable is a data object with content that can change during runtime. A variable is identified by a name. The name is also used to address the data object at runtime. The starting value of an ABAP variables is always well-defined.

- **Constants:** Constants are similar to variables. But in contrast to variables the value is hard coded in the source code and must not change during runtime. Like variables, constants have a name by which they can be re-used.

- **Literals:** The value of literals is also hard-coded in the source code. In contrast to constants, literals don't have a name. Because of that you cannot reuse a literal. Only use literals to specify the values for constants and the starting values for variables.

ABAP data objects are always typed: Every data object is based on a data type which determines the kind of information they can contain. The data type of an ABAP data object stays the same throughout a program execution.

### Declaration of Variables

A variable in an ABAP program is declared with keyword DATA.

A DATA statement consists of three parts. Let's look at each part in more detail.

![Ilustration - Declaration of Variables](https://learning.sap.com/service/media/topic/bd745e90-2cf2-49af-8d95-2d8b9a59610e/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Data_Objects_and_Types_002.png)

- **DATA:** Keyword ``DATA`` is followed by the name of the variable. The name of a variable may be up to 30 characters long. It may contain the characters A-Z, the digits 0-9, and the underscore character. The name must begin with a letter or an underscore.

- **TYPE:** The type of the variable is specified after addition ``TYPE``. In the example, built-in types i (for integer numbers) and string (character string with variable length) are used.

- **VALUE:** Addition ``VALUE`` is optional and you can use it to specify a start value for the variable. If VALUE is missing, the variable is created with an initial value that depends on the technical type of the variable.

### Sources of ABAP Data Types

![Ilustration - Sources of ABAP Data Types](https://learning.sap.com/service/media/topic/bd745e90-2cf2-49af-8d95-2d8b9a59610e/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Data_Objects_and_Types_003.png)

**ABAP offers the following sources of Data Types:**

**ABAP Built-in:** ABAP has a set of 13 predefined data types for simple numeric, char-like, and binary data objects.

**TYPES Statement:** Statement TYPES allows you to define data types and reuse them in different places, depending on the location of the definition.

**ABAP Dictionary:** The ABAP Dictionary is a part of the ABAP Repository. Among other things, it manages global data types which are available throughout the system. ABAP Dictionary types not only define technical properties, they add semantic information, for example, labels. ABAP Dictionary types are particularly useful when implementing user interfaces.

### Try It Out: Predefined ABAP TYPES

1. Like in the first exercise of this course, create a new global class that implements interface `IF_OO_ADT_CLASSRUN`.

2. Copy the following code snippet to the implementation part of method `if_oo_adt_classrun~main( )`:

    ```abap

    * Data Objects with Built-in Types
    *********************************************************************

        " comment/uncomment the following declarations and check the output
        DATA variable TYPE string.
    *     DATA variable TYPE i.
    *     DATA variable TYPE d.
    *     DATA variable TYPE c LENGTH 10.
    *     DATA variable TYPE n LENGTH 10.
    *     DATA variable TYPE p LENGTH 8 DECIMALS 2.
    
    * Output
    ***********************************************************************
    
        out->write(  'Result with Initial Value)' ).
        out->write(   variable ).
        out->write(  '---------' ).
    
        variable = '19891109'.
    
        out->write(  'Result with Value 19891109' ).
        out->write(   variable ).
        out->write(  '---------' ).

    ```

3. Press ``CTRL + F3`` to activate the class and ``F9`` to execute it as a console app.
4. Analyze the console output. Uncomment different declarations of **variable**. Try your own declarations to get familiar with the concepts.

## Constants and Literals

### Declaration of Constants

![Ilustration - Declarations of Constants](https://learning.sap.com/service/media/topic/f7f5f39a-de5f-420e-a567-e49aed986f93/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Data_Objects_and_Types_007.png)

A constant is a data object with a hard-coded value that must not be changed during runtime. Any write access to a constant leads to a syntax error.

In ABAP, you declare a constant using keyword CONSTANTS. A CONSTANT statement consists of the same parts as a DATA statement. The only difference is, that the VALUE addition is mandatory.

You can use the VALUE addition in the special form VALUE IS INITIAL, if the value of the constant should be the type-specific initial value.

### Literals in ABAP

Literals are anonymous data objects with a hard-coded value. Literals are often used to define non-initial values for constants and non-initial starting values for variables.

![Ilustration - Literals in ABAP](https://learning.sap.com/service/media/topic/f7f5f39a-de5f-420e-a567-e49aed986f93/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Data_Objects_and_Types_008.png)

Technically, you can use literals anywhere in your code. To support readability and maintainability, it is recommended to define and use constants, instead.

**ABAP knows three types of literals:**

- Number Literals are integer numbers with or without sign. Number literals usually have data type I. Only if the value is too large for data type I, type P is used, with a sufficient length and without decimal places.
- Text Literals are character strings in simple quotes. Text literals have type C. The length is derived from the content in quotes. Trailing blanks are ignored.
- String Literals are character strings in a pair of back quotes (`). String literals are of type STRING. They should be used to provide values for string typed data objects.

## Try It Out: Data Objects

1. Like in the first exercise of this course, create a new global class that implements interface ``IF_OO_ADT_CLASSRUN``.
2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:
    
    ```abap

    * Example 1: Local Types
    *********************************************************************
    
    * Comment/Uncomment the following lines to change the type of my_var
        TYPES my_type TYPE p LENGTH 3 DECIMALS 2.
    *    TYPES my_type TYPE i .
    *    TYPES my_type TYPE string.
    *    TYPES my_type TYPE n length 10.
    
    * Variable based on local type
        DATA my_variable TYPE my_type.
    
        out->write(  `my_variable (TYPE MY_TYPE)` ).
        out->write(   my_variable ).
    
    * Example 2: Global Types
    **********************************************************************
    
    * Variable based on global type .
        " Place cursor on variable and press F2 or F3
        DATA airport TYPE /dmo/airport_id VALUE 'FRA'.
    
        out->write(  `airport (TYPE /DMO/AIRPORT_ID )` ).
        out->write(   airport ).
    
    * Example 3: Constants
    **********************************************************************
    
        CONSTANTS c_text   TYPE string VALUE `Hello World`.
        CONSTANTS c_number TYPE i      VALUE 12345.
    
        "Uncomment this line to see syntax error ( VALUE is mandatory)
    *  constants c_text2   type string.
    
        out->write(  `c_text (TYPE STRING)` ).
        out->write(   c_text ).
        out->write(  '---------' ).
    
        out->write(  `c_number (TYPE I )` ).
        out->write(   c_number ).
        out->write(  `---------` ).
    
    * Example 4: Literals
    **********************************************************************
    
        out->write(  '12345               ' ).    "Text Literal   (Type C)
        out->write(  `12345               ` ).    "String Literal (Type STRING)
        out->write(  12345                  ).    "Number Literal (Type I)
    
        "uncomment this line to see syntax error (no number literal with digits)
    *    out->write(  12345.67                  ).

    ```
    
3. Press ``CTRL + F3`` to activate the class and ``F9`` to execute it as a console app.
4. Play around with the source code to get familiar with the concepts.
   - Uncomment different declarations of **my_type** and use **F2** and **F3** to analyze the definition of variable **my_variable**.
   - Use **F2** and **F3** to analyze the definition of variable **airport**.
   - Uncomment the definition of constant **c_text2** to see the syntax error.
   - Uncomment the last code line to see the syntax error.
   - ``...``

## Value Assignments to Data Objects

In SAP ABAP, the primary methods for assigning values to data objects involve the **``assignment operator``** (``=``), the legacy ``MOVE`` statement, and modern constructor expressions like ``VALUE``. Assignments between data objects follow specific conversion rules, which allow for a wide range of automatic type conversions.

### Core Assignment Mechanisms

- **``Assignment Operator`` (``=``):** This is the modern, recommended way to assign values. It has the same effect as the MOVE statement.

```ABAP
DATA lv_source TYPE i VALUE 10.
DATA lv_target TYPE i.

lv_target = lv_source. " Assigns the value of lv_source to lv_target
```

- **``MOVE ... TO ...`` Statement:** This is the older, obsolete form of assignment but might still be encountered in legacy code.

```ABAP
MOVE lv_source TO lv_target. " Obsolete form
```

- **Initialization with ``CLEAR``:** The ``CLEAR`` statement resets a data object to its type-specific initial value (e.g., 0 for numeric types, blank for character types).
 
### Modern Value Construction and Assignment

Modern ABAP introduces constructor expressions for more efficient and readable value assignments, especially for complex data types.

**``VALUE`` Operator:** Used to construct and assign values to complex data objects like structures and internal tables.

``` ABAP
TYPES: BEGIN OF ty_address,
         street TYPE c LENGTH 30,
         city   TYPE c LENGTH 20,
       END OF ty_address.

DATA: ls_address TYPE ty_address.

" Assigning a structure using VALUE
ls_address = VALUE ty_address( street = 'Main Street' city = 'Barro' ).

```

**``NEW`` Operator:** Used for creating anonymous data objects dynamically and constructing their values at the same time, returning a reference to the newly created object.

### Advanced Assignments

- **Field Symbols and ``ASSIGN``:** Field symbols are placeholders that point to the memory area of a data object. The ASSIGN statement links a data object (or a part of it) to a field symbol.

``` ABAP
DATA lv_data TYPE c LENGTH 20 VALUE 'Hello World'.
FIELD-SYMBOL <fs> TYPE any.

ASSIGN lv_data TO <fs>.
IF <fs> IS ASSIGNED.
  <fs> = 'ABAP World'. " Changes the value of lv_data
ENDIF.

```

- **Dereferencing Data References:** To assign a value to the data object pointed to by a data reference variable, you use the dereferencing operator ``->*``.

``` ABAP
DATA lr_data TYPE REF TO data.
" ... (code to create and assign reference to lr_data) ...
lr_data->* = 'Some Value'.

```

- **``MOVE-CORRESPONDING``:** This statement assigns components of a source structure to identically named components of a target structure.

### Conversion Rules

ABAP has extensive automatic conversion rules. When assigning values between data objects of different types, ABAP attempts to convert the source value to match the target type. However, it is a best practice to only perform assignments between compatible types or use the ``EXACT`` operator for lossless assignments to avoid unexpected results or runtime exceptions. 

## Implicit Type Conversions

Although ABAP is a typed programming language, in a value assignment the type of the source expression and the type of the target variable can be different. If that is the case, the runtime attempts in a type conversion.

![Ilustration - Implicit Type Conversions](https://learning.sap.com/service/media/topic/bfbe0fb7-1c3d-4db9-bcea-89e584fc7ae7/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Value_Assignements_002.png)

As shown in the figure, if possible, try to avoid type conversions for the following reasons:

-   Additional Runtime consumption: Values with type conversions require more runtime than value assignments with identical types.

-   Potential Runtime Errors: Some combinations of source type and target type can lead to runtime errors. If, for example, the target variable has a numeric type l and the source expression has a character like type, then the runtime will raise an error if it cannot translate the text into a number.

-   Potential Information Loss: Some combinations of source type and target type do not cause runtime errors but can lead to a loss of data. If, for example, source and target are of both of type C but the type of the target variable is shorter. Then the runtime simply truncates the value of the source.

## Resetting Variables

Statement ``CLEAR`` is a special kind of value assignment. It is used to reset a variable to its type-specific initial value.

![Ilustration - Resetting Values](https://learning.sap.com/service/media/topic/bfbe0fb7-1c3d-4db9-bcea-89e584fc7ae7/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Value_Assignements_003.png)

> **NOTE:** The ``CLEAR`` statement disregards the starting value from the ``VALUE`` addition. After ``CLEAR`` the variable always contains the type-specific initial value.

## Inline Declarations in Value Assignments

In modern SAP ABAP (release 7.40 and later), inline declarations allow you to declare variables or field symbols directly at their first point of use, rather than at the beginning of a program. This feature enhances code readability and reduces the number of explicit ``DATA ``statements.

### How to use Inline Declarations in Value Assignments

You use the declaration operators ``DATA(...)`` or ``FINAL(...)`` (for immutable variables) on the left-hand side (LHS) of an assignment or in other specific "declaration positions". The ABAP compiler automatically infers the variable's data type from the context or the value being assigned on the right-hand side (RHS).

**Syntax:**

``` ABAP
" Traditional declaration
DATA lv_text TYPE string.
lv_text = 'Hello World'.

" Inline declaration
DATA(lv_text) = 'Hello World'.

" Inline declaration of an immutable variable (cannot be reassigned)
FINAL(lv_date) = cl_demo_date_time=>get_user_date( ).
```

### Key Use Cases and Examples

Inline declarations can be used in numerous contexts beyond simple assignments:

- **Assignments:** Declaring a variable and assigning it a value simultaneously.

```ABAP
DATA(lv_count) = 50 * ( 2 / 5 ). " Be careful with integer division type inference
```

- **Internal Tables and Loops:** Declaring an internal table as the target of a SELECT statement, or declaring a work area/field symbol within a LOOP.

``` ABAP
SELECT * FROM scarr INTO TABLE @DATA(lt_carriers).

LOOP AT lt_carriers INTO DATA(ls_carrier).
  " Use ls_carrier
ENDLOOP.

LOOP AT lt_carriers ASSIGNING FIELD-SYMBOL(<fs_carrier>).
  " Use <fs_carrier>
ENDLOOP.
```

- **Method Calls:** Declaring variables to receive the results of method calls or function modules.

``` ABAP
DATA(lx_ixml) = cl_ixml=>create( ).

" For importing parameters
oref->meth( IMPORTING p1 = DATA(la_param1) ).
```

- **Value Operator:** Using the ``VALUE`` operator to construct data with an inline declaration.

``` ABAP
TYPES t_itab TYPE TABLE OF i WITH NON-UNIQUE KEY table_line.
DATA(lt_itab) = VALUE t_itab( ( 1 ) ( 2 ) ( 3 ) ).
```

### Important Considerations

- **ABAP Version:** Inline declarations are available in ABAP 7.40 and above.
- **Static Type Determination:** The compiler must be able to determine the variable's type statically (at compile time).
- **Context:** A variable declared inline cannot be used in a read position within the same statement where it is declared.
- **Performance:** For specific use cases like ``SELECT`` statements, using inline declarations can create standard tables with no explicit keys, which might impact performance in subsequent operations (e.g., in a ``LOOP`` where a sorted key would be beneficial). You may need to define the type explicitly beforehand for specific table types (e.g., HASHED or SORTED) to ensure optimal performance.
- **Tool Support:** ABAP Development Tools (ADT) in Eclipse provide quick assists to convert inline declarations into explicit ones, if needed for complex scenarios or type modification.

## Processing Data

### Arithmetic Calculations

Arithmetic expressions are ABAP expressions with a combination of values, operators, and functions that the runtime system processes to calculate a result. For arithmetic expressions the result type depends on the type of the operands used as input to the expression.

You can use an arithmetic expression in any reading operand position, for example, the right-hand side of a value assignment.

## Try It Out: Arithmetic Calculations

1. Like in the first exercise of this course, create a new global class that implements interface ``IF_OO_ADT_CLASSRUN``.
2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:

    ``` ABAP

    *  Declarations
    **********************************************************************

        " comment/uncomment these line for different result types
        TYPES t_result TYPE p LENGTH 8 DECIMALS 2.
    *     TYPES t_result TYPE p LENGTH 8 DECIMALS 0.
    *     TYPES t_result TYPE i.
    
        DATA result TYPE t_result.

    *  Calculations
    **********************************************************************
        " comment/uncomment these lines for different calculations

        result = 2 + 3.
    *     result = 2 - 3.
    *     result = 2 * 3.
    *     result = 2 / 3.
    *
    *     result = sqrt( 2 ).
    *     result = ipow( base = 2 exp = 3 ).
    *
    *    result = ( 8 * 7 - 6 ) / ( 5 + 4 ).
    *   result = 8 * 7 - 6 / 5 + 4.

    * Output
    **********************************************************************

        out->write( result ).

    ```
    
3. Press ``CTRL + F3`` to activate the class and ``F9`` to execute it as a console app.
4. Play around with the source code to get familiar with the concepts.
   - Uncomment different calculations and type definitions.
   - Implement your own calculations.

## String Processing

![Ilustration - String Processing 1](https://learning.sap.com/service/media/topic/ca63364f-4ad8-490f-9b8b-3e414b496003/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_String_Processing_001.png)

String templates are ABAP expressions of result type string. You can use string templates in any reading operand position, for example, the right-hand side of a value assignment.

A string template begins and ends with a pipe-symbol ( | ). The simplest possible string template contains nothing but literal text. In this form a string template is not really different from a string literal.

What distinguishes a string template from a string literal is the ability to embed expressions. An embedded expression is an ABAP expression surrounded by a pair of curly brackets ( { and } ). At runtime, ABAP evaluates the embedded expression and translates the result into a string. In the final result, this string replaces the embedded expression (together with the surrounding curly brackets).

> **Note:** ABAP syntax requires at least one blank after the opening bracket and at least one blank before the closing bracket.

Of course one string template can contain more than one embedded expression.

Inside the curly brackets you can place any kind of ABAP expression: arithmetic expressions, like in the example above, but single variables or even literals can serve as embedded expressions.

![Ilustration - String Processing 2](https://learning.sap.com/service/media/topic/ca63364f-4ad8-490f-9b8b-3e414b496003/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_String_Processing_002.png)

One important use case for string templates is the controlled formatting of data for output.

In the first example, variable the_date is of type d and contains a date in the internal (raw) format YYYYMMDD (where YYYY stands for the year, MM for the two-digit month and DD for the two-digit date). When you use variable the_date as embedded expression in a string template the result will be the same as the internal format. But when you add format option DATE = <date_format> within the curly brackets, the system will format the value as a date. If you add DATE = ISO, the output will be in ISO format. With DATE = USER the output format depends on the user settings of the current user.

The second example illustrates some of the options you can use for formatting numbers. Using NUMBER you control the general formatting of numbers, for example, whether a decimal point is used or a decimal comma. Using SIGN you control the position of the sign and whether a plus sign (+) is displayed or not. Using STYLE you can choose from several pre-defined styles, like a scientific style or an engineering style.

![Ilustration - String Processing 3](https://learning.sap.com/service/media/topic/ca63364f-4ad8-490f-9b8b-3e414b496003/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_String_Processing_003.png)

You can join fields together using the concatenation operator &&. You can join any combination of data objects and string expressions.

The constituent parts of the expression are joined with no space or other separator between them. If you need spaces or another separator character, you must remember to insert it yourself as part of the expression as shown in the second example.

## Try It Out: String Processing

1.  Like in the first exercise of this course, create a new global class that implements interface ``IF_OO_ADT_CLASSRUN``.
2.  Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:

    ``` ABAP

     METHOD if_oo_adt_classrun~main.

    *  Declarations
    **********************************************************************
        TYPES t_amount TYPE  p LENGTH 8 DECIMALS 2.

        DATA amount   TYPE t_amount VALUE '3.30'.
        DATA amount1  TYPE t_amount VALUE '1.20'.
        DATA amount2  TYPE t_amount VALUE '2.10'.

        DATA the_date  TYPE d                     VALUE '19891109'.
        DATA my_number TYPE p LENGTH 3 DECIMALS 2 VALUE '-273.15'.

        DATA part1 TYPE string VALUE `Hello`.
        DATA part2 TYPE string VALUE `World`.

    *  String Templates
    **********************************************************************

        " comment/uncomment the following lines for different examples
        DATA(text) = |Hello World|.
    *     DATA(text) = |Total: { amount } EUR|.
    *     DATA(text) = |Total: { amount1 + amount2 } EUR|.
    
    *  Format Options
    **********************************************************************

        "Date
    *    DATA(text) = |Raw Date: { the_date             }|.
    *    DATA(text) = |ISO Date: { the_date Date = ISO  }|.
    *    DATA(text) = |USER Date:{ the_date Date = USER }|.

        "Number
    *    DATA(text) = |Raw Number { my_number                    }|.
    *    DATA(text) = |User Format{ my_number NUMBER = USER      }|.
    *    DATA(text) = |Sign Right { my_number SIGN = RIGHT       }|.
    *    DATA(text) = |Scientific { my_number STYLE = SCIENTIFIC }|.

    * String expression (concatenation Operator)
    **********************************************************************

    *    DATA(text) = part1 && part2.
    *    DATA(text) = part1 && | | && part2.
    *    DATA(text) = |{ amount1 } + { amount2 }| &&
    *                 | = | &&
    *                 |{ amount1 + amount2 }|.

    * Output
    **********************************************************************

        out->write( text ).

    ```
    
3.  Press CTRL + F3 to activate the class and F9 to execute it as a console app.
4.  Play around with the source code to get familiar with the concepts.
    - Uncomment different value assignments for inline-declared variable **text** to see different result.
    - Try your own assignements.

## Declare Variables and Process Data

In this exercise, you create a program that does a calculation based on two numbers, then formats and outputs the result.

- **Template:** none
- **Solution:** ``/LRN/CL_S4D400_BTS_COMPUTE`` (global Class)

### Task 1: Variable Declaration

In your own package, create a new global class and define numeric variables for the input.

#### Steps

1. Create a new global class that implements the interface **IF_OO_ADT_CLASSRUN** (suggested name: **ZCL_##_COMPUTE**, where ## stands for your group number).

  a. Choose _File_→_New_→_ABAP Class_.
   
  b. Enter the name of your package in the _Package_ field. In the _Name_ field, enter the name **ZCL_##_COMPUTE**, where ## stands for your group number.

  c. Enter a description.

  d. In the _Interfaces_ group box, choose _Add_.

  e. Enter **IF_OO_ADT_CLASSRUN**. When the interface appears in the hit list, double-click it to add it to the class definition.

  f. Choose _Next_.

  g. Select your transport request and choose _Finish_.

2. In the ``IF_OO_ADT_CLASSRUN~MAIN`` method, declare two variables for integer values (suggested names: **number1** and **number2**).

  a. Add the following code:

    ``` ABAP
    DATA number1 TYPE i.
    DATA number2 TYPE i.
    ```

3. Implement a value assignment for each of the two numeric variables. Assign a negative value to the first variable and a positive value to the second variable.

  a. Add the following code:
          
    ``` ABAP
    number1 = -8.
    number2 =  3. 
    ```

### Task 2: Data Processing

Calculate a ratio and write the result to the console.

#### Steps

1. Implement an arithmetic expression that calculates the ratio of the two numbers. Store the result in a variable that you declare using an inline declaration (suggested name: **result**).

  a. Add the following code:

    ``` ABAP
    DATA(result) = number1 / number2.
    ```

2. Implement a string template that puts together a text like this:"6 / 3 = 2", with the actual values of the variables instead of 6, 3, and 2. Store the result in a variable that you declare using an inline declaration (suggested name: **output**).
    
  a. Add the following code:
          
    ``` ABAP
    DATA(output) = |{ number1 } / { number2 } = { result }|.          
    ```
    
3. Call method **out->write( … )** to write the text to the console.

  a. Proceed as you have done in the previous exercise.

### Task 3: Activate and Test

Activate and test the console app.

#### Steps

1. Activate the class.

      a. Press **Ctrl + F3** to activate the class.

2. Execute the console app.

      a. Press **F9** to execute the console app.

3. Test the app with different input values.

      a. Edit the value assignments, then activate and execute as before.

### Task 4: Control the Result Precision

Make sure the result is rounded to a value with 2 decimal places, and not to an integer value.

#### Steps

1. Instead of an inline declaration, use an explicit declaration of variable **result**. Use a numeric type that allows to specify the number of decimal places.

  a. At the beginning of method ``if_oo_adt_classrun~main``, add the following code:
          
    ``` ABAP
    DATA result TYPE p LENGTH 8 DECIMALS 2.
    ```
  
  b. In the code line with the inline declaration, replace ``DATA(result)`` with ``result``.
    
2. Activate the class and test again.

  a. Proceed as you have done before.

  b. Compare your code to the following extract from the model solution:

    ``` ABAP
    METHOD if_oo_adt_classrun~main.
          
    *  Declarations
    ***************************
          
          DATA number1 TYPE i.
          DATA number2 TYPE i.
          
          DATA result TYPE p LENGTH 8 DECIMALS 2.
          
              
    *  Input Values
    **************************
          
          number1 = -8.
          number2 =  3.
          
    *  Calculation
    **************************
          
    *     DATA(result) = number1 / number2.   "with inline declaration 
           
        result = number1 / number2.
          
         DATA(output) = |{ number1 } / { number2 } = { result }|.
          
    *  Output
    **************************
          
          out->write( output ).
          
    ENDMETHOD.
    ```

## Using Control Structures in ABAP

### Conditional Branching

![](https://learning.sap.com/service/media/topic/db02fc39-132b-4b1e-b367-a1c3f9ba65b0/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-__Conditional_Branching_001.png)

A conditional branching is a control structure that allows you to make the execution of code dependent on logical conditions.

The most common conditional branching consists of a pair of keywords IF and ENDIF. The code block between IF and ENDIF is only executed, if the condition after IF is fulfilled.

You can add more code blocks by extending the IF … ENDIF structure with up to one keyword ELSE and an arbitrary number of keywords ELSEIF. By adding keyword ELSE you ensure that always exactly one of the code blocks is executed. If ELSE is missing it can happen that none of the code blocks is executed.

The code block to be executed is determined as follows:

- First, the IF condition is evaluated. If it is fulfilled, the related code block is executed and the program continues after ENDIF.
- Only if the IF condition is not fulfilled, the condition after the first ELSEIF is evaluated. If it is fulfilled, the related code block is executed and the program continues after ENDIF.
- This is done consecutively for all ELSEIF conditions. If none of the conditions is fulfilled and the structure contains ELSE, the code block after ELSE is executed. Otherwise the none of the code blocks is executed.

> **Hint:** As opposed to many other programming languages, ABAP requires a delimiter (.) after each of the logical conditions and even after keyword ELSE.

![](https://learning.sap.com/service/media/topic/db02fc39-132b-4b1e-b367-a1c3f9ba65b0/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-__Conditional_Branching_002.png)

Logical conditions are a combination of comparisons, logical operations, expressions and functions that the runtime system evaluates to decide whether the condition is true or false.

The most common use-case for logical conditions is after keywords IF or ELSEIF in an IF ... ENDIF. structure.

The first example is a simple comparison: The condition is true if the two data objects x and y have the same value.

The second example is a bit more sophisticated: Either the value of x is greater than or equal to y and less than twice the value of y or it is less than or equal to y and greater than twice the value of y.

The third example makes use of arithmetic function abs( ) and logical expression BETWEEN <expression1> AND <expression2> . The condition is true if the absolute value of x lies between the absolute value of y and the absolute value of two times y.

For simple value comparisons you can use operators =, <>, >, <, >=, and <=. You can not only compare the values of data objects, but the values of many other expressions, like the arithmetic expression 2 * y in the example.

> **Note:** ABAP uses the same symbol (=) for value assignments and for value comparisons. The distinction is made based on the position.

You can use operators AND and OR to combine logical expressions and operator NOT to negate an expression. Without brackets, NOT binds stronger than AND and AND stronger than OR.

ABAP knows some special logical expressions:

- <data object> IS INITIAL is true if <data object> contains its type-specific initial value
- <data object> IS NOT INITIAL is true if <data object> contains a value that is different from the type-specific initial value
- <data object> BETWEEN <expression1> AND <expression2>

Some special ABAP functions are predicate functions. This means that they are logical conditions themselves. Contains( ) is a function that compares character-like values and line_exists( ) performs an existence check for a row in an internal table.

![](https://learning.sap.com/service/media/topic/db02fc39-132b-4b1e-b367-a1c3f9ba65b0/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-__Conditional_Branching_003.png)

A second technique for conditional branching is the CASE … WHEN .. ENDCASE control structure.

Conditional branching with CASE .. ENDCASE is a special case of the more general branching with IF … ENDIF. You can use CASE in situations where the branching depends on the value of a single data object, which you consecutively compare to a set of possible values, using an equals comparison each time.

In the example, the value of data object number is compared to values 1 and 2. If the value equals 1, <code_block_1> is executed and if the value euqals 2, <code_block_2> is executed instead. For any other value, the code block after WHEN OTHERS is executed.

Any conditional branching with CASE … ENDCASE could be implemented with an IF … ENDIF structure, as well. This is illustrated with the example on the right.

> **Hint:** You should use CASE … ENDCASE when dealing with the special case to increase readability of your code.

### Try It Out: Conditional Branching

1. Like in the first exercise of this course, create a new global class that implements interface ``IF_OO_ADT_CLASSRUN``.

2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:

  ``` ABAP

  * Declarations
  **********************************************************************

      CONSTANTS c_number TYPE i VALUE 0.
  *    CONSTANTS c_number TYPE i VALUE 1.
  *    CONSTANTS c_number TYPE i VALUE 2.
  *    CONSTANTS c_number TYPE i VALUE -1.
  *    CONSTANTS c_number TYPE i VALUE -2.

  * Example 1: Simple IF ... ENDIF.
  **********************************************************************

      out->write(  `--------------------------------` ).
      out->write(  `Example 1: Simple IF ... ENDIF.` ).
      out->write(  `-------------------------------` ).

      IF c_number = 0.
        out->write( `The value of C_NUMBER equals zero`   ).
      ELSE.
        out->write( `The value of C_NUMBER is NOT zero`   ).
      ENDIF.

  * Example 2: Optional Branches ELSEIF and ELSE
  **********************************************************************

      out->write(  `--------------------------------------------` ).
      out->write(  `Example 2: Optional Branches ELSEIF and ELSE` ).
      out->write(  `--------------------------------------------` ).

      IF c_number = 0.
        out->write( `The value of C_NUMBER equals zero`            ).
      ELSEIF c_number > 0.
        out->write( `The value of C_NUMBER is greater than zero`   ).
      ELSE.
        out->write( `The value of C_NUMBER is less than zero`      ).
      ENDIF.
    
  *  Example 3: CASE ... ENDCASE
  **********************************************************************
    
      out->write(  `---------------------------` ).
      out->write(  `Example 3: CASE ... ENDCASE` ).
      out->write(  `---------------------------` ).
    
      CASE c_number.
        WHEN 0.
          out->write( `The value of C_NUMBER equals zero`             ).
        WHEN 1.
          out->write( `The value of C_NUMBER equals one`              ).
        WHEN 2.
          out->write( `The value of C_NUMBER equals two`              ).
        WHEN OTHERS.
          out->write( `The value of C_NUMBER equals non of the above` ).
      ENDCASE.
  ```

3.  Press ``CTRL + F3`` to activate the class and ``F9`` to execute it as a console app.
4.  Analyze the console output. Play around with the source code to get familiar with the concepts; Uncomment different declarations of constant **c_number** with different values to see, which branches of the code get executed.

## Exception Handling

![](https://learning.sap.com/service/media/topic/fcebe95d-4308-4b5a-a822-441ea859ec36/S4D400_24_en-US_media/S4D400_24_en-US_images/03_-_Handle_Exceptions_001.png)

In ABAP, an exception is an error situation during execution of an ABAP program. An exception is raised by the code that detects the error situation.

Depending on who raises the exception, we distinguish between system exceptions and application exceptions.

Without further measures exceptions result in runtime errors. A runtime error terminates the program and is documented by default in a short dump.

You can avoid the runtime error if the exception in question is catchable. A catchable exception can be treated in the program using statements TRY … CATCH … ENDTRY.

All application exceptions and many system exceptions are catchable. Later in this course you will learn how to raise application exceptions. At this point we will focus on the handling of catchable system exceptions.

![](https://learning.sap.com/service/media/topic/fcebe95d-4308-4b5a-a822-441ea859ec36/S4D400_24_en-US_media/S4D400_24_en-US_images/03_-_Handle_Exceptions_003.png)

To prevent a program from terminating because of a catchable exception, you have to surround the code from where the exception originates with statements TRY and ENDTRY. By doing so, the code becomes part of the TRY block of the TRY … ENDTRY structure.

Before the ENDTRY statement you have to add a CATCH statement followed by the ID of the exception you want to handle. Optionally, you can add more than one CATCH statement to handle several different exceptions. Each CATCH statement should be followed by code to handle this exception. This code is called the CATCH block.

When program execution reaches the TRY statement, it continues with the code in the TRY block. Three things can happen then:

1. If no exception is raised during the TRY block, the CATCH blocks are ignored. Execution continues after the ENDTRY statement.
2. If an exception is raised during the TRY block, for which a matching CATCH exists, execution of the TRY block is terminated and the CATCH block for this exception is executed. Afterward, execution continues after the ENDTRY statement.
3. If an exception is raised during the TRY block for which no matching CATCH exists, the program terminates with a runtime error.

Now that you have learned about exceptions, let's see how you can handle them.

### Try It Out: Exception Handling

1. Like in the first exercise of this course, create a new global class that implements interface ``IF_OO_ADT_CLASSRUN``.
2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:

  ``` ABAP
  * Declarations
  **********************************************************************
      DATA result TYPE i.

      DATA numbers TYPE TABLE OF i.
    
  * Preparation
  **********************************************************************
    
      APPEND 123 TO numbers.
    
  * Example 1: Conversion Error (no Number)
  **********************************************************************
    
      CONSTANTS c_text TYPE string VALUE 'ABC'.
  *    CONSTANTS c_text TYPE string VALUE '123'.
    
      out->write(  `---------------------------` ).
      out->write(  `Example 1: Conversion Error` ).
      out->write(  `---------------------------` ).
    
      TRY.
          result = c_text.
          out->write( |Converted content is { result }|  ).
        CATCH cx_sy_conversion_no_number.
          out->write( |Error: { c_text } is not a number!| ).
      ENDTRY.
    
  * Example 2: Division by Zero
  **********************************************************************
    
      CONSTANTS c_number TYPE i VALUE 0.
  *     CONSTANTS c_number TYPE i VALUE 7.
    
      out->write(  `---------------------------` ).
      out->write(  `Example 2: Division by Zero` ).
      out->write(  `---------------------------` ).
    
      TRY.
          result = 100 / c_number.
          out->write( |100 divided by { c_number } equals { result }| ).
        CATCH cx_sy_zerodivide.
          out->write(  `Error: Division by zero is not defined!` ).
      ENDTRY.
    
  * Example 3: Itab Error (Line Not Found)
  ***********************************************************************
    
      CONSTANTS c_index TYPE i VALUE 2.
  *    CONSTANTS c_index TYPE i VALUE 1.
    
      out->write(  `-------------------------` ).
      out->write(  `Example 3: Line Not Found` ).
      out->write(  `-------------------------` ).
    
      TRY.
          result = numbers[ c_index ].
          out->write( |Content of row { c_index } equals { result }| ).
        CATCH cx_sy_itab_line_not_found.
          out->write(  `Error: Itab has less than { c_index } rows!` ).
      ENDTRY.
    
    
  * Example 4: Combination of Different Exceptions
  ***********************************************************************
  *    CONSTANTS c_char TYPE c LENGTH 1 VALUE 'X'.
  *    CONSTANTS c_char TYPE c length 1 value '0'.
      CONSTANTS c_char TYPE c LENGTH 1 VALUE '1'.
  *    CONSTANTS c_char TYPE c length 1 value '2'.
    
      out->write(  `----------------------` ).
      out->write(  `Example 4: Combination` ).
      out->write(  `----------------------` ).
    
      TRY.
          result = numbers[ 2 / c_char ].
          out->write( |Result: { result } | ).
        CATCH cx_sy_zerodivide.
          out->write( `Error: Division by zero is not defined`  ).
        CATCH cx_sy_conversion_no_number.
          out->write( |Error: { c_char } is not a number! | ).
        CATCH cx_sy_itab_line_not_found.
          out->write( |Error: Itab contains less than { 2 / c_char } rows| ).
      ENDTRY.
  ```
    
3.  Press ``CTRL + F3`` to activate the class and ``F9`` to execute it as a console app.
4.  Analyze the console output. Play around with the source code to get familiar with the concept:
  - Comment the exception handling to make the system raise a runtime error.
  - Change the values of constants **c_number**, **c_text**, **c_index**, and **c_char** in a way that no exceptions are raised.

## Iterations

![](https://learning.sap.com/service/media/topic/e790fd3a-9a6c-464b-889d-d5b0c6f6741e/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Implement_Itterations_001.png)

Iterations are control structures that define a block of code which is executed several times.

The simplest form of iteration consists of a code block surrounded by pair of statements DO and ENDDO. Without further measures this establishes an endless loop which must be avoided by one of the following possibilities:

**Specified number of iterations:** By extending the DO statement with an integer expression followed by keyword TIMES, you can specify explicitly how often the code block is to be iterated. The integer expression can be as simple as number literal, but also arithmetic calculations are an option. If the value of the expression equals 0, the code block between DO and ENDDO is not executed at all and the program immediately continues with the code after ENDDO.

**Abort based on a logical condition:**
  - You can abort an iteration any time using the EXIT statement. The program then continues with the code after ENDDO. Be aware that outside of iterations EXIT has a different effect. There it terminates the processing of the current processing block, for example the current method.

  - Usually, EXIT is surrounded by IF and ENDIF to terminate the iteration depending on an abort condition. Be aware that such iterations can turn into endless loops, if the abort condition never gets true.

  - Of course it is possible to combine the two techniques, that is, explicitly specify the number of iterations and then leave the iteration with EXIT. Thus the number of iterations becomes a maximum number that might not be reached at runtime.

**Based on an internal table:** A third type of iteration is the LOOP … ENDLOOP structure that is used to consecutively read the rows of an internal table. Here, the number of iterations is determined by the number of rows in the internal table.

In the code block between DO and ENDDO, you can implement read-accesses to ABAP built-in data object sy-index. This integer variable serves as an iteration counter, that is, the ABAP runtime increases it by one at the beginning of each new iteration.

In contradiction to what you might be used to from other programming languages, sy-index starts with value 1 during the first iteration.

ABAP built-in variable sy-tabix can fulfill a similar purpose for iterations with LOOP. But be aware that strictly speaking sy-tabix is not really a counter but it identifies the position of the table row that is processed in the current iteration. Later we will see the difference when not all rows of an internal table are processed in a LOOP … ENDLOOP structure.

### Try It Out: Iterations

1. Like in the first exercise of this course, create a new global class that implements interface ``IF_OO_ADT_CLASSRUN``.
2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:
    
  ```ABAP
  * Declarations
  **********************************************************************
    
      CONSTANTS c_number TYPE i VALUE 3.
  *    CONSTANTS c_number TYPE i VALUE 5.
  *    CONSTANTS c_number TYPE i VALUE 10.
    
      DATA number TYPE i.
    
  * Example 1: DO ... ENDDO with TIMES
  **********************************************************************
    
      out->write(  `----------------------------------` ).
      out->write(  `Example 1: DO ... ENDDO with TIMES` ).
      out->write(  `----------------------------------` ).

      DO c_number TIMES.
        out->write(  `Hello World` ).
      ENDDO.
    
  * Example 2: DO ... ENDDO with Abort Condition
  **********************************************************************
    
      out->write(  `-------------------------------` ).
      out->write(  `Example 2: With Abort Condition` ).
      out->write(  `-------------------------------` ).
    
      number = c_number * c_number.
    
      " count backwards from number to c_number.
      DO.

        out->write( |{ sy-index }: Value of number: {  number }| ).
        number = number - 1.

        "abort condition
        IF number <= c_number.
          EXIT.
        ENDIF.
    
      ENDDO.
  ```

3.  Press ``CTRL + F3`` to activate the class and ``F9`` to execute it as a console app.
4.  Analyze the console output. Play around with the source code to get familiar with the concepts; Uncomment different declarations of constant **c_number** to see how the different values affect the result.

## Working with Simple Internal Tables

### Internal Tables

Internal tables are variable data objects in which you can store several values of identical type. This type has to be specified in the declaration and is called the row type of the internal table.

![](https://learning.sap.com/service/media/topic/d3b54459-70e2-4ab5-aa86-a9033abd0764/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Define_Simple_itab_001.png)

Each value occupies one row of the internal table. The number of rows is not restricted. Theoretically, you can store any number of values in one internal table. Limitations only come from technical boundaries like available memory or system configuration.

The initial value of an internal table is an empty table or, in other words, a table with 0 lines. There are different techniques for filling an internal table. The example uses the APPEND statement to add a new row at the end of the internal table and fill it with a value.

### Table Types

The type of an internal table is called a table type.

In the previous example we used TYPE TABLE OF in the DATA statement directly. The table type was bound to the declared variable.

As an alternative you can use TYPE TABLE OF in a TYPES statement to define a table type with a name. You can then use this table type, for example, in a DATA statement. The visibility of these types depends on the position of the TYPES statement.

![](https://learning.sap.com/service/media/topic/d3b54459-70e2-4ab5-aa86-a9033abd0764/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Define_Simple_itab_002.png)

There are also table types in the ABAP Dictionary. These table type are maintained with a dedicated editor. They are called global table types because they are visible anywhere in the system.

### Data Processing with Simple Internal Tables

![](https://learning.sap.com/service/media/topic/dbcb07f7-f2b8-44a5-b6e0-4d521c04e596/S4D400_24_en-US_media/S4D400_24_en-US_images/02_Process_data_simple_itab_002.png)

The initial value of an internal table is an empty table, that is, an internal table with zero rows.

You already learned that with statement CLEAR you can reset an ABAP variable to its type-specific initial value.

When you use CLEAR for an internal table, you delete all its content and set the number of rows to zero.

![](https://learning.sap.com/service/media/topic/dbcb07f7-f2b8-44a5-b6e0-4d521c04e596/S4D400_24_en-US_media/S4D400_24_en-US_images/02_Process_data_simple_itab_003.png)

There are various ways to retrieve data from an internal table. This example retrieves the content of a single row using an internal table expression. In the table expression, the name of the internal table is followed immediately by a pair of square brackets. An integer expression inside the brackets specifies the position of the row to be read.

> **Note:** Correct syntax requires at least one blank after the opening bracket and before the closing bracket.

#### Reading from an Internal Table in a Loop

![](https://learning.sap.com/service/media/topic/dbcb07f7-f2b8-44a5-b6e0-4d521c04e596/S4D400_24_en-US_media/S4D400_24_en-US_images/02_Process_data_simple_itab_005.png)

When you implement a loop over an internal table, you can use an inline declaration after addition INTO instead of declaring the work area explicitly with a DATA statement.

By doing so, you not only reduce the amount of code you have to type, you also ensure that the type of work area fits the line type of the internal table, because the type of the inline declared data object is derived from the context, which, in this case, is the internal table.

### Try It Out: Simple Internal Tables

1. Like in the first exercise of this course, create a new global class that implements interface ``IF_OO_ADT_CLASSRUN``.
2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:

  ``` ABAP
  * Declarations
  ***********************************************************************

      " Internal tables
      DATA numbers TYPE TABLE OF i.

      "Table type (local)
      TYPES tt_strings TYPE TABLE OF string.
      DATA texts1      TYPE tt_strings.

      " Table type (global)
      DATA texts2 TYPE string_table.

      " Work areas
      DATA number TYPE i VALUE 1234.
      DATA text TYPE string.

  * Example 1: APPEND
  ************************************************************************
    
      APPEND 4711       TO numbers.
      APPEND number     TO numbers.
      APPEND 2 * number TO numbers.

      out->write(  `-----------------` ).
      out->write(  `Example 1: APPEND` ).
      out->write(  `-----------------` ).

      out->write( numbers ).

  * Example 2: CLEAR
  ************************************************************************

      CLEAR numbers.

      out->write(  `----------------` ).
      out->write(  `Example 2: CLEAR` ).
      out->write(  `----------------` ).

      out->write( numbers ).

  * Example 3: table expression
  ************************************************************************
      APPEND 4711       TO numbers.
      APPEND number     TO numbers.
      APPEND 2 * number TO numbers.

      out->write(  `---------------------------` ).
      out->write(  `Example 3: Table Expression` ).
      out->write(  `---------------------------` ).

      number = numbers[ 2 ] .
    
      out->write( |Content of row 2: { number }|    ).
      "Direct use of expression in string template
      out->write( |Content of row 1: { numbers[ 1 ]  }| ).
    
  * Example 4: LOOP ... ENDLOOP
  ************************************************************************
        out->write(  `---------------------------` ).
        out->write(  `Example 4: LOOP ... ENDLOOP` ).
        out->write(  `---------------------------` ).
    
        LOOP AT numbers INTO number.
    
          out->write( |Row: { sy-tabix } Content { number }| ).
    
        ENDLOOP.
    
  * Example 5: Inline declaration in LOOP ... ENDLOOP
  ************************************************************************
      out->write(  `-----------------------------` ).
      out->write(  `Example 5: Inline Declaration` ).
      out->write(  `-----------------------------` ).
    
      LOOP AT numbers INTO DATA(number_inline).
        out->write( |Row: { sy-tabix } Content { number_inline }| ).
      ENDLOOP.
  ```

3.  Press ``CTRL + F3`` to activate the class and ``F9`` to execute it as a console app.
4.  Analyze the console output. Play around with the source code to get familiar with the concepts.

## Exercise - Work with Simple Internal Tables

In this exercise, you use iterations and simple internal tables to calculate and display the Fibonacci numbers.

> **Note:** The Fibonacci numbers are a sequence of numbers in which each number is the sum of the two preceding ones. The sequence starts with 0 and 1, and each subsequent number is the sum of the two previous numbers. The sequence has various mathematical properties, patterns, and applications in fields such as mathematics, computer science, nature, and art.

- **Template:** none
- **Solution:** ``/LRN/CL_S4D400_BTS_ITERATE`` (global Class)

### Task 1: Calculate the Numbers

In your own package, create a new global class. Define a constant for the number of iterations and an internal table for the Fibonacci numbers. Then calculate the Fibonacci numbers in an iteration and store them in the internal table.

#### Steps

1. Create a new global class that implements the interface **IF_OO_ADT_CLASSRUN** (suggested name: **ZCL_##_ITERATE**, where ## stands for your group number).

    a. Choose _File_→_New_→_ABAP Class_.

    b. Enter the name of your package in the _Package_ field. In the _Name_ field, enter the name **ZCL_##_ITERATE**, where ## stands for your group number.

    c. Enter a description.

    d. In the _Interfaces_ group box, choose _Add_.
   
    e. Enter **IF_OO_ADT_CLASSRUN**. When the interface appears in the hit list, double-click it to add it to the class definition.
   
    f. Choose _Next_.

    g. Select your transport request and choose _Finish_.

2. In the `IF_OO_ADT_CLASSRUN~MAIN` method, define a constant for the number of iterations (suggested name: **count**) with a small value, for example **20**.

    a. Add the following code:
      ``` ABAP
      CONSTANTS max_count TYPE i VALUE 20.
      ```

3. Declare a simple internal table to store the Fibonacci numbers (suggested name: **numbers**).

    a. Add the following code:

      ``` ABAP      
      DATA numbers TYPE TABLE OF i.
      ```

4. Implement an iteration that is executed **max_count** times.

    a. After the declarations, add the following code:

      ``` ABAP
      DO max_count TIMES.

      ENDDO.    
      ```

5. Inside the iteration, implement a case distinction based on the built-in iteration counter: In the first iteration, add the value **0** to the internal table **numbers**. In the second iteration, add the value **1** to the end of internal table **numbers**.

    > **Hint:** The built-in iteration counter is system field **sy-index**.

    a. Between `DO` and `ENDDO`, add the following code:

      ``` ABAP
      CASE sy-index.
        WHEN 1.
          APPEND 0 TO numbers.
        WHEN 2.
          APPEND 1 TO numbers.
      ENDCASE.
      ```

6. Add a branch that is executed for all other values. In this branch, calculate the new entry as the sum of the two preceding entries in **numbers**.

    > **Hint:** Access the preceding entries using index **sy-index - 1** and **sy-index - 2**.

    a. Adjust the code as follows:

    ``` ABAP
    CASE sy-index.
      WHEN 1.
        APPEND 0 TO numbers.
      WHEN 2.
        APPEND 1 TO numbers.
      WHEN OTHERS.
        APPEND numbers[  sy-index - 2 ]
             + numbers[  sy-index - 1 ]
            TO numbers.
    ENDCASE.
    ```

### Task 2: Prepare a Formatted Output

In a loop over the internal table, prepare a formatted output that lists each Fibonacci number with its respective sequential number. Prepare the output in another internal table of row type _string_.

#### Steps

1. At the beginning of the method **if_oo_adt_classrun~main**, declare an internal table of row type _string_ (suggested name: **output**).
    
2. Implement a loop over the internal table **numbers** to read the Fibonacci numbers one by one into a variable **number**.
    
    > **Hint:** Use an inline declaration for **number**.
    
    a. At the end of the method, add the following code:

      ``` ABAP
      LOOP AT numbers INTO DATA(number).
      ENDLOOP.
      ```

3. Declare an integer variable (suggested name: **counter**), that you set to zero before the loop and increase by 1 in each iteration.

    > **Hint:** You can use an inline declaration for **counter**.

    a. Adjust the code as follows:

      ``` ABAP
      DATA(counter) = 0.
      LOOP AT numbers INTO DATA(number).

        counter = counter + 1.

      ENDLOOP.
      ```

4. In each iteration, add a new row to the internal table **output** which contains the content of **counter** (l4 characters wide, left-justified), a colon (;) and the content of **number** (10 characters wide, right justified).
    
    > **Hint:** Use a string template with suitable format options to adjust the format.
    
    a. Adjust the code as follows:
 
      ``` ABAP
      DATA(counter) = 0.
        LOOP AT numbers INTO DATA(number).
          
          counter = counter + 1.
          
          APPEND |{ counter WIDTH = 4 }: { number WIDTH = 10 ALIGN = RIGHT }|
              TO output.
          
      ENDLOOP.
      ```

### Task 3: Output and Test

Write the result to the console. Then activate and test your class as a console app.

#### Steps

1. At the end of the method, call the method **out->write** to write the content of **output** to the console. Supply the importing parameter **name** with a suitable caption.

    a. After ENDLOOP., add the following code:

      ``` ABAP
      out->write( 
              data   = output
              name   = |The first { max_count } Fibonacci Numbers|
                    ) .
      ```

2. Activate the class.

    a. Press **Ctrl + F3** to activate the class.

3. Execute your class as a console app.

    a. Press **F9** to execute the class as console app.

## Debugging an ABAP Program

### Starting the Debugger

![](https://learning.sap.com/service/media/topic/bdd75ef0-ae08-4b90-b049-976bf699ae81/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Enter_debugging_mode_002.png)

To debug an ABAP program, you set a breakpoint then run the program normally. When the program reaches the breakpoint, the system interrupts it and opens the ABAP Debug perspective in ADT. You can then execute each subsequent statement individually to see what effect it has on the program. You can also inspect the contents of all of the variables in the program to see if any of the values are unexpected.

To set or remove a breakpoint, right-click the left margin of the editor and choose Toggle Breakpoint. As an alternative you can double-click the left margin. Note that the program has to be activated before you can set breakpoints.

Breakpoints are user-specific and are persistent - they remain active even after you have logged off from ADT and back on again. To prevent the debugger from starting at a breakpoint you must either delete the breakpoint (using the Toggle Breakpoint function) or deactivate it using the corresponding function in the context menu.

> **NOTE:** Depending on your personalization settings, ADT will ask for confirmation, first, before automatically opening the debug perspective.

### The Debug Perspective in ADT

When you debug an ABAP program using ABAP Development Tools, you use the Debug perspective. This is a customized version of the standard Eclipse Debug perspective, and it contains views and functions that are particularly important for debugging.

Let's look at some important elements of the debugger perspective.

> **Tutorial:** [How To Start The Debugger](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_42BD2588C06F3CA6:demo)

### Control of Code Execution

#### Some Navigation Functions

Once you started debugging, use the navigation functions to control the execution of the code.

Some important navigation functions are as follows:

- **Step Into (`F5`):** Choose _Step Into_ or press **F5** to execute a single step. Use this function for a step-by-step analysis. If, for example, you want to see which code block of a control structure is actually executed.

- **Resume (`F8`):** Choose _Resume_ or press **F8** to execute the program up to the next breakpoint. If the debugger does not hit any more breakpoints, the program is executed to the end and the debugging session terminates.

- **Run to Line (`Shift+F8`):** Choose _Run to Line_ or press **Shift+F8** to execute the program up to the current cursor position. Clicking on a code line and choosing this function is a convenient alternative to setting a breakpoint, choosing Resume and removing the breakpoint again.

- **Jump to Line (`Shift+F12`):** Choose _Jump to Line_ or press **Shift+F12** to skip some lines of code or to jump backwards to some already executed code. This function can be helpful to simulate what would happen if a certain piece of code was removed or to repeat debugging a bit of code you missed analysis the first time. Keep in mind that this is actually jumping, not executing coding. When you jump backwards, changes to data objects are not reverted!

- **Terminate:** Choose _Terminate_ if you are done with debugging and you do not want to execute the remaining program. The debug session terminates immediately.

### Special Breakpoints

![](https://learning.sap.com/service/media/topic/e5e7acf8-ee1d-4414-a64f-6e3bf7c39713/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Control_Execution_002.png)

You learned that you can create and manage breakpoints by clicking on the left margin of the ABAP editor view. This also works with the ABAP editor view in the Debug perspective.

In addition, you can switch to the Breakpoints view and manage your breakpoints there.

In the Breakpoints view you can also create special breakpoints:

- **Statement Breakpoint:**
  A statement breakpoint is not attached to a specific line of code but to a specific ABAP statement. A statement breakpoint on statement CLEAR, for example, causes the program to stop in the debugger whenever a CLEAR statement is executed - no matter where this statement is located.

  To create a statement breakpoint, open the dropdown list from the toolbar of the Breakpoints view and choose _Add Statement Breakpoint …_

- **Exception Breakpoint:**
  An exception breakpoint is attached to a specific exception. It causes the program to stop in the debugger whenever this particular exception is raised - no matter if this exception is handled by the program or causes a runtime error. To create an exception breakpoint, open the dropdown list from the toolbar of the Breakpoints view and choose Add Exception Breakpoint … .

- **Conditional Breakpoint:**
  You turn a breakpoint into a conditional breakpoint by adding a condition. If program execution hits a conditional breakpoint, the program only stops in the debugger, if the condition is fulfilled. If, for example, a breakpoint is located between DO and ENDDO it will cause the program to stop in the debugger in every iteration. But if you add a condition sy-index > 20 the debugger will ignore this breakpoint during the first 20 iterations and only stop in the following iterations.

  To add a condition to a breakpoint, choose it in the list of breakpoints and enter the condition in field Condition. Press Enter to save the breakpoint with the condition.

### Watchpoints

If an unexpected value of a variable is causing you problems, you can track its value during the course of the program using a watchpoint.

> **Tutorial:** [How To Control The Execution Of Code](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_645B252C3E912386:demo)

## Analysis of Data Objects

### Display Content of Data Objects

Watch this video to learn how to display the content of data objects in the debugger.

### Display Content of Internal Tables

Watch this video to learn how to display the content of internal tables.

### Changing the Values of Variable

Depending on your authorizations, you can change the value of variables during debugging.

For simple variables, locate the variable in the Variables view, right-click on it and choose Change Value ….

To change the content of an internal table, we have to distinguish between changing the value of an existing row and adding or deleting of a rows.

As shown in the figure, the following functions are available when you right-click in the ABAP internal table view:

Change Value …

Choose Change Value …. to change the content of an existing row.

Insert Row …

Choose Insert Row …. to add a new row. You can decide whether you want to append the new row or insert it at the chose position.

Delete Selected Rows …

Choose Delete Selected Rows … to remove the rows you selected before you right-clicked. To select a row, left-click it. To select more than one row, hold down the Ctrl key or the Shift key when you left-click additional rows.

Delete Rows …

Choose Delete Rows …to remove a larger range of rows, or even all rows. You are asked for the number of the start row and the end row you want to delete.

## How To Analyze The Contents Of Data Objects

## Debug ABAP Code

In this exercise, you analyze ABAP code using the ABAP debugger.

#### Template:

-   /LRN/CL\_S4D400\_BTT\_DEBUG (global Class)

#### Solution:

-   None, the class remains unchanged.

### Task 1: Preparation

Copy the class that you want to debug.

#### Steps

1.  Open the source code of global class /LRN/CL\_S4D400\_BTT\_DEBUG in the ABAP editor.
    
    1.  In the _Eclipse_ toolbar, choose _Open ABAP Development Object_. Alternatively, press **Ctrl + Shift + A**.
        
    2.  Enter **/LRN/CL\_S4D400\_BTT** as search string.
        
    3.  From the list of development objects choose _/LRN/CL\_S4D400\_BTT\_DEBUG_, then choose _Ok_.
        
2.  Link the _Project Explorer_ view with the editor.
    
    1.  In the _Project Explorer_ toolbar, find the _Link with Editor_ button. If it is not yet pressed, choose it. As a result, the development object, that is open in the editor, should be highlighted in the _Project Explorer_.
        
3.  Copy class /LRN/CL\_S4D400\_BTT\_DEBUG to a class in your own package (suggested name: ZCL\_##\_DEBUG, where ## stands for your group number).
    
    1.  In the _Project Explorer_ view, right-click class /LRN/CL\_S4D400\_BTT\_DEBUG to open the content menu.
        
    2.  From the context menu, choose _Duplicate ..._.
        
    3.  Enter the name of your package (**ZS4D400\_##**, where ## stands for your group number) and the name for the new class (**ZCL\_##\_DEBUG**), then choose _Next_.
        
    4.  Confirm the transport request and choose _Finish_.
        
4.  Activate and test the class.
    
    1.  Press **Ctrl + F3** to activate the class.
        
    2.  Press **F9** to run the class.
        
5.  Check the output in the _Console_ view.
    
    1.  Check the _Console_ view that should have opened as a new tab below the editor view.
        
    2.  If the _Console_ view is not visible, open it by choosing _Window_→_Show view_→_Other_. Double-click _Console_ in the hit list.
        

### Task 2: Analyze the Starting Values

Enter the debugger at the first executable statement in method **if\_oo\_adt\_classrun~main( )** and analyze the values of some variable and constant data objects.

#### Steps

1.  Set a breakpoint at the first statement that does not define a type or declare a data object.
    
    1.  Double-click the left-hand margin of the editor next to the line  loan\_remaining = loan\_total.to set a break point.
        
2.  Run the class as a console app and enter the debugger.
    
    1.  Press **F9** to run the class.
        
    2.  If you are asked whether you want to switch to the Debug perspective, mark _Remember my decision_ and choose _OK_.
        
3.  Display the value of data object **loan\_remaining** and **loan\_total** in the _Variables_ view.
    
    1.  In the current code line (the one with a green background) double-click loan\_remaining.
        
    2.  In the same code line, double-click **loan\_total**.
        

### Task 3: Control Program Execution

Set break points and watch points. Execute single steps or resume execution until the next break point or watch point is reached. Supervise the value changes of the data objects.

#### Steps

1.  Execute a single step to debug the value assignment in the current line.
    
    1.  In the toolbar, choose _Step Into (F5)_ or press **F5**.
        
    2.  Check that the value of **loan\_remaining** changed from **0..00** to **5000.00**.
        
2.  Display the content of data object **spec\_repay\_mode**. Then execute another single step to see which WHEN branch of the CASE - control structure is executed.
    
    1.  In the next code line, double-click **spec\_repay\_mode**.
        
    2.  Press **F5** to see that the program jumps to code line WHEN 'Q'..
        
3.  Set a watch point for variable **loan\_remaining** and resume program execution. Where does the program execution stop again?
    
    1.  In the _Variables_ view, right-click on _LOAN\_REMAINING_ and choose _Set Watchpoint_.
        
    2.  In the toolbar, choose _Resume (F8)_ or press **F8**.
        
    3.  Program execution stops immediately after code line loan\_remaining = loan\_remaining - repayment\_month., also to be precise, in the next code line with executable code.
        
4.  Display the content of data object **repayment\_plan**. Then execute another single step to see how it is filled with the APPEND statement.
    
    1.  Double-click on **repayment\_plan** at the end of the APPEND statement.
        
    2.  Press **F5** to see how **repayment\_plan** is filled with a first row.
        
5.  Inspect the string template in the APPEND statement and relate it to the resulting first row in internal table **repayment\_plan**. Display the content of the data objects that appear in the embedded expressions.
    
    1.  Double-click the data objects that appear between the curly brackets to display their contents.
        
6.  Set another watch point for variable **repayment\_plan** and resume program execution for a few times. Whenever execution reaches one of the watch points, analyze the value of **loan\_remaining** and new rows are added to **repayment\_plan**.
    
    1.  In the _Variables_ view, right-click on _REPAYMENT\_PLAN_ and choose _Set Watchpoint_.
        
    2.  Press **F8** to resume program execution.
        
    3.  Analyze the data objects in the _Variables_ view and the _ABAP Internal Table (Debugger)_ view.
        
7.  After a while, set a statement break point for all EXIT statements and delete the two watch points.
    
    1.  Navigate to the _Breakpoints_ view.
        
    2.  In the toolbar of the _Breakpoints_ view, expand the dropdown button on the very left and choose _Add Statement Breakpoint ..._.
        
    3.  On the dialog window that appears, enter **EXIT** as search string, click on the value _EXIT_ in the hitlist, and choose _OK_.
        
    4.  In the _Breakpoints_ view, right-click the watch point _LOAN\_REMAINING_ and choose _Remove_.
        
    5.  In the _Breakpoints_ view, right-click the watch point _REPAYMENT\_PLAN_ and choose _Remove_.
        
8.  Resume program execution until you reach the first EXIT statement. Deactivate the statement breakpoint for the **EXIT** statement. Then execute single steps until you reach the output part of the program.
    
    1.  Press **F8** to resume program execution.
        
    2.  Switch to the _Breakpoints_ view and deselect the line that says **EXIT \[Statement\]**.
        
    3.  Press **F5** until you reach the first code line that starts with out->write(.
        
9.  Open the _Console_ view. Execute the remaining program and pursue the output on the _console_ view.
    
    Do not press **F5** to debug the output. Use **F6** instead.
    
    1.  Press **F6** several times until you reach the end of the application.
        
    2.  Analyze the addition output on the _Console_ view and compare it to the string template in the previous code line.
        
10.  When the application is terminated, do not forget to switch back to the _ABAP perspective_.
    
    1.  Choose _ABAP_ on the very right of the Eclipse toolbar.