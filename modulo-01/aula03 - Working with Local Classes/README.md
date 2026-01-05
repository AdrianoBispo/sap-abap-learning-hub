# Working with Local Classes

## Local And Global Classes

Classes in ABAP can be either local or global.

![](https://learning.sap.com/service/media/topic/d1457294-3944-4877-9568-11f92debadfb/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Define_a_local_class_001.png)

Global classes are stored centrally and are contained in their own repository object called a class pool. A global class can serve as main program. Global classes can also contain logic to be reused by other ABAP programs, including other global classes.

Local classes are defined as part of an ABAP program, for example a global class. You can use them only in the program or class in which they are defined. Local classes are useful for entities or functions that you only need in a single program.

The ABAP syntax of both local and global classes is almost identical. In this course, you will be working with local classes in your global class. The global class with method if\_oo\_adt\_classrun~main will only serve as a kind of main program.

### Local Classes in Global Classes

As shown in the figure below, when you open a global class in ADT, the focus is on tab Global Class, first. Here you find the source code of the global class itself. To see or enter the source code of local classes, you have to navigate to tab Local Types.

![](https://learning.sap.com/service/media/topic/d1457294-3944-4877-9568-11f92debadfb/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Define_a_local_class_002.png)

ADT provides a source code template for local class. To use this template proceed as follows:

1. In the ABAP editor, type **lcl** and press **Ctrl + Space**.
2. From the list that displays choose _lcl - Local class_ and press **Enter**.
3. Adjust the name of the new local class.

For the classes in this course you have to remove the create private addition from the ``CLASS … DEFINITION`` statement.

> **Tutorial:** [How To Create A Local Class In A Global Class](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_101F7AF901D991BF:demo)

Demo[Start Demo](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_101F7AF901D991BF:demo)

## Source Code of a Class in ABAP

![](https://learning.sap.com/service/media/topic/cdcb9cdd-c994-4a3e-bfb2-e84bec2f1d38/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Define_a_local_class_003.png)

In ABAP. the source code of a class has two parts - the definition and the implementation. The definition part of a class is subdivided into up to three sections, called the visibility sections of the class.

- **Definition:** The definition part of a class contains the definition and declaration of all of the elements in the class, that is, the types, the constants, the attributes, and the methods. It begins with ``CLASS <class_name> DEFINITION``. and ends with ``ENDCLASS``.

- **Implementation:** The implementation part of a class contains the executable code of the class, namely the implementation of its methods. It begins with ``CLASS <class\_name> IMPLEMENTATION``. and ends with ``ENDCLASS``. The implementation part of a class is optional. It becomes mandatory as soon as the class definition contains executable methods.

- **Visibility sections:** Each visibility section of a class starts with one of the statements ``PUBLIC SECTION``, ``PROTECTED SECTION``, ``PRIVATE SECTION`` and ends implicitly when the next section begins. The last section ends with statement ``ENDCLASS``. All declarations of a class have to be inside one of the sections. In other words: No declarations are allowed between the beginning of the class definition and the beginning of the first section.

  The section in which a declaration is located defines the visibility of the declared element of the class.

  The three visibility sections of a class are optional; if you do not need a particular section, you do not have to declare it. But if a class definition consists of more than one section, they must follow the order ``PUBLIC SECTION`` - ``PROTECTED SECTION`` - ``PRIVATE SECTION``.

### Example: Attributes of the Flight Connection Class

![](https://learning.sap.com/service/media/topic/cdcb9cdd-c994-4a3e-bfb2-e84bec2f1d38/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Define_a_local_class_006.png)

Let us start implementing our _UML_ model in _ABAP_, and take the attributes of the connection class to begin with. To declare an attribute, use the ``DATA`` statement within the appropriate visibility section.

The attribute conn_counter is underlined, which denotes a static attribute. You declare static attributes using the _CLASS-DATA_ statement. The syntax of _CLASS-DATA_ is identical to that of _DATA_.

> **NOTE:** We begin with public attributes at this point. Later in the course, when we discuss encapsulation, we will turn them into private attributes.

## Exercise - Define a Local Class

In this exercise, you define a local class inside of a global class.

- **Template:** none

- **Solution:** ``/LRN/CL_S4D400_CLS_LOCAL_CLASS`` (global Class)

### Task 1: Create a Global Class

In your own package, create a new ABAP class.

#### Steps

1. Create a new global class that implements interface ``IF_OO_ADT_CLASSRUN`` (suggested name: **ZCL_##_LOCAL_CLASS**, where ## stands for your group number).

    a. Choose _File_→_New_→_ABAP Class_.

    b. Enter the name of your package in the _Package_ field. In the _Name_ field, enter the name **ZCL_##_LOCAL_CLASS**, where **##** is your group number. Enter a description.

    c. In the _Interfaces_ group box, choose _Add_.

    d. Enter **IF_OO_ADT_CLASSRUN**. When the interface appears in the hit list, double-click it to add it to the class definition.

    e. Choose _Next_.

    f. Select your transport request and choose _Finish_.

### Task 2: Define a Local Class

Define a local class **lcl_connection** and declare some public attributes.

#### Steps

1. Inside the global class, create a new local class **lcl_connection**. Use code completion to generate the code, but make sure you remove the create private addition from the generated code.

    a. Switch to the _Local Types_ tab.

    b. Type **lcl** into the editor and press **Ctrl + Space**.

    c. Double-click _lcl - class_ in the pop-up.

    d. While there is still a frame visible around **lcl** in the line class lcl definition create private., complete the name of the class to **lcl_connection**.

    e. Delete the words create private at the end of the code row class ``lcl_connection`` definition..

2. In local class **lcl_connection**, declare the following **public** attributes:

    | Attribute Name | Scope | Data Type |
    | --- | --- | --- |
     **carrier_id** | instance | ``/DMO/CARRIER_ID`` |
     **connection_id** | instance | ``/DMO/CONNECTION_ID`` |
     **conn_counter** | static | ``I`` |

    a. After line ``PUBLIC SECTION``. and before line ``PROTECTED SECTION``., add the following code:

      ``` ABAP
      DATA carrier_id    TYPE /dmo/carrier_id.
      DATA connection_id TYPE /dmo/connection_id.

      CLASS-DATA conn_counter TYPE i.
      ```

3. Activate the class.

    > **Note:** Because the **if_oo_adt_classrun~main** method does not contain executable code, yet, there is nothing to test or debug at this point.

    a. Press **Ctrl + F3** to activate the class.

    b. Compare your code on tab _Local Types_ to the following extract from the model solution:
          
      ``` ABAP

      *"* use this source file for the definition and implementation of
      *"* local helper classes, interface definitions and type
      *"* declarations

      CLASS lcl_connection DEFINITION.

        PUBLIC SECTION.

          DATA carrier_id    TYPE /dmo/carrier_id.
          DATA connection_id TYPE /dmo/connection_id.

          CLASS-DATA conn_counter TYPE i.

        PROTECTED SECTION.
        PRIVATE SECTION.

      ENDCLASS.

      CLASS lcl_connection IMPLEMENTATION.

      ENDCLASS.
      ```

## Creating Instances of a Class

### Instance Creation

![](https://learning.sap.com/service/media/topic/f6724740-2e10-40c6-a509-314532437592/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Create_instances_001.png)

You work with attributes like normal variables of the same type. Outside the class, however, the attribute name is not sufficient to identify the attribute unambiguously. To address a static attribute outside the class, first type the class name, then the static component selector (=>), and only then the attribute name. The static component selector is a double arrow made up of an equals sign and the greater than sign.

> **Hint:** No blanks are allowed before or after the component selector.

![](https://learning.sap.com/service/media/topic/f6724740-2e10-40c6-a509-314532437592/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Create_instances_002.png)

For instance attributes the situation is even more complicated: In order to access an instance component, you need a reference variable.

A reference variable is a special kind of variable that you use to create, address, and manage an object. A reference variable is used to point at the instance of a class in the program memory. You declare reference variables using the DATA statement with the addition TYPE REF TO followed by the name of a class.

The initial value of a reference variable is called the NULL reference; the reference does not yet point anywhere.

![](https://learning.sap.com/service/media/topic/f6724740-2e10-40c6-a509-314532437592/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Create_instances_003.png)

To create a new instance of a class, you use the NEW operator. The example above, uses a NEW #( ) expression on the right hand side of a value assignment. The result of the expression is the memory address of the newly created instance. This reference is then stored in the reference variable on the left-hand side of the assignment.

You may have noticed that the name of the class that you want to instantiate does not appear anywhere in the expression. However, from the location of the NEW #( ) expression, the system already knows that the target variable connection has the type REF TO lcl\_connection, and consequently it knows that it should create an instance of the class lcl\_connection. The pound sign after the NEW operator means "use the type of the variable before the equals sign". (In more advanced scenarios, you can actually specify the name of the class in place of the pound sign).

> **Hint:** There must be at least one blank between the brackets.

When you address a class for the first time (which could be accessing a static component or creating an instance of the class), the runtime system also loads the class definition into the program memory. This class definition contains all of the static attributes, which only exist once in the class instead of once for each instance.

![](https://learning.sap.com/service/media/topic/f6724740-2e10-40c6-a509-314532437592/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Create_instances_004.png)

You address static components using the class name and the static component selector. This does not work for instance components because you have to specify the instance you want to access.

To address an instance attribute outside the class, first type the reference variable, then the instance component selector (->), and only then the attribute name. The instance component selector is an arrow made up of a dash and the greater than sign.

> **NOTE:** Unlike many other programming languages, ABAP uses different characters for instance component selector and static component selector.

![](https://learning.sap.com/service/media/topic/f6724740-2e10-40c6-a509-314532437592/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Create_instances_005.png)

One of the main characteristics of object oriented programming is the fact that you can create multiple instances of the same class. Each instance is created in a different place in the program memory and the values of instance attributes in one instance are independent from the values in other instances. But as the graphic illustrates, instances of the same class share the value for the static attributes.

![](https://learning.sap.com/service/media/topic/f6724740-2e10-40c6-a509-314532437592/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Create_instances_006.png)

If you assign one reference variable to another, the system copies the address of the object to which the first variable is pointing into the second reference variable. The result of this is that you have two reference variables that point to the same object.

![](https://learning.sap.com/service/media/topic/f6724740-2e10-40c6-a509-314532437592/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Create_instances_007.png)

You can use the same reference variable to create more than one instance of a class. Each time you use the NEW #( ) expression, the system creates a new instance of the class and places the address of the new instance into the reference variable. However, the address of the new instance overwrites the address of the previous instance.

In the example above, the address of lcl\_connection (2) overwrites the address of lcl\_connection (1). Consequently, there is no longer a reference variable in the program pointing to lcl\_connection (1). When this happens to an instance, it can no longer be addressed from the program.

To prevent the program memory from becoming filled with objects that can no longer be addressed and eventually overflowing, the runtime system has a component called the garbage collector. The garbage collector is a program that runs periodically to look for and destroy objects to which no more references point. If during a program you delete the last reference to an object by overwriting it or using the CLEAR statement, the garbage collector will destroy the object on its next pass.

> **NOTE:** At the end of a program, when all of the reference variables are freed, the garbage collector will destroy all of the instances to which they had pointed. You do not have to worry about resource management in the program yourself.

> **Tutorial:** [How to Create Instances of an ABAP Class](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_EA37F7B8BEAF78A0:demo)

## Instance Management in an Internal Table

![](https://learning.sap.com/service/media/topic/dc6d312f-733b-41d8-93b8-f7b5ab614475/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Create_instances_008.png)

One way in which you can keep objects alive is to place the references into an internal table. This is a technique that you may well want to use if you are creating a whole series of objects. It enables you to use a single reference variable to create lots of objects. Although the reference variable is overwritten with the address of the next object, the existing objects are safe because the internal table contains a reference to them. You therefore never delete the "last" reference to the objects.

> **Tutorial:** [How To Manage Instances in an Internal Table](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_68826944F2FA597:demo)

## Exercise - Create and Manage Instances

In this exercise, you create and manage instances of your local class.

- **Template:** ``/LRN/CL_S4D400_CLS_LOCAL_CLASS`` (global Class)

- **Solution:** ``/LRN/CL_S4D400_CLS_INSTANCES`` (global Class)

### Task 1: Copy Template (Optional)

Copy the template class. If you finished the previous exercise you can skip this task and continue editing your class **ZCL_##_LOCAL_CLASS**.

#### Steps

1. Open the source code of global class **/LRN/CL_S4D400_CLS_LOCAL_CLASS** in the ABAP editor.
    
    a. In the _Eclipse_ toolbar, choose _Open ABAP Development Object_. Alternatively, press **Ctrl + Shift + A**.
          
    b. Enter **/LRN/CL_S4D400_CLS** as search string.
          
    c. From the list of development objects choose ``/LRN/CL_S4D400_CLS_LOCAL_CLASS``, then choose _OK_.
    
2. Link the _Project Explorer_ view with the editor.
    
    a. In the _Project Explorer_ toolbar, find the _Link with Editor_ button. If it is not yet pressed, choose it. As a result, the development object, that is open in the editor, should be highlighted in the _Project Explorer_.
    
3. Copy class **/LRN/CL_S4D400_CLS_LOCAL_CLASS** to a class in your own package (suggested name: **ZCL_##_INSTANCES**, where ## stands for your group number).
    
    a. In the _Project Explorer_ view, right-click class **/LRN/CL_S4D400_CLS_LOCAL_CLASS** to open the content menu.
          
    b. From the context menu, choose _Duplicate ..._.
          
    c. Enter the name of your package in the _Package_ field. In the _Name_ field, enter the name **ZCL_##_INSTANCES**, where ## stands for your group number.
          
    d. Adjust the description and choose _Next_.
          
    e. Confirm the transport request and choose _Finish_.

### Task 2: Create an Instance

In the **main** method of your global class, declare a reference variable and create an instance of your local class.

#### Steps

4. In method **if\_oo\_adt\_classrun~main** of your global class, declare a reference variable (suggested name: **connection**) and type it with your local class **lcl\_connection**.
    
    a. Switch to the _Global Class_ tab.
          
    b. Adjust the code as follows:
          
      ``` ABAP
      METHOD if_oo_adt_classrun~main.

        DATA connection TYPE REF TO lcl_connection.

      ENDMETHOD.
      ```

5. Create an instance of the class and set the attributes **carrier_id** and **connection_id** (Suggested values: "LH" for attribute **carrier_id** and "0400" for attribute **connection_id**).
    
    a. Adjust the code as follows:
        
      ``` ABAP
      DATA connection TYPE REF TO lcl_connection.

      connection = new #( ). 

      connection->carrier_id    = 'LH'.
      connection->connection_id = '0400'.
      ```
          
      **Hint:** After typing the component selector (->), press **Strg + Space** to choose the attribute names from a suggestion list.

6. Activate the class and use the debugger to analyze step by step what happens.
    
    1. Press **Ctrl + F3** to activate the class.
          
    2. Double-click the left-hand margin of the editor next to the line connection = new #( ).to set a break point.
          
    3. Press **F9** to run the class.
          
    4. Double-click the word **connection** to display the content of reference variable **connection**.
          
    5. Press **F5** to go one step further in the debugger. Check that the value of **connection** has changed.
          
    6. In the _Variables_view, expand the branch **connection** to display the initial attributes of the class.
          
    7. Press **F5** to go one step further in the debugger. Check that the value of **carrier_id** has changed.
          
    8. Press _F5_ again. Check that the value of **Connection_ID** has changed.

### Task 3: Manage Several Instances

Create more instances of your local class and store the references to these instances in an internal table.

#### Steps

1. In method **if_oo_adt_classrun~main( )** of your global class, declare an internal table with the line type ``TYPE REF TO lcl_connection`` (suggested name: **connections**).
    
    a.  Adjust the code as follows:
          
      ``` ABAP
      DATA connection TYPE REF TO lcl_connection.
      DATA connections TYPE TABLE OF REF TO lcl_connection.

      connection = new #( ). 

      connection->carrier_id    = 'LH'.
      connection->connection_id = '0400'.
      ```

2. After instantiating the class and setting the attributes, append the object reference to the internal table.

    a.  Adjust the code as follows:
      ``` ABAP
      connection = NEW #( ).
          
      connection->carrier_id = 'LH'.
      connection->connection_id = '0400'.
          
      APPEND connection TO connections.
      ```

3. Create two more instances of class **lcl_connection**, set their instance attributes to different values than in the first instance and append the references to the new instances to internal table **connections**. Use the same reference variable **connection** for all three instances.

    a. Adjust the code as follows:
          
      ``` ABAP
      connection = NEW #(  ).

      connection->carrier_id    = 'LH'.
      connection->connection_id = '0400'.

      APPEND connection TO connections.

      connection = NEW #(  ).

      connection->carrier_id    = 'AA'.
      connection->connection_id = '0017'.

      APPEND connection TO connections.

      connection = NEW #(  ).

      connection->carrier_id    = 'SQ'.
      connection->connection_id = '0001'.

      APPEND connection TO connections.
      ```

4. Activate the class and use the debugger to analyze step by step what happens.
    
    a. Press **Ctrl + F3** to activate the class.
          
    b. Double-click the left-hand margin of the editor next to the first line connection = new #( ).to remove the break point.
          
    c. Double-click the left-hand margin of the editor next to the first line APPEND connection TO connections.to set a new break point.
          
    d. Press **F9** to run the class.
          
    e. Double-click the word **connection** to display the content of reference variable **connection**.
          
    f. Double-click the word **connections** to display the content of internal table **connections**.
          
    g. Press **F5** to go one step further in the debugger. Check that the content of **connections** has changed.
          
    h. In the _Variables_ view, expand the branch **connections** to display content of the internal table.
          
    i. Press **F5** several times until you reach the end of the program. While you do so, check the content of the internal table.
          
    j. Compare your code on tab _Global Class_ to the following extract from the model solution:
          
      ``` ABAP
      METHOD if_oo_adt_classrun~main.

          DATA connection TYPE REF TO lcl_connection.
          DATA connections TYPE TABLE OF REF TO lcl_connection.
          
      * First Instance
      *********************************************************************
          
          connection = NEW #(  ).
          
          connection->carrier_id    = 'LH'.
          connection->connection_id = '0400'.
          
          APPEND connection TO connections.
          
      * Second Instance
      *********************************************************************
          connection = NEW #(  ).
          
          connection->carrier_id    = 'AA'.
          connection->connection_id = '0017'.
          
          APPEND connection TO connections.
          
      * Third Instance
      *********************************************************************
          connection = NEW #(  ).
          
          connection->carrier_id    = 'SQ'.
          connection->connection_id = '0001'.
          
          APPEND connection TO connections.
          
      ENDMETHOD.
      ```

## Defining and Calling Methods

### Method Definition

![](https://learning.sap.com/service/media/topic/cc68f1eb-472c-42f2-b187-b4fda4f9e198/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Define_and_Call_Methods_001.png)

In the definition part of a class you use METHODS to define an instance method and CLASS-METHODS to define a static method. The name of the method is followed by the method's signature; that is, the set of values that the method exchanges with its caller and the exceptions that may arise during the method.

The signature of a method consists of parameters and exceptions. Each parameter has a name and a type.

ABAP knows the following kinds of parameters:

- **Importing Parameters:** Importing parameters are values that the method receives from the caller. A method can have any number of importing parameters.

  By default, importing parameters are mandatory, but there are two ways to make them optional:

  - Using the ``OPTIONAL`` addition. The parameter is optional and its default value is the initial value appropriate to the type of the parameter
  - Using the ``DEFAULT`` <val> addition. The parameter is optional and its default value is the value that you specified as <val>.

  Inside of a method, you may not change importing parameters. If you attempt to do so, you will cause a syntax error.

- **Exporting Parameters:** Exporting parameters are results that are returned by the method. A method can have any number of exporting parameters. All exporting parameters are optional - a calling program only uses the values that it actually needs.

- **Changing Parameters:** Changing parameters are values that the method receives from the caller. Unlike importing parameters, the method can change the values of these parameters. They are then returned to the caller under the same name. A method can have any number of changing parameters. Changing parameters are mandatory by default; you can make them optional in the same way as importing parameters.

- **Returning Parameters:** A returning parameter is a method result that can be used directly in an expression. A method can only have one returning parameter. Returning parameters have to use a special form of parameter passing which is called pass-by-value. This form of parameter passing is defined by surrounding the parameter name in brackets (no blanks!) and preceding it with keyword ``VALUE``.

- Keyword ``RAISING`` is used to list the exceptions the method might raise to indicate an error situation. The calling program can then react to the error.

![](https://learning.sap.com/service/media/topic/cc68f1eb-472c-42f2-b187-b4fda4f9e198/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Define_and_Call_Methods_002.png)

As an example, add two methods to our class lcl_connection.

A set_attributes( ) method to set the values for attribute i_carrier_id and i_connection_id and a get_attributes( ) method to return the values of these attributes. Where set_attributes( ) needs two importing parameters, one for each of attribute, the get_attributes( ) method needs two exporting parameters.

For every method that you define, you must also create an implementation in the implementation part of the class. Until you do so, you see syntax errors in your class; the syntax check tells you that the implementation of the method is missing.

> **Hint:** ADT offers a quickfix to add the missing implementation. To use this quick fix, proceed as follows:
>   1. Position the cursor in a METHODS statement with an error and press **Ctrl + 1**.
> 
>   2. From the list of possible quick fixes choose _Add implementation for_ … and press **Enter** . If the implementation is missing for several methods, the quickfix title reads _Add … unimplemented methods_.

> **Tutorial:** [How to Define Methods](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_D41C6BBE46D32482:demo)

### Method Implementation

![](https://learning.sap.com/service/media/topic/cb10d0f8-6c3c-4c0c-ab86-a677798128e4/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Define_and_Call_Methods_004.png)

You must implement every method that you define. You do this in the implementation part of the class, by adding a pair of statements METHOD <method\_name> and ENDMETHOD.

The method implementation contains ABAP statements that can access the parameters of the method (you are not allowed to change importing parameters) and all types, attributes, and constants that you declared in the class, independent from their visibility. Instance methods may access both instance attributes and static attributes. Static methods may only access static components.

Inside the implementation part of a class, you can access the attributes of that class without a reference variable and '->' (or the class name and '=>' , in case of static attributes).

![](https://learning.sap.com/service/media/topic/cb10d0f8-6c3c-4c0c-ab86-a677798128e4/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Define_and_Call_Methods_005.png)

Only in the implementation of instance methods, ABAP offers built-in variable ME. ME is a reference variable, typed with the current class and filled at runtime with the address of the current instance. The use of ME is optional and should be avoided, unless the name of an attribute collides with the name of another data object, for example a parameter name.

In the example, the import parameters of method set\_attributes( ) have the same names as the corresponding attributes. In such a situation, the name by itself denotes the parameter. Only by putting me-> in front it becomes clear that an attribute of the current instance is to be accessed.

![](https://learning.sap.com/service/media/topic/cb10d0f8-6c3c-4c0c-ab86-a677798128e4/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Define_and_Call_Methods_006.png)

If an error occurs during the execution of a method, the method can trigger an exception using statement RAISE EXCEPTION TYPE, followed by the name of the exception.

> **NOTE:** Technically, exception names are the names of special ABAP classes and statement RAISE EXCEPTION TYPE creates an instance of the referenced class. The instance is referred to as exception object and the special kind of ABAP classes are called exception classes.

The moment an exception is raised, the method execution is terminated.

Control is returned to the calling program if all prerequisites are met. Otherwise the entire program terminates with a runtime error.

The prerequisites for continuing the program are as follows:

- The raised exception is declared in the ``RAISING`` clause of the method signature
- The method call is surrounded by a ``TRY … ENDTRY`` control structure
- The ``TRY … ENDTRY`` control structure contains a ``CATCH`` block for the exception

> **NOTE:** For some exceptions, the ABAP editor displays a syntax warning with a quick fix when you raise an exception that is not yet declared in the ``RAISING`` clause of the method.

> **Tutorial:** [How to Implement Methods](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_6BF8538B5A617BA1:demo)

### Method Calls

![](https://learning.sap.com/service/media/topic/bd0f582f-ee94-494d-a211-b566b5b903ec/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Define_and_Call_Methods_007.png)

You call an instance method using a reference variable and the instance component selector (->). The component selector is followed by the name of the method you want to call. For static methods you use the class name and the static component selector (=>). On both cases, the parameter passing takes place in a pair of brackets. The brackets are needed in any case. They remain empty if no parameters are to be passed.

> **NOTE:** No blank is allowed between the method name and the opening bracket. On the other hand you need at least one blank after the opening bracket and before the closing bracket.

Importing parameters are listed after keyword EXPORTING because the values that are imported by the called method are exported by the calling program. The parameter names are listed on the left-hand side of an equals sign (=) and an expression on the right-hand side.. This expression can be as simple as a literal, constant, or variable but any other expressions are also allowed as long as the type of the expression matches the type of the parameter.

Exporting parameters are listed after keyword IMPORTING. Note, that for exporting parameter the parameter name is also listed on the left-hand side of the equals sign. The variable on the right-hand side has to have the same type as the parameter.

Changing parameters are listed after keyword CHANGING. The type of the variable on the right-hand side has to match the type of the parameter on the left hand side.

![](https://learning.sap.com/service/media/topic/bd0f582f-ee94-494d-a211-b566b5b903ec/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Define_and_Call_Methods_008.png)

The example illustrates the call of instance methods set_attributes( ) and get_attributes( ). The parameter names are always on the left-hand side of the equals sign (=).

> **Hint:** You can use code completion in ADT to generate the method call including the parameter passing. To do so, proceed as follows:
> 
>   1. Type in the reference variable (or class name) and the component selector.
>   2. Once you typed the component selector press **Ctrl + Space** to display a list of components you can address.
>   3. If you choose a method, and press **Shift + Enter** to insert the name of the method and its full signature into your code. Optional parameters are listed in comment lines.

![](https://learning.sap.com/service/media/topic/bd0f582f-ee94-494d-a211-b566b5b903ec/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Define_and_Call_Methods_010.png)

When calling a method, keyword IMPORTING is always needed to receive the value of exporting parameters. But keyword EXPORTING becomes optional, if you only want to supply importing parameters.

For methods with only one mandatory importing parameter, you can even omit the explicit parameter assignment. In the example, parameter i_carrier_id is optional with default value 'LH'. Therefore the value between the brackets is assigned to the remaining mandatory importing parameter i_connection_id.

> **Tutorial:** [How to Call Methods](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_2DEC8B2FBF8684A2:demo)

### Exception Raising

![](https://learning.sap.com/service/media/topic/ffc7cf1b-a0df-423e-985d-95221f7e80a7/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Define_and_Call_Methods_011.png)

In a previous section of this class, you learned how to handle runtime errors with TRY … ENDTRY control structures.

Exceptions declared in the RAISING clause of a method definition are handled in exactly the same way:

- Place the method call in the TRY-block of the TRY … ENDTRY control structure.
- Define a CATCH block with the name of the exception you want to handle.

> **NOTE:** If there is code that you want to skip in case the method call failed, you can place this code inside the try block, after the method call.

### Try It Out: Method Calls and Exception Handling

1. Like in the first exercise of this course, create a new global class that implements interface ``IF_OO_ADT_CLASSRUN``.

2. Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:

  ``` ABAP
      CONSTANTS c_carrier_id TYPE /dmo/carrier_id       VALUE 'LH'.
      CONSTANTS c_connection_id TYPE /dmo/connection_id VALUE '0400'.

      DATA connection  TYPE REF TO lcl_connection.
      DATA connections  TYPE TABLE OF REF TO lcl_connection.

  * Create Instance
  *********************************************************************

      connection = NEW #(  ).

  * Call Method and Handle Exception
  *********************************************************************
      out->write(  |i_carrier_id    = '{ c_carrier_id }' | ).
      out->write(  |i_connection_id = '{ c_connection_id }'| ).

      TRY.
          connection->set_attributes(
            EXPORTING
              i_carrier_id    = c_carrier_id
              i_connection_id = c_connection_id
          ).

          APPEND connection TO connections.
          out->write( `Method call successful` ).
        CATCH cx_abap_invalid_value.
          out->write( `Method call failed`     ).
      ENDTRY.
    ```

3. Navigate to tab _Local Types_ and insert the following code snippet there:
  ``` ABAP    
  CLASS lcl_connection DEFINITION.
    PUBLIC SECTION.

  * Attributes
      DATA carrier_id    TYPE /dmo/carrier_id.
      DATA connection_id TYPE /dmo/connection_id.

  * Methods
      METHODS set_attributes
        IMPORTING
          i_carrier_id    TYPE /dmo/carrier_id  DEFAULT 'LH'
          i_Connection_id TYPE /dmo/connection_id
        RAISING
          cx_abap_invalid_value.

  ENDCLASS.
    
  CLASS lcl_connection IMPLEMENTATION.
    
    METHOD set_attributes.
    
      IF i_carrier_id IS INITIAL OR i_connection_id IS INITIAL.
        RAISE EXCEPTION TYPE cx_abap_invalid_value.
      ENDIF.
    
      carrier_id    = i_carrier_id.
      connection_id = i_connection_id.
    
    ENDMETHOD.
    
  ENDCLASS.
  ```
    
4.  Press ``CTRL + F3`` to activate the class and ``F9`` to execute it as a console app.
5.  Analyze the console output. Debug the program, play around with the source code to get familiar with the concepts. In particular, change the values of the two constants to debug the raising of the exception.

### Functional Methods

![](https://learning.sap.com/service/media/topic/cb5ae5fe-d734-4edb-9eb3-1908e96d7f1b/S4D400_24_en-US_media/S4D400_24_en-US_images/02_-_Define_and_Call_Methods_013.png)

Methods that have a returning parameter are called functional methods. It is not possible to define more than one returning parameter for the same method. It is mandatory that you define the returning parameter in the form ``VALUE``(<parameter_name>). Besides the returning parameter a functional method can have any combination of other parameters. It is most common however, to add importing parameters only. The reason is, that with additional exporting or changing parameters you loose the biggest advantage of functional methods, namely, that you can use the result of a functional method directly in other ABAP expressions.

The figure shows two examples of how you can use functional methods in expressions; the first is a simple example in which the returning parameter is assigned directly to a variable. The second example shows how the call of the functional method is used as input for another method. The system executes the functional method, and then uses the value of the returning parameter as input for method write( ).

### Try It Out: Functional Methods

1.  Like in the first exercise of this course, create a new global class that implements interface ``IF_OO_ADT_CLASSRUN``.
2.  Copy the following code snippet to the implementation part of method ``if_oo_adt_classrun~main( )``:

  ``` ABAP    
      DATA connection  TYPE REF TO lcl_connection.
      DATA connections  TYPE TABLE OF REF TO lcl_connection.
    
  * Create Instance
  *********************************************************************
    
      connection = NEW #(  ).
    
      connection->set_attributes(
        EXPORTING
          i_carrier_id    = 'LH'
          i_connection_id = '0400'
      ).
    
      APPEND connection TO connections.
    
  * Calling Functional Method
  *********************************************************************
      " in a value assignment (with inline declaration for result)
      DATA(result) = connection->get_output( ).
    
      " in logical expression
      IF connection->get_output(  ) IS NOT INITIAL.
    
        " as operand in a statement
        LOOP AT connection->get_output(  ) INTO DATA(line).
    
        ENDLOOP.
    
        "  to supply input parameter of another method
        out->write( data = connection->get_output( )
                      name = `  ` ).
    
      ENDIF.

  ```

3. Navigate to tab _Local Types_ and insert the following code snippet there:

  ``` ABAP
    
  CLASS lcl_connection DEFINITION.
    PUBLIC SECTION.
    
  * Attributes
      DATA carrier_id    TYPE /dmo/carrier_id.
      DATA connection_id TYPE /dmo/connection_id.
    
  * Methods
      METHODS set_attributes
        IMPORTING
          i_carrier_id    TYPE /dmo/carrier_id  DEFAULT 'LH'
          i_Connection_id TYPE /dmo/connection_id.
    
      " Functional Method
      METHODS get_output
        RETURNING VALUE(r_output) TYPE string_table.
    
  * PROTECTED SECTION.
    
  * PRIVATE SECTION.
    
  ENDCLASS.
    
  CLASS lcl_connection IMPLEMENTATION.
    
    METHOD set_attributes.
    
      carrier_id    = i_carrier_id.
      connection_id = i_connection_id.
    
    ENDMETHOD.
    
    METHOD get_output.
    
      APPEND |------------------------------| TO r_output.
      APPEND |Carrier:     { carrier_id    }| TO r_output.
      APPEND |Connection:  { connection_id }| TO r_output.
    
    ENDMETHOD.
    
  ENDCLASS.
  ```
    
4.  Press ``CTRL + F3`` to activate the class and ``F9`` to execute it as a console app.
5.  Analyze the console output. Debug the program, play around with the source code to get familiar with the concepts. In particular, debug the different calls of functional method _get_output( )._.

## Exercise - Define and Call Methods

In this exercise, you create and manage instances of your local class.

- **Template:** ``/LRN/CL_S4D400_CLS_INSTANCES`` (global Class)

- **Solution:** ``/LRN/CL_S4D400_CLS_METHODS`` (global Class)

### Task 1: Copy Template (Optional)

Copy the template class. If you finished the previous exercise, you can skip this task and continue editing your class **ZCL_##_LOCAL_CLASS** or your class **ZCL_##_INSTANCES**.

#### Steps

1. Open the source code of global class **/LRN/CL_S4D400_CLS_INSTANCES** in the ABAP editor.
    
    a. In the _Eclipse_ toolbar, choose _Open ABAP Development Object_. Alternatively, press **Ctrl + Shift + A**.
          
    b. Enter **/LRN/CL_S4D400_CLS** as search string.
          
    c. From the list of development objects choose _/LRN/CL\_S4D400\_CLS\_INSTANCES_, then choose _OK_.

2. Link the _Project Explorer_ view with the editor.
    
    a. In the _Project Explorer_ toolbar, find the _Link with Editor_ button. If it is not yet pressed, choose it. As a result, the development object, that is open in the editor, should be highlighted in the _Project Explorer_.
    
3. Copy class **/LRN/CL_S4D400_CLS_INSTANCES** to a class in your own package (suggested name: **ZCL_##_METHODS**, where ## stands for your group number).
    
    a. In the _Project Explorer_ view, right-click class **/LRN/CL_S4D400_CLS_INSTANCES** to open the content menu.
          
    b. From the context menu, choose _Duplicate ..._.
          
    c. Enter the name of your package in the _Package_ field. In the _Name_ field, enter the name **ZCL_##_METHODS**, where ## stands for your group number.
          
    d. Adjust the description and choose _Next_.
          
    e. Confirm the transport request and choose _Finish_.

### Task 2: Define Methods

In your local class **lcl_connection**, define instance methods **get_output** and **set_attributes**.

#### Steps

1. Navigate to the definition of the local class **lcl_connection**.
    
    a. In the global class, choose _Local Types_.
    
2. Add a method **get_output** to the public section of the class. It should have one returning parameter **r_output** of global table type **STRING_TABLE**.

    > **NOTE:** Remember to surround the name of the returning parameter with ``VALUE( )``.

    a. Adjust the code as follows:

      ``` ABAP
      PUBLIC SECTION.
          
        DATA carrier_id    TYPE /dmo/carrier_id.
        DATA connection_id TYPE /DMO/Connection_id.
          
        CLASS-DATA conn_counter TYPE i.
          
        METHODS get_output
          returning
              value(r_output) type string_table.
          
        PROTECTED SECTION.          
      ```
          
    > **NOTE:** For the moment, ignore the syntax error _Implementation missing for method "GET_OUTPUT"_.

3. Add a method **set_attributes** to the public section of the class. It should have one importing parameter for each instance attribute of the class. Use the same types for the parameters that you used to type the attributes. To distinguish the parameters from the attributes, add prefix **i_** to the parameter names. In addition, the method should raise exception **CX_ABAP_INVALID_VALUE**.

    a. Adjust the code as follows:
      ``` ABAP
      PUBLIC SECTION.
          
          DATA carrier_id    TYPE /dmo/carrier_id.
          DATA connection_id TYPE /DMO/Connection_id.
          
          CLASS-DATA conn_counter TYPE i.
          
          METHODS set_attributes
            IMPORTING
              i_carrier_id    TYPE /dmo/carrier_id
              i_connection_id TYPE /dmo/connection_id
            RAISING
              cx_abap_invalid_value.
          
          METHODS get_output
            returning
              value(r_output) type string_table.
          
      PROTECTED SECTION.
      ```
          
    > **NOTE:** For the moment, ignore the syntax error _Implementation missing for method "SET_ATTRIBUTES"_.

### Task 3: Implement Methods

#### Steps

1. Use a quick fix to add the method implementations to the class.
    
    a. Position the cursor on the name of one of the methods in the editor and press **Ctrl + 1**.
          
    b. Double-click the suggestion _Add 2 unimplemented methods_.
    
2. Implement the method **get_output**. Append some string templates to returning parameter **r_output**. In the string templates, use embedded expressions to add the values of attributes **carrier_id** and **connection_id**.

    a. In the local class add the following code between the statements ``METHOD get_output.`` and ``ENDMETHOD``.

      ``` ABAP
            APPEND |------------------------------| TO r_output.
            APPEND |Carrier:     { carrier_id    }| TO r_output.
            APPEND |Connection:  { connection_id }| TO r_output.
      ```
    
3. Implement the method **set_attributes**. If either of the two importing parameters is empty (IS INITIAL), raise exception **CX_ABAP_INVALID_VALUE**. Otherwise, fill the two instance attributes with the values of the importing parameters.

    a. Adjust the code as follows:          
      ``` ABAP          
      METHOD set_attributes.

        IF i_carrier_id IS INITIAL OR i_connection_id IS INITIAL.
          RAISE EXCEPTION TYPE cx_abap_invalid_value.
        ENDIF.
          
        carrier_id    = i_carrier_id. 
        connection_id = i_connection_id.
          
      ENDMETHOD. 
      ```

4. Activate your class.

    a. Choose **Ctrl + F3** to activate the class.

### Task 4: Call a Functional Method

In the **main** method of your global class, call the functional method **get_output**.

#### Steps

1. Switch to the implementation of method **if_oo_adt_classrun~main** in the global class.
    
    a. In the global class, choose _Global Class_ scroll down to the implementation of method **if_oo_adt_classrun~main**.

2. At the end of the method, add a loop over the internal table **connections** with reference variable **connection** as a work area.

    a. Add the following code before ``ENDMETHOD.``.
      ``` ABAP
      LOOP AT connections INTO connection.

      ENDLOOP.
      ```

3. In the loop, call functional method **get_output** for each instance in turn and write the result to the console.
    
    > **Hint:** You can use the call of method **get_output** directly as input for **out->write( ).**
    
    a. Adjust the code as follows:
      ``` ABAP
        LOOP AT connections INTO connection.
          
          out->write( connection->get_output( ) ).
          
        ENDLOOP.
      ```

4. Activate the class. Execute it as a console app and analyze the console output.
    
    a. Press **Ctrl + F3** to activate the class.
          
    b. Press **F9** to run the class.
          
    c. Analyze the output on the _Console_ window.

### Task 5: Use Code Completion and Handle Exceptions

Use code completion to call the method **set_attributes** and handle the exception the method raises.

#### Steps

1. For the first instance of class **lcl_connection**, replace the direct access to attributes **carrier_id** and **connection_id** with a call of the instance method **set_attributes( )**. Use code completion to insert the full signature of the method.
    
    > **HINT:** Press **Ctrl + Space** after you typed the component select (->) to display a list of available attributes and methods. Choose the method and then press **Shift + Enter** to insert the full signature of the method.
    
    a.  Place the cursor in the next line after the first connection = NEW #( )..
          
    b.  Type connection-> and press **Ctrl + Space**.
          
    c.  Choose ``set_attributes`` and press **Shift + Enter**.
          
    d.  Pass values to the importing parameters. Use the same literals that you used previously to set the attributes of this instance.
          
    e.  Remove or comment the direct access to the instance attributes for this instance.
    
2. Handle the exception. Surround the method call with ``TRY. and ENDTRY..`` Add a CATCH-Block to handle exception **CX_ABAP_INVALID_VALUE**. Make sure the new instance is only added to internal table **connections** if the method call was successful.
    
    a. Uncomment the generated code line ``CATCH cx_abap_invalid_value.`` .
          
    b. Add ``TRY.`` before the method call .
          
    c. Add ``ENDTRY.`` after the ``CATCH`` statement.
          
    d. Between the `CATCH` statement and the ENDTRY statement add ``out->write( `Method call failed` )``.
          
    e. Move the ``APPEND`` statement up to between the method call and ``CATCH`` statement.
          
    f. The complete method call should now look like this:
          
      ``` ABAP
          TRY.
              connection->set_attributes(
                EXPORTING
                    i_carrier_id    = 'LH'
                    i_connection_id = '0400'
              ).
          
      *       connection->carrier_id    = 'LH'.
      *       connection->connection_id = '0400'. 
          
              APPEND connection TO connections.
          
            CATCH cx_abap_invalid_value.
              out->write( `Method call failed` ).
          ENDTRY.  
      ```

3. Repeat the previous step for the other two instances of class **lcl_connection**.
    
    a. After this step, the implementation of method **if_oo_adt_classrun~main** should look similar to this:
          
      ``` ABAP
      METHOD if_oo_adt_classrun~main.
          
          DATA connection TYPE REF TO lcl_connection.
          DATA connections TYPE TABLE OF REF TO lcl_connection.
          
      * First Instance
      *********************************************************************
          connection = NEW #(  ).
          
          TRY.
              connection->set_attributes(
                EXPORTING
                  i_carrier_id    = 'LH'
                  i_connection_id = '0400'
              ).
          
      *        connection->carrier_id    = 'LH'.
      *        connection->connection_id = '0400'.
          
              APPEND connection TO connections.
          
            CATCH cx_abap_invalid_value.
              out->write( `Method call failed` ).
          ENDTRY.
          
      * Second instance
      *********************************************************************
          
          connection = NEW #(  ).
          
          TRY.
              connection->set_attributes(
                EXPORTING
                  i_carrier_id    = 'AA'
                  i_connection_id = '0017'
              ).
          
      *        connection->carrier_id    = 'AA'.
      *        connection->connection_id = '0017'.
          
              APPEND connection TO connections.
          
            CATCH cx_abap_invalid_value.
              out->write( `Method call failed` ).
          ENDTRY.
          
      * Third instance
      *********************************************************************
          connection = NEW #(  ).
          
          TRY.
              connection->set_attributes(
                EXPORTING
                  i_carrier_id    = 'SQ'
                  i_connection_id = '0001'
              ).
          
      *         connection->carrier_id    = 'SQ'.
      *         connection->connection_id = '0001'.
          
              APPEND connection TO connections.
          
            CATCH cx_abap_invalid_value.
              out->write( `Method call failed` ).
          ENDTRY.
          
          
      * Output
      *********************************************************************
          
          LOOP AT connections INTO connection.
          
            out->write( connection->get_output( ) ).
          
          ENDLOOP.
          
      ENDMETHOD.
      ```

4. Activate the class. Execute it and debug the method calls.
    
    a. Press **Ctrl + F3** to activate the class.
          
    b. Press **F9** to run the class.

## Using Encapsulation to Ensure Consistency

### Data Encapsulation

![](https://learning.sap.com/service/media/topic/c3b6ae51-c7d6-43c0-8f49-8fdd96e86af5/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Explain_encapsulation_001.png)

In object-oriented programming, an object corresponds to a real-life object such as an employee, a vehicle, or, in our case, a flight connection. It has attributes that describe it, in the case of a flight connection, those are the carrier id and the flight number.

Now let us consider what happens when a program that is using this object wants to provide values for these attributes. It is obvious that only those combinations of carrier id and flight number should be accepted that correspond to a flight connection in the real world.

In object orientation, the client program should not be able to change the attribute values directly. Instead, it should need to call a method to perform this task. The method, which comes with the object, can then check whether the combination of carrier id and flight number is valid and, if this is not the case - reject the change.

As developers of class lcl_connection, we can ensure the use of method set_attributes( ) by making attributes carrier_id and connection_id private, or at least read only.

This concept is called data encapsulation; The information about the flight connection is managed by the connection object itself and cannot be manipulated by anyone else. This ensures that no other point in the program can bypass the check of the consistency - either knowingly or unknowingly. This is one of the major advantages of object-orientation.

In object oriented programming, it is recommended to use data encapsulation as much as possible!

![](https://learning.sap.com/service/media/topic/c3b6ae51-c7d6-43c0-8f49-8fdd96e86af5/S4D400_24_en-US_media/S4D400_24_en-US_images/01_-_Explain_encapsulation_002.png)

With the public attributes we defined by now, it was possible to read and change the values of the attributes anywhere inside and outside the class.

In order to restrict access to the attributes you have two options:

1. Keep the DATA statement or CLASS-DATA statement in the public section and add addition READ-ONLY. In doing so, write access to the attribute is forbidden outside of the class, but read access is still possible.
    
    > **NOTE:** Addition READ-ONLY is only allowed in the public section of a class.
    
2. Move the DATA statement or CLASS-DATA statement from the public section to one of the other visibility sections, for example the private section. In doing so, read access and write access to the attribute is forbidden outside of the class.
    
    > **Hint:** ADT offers a quick fix to change the visibility of an attribute. To use it place the cursor in the attribute name, press **Ctrl + 1**, and choose Make <attribute> private.

> **Tutorial:** [How To Use Private Attributes](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_2EF94ED3881D7EAE:demo)

### Instance Constructor

![](https://learning.sap.com/service/media/topic/c08b4b1a-506c-4bba-81a3-081f242bec39/S4D400_24_en-US_media/S4D400_24_en-US_images/02-_Use_Constructors_001.png)

By making your attributes private - or read-only, at least - you can ensure that the client program uses the available set_attributes( ) method to set the values for attributes carrier_id and connection_id.

But there is still potential for inconsistencies:

- You cannot force the program to call set_attributes( ) for each new instance. As a result, there can be instances with initial values for carrier_id and connection_id.
- The client program can call set_attributes( ) several times for the same instance. This should also not be possible.

What you need is a technique to enforce non-initial values during instantiation and to prevent later changes.

To solve these problems, object-oriented programming languages use constructor methods.

![](https://learning.sap.com/service/media/topic/c08b4b1a-506c-4bba-81a3-081f242bec39/S4D400_24_en-US_media/S4D400_24_en-US_images/02-_Use_Constructors_002.png)

The runtime system calls the constructor automatically when you create a new instance of the class, but you may not call it explicitly. Thus the constructor is guaranteed to run once and once only for each instance that you create.

From a syntax perspective a constructor method has following properties:

- a public instance method with the reserved name constructor
- may have importing parameters, for example to obtain starting values for the attributes of the new instance
- may raise exceptions

> **NOTE:** In ABAP, it is not possible to define more than one constructor method in the same class.

> **Hint:** In ADT, you can use a quick fix to generate a constructor method. To use this quick fix proceed as follows:
>   1.  Either in the definition or implementation part, place the cursor in the class name and press ``Ctrl + 1``.
>   2. From the list of available quick fixes, choose Generate constructor.
>   3. On the dialog window that appears, select the attributes you want to initialize with the constructor and choose Finish
> Once you generated the constructor you can adjust its definition and implementation to your needs.

![](https://learning.sap.com/service/media/topic/c08b4b1a-506c-4bba-81a3-081f242bec39/S4D400_24_en-US_media/S4D400_24_en-US_images/02-_Use_Constructors_003.png)

The example shows the constructor of class lcl_connection. The generated definition contains an importing parameter for each of the attributes carrier_id and connection_id. The generated parameters have the same names as the related attributes.

The generated part of the implementation contains a value assignment for each of the attributes with the related importing parameter on the right. Self-reference me-> is needed to distinguish the attributes from the parameters of identical name.

![](https://learning.sap.com/service/media/topic/c08b4b1a-506c-4bba-81a3-081f242bec39/S4D400_24_en-US_media/S4D400_24_en-US_images/02-_Use_Constructors_005.png)

The example shows some manual additions to the generated constructor of class lcl_connection.

In order to reject the creation of instances with initial values, a consistency check was added to the implementation and a RAISING clause to the definition of the constructor.

Because the constructor is executed once and only once for each new instance of class lcl_connection, the constructor implementation is the perfect place to increment static attribute conn_counter.

![](https://learning.sap.com/service/media/topic/c08b4b1a-506c-4bba-81a3-081f242bec39/S4D400_24_en-US_media/S4D400_24_en-US_images/02-_Use_Constructors_006.png)

When you instantiate a class that has a constructor, the system calls the constructor method automatically. If the constructor has importing parameters, you pass them in the NEW expression exactly as you would pass parameters to a normal method.

> **NOTE:** A constructor can have only importing parameters. Keyword EXPORTING is neither needed nor allowed in the NEW expression.

If the constructor has exceptions, you must ensure that you catch them by enclosing the instantiation in a ``TRY… CATCH...ENDTRY`` block. If an constructor raises an exception, control returns immediately to the program containing the NEW expression. In this case, you do not receive a new instance of the class.

> **Tutorial:** [How To Define An Instance Constructor](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_AA201DE9A603868E:demo)

### Static Constructor

![](https://learning.sap.com/service/media/topic/a501c233-9223-4d46-a49f-20633f540550/S4D400_24_en-US_media/S4D400_24_en-US_images/02-_Use_Constructors_007.png)

While the instance constructor is called once when each instance of a class is created, you will sometimes need to perform actions once only for the entire class. For this purpose ABAP allows you to define a static constructor, also known as class constructor.

The runtime system calls the static constructor once only when the class is addressed for the first time during the execution of a program.

The first addressing of a class could be one of the following:

- First instantiation of the class
- First call of a static method
- First access to a public static attribute

> **NOTE:** This list is not complete. There are other actions (related to inheritance) that can cause a class to be addressed for the first time.

A typical use case for the static constructor is the dynamic initialization of static attributes with non-initial values. Therefore it is important that the runtime calls the static constructor before creating the instance, calling the static method, or addressing the static attribute.

From a syntax perspective a constructor method has following properties:

- A public static method with the reserved name class_constructor
- Without parameters or exceptions

> **NOTE:** A static constructor must not have a signature because it is impossible to tell exactly when a class will be addressed for the first time.

> **Hint:** In ADT, you can use a quick fix to generate a class constructor method. To use this quick fix proceed as follows:
> 
>   1.  Either in the definition or implementation part, place the cursor in the class name and press **Ctrl + 1**.
>   2.  From the list of available quick fixes, choose Generate class constructor

> **Tutorial:** [How To Debug The Execution Of Constructors](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_5189FB6B111ACAAE:demo)

## Exercise - Use Private Attributes and a Constructor

In this exercise, you make the the static attribute of your class read-only and the instance attributes private. You use a constructor to set their values.

- **Template:** ``/LRN/CL_S4D400_CLS_METHODS`` (global Class)

- **Solution:** ``/LRN/CL_S4D400_CLS_CONSTRUCTOR`` (global Class)

### Task 1: Copy Template (Optional)

Copy the template class. If you finished the previous exercise, you can skip this task and continue editing your class (``ZCL_##_METHODS``, **ZCL_##_LOCAL_CLASS** or **ZCL_##_INSTANCES**).

#### Steps

1. Copy class **/LRN/CL_S4D400_CLS_METHODS** to a class in your own package (suggested name: **ZCL_##_CONSTRUCTOR**, where ## stands for your group number).
    
    a. Open the source code of the global class **/LRN/CL_S4D400_CLS_METHODS**.
          
    b. Link the _Project Explorer_ view with the editor.
          
    c. In the _Project Explorer_ view, right-click the class **/LRN/CL_S4D400_CLS_METHODS** to open the context menu.
          
    d. From the context menu, choose _Duplicate ..._.
          
    e. Enter the name of your package in the _Package_ field. In the _Name_ field, enter the name **ZCL_##_CONSTRUCTOR**, where ## stands for your group number.
          
    f. Adjust the description and choose _Next_.
          
    g. Confirm the transport request and choose _Finish_.

### Task 2: Make Attributes Private

Change the visibility of the attributes **carrier_id** and **connection_id** to enforce the use of the methods **set_attributes( )** and **get_output( )**.

#### Steps

1. Set the visibility of the attribute **carrier_id** to private using a quick fix.
    
    a. Switch to the _Local Types_ tab.
          
    b. Go to the declaration of the attribute **carrier_id** in local class **lcl_connection**.
          
    c. Place the cursor on **carrier_id** and press **Ctrl + 1**.
          
    d. Double-click the suggestion *Make carrier_id private*.
          
    e. Check that the declaration of the attribute **carrier_id** has moved to the private section.
    
2. Set the visibility of the attribute **connection_id** to private using a quick fix.
    
    a.  Go to the declaration of the attribute **connection_id** in the local class **lcl_connection**.
          
    b.  Place the cursor on **connection_id** and press **Ctrl + 1**.
          
    c.  Double-click the suggestion *Make connection_id private*.
          
    d.  Check that the declaration of attribute **connection_id** has moved to the private section.

### Task 3: Create Instance Constructor

Replace the public method **set_attributes** with an instance constructor to ensure that the attributes are set during the creation of a new instance and that they are not changed afterward.

#### Steps

1. Comment out the definition and implementation of the method **set_attributes**.
    
    a. Select all lines that belong to the METHODS set_attributes statement, including the line with the period sign (.).
          
    b. Press **Ctrl + <** to add a star sign (*) in front of each selected line.
          
    c. Select all lines that belong to the implementation of the method **set_attributes** including the corresponding ENDMETHOD. statement and press **Ctrl + <** again.

2. Add an instance constructor to the local class **lcl_connection** using a quick fix. Ensure that the constructor has importing parameters corresponding to attributes **carrier_id** and **connection_id**.
    
    a. Place the cursor on the name of the class and press **Ctrl + 1**.
          
    b. Double-click on _Generate constructor_.
          
    c. In the dialog box, ensure that **carrier_id** and **connection_id** are selected and choose _Finish_.

3. Extend the generated constructor definition. Add exception **CX_ABAP_INVALID_VALUE** to the definition of the method **constructor**.
    
    a.  Navigate to the generated definition of the method **constructor**.
          
    b.  Adjust the code as follows:
      ``` ABAP
      METHODS constructor
            IMPORTING
              i_connection_id TYPE /dmo/connection_id
              i_carrier_id    TYPE /dmo/carrier_id
            RAISING
              cx_ABAP_INVALID_VALUE.
      ```
    
4. Extend the generated constructor implementation. If either of the importing parameters is initial, raise exception **CX_ABAP_INVALID_VALUE**.

    a. Navigate from the definition to the implementation of the method **constructor**, for example, by placing the cursor on **constructor** and pressing **F3**.

    b. Adjust the code as follows:

      ``` ABAP
      METHOD constructor.
           
          IF i_carrier_id IS INITIAL OR i_connection_id IS INITIAL.
            RAISE EXCEPTION TYPE cx_abap_invalid_value.
          ENDIF.
          
          me->connection_id = i_connection_id.
          me->carrier_id    = i_carrier_id.
          
      ENDMETHOD. 
      ```

5. In the constructor implementation, add a statement to increase the value of the static attribute **conn_counter** by one. Make sure the statement is only executed if no exception was raised.

    a. Adjust the code as follows:

      ``` ABAP
          me->connection_id = i_connection_id.
          me->carrier_id    = i_carrier_id.
             
          conn_counter = conn_counter + 1.
          
      ENDMETHOD.
      ```

6. Make sure that it is not possible to change the value of **conn_counter** from outside the class.

    a. Add READ-ONLY to the declaration of the static attribute **conn_counter**.

    b. Adjust the code as follows:

      ``` ABAP
          CLASS-DATA conn_counter TYPE i READ-ONLY.
      ```

### Task 4: Use the Constructor

Adjust the instantiation of class **lcl_connection** to supply the parameters of the instance constructor and handle the exception.

#### Steps

1. In method **if_oo_adt_classrun~main**, go to the NEW #( ) expression for the first instance of **lcl_connection**.
    
    a. Switch to the _Global Class_ tab.
          
    b. Find the first line with connection = NEW #( )..
    
2. In the NEW expression, supply the import parameters of **constructor**.
    
    > **Hint:** You can copy the parameter passing from the call of method **set_attributes** for this instance.
    
    a. Adjust the NEW expression as follows:
    
      ``` ABAP

          connection = NEW #( 
                              i_carrier_id    = 'LH'
                              i_connection_id = '0400' 
                            ).

      ```

3. Remove or comment out all calls of the method **set_attributes**.
    
    a. Select all lines that belong to the connection->set_attributes( ... ). statement, including the line with the closing bracket and the period sign (.).
          
    b. Press **Ctrl + <** to add a star sign (*) in front of each selected line.

4. Move the instance creation into the TRY block of the exception handling.
    
    a. Move the TRY. statement up, to before the instance creation.
          
    b. Your first instance creation should now look like this:

      ``` ABAP

      TRY.
          connection = NEW #( 
                              i_carrier_id    = 'LH'
                              i_connection_id = '0400'
                            ).
             
      *         connection->set_attributes(
      *           EXPORTING
      *             i_carrier_id    = 'LH'
      *             i_connection_id = '0400'
      *         ).
          
          APPEND connection TO connections.
          
        CATCH cx_abap_invalid_value.
          out->write( `Method call failed` ).
      ENDTRY.

      ```

5. Repeat the previous steps for the other instances of your local class.

    a. After this step, the implementation of the method **if_oo_adt_classrun~main** should look similar to this:
      ``` ABAP

      METHOD if_oo_adt_classrun~main.
          
          DATA connection TYPE REF TO lcl_connection.
          DATA connections TYPE TABLE OF REF TO lcl_connection.
          
      * First Instance
      *********************************************************************
          TRY.
            connection = NEW #( 
                                i_carrier_id    = 'LH'
                                i_connection_id = '0400'
                              ).
             
      *         connection->set_attributes(
      *           EXPORTING
      *             i_carrier_id    = 'LH'
      *             i_connection_id = '0400'
      *         ).
          
              APPEND connection TO connections.
          
            CATCH cx_abap_invalid_value.
              out->write( `Method call failed` ).
          ENDTRY.
          
      * Second instance
      *********************************************************************
          TRY.
            connection = NEW #(  
                                i_carrier_id    = 'AA'
                                i_connection_id = '0017'
                              ).
                  APPEND connection TO connections.
          
            CATCH cx_abap_invalid_value.
              out->write( `Method call failed` ).
          ENDTRY.
          
      * Third instance
      *********************************************************************
          TRY.
            connection = NEW #(  
                                i_carrier_id    = 'SQ'
                                i_connection_id = '0001'
                              ).
                APPEND connection TO connections.
          
            CATCH cx_abap_invalid_value.
              out->write( `Method call failed` ).
          ENDTRY.
          
      * Output
      *********************************************************************
          LOOP AT connections INTO connection.
            out->write( connection->get_output( ) ).
          ENDLOOP.
      ENDMETHOD.

      ```
    
6. Activate the class. Execute it and debug the instantiation.
    
    a. Press **Ctrl + F3** to activate the class.
          
    b. Press **F9** to run the class.