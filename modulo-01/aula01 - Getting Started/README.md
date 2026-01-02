# Getting Started

## Different Types of ABAP Project

![Ilustração](./images/Ilustracao.png)

Eclipse as a development environment is not embedded in the ABAP system. Instead, you have to connect to each ABAP system in which you want to work, and each connection is represented in Eclipse by a project. There are two kinds of project in ADT - ABAP Projects, which you use to connect to an on-premise ABAP system, and ABAP Cloud Projects, which you use to connect either to SAP BTP ABAP or to SAP S/4HANA Cloud.

> **NOTE:** In the following, we are only going to discuss how to access a cloud system.

## Locating the ABAP Environment

![Ilustração](./images/Ilustracao02.png)

The SAP Business Technology Platform is SAP's platform as a service (PAAS). To access it, you need to create a global account. There are various subscription models available, depending on whether you need to run large-scale productive environments or just a single-user environment for your own continuing professional development.

Within a global account, there are one or more subaccounts. Each subaccount can be configured differently, so that a single enterprise can run several different platforms but manage their subscription using just the single global account. Inside the subaccount, you deploy a runtime such as Cloud Foundry or Kyma. Once you have done this, you can deploy an ABAP instance.

> **NOTE:** In this course we are using an ABAP instance deployed on the SAP Business Technology Platform (SAP BTP). However, the material is also relevant for other ABAP deployments, such as an on-premise SAP S/4HANA system or an SAP S/4HANA Cloud system. [How to Create an ABAP Cloud Project](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_CFA821567D1C62AF:demo)

## Investigation of ABAP Code

### Working in the Project Explorer

To work with ABAP development objects, you usually display the contents of a package in the project explorer. Packages are containers for development objects that logically belong together.

[Ilustration - Working in the Project Explorer](./images/02_Analyzing_ABAP_Coding_001.png)

To add a package to your favorites, right-click the _Favorite Packages_ node in the project explorer and choose _Add Package_. A dialog box appears, in which you can enter a filter term. The system then displays only the packages that contain this term. Double-click the package that you want to add to your favorites.

### Opening Development Objects

There are two ways to open a development object in ADT.

[Ilustration - Opening Development Objects](./images/02_Analyzing_ABAP_Coding_002.png)

The first is to locate the object name in the project explorer and double-click it. The other is to use the keyboard shortcut, ``Ctrl + Shift + A``. This opens a dialog box in which you can enter part of the name of an object.

### Investigating a Development Object

When you are working with ABAP code, there are certain function keys in ADT that will help you.

Let's look at these function keys.

[Ilustration - Investing a Development Object](./images/02_Analyzing_ABAP_Coding_003.png)

- **`F1`**: The F1 key displays the ABAP language help for the current statement. A statement is the name for a command in ABAP.
- **`F2`**: The F2 key displays information about the element on which the cursor is placed.
- **`F3`**: The F3 key navigates to the definition of the object on which the cursor is placed. You can return from there to the original object using the key combination, ``ALT + LEFT ARROW``, on your keyboard.

## Create an ABAP Cloud Project and Investigate ABAP Coding

In this exercise, you start your ABAP cloud project and analyze some existing ABAP code to get familiar with some basic features of the ABAP development environment.

### Prerequisites

If you want to do the exercises as described in this learning journey, you have to book the dedicated hands-on practice system. Follow this link for details: [https://learning.sap.com/practice-systems/basic-abap-programming](https://learning.sap.com/practice-systems/basic-abap-programming).

In addition to the **service key**, which you will receive during the booking process, you need a local installation of the _Eclipse IDE_ with the _ABAP Development Tools_ plug-in installed on top. Follow this link for a step-by-step instruction [https://developers.sap.com/tutorials/abap-install-adt.html](https://developers.sap.com/tutorials/abap-install-adt.html).

### Task 1: Create an ABAP Cloud Project

Open Eclipse, switch to the ABAP perspective and create an ABAP cloud project.

#### Steps

1. Open Eclipse and switch to the ABAP perspective.

      a. Open Eclipse and close all tabs.

      b. Choose _Window_→_Perspective_→_Open Perspective_→_Other_.

      c. In the dialog box, double-click _ABAP_.

2. Create an _ABAP Cloud Project_.

      a. Choose _File_→_New_→_ABAP Cloud Project_.

      b. Towards the bottom of the popup box choose _Extract_.

      c. Choose _Import_ and select the file containing the service key that you have been given. After importing click _Copy to Clipboard_ to copy the ABAP service instance URL and then click _Close_.

      d. In the top of the popup box in the _ABAP Service Instance URL:\*_ field paste the ABAP Service Instance URL with the key combination _Ctrl + V_. When finished choose _Next_.

      e. Choose _Open Logon Page in Browser_.

      f. You will be prompted to logon to the system. Enter the user and password that you used to register to SAP Learning Hub.

      g. When you see the message, _You have been successfully logged on_, close the browser window and return to Eclipse.

      h.  To finish creating the project, choose _Finish_.

### Task 2: Analyze ABAP Class /DMO/CL_FLIGHT_LEGACY

Use some functions of the _ABAP Development Tools_ to analyze the source code of ABAP class **/DMO/CL_FLIGHT_LEGACY**.

> **NOTE:** You are not required to read the code in this exercise. Instead, concentrate on the navigation and display functions in the development environment.

#### Steps

1. Open the ABAP class **/DMO/CL_FLIGHT_LEGACY** in the editor view.

      a. In the Eclipse menu, choose _Navigate_→_Open ABAP Development Object ..._ or press **Ctrl + Shift + A**

      b. In the input field, enter **/DMO/CL_FLIGHT** as search string.

      c. In the list of matching items, click on `/DMO/CL_FLIGHT_LEGACY` _(Global Class)_ and choose _OK_.

2. Open the _Properties_ tab to display the administrative data of development object **/DMO/CL_FLIGHT_LEGACY**.

      a. Place the cursor anywhere in the source code of class **/DMO/CL_FLIGHT_LEGACY**.

      b. In the tab strip below the editor view, navigate to tab _Properties_.

      c. Analyze the administrative data that are displayed there, for example the original language, the time stamp of the last change or the user that at first created the object.

3. Locate the ABAP class **/DMO/CL_FLIGHT_LEGACY** in the _Project Explorer_ view on the left of the display.

      a. Place the cursor anywhere in the source code of the class **/DMO/CL_FLIGHT_LEGACY**.

      b. On the tool bar of the project explorer, choose _Link with Editor_. This should expand a part of the tree under your ABAP project with the ABAP class **/DMO/CL_FLIGHT_LEGACY** as its end point.

4. Find the string **get_instance( )->get** in the source code of ABAP class **/DMO/CL_FLIGHT_LEGACY** (There has to be exactly one blank between the brackets).

      a. Place the cursor in the first row of the source code of class **/DMO/CL_FLIGHT_LEGACY**.

      b. Press **Ctrl + F** to open the _Find/Replace_ dialog.

      c. In the input field labeled with _Find:_ enter **get_instance( )->get** and choose _Find_.

5. Navigate to the definition of code element get(.

      a.  In the current code line, place the cursor on get( and choose _Navigate_→_Navigate to_. Alternatively, press **F3**.

6. A few code lines down there is a line starting with SELECT. Without navigating away, display some information about code element /dmo/travel after keyword FROM.

      a. In the code line starting with SELECT, place the cursor on /dmo/booking and choose _Source Code_→_Show Code Element Information_. Alternatively, press **F2**.

7. Display the ABAP Language help for the keyword SELECT.

      a. At the beginning of the current code line, place the cursor on SELECT and choose _Source Code_→_Show ABAP Language Help_. Alternatively, press **F1**.

## Understanding Software Structure and Logistic

### Organizing Development

[Ilustration - Organizing Development](./images/03_Software_Logistics_001.png)

When you create a development object in the ABAP environment, you must assign it to a package. Packages serve as containers for all of the development objects that logically belong together. Each package is also assigned to a software component. The complete set of development objects in the system is referred to as the ABAP Repository. Consequently, development objects are also often called repository objects.

You develop your applications in a development environment, but must then ensure that they can be tested in an appropriate test environment before being moved on to the production environment. Typically, you will have a single global account and a subaccount for each of the development, test, and production environments. Software components allow you to transport your objects.

### Transport Request

[Ilustration - Transport Request](./images/03_Software_Logistics_002.png)

When you create a new development object or change an existing one, you must assign it to a transport request. Transport requests ensure that all development objects that logically belong together are transported together into the test, and subsequently the production system.

Each transport request has an owner, and the owner can assign other users to the request. In this way, transport requests support team development.

When an object is included in a transport request, it is locked. This means that it can only be edited by a user who is assigned to the same request.

When work on all of the objects in the request is finished, all of the developers assigned to it must release their work. After this, the owner of the request can release the entire request. If the transport request belongs to a transportable software component, the system administrator can import it into the test system for testing.

When you release any kind of transport request, the system releases the locks on the objects in the request, so that any developer can access them again.

To learn how to create an ABAP package, refer to the demonstration, How to Create a Package.

> Tutorial: [How to Create an ABAP Package](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_E85FDC0DEC83B2B8:demo)

### Create an ABAP Package

In this exercise, you first add package **/LRN/S4D400_EXERCISE** to the list of _Favorite Packages_. You then create a new package under the superpackage **ZLOCAL** (suggested name: **ZS4D400_##** where ## is your group number).

> **NOTE:** This package will be the home for all development objects that you will create in this course.

Your new package should have the following attributes:

| Attribute | Value |
| --- | --- |
 _Name_ | **ZS4D400_##**, where **##** is your group number |
 _Description_ | **My ABAP Package** |
 _Add to favorite packages_ | Checked |
 _Superpackage_ | **ZLOCAL** |
 _Package Type_ | **Development** |
 _Software Component_ | **ZLOCAL** |
 _Application Component_ | Leave this field blank |
 _Transport Layer_ | Leave this field blank |

#### Steps

1. Add package **/LRN/S4D400_EXERCISE** to the list of _Favorite Packages_.

      a. In the _Project Explorer_ on the left, expand your ABAP cloud project and then sub node _Favorite Packages_.

      b. Right-click _Favorite Packages_ and choose _Add Package ..._.

      c. In the search field, enter **/LRN/S4D400**.
  
      d. From the list of matching items, select `/LRN/S4D400_EXERCISE`, then choose _OK_.

2. In your ABAP Cloud project, create a new package with the attributes listed above. When you are prompted for a transport choose the transport request in which you are involved. If no transport request is listed, create a new request.

      a. In the Project Explorer, right-click on your ABAP Cloud Project and choose _New_→_ABAP Package_.

      b. Enter the package name **ZS4D400_##** where **##** is your group number.

      c. Enter the description **My ABAP Package**.

      d. Mark the checkbox _Add to favorite packages_

      f. Enter the superpackage **ZLOCAL**.
 
      g. Ensure that the _Package Type_ is set to **Development**.

      h. Choose _Next_.
 
      i. Ensure the _Software component_ is set to **ZLOCAL**.

      j. Ensure that _Application component_ is empty.

      k. Ensure that _Transport layer_ is empty.

      l. Choose _Next_.

      m. Check if there is a transport request listed under option _Choose from requests in which I am involved_. If this is the case, choose this option. If the list is empty, choose the option _Create a new request_ and enter a request description, for example **ABAP Exercises**.

      n. Choose _Finish_.

3. Ensure that your new package has been added to the _Favorite Packages_.

      a. In the _Project Explorer_ on the left, expand your ABAP cloud project and then sub node _Favorite Packages_.
 
      b. Make sure your own package, **ZS4D400_##**, is listed here. If not, add it like you added package **/LRN/S4D400_EXERCISE** earlier.

## Developing Your First ABAP Application

### A 'Hello World' App in ABAP

As in all other programming languages, the first thing that you should do in ABAP is familiarize yourself with the development environment and the most elementary aspects of the language by writing a short "Hello World" app.

The main user interface technology that you will use in modern ABAP programming is Fiori Elements. However, ADT provides a console for you to create output quickly and simply in test applications.

> Tutorial: [How to Create a 'Hello World' Application](https://learnsap.enable-now.cloud.sap/pub/mmcp/index.html?library=library.txt&show=project!PR_B569C1AE21873BA7:demo)

## Create a 'Hello World' Application

### Task 1: Create a Hello World Application

In your package, create an new ABAP class. Let the class implement interface **IF_OO_ADT_CLASSRUN** so that you can use the class as the main program for an Eclipse console app.

#### Steps

1. In your package, create a new ABAP class with the name **ZCL_##_HELLO_WORLD**. Ensure that it uses the interface **IF_OO_ADT_CLASSRUN**. When you are prompted to assign the class to a transport request, use the transport request that you created in the previous task.

      a. Choose _File_→_New_→_ABAP Class_.

      b. Enter your package **ZS4D400_##**, where **##** is your group number.

      c. Enter the name **ZCL_##_HELLO_WORLD** where **##** is your group number, and enter a description for your class.

      d. Choose _Add..._ (next to the _Interfaces_ group box).

      e. Enter the filter text **IF_OO_ADT_CLASSRUN**. Double-click the matching entry in the hit list.

      f. Choose _Next_.

      g. Select _Choose from requests in which I am involved_ and your own transport request.

      h. Choose _Finish_.

2. In the **if_oo_adt_classrun~main( )** method, use ``out->write( )`` to output the phrase _Hello World_.

      a. In the editor, enter the following coding between ``METHOD if_oo_adt_classrun~main`` and ``ENDMETHOD.``:

          ```abap

          1
          2 out->write( 'Hello World' ).
          3

          ```

3. Activate and test your class.

      a. Activate the class with the keyboard shortcut **Ctrl + F3**.

      b. Run the class with the **F9** key.

4. Check the output in the _Console_ view of Eclipse.

      a. Check the _Console_ view that should have opened as a new tab below the editor view.

      b. If the _Console_ view is not visible, open it by choosing _Window_→_Show view_→_Other_. Double-click _Console_ in the hit list.
