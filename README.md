# Build transactional SAP Fiori elements-based Apps with nominated APIs using Tier 2 extensibility model in ABAP RESTful Application Programming Model (RAP)
> When you are developing an RAP Application in SAP S/4 Private Cloud Edition and you identify a artifact like `BAPI` which has a missing C1 released artifact but you find it as `nominated API` , you mitigate the situation by creating a Tier 2 wrapper  
>

You can have a brief understanding on: [ ABAP Cloud - How to mitigate missing released SAP APIs in SAP S/4HANA Cloud, private edition and SAP S/4HANA – The new ABAP Cloud API enablement guide](https://community.sap.com/t5/enterprise-resource-planning-blogs-by-sap/abap-cloud-how-to-mitigate-missing-released-sap-apis-in-sap-s-4hana-cloud/ba-p/13561479)

# Level: Intermediate

# Introduction

**Description**
This repository contains the material for the `Devtoberfest 2024- Clean Core Extensibility with ABAP Cloud in SAP S/4HANA Cloud Private Edition` . 

It includes two packages:
>ZDSAG_CLASIFICATION_CLOUD(ABAP Cloud Language Version) - Contains managed Business Object (BO) with BO nodes or entities, BO projection views, late numbering, determine actions, actions, side effects, functions, business event exposure, and additional save.

>ZDSAG_CLASSIFICATION_C1( Standard ABAP Language Version) - Contains wrapper for Nominated APIs and CDS entity for not C1 released artifact.


## Table of Content
- [Requirements](#requirements)
- [Overview](#overview)
- [Recording](#recording)
- [Solution Overview](#solution-overview)
- [How to download and install this example](#how-to-download-and-install-this-example)
- [How to obtain support](#how-to-obtain-support) 
- [Further Information](#ℹfurther-information)
## 📋 Requirements
[^Top of page](#)

> You need the latest version of the ABAP Development Tools for Eclipse (ADT) on your laptop or PC as well as the access to an appropriate ABAP system* to carry out the practical exercises of this workshop.
>
> (*) The supported ABAP systems are release 2023 of SAP S/4HANA Cloud Private Edition and SAP S/4HANA.

<details>
  <summary>Click to expand!</summary>

The requirements to follow the exercises in this repository are:
1. [Install the latest Eclipse platform and the latest ABAP Development Tools (ADT) plugin](https://developers.sap.com/tutorials/abap-install-adt.html)
2. [Understand the sample scenario how to mitigate a missing released SAP API](https://developers.sap.com/tutorials/abap-s4hanacloud-purchasereq-understand-scenario.html) (_Read Step 4 for technical requirements_)
3. [Create an ABAP Cloud Project](https://developers.sap.com/tutorials/abap-environment-create-abap-cloud-project.html)

</details>

## 🔎Overview

> This repository is all about how to build wrapper around nominated APIs and use it as RAP BO  ; especially about how to use  when building greenfield implementations.

<details>
  <summary>Click to expand!</summary>

### Business Scenario
In a nutshell
> We create/update `Classification` using nominated APIs
> 
> Assign the `Classification` to `Material Master` using nominated API.
>
> If the Material is assigned with `Classification` for the first time we raise an BOR event 
>
> 
This session material guides you through the development of the OData service of a SAP Fiori elements based _Classification Processing App_ with RAP, using the _managed_ business object (BO) runtime implementation with unmanaged save and  late numbering.
>  
> Here we chose  `unmanaged save` as we intend to use nominated APIs aka `BAPI` for CRUDQ trasanctional capabilities .
>
The OData service you are going to implement is based on the SAP GUI transaction CL01/CL02/CL03.
>
> To set the business context, the scenario is the following: The department responsible for managing "Classification" is requesting you to build a new Fiori app with draft capabilities for processing (i.e. creating, updating and deleting) classification.
>


<details>
  <summary>Click to expand!</summary>

>Scenario 1: Creation and Update existing `Classification`
>
The resulting _Classification Processing App_ app is a SAP Fiori elements-based List Report app with search, filter, and draft capabilities for processing _Classification_ . A navigation to an Object Page for displaying the details of each _Classification_ entry in the list report is offered. The application will look like this: 

**List Report**:
<img src="images/ListReport.png" alt="Classification App - List Report" width="100%">
  
**Object Page**: 
<img src="images/ObjectPage.png" alt="Classification App - Object Page" width="100%">

Let us breakdown of  SAP GUI transaction _CL01_ which comprises of different section(information stored across multiple database)   _Classification App_ .

**Basic Data**:
<img src="images/CL01_Basic_Data.png" alt="CL01-Basic Data" width="100%">

**Keywords**:
<img src="images/CL01_Keywords.png" alt="CL01-Keywords" width="100%">

**Characteristics**:
<img src="images/CL01_Characteristics.png" alt="CL01-Characteristics" width="100%">

**So we would be building a RAP application that would be a mimic of these sections from CL01/CL02/CL03.**
<img src="images/GUI_RAP application.png" alt="SAP GUI CL01 transaction to RAP Application" width="80%">

**Disclaimer**: We have taken only few sections from CL01 transaction for this sample.


Below is the simplified _Classification_ data model underlying the app.

<img src="images/Flowdiagram.png" alt="Classification Creation/Updation Model" width="100%">

>Scenario 2: Assign existing `Classification` to a `Material Master`
>
We use nominated API , to assign existing `Classification` to existing `Material`.

> We achieve this help of `Action` in RAP.

**Action to do this assignment of Classification to Material**:
<img src="images/RAP _Action.png" alt="RAP Action" width="100%">

**We can do this in classic way directly using SAP GUI transaction MM01/MM02**:
<img src="images/MM01_MM02_MM03_Assignment.png" alt="CL01-Keywords" width="100%">

</details>
</details>

## 🛠 Solution Overview

> You can import the solution package **`ZDSAG_CLASSIFICATION_C1`** **`ZDSAG_CLASIFICATION_CLOUD`** into your system* by following [How to download and install this example](#how-to-download-and-install-this-example). 
>
> (*) The supported ABAP systems are 2023 of SAP S/4HANA Cloud Private Edition and SAP S/4HANA.
<details>
  <summary>Click to expand!</summary>

**ZDSAG_CLASSIFICATION_C1**

>Lets see what are the objects present in this package:

1. ZDSAG_BAPI_CLASS_CREATE - Tier 2 Wrapper for nominated API of Classification Creation - `BAPI_CLASS_CREATE`
2. ZDSAG_BAPI_CLASS_CHANGE - Tier 2 Wrapper for nominated API of Classification Update -`BAPI_CLASS_CHANGE`
3. ZDSAG_BAPI_OBJCL_CREATE - Tier 2 Wrapper for nominated API of Classification Assignment to Material Master - `BAPI_OBJCL_CREATE`
4. ZDSAG_GET_CLASS_DETAIL  - Tier 2 Wrapper for reading existing 'Classification' details - here we combine usage of nominated API `BAPI_CLASS_GET_CHARACTERISTICS` and `I_CLFNCLASS`(Basic Data) and `I_CLFNCLASDESCRIPTION`(Keywords).
5. ZDSAG_BOR_EVENT_HANDLER - BOR Event Handler Implementation

**Note:**  
The package contains other objects as well, but we have given overview of only few key artifacts.

**ZDSAG_CLASIFICATION_CLOUD**

>Lets see what are the objects present in this package:

>BO - Business Object

1. ZDSAG_R_CLASSIFICATION - Root BO for `Basic Data`
2. ZDSAG_C_CLASSIFICATION - Root BO Projection View
3. ZDSAG_R_CLASSDESCRIPTION - BO for `Keywords`
4. ZDSAG_C_CLASSDESCRIPTION - Projection view for `Keywords` BO
5. ZDSAG_R_CLASSCHARACTERS - BO for `Characteristics`
6. ZDSAG_C_CLASSCHARACTERS - Projection view for `Characteristics` BO
7. ZDSAG_A_CLASSIFICATION_CREATE - Abstract entity for `Classification` created RAP event

**Note:**  
The package contains other objects as well, but we have given overview of only few key artifacts.
</details>  


## 📤 How to download and install this example

Follow this instructions to import the solution:

1. [Step 1: Install the standalone abapGit version](https://docs.abapgit.org/user-guide/getting-started/install.html) if you have not already done so.
2. [Step 2: Install an Offline repository ](https://docs.abapgit.org/user-guide/projects/offline/install.html) -you can create package here if not done before.
3. Step 3: Download Code of this Git hub repository in Zip format <img src="images/DownloadZIPCode.png" alt="Download ZIP code" width="100%">
4. [Step 4: Using installed standalone abapGit in Step 1 and installed offline repo from Step 2 Import ZIP file downloaded from Step 3 ](https://docs.abapgit.org/user-guide/projects/offline/import-zip.html).
>First Install Package `ZDSAG_CLASSIFICATION_C1`

>Next Install Package `ZDSAG_CLASIFICATION_CLOUD`
6. Now pull/import the solution implementation using the context menu _**Pull...**_.
7. Activate the imported development objects (**Ctrl+Shift+F3**).


## 📹Recording 
[^Top of page](#)

For a compact overview of this repository , watch the session replay from SAP Devtoberfest 2024 (_gated content_):  

⏺  [Clean Core Extensibility with ABAP Cloud in SAP S/4HANA Cloud Private Edition - Youtube Video ](https://www.youtube.com/watch?v=HQPXI1Ba-Gk&list=PL6RpkC85SLQDHz97qsNTNAE2jnUKj8X5d&index=65)

**Note:**  
The code available in this repository is slightly different from this recording.

## Known Issues
<!-- You may simply state "No known issues. -->

## How to obtain support
[Create an issue](https://github.com/SAP-samples/abap-platform-nominated-apis-consumption/issues) in this repository if you find a bug or have questions about the content.
 
For additional support, [ask a question in SAP Community](https://answers.sap.com/questions/ask.html).
## ℹFurther Information
[^Top of page](#)

You can find further information on the ABAP RESTful Application Programming Model (RAP) here:
  - 📃 [State-of-the-Art ABAP Development with RAP | SAP Community](https://community.sap.com/topics/abap/rap)- A collection of diverse getting started materials.
  - 📃 [ABAP Cloud API Enablement Guidelines for SAP S/4HANA Cloud, private edition, and SAP S/4HANA](https://www.sap.com/documents/2023/05/b0bd8ae6-747e-0010-bca6-c68f7e60039b.html).
  - 📃 [The list of available S/4HANA Cloud Private Edition Classic APIs ](https://github.com/SAP/abap-atc-cr-cv-s4hc/blob/main/src/objectReleaseInfoLatest.csv)
  - 📃 [Understand the sample scenario how to mitigate a missing released SAP API](https://developers.sap.com/tutorials/abap-s4hanacloud-purchasereq-understand-scenario.html)- A collection of diverse getting started materials.   
  - 📄 [ABAP Cloud – SAP S/4HANA extensibility – May 2023 update | SAP Blogs](https://blogs.sap.com/2023/05/26/abap-cloud-sap-s-4hana-extensibility-may-2023-update/)
  - ❓ Most frequently asked questions: [RAP FAQ](https://blogs.sap.com/2020/10/16/abap-restful-application-programming-model-faq/) 
  - 🛠 [Develop and Run a Fiori Application with SAP Business Application Studio | SAP Tutorials](https://developers.sap.com/tutorials/abap-environment-deploy-cf-production.html)
  - 🛠 [Landing page with various hands-on workshop materials on ABAP Cloud, including RAP and embedded analytics](https://github.com/SAP-samples/abap-platform-rap-workshops/blob/main/README.md) 

## Contributing
If you wish to contribute code, offer fixes or improvements, please send a pull request. Due to legal reasons, contributors will be asked to accept a DCO when they create the first pull request to this project. This happens in an automated fashion during the submission process. SAP uses [the standard DCO text of the Linux Foundation](https://developercertificate.org/).

## License
Copyright (c) 2024 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.
