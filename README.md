# Build transactional SAP Fiori elements-based Apps with nominated APIs using Tier 2 extensibility model in ABAP RESTful Application Programming Model (RAP)
> When you are developing an RAP Application in SAP S/4 Private Cloud Edition and you identify a artifact like `BAPI` which has a missing C1 released artifact but you find it as `nominated API` , you mitigate the situation by creating a Tier 2 wrapper  
>

You can have a brief understanding on: [ ABAP Cloud - How to mitigate missing released SAP APIs in SAP S/4HANA Cloud, private edition and SAP S/4HANA – The new ABAP Cloud API enablement guide](https://community.sap.com/t5/enterprise-resource-planning-blogs-by-sap/abap-cloud-how-to-mitigate-missing-released-sap-apis-in-sap-s-4hana-cloud/ba-p/13561479)

# Level: Intermediate

# Introduction

**Description**
This repository contains the material for the `Devtoberfest 2024- Clean Core Extensibility with ABAP Cloud in SAP S/4HANA Cloud Private Edition` . 

It includes two packages:
>ZDSAG_PRODUCT_LIST_CLOUD(ABAP Cloud Language Version) - Contains managed Business Object (BO) with BO nodes or entities, BO projection views,actions, side effects, functions, business event exposure.

>ZDSAG_PRODUCT_LIST_C1( Standard ABAP Language Version) - Contains wrapper for Nominated APIs and CDS entity for not C1 released artifact.


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
3. [Create software component using `ABAP For Cloud` as `ABAP Language Version`](https://help.sap.com/docs/ABAP_PLATFORM_NEW/b5670aaaa2364a29935f40b16499972d/8da870021323498db15a1c56cfc9b302.html)
4. You have created an ABAP Project in ADT that allows you to access your Application Server as mentioned above. Your log-on language is English.
5. You have downloaded and installed the `zabapgit_standalone` report. Make sure to use the most recent version as indicated on the [installation page](https://docs.abapgit.org/). 
6. You have installed the certificate files for github.com, see [abapGit Documentation](https://docs.abapgit.org/guide-ssl-setup.html).
   
</details>

## 🔎Overview

> This repository is all about how to build wrapper around nominated APIs and use it as RAP BO  ; especially about how to use  when building greenfield implementations.

<details>
  <summary>Click to expand!</summary>

### Business Scenario
In a nutshell
> We display the list of available materials from `Material Master`in system
> 
> Assign the `Material Master` to `Classification` using nominated API.
>
> If the Material is assigned with `Classification` for the first time we raise an BOR event 
>
> 
This session material guides you through the development of the OData service of a SAP Fiori elements based _Classification Assignment to Material App_ with RAP, using the _managed_ business object (BO) runtime implementation .
>  
The OData service you are going to implement is based on the SAP GUI transaction MM01/MM02.
>
> To set the business context, the scenario is the following: The department responsible for managing  "Materials  & Classification" is requesting you to build a new display only Fiori app with draft capabilities for processing assignment of Material with Classification.
>


<details>
  <summary>Click to expand!</summary>

>Scenario 2: Assign existing `Materail Master` to a `Classification`
>
We use nominated API , to assign existing `Material` to existing `Classification`.

> We achieve this help of `Action` in RAP.

**Action to do this assignment of Classification to Material**:
<img src="images/RAP _Action.png" alt="RAP Action" width="100%">

**We can do this in classic way directly using SAP GUI transaction MM01/MM02**:
<img src="images/MM01_MM02_MM03_Assignment.png" alt="CL01-Keywords" width="100%">

</details>
</details>

## 🛠 Solution Overview

> You can import the solution package **`ZDSAG_PRODUCT_LIST_C1`** **`ZDSAG_PRODUCT_LIST_CLOUD`** into your system* by following [How to download and install this example](#how-to-download-and-install-this-example). 
>
> (*) The supported ABAP systems are 2023 of SAP S/4HANA Cloud Private Edition and SAP S/4HANA.
<details>
  <summary>Click to expand!</summary>

**ZDSAG_CLASSIFICATION_C1**

>Lets see what are the objects present in this package:

1. ZCL_DSAG_BAPI_OBJCL - Tier 2 Wrapper for nominated API of Material Master to Classification Assignment - `BAPI_OBJCL_CREATE`
2. ZCL_DSAG_CLASS_ASSIGNMENT_CHK  - Tier 2 Wrapper for checking existing `Classification` details assigned to `Material Master` before triggering assignment .
3. ZCL_DSAG_BOR_HANDLER_PRODUCT - BOR Event Handler Implementation

**Note:**  
The package contains other objects as well, but we have given overview of only few key artifacts.

**ZDSAG_CLASIFICATION_CLOUD**

>Lets see what are the objects present in this package:

>BO - Business Object

1. ZDSAG_R_PRODUCT - Root BO for `Material Master`
2. ZDSAG_C_PRODUCT - Root BO Projection View
3. ZDSAG_I_CLASSIFICATIONHELPER - BO for `Classification`
4. ZDSAG_I_CHARACTERISTICHELPER - BO for `Characteristics`
   
**Note:**  
The package contains other objects as well, but we have given overview of only few key artifacts.
</details>  


## 📤 How to download and install this example

Use the <em>zabapgit_standalone</em> program to install the <em>RAP Nominated APIs consumption Scenario</em> by executing the following steps:
1.  Create software component `ZABAP_ON_CLOUD` using `ABAP For Cloud` as `ABAP Language Version` as mentioned in `Requirement` section of this file.
2.	In your ABAP project, create the package `ZCLASSIFICATION_SAMPLE` as target package for the demo content. Use `ZABAP_ON_CLOUD` as software component. Assign it to a new transport request that you only use for the demo content import. 
3.	In your ABAP project, run the program `zabapgit_standalone`.  
4.	Choose `New Online` and enter the following URL of this repository  `https://github.com/SAP-samples/abap-platform-nominated-apis-consumption`. 
5.	In the package field, enter the newly created package `ZCLASSIFICATION_SAMPLE`. In the branch field, select the branch `ABAP-platform-2023-classification-maintenance`.
6.	Leave the other fields unchanged and choose `Create Online Repo`.
7. Enter your credentials for abapgit. You will see the available artifacts to import into your ABAP system. 
8.	Choose `Pull` and confirm every subpackage on your transport request. 
9.	Select the package `ZCLASSIFICATION_SAMPLE` to be overwritten with the demo content. 
10. You will get an information screen telling you to only make repairs when they are urgent, which you can confirm.  
11. In the following screen, select all inactive objects and confirm the activation.
12.	Once the cloning has finished, refresh your project tree.


As a result of the installation procedure above, the ABAP system creates an inactive version of all artifacts from the demo content and adds the following sub packages to the target package: 
* `ZDSAG_PRODUCT_LIST_C1`
* `ZDSAG_PRODUCT_LIST_CLOUD`

## Configuration

To generate service artifacts for the service bindings:
1. In each service binding, choose the button `Publish`.


NOTE: In case the activation via the button in the service bindings is not possible, you can use Gateway tools to activate the service, see [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/fc4c71aa50014fd1b43721701471913d/b58a3c27df4e406f9335d4b346f6be04.html?version=202210.LATEST#%EE%81%B0-service-transport2).  



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
