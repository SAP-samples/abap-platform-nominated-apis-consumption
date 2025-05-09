# Build transactional SAP Fiori elements-based Apps with nominated APIs using Tier 2 extensibility model in ABAP RESTful Application Programming Model (RAP)
> When you are developing an RAP Application in SAP S/4 Private Cloud Edition and you identify a artifact like `BAPI` which has a missing C1 released artifact but you find it as `nominated API` , you mitigate the situation by creating a Tier 2 wrapper  
>

You can have a brief understanding on: [ ABAP Cloud - How to mitigate missing released SAP APIs in SAP S/4HANA Cloud, private edition and SAP S/4HANA – The new ABAP Cloud API enablement guide](https://community.sap.com/t5/enterprise-resource-planning-blogs-by-sap/abap-cloud-how-to-mitigate-missing-released-sap-apis-in-sap-s-4hana-cloud/ba-p/13561479)

# Level: Intermediate

# Introduction

**Description**
This repository contains the material for the `Devtoberfest 2024- Clean Core Extensibility with ABAP Cloud in SAP S/4HANA Cloud Private Edition`  & `SAP Teched 2024- AD106 | Clean core extensibility of SAP S/4HANA Cloud Private Edition with ABAP Cloud`. 

## 🔎Overview

> This repository is all about how to build wrapper around nominated APIs and use it as RAP BO  ; especially about how to use  when building greenfield implementations.

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

## ABAP Platform
Choose this if you are working on SAP S/4HANA, on-premise edition or SAP S/4HANA Cloud, private edition. 
To import the reference scenario into your ABAP system, follow the steps in the relevant README file and download the sources from the branch corresponding to your your backend version: 

* [README](../ABAP-platform-2023-classification-maintenance/README.md) of branch <em>ABAP-platform-2023-Classification-Maintenance </em> (Application Server ABAP 7.58)
* [README](../ABAP-platform-2023-product-classification-assignment/README.md) of branch <em>ABAP-platform-2023-Product Classification Assignment </em> (Application Server ABAP 7.58)

## 📹Recording 
[^Top of page](#)

For a compact overview of this repository , watch the session replay from SAP Devtoberfest 2024 (_gated content_) & SAP Teched 2024 (_gated content_):  

⏺  [Clean Core Extensibility with ABAP Cloud in SAP S/4HANA Cloud Private Edition - Youtube Video ](https://www.youtube.com/watch?v=HQPXI1Ba-Gk&list=PL6RpkC85SLQDHz97qsNTNAE2jnUKj8X5d&index=65)

⏺  [AD106 | Clean core extensibility of SAP S/4HANA Cloud Private Edition with ABAP Cloud ](https://www.sap.com/events/teched/virtual/flow/sap/te24/catalog/page/catalog/session/1721791179982001rM76)

**Note:**  
The code available in this repository is slightly different from these recording.

## Known Issues
No known issues. 

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


## How to obtain support
[Create an issue](https://github.com/SAP-samples/abap-platform-nominated-apis-consumption/issues) in this repository if you find a bug or have questions about the content.
 
For additional support, [ask a question in SAP Community](https://answers.sap.com/questions/ask.html).

## Contributing
If you wish to contribute code, offer fixes or improvements, please send a pull request. Due to legal reasons, contributors will be asked to accept a DCO when they create the first pull request to this project. This happens in an automated fashion during the submission process. SAP uses [the standard DCO text of the Linux Foundation](https://developercertificate.org/).

## License
Copyright (c) 2024 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.

[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/abap-platform-nominated-apis-consumption)](https://api.reuse.software/info/github.com/SAP-samples/abap-platform-nominated-apis-consumption)
