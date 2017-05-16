---
title:  "CELL5M-SSA DATA PACKAGE"
author: "IFPRI/HarvestChoice"
date:   "2017-05-16"
---


--------------------------------------------------------------------------------------

## About

*HarvestChoice CELL5M-SSA* is an on-line database of over 750 biophysical and socio-economic indicators for Africa south of the Sahara. This database was created as the central core to a decision-support system for policy analysts, development practitioners, and researchers to explore the spatial relationships at the nexus of our climate, the environment, agriculture, nutrition, health, and poverty. All indicators are currently geo-referenced to a spatial resolution of **5 arc-minute** (equivalent to around 10 km^2 at the Equateur).

## Data Retrieval

Several options are available to retrieve subsets of *HarvestChoice* indicators.

* **HarvestChoice Data Services v3.0**  
The preferred method of data retrieval is through HarvestChoice RESTful API published under [HarvestChoice Data Services v3.0](http://harvestchoice.github.io/hc-api3/). This API provides a full range of methods to retrieve, summarize, aggregate, graph and map HarvestChoice 5-arc-minute spatial layers for sub-Saharan Africa.

This data package was generated through this API. Here is the Curl commmand that was used to retrieve this archive:


```bash

curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/datapackage \
 -d '{"var":["sorg_p"]}' \
 -X POST -H 'Content-Type:application/json'

```

Other data retrieval and data visualization options are listed below. 

* **HarvestChoice MAPPR**  
An option to interactively explore and summarize HarvestChoice data layers is to use [HarvestChoice MAPPR](http://harvestchoice.org/mappr) on-line platform. Individual indicators may be mapped and summarized across pre-defined spatial domains or across user-selected geographies. MAPPR also provides advanced query tools for marketshed analyses.


* **HarvestChoice TABLR**  
Another tool for quick data summaries is [HarvestChoice TABLR](http://harvestchoice.org/tablr). This is an on-line report generator to quickly retrieve and export subsets of CELL5M-SSA. Best for users who do not need to visualize results on a map.

* **HarvestChoice Data Catalog**  
For more information about the *International Food Policy Resaearch Institute (IFPRI)* and *HarvestChoice Program*, and to explore individual data layers, visit [HarvestChoice website](http://harvestchoice.org/).


## Metadata

This package contains both a `META.csv` file with complete metadata records. A `datapackage.json` file compliant with the latest **Tabular Data Package** RFC specifications documented at http://dataprotocols.org/tabular-data-package/ is also included.

## Use Terms

Please refer to the [terms.html](./terms.html) file included in the root of this data package.


## Citation

Please cite and reference:

> HarvestChoice; International Food Policy Research Institute (IFPRI); University of Minnesota, 2016, *"CELL5M: A Multidisciplinary Geospatial Database for Africa South of the Sahara"*, http://dx.doi.org/10.7910/DVN/G4TBLF, Harvard Dataverse.


