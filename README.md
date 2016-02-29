---
title: "HarvestChoice - Data API (server component)"
author: "Bacou, Melanie"
date: "8/13/2015"
output: html_document
---

## About

> CELL5M: A Multidisciplinary Geospatial Database for Africa South of the Sahara

*HarvestChoice CELL5M-SSA* is an on-line database of over 750 biophysical and socio-economic indicators for Africa south of the Sahara. This database was created as the central core to a decision-support system for policy analysts, development practitioners, and researchers to explore the spatial relationships at the nexus of our climate, the environment, agriculture, nutrition, health, and poverty. All indicators are currently geo-referenced to a spatial resolution of **5 arc-minute** (equivalent to around 10 km^2^ at the Equateur).

This repo contains an R package for interacting with CELL5M-SSA layers. Currently this R package cannot be installed locally. Instead all data access methods are made available through a REST API (credits to [OpenCPU](http://github.com/jeroenooms/opencpu). HTTP POST requests may be sent using any REST-compatible client (e.g. cURL). Sample requests using cURL at the command line, in JavaScript, as well as in R and STATA are provided in the package documentation.

More about HarvestChoice spatial datasets for sub-Saharan Africa at http://harvestchoice.org/data/. To view this data in your browser visit [HarvestChoice MAPPR](http://apps.harvestchoice.org/mappr).


The package documentation is at http://harvestchoice.github.io/hc-api3. All methods may also be viewed through the API at http://hcapi.harvestchoice.org/ocpu/library/hcapi3/html.

HTTP POST requests may be tested interactively at the command line. To test requests in the console, use the following `curl` syntax. Arguments may be passed as a well-structured JSON array with the extra `-H Content-Type:application/json` flag.

```sh
curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi \
-d '{"var":["bmi","maiz_h"], "iso3":"TZA", "format":"tif"}' \
-X POST -H 'Content-Type:application/json'
```
