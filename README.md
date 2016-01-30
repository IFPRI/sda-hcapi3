---
title: "HarvestChoice - Data API (server component)"
author: "Bacou, Melanie"
date: "8/13/2015"
output: html_document
---


HarvestChoice Data Services v3: Access HarvestChoice 10km Spatial Layers for sub-Saharan Africa.

Port of HarvestChoice data API to an OpenCPU server at http://hcapi.harvestchoice.org/ocpu/library/hcapi3. More about HarvestChoice spatial datasets for sub-Saharan Africa at http://harvestchoice.org/data/. To view this data in your browser visit [HarvestChoice MAPPR](http://apps.harvestchoice.org/mappr).

Currently this R package cannot be installed locally. Instead all data access methods are made available through a REST API (credits to [OpenCPU](http://github.com/jeroenooms/opencpu). HTTP POST requests may be sent using any REST-compatible client (e.g. cURL). Sample requests using cURL at the command line, in JavaScript, as well as in R and STATA are provided in the package documentation.

For a simple R interface with HarvestChoice data API, install `hcapi` instead.

The package documentation is at http://harvestchoice.github.io/hc-api3. Each method documentation may also be viewed at http://hcapi.harvestchoice.org/ocpu/library/hcapi3/man/{method}/html, e.g. http://hcapi.harvestchoice.org/ocpu/library/hcapi3/man/getLayer/html

HTTP POST requests may be tested interactively at http://hcapi.harvestchoice.org/ocpu/. To test requests in the console, use the following `curl` syntax. Arguments may be passed as well-structured json array with curl `-H Content-Type:application/json` flag or more directly as URL arguments using `-d 'param1="value1"&param2="value2"` construct.
