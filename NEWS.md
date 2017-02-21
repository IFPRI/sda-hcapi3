# TODO

- [ ] gridify circa 2008 '11 PPP poverty estimates from Jul. 2016, also create combined urban/rural poverty densities and add correct weighing scheme (i.e. male nad female estimates should be weighted by male and female population respectively)
- [ ] ~~crop calendar (needs meta from Jawoo), have ASC and DBF~~
- [x] updated urban mask, slope, market access 2010 (in API, but not in MAPPR yet)
- [ ] drought occurence/median duration
- [ ] long term rain/temp (updated, higher resolution)
- [ ] 2015 population density (use revised land mask)
- [ ] GPW population projections to 2030
- [ ] ~~conflict casualties ACLED~~
- [ ] DHS gender-disag vars (Carlo selected key vars on Basecamp)
- [x] SPAM2005v3r0 (grab from SQL Server)
- [ ] Verify water/urban/rural land cover variables (`AREA_WBODY`, `PCT_RUR`, `PCT_RUR_100`, `PCT_URB`, `PCT_URB_100`, `AREA_CR_LC`, `AREA_LAND`, `AREA_RUR`, `AREA_TOTAL`, `AREA_URB`)
- [ ] Not all `varDesc` are complete in the variable inventory, need to fill in by hand


# NEWS

## 2016.12.22

Added slope and most recent market access variables to variable inventory in SQL Server.

```
[1]  "SLOPE"              "TT10_100K"          "TT10_20K"           "TT10_250K"         
[5]  "TT10_500K"          "TT10_50K"

```

Added 50 circa 2008 '11 PPP standardized poverty and expenditure variables to CELL5M database and variable inventory in SQL Server (for MAPPR). These estimates are based on the Feb. 2016 version of https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SEPATX. 

```
 [1] "FTPOV08_GAP190" "FTPOV08_GAP310" "FTPOV08_PN190"  "FTPOV08_PN310"  "FTPOV08_PT190"  "FTPOV08_PT310" 
 [7] "FTPOV08_SD190"  "FTPOV08_SD310"  "FTPOV08_SEV190" "FTPOV08_SEV310" "MTPOV08_GAP190" "MTPOV08_GAP310"
[13] "MTPOV08_PN190"  "MTPOV08_PN310"  "MTPOV08_PT190"  "MTPOV08_PT310"  "MTPOV08_SD190"  "MTPOV08_SD310" 
[19] "MTPOV08_SEV190" "MTPOV08_SEV310" "RTPOV08_GAP190" "RTPOV08_GAP310" "RTPOV08_PN190"  "RTPOV08_PN310" 
[25] "RTPOV08_PT190"  "RTPOV08_PT310"  "RTPOV08_SD190"  "RTPOV08_SD310"  "RTPOV08_SEV190" "RTPOV08_SEV310"
[31] "TPOV08_GAP190"  "TPOV08_GAP310"  "TPOV08_PN190"   "TPOV08_PN310"   "TPOV08_PT190"   "TPOV08_PT310"  
[37] "TPOV08_SD190"   "TPOV08_SD310"   "TPOV08_SEV190"  "TPOV08_SEV310"  "UTPOV08_GAP190" "UTPOV08_GAP310"
[43] "UTPOV08_PN190"  "UTPOV08_PN310"  "UTPOV08_PT190"  "UTPOV08_PT310"  "UTPOV08_SD190"  "UTPOV08_SD310" 
[49] "UTPOV08_SEV190" "UTPOV08_SEV310"

 [1] "FFOODEXPM08_PPP11"  "FGINI08_PPP11"      "FNFOODEXPM08_PPP11" "FOODEXPM08_PPP11"   "FPCEXPM08_PPP11"   
 [6] "GINI08_PPP11"       "MFOODEXPM08_PPP11"  "MGINI08_PPP11"      "MNFOODEXPM08_PPP11" "MPCEXPM08_PPP11"   
[11] "NFOODEXPM08_PPP11"  "PCEXPM08_PPP11"     "RFOODEXPM08_PPP11"  "RGINI08_PPP11"      "RNFOODEXPM08_PPP11"
[16] "RPCEXPM08_PPP11"    "UFOODEXPM08_PPP11"  "UGINI08_PPP11"      "UNFOODEXPM08_PPP11" "UPCEXPM08_PPP11" 
[21] "URB_2010"

```

Added SPAM2005 v3r0 crop production statistics to CELL5M API and SQL Server (599 variables updated).


## 2016.11.13

Updated circa 2008 '11 PPP sub-national poverty estimates and matching province/district survey maps and CELL5M concordance. Created bundled Data Package for IFPRI Dataverse. Refer to `./R/cell5mDataUpdate.2016.07_svy.R`.

## 2016.07.13

Added slope, 2010 market access layers.

## 2016.03.24

Fixes to 71 household survey maps and CELL5M concordance.

## 2016.03.22

Modify the administrative concordance to prepare for the latest 2011 PPP poverty estimates (also split SDN into SDN and SSD).

## 2015.08.13

Corrections to variable inventory to address issues reported at ESRI Conference and fix aggregation formulas.

## 2015.08.10

Mapped all new survey estimates for SSA (poverty and farm management), need to choose a new reference year (2008?) and intersect with CELL5M.

## 2015.07.26

Upgraded Apache to 2.4 and rebuilt RApache and OpenCPU on Buster. Further to HC Data API documented at http://github.com/harvestchoice/hc-api3.

## 2015.04.18

Pushed all code from `./R/cell5mDataUpdate.2015.04.R` to MXDs.

## 2015.02.26

Pushed new rasters to MXDs.

## 2015.02.22

Re-started work on DHS surveys, using Joe's latest estimates.

## 2014.11.19

HC Data Services use a shared persistent Rserve session to access latest CELL5M data snapshot. Use script `./rserve/start` to initialize this session. Use `killall Rserve` to kill it, when needed. This session needs to be restarted after any CELL5M data.

## 2014.11.11

HC Data Services now using another REST framework http://hcapi.harvestchoice.org/ocpu/test/ with a its own code repo at https://github.com/harvestchoice/hcapi3.
