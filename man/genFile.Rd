% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genFile.R
\name{genFile}
\alias{genFile}
\title{Download HarvestChoice indicators in raster and/or tabular formats}
\usage{
genFile(var, iso3 = "SSA", by = NULL, format = c("csv", "geojson", "tif",
  "dta", "asc", "rds", "grd"), dir = ".", ...)
}
\arguments{
\item{var}{character array of indicator codes, passed to \code{\link{getLayer}}}

\item{iso3}{character array of ISO3 country or region codes, passed to \code{\link{getLayer}}}

\item{by}{character array of indicator codes to summarize by, passed to \code{\link{getLayer}}}

\item{format}{output format, one of "csv", "json", "tif", "dta", "asc", "grd", "rds".}

\item{dir}{output directory, default to current working directory}

\item{...}{any other optional argument passed to \code{\link{getLayer}},
e.g. \code{as.class}, \code{collapse}.}
}
\value{
character, array of generated file names included in the data package
}
\description{
Package results from \code{\link{getLayer}} into tabular or spatial raster format.
Also includes README and Tabular Data Package specifications.
Currently supported export formats include CSV (csv), STATA (dta), GeoJSON (geojson),
GeoTIFF (tif), R raster (grd), RData (rda), ESRI ASCII raster (asc), and netCDF (nc).
Calling \code{genFile(var="bmi", iso3="TZA", format="dta")} is equivalent to calling
the convenience method \code{\link{hcapi}(var="bmi", iso3="TZA", format="dta")}.
}
\details{
API call: total wheat harvested area across 16 agro-ecological zones in Nigeria and
Ethiopia in STATA format

\code{$ curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/genFile \
 -d '{"var" : "whea_h", "iso3" : ["NGA", "ETH"], "by" : "AEZ16_CLAS", "format" : "dta"}' \
 -X POST -H 'Content-Type:application/json'

/ocpu/tmp/x0e654538b7/R/.val
/ocpu/tmp/x0e654538b7/stdout
/ocpu/tmp/x0e654538b7/warnings
/ocpu/tmp/x0e654538b7/source
/ocpu/tmp/x0e654538b7/console
/ocpu/tmp/x0e654538b7/info
/ocpu/tmp/x0e654538b7/files/DESCRIPTION
/ocpu/tmp/x0e654538b7/files/README
/ocpu/tmp/x0e654538b7/files/whea_h-AEZ16_CLAS-NGA.dta
}

GET all generated files in a ZIP archive

\code{$ wget http://hcapi.harvestchoice.org/ocpu/tmp/x0e654538b7/zip
}

API call: sorghum production in Nigeria in ESRI ASCII raster format

\code{$ curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/genFile \
 -d '{"var" : "sorg_p", "format" : "asc"}' \
 -X POST -H "Content-Type:application/json"
--
/ocpu/tmp/x02a7a044c7/R/.val
/ocpu/tmp/x02a7a044c7/stdout
/ocpu/tmp/x02a7a044c7/warnings
/ocpu/tmp/x02a7a044c7/source
/ocpu/tmp/x02a7a044c7/console
/ocpu/tmp/x02a7a044c7/info
/ocpu/tmp/x02a7a044c7/files/DESCRIPTION
/ocpu/tmp/x02a7a044c7/files/README
/ocpu/tmp/x02a7a044c7/files/sorg_p--SSA.asc
/ocpu/tmp/x02a7a044c7/files/sorg_p--SSA.asc.aux.xml
/ocpu/tmp/x02a7a044c7/files/sorg_p--SSA.prj
}

GET all generated files in a ZIP archive

\code{$ wget http://hcapi.harvestchoice.org/ocpu/tmp/x02a7a044c7/zip
}
}
\examples{
# Total wheat harvested area across 16 agro-ecological zones in Nigeria and Ethiopia
# in STATA format
x <- genFile("whea_h", iso3=c("NGA", "ETH"), by="AEZ16_CLAS", format="dta")

# Load generated STATA file
require(foreign)
x <- read.dta(x[1])

# Plot histogram with full layer title
hist(x$whea_h, xlab=vi["whea_h", varLabel])

# Sorghum production in Nigeria in ESRI ASCII raster format
x <- genFile("sorg_p", iso3="NGA", format="asc")
x

# Load and plot generated raster
require(raster)
x <- raster(x[1])
plot(x, main=vi["sorg_p", varLabel])
cellStats(x, "mean")

}
\seealso{
\link{datapackage} to generate associated metadata records
}
