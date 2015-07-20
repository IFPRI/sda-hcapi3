#' Subset, summarize, and download HarvestChoice 5-arc-minute spatial indicators
#'
#' Main method to subset and/or aggregate HarvestChoice layers.
#' This method also aggregates classified variables by continuous variables.\\
#' e.g. \code{getLayer(var=c("whea_h", "AEZ16_CLAS"), by=c("ADM2_NAME_ALT", "bmi"))}.
#' It does so by returning the dominant class of a classified variable within each \code{by}
#' class, and by automatically classifying any continuous variable passed to \code{by}
#' using default value breaks as specified in the variable metadata.
#' The formula used to aggregate classified variables by choosing the dominant class is
#' \code{names(sort(table({varCode}), decreasing=T)[1])}. This formula computes the
#' frequency of each class, ranks them by decreasing frequency, and retains the top one.
#' Layers can also be summarized over a spatial area (passed as an integer array of CELL5M ids).
#' Use \code{format} argument to control the output format for the spatial layer (see below).
#'
#' @param var character array of variable codes, passed to \code{\link{getLayer}}
#' @param iso3 character array of ISO3 country or region codes, passed to \code{\link{getLayer}}
#' @param by character array of variable codes to summarize by, passed to \code{\link{getLayer}}
#' @param format output format, one of "default" (R data.table, default), "csv", "json", tif",
#' "dta", "asc", "grd", "rds", else "png" to plot the rasters, or "stats" to plot
#' histogram and univariate statistics.
#' @param wkt WKT representation of a spatial object (points, multipoints, or polygons)
#' @param ... other optional arguments passed to \code{\link{getLayer}},
#' e.g. \code{collapse}, \code{as.class}
#' @return a data.table (or other formats) of \code{var} indicators aggregated by \code{by} domains
#' @examples
#' # Mean BMI and cassava yield across districts in Tanzania
#' x <- hcapi(c("bmi", "cass_y"), iso3="TZA", by=c("ADM1_NAME_ALT", "ADM2_NAME_ALT"))
#' x
#'
#' # Plot results for Mara province
#' require(lattice)
#' barchart(ADM2_NAME_ALT~bmi, data=x[ADM1_NAME_ALT=="Mara"])
#'
#' # Mean BMI and cassava yield across districts in Tanzania in GeoTIFF
#' x <- hcapi("bmi", iso3="TZA", format="tif")
#' x
#'
#' # Load the generated TIF raster (one band only)
#' require(raster)
#' x <- raster(x[1])
#'
#' # Plot the `bmi` series
#' plot(x)
#'
#' # Equivalent request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi \
#' # -d '{"var":"bmi", "iso3":"TZA", "format":"tif"}' \
#' # -X POST -H 'Content-Type:application/json'
#'
#' # /ocpu/tmp/x0bc1ac9bdf/R/.val
#' # /ocpu/tmp/x0bc1ac9bdf/stdout
#' # /ocpu/tmp/x0bc1ac9bdf/warnings
#' # /ocpu/tmp/x0bc1ac9bdf/source
#' # /ocpu/tmp/x0bc1ac9bdf/console
#' # /ocpu/tmp/x0bc1ac9bdf/info
#' # /ocpu/tmp/x0bc1ac9bdf/files/bmi--TZA.tfw
#' # /ocpu/tmp/x0bc1ac9bdf/files/bmi--TZA.tif
#' # /ocpu/tmp/x0bc1ac9bdf/files/bmi--TZA.tif.aux.xml
#' # /ocpu/tmp/x0bc1ac9bdf/files/DESCRIPTION
#' # /ocpu/tmp/x0bc1ac9bdf/files/README
#'
#' # Use wget (at the command line) to download all generated files in a ZIP archive
#' # wget http://hcapi.harvestchoice.org/ocpu/tmp/x0bc1ac9bdf/zip
#'
#' # The method may be expanded to summarize classified (discrete) variables by continuous
#' # variables. For example the call below returns the dominant agro-ecological zone and
#' # average stunting in children under 5 over Ivory Coast's provinces by elevation class
#' hcapi(c("AEZ8_CLAS", "stunted_moderate"), iso3="CIV", by=c("ADM1_NAME_ALT", "ELEVATION"))
#'
#' # An equivalent request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi/json \
#' # -d '{"var":["AEZ8_CLAS","stunted_moderate"], "iso3":"CIV", "by":["ADM1_NAME_ALT","ELEVATION"]}' \
#' # -X POST -H 'Content-Type:application/json'
#'
#' @export
hcapi <- function(var, iso3="SSA", by=NULL, wkt=NULL, format="default", ...) {
  if (!missing(wkt)) return(getLayerWKT(var, iso3, by, wkt, ...))
  if (format=="default") return(getLayer(var, iso3, by, ...))
  if (format %in% c("png", "plot")) return(genPlot(var, iso3, ...))
  if (format=="stats") return(stats(var, iso3))
  else return(genFile(var, iso3, by, format, ...))
}

