#' Query, subset, summarize, and download HarvestChoice indicators
#'
#' Wrapper method to query, subset and/or aggregate HarvestChoice layers.
#' This method may also be used to summarize classified variables along continuous
#' variables, e.g. \code{hcapi(var="AEZ16_CLAS", by="bmi")}. Here \code{AEZ16_CLAS} is
#' a classified (categorical) raster, and \code{bmi} is a continuous raster, but the
#' request is valid. The dominant class of \code{AEZ16_CLAS} is returned along intervals
#' of \code{bmi}. Default interval breaks are used but custom intervals may also be
#' defined, e.g. \code{hcapi(var="AEZ16_CLAS", by=list(bmi=c(0,5,10,15,20,25)))}.
#' The dominant class of a variable \code{var} is defined by \code{\link{dominant}(var)}.
#' Layers may also be summarized over spatial points are areas (passed as WKT representations
#' using argument \code{wkt}). Use the \code{format} argument to control the output
#' format (see examples below).
#'
#' API call: mean body mass index and cassava yield across provinces and districts of
#' Tanzania
#'
#' \code{$ curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi \
#'  -d '{"var":"cass_y", "iso3":"CIV", "format":"tif"}' \
#'  -X POST -H 'Content-Type:application/json'
#' --
#' /ocpu/tmp/x0bc1ac9bdf/R/.val
#' /ocpu/tmp/x0bc1ac9bdf/stdout
#' /ocpu/tmp/x0bc1ac9bdf/warnings
#' /ocpu/tmp/x0bc1ac9bdf/source
#' /ocpu/tmp/x0bc1ac9bdf/console
#' /ocpu/tmp/x0bc1ac9bdf/info
#' /ocpu/tmp/x0bc1ac9bdf/files/bmi--TZA.tfw
#' /ocpu/tmp/x0bc1ac9bdf/files/bmi--TZA.tif
#' /ocpu/tmp/x0bc1ac9bdf/files/bmi--TZA.tif.aux.xml
#' /ocpu/tmp/x0bc1ac9bdf/files/DESCRIPTION
#' /ocpu/tmp/x0bc1ac9bdf/files/README
#' }
#'
#' GET all generated files in a ZIP archive
#'
#' \code{$ wget http://hcapi.harvestchoice.org/ocpu/tmp/x0bc1ac9bdf/zip
#' }
#'
#' API call: the method may be expanded to summarize classified (discrete) variables
#' along continuous variables. For example the call below returns the dominant
#' agro-ecological zone and average stunting in children under 5 over Ethiopia's
#' provinces and generic elevation zones
#'
#' \code{$ curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi/json \
#'  -d '{"var":["AEZ8_CLAS","stunted_moderate"], "iso3":"ETH", "by":["ADM1_NAME_ALT","ELEVATION"]}' \
#'  -X POST -H 'Content-Type:application/json'
#' }
#'
#' @param var character array of variable codes, passed to \code{\link{getLayer}}
#' @param iso3 character array of ISO3 country or region codes, passed to \code{\link{getLayer}}
#' @param by character array of variable codes to summarize by, passed to \code{\link{getLayer}}
#' @param format output format, one of "data.table", "csv", "tif", "dta", "asc", "grd", "rds",
#' else "png" to plot the rasters, or "hist" to plot histogram and univariate statistics
#' @param wkt WKT representation of a spatial object (points, multipoints, or polygons, multipolygons)
#' @param ... other optional arguments passed to \code{\link{getLayer}}, \code{\link{genFile}},
#' or to \code{\link{genPlot}}, e.g. \code{collapse}, \code{as.class}, \code{dir}, \code{pal}.
#'
#' @return a data.table (or other formats) of \code{var} indicators summarized by \code{by} domains
#' @seealso \link{getLayer} \link{getLayerWKT} \link{genFile}
#' @examples
#' # Mean body mass index and cassava yield across provinces and districts of Tanzania
#' x <- hcapi(c("bmi", "cass_y"), iso3="TZA", by=c("ADM1_NAME_ALT", "ADM2_NAME_ALT"))
#' x
#'
#' # Plot results for Mara province
#' require(lattice)
#' barchart(ADM2_NAME_ALT~bmi, data=x[ADM1_NAME_ALT=="Mara"], col="grey90")
#'
#' # Mean cassava yield in Ivory Coast in GeoTIFF raster format
#' x <- hcapi("cass_y", iso3="CIV", format="tif")
#' x
#'
#' # Plot the generated TIF raster (one band only)
#' require(raster)
#' plot(raster(x[2]))
#'
#' # The method may be expanded to summarize classified (discrete) variables along continuous
#' # variables. For example the call below returns the dominant agro-ecological zone and
#' # average stunting in children under 5 over Ethiopia's provinces by elevation class
#' x <- hcapi(c("AEZ8_CLAS", "stunted_moderate"), iso3="ETH", by=c("ADM1_NAME_ALT", "ELEVATION"))
#' x
#'
#' # Mean harvested maize area summarized across a custom polygon
#' x <- hcapi(var="maiz_h", wkt="POLYGON((-16.35819663578485006 15.36599264077935345,
#' -15.42501860768386379 15.69472580976947462, -15.11749403024149174 14.83577785208561117,
#' -16.13550642453347805 14.68731771125136376, -16.35819663578485006 15.36599264077935345))")
#' x
#'
#' @export
hcapi <- function(var, iso3="SSA", by=NULL, wkt=NULL, format=NULL, ...) {

  # Dispatch
  if (missing(var)) {
    stop("Argument 'var' is missing. Please enter a valid indicator code.")
  } else if (!missing(wkt)) {
    return(getLayerWKT(var, iso3=iso3, by=by, wkt=wkt, ...))
  } else if (missing(format)) {
    return(getLayer(var, iso3=iso3, by=by, ...))
  } else if (format == "png") {
    return(genPlot(var, iso3=iso3, ...))
  } else if (format == "hist") {
    return(stats(var, iso3=iso3))
  } else return(genFile(var, iso3=iso3, by=by, format=format, ...))

}

