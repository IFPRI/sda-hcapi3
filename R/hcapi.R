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
#' @param var character array of variable codes, passed to \code\link{getLayer}
#' @param iso3 character array of ISO3 country or region codes, passed to \code\link{getLayer}
#' @param by character array of variable codes to summarize by, passed to \code\link{getLayer}
#' @param format output format, one of "dt" (R data.table, default), "csv", "json", tif",
#' "dta", "asc", "grd", "rda"
#' @param wkt WKT representation of a spatial object (points, multipoints, or polygons)
#' @param ... other optional arguments passed to \code\link{getLayer},
#' e.g. \code{collapse}, \code{as.class}
#' @return a data.table (or other formats) of \code{var} indicators aggregated by \code{by} domains
#' @examples
#' # Mean BMI and cassava yield across districts in Tanzania
#' hcapi(c("bmi", "cass_y"), iso3="TZA", by=c("ADM1_NAME_ALT", "ADM2_NAME_ALT"), format="tif")
#'
#' # Equivalent request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi/zip \
#' # -d "{'var':['bmi','cass_y'], 'iso3':'TZA', 'by':['ADM1_NAME','ADM2_NAME'], 'format':'tif'}" \
#' # -X POST -H "Content-Type:application/json"
#'
#' # The method may be expanded to summarize classified (discrete) variables by continuous
#' # variables. For example the call below returns the dominant agro-ecological zone and
#' # average stunting in children under 5 over Ivory Coast's provinces by elevation class
#' hcapi(c("AEZ8_CLAS", "stunted_moderate"), iso3="CIV", by=c("ADM1_NAME_ALT", "ELEVATION"))
#'
#' # An equivalent request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi/json \
#' # -d '{"var":["AEZ8_CLAS","stunted_moderate"], "iso3":"CIV", "by":["ADM1_NAME_ALT","ELEVATION"]}' \
#' # -X POST -H "Content-Type:application/json"
#'
#' @export
hcapi <- function(var, iso3="SSA", by=NULL, wkt=NULL, format="dt", ...) {
  if (!missing(wkt)) return(getLayerWKT(var, iso3, by, wkt, ...))
  if (format=="dt") return(getLayer(var, iso3, by, ...))
  else return(genFile(var, iso3, by, format, ...))
}


