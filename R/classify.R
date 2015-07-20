#' Create custom domains for spatial targeting
#'
#' All HarvestChoice spatial layers come with default value breaks to create map legends
#' and to automatically convert continuous variables into classified variables (this is needed
#' when summarizing). For example, the value breaks for cassava production may be returned
#' using \code{describe("cass_p")$classBreaks}. They are
#' \code{c(0, 1074, 4260, 10051, 19920, 40085, 101843)}.
#' This method extends the classification logic to allow for any custom value break(s).
#' For example one can summarize a set of selected indicators over areas
#' of low population density, and/or areas covering a single farming system (or any
#' combination of custom domains). Note that calling \code{classify(...)} is equivalent
#' to calling the convenience method \code{hcapi(...)} with the same arguments.
#'
#' @param var character array of variable names (all types are accepted)
#' @param by named list of numeric or character arrays representing custom value
#' breaks for each variable to summarize by
#' @param ... other arguments passed to \code{\link{hcapi3:getLayer}} e.g. iso3, collapse
#'
#' @return a data.table of \code{var} indicators aggregated by custom \code{by} domains
#' @seealso \code{\link{hcapi3:similar}} for identifying areas with similar patterns
#' @examples
#' # Return average wealth index and poverty rates for areas of medium rural
#' # population density and low market access (under 5hrs travel time)
#' x <- classify(c("stunted_low","TPOV_PT200"), by=list(PD05_RUR=c(60,100,500), TT_20K=c(0,2,4,8)))
#' x
#'
#' # Plot
#' require(lattice)
#' require(latticeExtra)
#'
#' asTheEconomist(
#'   barchart(stunted_low+TPOV_PT200~PD05_RUR|TT_20K,
#'     data=x[!is.na(PD05_RUR) & !is.na(TT_20K)]))
#'
#' # Equivalent cUrl request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/classify/json \
#' # -d '{"var" : ["wealth", "TPOV_PT200"], "by" : {"PD05_RUR" : [60,100,500], "TT_20K" : [0,2,4,8]}}' \
#' # -X POST -H "Content-Type:application/json"
#'
#' # Return average rainfall in 2 Ethiopia districts for areas below 1200m elevation
#' x <- classify("pre_mean", by=list(ADM2_NAME_ALT=c("Bale", "Hadiya"), ELEVATION=c(0,600,1200)))
#' x
#'
#' # Equivalent cUrl request at the command line passing a well-formatted JSON object
#' # Note that the `/classify/json` or `/hcapi/json` endpoints are equivalent
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi/json \
#' # -d '{"var" : "pre_mean", "by" : {"ADM2_NAME_ALT" : ["Bale", "Hadidya"], "ELEVATION" : [0,600,1200]}}' \
#' # -X POST -H "Content-Type:application/json"
#'
#' @export
classify <- function(var, by, ...) {
  out <- getLayer(var=var, by=by, ids=NULL, ...)
  return(out)
}