#' Create custom domains for spatial targeting
#'
#' All HarvestChoice spatial layers come with default value breaks to create map legends
#' and to automatically convert continuous variables into classified variables (this is needed
#' when summarizing). For example, the value breaks for cassava production may be returned
#' using \code{getMeta("cass_p")}. They are \code{c(0, 1074, 4260, 10051, 19920, 40085, 101843)}.
#' This method extends the classification logic to allow for any custom value break(s).
#' For example one can summarize a set of selected indicators over areas
#' of low population density, and/or areas covering a single farming system (or any
#' combination of custom domains).
#'
#' @param var character array of variable names (all types are accepted)
#' @param by named list of numeric or character arrays representing custom value breaks for each variable to summarize by
#' @param ... other arguments passed to \code{getLayer()} (e.g. iso3, collapse)
#' @return a data.table of \code{var} indicators aggregated by custom \code{by} domains
#' @examples
#' # Return average wealth index and poverty rates for areas of medium rural
#' # population density and low market access (under 5hrs travel time)
#' reClassify(var=c("wealth","TPOV_P200"),
#'            by=list(PD05_RUR=c(60, 100), TT_20K=c(0,5)))
#'
#' @export
classify <- function(var, by, ...) {
  out <- getLayer(var=var, by=by, ids=NULL, ...)
  return(out)
}