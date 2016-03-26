
#' Rank agricultural productions across geographies
#'
#' Returns top and bottom commodities in terms of aggregate production, harvested area,
#' and/or value of production.
#'
#' @param var one or several of "h", "p", or "v" for ranking by harvested area (h),
#' production (p), and/or value of production (v).
#' This argument may be expanded in the future to include additional sub-groups of indicators.
#' @param by by default returns aggregate production across all selected countries.
#' \code{by} may be specified to return aggregates across sub-zones (e.g. across provinces,
#' districts, agro-ecological zones, etc.), see examples.
#' @param ... optional arguments passed to \code{\link{hcapi}}, e.g. \code{iso3} and \code{wkt}
#' @return data.table(s) of commodities ranked from top to bottom
#' @seealso \link{hcapi}
#' @examples
#' # Rank commodities in Angola and Mozambique by harvested area and production
#' genRank(c("h", "p"), iso3=c("AGO", "MOZ"))
#'
#' # Rank commodities across Angola's agro-ecological zones by production
#' genRank("p", iso3="AGO", by="AEZ5_CLAS")
#'
#' # Equivalent request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/genRank/json \
#' # -d '{"var" : ["h", "p"], "iso3" : ["AGO", "MOZ"]}' \
#' # -X POST -H 'Content-Type:application/json'
#'
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/genRank/json \
#' # -d '{"var" : "p", "iso3" : "AGO", "by" : "AEZ5_CLAS"}' \
#' # -X POST -H 'Content-Type:application/json'
#'
#' @export
genRank <- function(var="h", by="ISO3", ...) {

  # This method relies on indicators marked as `isProduct` in the variable inventory
  var <- tolower(var)
  stopifnot(var %in% c("h", "p", "v"))

  var.h <- vi[published==T & isProduct==T & substr(varCode, 5, 6)=="_h", varCode]
  var.p <- vi[published==T & isProduct==T & substr(varCode, 5, 6)=="_p", varCode]
  var.v <- vi[published==T & isProduct==T & substr(varCode, 5, 6)=="_v", varCode]

  data <- lapply(var, function(x) {
    var <- switch(x, h=var.h, p=var.p, v=var.v)
    val <- switch(x, h="Havested Area (ha)", p="Production (mt)", v="Value of Production (intl. $)")
    # Do not support summarizing across lower admin units (yet)
    dt <- hcapi(var, ..., by=by, as.class="data.table")
    dt[, ISO3 := NULL]
    dt <- melt(dt, measure.vars=var)
    dt <- dt[!is.na(value)]
    setorderv(dt, c("ADM0_NAME", by, "value"), c(1, rep(1, length(by)), -1), na.last=T)
    setnames(dt, c("ADM0_NAME", by, "variable", "value"),
      c("Country", vi[by][, varLabel], "Commodity", val))
    levels(dt$Commodity) <- vi[levels(dt$Commodity), cat3]
    return(dt)
  })

  if(length(data)==1) data <- data[[1]]
  return(data)
}


