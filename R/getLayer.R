#' Query, subset, and/or summarize HarvestChoice indicators
#'
#' Workhorse method to query, subset and/or aggregate HarvestChoice layers.
#' This method may also be used to summarize classified variables along continuous
#' variables, e.g. \code{\link{getLayer}(var="AEZ16_CLAS", by="bmi")}.
#' It does so by returning the dominant class of a classified variable within each \code{by}
#' class, and by splitting any continuous variable passed to \code{by} using default value
#' breaks. The dominant class of a variable \code{var} is defined as \code{\link{dominant}(var)}.
#' Variables may also be summarized over custom areas (passed as an integer array of
#' gridcell IDs). Note that calling \code{\link{getLayer}} is equivalent to using the
#' convenience method \code{\link{hcapi}} with the same arguments.
#'
#' API call: mean body mass index and maize yield across districts in Tanzania
#'
#' \code{$ curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi/json \
#'  -d '{"var":["AEZ8_CLAS","bana_h"], "iso3":"CIV", "by":["ADM1_NAME_ALT","ELEVATION"]}' \
#'  -X POST -H 'Content-Type:application/json'
#' }
#'
#' @param var character array of variable names (all types are accepted). Use e.g.
#' \code{\link{category}("poverty")} to search for valid variable codes).
#' @param iso3 optional array of 3-letter country or regional code(s) to filter by.
#' Use \code{iso} to view all available codes.
#' @param by optional character array of variables to summarize by (all types are accepted)
#' @param ids optional array of gridcell ids to filter by (if \code{collapse=FALSE}) or
#' summarize by (if collapse=TRUE).
#' @param collapse if TRUE collapses over \code{by} variables. If FALSE always return
#' all pixel values (useful for plotting and to convert to spatial formats).
#' @param as.class one of "data.table" (default) or "list". By default returns a simple
#' data.table. If \code{as.class="list"} returns a list with variable metadata.
#'
#' @return a data.table (or json array) of \code{var} indicators aggregated by
#' \code{by} domains
#' @seealso \link{hcapi} and \link{getLayerWKT}
#' @examples
#' # Mean body mass index and maize yield across districts in Tanzania
#' x <- getLayer(c("bmi", "maiz_y"), iso3="TZA", by=c("ADM1_NAME_ALT", "ADM2_NAME_ALT"))
#' x
#'
#' # Plot results for Mara province
#' require(lattice)
#' barchart(ADM2_NAME_ALT~bmi, x[ADM1_NAME_ALT=="Mara"], col="grey90")
#'
#' # The method may be expanded to summarize classified (discrete) variables by continuous
#' # variables. For example the call below returns the dominant agro-ecological zone and
#' # banana harvested area over Ivory Coast's provinces and elevation zones
#' x <- hcapi(c("AEZ8_CLAS", "bana_h"), iso3="CIV", by=c("ADM1_NAME_ALT", "ELEVATION"))
#' x
#'
#' @export
getLayer <- function(var, iso3="SSA", by=NULL, ids=NULL, collapse=TRUE, as.class="data.table") {

  # Validate list of variables
  bad <- vi[c(var, by)][is.na(varTitle), .(varCode, varTitle)][, varCode]
  if (length(bad)>0) stop("Variable '", paste0(bad, collapse=", "),
      "' is not a valid indicator code.
    Use category() to query HarvestChoice metadata.")

  iso3 <- match.arg(iso3, iso, several.ok=TRUE)
  as.class <- match.arg(as.class, c("data.table", "list"))

  # If pixel ids are passed ignore any country filter
  if (length(ids)>0) iso3 <- "SSA"
  # If "SSA" in iso3 then limit to SSA
  if ("SSA" %in% iso3) iso3 <- "SSA"

  if (length(by)>0 | (length(ids)>0 & collapse==T)) {
    # Construct generic aggregation formula
    setkey(vi, varCode)
    agg <- vi[var, aggFunR]
    agg <- paste(var, agg, sep="=")
    agg <- paste(agg, collapse=", ")

    if (collapse==F) {
      # Aggregate but don't collapse (for plotting and raster formats)
      agg <- paste0("CELL5M, X, Y, ",  paste0(agg, collapse=", "))
    }

    # Construct `by` statement
    bynum <- as.character(NA)
    fltr <- as.character(NA)

    if (length(by)>0) { switch(class(by),

      `character` = {
        # Simple array of variable codes
        # If `by` include continuous variables, then auto-classify before grouping
        bynum <- vi[by][!type=="class", varCode]
        byclass <- setdiff(by, bynum)

        if (length(bynum)>0) {
          # Classify continuous `by` variables using `classBreaks`
          bynum <- sapply(bynum, function(i) {
            b <- as.integer(unlist(strsplit(vi[i][, classBreaks], "|", fixed=T)))
            paste0(i, "=cut(", i, ", c(", paste(b, collapse=", "), "),
              dig.lab=", vi[i][, dec], ", ordered_result=T)")
          })
        }

        bynum <- c(byclass, bynum)
        bynum <- bynum[!is.na(bynum)]
        bynum <- paste(bynum, collapse=", ")
      },

      `list` = {
        # Complex list with custom breaks and/or filters
        # e.g. list(AEZ5_CLAS=c("abc", "xyz"), TT_20K=c(2, 5))
        # Filter
        bynum <- vi[names(by)][!type=="class", varCode]
        fltr <- setdiff(names(by), bynum)
        fltr <- sapply(fltr, function(i) paste0(i,
          " %in% c('", paste0(by[[i]], collapse="', '"), "')"))
        fltr <- paste0(fltr, collapse=" & ")

        # Classify
        bynum <- sapply(bynum, function(i) paste0(i,
          "=cut(", i, ", c(", paste0(by[[i]], collapse=", "), "),
          dig.lab=", vi[i][, dec], ", ordered_result=T)"))
        bynum <- paste(bynum, collapse=", ")
        by <- names(by)
      })

      # If a country is selected add country name
      if (!"SSA" %in% iso3) bynum=paste("ISO3, ADM0_NAME, ", bynum)
    }

    # Put entire query string together
    data <- paste0("dt",
      if (length(ids)>0) paste0("[CELL5M %in% c(", paste0(ids, collapse=","), ")]"),
      if (!"SSA" %in% iso3) paste0("[ISO3 %in% c('", paste0(iso3, collapse="','"), "')]"),
      if (!is.na(fltr)) paste0("[", fltr, "]"), "[, .(", agg, ")",
      if (!is.na(bynum)) ", keyby=.(", bynum, ")", "]")

    # Eval in Rserve (through socket instead of DB connection)
    rc <- RS.connect(port=getOption("hcapi3.port"), proxy.wait=F)
    eval(parse(text=paste0("data <- RS.eval(rc, ", data, ")")))
    RS.close(rc)

  } else {
    # No aggregation, return pixel values
    vars <- c(g, var, by)
    # Don't duplicate any variable
    vars <- unique(vars)
    vars <- vars[!is.na(vars)]

    # Put query string together
    data <- paste0("dt",
      if (length(ids)>0) paste0("[CELL5M %in% c(", paste0(ids, collapse=","), ")]"),
      if (!"SSA" %in% iso3) paste0("[ISO3 %in% c('", paste0(iso3, collapse="','"), "')]"),
      "[, .(", paste0(vars, collapse=", "), ")]")

    # Eval in Rserve
    rc <- RS.connect(getOption("hcapi3.host"), getOption("hcapi3.port"), proxy.wait=F)
    eval(parse(text=paste0("data <- RS.eval(rc, ", data, ")")))
    RS.close(rc)
    setkey(data, ADM0_NAME, ADM1_NAME_ALT, ADM2_NAME_ALT)
  }

  # Rounding (ugly but fast)
  var <- names(data)[sapply(data, is.numeric)]
  var <- setdiff(var, c("X", "Y"))
  for(i in var) eval(parse(text=paste0("data[, ", i, " := round(", i, ", ", vi[i][, dec], ")]")))

  if (as.class=="list") {
    # Also return metadata (mimick earlier API behavior)
    data <- list(meta=vi[names(data)], data=data)
  }

  return(data)
}
