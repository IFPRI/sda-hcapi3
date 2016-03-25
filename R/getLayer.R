#' Subset, and/or summarize HarvestChoice 5-arc-minute spatial indicators
#'
#' Workhorse method to subset and/or aggregate HarvestChoice layers.
#' This method also aggregates classified variables by continuous variables.\\
#' e.g. \code{getLayer(var="AEZ16_CLAS", by="bmi")}.
#' It does so by returning the dominant class of a classified variable within each \code{by}
#' class, and by automatically classifying any continuous variable passed to \code{by}
#' using default value breaks as specified in the variable metadata.
#' The dominant class of a variable \code{x} is defined as \code{\link{dominant}(x)}.
#' Layers may also be summarized over a spatial area (passed as an integer array of CELL5M IDs).
#' Note that calling \code{getLayer(...)} is equivalent to using the convenience method
#' \code{hcapi(...)} with the same arguments.
#'
#' @param var character array of variable names (all types are accepted)
#' @param iso3 optional array of country or regional codes to filter by (3-letter code)
#' @param by optional character array of variables to group by (all types are accepted)
#' @param ids optional gridcell ids to filter by (if collapse=F) or summarize by (if collapse=T)
#' @param collapse if TRUE collapses over \code{by} variables. If FALSE always return
#' all pixel values (useful for plotting and to convert to spatial formats).
#' @param as.class one of "data.table" (default) or "list". By default returns a simple data.table.
#' If \code{as.class="list"} returns a well-constructed list with variable metadata
#' @return a data.table (or json array) of \code{var} indicators aggregated by \code{by} domains
#' @seealso \link{hcapi} \link{getLayerWKT}
#' @examples
#' # Mean body mass index and maize yield across districts in Tanzania
#' x <- hcapi(c("bmi", "maiz_y"), iso3="TZA", by=c("ADM1_NAME_ALT", "ADM2_NAME_ALT"))
#' head(x)
#'
#' # Plot results for Mara province
#' lattice::barchart(ADM2_NAME_ALT~bmi, x[ADM1_NAME_ALT=="Mara"], col="grey90")
#'
#'
#' # Mean BMI and cassava yield across districts in Tanzania in GeoTIFF
#' x <- hcapi("bmi", iso3="TZA", format="tif")
#' head(x)
#'
#' # Plot the `bmi` series
#' plot(raster(x[2]))
#'
#' # Equivalent request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi \
#' # -d '{"var":["bmi", "maiz_y"], "iso3":"TZA", "format":"tif"}' \
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
#' # banana harvested area over Ivory Coast's provinces across elevation zones
#' x <- hcapi(c("AEZ8_CLAS", "bana_h"), iso3="CIV", by=c("ADM1_NAME_ALT", "ELEVATION"))
#' head(x)
#'
#' # An equivalent request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi/json \
#' # -d '{"var":["AEZ8_CLAS","bana_h"], "iso3":"CIV", "by":["ADM1_NAME_ALT","ELEVATION"]}' \
#' # -X POST -H 'Content-Type:application/json'
#'
#' @export
getLayer <- function(var, iso3="SSA", by=NULL, ids=NULL, collapse=TRUE, as.class="data.table") {

  setkey(vi, varCode)
  # If pixel ids are passed ignore any country filter
  if (length(ids)>0) iso3 <- "SSA"
  # If "SSA" in iso3 then limit to SSA
  if ("SSA" %in% iso3) iso3 <- "SSA"

  # Validate list of variables
  vars <- c(var, by)
  vars <- vi[vars][is.na(varTitle), list(varCode, varTitle)][, varCode]
  if (length(vars)>0) return(
    cat("Variable", paste0(vars, collapse=", "),
      "was not found in HarvestChoice data catalog.",
      "Use describe() or category() to query HarvestChoice metadata."))

  if (length(by)>0 | (length(ids)>0 & collapse==T)) {
    # Construct generic aggregation formula
    setkey(vi, varCode)
    agg <- vi[var][, aggFunR]
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
      if (iso3!="SSA") bynum=paste("ISO3, ADM0_NAME, ", bynum)
    }

    # Put entire query string together
    data <- paste0("dt",
      if (length(ids)>0) paste0("[CELL5M %in% c(", paste0(ids, collapse=","), ")]"),
      if (iso3!="SSA") paste0("[ISO3 %in% c('", paste0(iso3, collapse="','"), "')]"),
      if (!is.na(fltr)) paste0("[", fltr, "]"), "[, .(", agg, ")",
      if (!is.na(bynum)) ", keyby=.(", bynum, ")", "]")

    # Eval in Rserve (through socket instead of DB connection)
    # Uncomment to connect from remote host
    # rc <- RS.connect(getOption("hcapi3.host"), getOption("hcapi3.port"), proxy.wait=F)
    rc <- RS.connect(port=getOption("hcapi3.port"), proxy.wait=F)
    eval(parse(text=paste0("data <- RS.eval(rc, ", data, ")")))
    RS.close(rc)

  } else {
    # No aggregation, return pixel values
    # Don't duplicate any variable
    vars <- c(g, var, by)
    vars <- unique(vars)
    vars <- vars[!is.na(vars)]

    # Put query string together
    data <- paste0("dt",
      if (length(ids)>0) paste0("[CELL5M %in% c(", paste0(ids, collapse=","), ")]"),
      if (iso3!="SSA") paste0("[ISO3 %in% c('", paste0(iso3, collapse="','"), "')]"),
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
    # Return json with metadata
    d.names <- vi[names(data)][, list(
      ColumnCode=varCode,
      ColumnName=varLabel,
      ColumnUnit=unit,
      ColumnDesc=varDesc,
      ColumnSource=sources)]
    data <- list(ColumnList=d.names, ValueList=as.matrix(data))
  }

  return(data)
}


#' Subset, and/or aggregate HarvestChoice 5-arc-minute layers (SQLite version, now deprecated)
#'
#' Workhorse method to subset and/or aggregate HarvestChoice layers.
#' This method also aggregates classified variables by continuous variables.
#' e.g. \code{getLayer(var=c("whea_h", "AEZ16_CLAS"), by=c("ADM2_NAME_ALT", "bmi"))}.
#' It does so by returning the dominant class of a classified variable within each \code{by}
#' class, and by automatically classifying any continuous variable passed to \code{by}
#' using value breaks specified as part of each variable metadata.
#' The formula used to aggregate classified variables by choosing the dominant class is
#' \code{names(sort(table({varCode}), decreasing=T)[1])}. This formula computes the
#' frequency of each class, ranks them by decreasing frequency, and retains the top one.
#' Layers can also be summarized over a spatial area (passed as WKT point, multipoints, or polygons).
#'
#' @param var character array of variable names (all types are accepted)
#' @param iso3 optional country or regional filter (3-letter code)
#' @param by optional character array of variables to group by (all types are accepted)
#' @param wkt optional WKT representation of a spatial object (points or polygons) to summarize over
#' @param collapse if FALSE always return all pixel values (useful for plotting and spatial formats)
#' @return a data.table of \code{var} indicators aggregated by \code{by}
#' @export
getLayerSQL <- function(var, iso3="SSA", by=NULL, wkt=NULL, collapse=TRUE) {

  setkey(vi, varCode)
  hc.conn <- dbConnect(SQLite(), dbname=getOption("hcdata.path"), flags=SQLITE_RO)

  if (length(wkt)>0) {
    # Convert WKT to CELL5M IDs, void any iso3 filter
    wkt <- getPixelID(wkt)
    if(length(wkt)>0) iso3 <- "SSA"
    if(class(wkt)=="SpatialPoints") collapse <- FALSE
  }

  if (length(by)>0) {
    # Aggregate. Make sure to also retrieve all dependent variable(s)
    vars <- unlist(strsplit(vi[var][, aggCodes], "|", fixed=T))
    vars <- unique(c(g, var, by, vars))
    data <- dbGetQuery(hc.conn, paste0("SELECT " , paste0(vars, collapse=", "), " FROM dt"))
    data <- data.table(data)

    # If `by` include continuous variables, then auto-classify before grouping
    # Beware that aggregation formulas typically require additional variables from `dt`
    bynum <- vi[by][!type=="class", varCode]

    if (length(bynum)>0) {
      # Classify using `classBreaks`
      byclass <- sapply(bynum, function(i) {
        b <- as.integer(unlist(strsplit(vi[i][, classBreaks], "|", fixed=T)))
        paste0(i, "=cut(", i, ", c(", paste0(b, collapse=", "), "), ordered_result=T)")
      })

      by <- c(vi[by][type=="class", varCode], byclass)
      by <- by[!is.na(by)]
      by <- paste(by, collapse=", ")
    }

    # Construct generic aggregation formula
    setkey(vi, varCode)
    agg <- vi[var][, aggFunR]
    agg <- paste(var, agg, sep="=")
    agg <- paste(agg, collapse=", ")

    if (collapse==F) {
      # Aggregate but don't collapse (for plotting and raster formats)
      agg <- paste0("CELL5M, X, Y, ",  paste0(agg, collapse=", "))
    }

    data <- eval(parse(text=paste0("data",
      if(length(wkt)>0) "[CELL5M %in% wkt]",
      if(iso3!="SSA") "[ISO3==iso3]",
      "[order(", by, ", na.last=T)",
      ", list(", agg, "), by=list(", by, ")]")))

  } else {
    # No aggregation. Don't duplicate variables
    vars <- c(g, var, by)
    vars <- unique(vars)
    vars <- vars[!is.na(vars)]
    data <- dbGetQuery(hc.conn, paste0("SELECT " , paste0(vars, collapse=", "), " FROM dt"))
    data <- data.table(data)
    data <- data[!is.na(X)]
    setkey(data, ADM0_NAME, ADM1_NAME_ALT, ADM2_NAME_ALT)
    if (length(wkt)>0) data <- data[CELL5M %in% wkt]
    if (iso3!="SSA") data <- data[ISO3==iso3]
  }

  # Rounding (ugly but fast)
  var <- names(data)[sapply(data, is.numeric)]
  for(i in var) eval(parse(text=paste0("data[, ", i, " := round(", i, ", ", vi[i][, dec], ")]")))
  dbDisconnect(hc.conn)
  return(data)
}
