#' Subset, and/or aggregate HarvestChoice 5-arc-minute layers
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
#'
#' @param var character array of variable names (all types are accepted)
#' @param iso3 optional country or regional filter (3-letter code)
#' @param by optional character array of variables to group by (all types are accepted)
#' @param collapse if FALSE always return all pixel values (useful for plotting and spatial formats)
#' @return a data.table of \code{var} indicators aggregated by \code{by}
#' @export
getLayer <- function(var, iso3="SSA", by=NULL, collapse=TRUE) {

  setkey(vi, varCode)

  hc.conn <- dbConnect(SQLite(), dbname=getOption("hcdata.path"), flags=SQLITE_RO)

  if (length(by)>0) {
    # Aggregate. Make sure to retrieve all dependent variable(s)
    vars <- unlist(strsplit(vi[var][, aggCodes], "|", fixed=T))
    vars <- unique(c(var, by, vars))
    data <- dbGetQuery(hc.conn, paste0("SELECT " , paste0(vars, collapse=", "), " FROM `dt`"))
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

    data <- eval(parse(text=paste0(
      ifelse(iso3=="SSA", "data", "data[ISO3==iso3]"),
      "[order(", by, ", na.last=T)",
      ", list(", agg, "), by=list(", by, ")]")))

  } else {
    # No aggregation. Don't duplicate variables
    vars <- c(g, var, by)
    vars <- unique(vars)
    vars <- vars[!is.na(vars)]
    data <- dbGetQuery(hc.conn, paste0("SELECT " , paste0(vars, collapse=", "), " FROM `dt`"))
    data <- data.table(data)
    data <- data[!is.na(X)]
    setkey(data, ADM0_NAME, ADM1_NAME_ALT, ADM2_NAME_ALT)
    if (iso3 != "SSA") data <- data[ISO3==iso3]
  }

  # Rounding (ugly but fast)
  var <- names(data)[sapply(data, is.numeric)]
  for(i in var) eval(parse(text=paste0("data[, ", i, " := round(", i, ", ", vi[i][, dec], ")]")))
  dbDisconnect(hc.conn)
  return(data)
}


#' Convert CELL5M layers to multiple formats and package for download
#'
#' @param var character array of CELL5M variable names
#' @param iso3 optional country or regional filter (3-letter code)
#' @param by optional character array of CELL5M variable to group by
#' @param format output format c("csv", "geojson", "tif", "dta", "asc", "rds", "RData")
#' @param ... any argument passed to getLayer()
#' @return character, path to generated data file
#' @export
genFile <- function(var, iso3="SSA", by=NULL,
  format=c("csv", "geojson", "tif", "shp", "dta", "asc", "rds", "rdata"), ...) {

  setkey(vi, varCode)
  fPath <- paste0(var[1], "-", paste0(by, collapse="-"), iso3, format)

  if (format %in% c("asc", "tif", "rdata")) {
    # Raster formats take more work
    # Process only the first layer (not all formats support multibands)
    require(raster)

    var <- var[1]
    d <- getLayer(var, iso3, by, collapse=F)
    cl <- as.character(unlist(vi[var][, strsplit(classLabels, "|", fixed=T)]))
    cc <- as.character(unlist(vi[var][, strsplit(classColors, "|", fixed=T)]))
    ct <- "Float32"

    if (vi[var][, type] == "class")  {
      # If categorical raster, then convert to 0-based integer and add labels
      d[[var]] <- as.integer(factor(d[[var]], levels=cl, ordered=T))-1L
      ct <- "Int16"
    }

    # Convert to spatial
    d <- SpatialPixelsDataFrame(d[, list(X, Y)], d,
      tolerance=0.00360015, proj4string=CRS("+init=epsg:4326"))
  }

  switch(format,
    # RData raster
    rdata = {
      fPath <- paste0(fPath, ".RData")
      save(raster(d, layer=var), file=fPath, compress=T) },

    # GeoTIFF
    tif = {
      fPath <- paste0(fPath, ".tif")
      writeGDAL(d[, var], fPath, driver="GTiff",
        mvFlag=-9999, type=ct, setStatistics=T,
        #catNames=list(cl), colorTables=list(cc),
        options=c("INTERLEAVE=BAND", "TFW=YES", "ESRI_XML_PAM=YES")) },

    # ESRI ASCII
    asc = {
      fPath <- paste0(fPath, ".asc")
      writeGDAL(d[, var], fPath, driver="AAIGrid",
        mvFlag=-9999, type=ct, setStatistics=T,
        #catNames=list(cl), colorTables=list(cc),
        options=c("INTERLEAVE=BAND", "TFW=YES", "ESRI_XML_PAM=YES")) },

    # ESRI Shapefile
    shp = {
      d <- getLayer(var, iso3, by, collapse=F)
      d <- SpatialPointsDataFrame(d[, list(X, Y)], d,
        proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
      writeOGR(d, dirname(fPath), basename(fPath), driver="ESRI Shapefile") },

    # GeoJSON
    geojson = {
      fPath <- paste0(fPath, ".geojson")
      d <- getLayer(var, iso3, by, collapse=F)
      d <- SpatialPointsDataFrame(d[, list(X, Y)], d,
        proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
      writeOGR(d, fPath, var[1], driver="GeoJSON") },

    # Stata (note var.labels still don't seem to work)
    dta = {
      fPath <- paste0(fPath, ".dta")
      d <- getLayer(var, iso3, by, ...)
      setattr(d, "var.labels", vi[names(d)][, paste0(varLabel, " (", unit, ")")])
      setattr(d, "datalabel", "Produced by HarvestChoice/IFPRI at http://api.harvestchoice.org/. Contact <info@harvestchoice.org>. Written by R.")
      setattr(d, "time.stamp", Sys.Date())
      write.dta(d, fPath, version=10L) },

    # RDS
    rds = {
      fPath <- paste0(fPath, ".rds")
      d <- getLayer(var, iso3, by, ...)
      attr(d, "var.labels") <- vi[names(d)][, varLabel]
      attr(d, "datalabel") <- "Produced by HarvestChoice/IFPRI at http://api.harvestchoice.org/. Contact <info@harvestchoice.org>. Written by R."
      attr(d, "time.stamp") <- as.character(as.Date(Sys.Date()))
      saveRDS(d, file=fPath, compress=T) },

    # CSV (default)
    { fPath <- paste0(fPath, ".csv")
      d <- getLayer(var, iso3, by, ...)
      write.csv(d, fPath, row.names=F, na="") }
  )

  #f <- list.files(dirname(fPath), paste0(basename(fPath), ".*"), full.names=T)
  #f <- c(f, genReadme(names(d)))
  #zip(paste0(fPath, ".zip"), f, flags="-9Xjm", zip="zip")
  genReadme(names(d))
  return(fPath)
}


