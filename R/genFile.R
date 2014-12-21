
#' Convert CELL5M layers to multiple formats and archive for download
#'
#' Package any result from \code{getLayer()} into the user-specified data format.
#' Also creates an accompanying README file with metadata and citation details.
#'
#' @param var character array of variable codes passed to getLayer(var, ...)
#' @param iso3 passed to getLayer()
#' @param by passed to getLayer()
#' @param format output format c("json", csv", "geojson", "tif", "shp", "dta", "asc", "rds", "rdata")
#' @param ... any other optional argument passed to getLayer(), e.g. \code{geom}, \code{collapse}.
#' @return character, path to generated ZIP file
#' @export
genFile <- function(var, iso3="SSA", by=NULL,
  format=c("csv", "geojson", "tif", "shp", "dta", "asc", "rds"), ...) {

  setkey(vi, varCode)
  # Construct temporary data file name
  fPath <- paste(paste(var[1], by[1], iso3[1], sep="-"), format[1], sep=".")

  if ( format %in% c("asc", "tif", "shp", "geojson") ) {
    # Call getLayer() and don't collapse for spatial formats
    d <- getLayer(var, iso3, by, collapse=F, ...)
  } else {
    d <- getLayer(var, iso3, by, ...)
  }

  if ( format %in% c("asc", "tif") ) {
    # Process only the first layer (not all raster formats support multibands)
    var <- var[1]
    cl <- as.character(unlist(vi[var][, strsplit(classLabels, "|", fixed=T)]))
    cc <- as.character(unlist(vi[var][, strsplit(classColors, "|", fixed=T)]))
    ct <- "Float32"

    if (vi[var][, type] == "class")  {
      # If categorical raster, then convert to 0-based integer and add labels
      d[[var]] <- as.integer(factor(d[[var]], levels=cl, ordered=T))-1L
      ct <- "Byte"
    }

    d <- SpatialPixelsDataFrame(d[, list(X, Y)], d,
      tolerance=0.00360015, proj4string=CRS("+init=epsg:4326"))
  }

  switch(format,

    # GeoTIFF
    tif = {
      writeGDAL(d[, var], fPath, driver="GTiff",
        mvFlag=-9999, type=ct, setStatistics=T,
        catNames=list(cl), colorTables=list(cc),
        options=c("INTERLEAVE=BAND", "TFW=YES", "ESRI_XML_PAM=YES")) },

    # ESRI ASCII
    asc = {
      writeGDAL(d[, var], fPath, driver="AAIGrid",
        mvFlag=-9999, type=ct, setStatistics=T,
        catNames=list(cl), colorTables=list(cc),
        options=c("INTERLEAVE=BAND", "TFW=YES", "ESRI_XML_PAM=YES")) },

    # ESRI Shapefile
    shp = {
      d <- SpatialPointsDataFrame(d[, list(X, Y)], d,
        proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
      writeOGR(d, dirname(fPath), basename(fPath), driver="ESRI Shapefile") },

    # GeoJSON
    geojson = {
      d <- SpatialPointsDataFrame(d[, list(X, Y)], d,
        proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
      writeOGR(d, fPath, var[1], driver="GeoJSON") },

    # Stata (note var.labels still don't seem to work)
    dta = {
      setattr(d, "var.labels", vi[names(d)][, paste0(varLabel, " (", unit, ")")])
      setattr(d, "datalabel", "Produced by HarvestChoice/IFPRI at http://hcapi.harvestchoice.org/. Contact <info@harvestchoice.org>.")
      setattr(d, "time.stamp", Sys.Date())
      write.dta(d, fPath, version=10L) },

    # RDS
    rds = {
      attr(d, "var.labels") <- vi[names(d)][, varLabel]
      attr(d, "datalabel") <- "Produced by HarvestChoice/IFPRI at http://hcapi.harvestchoice.org/. Contact <info@harvestchoice.org>."
      attr(d, "time.stamp") <- as.character(as.Date(Sys.Date()))
      saveRDS(d, file=fPath, compress=T) },

    # CSV (default)
    { write.csv(d, fPath, row.names=F, na="") }
  )

  f <- list.files(dirname(fPath), paste0(basename(fPath), ".*"), full.names=T)
  f <- c(f, genReadme(names(d)))
  fPath <- paste(fPath, "zip", sep=".")
  zip(fPath, f, flags="-9Xjm", zip="zip")
  return(fPath)
}
