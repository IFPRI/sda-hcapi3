
#' Convert CELL5M layers to raster and/or tabular formats and create archive for download
#'
#' Package any result from \code{getLayer()} into the user-specified tabular or raster
#' format. Also includes a README file with metadata and citation details. Currently
#' supported export formats include CSV (csv), STATA (dta), GeoJSON (json), GeoTIFF
#' (tif), R raster (grd), R Data (rda), and ESRI ASCII raster (asc).
#'
#' @param var character array of variable codes, passed to getLayer(var, ...)
#' @param iso3 character array of ISO3 country or region codes, passed to getLayer()
#' @param by character array of variable codes to summarize by, passed to getLayer()
#' @param format output format c("csv", "json", "tif", "dta", "asc", "grd", "rda")
#' @param ... any other optional argument passed to getLayer(), e.g. \code{by}, \code{collapse}.
#' @return character, path to generated ZIP file
#' @examples
#' # Total wheat harvested area across 16 agro-ecological zones in Nigeria and Ethiopia
#' # in STATA format
#' genFile("whea_h", iso3=c("NGA", "ETH"), by="AEZ16_CLAS", format="dta")
#'
#' # Sorghum production returned in ESRI ASCII raster format
#' genFile("sorg_p", format="asc")
#'
#' # Equivalent cUrl requests at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/genFile \
#' # -d '{"var" : "whea_h", "iso3" : ["NGA", "ETH"], "by" : "AEZ16_CLAS", "format" : "dta"}' \
#' # -X POST -H "Content-Type:application/json"
#'
#' # Download generated ZIP file
#' # wget http://hcapi.harvestchoice.org/ocpu/tmp/x0fe45cc2aa/files/whea_h-AEZ16_CLAS-NGA.dta.zip
#'
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/genFile \
#' # -d '{"var" : "sorg_p", "format" : "asc"}' \
#' # -X POST -H "Content-Type:application/json"
#'
#' # Download generated ZIP file
#' # wget http://hcapi.harvestchoice.org/ocpu/tmp/x0760ca93d2/files/sorg_p--SSA.asc.zip
#'
#' @export
genFile <- function(var, iso3="SSA", by=NULL,
  format=c("csv", "geojson", "tif", "dta", "asc", "rda", "grd"), ...) {

  setkey(vi, varCode)

  # Validate format
  format <- tolower(format)
  if ( format %in% c("tiff", "geotiff", "geotif") ) format <- "tif"
  if ( format %in% c("geojson", "json") ) format <- "geojson"
  if ( format %in% c("grid", "grd") ) format <- "grd"
  if ( format %in% c("rdata", "RDA", "rda", "RData", "Rdata", "rds") ) format <- "rda"

  # Construct temporary data file name
  fPath <- paste(paste(var[1], by[1], iso3[1], sep="-"), format[1], sep=".")

  if ( format %in% c("grd", "asc", "tif", "geojson") ) {
    # Call getLayer() and don't collapse for spatial formats
    d <- getLayer(var, iso3, by, collapse=F, ...)
  } else {
    d <- getLayer(var, iso3, by, ...)
  }

  if ( format %in% c("asc", "tif", "grd") ) {
    # Process only the first layer (not all raster formats support multibands)
    var <- var[1]

    cl <- as.character(unlist(vi[var][, strsplit(classLabels, "|", fixed=T)]))
    cc <- as.character(unlist(vi[var][, strsplit(classColors, "|", fixed=T)]))
    ct <- "Float32"

    if ( vi[var][, type] == "class" )  {
      # If categorical raster, then convert to 0-based integer and add labels
      d[[var]] <- as.integer(factor(d[[var]], levels=cl, ordered=T))-1L
      ct <- "Byte"
    }

    # Convert to unprojected raster
    d <- SpatialPixelsDataFrame(d[, list(X, Y)], d,
      tolerance=0.00360015, proj4string=CRS("+init=epsg:4326"))
  }

  switch(format,

    tif = {
      # GeoTIFF
      writeGDAL(d[, var], fPath, driver="GTiff",
        mvFlag=-9999, type=ct, setStatistics=T,
        catNames=list(cl), colorTables=list(cc),
        options=c("INTERLEAVE=BAND", "TFW=YES", "ESRI_XML_PAM=YES")) },

    grd = {
      # Raster grid
      writeGDAL(d[, var], fPath, driver="raster",
        mvFlag=-9999, type=ct, setStatistics=T,
        catNames=list(cl), colorTables=list(cc),
        options=c("INTERLEAVE=BAND", "TFW=YES", "ESRI_XML_PAM=YES")) },

    asc = {
      # ESRI ASCII grid
      writeGDAL(d[, var], fPath, driver="AAIGrid",
        mvFlag=-9999, type=ct, setStatistics=T,
        catNames=list(cl), colorTables=list(cc),
        options=c("INTERLEAVE=BAND", "TFW=YES", "ESRI_XML_PAM=YES")) },

    geojson = {
      # GeoJSON
      d <- SpatialPointsDataFrame(d[, list(X, Y)], d,
        proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
      writeOGR(d, fPath, var[1], driver="GeoJSON") },

    dta = {
      # Stata
      setattr(d, "var.labels", vi[names(d)][, paste0(varLabel, " (", unit, ")")])
      setattr(d, "datalabel", "Produced by HarvestChoice/IFPRI at http://hcapi.harvestchoice.org/. Contact <info@harvestchoice.org>.")
      setattr(d, "time.stamp", Sys.Date())
      write.dta(d, fPath, version=10L) },

    rda = {
      # RDA
      attr(d, "var.labels") <- vi[names(d)][, varLabel]
      attr(d, "datalabel") <- "Produced by HarvestChoice/IFPRI at http://hcapi.harvestchoice.org/. Contact <info@harvestchoice.org>."
      attr(d, "time.stamp") <- as.character(as.Date(Sys.Date()))
      save(d, file=fPath, compress=T) },

    {
      # CSV (default)
      write.csv(d, fPath, row.names=F, na="") }
  )

  f <- list.files(dirname(fPath), paste0(basename(fPath), ".*"), full.names=T)
  f <- c(f, genReadme(names(d)))
  fPath <- paste(fPath, "zip", sep=".")
  zip(fPath, f, flags="-9Xjm", zip="zip")
  return(fPath)
}
