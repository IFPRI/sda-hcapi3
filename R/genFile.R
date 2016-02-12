#' Convert CELL5M layers to raster and/or tabular formats for download
#'
#' Package any result from \code{\link{getLayer}} into the user-specified tabular or
#' spatial raster format. Also includes a README file with metadata and citation details.
#' Currently supported export formats include CSV (csv), STATA (dta), GeoJSON (json), GeoTIFF
#' (tif), R raster (grd), RData (rda), ESRI ASCII raster (asc), and netCDF (nc).
#' Calling \code{genFile(var="bmi", iso3="TZA", format="dta")} is equivalent to calling
#' the convenience method \code{hcapi(var="bmi", iso3="TZA", format="dta")}.
#'
#' @param var character array of variable codes, passed to \code{\link{getLayer}}
#' @param iso3 character array of ISO3 country or region codes, passed to \code{\link{getLayer}}
#' @param by character array of variable codes to summarize by, passed to \code{\link{getLayer}}
#' @param format output format, one of "csv", "json", "tif", "dta", "asc", "grd", "rds".
#' @param path output directory, default to current working directory
#' @param ... any other optional argument passed to \code{\link{getLayer}},
#' e.g. \code{by}, \code{collapse}.
#'
#' @return character, array of generated file names
#'
#' @examples
#' # Total wheat harvested area across 16 agro-ecological zones in Nigeria and Ethiopia
#' # in STATA format
#' x <- genFile("whea_h", iso3=c("NGA", "ETH"), by="AEZ16_CLAS", format="dta")
#'
#' # Load generated STATA file
#' require(foreign)
#' x <- read.dta(x[1])
#'
#' # Plot histogram with full layer title
#' hist(x$whea_h, xlab=vi["whea_h"][, varTitle], main=NULL)
#'
#' # Sorghum production in Nigeria returned in ESRI ASCII raster format
#' x <- genFile("sorg_p", iso3="NGA", format="asc")
#'
#' # Load and plot generated raster
#' require(raster)
#' x <- raster(x[1])
#' plot(x, main=vi["sorg_p"][, varTitle])
#' cellStats(x, "mean")
#'
#' # Equivalent cUrl requests at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/genFile \
#' # -d '{"var" : "whea_h", "iso3" : ["NGA", "ETH"], "by" : "AEZ16_CLAS", "format" : "dta"}' \
#' # -X POST -H 'Content-Type:application/json'
#'
#' # /ocpu/tmp/x0e654538b7/R/.val
#' # /ocpu/tmp/x0e654538b7/stdout
#' # /ocpu/tmp/x0e654538b7/warnings
#' # /ocpu/tmp/x0e654538b7/source
#' # /ocpu/tmp/x0e654538b7/console
#' # /ocpu/tmp/x0e654538b7/info
#' # /ocpu/tmp/x0e654538b7/files/DESCRIPTION
#' # /ocpu/tmp/x0e654538b7/files/README
#' # /ocpu/tmp/x0e654538b7/files/whea_h-AEZ16_CLAS-NGA.dta
#'
#' # Use wget (at the command line) to download all generated files in a ZIP archive
#' # wget http://hcapi.harvestchoice.org/ocpu/tmp/x0e654538b7/zip
#'
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/genFile \
#' # -d '{"var" : "sorg_p", "format" : "asc"}' \
#' # -X POST -H "Content-Type:application/json"
#'
#' # /ocpu/tmp/x02a7a044c7/R/.val
#' # /ocpu/tmp/x02a7a044c7/stdout
#' # /ocpu/tmp/x02a7a044c7/warnings
#' # /ocpu/tmp/x02a7a044c7/source
#' # /ocpu/tmp/x02a7a044c7/console
#' # /ocpu/tmp/x02a7a044c7/info
#' # /ocpu/tmp/x02a7a044c7/files/DESCRIPTION
#' # /ocpu/tmp/x02a7a044c7/files/README
#' # /ocpu/tmp/x02a7a044c7/files/sorg_p--SSA.asc
#' # /ocpu/tmp/x02a7a044c7/files/sorg_p--SSA.asc.aux.xml
#' # /ocpu/tmp/x02a7a044c7/files/sorg_p--SSA.prj
#'
#' # Then use wget to download all generated files in a ZIP archive
#' # wget http://hcapi.harvestchoice.org/ocpu/tmp/x02a7a044c7/zip
#'
#' @export
genFile <- function(var, iso3="SSA", by=NULL,
  format=c("csv", "geojson", "tif", "dta", "asc", "rds", "grd"), path=".", ...) {

  setkey(vi, varCode)

  # Validate format
  format <- tolower(format)
  if (format %in% c("tiff", "geotiff", "geotif", "tif")) format <- "tif"
  else if (format %in% c("geojson", "json")) format <- "geojson"
  else if (format %in% c("grid", "grd")) format <- "grd"
  else if (format %in% c("rdata", "rda", "rdata", "rds")) format <- "rds"
  else if (format %in% c("dta", "stata")) format <- "dta"
  else if (format %in% c("asc", "ascii")) format <- "asc"
  else if (format %in% c("csv", "xls", "xlsx")) format <- "csv"
  else if (format %in% c("nc", "netcdf")) format <- "nc"
  else return(paste(format, "is not a recognized format."))

  # Construct temporary file name
  fPath <- paste(paste("hcapi", var[1], by[1], tolower(iso3[1]), sep="-"), format, sep=".")

  # If many variables, simply use `cat2` file name instead
  if ( length(var)>1 ) {
    fPath <- gsub(" ", "_", tolower(vi[var[1], cat2]), fixed=T)
    fPath <- paste(paste("hcapi", fPath, tolower(iso3[1]), sep="-"), format, sep=".")
  }

  if ( format %in% c("grd", "asc", "tif", "geojson") ) {
    # Call getLayer() and don't collapse for spatial formats
    d <- getLayer(var, iso3, by, collapse=F, ...)
  } else {
    d <- getLayer(var, iso3, by, ...)
  }

  if ( format %in% c("asc", "tif", "grd", "nc") ) {
    # Raster formats require more work
    fPath <- paste(paste("hcapi", tolower(iso3[1]), sep="-"), format, sep=".")
    pts <- d[, .(X, Y)]
    var <- setdiff(names(d), g)
    d <- d[, .SD, .SDcols=var]

    cl <- lapply(var,
      function(x) as.character(unlist(vi[x][, strsplit(classLabels, "|", fixed=T)])))
    cc <- lapply(var,
      function(x) as.character(unlist(vi[x][, strsplit(classColors, "|", fixed=T)])))
    ct <- lapply(var, function(x) "Float32")
    mv <- lapply(var, function(x) -9999)

    names(cl) <- names(cc) <- names(ct) <- names(mv) <- var

    for (i in var) {
      if ( vi[i][, type] == "class" )  {
        # If categorical raster, then convert to 0-based integer and add labels
        d[[i]] <- as.integer(factor(d[[i]], levels=cl[[i]], ordered=T))-1L
        ct[[i]] <- "Byte"
        mv[[i]] <- 255
      }
    }

    # Convert to unprojected raster
    d <- SpatialPixelsDataFrame(pts, data.frame(d),
      tolerance=0.00360015, proj4string=CRS("+init=epsg:4326"))
  }

  # File path
  fPath <- paste(path, fPath, sep="/")

  switch(format,
    tif = {
      # GeoTIFF, write by layer to preserve color palettes
      lapply(names(d), function(x) writeGDAL(d[, x], drivername="GTiff",
        gsub(".", paste0("-", x, "."), fPath, fixed=T),
        type=ct[[x]], mvFlag=mv[[x]], catNames=cl[x], colorTables=cc[x],
        options=c("INTERLEAVE=BAND", "TFW=YES", "ESRI_XML_PAM=YES"))) },

    nc = {
      # netCDF, write brick and layer names and units
      # Note that named layers are not implemented yet
      writeRaster(brick(d), fPath, format="CDF",
        overwrite=T, xname="lon", yname="lat", zname="indicator") },

    grd = {
      # Native raster grid
      writeRaster(brick(d), fPath, overwrite=T) },

    asc = {
      # ESRI ASCII grid, write by layer
      lapply(names(d), function(x) writeGDAL(d[, x], drivername="AAIGrid",
        gsub(".", paste0("-", x, "."), fPath, fixed=T),
        type=ct[[x]], mvFlag=mv[[x]], catNames=cl[x], colorTables=cc[x],
        options=c("INTERLEAVE=BAND", "TFW=YES", "ESRI_XML_PAM=YES"))) },

    geojson = {
      # GeoJSON
      d <- SpatialPointsDataFrame(d[, .(X, Y)], d,
        proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
      writeOGR(d, fPath, var[1], driver="GeoJSON") },

    dta = {
      # Stata
      setattr(d, "var.labels", vi[names(d)][, paste0(varLabel, " (", unit, ")")])
      setattr(d, "datalabel", "Produced by HarvestChoice/IFPRI at http://hcapi.harvestchoice.org/. Contact <info@harvestchoice.org>.")
      setattr(d, "time.stamp", Sys.Date())
      write.dta(d, fPath, version=12L) },

    rds = {
      # RDS
      attr(d, "var.labels") <- vi[names(d)][, varLabel]
      attr(d, "datalabel") <- "Produced by HarvestChoice/IFPRI at http://hcapi.harvestchoice.org/. Contact <info@harvestchoice.org>."
      attr(d, "time.stamp") <- as.character(as.Date(Sys.Date()))
      saveRDS(d, file=fPath) },

    {
      # CSV (default)
      write.csv(d, fPath, row.names=F, na="") }
  )


  # Add README and TERMS
  readme(names(d), paste(path, "readme.csv", sep="/"))
  file.copy(paste(path.package("hcapi3"), "www/terms.html", sep="/"), paste(path, "terms.html", sep="/"))
  f <- list.files(path, paste0("^", strsplit(basename(fPath), ".", fixed=T)[[1]][1]), full.names=T)
  f <- c(f, paste(path, c("terms.html", "readme.csv"), sep="/"))
  return(f)
}

#fPath <- paste(fPath, "zip", sep=".")
#zip(fPath, f, flags="-9Xjm", zip="zip")

