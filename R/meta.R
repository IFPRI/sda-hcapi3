
#' Write HarvestChoice CELL5M auxiliary files to CSV and JSON per Tabular Data
#' Package RFC specifications
#'
#' This information is appended to all data downloads, but may also be queried
#' separately. Variable metadata is made available in tabular format (.csv)
#' and in JSON (.json) format in conformity with the latest Tabular Data Package RFC
#' specifications documented at \url{http://dataprotocols.org/tabular-data-package/}.
#'
#' @param var character array of CELL5M variable codes
#' @param format character, file extension corresponding to the data package output
#' format. See \code{\link{genFile}}. If \code{NULL} no auxiliary file is produced
#' aside from metadata records in \code{meta.csv}.
#' @param dir character, optional output directory. Default to current working directory.
#' @return character, paths to all generated auxiliary files
#' @examples
#' # Generate auxiliary files for soil organic carbon concentration and 2012 total population
#' f <- meta(c("soc_d5", "PN12_TOT"))
#' f
#'
#' # Equivalent cUrl request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/meta \
#' # -d '{"var" : ["soc_d5", "PN12_TOT"]}' \
#' # -X POST -H 'Content-Type:application/json'
#'
#' @export

meta <- function(var, format=NULL, dir=".") {

  # Auxiliary file names
  d <- paste(dir, c("meta.csv", "README.md", "datapackage.json"), sep="/")

  # Retrieve metadata records
  dt <- vi[var][, list(
    Code=varCode,
    Label=varLabel,
    Details=varDesc,
    Type=type,
    Unit=unit,
    Version=version,
    `Aggregation Formula`=aggFunR,
    Period=ifelse(is.na(yearEnd), year, paste(year, yearEnd, sep=" - ")),
    Category=cat1,
    Subcategory=cat2,
    Item=cat3,
    Source=ifelse(is.na(sources), sourceMini, sources),
    Contact=owner,
    Acquired=format(Sys.Date(), "%m/%d/%Y"),
    Citation=citation)]

  # Write to `meta.csv`
  write.csv(dt, d[1], row.names=F, na="")

  # Add Tabular Data Package specifications or stop here
  if( missing(format) ) return(d[1])
  format <- tolower(format)
  if( !format %in% c("asc", "csv", "dta", "geojson", "grd", "nc", "rds", "tif") ) return(d[1])

  # Write custom `README.md` and `README.html`
  r <- readLines(system.file("./www/readme.md", package="hcapi3"))
  r <- gsub("$date$", Sys.Date(), r, fixed=T)
  r <- gsub("$json$", toJSON(list(var=var)), r, fixed=T)
  write(r, d[2])

  # Write `datapackage.json`
  j <- list(
    name="IFPRI-CELL5M-SSA",
    datapackage_version="1.0-beta-2",
    title="CELL5M: A Multidisciplinary Geospatial Database for Africa South of the Sahara",
    description="HarvestChoice CELL5M-SSA is an on-line database of over 750 biophysical and socio-economic indicators for Africa south of the Sahara. All indicators are currently geo-referenced to a spatial resolution of 5-arc-minute (equivalent to around 10 sq. km. at the Equateur).",
    version=as.character(packageVersion("hcapi3")),
    last_updated=Sys.Date(),
    homepage="http://harvestchoice.org/",
    image="http://harvestchoice.github.io/hc-api3/img/hc-logo-white.png",
    sources=list(
      name="IFPRI/HarvestChoice",
      web="http://harvestchoice.org/",
      email="info@harvestchoice.org"),
    keywords=c("climate", "agriculture", "poverty", "market access", "nutrition",
      "Africa South of the Sahara", "climate adaptation", "farming", "soils", "livestock"),
    license="ODC-BY-1.0",
    publishers="International Food Policy Research Institute (IFPRI)",
    resources=list(
      # This assumes that data files have been generated in the same `dir`
      # and that there is only one, not perfect but well...
      name="CELL5M-SSA Data Extracts",
      path=setdiff(list.files(dir, paste0("^.*\\.", format, "$")), "meta.csv"),
      format=switch(format,
        asc="ESRI ASCII Grid",
        csv="csv",
        dta="STATA 12",
        geojson="GeoJSON",
        grd="R Raster",
        nc="netCDF",
        rds="R Data",
        tif="GeoTIFF"),
      mediatype=switch(format,
        asc="application/x-ascii-grid",
        csv="text/csv",
        dta="application/x-stata-dta",
        geojson="application/vnd.geo+json",
        grd="application/octet-stream",
        nc="application/x-netcdf",
        rds="application/octet-stream",
        tif="image/tiff"),
      schema=list(fields=dt[, .(
        name=Code,
        type=ifelse(Type=="class", "string", "numeric"),
        description=paste(Label, Details, sep=" - "),
        unit=Unit)]))
  )

  write(toJSON(j, dataframe="rows", pretty=T, auto_unbox=T), file=d[3])
  return(d)
}
