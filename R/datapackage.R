
#' Write auxiliary files to CSV and JSON (Tabular Data Package RFC specifications)
#'
#' Auxiliary files and metadata are appended to all data packages, but may also be
#' generated separately. Variable metadata is made available in tabular (.csv)
#' and in JSON (.json) format in conformity with Tabular Data Package RFC
#' specifications at \url{http://dataprotocols.org/tabular-data-package/}.
#'
#' API call: Generate auxiliary files for soil organic carbon concentration and
#' 2012 total population
#'
#' \code{$ curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/datapackage \
#'  -d '{"var" : ["soc_d5", "PN12_TOT"]}' \
#'  -X POST -H 'Content-Type:application/json'
#' }
#'
#' @param var character array of HarvestChoice indicator code(s)
#' @param format file extension corresponding to the data package output
#' format. See \code{\link{genFile}}. If \code{NULL} no auxiliary file is produced
#' aside from metadata records in \code{meta.csv}.
#' @param dir character, optional output directory. Default to current working directory.
#' @return character array of generated auxiliary file names
#' @seealso \link{indicator} and \link{category} for other convenience
#' methods to query HarvestChoice metadata.
#' @examples
#' # Generate auxiliary files for soil organic carbon concentration and 2012 total population
#' x <- datapackage(c("soc_d5", "PN12_TOT"))
#' x <- fread(x[1])
#' x
#'
#' @export

datapackage <- function(var, format=NULL, dir=".") {

  # Auxiliary file names
  d <- paste(dir, c("meta.csv", "README.md", "datapackage.json"), sep="/")

  # Retrieve metadata records
  dt <- vi[var][, list(
    code=varCode,
    label=varLabel,
    description=varDesc,
    type=type,
    unit=unit,
    version=version,
    aggFormula=aggFunR,
    period=ifelse(is.na(yearEnd), year, paste(year, yearEnd, sep=" - ")),
    category=cat1,
    subcategory=cat2,
    item=cat3,
    source=ifelse(is.na(sources), sourceMini, sources),
    contact=owner,
    downloadedOn=format(Sys.Date(), "%m/%d/%Y"),
    citation=citation)]

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
        name=code,
        type=ifelse(type=="class", "string", "numeric"),
        description=paste(label, description, sep=" - "),
        unit=unit)]))
  )

  write(toJSON(j, dataframe="rows", pretty=T, auto_unbox=T), file=d[3])
  return(d)
}
