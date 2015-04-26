
#' Return HarvestChoice variable metadata
#'
#' @param var character array of HarvestChoice variables
#' @param cat optional category filter
#' @param version optional version filter
#' @param raster return only raster variables
#' @param as.class simple "data.table" or if "list" group variables by category
#' @param css include Carto CSS rules
#' @return a data.table or list of variable metadata
#' @examples
#' # Show complete metadata for all variables under 'Cassava' category
#' describe(cat="cassava", raster=T)
#'
#' # Equivalent cUrl request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/describe/json \
#' # -d '{"cat" : "cassava", "raster" : true, "as.class" : "list"}' \
#' # -X POST -H "Content-Type:application/json"
#' @export
describe <- function(var, cat, version, raster=FALSE, as.class="data.table", css="json") {

  out <- vi[, list(
    Label=varLabel,
    Code=varCode,
    Unit=unit,
    Type=type,
    Period=ifelse(is.na(yearEnd), year, paste(year, yearEnd, sep=" - ")),
    Category=cat1,
    Subcategory=cat2,
    Item=cat3,
    Source=ifelse(is.na(sources), sourceMini, sources),
    Contact=owner,
    Details=paste(varTitle, varDesc, sep=". "),
    Citation=citation,
    Version=version,
    `In Table`=tbName,
    Formula=aggFunR,
    isRaster=genRaster,
    dTopic,
    dCrop,
    dKeywords,
    classBreaks,
    classLabels,
    classColors,
    Website=ifelse(genRaster, paste0("http://harvestchoice.org/data/", tolower(varCode)), NA),
    WMS=ifelse(genRaster, paste0("http://dev.harvestchoice.org:6080/arcgis/services/", mxdName, "/MapServer/WMSServer"), NA),
    `Downloaded on`=as.character(as.Date(Sys.Date())))]

  # Optional filters
  if (!missing(var)) out <- out[Code %in% var]
  if (!missing(version)) out <- out[Version==paste0("SChEF r", version)]
  if (raster==T) out <- out[isRaster==T]
  if (!missing(cat)) {
    out <- out[tolower(Category) %like% tolower(cat) |
        tolower(Subcategory) %like% tolower(cat) |
        tolower(Item) %like% tolower(cat)]
  }

  if (css=="carto") {
    # Add CartoCSS
    out[, `:=` (classBreaks=NULL, classLabels=NULL, classColors=NULL)]
    out <- out[isRaster==T]
    mss <- sapply(out$Code, getCartoCSS)
    out[, CartoCSS := mss]
  }

  setkey(out, Category, Subcategory, Item)

  if (by.category) {
    # Group by category
    setkey(out, Category, Subcategory, Item, Label, Code)
    out <- split(out, out$Category)
    out <- lapply(out, function(x) split(x, x$Subcategory))
    out <- lapply(out, function(x) lapply(x, function(y) split(y, y$Item)))
  }

  return(out)
}


#' Generate HarvestChoice data use terms, citation, and variable metadata
#'
#' @param var character array of CELL5M variable names
#' @return character, path to generated README file
#' @examples
#' # README for soil organic carbon concentration
#' f <- genReadme("soc_d5")
#' print(readLines(f))
#'
#' @export
genReadme <- function(var) {

  meta <- vi[var][, list(
    Code=varCode,
    Label=varLabel,
    Details=str_wrap(paste(varTitle, varDesc, sep=". "), 78),
    Type=type,
    Unit=unit,
    Version=version,
    `Aggregation Formula`=aggFunR,
    Period=ifelse(is.na(yearEnd), year, paste(year, yearEnd, sep=" - ")),
    Category=cat1,
    `Sub-category`=cat2,
    Item=cat3,
    Source=str_wrap(ifelse(is.na(sources), sourceMini, sources), 79),
    Contact=owner,
    Citation=str_wrap(citation, 77))]

  meta <- split(meta, meta$Code)
  meta <- sapply(meta, function(x) paste(names(x), x, sep=":\t", collapse="\n"))
  meta <- paste(meta, collapse=paste0("\n", str_dup("-", 86), "\n"))

  readme <- gsub("{date}", Sys.Date(), readme, fixed=T)
  readme <- gsub("{meta}", meta, readme, fixed=T)
  fPath <- "./README"
  writeLines(readme, fPath)
  return(fPath)
}


#' Return list of HarvestChoice variable categories
#'
#' @param cat optional category filter
#' @param as.class "data.table" simple data table or "list" group by category
#' @return a data.table or list of variable categories
#' @examples
#' category("Demographics")
#' @export
category <- function(cat, as.class="data.table") {

  out <- vi[published==T, .N, keyby=list(Category=cat1, Subcategory=cat2, Item=cat3)]

  if (!missing(cat)) {
    out <- out[tolower(Category) %like% tolower(cat) |
        tolower(Subcategory) %like% tolower(cat) |
        tolower(Item) %like% tolower(cat)]
  }

  if (as.class=="list") {
    out <- split(out, out$Category)
    out <- lapply(out, function(x) split(x, x$Subcategory))
  }
  return(out)
}



#' Generate CartoCSS snippet to symbolize raster tiles
#'
#' @param var HarvestChoice variable name
#' @param iso3 optional country or region filter (3-letter code)
#' @param pal optional Brewer color palette used for plotting
#' @param legend if TRUE returns HarvestChoice legend, otherwise returns default legend
#' @param ... any argument passed to getLayer(), e.g. by=FS_2012, iso3=GHA
#' @return character, CartoCSS snippet for requested variable
#' @export
getCartoCSS <- function(var, pal="BuGn", legend=TRUE, ...) {

  setkey(vi, varCode)
  var <- var[1]
  d <- getLayer(var, collapse=TRUE, ...)

  # With fixed symbology
  cc <- unlist(vi[var][, strsplit(classColors, "|", fixed=T)])
  cv <- as.numeric(unlist(vi[var][, strsplit(classBreaks, "|", fixed=T)]))
  if ( vi[var][, type]=="continuous" ) cv <- c(cv, ceiling(max(d[[var]], na.rm=T)))
  cl <- unlist(vi[var][, strsplit(classLabels, "|", fixed=T)])

  if (is.na(cv) | legend==F) {
    # Symbology is missing or use default symbology

    if (vi[var][, type]=="class") {
      # Categorical raster
      cl <- levels(factor(d[[var]]))
      cv <- 1:length(cl)-1

    } else {
      # Continuous raster, and we need the max value
      require(classInt)
      cv <- cl <- classIntervals(d[[var]], n=8, style="pretty")$brks
      cv <- c(cv[-1], ceiling(max(d[[var]], na.rm=T)))
    }
    cc <- colorRampPalette(brewer.pal(length(cv), pal))(length(cv))
  }

  # Raster symbology
  out <- paste0("#", tolower(var), "tif",
    " { raster-scaling: bilinear;
    raster-colorizer-default-mode: linear;
    raster-colorizer-default-color: transparent;
    raster-colorizer-stops:
    stop(-9999, transparent, linear)",
    paste0("stop(", cv, ", ", cc, ")", collapse=" "),
    " }")

  out <- gsub("\n", "", out, fixed=T)
  return(out)
}
