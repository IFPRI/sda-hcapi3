
#' Return HarvestChoice variable metadata
#'
#' @param var optional character array of HarvestChoice variable codes
#' @param cat optional category filter (fuzzy matches)
#' @param version optional version filter
#' @param raster return only raster variables
#' @param as.class simple "data.table" or if "list" group variables by category
#' @param css include Carto CSS rules
#' @return a data.table or hierarchical list of variable metadata
#' @seealso \code{\link{category}} to view HarvestChoice indicator categories
#' @examples
#' # Show complete metadata for all variables under 'Cassava' category
#' describe(cat="cassava", raster=T)
#'
#' # Equivalent cURL request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/describe/json \
#' # -d '{"cat" : "cassava", "raster" : true, "as.class" : "list"}' \
#' # -X POST -H "Content-Type:application/json"
#'
#' describe(raster=T, version="SChEF r2.3", as.class="list")[1:2]
#'
#' # Equivalent cURL request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/describe/json \
#' # -d '{"raster" : true, "version" : "SChEF r2.3", "as.class" : "list"}' \
#' # -X POST -H "Content-Type:application/json"
#'
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
    mss <- sapply(out$Code, genCartoCSS)
    out[, CartoCSS := mss]
  }

  setkey(out, Category, Subcategory, Item)

  if (as.class=="list") {
    # Group by category
    setkey(out, Category, Subcategory, Item, Label, Code)
    out <- split(out, out$Category)
    out <- lapply(out, function(x) split(x, x$Subcategory))
    out <- lapply(out, function(x) lapply(x, function(y) split(y, y$Item)))
  }

  return(out)
}


#' Show HarvestChoice variable categories (3-level deep)
#'
#' @param cat optional fuzzy filter
#' @param as.class "data.table" simple data table or "list" grouped by category
#' @return a data.table with \code{N} showing the number of indicators in each category,
#' or a list of variable categories
#' @examples
#' # List all HarvestChoice indicators matching category "Demographics"
#' category("demographics")
#'
#' # List all HarvestChoice indicators matching "cassava", return as a hierarchical list
#' category("cassava", as.class="list")
#'
#' # Equivalent request using cURL at the command line and passing well-formatted JSON
#' # objects
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/category/json \
#' # -d '{"cat" : "demographics'} \
#' # -X POST -H "Content-Type:application/json"
#'
#' #' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/category/json \
#' # -d '{"cat" :" cassava", "as.class" : "list"} \
#' # -X POST -H "Content-Type:application/json"
#' @export
category <- function(cat, as.class="data.table") {

  out <- vi[published==T, list(varCode, varLabel),
    keyby=list(Category=cat1, Subcategory=cat2, Item=cat3)]

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
#' @param var HarvestChoice variable code
#' @param iso3 optional country or region filter (3-letter code)
#' @param pal optional Brewer color palette name used for plotting
#' @param legend if TRUE returns HarvestChoice legend, otherwise returns default legend
#' @param ... any argument passed to \code{\link{getLayer}}, e.g. \code{by="FS_2012", iso3="GHA"}
#' @return character, CartoCSS snippet for variable \code{var}
#' @examples
#' genCartoCSS("soc_d5")
#' @export
genCartoCSS <- function(var, pal="BuGn", legend=TRUE, ...) {

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
