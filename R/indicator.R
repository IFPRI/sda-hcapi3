
#' Return HarvestChoice indicator metadata
#'
#' @param q search string to match against HarvestChoice indicator codes,
#' labels, and categories. Will attempt a fuzzy match against these metadata elements.
#' @param version optional version filter
#' @param as.class "data.table" (default) or "list" for a list of indicators grouped
#' by category
#' @param cartoCSS also include formatted CartoCSS rules
#' @return a data.table or hierarchical list of indicator metadata
#' @seealso \code{\link{category}} to view HarvestChoice indicator categories
#' @examples
#' # Show complete metadata for all HarvestChoice indicators matching 'cassava' or
#' 'population'
#' indicator("cassava")
#'
#' # Equivalent cURL request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/indicator/json \
#' # -d '{"q" : "cassava", "as.class" : "list"}' \
#' # -X POST -H "Content-Type:application/json"
#'
#' indicator("population", as.class="list")[1:2]
#'
#' # Equivalent cURL request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/indicator/json \
#' # -d '{"q" : "population", as.class" : "list"}' \
#' # -X POST -H "Content-Type:application/json"
#'
#' @export
indicator <- function(q, version=NULL, as.class="data.table", cartoCSS=FALSE) {

  if(missing(q)) stop("'q' is missing. Enter a search string to query HarvestChoice metadata records,
    or use 'category()' to return a complete catalog.")
  q <- tolower(q)

  out <- vi[published==TRUE & (
    tolower(varCode) %like% q |
      tolower(varLabel) %like% q |
      tolower(cat1) %like% q |
      tolower(cat2) %like% q |
      tolower(cat3) %like% q
  ), .(
    label=varLabel,
    code=varCode,
    unit=unit,
    type=type,
    period=ifelse(is.na(yearEnd), year, paste(year, yearEnd, sep=" - ")),
    category=cat1,
    subcategory=cat2,
    item=cat3,
    sources=ifelse(is.na(sources), sourceMini, sources),
    contact=owner,
    description=paste(varTitle, varDesc, sep=". "),
    citation=citation,
    version=version,
    aggFormula=aggFunR,
    isRaster=genRaster,
    url=ifelse(genRaster, paste0("http://harvestchoice.org/data/", tolower(varCode)), NA),
    wms=ifelse(genRaster, paste0("http://apps.harvestchoice.org/arcgis/services/", mxdName, "/MapServer/WMSServer"), NA),
    downloadedOn=Sys.Date()
  )]

  if(nrow(out)==0) stop("No matching record found. Enter a different search string, or use
    'category()' to return a complete catalog.")

  # Optional filters
  if (!missing(version)) out <- out[version==paste0("SChEF r", version)]

  # Add CartoCSS
  if (cartoCSS==TRUE) {
    out <- out[isRaster==T]
    mss <- sapply(out$Code, genCartoCSS)
    out[, CartoCSS := mss]
  }

  setkey(out, category, subcategory, item, label, code)

  if (as.class=="list") {
    # Group by category
    out <- split(out, out$category)
    out <- lapply(out, function(x) split(x, x$subcategory))
    out <- lapply(out, function(x) lapply(x, function(y) split(y, y$item)))
  }

  return(out)
}


#' Return HarvestChoice indicator categories (3-level deep)
#'
#' @param q optional fuzzy filter
#' @param as.class "data.table" or "list" of indicator codes grouped by category
#' @return a data.table showing the number of indicators in each category,
#' or a list of indicators grouped by category
#' @seealso \code{\link{indicator}} to view complete indicator metadata records
#' @examples
#' # List all HarvestChoice indicators matching category 'demographics'
#' category("demographics")
#'
#' # List all HarvestChoice indicators matching 'cassava', return a hierarchical list
#' category("cassava", as.class="list")
#'
#' # Equivalent request using cURL at the command line and passing well-formatted JSON
#' # objects
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/category/json \
#' # -d '{"q" : "demographics'} \
#' # -X POST -H "Content-Type:application/json"
#'
#' #' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/category/json \
#' # -d '{"q" :" cassava", "as.class" : "list"} \
#' # -X POST -H "Content-Type:application/json"
#' @export
category <- function(q=NULL, as.class="data.table") {

  if (missing(q) | q=="") {
    out <- vi[, .(code=varCode, label=varLabel),
      keyby=.(category=cat1, subcategory=cat2, item=cat3)]
  } else {
    q <- tolower(q)
    out <- vi[published==TRUE & (
      tolower(cat1) %like% q |
        tolower(cat2) %like% q |
        tolower(cat3) %like% q
    ), .(code=varCode, label=varLabel),
      keyby=.(category=cat1, subcategory=cat2, item=cat3)]
  }

  if(nrow(out)==0) stop("No matching record found. Enter a different search string, or use
    'category()' to return a complete catalog.")

  setkey(out, category, subcategory, item, label, code)

  if (as.class=="list") {
    out <- split(out, out$category)
    out <- lapply(out, function(i) split(i, i$subcategory))
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
