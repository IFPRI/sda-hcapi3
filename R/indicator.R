
#' Return HarvestChoice indicator metadata
#'
#' Return complete metadata records for selected HarvestChoice indicators.
#'
#' # API call: metadata records for all indicators matching 'cassava' or 'population'
#'
#' \code{
#' curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/indicator/json \
#'  -d '{"q" : ["cassava", "population"]}' \
#'  -X POST -H "Content-Type:application/json"
#' }
#'
#' API call: metadata records for all indicators matching 'population' in a list
#'
#' \code{
#' curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/indicator/json \
#'  -d '{"q" : "population", "as.class" : "list"}' \
#'  -X POST -H "Content-Type:application/json"
#' }
#'
#' @param q character array of pattern(s) to search against HarvestChoice indicator codes,
#' labels, and categories. Will attempt a fuzzy match.
#' @param version optional version filter
#' @param as.class "data.table" (default) or "list" for a list of indicators grouped
#' by category
#' @param cartoCSS also include formatted CartoCSS rules
#' @return a data.table or hierarchical list of indicator metadata
#' @seealso \code{\link{category}} to view HarvestChoice indicator categories
#' @examples
#' # Show complete metadata for all HarvestChoice indicators matching 'cassava' or
#' 'population'
#' indicator(c("cassava", "population"))
#'
#' # Show complete metadata for all HarvestChoice indicators matching 'population'
#' # in a hierarchical list
#' indicator("population", as.class="list")[1:2]
#'
#' @export
indicator <- function(q, version=NULL, as.class="data.table", cartoCSS=FALSE) {

  if(missing(q) | paste(q, collapse="")=="") stop(
    "'q' is missing. Enter a search string to query HarvestChoice metadata records,
    or use 'category()' to return a complete catalog.")

  q <- tolower(q)

  out <- vi[published==TRUE & (
    stringr::str_detect(tolower(varCode), q) |
      stringr::str_detect(tolower(varLabel), q) |
      stringr::str_detect(tolower(cat1), q) |
      stringr::str_detect(tolower(cat2), q) |
      stringr::str_detect(tolower(cat3), q)
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
    wms=ifelse(genRaster, paste0("http://apps.harvestchoice.org/arcgis/rest/services/", mxdName, "/MapServer/WMSServer"), NA),
    downloadedOn=Sys.Date()
  )]

  if(nrow(out)==0) stop("No matching record found. Enter a different search string,
or use 'category()' to return a complete catalog.")

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
#' Return a compact list of indicator categories, codes and labels
#'
#' \code{
#' # API call: list all HarvestChoice indicators matching category 'demographics'
#' curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/category/json \
#'  -d '{"q" : "demographics'} \
#'  -X POST -H "Content-Type:application/json"
#'
#' # API call: list all HarvestChoice indicators matching 'cassava' in a hierarchical list
#' curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/category/json \
#'  -d '{"q" :" cassava", "as.class" : "list"} \
#'  -X POST -H "Content-Type:application/json"
#'
#' # To return a complete list of published indicators omit 'q'
#' curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/category/json \
#'  -X POST -H "Content-Type:application/json"
#' }
#'
#' @param q character array of pattern(s) to search for. If omitted will return all
#' available indicators.
#' @param as.class "data.table" or "list" of indicator codes grouped by category
#' @return a data.table showing the number of indicators in each category,
#' or a list of indicators grouped by category
#' @seealso \code{\link{indicator}} to view complete indicator metadata records
#' @examples
#' # List all HarvestChoice indicators matching category 'demographics'
#' category("demographics")
#'
#' # List all HarvestChoice indicators matching 'cassava' in a hierarchical list
#' category("cassava", as.class="list")
#' @export
category <- function(q=NULL, as.class="data.table") {

  if (missing(q) | paste(q, collapse="")=="") {
    out <- vi[, .(code=varCode, label=varLabel),
      keyby=.(category=cat1, subcategory=cat2, item=cat3)]
  } else {
    q <- tolower(q)
    out <- vi[published==TRUE & (
      stringr::str_detect(tolower(varCode), q) |
        stringr::str_detect(tolower(cat1), q) |
        stringr::str_detect(tolower(cat2), q) |
        stringr::str_detect(tolower(cat3), q)
    ), .(code=varCode, label=varLabel),
      keyby=.(category=cat1, subcategory=cat2, item=cat3)]
  }

  if(nrow(out)==0) stop("No matching record found. Enter a different search string,
or use 'category()' to return a complete catalog.")

  setkey(out, category, subcategory, item, label, code)

  if (as.class=="list") {
    out <- split(out, out$category)
    out <- lapply(out, function(i) split(i, i$subcategory))
  }

  return(out)
}



#' Generate CartoCSS snippet to symbolize raster tiles
#'
#' Helper function to generate CartoCSS rules for processing rasters using Mapnik
#'
#' @param var character arry of HarvestChoice indicator code(s)
#' @param iso3 optional country or region filter (3-letter code)
#' @param pal optional Brewer color palette name used for plotting
#' @param legend if TRUE returns HarvestChoice legend, otherwise returns default legend
#' @param ... any argument passed to \code{\link{getLayer}}, e.g.
#' \code{by="FS_2012"}
#' @return character array of CartoCSS snippet for indicator(s) \code{var}
#' @examples
#' # CartoCSS for soil organic carbon and cassava value of production
#' genCartoCSS(c("soc_d5", "cass_v"))
#' @export
genCartoCSS <- function(var, pal="BuGn", legend=TRUE, ...) {

  setkey(vi, varCode)
  var <- match.arg(var, vi$varCode, several.ok=TRUE)
  out <- sapply(var, function(x) {

    d <- getLayer(x, collapse=TRUE, ...)

    # With fixed symbology
    cc <- unlist(vi[x, strsplit(classColors, "|", fixed=T)])
    cv <- as.numeric(unlist(vi[x, strsplit(classBreaks, "|", fixed=T)]))
    if (vi[x, type]=="continuous") cv <- c(cv, ceiling(max(d[[x]], na.rm=T)))
    cl <- unlist(vi[x, strsplit(classLabels, "|", fixed=T)])

    # Symbology is missing or use default symbology
    if (length(cv)==0 | legend==FALSE) {

      if (vi[x, type]=="class") {
        # Categorical raster
        cl <- levels(factor(d[[x]]))
        cv <- 1:length(cl)-1

      } else {
        # Continuous raster, and we need the max value
        require(classInt)
        cv <- cl <- classIntervals(d[[x]], n=8, style="pretty")$brks
        cv <- c(cv[-1], ceiling(max(d[[x]], na.rm=T)))
      }
      cc <- colorRampPalette(brewer.pal(length(cv), pal))(length(cv))
    }

    # Raster symbology
    y <- paste0("#", tolower(x), " {
      raster-scaling: bilinear;
      raster-colorizer-default-mode: linear;
      raster-colorizer-default-color: transparent;
      raster-colorizer-stops:
      stop(-9999, transparent, linear)
      ",
      paste0("stop(", cv, ", ", cc, ")
        ", collapse=" "),
      "}")

    y <- gsub("\n", " ", y, fixed=T)
  })

  return(out)
}
