
#' Write HarvestChoice variable metadata to CSV
#'
#' This information is appended to all data downloads, but may also be queried
#' separately. Variable metadata is now made available in tabular format (csv).
#'
#' @param var character array of CELL5M variable codes
#' @param file output file path, default ./README.csv in current directory
#' @return character, path to generated README file
#' @examples
#' # README for soil organic carbon concentration and 2012 total population
#' f <- readme(c("soc_d5", "PN12_TOT"))
#' read.table(f)
#'
#' # Equivalent cUrl requests at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/readme \
#' # -d '{"var" : ["soc_d5", "PN12_TOT"]}' \
#' # -X POST -H 'Content-Type:application/json'
#'
#' @export

readme <- function(var, file="./readme.csv") {

  # Retrieve meta
  meta <- vi[var][, list(
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

  # Write one variable at a time
  write.csv(meta, file, row.names=F, na="")
  return(file)
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
