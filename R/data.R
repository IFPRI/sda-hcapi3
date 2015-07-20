#' HarvestChoice inventory of 5-arc-minute spatial indicators
#'
#' Metadata elements for HarvestChoice collection of 5-arc-minute rasters for
#' sub-Saharan Africa.
#'
#' @docType data
#' @keywords datasets
#' @name vi
#' @usage vi.R
#' @format A data.table object
#' @examples
#' # List of metadata elements that may be accessed though `vi`
#' names(vi)
#'
#' # Print full metadata for cassava yield
#' knitr::kable(data.frame(t(vi["cass_y"])), "rst", row.names=T)
#'
#' # Get specific metadata elements
#' vi["cass_y"][, list(varTitle, units)]
NULL

#' List of ISO3 country and/or region codes
#'
#' List of ISO3 country codes and corresponding country/region labels.
#'
#' @docType data
#' @keywords datasets
#' @name iso
#' @usage iso.R
#' @format arrays
#' @note Country boundaries are derived from FAO GAUL 2008 (2009 eds.)
#' @examples
#' # Lookup lists
#' names(iso)
#'
NULL
