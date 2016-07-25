#' Catalog of HarvestChoice indicators
#'
#' Complete metadata records for HarvestChoice 5-arc-minute inidcators.
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
#' t(vi["cass_y"])
#'
#' # Get specific metadata elements
#' vi["cass_y", .(varTitle, unit)]
#'
#' # Count of layers by category
#' vi[, .N, by=.(cat1, cat2)]
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
#' iso
#'
NULL
