
.onLoad <- function(libname, pkgname) {
  # Set remote Rserve server and port
  options(na.last=T, hcapi3.host="hcapi.harvestchoice.org", hcapi3.port=9100L)
  # data.table unique() and duplicated() new behavior can break code
  options(datatable.old.unique.by.key=TRUE)
}