#' Rank regions, districts, or pixels by similarity to a reference unit for a set of indicators
#'
#' @param x region (ADM1_CODE), district (ADM2_CODE), or pixel code (CELL5M)
#' @param var character array of HarvestChoice variable codes
#' @param by reference geography, 1 for regions, 2 for districts, 0 for pixel)
#' @param iso3 optional country or region filter (3-letter code)
#' @return a data.table of regions, districts, or pixels ranked by similarity to \code{x}
#' @export
getSimilar <- function(x, var, by=0, iso3="SSA") {

  setkey(vi, varCode)

  # What to benchmark
  by <- switch(by, `1`="ADM1_CODE_ALT", `2`="ADM2_CODE_ALT", `0`="CELL5M")

  # Get values for reference unit
  out <- getLayer(var, iso3, by)
  out <- eval(parse(text=paste0("out[", by, "!=0]")))
  ref <- eval(parse(text=paste0("out[", by, "==x]")))

  # Compute aggregate distance and return a ranking of homologous units
  # With continuous variables, use absolute numeric difference
  # With classified variables, use TRUE/FALSE dummy
  # Convert any classified variable to dummy, first
  facVar <- vi[var][type=="class", varCode]

  if (length(facVar)>0) {
    numVar <- names(out)
    numVar <- numVar[numVar!=facVar]
    facOut <- out[, lapply(facVar, function(i) .SD[[i]]==ref[[i]]), .SDcols=facVar]
    setnames(facOut, facVar)
    ref[[facVar]] <- TRUE
    out <- cbind(out[, .SD, .SDcols=numVar], facOut)
  }

  tmp <- out[, lapply(.SD, scale), .SDcols=var]
  tmp <- cbind(out[[by]], tmp)
  setnames(tmp, 1, by)
  ref <- eval(parse(text=paste0("tmp[", by, "==x]")))
  tmp <- tmp[, lapply(var, function(i) abs(.SD[[i]]-ref[[i]])), .SDcols=var]
  tmp <- tmp[, list(Score=rowSums(.SD, na.rm=T))]
  out <- cbind(out, tmp)[order(Score)]
  return(out)
}

