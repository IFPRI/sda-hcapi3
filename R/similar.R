#' Rank spatial units by similarity
#'
#' Rank regions, districts, or pixels by similarity to a reference unit across a set of
#' indicators. To generate a distance score, we center and scale all numeric variables
#' and convert classified variables to 0 or 1 (1 if same as reference unit, 0 otherwise).
#' For each variable we compute the absolute percent deviation between each unit and the
#' reference unit, and we then sum the deviations across all variables to generate an
#' aggregate distance score. The lower the score, the more similar the unit for the
#' selected indicators.
#'
#' @param x single integer code for the reference geography, either a region code
#' (ADM1_CODE_ALT), district code (ADM2_CODE_ALT), or gridcell code (CELL5M) to rank against
#' @param var character array of HarvestChoice variable codes used in the ranking
#' @param by single integer indicating the type of reference geography (0-gridcell,
#' 1-region, 2-district)
#' @param iso3 optional country or region filter (3-letter code)
#' @examples
#' # Rank all districts in Ghana by similarity to district 16657 (Kpando)
#' # based on the length of growing period (LGP_AVG), irrigated cropland (GMIA_V5),
#' # and cassava value of production (cass_v).
#' x <- similar(16657, c("LGP_AVG", "GMIA_V5", "cass_v"), by=2, iso3="GHA")
#'
#' # Equivalent cUrl request at the command line
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/similar/json \
#' # -d '{"x" : 16657, "var" : ["LGP_AVG", "GMIA_V5", "cass_v"], "by" : 2, "iso3" : "GHA"}' \
#' # -X POST -H "Content-Type:application/json"
#'
#' @return a data.table of regions, districts, or pixels ranked by similarity to
#' a reference geography \code{x}
#' @export
similar <- function(x, var, by=0, iso3="SSA") {

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

