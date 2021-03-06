#' Generate histogram and boxplot of HarvestChoice indicators
#'
#' Display a histogram and univariate statistics for any selected HarvestChoice variable(s).
#' See examples below. Note that calling \code{stats(...)} is equivalent to calling the
#' convenience method \code{hcapi(..., format="stats")}.
#'
#' # API call: generate 2 plots showing farming systems and 2012 population density in Ghana

#' \code{$ curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/stats \
#'  -d '{"var":["bmi", "cass_y"]}' \
#'  -X POST -H 'Content-Type:application/json'
#'  --
#' /ocpu/tmp/x09db409895/R/.val
#' /ocpu/tmp/x09db409895/graphics/1
#' /ocpu/tmp/x09db409895/graphics/2
#' /ocpu/tmp/x09db409895/source
#' /ocpu/tmp/x09db409895/console
#' /ocpu/tmp/x09db409895/info
#' /ocpu/tmp/x09db409895/files/DESCRIPTION
#' }
#'
#' Use wget (at the command line) to download the generated plot(s)
#'
#' \code{$ wget http://hcapi.harvestchoice.org/ocpu/tmp/x09db409895/graphics/1/png
#' $ wget http://hcapi.harvestchoice.org/ocpu/tmp/x09db409895/graphics/2/png
#' }
#'
#' @param var character array of variable codes to plot
#' @param iso3 optional country or region filter (3-letter code)
#' @return plot
#' @examples
#' # Plots of BMI and cassava yield distribution over sub-Saharan Africa
#' stats(c("bmi", "cass_y"))
#'
#' @export
stats <- function(var, iso3="SSA", by=NULL) {

  d <- getLayer(var, iso3, by)

  for(i in var) {
    t <- summary(d[[i]])
    p <- hist(d[[i]], plot=F)
    b <- (max(p$counts)-min(p$counts))/20

    par(adj=0, font.main=1, cex.lab=.4)
    hist(d[[i]], n=30, xlab=NA, ylab=NA, ylim=c(-b, max(p$counts)),
      col="grey90", border="white",
      main=paste(vi[varCode==i, varTitle], names(iso)[iso==iso3], sep="\n"))
    boxplot(d[[i]], horizontal=T, at=-b, border="red", boxwex=b*2, axes=F, outline=F, add=T)
    legend(x="topright", legend=paste(c("N", names(t)), c(dim(d)[1], t), sep=":   "),
      bg=NA, box.col=NA)
  }
}
