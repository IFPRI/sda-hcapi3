#' Plot HarvestChoice 5-arc-minute spatial indicators
#'
#' Method to plot HarvestChoice rasters with mutiple layout and symbology options.
#' See examples below. Note that calling \code{genPlot(...)} is equivalent to calling
#' the convenience function \code{\link{hcapi}(..., format="plot")}.
#'
#' API call: generate 2 plots showing farming systems and 2012 population density for Ghana
#'
#' \code{$ curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi \
#'  -d '{"var":["FS_2012_TX", "PD12_TOT"], "iso3":"GHA", "format":"plot"}' \
#'  -X POST -H 'Content-Type:application/json'
#' --
#' /ocpu/tmp/x03d5aa8e98/R/.val
#' /ocpu/tmp/x03d5aa8e98/stdout
#' /ocpu/tmp/x03d5aa8e98/source
#' /ocpu/tmp/x03d5aa8e98/console
#' /ocpu/tmp/x03d5aa8e98/info
#' /ocpu/tmp/x03d5aa8e98/files/DESCRIPTION
#' /ocpu/tmp/x03d5aa8e98/graphics/1
#' /ocpu/tmp/x03d5aa8e98/graphics/2
#' }
#'
#' GET all generated plots in a ZIP archive
#'
#' \code{
#' $ wget http://hcapi.harvestchoice.org/ocpu/tmp/x03d5aa8e98/graphics/last/png?width=640
#' $ wget http://hcapi.harvestchoice.org/ocpu/tmp/x03d5aa8e98/graphics/last/svg
#' }
#'
#' @param var character array of variable codes to plot
#' @param iso3 optional ISO3 country or region code(s)
#' @param pal optional Brewer color palette used for plotting, e.g. "Blues"
#' @param layout one of "default" or "thumbnail" to control legend and axes
#' @param style passed to \code{\link[classInt:classIntervals]{classIntervals}}
#' @param ... any argument passed to \code{\link[classInt:classIntervals]{classIntervals}},
#' e.g. \code{n=9} and \code{dataPrecision=0}.
#'
#' @seealso \link{hcapi} link\[classInt:classIntervals]{classIntervals}
#' @return Plot
#' @examples
#' # Generate standard raster plot of 2012 population density for sub-Saharan Africa
#' genPlot("PD12_TOT", pal="YlOrRd", style="pretty", n=8)
#'
#' # Generate 3 raster plots for Ghana with legend and title but not axes
#' genPlot(c("AEZ16_CLAS", "cass_h"), iso3="GHA", layout="print")
#'
#' # Generate 3 raster plots for Nigeria with the specified dimensions
#' hcapi(c("FS_2012", "yield_l_cv", "soc_d15"), iso3="NGA", format="plot")
#'
#' @export
genPlot <- function(var, iso3="SSA", pal=character(0), layout="default", style=NULL, ...) {

  fPath <- character(0)
  layout <- match.arg(layout, c("default", "thumbnail"))

  # Get GAUL country boundaries
  rc <- RS.connect(port=getOption("hcapi3.port"), proxy.wait=F)
  g0 <- RS.eval(rc, g0)

  for (i in var) for (ii in iso3) {

    # Get HC symbology
    cv <- as.integer(unlist(strsplit(vi[i, classBreaks], "|", fixed=T)))
    cl <- as.character(unlist(strsplit(vi[i, classLabels], "|", fixed=T)))
    cc <- tolower(as.character(unlist(strsplit(vi[i, classColors], "|", fixed=T))))

    # Get data
    r <- getLayer(i, iso3=ii, collapse=F)
    setnames(r, i, "var")

    switch(vi[i, type],
      class = {
        # Convert categorical rasters to 1-based integer
        r[, var := as.integer(factor(var, levels=cl, ordered=T))]
      },

      continuous = {
        if (!missing(style)) {
          # Re-classify using classIntervals()
          require(classInt)
          cv <- classIntervals(r$var, ...)$brks
        }

        # Classify to 1-based integer using `cv`
        cv <- sort(unique(c(min(r$var, na.rm=T)-1, cv, max(r$var, na.rm=T)+1)))
        r[, var := cut(var, cv)]
        cl <- levels(r$var)
        cl <- sapply(strsplit(cl, ",", cl, fixed=T), `[`, 2)
        cl <- as.numeric(gsub("]", "", cl, fixed=T))
        cl <- prettyNum(cl, big.mark=",")
        r[, var := as.integer(var)]

        # Plot with HC symbology
        if (length(pal)>0) {
          cc <- colorRampPalette(brewer.pal(9, pal))(255)
        } else {
          cc <- colorRampPalette(cc)(255)
        }
      })

    # Convert to spatial
    r <- SpatialPixelsDataFrame(r[, list(X, Y)], data.frame(layer=r$var),
      tolerance=0.00564023, proj4string=CRS("+init=epsg:4326"))
    r <- raster(r)

    # Annotations
    txt1 <- str_wrap(paste0(vi[i, varTitle], " (", vi[i, unit], ")  - ", names(iso)[iso==ii]), 60)
    txt2 <- str_wrap(paste0(vi[i, sources], " \u00a9HarvestChoice/IFPRI, 2015."), 90)

    # Set global graphic parameters
    par(bty="n", cex.main=.9, cex.sub=.7, cex.axis=.6, col.axis="grey50", fg="grey50")

    switch(layout,
      default = {

        # Plot with axes (default)
        plot(r, legend=F, col=cc, axes=F)

        # Add gridlines
        axis(1, tck=1, lty=3, lwd=.5, col="grey80")
        axis(2, tck=1, lty=3, lwd=.5, col="grey80")

        # Add legend
        plot(r,
          legend.only=T, legend.shrink=1, legend.width=1, col=cc,
          legend.args=list(NA, side=4, font=2, line=2.3),
          axis.args=list(at=1:length(cl), labels=cl, col.axis="grey10"))

        # Add annotations
        title(main=txt1, col.main="grey10", adj=0, font.main=1, line=1)
        title(sub=txt2, col.sub="grey10", adj=0, line=(nchar(txt2) %/% 100)+2, font.sub=1)
      },

      thumbnail = {
        # Set margins
        par(xpd=T, xaxs="i", yaxs="i")
        # Remove axes and legend (need to use image() instead of plot())
        image(r, col=cc, asp=1, axes=F)
      }
    )

    if (ii!="SSA") {
      # Also add province boundaries
      g1 <- RS.eval(rc, g1)
      plot(g1[g1$ADM0_NAME==names(iso)[iso==ii],], col=NA, border="gray40", lwd=.1, add=T)
    }

    # Always add country boundaries
    plot(g0, col=NA, border="gray30", lwd=.2, add=T)

  }

  RS.close(rc)
}

