#' Generate generic plots of HarvestChoice 5-arc-minute spatial indicators
#'
#' Method to plot HarvestChoice rasters with mutiple layout and symbology options.
#' See examples below. Note that calling \code{genPlot(...)} is equivalent to calling
#' the convenience method \code{\link{hcapi}(..., format="plot")}.
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
#' GET individual plots or all generated plots in a ZIP archive
#'
#' \code{
#' $ wget http://hcapi.harvestchoice.org/ocpu/tmp/x03d5aa8e98/graphics/1/png?width=640
#' $ wget http://hcapi.harvestchoice.org/ocpu/tmp/x03d5aa8e98/graphics/last/svg
#' $ wget http://hcapi.harvestchoice.org/ocpu/tmp/x03d5aa8e98/zip
#' }
#'
#' @param var character array of variable codes to plot
#' @param iso3 optional ISO3 country or region code(s)
#' @param pal optional Brewer color palette used for plotting, e.g. "Blues"
#' @param layout one of "default" or "thumbnail" to control legend and axes
#'
#' @seealso \link{hcapi} link\[classInt:classIntervals]{classIntervals}
#' @return raster plot
#' @examples
#' # Generate standard raster plot of 2012 population density for Ethiopia
#' genPlot("PD12_TOT", iso3="ETH", pal="YlOrRd")
#'
#' # Generate 2 raster plots for Ghana (thumbnail format with no legend)
#' genPlot(c("AEZ16_CLAS", "cass_h"), iso3="GHA", layout="thumbnail")
#'
#' # Plot farming systems and yield variability across SSA
#' hcapi("FS_2012_TX", format="plot")
#' hcapi("yield_l_cv", format="plot")
#'
#' @export
genPlot <- function(var, iso3="SSA", pal=character(0), layout="default") {

  layout <- match.arg(layout, c("default", "thumbnail"))

  # Get GAUL country boundaries
  rc <- RS.connect(port=getOption("hcapi3.port"), proxy.wait=F)

  for (i in var) for (ii in iso3) {

    # Get HC symbology
    cv <- as.numeric(unlist(strsplit(vi[i, classBreaks], "|", fixed=T)))
    cl <- as.character(unlist(strsplit(vi[i, classLabels], "|", fixed=T)))
    cc <- tolower(as.character(unlist(strsplit(vi[i, classColors], "|", fixed=T))))

    # Get the data
    r <- getLayer(i, iso3=ii, collapse=F)
    setnames(r, i, "var")
    # Remove buffer pixels
    if ("ADM1_NAME_ALT" %in% names(r)) r <- r[ADM1_NAME_ALT != "buffer gridcell"]
    if ("ADM2_NAME_ALT" %in% names(r)) r <- r[ADM2_NAME_ALT != "buffer gridcell"]

    switch(vi[i, type],

      class = {
        # Make sure classes are in the same order as colors
        r[, var := factor(var, levels=cl, ordered=T)]
      },

      continuous = {
        if (length(pal)>0) {
          # Plot with selected palette
          cc <- colorRampPalette(brewer.pal(9, pal))(100)
        } else {
          # Plot with default HC symbology
          cc <- colorRampPalette(cc)(100)
        }
      })

    # Convert to spatial
    r <- SpatialPixelsDataFrame(r[, list(X, Y)], data.frame(layer=r$var),
      proj4string=CRS("+init=epsg:4326"))

    # Annotations
    txt1 <- str_wrap(paste0(vi[i, varTitle], " (", vi[i, unit], ")  - ", names(iso)[iso==ii]), 60)
    txt2 <- str_wrap(paste0(vi[i, sources], " \u00a9HarvestChoice/IFPRI, 2016."), 70)

    # Set global graphic parameters
    par(bty="n", cex.main=.9, cex.sub=.8, cex.axis=.8, col.axis="grey30", fg="grey30")

    switch(layout,

      default = {
        # Plot with scale (default)
        plot(r, col=cc,
          scale.size=lcm(ifelse(length(cc)==100, 2.5, 5)),
          scale.frac=ifelse(length(cc)==100, 0.2, 0.1))

        # Add annotations
        title(main=txt1, col.main="grey10", adj=0, font.main=1, line=1)
        title(sub=txt2, col.sub="grey10", adj=0, line=(nchar(txt2) %/% 100)+4, font.sub=1)
      },

      thumbnail = {
        plot(r, col=cc, what="image")
      })

    if (ii=="SSA") {
      # Add country boundaries
      g0 <- RS.eval(rc, g0)
      plot(g0[g0$iso_a3 %in% iso,], col=NA, border="gray20", lwd=.2, add=TRUE)

    } else {
      # Add province boundaries
      g1 <- RS.eval(rc, g1)
      plot(g1[g1$ADM0_NAME==names(iso)[iso==ii],], col=NA, border="gray30", lwd=.1, add=TRUE)
    }
  }

  RS.close(rc)
}

