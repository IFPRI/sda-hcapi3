#' Plot HarvestChoice 5-arc-minute spatial indicators
#'
#' Method to plot HarvestChoice rasters with mutiple layout and symbology options.
#' See examples below. Note that calling \code{genPlot(...)} is equivalent to calling
#' the convenience function \code{hcapi(..., format="png")}.
#'
#' @param var character array of variable codes to plot
#' @param iso3 optional ISO3 country or region code(s)
#' @param pal optional Brewer color palette used for plotting, e.g. "Blues"
#' @param layout one of "default", "print", or "thumbnail" to control legend and axes
#' @param style one of \code{\link[classInt:classIntervals]{classIntervals}} \code{style}
#' @param units one of "px" (default), "in", "cm" or "mm".
#'   Passed to \code{\link[grDevices:png]{png}}
#' @param resolution in ppi, by default set to 300ppi for print layout.
#'   Passed to \code{\link[grDevices:png]{png}}
#'   options (e.g. "kmeans" or "pretty") or "default" to use default breaks
#' @param n \code{\link[classInt:classIntervals]{classIntervals}} \code{n} argument
#'   to control the number of breaks
#' @param width plot width in pixel (unless \code{units} is specified)
#' @param height plot height in pixel (unless \code{units} is specified)
#' @param ... any argument passed to \code{\link[grDevices:png]{png}}, e.g. pointsize
#'
#' @return Array of generated file names, one for each plot
#' @examples
#' # Generate standard raster plot of 2012 population density for sub-Saharan Africa
#' genPlot("PD12_TOT", pal="OrRd")
#'
#' # Generate 3 raster plots for Ghana with legend and title but not axes
#' genPlot(c("AEZ16_CLAS", "whea_h"), iso3="GHA", layout="print")
#'
#' # Generate 3 raster plots for Nigeria with the specified dimensions
#' genPlot(c("FS_2012", "yield_l_cv", "soc_d15"), width=5, height=5,
#' units="in", res=200, pointsize=8)
#'
#' # This method may be called via HTTP POST request using e.g. cUrl at the command line
#' # Return 2 plots showing farming systems and 2012 population density in Ghana
#' # curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/hcapi \
#' # -d '{"var":["FS_2012_TX", "PD12_TOT"], "iso3":"GHA", "format":"png"}' \
#' # -X POST -H 'Content-Type:application/json'
#'
#' # /ocpu/tmp/x03d5aa8e98/R/.val
#' # /ocpu/tmp/x03d5aa8e98/stdout
#' # /ocpu/tmp/x03d5aa8e98/source
#' # /ocpu/tmp/x03d5aa8e98/console
#' # /ocpu/tmp/x03d5aa8e98/info
#' # /ocpu/tmp/x03d5aa8e98/files/DESCRIPTION
#' # /ocpu/tmp/x03d5aa8e98/files/FS_2012_TX.GHA.png
#' # /ocpu/tmp/x03d5aa8e98/files/PD12_TOT.GHA.png
#'
#' # Use wget (at the command line) to download all generated plots
#' # wget http://hcapi.harvestchoice.org/ocpu/tmp/x03d5aa8e98/zip
#'
#' @export
genPlot <- function(var, iso3="SSA", pal=character(0),
  layout="default", style="default", n=integer(0),
  width=switch(layout, default=640, print=5, thumbnail=120),
  height=switch(layout, default=640, print=5, thumbnail=120),
  units=switch(layout, default="px", print="in", thumbnail="px"),
  res=switch(layout, print=300, NA), ...) {

  iso3 <- iso3[1]
  fPath <- character(0)
  setkey(vi, varCode)

  layout <- match.arg(layout, c("default", "print", "thumbnail"))
  style <- match.arg(style, c("default",
    "fixed", "sd", "equal", "pretty", "quantile", "kmeans",
    "hclust", "bclust", "fisher", "jenks"))

  # Get GAUL country boundaries
  rc <- RS.connect(port=getOption("hcapi3.port"), proxy.wait=F)
  g0 <- RS.eval(rc, g0)

  for (i in var) for (ii in iso3) {

    # Get HC symbology
    cv <- as.integer(unlist(strsplit(vi[i][, classBreaks], "|", fixed=T)))
    cl <- as.character(unlist(strsplit(vi[i][, classLabels], "|", fixed=T)))
    cc <- tolower(as.character(unlist(strsplit(vi[i][, classColors], "|", fixed=T))))

    # Get data
    r <- getLayer(i, iso3=ii, collapse=F)
    setnames(r, i, "var")

    switch(vi[i][, type],
      class = {
        # Convert categorical rasters to 1-based integer
        r[, var := as.integer(factor(var, levels=cl, ordered=T))]
      },

      continuous = {
        if (style!="default") {
          # Re-classify using classIntervals()
          require(classInt)
          cv <- classIntervals(r$var, style=style, if(!missing(n)) n=n)$brks
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
          cc <- colorRampPalette(brewer.pal(9, pal))(length(cl))
        } else {
          cc <- colorRampPalette(cc)(length(cl))
        }
      })

    # Convert to spatial
    r <- SpatialPixelsDataFrame(r[, list(X, Y)], data.frame(layer=r$var),
      tolerance=0.00564023, proj4string=CRS("+init=epsg:4326"))
    r <- raster(r)

    # Crop to SSA (the grid was buffered)
    if (ii=="SSA") r <- crop(r, g0)

    # Open plot device
    j <- c(i, if(ii!="SSA") ii, if(layout!="default") layout, "png")
    j <- paste(j, collapse=".")
    png(j, width=width, height=height, units=units, res=res, ...)

    # Set global graphic parameters
    par(family="Helvetica-Narrow", bty="n", cex.axis=.6, col.axis="grey50", fg="grey50")

    switch(layout,
      default = {

        # Set margins
        par(mar=c(6,3,4,1), oma=c(0,0,0,7))

        # Plot with axes (default)
        plot(r, legend=F, col=cc)

        # Add gridlines
        axis(1, tck=1, lty=3, lwd=.5, col="grey80")
        axis(2, tck=1, lty=3, lwd=.5, col="grey80")

        # Add legend
        plot(r, legend.only=T, legend.width=1.5, col=cc,
          axis.args=list(at=1:length(cl), labels=cl, col.axis="grey10"))

        # Add annotations
        title(col.main="grey10", adj=0, font.main=1, line=1,
          main=str_wrap(paste0(
            vi[i][, varTitle], " (", vi[i][, unit], ") - ", names(iso)[iso==ii]), 50))
        title(cex.sub=ifelse(width<10, .7, .8), col.sub="grey10", adj=0, line=5, font.sub=1,
          sub=str_wrap(paste0("Source: ", vi[i][, sources], " \u00a9HarvestChoice/IFPRI, 2015."), 90))
      },

      print = {
        # Set margins
        par(mar=c(6,1,3,1), oma=c(0,0,0,7), xaxs="i", yaxs="i")

        # Remove axes
        plot(r, legend=F, col=cc, axes=F)

        # Add legend
        plot(r, legend.only=T, legend.width=1.5, col=cc,
          axis.args=list(at=1:length(cl), labels=cl, col.axis="grey10"))

        # Add annotations
        title(col.main="grey10", adj=0, font.main=1, line=0,
          main=str_wrap(paste0(
            vi[i][, varTitle], " (", vi[i][, unit], ") - ", names(iso)[iso==ii]), 50))
        title(cex.sub=ifelse(width<10, .7, .8), col.sub="grey10", adj=0, line=2, font.sub=1,
          sub=str_wrap(paste0("Source: ", vi[i][, sources], " \u00a9HarvestChoice/IFPRI, 2015."), 90))
      },

      thumbnail = {
        # Set margins
        par(mar=c(0,0,0,0), oma=c(0,0,0,0), xpd=T, xaxs="i", yaxs="i")

        # Remove axes and legend (need to use image() instead of plot())
        image(r, col=cc, asp=1, axes=F)
      }
    )

    # Always add country boundaries
    plot(g0, col=NA, border="gray50", lwd=.1, add=T)

    if (ii!="SSA") {
      # Also add province boundaries
      g1 <- RS.eval(rc, g1)
      plot(g1[g1$ADM0_NAME==names(iso)[iso==ii],], col=NA, border="gray", lwd=.1, add=T)
    }

    dev.off()
    fPath <- c(fPath, j)
  }

  RS.close(rc)
  return(fPath)
}

