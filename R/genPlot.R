#' Plot HarvestChoice 5-arc-minute spatial indicators
#'
#' Method to plot HarvestChoice rasters with mutiple symbology options.
#' See examples below.
#'
#' \figure{PD12_TOT.png}{options: width="4in"}
#' \figure{AEZ16_CLAS.GHA.print.png}{options: width="4in"}
#' \figure{whea_h.GHA.print.png}{options: width="4in"}
#' \figure{FS_2012.png}{options: width="4in"}
#' \figure{yield_l_cv.png}{options: width="4in"}
#' \figure{soc_d15.png}{options: width="4in"}
#'
#' @param var character array of variable codes to plot
#' @param iso3 optional ISO3 country or region code to filter by
#' @param pal optional Brewer color palette used for plotting, e.g. "Blues"
#' @param format one of "default", "print", or "thumbnail" to control legend and axes
#' @param style one of \code\link[classInt:classIntervals]{classIntervals} \code{style}
#' options (e.g. "kmeans" or "pretty") or "default" to use default breaks
#'
#' @param n \code\link[classInt:classIntervals]{classIntervals} \code{n} argument
#' to control the number of breaks
#'
#' @param width plot width in pixel (unless \code{units} is specified)
#' @param height plot height in pixel (unless \code{units} is specified)
#' @param ... any argument passed to \code\link[grDevices:png]{png}, e.g. units, res,
#' pointsize
#'
#' @return Array of generated file names, one for each plot
#' @examples
#' # Generate standard raster plot of 2012 population density for sub-Saharan Africa
#' genPlot("PD12_TOT", pal="OrRd")
#'
#' # Generate 3 raster plots for Ghana with legend and title but not axes
#' genPlot(c("AEZ16_CLAS", "whea_h"), iso3="GHA", format="print")
#'
#' # Generate 3 raster plots for Nigeria with the specified dimensions
#' genPlot(c("FS_2012", "yield_l_cv", "soc_d15"), width=5, height=5,
#' units="in", res=200, pointsize=8)
#'
#' @export
genPlot <- function(var, iso3="SSA", pal, format="default", style="default", n,
  width=640, height=640, ...) {

  iso3 <- iso3[1]
  fPath <- NA
  setkey(vi, varCode)

  # Get GAUL country boundaries
  rc <- RS.connect(getOption("hcapi3.host"), getOption("hcapi3.port"), proxy.wait=F)
  g0 <- RS.eval(rc, g0)

  for (i in var) {

    # Quick validation
    cat(i, ": ", vi[i][, varLabel], fill=T)
    if (is.na(vi[i][, varLabel])) next

    # Get HC symbology
    cv <- as.integer(unlist(strsplit(vi[i][, classBreaks], "|", fixed=T)))
    cl <- as.character(unlist(strsplit(vi[i][, classLabels], "|", fixed=T)))
    cc <- tolower(as.character(unlist(strsplit(vi[i][, classColors], "|", fixed=T))))

    # Get data
    r <- getLayer(i, iso3, collapse=F)
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
        if (!missing(pal)) {
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
    if (iso3=="SSA") r <- crop(r, g0)

    # Open plot device
    j <- c(i, if(iso3!="SSA") iso3, if(format!="default") format, "png")
    j <- paste(j, collapse=".")

    png(j, width=width, height=height, ...)

    # Global graphic parameters
    par(family="Helvetica-Narrow", bty="n", cex.axis=.6, col.axis="grey50", fg="grey50")

    switch(format,
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
            vi[i][, varTitle], " (", vi[i][, unit], ") - ", names(iso)[iso==iso3]), 50))
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
            vi[i][, varTitle], " (", vi[i][, unit], ") - ", names(iso)[iso==iso3]), 50))
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

    if (iso3!="SSA") {
      # Also add province boundaries
      g1 <- RS.eval(rc, g1)
      plot(g1[g1$ADM0_NAME==names(iso)[iso==iso3],], col=NA, border="gray", lwd=.1, add=T)
    }

    dev.off()
    fPath <- c(fPath, j)
  }

  # Close connection
  RS.close(rc)
  return(fPath[-1])
}

