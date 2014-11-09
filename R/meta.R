#' Generate HarvestChoice data use terms, citation, and variable metadata
#'
#' @param var character array of CELL5M variable names
#' @return character, path to generated README file
#' @export
genReadme <- function(var) {

  meta <- vi[var][, list(
    Code=varCode,
    Label=varLabel,
    Details=str_wrap(paste(varTitle, varDesc, sep=". "), 78),
    Unit=unit,
    Type=type,
    Period=ifelse(is.na(yearEnd), year, paste(year, yearEnd, sep=" - ")),
    Category=cat1,
    `Sub-category`=cat2,
    Item=cat3,
    Source=str_wrap(ifelse(is.na(sources), sourceMini, sources), 79),
    Contact=owner,
    Version=version,
    Citation=str_wrap(citation, 77))]

  meta <- split(meta, meta$Code)
  meta <- sapply(meta, function(x) paste(names(x), x, sep=":\t", collapse="\n"))
  meta <- paste(meta, collapse=paste0("\n", str_dup("-", 86), "\n"))

  readme <- gsub("{date}", Sys.Date(), readme, fixed=T)
  readme <- gsub("{meta}", meta, readme, fixed=T)
  fPath <- "./README"
  writeLines(readme, fPath)
  return(fPath)
}


#' Generate single raster plot of HarvestChoice layer
#'
#' @param var variable to plot (character of length 1)
#' @param iso3 optional ISO3 country code to filter on
#' @param pal optional Brewer color palette used for plotting
#' @param format one of c("default", "print", "thumbnail") control legend and axes
#' @param legend one of c("default", "auto") uses HarvestChoice legend breaks or R default
#' @return plot
#' @export
getPlot <- function(var, iso3="SSA", pal, format="default", legend="default", ...) {

  # Get HC symbology
  var <- var[1]
  setkey(vi, varCode)
  cv <- as.integer(unlist(strsplit(vi[var][, classBreaks], "|", fixed=T)))
  cl <- as.character(unlist(strsplit(vi[var][, classLabels], "|", fixed=T)))
  cc <- tolower(as.character(unlist(strsplit(vi[var][, classColors], "|", fixed=T))))

  # Convert data to spatial
  r <- getLayer(var, iso3, collapse=FALSE, ...)
  r <- r[!is.na(Y)]

  # Convert categorical rasters to 0-based integer
  # Note: cv was created 0-based to match ESRI ASCII format
  if (vi[var][, type=="class"]) r[[var]] <- as.integer(factor(r[[var]], levels=cl, ordered=T))-1L

  r <- SpatialPixelsDataFrame(r[, list(X, Y)], data.frame(layer=r[[var]]),
    tolerance=0.00564023, proj4string=CRS("+init=epsg:4326"))
  r <- raster(r)

  # Plot with HC symbology
  if (!missing(pal)) cc <- colorRampPalette(brewer.pal(9, pal))(length(cc))

  if (vi[var][, type=="class"]) {
    # Categorical variable
    args <- list(at=cv+.5, labels=cl, col="black", col.axis="black")

  } else {
    # Continuous variable
    if ( length(by)>0 ) {
      # Use all classified values
      cv <- unique(getValues(r))
      args <- NULL

    } else if (legend=="auto") {
      # Use default raster breaks
      require(classInt)
      cv <- classIntervals(getValues(r), style="quantile")$brks
      cv <- unique(cv)
      args <- NULL

    } else {
      # Use HC breaks
      cv <- unique(c(0, cv, ceiling(maxValue(r))))
      cv <- cv[order(cv)]
      cv <- cv[which(cv <= ceiling(maxValue(r)))]
      args <- list(at=cv, labels=cl, col="black", col.axis="black")
    }

    if (cc[1]=="#ffffffff") cc <- cc[-1]
  }

  # Global graphic parameters
  par(bty="n", family="Helvetica-Narrow", cex.axis=.8, cex.sub=.9, font.main=1, adj=0)

  switch(format,
    default = {
      # Plot with axes and legend (default)
      plot(r, col=colorRampPalette(cc)(length(cv)), main=NULL,
        axis.args=args, legend.width=1.5, legend.mar=7)

      # Add gridlines
      axis(1, tck=1, lty=3, lwd=.5, col="gray")
      axis(2, tck=1, lty=3, lwd=.5, col="gray")

      # Add annotations
      title(main=str_wrap(paste(vi[var][, varTitle], names(iso)[iso==iso3], sep=", "), width=74))
      title(sub=str_wrap(paste(vi[var][, sources],
        "\u00a9 HarvestChoice/IFPRI, 2014.", sep=" "), 74), line=6) },

    print = {
      # Remove axes
      plot(r, breaks=cv, col=cc, axes=F, main=NULL, axis.args=args,
        legend.width=1.1, legend.height=1)

      # Add annotations
      title(main=paste(str_wrap(vi[var][, varTitle], 70), names(iso)[iso==iso3], sep=", "),
        line=3)
      title(sub=str_wrap(paste(vi[var][, sources], "\u00a9 HarvestChoice/IFPRI, 2014.", sep=" "), 74),
        line=5) },

    thumbnail = {
      # Remove axes and legend
      image(r, breaks=cv, col=cc[-length(cc)], xlim=c(-20,60), ylim=c(-38, 30), xaxs="i", yaxs="i",
        asp=1, axes=F, main=NULL) }
  )

  # Always add country boundaries
  plot(g0, col=NA, border="dimgray", lwd=.1, add=T)

  if (iso3!="SSA") {
    # Also add province boundaries
    plot(g1[g1$ADM0_NAME==names(iso)[iso==iso3],], col=NA, border="gray", lwd=.1, add=T)
  }
}


#' Write layer plot to PNG
#'
#' @param var character array of variables to plot
#' @param iso3 optional country or region filter (3-letter code)
#' @param format format argument passed to genPlot()
#' @param width plot width in units passed to png()
#' @param height plot height in units passed to png()
#' @param res pixel-per-inch passed to png(), used if units="in"
#' @param units one of c("px", "in") passed to png()
#' @param cache if TRUE use image in server cache, if FALSE rebuild image
#' @param ... parameters passed on to genPlot()
#' @return character, path to generated ZIP file
#' @export
genPlot <- function(var, iso3="SSA", format="default",
  width=switch(format, 640, "print"=5, "thumbnail"=180),
  height=switch(format, 640, "print"=5, "thumbnail"=200),
  units=switch(format, "px", "print"="in", "thumbnail"="px"),
  res=switch(units, "in"=200, "px"=72),
  cache=TRUE, ...) {

  width <- as.integer(width)
  height <- as.integer(height)
  res <- as.integer(res)

  for(i in var) {
    fPath <- paste0(format, "-", width, "x", height, units, res, i, iso3, ".png")

    if (!file.exists(fPath) | cache!=TRUE) {
      # File does not exist on disk, so create
      png(fPath, width=width, height=height, res=res, units=units,
        pointsize=round(ifelse(units=="in", (width*11)/(640/72), (width/res)*11/(640/72))))

      switch(format,
        # Optimize plot margin sizes
        "default" = par(mar=c(7.1,3.1,4.1,1.1), oma=c(0,0,0,6)),
        "print" = par(mar=c(6.1,3.1,6.1,1.1), oma=c(0,0,2,6), xaxs="i", yaxs="i"),
        "thumbnail" = par(mar=c(0,0,0,0), oma=c(0,0,0,0)))

      getPlot(i, iso3, format=format, ...)
      dev.off()
    }
  }

  fPath <- paste0(format, "-", width, "x", height, units, res, var[1], iso3, ".png")
  return(fPath)
}


#' Generate histogram and boxplot of HarvestChoice layer(s)
#'
#' @param var character array of variables to summarize
#' @param iso3 optional country or region filter (3-letter code)
#' @return plot
#' @export
genStats <- function(var, iso3="SSA", by=NULL) {

  d <- getLayer(var, iso3, by)

  for(i in var) {
    t <- summary(d[[i]])
    p <- hist(d[[i]], plot=F)
    b <- (max(p$counts)-min(p$counts))/20

    hist(d[[i]], n=30, xlab=NA, ylim=c(-b, max(p$counts)), col="azure3",
      main=paste(vi[varCode==i, varTitle], names(iso)[iso==iso3], sep=" - "))
    boxplot(d[[i]], horizontal=T, at=-b, border="blue", boxwex=b*2, axes=F, outline=F, add=T)
    legend(x="topright", legend=paste(c("N", names(t)), c(dim(d)[1], t), sep=":   "), bg="white")
  }

}

#' Return list of HarvestChoice variable categories
#'
#' @param group optional category filter
#' @return a data.table of variable categories
#' @export
getGroups <- function(group) {

  out <- vi[, list(Title=unique(varTitle)), keyby=list(Category=cat1, Subcategory=cat2, Item=cat3, Code=varCode)]

  if (!missing(group)) {
    out <- out[tolower(Category) %like% tolower(group) |
        tolower(Subcategory) %like% tolower(group) |
        tolower(Item) %like% tolower(group) |
        tolower(Code) %like% tolower(group)]
  }

  out <- split(out, out$Category)
  out <- lapply(out, function(x) split(x, x$Subcategory))
  return(out)
}


#' Return HarvestChoice variable metadata
#'
#' @param var character array of HarvestChoice variables
#' @param group optional category filter
#' @param version optional version filter
#' @param raster return only raster variables
#' @param by.group group variables by category
#' @param css include Carto CSS rules
#' @return a data.table of variable metadata
#' @export
getMeta <- function(var, group, version, raster=FALSE, by.group=FALSE, css="json") {

  out <- vi[, list(
    Label=varLabel,
    Code=varCode,
    Unit=unit,
    Type=type,
    Period=ifelse(is.na(yearEnd), year, paste(year, yearEnd, sep=" - ")),
    Category=cat1,
    Subcategory=cat2,
    Item=cat3,
    Source=ifelse(is.na(sources), sourceMini, sources),
    Contact=owner,
    Details=paste(varTitle, varDesc, sep=". "),
    Citation=citation,
    Version=version,
    `In Table`=tbName,
    Formula=aggFun,
    isRaster=genRaster,
    dTopic,
    dCrop,
    dKeywords,
    classBreaks,
    classLabels,
    classColors,
    Website=ifelse(genRaster, paste0("http://harvestchoice.org/data/", tolower(varCode)), NA),
    WMS=ifelse(genRaster, paste0("http://dev.harvestchoice.org:6080/arcgis/services/", mxdName, "/MapServer/WMSServer"), NA),
    `Downloaded on`=as.character(as.Date(Sys.Date())))]

  # Optional filters
  if (!missing(var)) out <- out[Code %in% var]
  if (!missing(version)) out <- out[Version==paste0("SChEF r", version)]
  if (raster==T) out <- out[isRaster==T]
  if (!missing(group)) {
    out <- out[tolower(Category) %like% tolower(group) |
        tolower(Subcategory) %like% tolower(group) |
        tolower(Item) %like% tolower(group)]
  }

  if (css=="carto") {
    # Add CartoCSS
    out[, `:=` (classBreaks=NULL, classLabels=NULL, classColors=NULL)]
    out <- out[isRaster==T]
    mss <- sapply(out$Code, getCartoCSS)
    out[, CartoCSS := mss]
    out[, GeoTiff := paste0("/v2.0/core/?format=tif&varCode=", Code)]
  }

  setkey(out, Category, Subcategory, Item)

  if (by.group) {
    # Group by category
    setkey(out, Category, Subcategory, Item, Label, Code)
    out <- split(out, out$Category)
    out <- lapply(out, function(x) split(x, x$Subcategory))
    out <- lapply(out, function(x) lapply(x, function(y) split(y, y$Item)))
  }

  return(out)
}


#' Generate CartoCSS snippet to symbolize raster tiles
#'
#' @param var HarvestChoice variable name
#' @param iso3 optional country or region filter (3-letter code)
#' @param pal optional Brewer color palette used for plotting
#' @param legend if TRUE returns HarvestChoice legend, otherwise returns default legend
#' @param ... any argument passed to getLayer(), e.g. by=FS_2012, iso3=GHA
#' @return character, CartoCSS snippet for requested variable
#' @export
getCartoCSS <- function(var, pal="BuGn", legend=TRUE, ...) {

  setkey(vi, varCode)
  var <- var[1]
  d <- getLayer(var, collapse=TRUE, ...)

  # With fixed symbology
  cc <- unlist(vi[var][, strsplit(classColors, "|", fixed=T)])
  cv <- as.numeric(unlist(vi[var][, strsplit(classBreaks, "|", fixed=T)]))
  if ( vi[var][, type]=="continuous" ) cv <- c(cv, ceiling(max(d[[var]], na.rm=T)))
  cl <- unlist(vi[var][, strsplit(classLabels, "|", fixed=T)])

  if (is.na(cv) | legend==F) {
    # Symbology is missing or use default symbology

    if (vi[var][, type]=="class") {
      # Categorical raster
      cl <- levels(factor(d[[var]]))
      cv <- 1:length(cl)-1

    } else {
      # Continuous raster, and we need the max value
      require(classInt)
      cv <- cl <- classIntervals(d[[var]], n=8, style="pretty")$brks
      cv <- c(cv[-1], ceiling(max(d[[var]], na.rm=T)))
    }
    cc <- colorRampPalette(brewer.pal(length(cv), pal))(length(cv))
  }

  # Raster symbology
  out <- paste0("#", tolower(var), "tif",
    " { raster-scaling: bilinear;
    raster-colorizer-default-mode: linear;
    raster-colorizer-default-color: transparent;
    raster-colorizer-stops:
    stop(-9999, transparent, linear)",
    paste0("stop(", cv, ", ", cc, ")", collapse=" "),
    " }")

  out <- gsub("\n", "", out, fixed=T)
  return(out)
}
