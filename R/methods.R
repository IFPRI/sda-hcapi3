#####################################################################################
# Title: CELL5M Methods
# Date: March 2014
# Project: HarvestChoice
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

# Require `dt` and `vi` in (moved to OpenCPU .Renviron)
# .onLoad <- function(libname, pkgname) {
#   if ( is.null(getOption("hcapi3.dataPath")) | is.null(getOption("hcapi3.metaPath")) ) {
#     cat('Please define the paths to HarvestChoice data snapshots using
# options(hcapi3.dataPath="", hcapi3.metaPath="")
# before loading this package.')
#   }
#   else {
#     # Load data into .hcapi
#     #assign("dt", readRDS(getOption("hcapi3.dataPath")), envir=globalenv())
#     load(getOption("hcapi3.metaPath"), envir=globalenv())
#   }
#
# }

#' Subset, and/or aggregate HarvestChoice 5-arc-minute layers
#'
#' Workhorse method to subset and/or aggregate HarvestChoice layers.
#' This method also aggregates classified variables by continuous variables.
#'
#' @param var character array of variable names
#' @param iso3 optional country or regional filter (3-letter code)
#' @param by optional character array of variables to group by
#' @param collapse if FALSE always return all pixel values (useful for plotting)
#' @return a data.table of \code{varCode} aggregated by \code{by}
#' @export
getLayer <- function(var, iso3="SSA", by=NULL, collapse=TRUE) {

  require(data.table)

  # Don't duplicate variables
  setkey(vi, varCode)
  vars <- c(g, var, by)
  vars <- unique(vars)
  vars <- vars[!is.na(vars)]
  data <- dt[, .SD, .SDcols=vars]
  data <- data[!is.na(X)]
  setkey(data, ADM0_NAME, ADM1_NAME_ALT, ADM2_NAME_ALT)
  if ( iso3 != "SSA") data <- data[ISO3==iso3]

  if ( length(by)>0 ) {
    # If `by` include continuous variables, then auto-classify before grouping
    # Beware that aggregation formulas typically require additional variables from `dt`
    bynum <- vi[by][!type=="class", varCode]

    if ( length(bynum)>0 ) {
      # Classify using `classBreaks`
      byclass <- sapply(bynum, function(i) {
        b <- as.integer(unlist(strsplit(vi[i][, classBreaks], "|", fixed=T)))
        paste0(i, "=cut(", i, ", c(", paste0(b, collapse=", "), "), ordered_result=T)")
      })

      by <- c(vi[by][type=="class", varCode], byclass)
      by <- by[!is.na(by)]
      by <- paste(by, collapse=", ")
    }

    # Construct generic aggregation formula
    setkey(vi, varCode)
    agg <- vi[var][, aggFunR]
    agg <- paste(var, agg, sep="=")
    agg <- paste(agg, collapse=", ")

    if ( collapse==F ) {
      # Aggregate but don't collapse (for plotting and raster formats)
      agg <- paste0("CELL5M, X, Y, ",  paste0(agg, collapse=", "))
    }

    data <- eval(parse(text=paste0("dt[order(", by, ")",
                                   ifelse(iso3!="SSA", "ISO3==iso3", ""),
                                   ", list(", agg, "), by=list(", by, ")]")))
  }

  # Rounding (ugly but fast)
  var <- names(data)[sapply(data, is.numeric)]
  for (i in var) eval(parse(text=paste0("data[, ", i, " := round(", i, ", ", vi[i][, dec], ")]")))
  return(data)
}


#' Convert CELL5M layers to multiple formats and package for download
#'
#' @param var character array of CELL5M variable names
#' @param iso3 optional country or regional filter (3-letter code)
#' @param by optional character array of CELL5M variable to group by
#' @param format output format c("csv", "geojson", "tif", "dta", "asc", "rds", "RData")
#' @param ... any argument passed to getLayer()
#' @return character, path to generated ZIP file
#' @export
makeZip <- function(var, iso3="SSA", by=NULL,
                    format=c("csv", "geojson", "tif", "shp", "dta", "asc", "rds", "rdata")) {

  require(data.table)
  require(foreign)
  require(rgdal)
  require(rgeos)
  require(raster)

  setkey(vi, varCode)
  fPath <- paste0(var[1], "-", paste0(by, collapse="-"), iso3, format)

  if ( !file.exists(paste0(fPath, ".zip")) ) {
    # File does not exist on disk, so create

    if ( format %in% c("asc", "tif", "rdata") ) {
      # Raster formats take more work
      # Process only the first layer (not all formats support multibands)
      var <- var[1]
      d <- getLayer(var, iso3, by, collapse=F)
      cl <- as.character(unlist(vi[var][, strsplit(classLabels, "|", fixed=T)]))
      cc <- as.character(unlist(vi[var][, strsplit(classColors, "|", fixed=T)]))
      ct <- "Float32"

      if ( vi[var][, type] == "class" )  {
        # If categorical raster, then convert to 0-based integer and add labels
        d[[var]] <- as.integer(factor(d[[var]], levels=cl, ordered=T))-1L
        ct <- "Int16"
      }

      # Convert to spatial
      d <- SpatialPixelsDataFrame(d[, list(X, Y)], d,
                                  tolerance=0.00360015, proj4string=CRS("+init=epsg:4326"))
    }

    switch (format,
            # RData raster
            rdata = save(raster(d, layer=var), file=paste0(fPath, ".rdata"), compress=T),

            # GeoTIFF
            tif = writeGDAL(d[, var], paste0(fPath, ".tif"), driver="GTiff",
                            mvFlag=-9999, type=ct, catNames=list(cl), colorTables=list(cc), setStatistics=T,
                            options=c("INTERLEAVE=BAND", "TFW=YES", "ESRI_XML_PAM=YES")),

            # ESRI ASCII
            asc = writeGDAL(d[, var], paste0(fPath, ".asc"), driver="AAIGrid",
                            mvFlag=-9999, type=ct, catNames=list(cl), colorTables=list(cc), setStatistics=T,
                            options=c("INTERLEAVE=BAND", "TFW=YES", "ESRI_XML_PAM=YES")),

            # ESRI Shapefile
            shp = {
              d <- getLayer(var, iso3, by, collapse=F)
              d <- SpatialPointsDataFrame(d[, list(X, Y)], d,
                                          proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
              writeOGR(d, dirname(fPath), basename(fPath), driver="ESRI Shapefile") },

            # GeoJSON
            geojson = {
              d <- getLayer(var, iso3, by, collapse=F)
              d <- SpatialPointsDataFrame(d[, list(X, Y)], d,
                                          proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
              writeOGR(d, paste0(fPath, ".geojson"), var[1], driver="GeoJSON") },

            # Stata (note var.labels still don't seem to work)
            dta = {
              d <- getLayer(var, iso3, by)
              setattr(d, "var.labels", vi[names(d)][, paste0(varLabel, " (", unit, ")")])
              setattr(d, "datalabel", "Produced by HarvestChoice/IFPRI at http://api.harvestchoice.org/. Contact <info@harvestchoice.org>. Written by R.")
              setattr(d, "time.stamp", Sys.Date())
              write.dta(d, paste0(fPath, ".dta"), version=10L) },

            # RDS
            rds = {
              d <- getLayer(var, iso3, by)
              attr(d, "var.labels") <- vi[names(d)][, varLabel]
              attr(d, "datalabel") <- "Produced by HarvestChoice/IFPRI at http://api.harvestchoice.org/. Contact <info@harvestchoice.org>. Written by R."
              attr(d, "time.stamp") <- as.character(as.Date(Sys.Date()))
              saveRDS(d, file=paste0(fPath, ".rds"), compress=T) },

            # CSV (default)
            csv = {
              d <- getLayer(var, iso3, by)
              write.csv(d, paste0(fPath, ".csv"), row.names=F, na="") }
    )

    f <- list.files(dirname(fPath), paste0(basename(fPath), ".*"), full.names=T)
    f <- c(f, genCitation(names(d)))
    zip(paste0(fPath, ".zip"), f, flags="-9Xjm")
  }

  return(paste0(basename(fPath), ".zip"))
}


#' Generate HarvestChoice data use terms, citation, and variable metadata
#'
#' @param var character array of CELL5M variable names
#' @return character, path to generated README file
#' @export
genCitation <- function(var) {

  require(data.table)
  require(stringr)

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
  fPath <- tempfile(pattern="README-", tmpdir="./")
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
genPlot <- function(var, iso3="SSA", pal, format="default", legend="default", ...) {

  require(RColorBrewer)
  require(raster)
  require(sp)
  require(stringr)
  require(data.table)

  # Get HC symbology
  setkey(vi, varCode)
  cv <- as.integer(unlist(strsplit(vi[var][, classBreaks], "|", fixed=T)))
  cl <- as.character(unlist(strsplit(vi[var][, classLabels], "|", fixed=T)))
  cc <- tolower(as.character(unlist(strsplit(vi[var][, classColors], "|", fixed=T))))

  # Convert data to spatial
  r <- getLayer(var, iso3, collapse=FALSE, ...)

  # Convert categorical rasters to 0-based integer
  # Note: cv was created 0-based to match ESRI ASCII format
  if ( vi[var][, type=="class"] ) r[[var]] <- as.integer(factor(r[[var]], levels=cl, ordered=T))-1L

  r <- SpatialPixelsDataFrame(r[, list(X, Y)], data.frame(layer=r[[var]]),
                              tolerance=0.00564023, proj4string=CRS("+init=epsg:4326"))
  r <- raster(r)

  # Plot with HC symbology
  if ( !missing(pal) ) cc <- colorRampPalette(brewer.pal(9, pal))(length(cc))

  if ( vi[var][, type=="class"]) {
    # Categorical variable
    args <- list(at=cv+.5, labels=cl, col="black", col.axis="black")

  } else {
    # Continuous variable
    if ( length(by)>0 ) {
      # Use all classified values
      cv <- unique(getValues(r))
      args <- NULL

    } else if ( legend=="auto" ) {
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

    if ( cc[1]=="#ffffffff" ) cc <- cc[-1]
  }

  # Global graphic parameters
  par(bty="n", family="Helvetica-Narrow", cex.axis=.8, cex.sub=.9, font.main=1, adj=0)

  switch (format,
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
  data(g0)
  plot(g0, col=NA, border="dimgray", lwd=.1, add=T)

  if ( iso3!="SSA") {
    # Also add province boundaries
    data(g1)
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
writePlot <- function(var, iso3="SSA", format="default",
                      width=switch(format, 640, "print"=5, "thumbnail"=180),
                      height=switch(format, 640, "print"=5, "thumbnail"=200),
                      units=switch(format, "px", "print"="in", "thumbnail"="px"),
                      res=switch(units, "in"=200, "px"=72),
                      cache=TRUE, ...) {

  width <- as.integer(width)
  height <- as.integer(height)
  res <- as.integer(res)

  for ( i in var ) {
    fPath <- paste0(format, "-", width, "x", height, units, res, i, iso3, ".png")

    if ( !file.exists(fPath) | cache!=TRUE ) {
      # File does not exist on disk, so create
      png(fPath, width=width, height=height, res=res, units=units,
          pointsize=round(ifelse(units=="in", (width*11)/(640/72), (width/res)*11/(640/72))))

      switch(format,
             # Optimize plot margin sizes
             "default" = par(mar=c(7.1,3.1,4.1,1.1), oma=c(0,0,0,6)),
             "print" = par(mar=c(6.1,3.1,6.1,1.1), oma=c(0,0,2,6), xaxs="i", yaxs="i"),
             "thumbnail" = par(mar=c(0,0,0,0), oma=c(0,0,0,0)))

      genPlot(i, iso3, format=format, ...)
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

  for ( i in var ) {
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
#' @param format one of "data.table" (default), "json", or "html" for a human-readable table
#' @return a data.table of variable categories
#' @export
getGroups <- function(group, format="data.table") {

  require(data.table)

  out <- vi[, .N, keyby=list(Category=cat1, Subcategory=cat2, Item=cat3)]

  if ( !missing(group) ) {
    out <- out[tolower(Category) %like% tolower(group) |
                 tolower(Subcategory) %like% tolower(group) |
                 tolower(Item) %like% tolower(group)]
  }

  if ( format=="json" ) {
    out <- split(out, out$Category)
    out <- lapply(out, function(x) split(x, x$Subcategory))
  }
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

  require(data.table)

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
  if ( !missing(var) ) out <- out[Code %in% var]
  if ( !missing(version) ) out <- out[Version==paste0("SChEF r", version)]
  if ( raster==T ) out <- out[isRaster==T]
  if ( !missing(group) ) {
    out <- out[tolower(Category) %like% tolower(group) |
                 tolower(Subcategory) %like% tolower(group) |
                 tolower(Item) %like% tolower(group)]
  }

  if ( css=="carto" ) {
    # Add CartoCSS
    out[, `:=` (classBreaks=NULL, classLabels=NULL, classColors=NULL)]
    out <- out[isRaster==T]
    mss <- sapply(out$Code, genCartoCSS)
    out[, CartoCSS := mss]
    out[, GeoTiff := paste0("/v2.0/core/?format=tif&varCode=", Code)]
  }

  setkey(out, Category, Subcategory, Item)

  if ( by.group ) {
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
genCartoCSS <- function(var, pal="BuGn", legend=TRUE, ...) {
  # Generate CartoCSS .mss for Mapnik
  require(data.table)
  require(RColorBrewer)

  setkey(vi, varCode)
  var <- var[1]
  d <- getLayer(var, collapse=TRUE, ...)

  # With fixed symbology
  cc <- unlist(vi[var][, strsplit(classColors, "|", fixed=T)])
  cv <- as.numeric(unlist(vi[var][, strsplit(classBreaks, "|", fixed=T)]))
  if ( vi[var][, type]=="continuous" ) cv <- c(cv, ceiling(max(d[[var]], na.rm=T)))
  cl <- unlist(vi[var][, strsplit(classLabels, "|", fixed=T)])

  if ( is.na(cv) | legend==F ) {
    # Symbology is missing or use default symbology

    if ( vi[var][, type]=="class" ) {
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



#' Rank regions, districts, or pixels by similarity to a reference unit for a set of indicators
#'
#' @param x region (ADM1_CODE), district (ADM2_CODE), or pixel code (CELL5M)
#' @param var character array of HarvestChoice variable codes
#' @param level reference geography, 1 for regions, 2 for districts, 0 for pixel)
#' @param iso3 optional country or region filter (3-letter code)
#' @return a data.table of regions, districts, or pixels ranked by similarity to \code{x}
#' @export
rankSimilar <- function(x, var, by=0, iso3="SSA") {

  require(data.table)
  setkey(vi, varCode)

  by <- switch(by,
               `1`="ADM1_CODE_ALT",
               `2`="ADM2_CODE_ALT",
               `0`="CELL5M")

  # Get values for reference unit
  out <- getLayer(var, iso3, by)
  out <- eval(parse(text=paste0("out[", by, "!=0]")))
  ref <- eval(parse(text=paste0("out[", by, "==x]")))

  # Compute aggregate distance and return top 10 homologous units
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


