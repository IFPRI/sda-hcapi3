#' Summarize HarvestChoice 5-arc-minute layers over WKT geometri(es)
#'
#' This function intersects any WKT with HarvestChoice 5-arc-minute grid and returns
#' a list of intersected gridcell IDs (CELL5M codes) typically to pass on as an argument
#' to \code{getLayer()}.
#'
#' @param var character array of variable names (all types are accepted)
#' @param wkt WKT representation of a spatial object (points, multipoints, or polygons)
#' @param ... other arguments passed to \code{getLayer()} (by, iso3, collapse)
#' @return a data.table of \code{var} indicators summarized across \code{wkt} geometry
#' @export
getLayerWKT <- function(var, wkt, ...) {

  # Eval in Rserve socket
  rc <- RS.connect(proxy.wait=F)
  d <- RS.eval(rc, dt[, list(X,Y,CELL5M)])
  RS.close(rc)

  d <- SpatialPixelsDataFrame(d[, list(X, Y)], data.frame(CELL5M=d$CELL5M),
    tolerance=0.00360015, proj4string=CRS("+init=epsg:4326"))
  d <- raster(d)

  wkt <- readWKT(wkt, p4s=CRS("+init=epsg:4326"))
  out <- extract(d, wkt, small=T)
  out <- out[!is.na(out)]
  out <- unique(out)
  out <- getLayer(var=var, ids=out, ...)
  return(out)
}