#' Summarize HarvestChoice indicators over WKT geometri(es)
#'
#' This function intersects (extracts) any spatial object (WKT point, multipoints,
#' polygons or multipolygons) with HarvestChoice 5-arc-minute grid and returns an array
#' of intersected gridcell IDs (CELL5M codes) to pass along as argument to \code{\link{getLayer}}.
#'
#' @param var character array of variable names to summarize (all types are accepted)
#' @param wkt unprojected (epsg:4326) WKT representation of a spatial object (points,
#' multipoints, or polygons)
#' @param ... other arguments passed to \code{\link{getLayer}} (e.g. \code{by},
#' \code{collapse}, \code{format})
#'
#' @return a data.table (or other \code{format}) of \code{var} indicators summarized
#' across \code{wkt} geometri(es)
#' @examples
#' # Mean harvested maize area summarized across a custom polygon
#' x <- getLayerWKT("maiz_h", wkt="POLYGON((-16.35819663578485006 15.36599264077935345,
#' -15.42501860768386379 15.69472580976947462, -15.11749403024149174 14.83577785208561117,
#' -16.13550642453347805 14.68731771125136376, -16.35819663578485006 15.36599264077935345))")
#' x
#'
#' @export
getLayerWKT <- function(var, wkt, ...) {

  # Eval in Rserve socket
  rc <- RS.connect(getOption("hcapi3.host"), getOption("hcapi3.port"), proxy.wait=F)
  d <- RS.eval(rc, dt[, list(X,Y,CELL5M)])
  RS.close(rc)

  d <- SpatialPixelsDataFrame(d[, list(X, Y)], data.frame(CELL5M=d$CELL5M),
    tolerance=0.00360015, proj4string=CRS("+init=epsg:4326"))
  d <- raster(d)

  wkt <- readWKT(wkt, p4s=CRS("+init=epsg:4326"))
  out <- extract(d, wkt, small=T)
  out <- out[!is.na(out)]
  out <- unique(out)
  out <- getLayer(var=var, ids=out, by="ISO3", ...)
  return(out)
}