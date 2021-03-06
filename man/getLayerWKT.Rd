% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getLayerWKT.R
\name{getLayerWKT}
\alias{getLayerWKT}
\title{Summarize HarvestChoice indicators over WKT geometri(es)}
\usage{
getLayerWKT(var, wkt, ...)
}
\arguments{
\item{var}{character array of variable names to summarize (all types are accepted)}

\item{wkt}{unprojected (epsg:4326) WKT representation of a spatial object (points,
multipoints, or polygons)}

\item{...}{other arguments passed to \code{\link{getLayer}} (e.g. \code{by},
\code{collapse}, \code{format})}
}
\value{
a data.table (or other \code{format}) of \code{var} indicators summarized
across \code{wkt} geometri(es)
}
\description{
This function intersects (extracts) any spatial object (WKT point, multipoints,
polygons or multipolygons) with HarvestChoice 5-arc-minute grid and returns an array
of intersected gridcell IDs (CELL5M codes) to pass along as argument to \code{\link{getLayer}}.
}
\examples{
# Mean harvested maize area summarized across a custom polygon
x <- getLayerWKT("maiz_h", wkt="POLYGON((-16.35819663578485006 15.36599264077935345,
-15.42501860768386379 15.69472580976947462, -15.11749403024149174 14.83577785208561117,
-16.13550642453347805 14.68731771125136376, -16.35819663578485006 15.36599264077935345))")
x

}
