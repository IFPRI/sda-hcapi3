% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicator.R
\name{indicator}
\alias{indicator}
\title{Return HarvestChoice indicator metadata}
\usage{
indicator(q, version = NULL, as.list = FALSE, cartoCSS = FALSE)
}
\arguments{
\item{q}{character array of pattern(s) to search against HarvestChoice indicator codes,
labels, and categories. Will attempt a fuzzy match.}

\item{version}{optional version filter}

\item{as.list}{if TRUE return a list of indicators grouped by category}

\item{cartoCSS}{also include formatted CartoCSS rules}
}
\value{
a data.table or hierarchical list of indicator metadata
}
\description{
Return complete metadata records for selected HarvestChoice indicators.
}
\details{
# API call: metadata records for all indicators matching 'cassava' or 'population'

\code{$ curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/indicator/json \
 -d '{"q" : ["cassava", "population"]}' \
 -X POST -H "Content-Type:application/json"
}

API call: metadata records for all indicators matching 'population' in a list

\code{$ curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/indicator/json \
 -d '{"q" : "population", "as.list" : "true"}' \
 -X POST -H "Content-Type:application/json"
}
}
\examples{
# Show complete metadata for all HarvestChoice indicators matching 'cassava' or
'population'
indicator(c("cassava", "population"))

# Show complete metadata for all HarvestChoice indicators matching 'population'
# in a hierarchical list
indicator("population", as.list=TRUE)[1:2]

}
\seealso{
\link{category} to view HarvestChoice indicator categories
}
