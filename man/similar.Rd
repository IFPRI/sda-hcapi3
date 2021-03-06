% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/similar.R
\name{similar}
\alias{similar}
\title{Rank spatial units by similarity}
\usage{
similar(x, var, by = 0, iso3 = "SSA")
}
\arguments{
\item{x}{single integer code for the reference geography, either a region code
(ADM1_CODE_ALT), district code (ADM2_CODE_ALT), or gridcell code (CELL5M) to rank against}

\item{var}{character array of HarvestChoice variable codes used in the ranking}

\item{by}{single integer indicating the type of reference geography (0-gridcell,
1-region, 2-district)}

\item{iso3}{optional country or region filter (3-letter code)}
}
\value{
a data.table of regions, districts, or pixels ranked by similarity to
a reference geography \code{x}
}
\description{
Rank regions, districts, or pixels by similarity to a reference unit across a set of
indicators. To generate a distance score, we center and scale all numeric variables
and convert classified variables to 0 or 1 (1 if same as reference unit, 0 otherwise).
For each variable we compute the absolute percent deviation between each unit and the
reference unit, and we then sum the deviations across all variables to generate an
aggregate distance score. The lower the score, the more similar the unit for the
selected indicators.
}
\details{
API call: rank Ghana districts by similarity to district 16657  (Kpando) based on
length of growing period, irrigated area, and cassava value of production

\code{$ curl http://hcapi.harvestchoice.org/ocpu/library/hcapi3/R/similar/json \
 -d '{"x" : 16657, "var" : ["LGP_AVG", "GMIA_V5", "cass_v"], "by" : 2, "iso3" : "GHA"}' \
 -X POST -H "Content-Type:application/json"
}
}
\examples{
# Rank all districts in Ghana by similarity to district 16657 (Kpando)
# based on length of growing period (LGP_AVG), irrigated cropland (GMIA_V5),
# and cassava value of production (cass_v).
similar(16657, c("LGP_AVG", "GMIA_V5", "cass_v"), by=2, iso3="GHA")

}
