% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/radiation_parameters.R
\name{rs_nearby_calculation}
\alias{rs_nearby_calculation}
\title{Solar radiation data from a nearby weather station}
\usage{
rs_nearby_calculation(rs_reg, ra_reg, ra)
}
\arguments{
\item{rs_reg}{A dataframe with the solar radiation at the regional location (MJ m-2 day-1).}

\item{ra_reg}{A dataframe with the extraterrestrial radiation at the regional location (MJ m-2 day-1).}

\item{ra}{A dataframe with the extraterrestrial radiation for daily periods (ra).}
}
\value{
A data.frame object with the Solar radiation data based on a nearby weather station
}
\description{
The solar radiation data is calculated based in a nearby weather station.
}
\examples{
\dontrun{
rs_nearby_df <- rs_nearby_calculation(rs_reg, ra_reg, ra)
}
}
\author{
Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha
}
