% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversion_factor_for_radiations.R
\name{radiation_conversion}
\alias{radiation_conversion}
\title{Conversion factors for radiation}
\usage{
radiation_conversion(data_to_convert, conversion_name)
}
\arguments{
\item{data_to_convert}{A data.frame with radiation values to convert.}

\item{conversion_name}{A character with the conversion_name summarize in the description of this function.}
}
\value{
A data.frame object wit the converted radiation.
}
\description{
Function to convert the radiation data. The conversion name can be understand as follow:

\itemize{
  \item  conversion_1 = MJ m-2 day-1 to J cm-2 day-1;
  \item  conversion_2 = MJ m-2 day-1 to cal cm-2 day-1;
  \item  conversion_3 = MJ m-2 day-1 to W m-2;
  \item  conversion_4 = MJ m-2 day-1 to mm day-1;
  \item  conversion_5 = cal cm-2 day-1 to MJ m-2 day-1;
  \item  conversion_6 = cal cm-2 day-1 to J cm-2 day-1;
  \item  conversion_7 = cal cm-2 day-1 to W m-2;
  \item  conversion_8 = cal cm-2 day-1 to mm day-1;
  \item  conversion_9 =  W m-2 to MJ m-2 day-1;
  \item  conversion_10  = W m-2 to J cm-2 day-1;
  \item  conversion_11  = W m-2 to cal cm-2 day-1;
  \item  conversion_12  = W m-2 to mm day-1;
  \item  conversion_13  = mm day-1 to MJ m-2 day-1;
  \item  conversion_14  = mm day-1 to J cm-2 day-1;
  \item  conversion_15  = mm day-1 to cal cm-2 day-1;
  \item  conversion_16  = mm day-1 to W m-2.
 }
}
\examples{
\dontrun{
radiation_conversion_df <- radiation_conversion(
  data_to_convert = df$rad,
  conversion_name = "conversion_1"
)
}
}
\author{
Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha
}
