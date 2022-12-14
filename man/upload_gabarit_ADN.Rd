% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Upload_gabarit.R
\name{upload_gabarit_ADN}
\alias{upload_gabarit_ADN}
\title{Upload Excel template}
\usage{
upload_gabarit_ADN(
  path,
  skip = 0,
  specimen = "Specimens",
  groupe = "Groupes",
  tissu = "Tissus",
  extraitADN = "Extraits_ADN_ARN",
  analyse_ext = "Analyse_Externe",
  sexage = "Sexage",
  sequencage = "Sequencage",
  hormone = "Hormones"
)
}
\arguments{
\item{path}{Path to the excel spreadsheet}

\item{skip}{N row to skip. Default value is 1 as there's a warning in the original file}

\item{specimen}{Name of the sheet in the Excel file containing specimen data}

\item{tissu}{Name of the sheet in the Excel file containing tissue data}

\item{analyse_ext}{Name of the sheet in the Excel file containing external analysis info}

\item{sexage}{of the sheet in the Excel file containing the sex determining method}

\item{sequencage}{Name of the sheet in the Excel file containing the sequencage info}

\item{hormone}{Name of the sheet in the Excel file containing the hormone info}

\item{group}{Name of the sheet in the Excel file containing group data}

\item{extrait}{Name of the sheet in the Excel file containing the DNA/RNA extract data}
}
\description{
Function to upload an Excel file based on the predefined template
}
\details{
NULL if you doesn't want a specif sheet to be uploaded. No other check than sheet name.
}
\examples{
# provide some examples of how to use your function


}
\seealso{
List references
}
