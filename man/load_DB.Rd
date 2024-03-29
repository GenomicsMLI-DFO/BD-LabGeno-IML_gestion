% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Connect_DB.R
\name{load_DB}
\alias{load_DB}
\title{Upload DB table}
\usage{
load_DB(table = "00_Liste_projets", DB = "LabGeno")
}
\arguments{
\item{table}{Name of the table you want to load.}

\item{DB}{Name of the OBDC database, as set in your computer.}
}
\description{
Function to upload one of the table in database
}
\details{
Possible tables are :  00_Liste_projets, 01_Groupes, 02_Specimens, 03_Tissus, 04_Extraits_ADN_ARN, 05_Analyse_Externe, 06_Sexage, 07_Sequencage, 08_Hormones, 09_GQ
}
\examples{
# Not to run
res <-load_DB()
head(res)
}
\seealso{
List references
}
