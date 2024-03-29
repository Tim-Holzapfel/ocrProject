% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/08_admin_regions.R
\name{gen_admin_regions}
\alias{gen_admin_regions}
\title{Admin Assign}
\usage{
gen_admin_regions()
}
\description{
The most computational expansive, this function assigns the
individual territories/districts to the rulers.
}
\section{General idea}{
The general idea is to compare two distinct data sets, in this case subsets
of the original data set where the first subset consists only of the actual
rulers and the second data set consists only of the administrative areas
(so the second dataset is basically everything but the rulers)
}

\author{
Tim Holzapfel
}
