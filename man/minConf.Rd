% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FTARM_SupAndConf.R
\name{minConf}
\alias{minConf}
\title{minConf}
\usage{
minConf(DB, searchFor1, searchFor2)
}
\arguments{
\item{DB}{The data set the two items originate from.}

\item{x}{The first of the two items to compute the confidence value for.}

\item{y}{The second of the two items to compute the confidence value for.}
}
\value{
The minimum acceptable confidence value for the pair of items.
}
\description{
Calculate the minimum confidence value for a pair of items in the data.
}
\details{
The minimum confidence value will be used for generating and expanding rules.
}
\author{
@skirwa
}
