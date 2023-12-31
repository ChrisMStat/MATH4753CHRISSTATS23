% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntickets.R
\name{ntickets}
\alias{ntickets}
\title{Airline Ticket Overbooking Problem}
\usage{
ntickets(N, gamma, p)
}
\arguments{
\item{N}{number of seats available on plane}

\item{gamma}{gamma value}

\item{p}{probability value}
}
\value{
a list including values: gamma, N, p, nd, nc & two plots (one for nd, one for nc)
}
\description{
Airline Ticket Overbooking Problem
}
\examples{
ntickets(N=200, gamma=0.02, p=0.95)

}
