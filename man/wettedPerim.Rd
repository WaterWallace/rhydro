% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wettedPerim.R
\name{wettedPerim}
\alias{wettedPerim}
\title{Calculate wetted perimeter for a particular height}
\usage{
wettedPerim(xs, level)
}
\arguments{
\item{xs}{dataframe, with columns "chain" and "rl".}

\item{level}{Numeric}
}
\value{
value
}
\description{
Calculate wetted perimeter for a particular height
}
\examples{
xs <- data.frame(chain = c(1,2,3,4,5,6,7),
    rl = c(10,9,7,4,8,9,11)
)
wettedPerim(xs, 1)
wettedPerim(xs, 8)
wettedPerim(xs, 11)
wettedPerim(xs, 20)

}
