% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CtsHypothesisTests.R
\name{showT.Test}
\alias{showT.Test}
\title{Conduct z-test}
\usage{
showT.Test(group1, group2 = NULL, mu = 0, paired = FALSE, verbose = 1)
}
\arguments{
\item{group1}{continuous data to test}

\item{group2}{optional: second group to include for two sample t-test}

\item{mu}{optional: mean to test against for one-sample t-test}

\item{paired}{boolean, if TRUE perform matched pairs t-test}

\item{verbose}{default is 1 which will create a graph. To turn this off use verbose = 0.}
}
\value{
results of call to t.test
}
\description{
Runs z-test and outputs graph for interpretation using stats::t.test
}
\examples{
x <- rnorm(100)
showT.Test(x, verbose = 0)
showT.Test(x)

}
