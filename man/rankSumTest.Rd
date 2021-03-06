% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rankSumTest.R
\name{rankSumTest}
\alias{rankSumTest}
\title{Implements rank-sum test based on band depth calculation}
\usage{
rankSumTest(controlData, testData, size.control, size.test)
}
\arguments{
\item{controlData}{Data for control subjects, each column must be a subject}

\item{testData}{Data for test subjects, each column must be a subject}

\item{size.control}{Size of experimental group for control data. Choose such that size.control < number of control subjects}

\item{size.test}{Size of experimental group for test data. Choose such that size.test < number of test subjects}
}
\description{
This function samples the reference group several times and gets the average p-value of the rank test. we apply the two-sided rank sum test 
with null hypothesis that data in the test and control groups are independent samples from identical continuous distributions.
The test is equivalent to a Mann-Whitney U-test.
}
\author{
Sara Lopez-Pintado \email{sl2929@columbia.edu}
}

