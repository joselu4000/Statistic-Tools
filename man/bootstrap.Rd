% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap.R
\name{bootstrap}
\alias{bootstrap}
\title{This function give you and method Bootstrap to simulate dataset with a previously data.}
\usage{
bootstrap(data, numsim, Graph, solution_data = NULL)
}
\arguments{
\item{data}{Your dataset with the variables by columns}

\item{numsim}{Number of simulation with de number of your data that you want}

\item{Graph}{You can decide whether to use (TRUE) or not (FALSE) print all associated plots}

\item{solution_data}{Optional param: You can decide if sustitute yours NAs data by means of the column or omit then is NULL default}
}
\value{
clean data, simulations, and some statistic
}
\description{
This function give you and method Bootstrap to simulate dataset with a previously data.
}
\examples{
# Previously install and read: "mlbench"
data("iris")
data <- iris[,-5]
BS <- bootstrap(data, numsim = 3, Graph = TRUE, solution_data = NULL)

}
