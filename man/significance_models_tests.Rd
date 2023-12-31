% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/significance.R
\name{significance_models_tests}
\alias{significance_models_tests}
\title{This function give you significance tests (maybe 2 by 2)}
\usage{
significance_models_tests(data_models, anova_forced = NULL, alpha = NULL)
}
\arguments{
\item{anova_forced}{You can forced anova tests (and 2 by 2)}

\item{alpha}{Optional param to change alpha for all tests}

\item{data}{Your dataset with yours models by column}
}
\value{
anova, mann-whitney, kruskal-wallis
}
\description{
This function give you significance tests (maybe 2 by 2)
}
\examples{
# Previously install and read: "mlbench"
data("iris")
data <- iris[,-5]
sig <- significance_models_tests(data, anova_forced = TRUE)

}
