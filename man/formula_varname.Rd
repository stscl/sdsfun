% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{formula_varname}
\alias{formula_varname}
\title{get variable names in a formula and data}
\usage{
formula_varname(formula, data)
}
\arguments{
\item{formula}{A formula.}

\item{data}{A \code{data.frame}, \code{tibble} or \code{sf} object of observation data.}
}
\value{
A list.
\describe{
\item{\code{yname}}{Independent variable name}
\item{\code{xname}}{Dependent variable names}
}
}
\description{
get variable names in a formula and data
}
\examples{
gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
formula_varname(PS_Score ~ EL_Score + OH_Score, gzma)
formula_varname(PS_Score ~ ., gzma)

}
