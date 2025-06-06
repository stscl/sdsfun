% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector_toolkits.R
\name{generate_subsets}
\alias{generate_subsets}
\title{generate subsets of a set}
\usage{
generate_subsets(set, empty = TRUE, self = TRUE)
}
\arguments{
\item{set}{A vector.}

\item{empty}{(optional) When \code{empty} is \code{TRUE}, the generated subset includes the empty set,
otherwise the empty set is removed. Default is \code{TRUE}.}

\item{self}{(optional) When \code{self} is \code{TRUE}, the resulting subset includes the set itself,
otherwise the set itself is removed. Default is \code{TRUE}.}
}
\value{
A list.
}
\description{
generate subsets of a set
}
\examples{
generate_subsets(letters[1:3])
generate_subsets(letters[1:3],empty = FALSE)
generate_subsets(letters[1:3],self = FALSE)
generate_subsets(letters[1:3],empty = FALSE,self = FALSE)

}
