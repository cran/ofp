\name{VECTOR}
\alias{is.VECTOR}
\alias{TEXT}
\alias{REAL}
\alias{COMPLEX}
\alias{INTEGER}
\alias{LOGICAL}
\alias{is.TEXT}
\alias{is.REAL}
\alias{is.COMPLEX}
\alias{is.INTEGER}
\alias{is.LOGICAL}
\alias{print.VECTOR}
\alias{[.VECTOR}
\alias{$.VECTOR}
\alias{$<-.VECTOR}
\title{enhanced vector objects}
\description{Vector objects with enhanced constructors, and simplified attribute access. Currently, there are some problems with creating matrices.}
\usage{
TEXT (x, \dots, call, dimension)
REAL (x, \dots, call, dimension)
COMPLEX (x, \dots, call, dimension)
INTEGER (x, \dots, call, dimension)
LOGICAL (x, \dots, call, dimension)
is.VECTOR (x)
is.TEXT (x)
is.REAL (x)
is.COMPLEX (x)
is.INTEGER (x)
is.LOGICAL (x)
\method{print}{VECTOR}(x, \dots)
\method{[}{VECTOR}(x, \dots)
\method{$}{VECTOR}(x, name)
\method{$}{VECTOR}(x, name) <- value
}
\arguments{
\item{x}{Seed vector.}
\item{\dots}{Argument list.}
\item{call}{Call used to resolve argument names.}
\item{dimension}{Dimension attribute. Dimension values with a length of one, define the length of the vector in the usual way. Dimension values with length greater than one, are equivalent to setting the dim attribute of the vector, after it's created. This is ignored if x is provided.}
\item{name}{Name of the attribute.}
\item{value}{Value of the attribute.}
}





