\name{VECTOR}
\alias{LOGICAL}
\alias{TEXT}
\alias{INTEGER}
\alias{REAL}
\alias{print.VECTOR}
\alias{$.VECTOR}
\alias{$<-.VECTOR}
\alias{+.TEXT}
\title{enhanced vector objects}
\description{Vector objects with enhanced constructors, and simplified attribute access.}
\usage{
LOGICAL (x, \dots, dimension=0)
TEXT (x, \dots, dimension=0)
INTEGER (x, \dots, dimension=0)
REAL (x, \dots, dimension=0)
\method{print}{VECTOR}(x, \dots)
\method{$}{VECTOR}(x, name)
\method{$}{VECTOR}(x, name) <- value
\method{+}{TEXT}(a, b)
}
\arguments{
\item{x}{Seed vector.}
\item{\dots}{Argument list.}
\item{dimension}{Dimension attribute. Dimension values with a length of one, define the length of the vector in the usual way. Dimension values with length greater than one, are equivalent to setting the dim attribute of the vector, after it's created. This is ignored if x is provided.}
\item{name}{Name of the attribute.}
\item{value}{Value of the attribute.}
\item{a}{First operand.}
\item{b}{Second operand.}
}


