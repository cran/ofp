\name{objref}
\alias{objref}
\alias{print.objref}
\alias{mutate.objref}
\alias{length.objref}
\alias{[.objref}
\alias{[<-.objref}
\alias{[[.objref}
\alias{[[<-.objref}
\title{object references}
\description{Environment-based object references to single objects. The referenced object is accessed by calling the reference as a function with no arguments. Where the reference is to a vector, square brackets may be used for direct access to the vector's elements.}
\usage{
objref (obj)
\method{print}{objref}(ref, \dots)
\method{mutate}{objref}(ref, obj, \dots)
\method{length}{objref}(x)
\method{[}{objref}(ref, \dots)
\method{[}{objref}(ref, \dots) <- value
\method{[[}{objref}(ref, \dots)
\method{[[}{objref}(ref, \dots) <- value
}
\arguments{
\item{ref}{.}
\item{x}{.}
\item{obj}{.}
\item{value}{.}
\item{\dots}{.}
}


