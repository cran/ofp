\name{LIST}
\alias{LIST}
\alias{is.LIST}
\alias{[.LIST}
\alias{[<-.LIST}
\title{enhanced list objects}
\description{A LIST object extends a list objecy. By default it automatically resolves argument names. Note that the single bracket system may be changed to a double bracket system.}
\usage{
LIST (..., resolve=TRUE, call=sys.call () )
is.LIST (obj)
\method{[}{LIST}(obj, \dots)
\method{[}{LIST}(obj, \dots)<-value
}
\arguments{
\item{resolve}{This should generally be true, and means that the function attempts to resolve argument names. If calling LIST with dots as an argument, it is advisable to either set resolve to false, or replace the call. Refer to is.cleancall for more information.}
\item{call}{The call containing the original arguments, that is, the call used to resolve the argument names.}
\item{obj}{A LIST object, or a potential LIST object.}
\item{value}{New value for an element object.}
\item{\dots}{For LIST, the objects to be in the LIST. For the extraction methods, the indices.}
}




