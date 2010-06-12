\name{constructors}
\alias{extend}
\alias{implant}
\alias{is.cleancall}
\title{construct objects}
\description{The extend function, extends an object, by extending it's class attribute. Both functions allow the user to specify further attributes (in the ofp sense) for the object, Noting that the functions attempt to resolve argument names, and can not be called with dots. e.g. The call extend (myobj, "myclass", ...) is not allowed. The function is.cleancall, checks a call to see if it contains dots.}
\usage{
extend (object, subclass, \dots, attributes)
implant (object, \dots, attributes)
is.cleancall (call=sys.call (-1) )
}
\arguments{
\item{object}{The object to extend.}
\item{subclass}{Subclass name.}
\item{\dots}{A possible list of attributes, ignored if attributes is provided.}
\item{attributes}{A possible list of attributes.}
\item{call}{.}
}



