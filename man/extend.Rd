\name{extend}
\alias{extend}
\alias{implant}
\title{extend an object}
\description{The extend function, extends an object, by extending it's class attribute. Both the extend and implant functions return the object. Both functions allow the user to specify further attributes (in the ofp sense) for the object, Noting that the functions attempt to resolve argument names, and can not be called with dots. e.g. The call extend (myobj, "myclass", ...) is not allowed.}
\usage{
extend (obj, subclass, ..., attributes)
implant (obj, ..., attributes)
}
\arguments{
\item{obj}{The object to extend.}
\item{subclass}{Subclass name.}
\item{...}{A possible list of attributes, ignored if attributes is provided}
\item{attributes}{A possible list of attributes.}
}


