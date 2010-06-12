\name{datafile}
\alias{datafile}
\title{create a data.frame object from a csv file}
\description{Experimental function, may be removed. Creates a data.frame object from a csv file.}
\usage{
datafile (pkg, file, global=FALSE, preview=FALSE, \dots)
}
\arguments{
\item{pkg}{The package name.}
\item{file}{The file name. If the package name is not given, this can be a path.}
\item{global}{If true, copy the data.frame's variables into the global environment.}
\item{preview}{If true, generate a preview, using the preview function.}
\item{\dots}{Arguments for a read.csv call.}
}


