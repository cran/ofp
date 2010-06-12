\name{clone}
\alias{clone}
\alias{clone.list}
\alias{clone.ENVIRONMENT}
\alias{clone.environment}
\alias{clone.FUNCTION}
\alias{clone.default}
\title{clone objects}
\description{These functions clone objects, copying environments. When cloning nested environments (environments within environments), duplicate and circular references are accounted for. However, when cloning lists that contain environments, or functions with environments, such references may not be accounted for.}
\usage{
clone (\dots)
\method{clone}{list}(object, \dots)
\method{clone}{ENVIRONMENT}(e, \dots)
\method{clone}{environment}(e, \dots)
\method{clone}{FUNCTION}(f, \dots)
\method{clone}{default}(object, \dots)
}
\arguments{
\item{object}{.}
\item{e}{.}
\item{f}{.}
\item{\dots}{.}
}


