\name{clone.ENVIRONMENT}
\alias{clone.ENVIRONMENT}
\alias{clone.environment}
\alias{ENVIRONMENT.clone}
\alias{clone.compenv}
\alias{compenv.clone}
\title{clone ENVIRONMENT objects}
\description{These functions clone environments, accounting for circular references. The ENVIRONMENT.clone function (not clone.ENVIRONMENT) should not be used by users, it may be made hidden in future releases. Note there may be an error cloning environments that contain lists that contain environments.}
\usage{
\method{clone}{ENVIRONMENT}(e, \dots)
\method{clone}{environment}(e, \dots)
ENVIRONMENT.clone (e, flags=objref (list () ) )
\method{clone}{compenv}(e, \dots)
compenv.clone (e, flags=objref (list () ) )
}
\arguments{
\item{e}{An environment.}
\item{flags}{User should ignore this.}
\item{\dots}{.}
}



