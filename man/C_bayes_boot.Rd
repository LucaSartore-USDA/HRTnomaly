\name{C_bayes_boot}
\alias{C_bayes_boot}

\title{Native C Interface Wrapper for the Function \code{bayes_boot}}

\description{
  These compiled symbols represent the direct entry points to the
  underlying C source code, masked behind the \code{C_} namespace fix.
}

\usage{
.C(C_my_c_plot, x)
.Call(C_my_c_predict, object, newdata)
}
\arguments{
  \item{x}{A vector formatted for native extraction.}
  \item{object}{A reference memory pointer.}
  \item{newdata}{A data matrix matrix.}
}
\keyword{internal}

