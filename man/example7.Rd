% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example7.R
\name{example7}
\alias{example7}
\title{Example ODE system 7}
\usage{
example7(t, y, parameters)
}
\arguments{
\item{t}{The value of \ifelse{html}{\out{<i>t</i>}}{\eqn{t}}, the independent
variable, to evaluate the derivative at. Should be a
\code{\link[base]{numeric}} \code{\link[base]{vector}} of
\code{\link[base]{length}} one.}

\item{y}{The values of \ifelse{html}{\out{<i>x</i>}}{\eqn{x}} and
\ifelse{html}{\out{<i>y</i>}}{\eqn{y}}, the dependent variables, to evaluate
the derivative at. Should be a \code{\link[base]{numeric}}
\code{\link[base]{vector}} of \code{\link[base]{length}} two.}

\item{parameters}{The values of the parameters of the system. Not used here.}
}
\value{
Returns a \code{\link[base]{list}} containing the values of the two
derivatives at
\ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}.
}
\description{
The derivative function of an example two-dimensional autonomous ODE system.
}
\details{
\code{example7} evaluates the derivatives of the following coupled ODE system
at the point \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{
\eqn{(t, x, y)}}:

\ifelse{html}{\out{<i>dx</i>/<i>dt</i> = -<i>x</i> - <i>y</i>,
<i>dy</i>/<i>dt</i> = 4<i>x</i> +
<i>y</i>.}}{\deqn{\frac{dx}{dt} = -x - y, \frac{dy}{dt} = 4x + y.}}

Its format is designed to be compatible with \code{\link[deSolve]{ode}} from
the \code{\link[deSolve]{deSolve}} package.
}
\seealso{
\code{\link[deSolve]{ode}}
}
\author{
Michael J Grayling
}
