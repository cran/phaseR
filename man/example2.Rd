% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example2.R
\name{example2}
\alias{example2}
\title{Example ODE system 2}
\usage{
example2(t, y, parameters)
}
\arguments{
\item{t}{The value of \ifelse{html}{\out{<i>t</i>}}{\eqn{t}}, the independent
variable, to evaluate the derivative at. Should be a
\code{\link[base]{numeric}} \code{\link[base]{vector}} of
\code{\link[base]{length}} one.}

\item{y}{The value of \ifelse{html}{\out{<i>y</i>}}{\eqn{y}}, the dependent
variable, to evaluate the derivative at. Should be a
\code{\link[base]{numeric}} \code{\link[base]{vector}} of
\code{\link[base]{length}} one.}

\item{parameters}{The values of the parameters of the system. Not used here.}
}
\value{
Returns a \code{\link[base]{list}} containing the value of the
derivative at \ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}.
}
\description{
The derivative function of an example one-dimensional autonomous ODE system.
}
\details{
\code{example2} evaluates the derivative of the following ODE at the point
\ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}:

\ifelse{html}{\out{<i>dy</i>/<i>dt</i> = <i>y</i>(1 - <i>y</i>)(2 -
<i>y</i>).}}{\deqn{\frac{dy}{dt} = y(1 - y)(2 - y).}}

Its format is designed to be compatible with \code{\link[deSolve]{ode}} from
the \code{\link[deSolve]{deSolve}} package.
}
\examples{
# Plot the flow field and several trajectories
example2_flowField     <- flowField(example2,
                                    xlim   = c(0, 4),
                                    ylim   = c(-1, 3),
                                    system = "one.dim",
                                    add    = FALSE,
                                    xlab   = "t")
example2_trajectory    <- trajectory(example2,
                                     y0     = c(-0.5, 0.5, 1.5, 2.5),
                                     tlim   = c(0, 4),
                                     system = "one.dim")
# Plot the phase portrait
example2_phasePortrait <- phasePortrait(example2,
                                        ylim = c(-0.5, 2.5),
                                        frac = 0.5)
# Determine the stability of the equilibrium points
example2_stability_1   <- stability(example2,
                                    ystar  = 0,
                                    system = "one.dim")
example2_stability_2   <- stability(example2,
                                    ystar  = 1,
                                    system = "one.dim")
example2_stability_3   <- stability(example2,
                                    ystar  = 2,
                                    system = "one.dim")
}
\seealso{
\code{\link[deSolve]{ode}}
}
\author{
Michael J Grayling
}
