\name{numericalSolution}
\alias{numericalSolution}
\title{Numerical Solution and Plotting}
\description{Numerically solves a two dimensional autonomous ODE system for a given initial condition. It then plots the dependent variables against the independent variable.}
\usage{numericalSolution(deriv, y0, t.start = 0, t.end, t.step = 0.01,
                  parameters = NULL, type = "two", colour = rep("black", 2),
                  grid = "yes", ...)}
\arguments{
  \item{deriv}{A function computing the derivative at a point for the ODE system to be analysed. Discussion of the required structure of these functions can be found in the package guide.}
  \item{y0}{The initial condition. Should be a vector of length two reflecting the location of the two dependent variables initially.}
  \item{t.start}{The value of the independent variable to begin the numerical integration at. Defaults to 0.}
  \item{t.end}{The value of the independent variable to end numerical integration at.}
  \item{t.step}{The step length of the independent variable, used in numerical integration. Decreasing t.step theoretically makes the numerical integration more accurate, but increases computation time. Defaults to 0.01.}
  \item{parameters}{Parameters of the ODE system, to be passed to deriv. Supplied as a vector; the order of the parameters can be found from the deriv file. Defaults to NULL.}
  \item{type}{If set to "one" the trajectories are plotted on the same graph. If set to "two" they are plotted on separate graphs. Defaults to "two".}
  \item{colour}{Sets the colours of the trajectories of the two dependent variables. Will be reset accordingly if it is not a vector of length two. Defaults to rep("black", 2).}
  \item{grid}{If set to "yes" grids are added to the plots. If set to "no", grids are not added. Defaults to "yes".}
  \item{\dots}{Additional arguments to be passed to plot.}
}
\value{Returns a list with the following components:
\item{colour}{As per input, but with possible editing if a colour vector of the wrong length was supplied.}
  \item{deriv}{As per input.}
  \item{parameters}{As per input.}
  \item{t.end}{As per input.}
  \item{t.start}{As per input.}
  \item{t.step}{As per input.}
  \item{t}{A vector containing the values of the independent variable at each integration step.}
  \item{x}{A vector containing the numerically computed values of the first dependent variable at each integration step.} 
  \item{y}{A vector containing the numerically computed values of the second dependent variable at each integration step.}
}
\author{Michael J. Grayling}
\seealso{\code{\link{ode}}, \code{\link{plot}}}
\examples{# A two dimensional autonomous ODE system, vanDerPol.
vanDerPol.numericalSolution <- numericalSolution(vanDerPol, y0 = c(4, 2),
                               t.end = 100, parameters = 3)
}