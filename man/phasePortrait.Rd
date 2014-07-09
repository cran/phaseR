\name{phasePortrait}
\alias{phasePortrait}
\title{Phase Portrait Plot}
\description{For a one dimensional autonomous ODE, it plots the phase portrait i.e. the derivative against the dependent variable. In addition, along the dependent variable axis it plots arrows pointing in the direction of dependent variable change with increasing value of the independent variable. From this stability of equilibrium points (i.e. locations where the horiztonal axis is crossed) can be determined.}
\usage{phasePortrait(deriv, y.lim, y.step = 0.01, parameters = NULL, points = 10,
                     frac = 0.75, arrow.head = 0.075, colour = "black", xlab = "y",
					 ylab = "f(y)", ...)}
\arguments{
  \item{deriv}{A function computing the derivative at a point for the ODE system to be analysed. Discussion of the required structure of these functions can be found in the package guide.}
  \item{y.lim}{Sets the limits of the dependent variable for which the derivative should be computed and plotted. Should be a vector of length two.}
  \item{y.step}{Sets the step length of the dependent variable vector for which derivatives are computed and plotted. Decreasing y.step makes the resulting plot more accurate, but comes at a small cost to computation time. Defaults to 0.01.}
  \item{parameters}{Parameters of the ODE system, to be passed to deriv. Supplied as a vector; the order of the parameters can be found from the deriv file.}
  \item{points}{Sets the density at which arrows are plotted along the horizontal axis. points arrows will be plotted. Fine tuning here, by shifting points up and down, allows for the creation of more aesthetically pleasing plots. Defaults to 10.}
  \item{frac}{Sets the fraction of the theoretical maximum length line segments can take without overlapping, that they actually attain. Fine tuning here assists the creation of aesthetically pleasing plots. Defaults to 0.75.}
  \item{arrow.head}{Sets the length of the arrow heads. Passed to arrows. Defaults to 0.075.}
  \item{colour}{Sets the colour of the line in the plot, as well as the arrows. Will be reset accordingly if it is not a vector of length one. Defaults to "black".}
  \item{xlab}{Label for the x-axis of the resulting plot. Defaults to "y".}
  \item{ylab}{Label for the y-axis of the resulting plot. Defaults to "f(y)".}
  \item{\dots}{Additional arguments to be passed to either plot or arrows.}
}
\value{Returns a list with the following components:
  \item{arrow.head}{As per input.}
  \item{colour}{As per input.}
  \item{deriv}{As per input.}
  \item{dy}{A vector containing the value of the derivative at each evaluated point.}
  \item{frac}{As per input.}
  \item{parameters}{As per input.}
  \item{points}{As per input.}
  \item{xlab}{As per input.}
  \item{y.lim}{As per input.}
  \item{y.step}{As per input.}
  \item{y}{A vector containing the values of the dependent variable for which the derivative was evaluated.}
  \item{ylab}{As per input.}
}
\author{Michael J. Grayling}
\seealso{\code{\link{arrows}}, \code{\link{plot}}}
\examples{# A one dimensional autonomous ODE system, example2.
example2.phasePortrait <- phasePortrait(example2, y.lim = c(-0.5, 2.5), points = 10,
                                        frac = 0.5)
}