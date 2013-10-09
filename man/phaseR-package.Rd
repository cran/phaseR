\name{phaseR-package}
\alias{phaseR-package}
\alias{phaseR}
\docType{package}
\title{Phase Plane Analysis of One and Two Dimensional Autonomous ODE Systems}
\description{phaseR is an R package for the qualitative analysis of one and two dimensional autonomous ODE systems, using phase plane methods. Programs are available to identify and classify equilibrium points, plot the direction field, and plot trajectories for multiple initial conditions. In the one dimensional case, a program is also available to plot the phase portrait. Whilst in the two dimensional case, additionally a program is available to plot nullclines. Many example systems are provided for the user.}
\details{
\tabular{ll}{
Package: \tab phaseR\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2013-10-06\cr
License: \tab GPL-3\cr
}
The package contains six main functions for performing phase plane analysis:
\itemize{
  \item \code{\link{flowField}}: Plots the flow or velocity field of a one or two dimensional autonomous ODE system.
  \item \code{\link{nullclines}}: Plots the nullclines of a two dimensional autonomous ODE system.
  \item \code{\link{numericalSolution}}: Numerically solves a two dimensional autonomous ODE system in order to plot the two dependent variables against the independent variable.
  \item \code{\link{phasePortrait}}: Plots the phase portrait of a one dimensional autonomous ODE system, for use in classifying equilibria.
  \item \code{\link{stability}}: Performs stability, or perturbation, analysis in order to classify equilibria.
  \item \code{\link{trajectory}}: Numerically solves a one or two dimensional ODE system to plot trajectories in the phase plane.
}

In addition, the package contains over 25 derivative functions for example systems. Links to these can be found in the packages index.

An accompanying guide with further information, examples, and exercises, can be found in the doc directory of the packages installation.

This package makes use of the ode function in the package \code{\link[deSolve:deSolve-package]{deSolve}}.
}
\author{Michael J. Grayling

Maintainer: Michael J. Grayling <mjg211@cam.ac.uk>
}
