\name{example12}
\alias{example12}
\title{Example ODE System Number Twelve}
\description{The derivative function of an example two dimensional autonomous ODE system.}
\usage{example12(t, y, parameters)}
\arguments{
  \item{t}{Value of t, the independent variable, to evaluate the derivative at. Should be a single number.}
  \item{y}{Values of x and y, the dependent variables, to evaluate the derivative at. Should be a vector of length 2.}
  \item{parameters}{Values of the parameters of the system. Not required here.}
}
\details{Evaluates the derivative of the following coupled ODE system at the point (t, x, y):

dx/dt = x - y,
dy/dt = x^2 + y^2 - 2

Format is designed to be compatible with ode from the deSolve package.}
\value{Returns a list dy containing the values of the two derivatives at (t, x, y).}
\author{Michael J. Grayling}
\seealso{\code{\link{ode}}}
\examples{# Plot the velocity field, nullclines and several trajectories.
example12.flowField  <- flowField(example12, x.lim = c(-4, 4),
						y.lim = c(-4, 4), points = 17, add = FALSE,
                        xlab = "x", ylab = "y")
y0                   <- matrix(c(2, 2, -3, 0, 0, 2, 0, -3), ncol = 2,
                        nrow = 4, byrow = TRUE)
example12.nullclines <- nullclines(example12, x.lim = c(-4, 4),
                        y.lim = c(-4, 4), points = 200)
example12.trajectory <- trajectory(example12, y0 = y0, t.end = 10,
                        xlab = "x", ylab = "y")

# Determine the stability of the equilibrium points.
example12.stability.1 <- stability(example12, y.star = c(1, 1))
example12.stability.2 <- stability(example12, y.star = c(-1, -1))
}