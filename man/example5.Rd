\name{example5}
\alias{example5}
\title{Example ODE System Number Five}
\description{The derivative function of an example two dimensional autonomous ODE system.}
\usage{example5(t, y, parameters)}
\arguments{
  \item{t}{Value of t, the independent variable, to evaluate the derivative at. Should be a single number.}
  \item{y}{Values of x and y, the dependent variables, to evaluate the derivative at. Should be a vector of length 2.}
  \item{parameters}{Values of the parameters of the system. Not required here.}
}
\details{Evaluates the derivatives of the following coupled ODE system at the point (t,x,y):

dx/dt = 2*x + y,
dy/dt = 2*x - y

Format is designed to be compatible with ode from the deSolve package.}
\value{Returns a list dy containing the values of the two derivatives at (t, x, y).}
\author{Michael J. Grayling}
\seealso{\code{\link{ode}}}
\examples{
# Plot the velocity field, nullclines and several trajectories.
example5.flowField  <- flowField(example5, x.lim = c(-3, 3),
                       y.lim = c(-3, 3), points = 19, add = FALSE,
                       xlab = "x", ylab = "y")
y0                  <- matrix(c(1, 0, -1, 0, 2, 2, -2, 2, 0, 3, 0, -3),
                       ncol = 2, nrow = 6, byrow = TRUE)
example5.nullclines <- nullclines(example5, x.lim = c(-3, 3), y.lim = c(-3, 3))
example5.trajectory <- trajectory(example5, y0 = y0, t.end = 10,
                       xlab = "x", ylab = "y")

# Plot x and y against t.
example5.numericalSolution <- numericalSolution(example5,
                              y0 = c(0, 3), t.end = 3)

# Determine the stability of the equilibrium point.
example5.stability <- stability(example5, y.star = c(0, 0))
}