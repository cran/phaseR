SIR <-
function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  beta <- parameters[1]
  nu   <- parameters[2]
  dy    <- rep(0, 2)
  dy[1] <- -beta*x*y
  dy[2] <- beta*x*y - nu*y
  list(dy)
}
