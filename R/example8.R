example8 <-
function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  dy    <- rep(0, 2)
  dy[1] <- y
  dy[2] <- -x - y
  list(dy)
}
