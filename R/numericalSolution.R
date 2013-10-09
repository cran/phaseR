numericalSolution <-
function(deriv, y0, t.start = 0, t.end, t.step = 0.01, parameters = NULL, type = "two", colour = rep("black", 2), grid = "yes", ...){
  if (t.start < 0){
    stop(paste("t.start is less than zero"))
  }
  if (t.end <= 0){
    stop(paste("t.end is less than or equal to zero"))
  }
  if (t.start >= t.end){
    stop(paste("t.end is less than or equal to t.start"))
  }
  if (t.step <= 0){
    stop(paste("t.step is less than or equal to zero"))
  }
  if (is.null(y0)){
    y0 <- locator(n = 1)
  }
  if (is.vector(y0) == FALSE){
    stop(paste("y0 is not a vector as required"))
  }
  if (length(y0) == 1){
    stop(paste("y0 should be a vector of length two"))
  }
  if ((type != "one") & (type != "two")){
    stop(paste("type must either be set to one or two"))
  }
  if (is.vector(colour) == FALSE){
    stop(paste("colour is not a vector as required"))
  }
  if (length(colour) == 1){
    colour <- rep(colour, length(y0))
    warning(paste("colour has been reset as required"))
  }
  if (length(colour) > 2){
    colour <- colour[1:2]
    warning(paste("colour has been reset as required"))
  }
  if ((grid != "yes") & (type != "no")){
    stop(paste("grid must either be set to yes or no"))
  }
  t <- seq(from = t.start, to = t.end, by = t.step)
  phase.trajectory <- ode(times = t, y = y0, func = deriv, parms = parameters, method = "rk")
  x <- phase.trajectory[, 2]
  y <- phase.trajectory[, 3]
  if (type == "one"){
    plot(t, x, col = colour[1], type = "l", ...)
    lines(t, y, col = colour[2], type = "l", ...)
    if (grid == "yes"){
      grid()
    }
  }
  if (type == "two"){
    old.par <- par
    par(mfcol = c(2, 1), oma = c(0, 0, 2, 0))
    plot(t, x, col = colour[1], type = "l", ...)
    if (grid == "yes"){
      grid()
    }
    plot(t, y, col = colour[2], type = "l", ...)
    if (grid == "yes"){
      grid()
    }
    par(mfcol=c(1,1))
  }
  output            <- list()
  output$colour     <- colour
  output$parameters <- parameters
  output$t.start    <- t.start
  output$t.step     <- t.step
  output$t.end      <- t.end
  output$t          <- t
  output$type       <- type
  output$x          <- x
  output$y0         <- y0
  output$y          <- y
}
