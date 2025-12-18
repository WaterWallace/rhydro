#' Rotate x,y,z coordinates around an axis
#'
#' @param new.RP1 dataframe, with columns "x", "y", "z"
#' @param new.RP2 dataframe, with columns "x", "y", "z"
#' @param target.RP1 dataframe, with columns "x", "y", "z"
#' @param target.RP2 dataframe, with columns "x", "y", "z"
#'
#' @return float, angle (degrees)
#'
#' @examples
#' m <- data.frame(x = c(2.5,2,3,1), y = c(3,5,10,15), z = c(3,2,1,3))
#' # plotting y ( chainage ) against x (alignment)
#' plot(y ~ x, m,
#'      xlim = c(-20,10),
#'     ylim =c(-10,20),
#'     col = "blue")
#' abline (1,0)
#' abline (v = 0)
#' surveyedRP1 <- data.frame(x = 2.5, y = 3)
#' surveyedRP2 <-data.frame(x = 1, y = 15)
#' requiredRP1 <-data.frame(x = -1.36, y = 9.5)
#' requiredRP2 <-data.frame(x = -7, y = 10)
#'
#' points(surveyedRP1, col="blue", pch = 4)
#' points(surveyedRP2, col="blue", pch = 4)
#' points(requiredRP1, col="red", pch = 4)
#' points(requiredRP2, col="red", pch = 4)
#'
# required angle shift to align
#' angle <- rpAlign(surveyedRP1, surveyedRP2, requiredRP1, requiredRP2)
#' print(angle)
#'
#' points(y ~ x, matrixRotate(m, deg = angle, rotateaxis = "z" ), col="orange")
#'
#' @export

rpAlign <- function(new.RP1, new.RP2, target.RP1, target.RP2)
{
  y <- new.RP2$y - new.RP1$y
  x <- new.RP2$x - new.RP1$x
  newangle <- atan2(y,x)

  y <- target.RP2$y - target.RP1$y
  x <- target.RP2$x - target.RP1$x
  reqangle <- atan2(y,x)

  # angular shift is the degrees to rotate the cross section
  # to align with reference XS
  anglularShift <- rad2deg( reqangle - newangle )
  return(anglularShift[1])
}
