#' Calculate Cross sectional area for a particular height
#'
#' Interpolates, trims cross section, then applies trapz()
#'
#' @param xscalc dataframe, with columns "chain" and "rl".
#' @param ght Numeric
#'
#' @return value
#' @importFrom pracma trapz
#'
#' @examples
#' xs <- data.frame(chain = c(1,2,3,4,5,6,7),
#'     rl = c(10,9,7,4,8,9,11)
#' )
#' calcArea(xs, 1)
#' calcArea(xs, 8)
#' calcArea(xs, 11)
#' calcArea(xs, 20)
#'
#' @export
calcArea <- function(xscalc, ght)
{
  if (ght <= min(xscalc[,2])) return (0)
  # Interpolate cross section at level of "ght"
  xscalc <- interpSeries(xscalc, ght)
  # Set any values higher than ght to ght
  if ( NROW(xscalc[xscalc[,2] > ght,][,2]) > 0 )   xscalc[xscalc[,2] > ght,][,2] <- ght

  # calculate offset
  offset <- max(xscalc[,2])
  if (ght > offset)
  {
    extrapolate <- ght - offset
  }else
  {
    extrapolate <- 0
  }

  # invert so it measures area under curve
  xscalc[,2] <- xscalc[,2]*-1
  #plot(xscalc)
  # offset to positive
  xscalc[,2] <- xscalc[,2]+offset+extrapolate

  #plot(xscalc)

  return (pracma::trapz(xscalc[,1], xscalc[,2]))

}
?calcArea

plot(xs)

