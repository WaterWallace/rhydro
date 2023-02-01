#' Calculate cross sectional area for a a range of heights
#'
#' Applies calcArea at intervals to produce a lookup table
#'     Below the lowest postive number in the second column is
#'     set to zero.  It is not interpolated to zero.
#'
#' @param rating dataframe, with columns for height and area
#' @param num Numeric or vector for desired values
#' @param logOffset Numeric to specify the log offset used.
#'     Leave as zero to set as the maximum value that the result is zero,
#'     i.e. one row less than the first positive value
#'
#' @return dataframe of height and area
#'
#' @examples
#' xs <- data.frame(chain = c(1,2,3,4,5,6,7),
#'     rl = c(10,9,7,4,8,9,11)
#' )
#' lookupTable <- areaLookup(xs, interval = 1)
#' # with default used for logOffset ( 4 in this case)
#' logInterpolate(lookupTable, 7.5)
#' # plot cross section
#' plot(xs)
#' # plot 1m interval lookup table
#' plot(lookupTable)
#' # plot log interpolated values
#' heights <- seq(min(lookupTable$height), max(lookupTable$height), by = 0.01 )
#' lines(heights, logInterpolate(lookupTable, heights), col="red")
#'
#' @export
#'
logInterpolate <- function (rating, num, logOffset = 0)
{
  ctf <- min(rating[rating[2] > 0,][1])

  if (logOffset == 0) {
    logOffset <- max(rating[rating[2] <= 0,][1]) # max height, of outputs less than or equal to zero
  }
  dfOffs <- rating[, 1] - logOffset
  num <- num - logOffset
  f.logarea <- approxfun(log(dfOffs), log(rating[, 2]))
  interpvalue <- exp(f.logarea(log(num)))

  interpvalue <- data.frame(ht = (num+logOffset), q = interpvalue)
  interpvalue$q[interpvalue$ht < ctf] <- 0

  return(interpvalue$q)
}
