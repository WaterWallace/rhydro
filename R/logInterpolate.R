#' Logarithmic interpolation between points
#'
#' Interpolates a set of points logarithmic ally
#'     Either sets its own log offset based on inputs, or as determined
#'     When subtracting a logoffset from the reference goes negative,
#'     NA will be returned for that range
#'     i.e. reference 2,2.5, subtracting logOffset 2.1, results in -0.1, 0.4,
#'     anything in the range of 2-2.5 is NA.
#'
#' @param rating dataframe, with columns for reference and a lookup value,
#'      i.e. a height reference, and a lookup area.
#' @param num Numeric or vector for desired values
#' @param logOffset Numeric to specify the log offset used.
#'     Leave as zero to set as the maximum value that the result is zero,
#'     i.e. one row less than the first positive value
#'
#' @return dataframe of height and area
#'
#' @examples
#' rating <- data.frame(level = c(2,2.5,4,6,10),
#'   discharge = c(0.1,10,100,1000,4000))
#'
#' logInterpolate(rating, 2.4)
#' logInterpolate(rating, c(2.4, 5.5, 9.5))
#' # A reference value that evaluates to zero or less when logOffset
#' # is subtracted will return NaN in that range.
#' # If numbers should be expected, the rating should be modified
#' # to at least be greater than the logOffset.
#' logInterpolate(rating, c(2.4, 5.5, 9.5), logOffset = 2.1)
#'
#' # i.e. logInterpolate can now interpolate between 2.11 and 2.5m.
#' rating <- data.frame(level = c(2.11,2.5,4,6,10),
#'   discharge = c(0.001,10,100,1000,4000))
#'
#' logInterpolate(rating, c(2.4, 5.5, 9.5), logOffset = 2.1)
#'
#' plot(rating)
#' values <- pretty(range(rating$level), n=20)
#' lines(values,
#'  logInterpolate(rating,values),
#'  col="red"
#' )
#'
#' @export
logInterpolate <- function (rating, num, logOffset = 0, base = 10)
{

  ctf <- min(rating[rating[2] > 0,1]) # return the reference line with the minimum >0 lookup value.
  if (logOffset == 0) {

    zeroQ <- rating[rating[2] <= 0,]
    if (nrow(zeroQ) > 0)
    {
      logOffset <- max(zeroQ[1]) # max height, of outputs less than or equal to zero
    }else{
      logOffset <- ctf-0.01 # set logOffset to 0.01 less than ctf
    }
  }

  num <- num - logOffset

  rating[,1] <- rating[,1] - logOffset # subtract log offset from ref
  rating <- rating[rating[,1] > 0 ,] # exclude levels zero or less once log offset removed

  num[num < 0] <- 0
  f.logarea <- approxfun(log(rating, base=base))
  interpvalue <- base ^ ( f.logarea(log(num, base=base)) )

  interpvalue <- data.frame(ht = (num+logOffset), q = interpvalue)
  interpvalue$q[interpvalue$ht < ctf] <- 0

  return(interpvalue$q)
}

