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
#'   discharge = c(0.1,10,100,1000,4000)
#'                       )
#'
#' logInterpolate(rating, 2.4)
#' logInterpolate(rating, c(2.4, 5.5, 9.5))
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

  ref <- rating[1]
  lookup <- rating[2]

  ctf <- min(rating[lookup > 0,1]) # return the reference line with the minimum >0 lookup value.

  if (logOffset == 0) {

    zeroQ <- rating[lookup <= 0,]
    if (nrow(zeroQ) > 0)
    {
      logOffset <- max(zeroQ[1]) # max height, of outputs less than or equal to zero
    }else{
      logOffset <- ctf-0.01 # set logOffset to 0.01 less than ctf
    }
  }
  dfOffs <- ref - logOffset
  num <- num - logOffset
  f.logarea <- approxfun(log(dfOffs, base=base)[,1], log(lookup, base=base)[,1])
  interpvalue <- base ^ ( f.logarea(log(num, base=base)) )

  interpvalue <- data.frame(ht = (num+logOffset), q = interpvalue)
  interpvalue$q[interpvalue$ht < ctf] <- 0

  return(interpvalue$q)
}
