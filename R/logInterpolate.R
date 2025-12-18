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
#'      qc for column 3 optional.
#'      i.e. a height reference, and a lookup area.
#' @param num Numeric or vector for desired values
#' @param logOffset Numeric to specify the log offset used.
#'     Leave as zero to set as the maximum value that the result is zero,
#'     i.e. one row less than the first positive value
#'
#' @return dataframe of either *Value*, or *Value* and *QC*
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
#'  logInterpolate(rating,values)$value,
#'  col="red"
#' )
#'
#' # interpolate quality code
#' rating$qc <- c(10,10,20,20,60)
#' logInterpolate(rating,c(1.9, 6.1, 9.5))
#'
#' @export
logInterpolate <- function (rating, num, logOffset = 0, base = 10)
{

  maxfun <- function(t, y, x, option = "max") {
    f.qual0 <- approxfun(t, y, method = "constant", ties = option,
                         f = 0, rule = 2)
    f.qual1 <- approxfun(t, y, method = "constant", ties = option,
                         f = 1, rule = 2)

    return(pmax(f.qual0(x), f.qual1(x)))
  }


  getLogOffs <- function(rating, logOffset){
    if(rating[rating[2] > 0] %>% length == 0){  return(min(rating[,1]) - 0.01) }
    if (logOffset > 0) return(logOffset)

    ZeroQ <- rating %>% dplyr::filter(.[[2]] <= 0)
    if (nrow(ZeroQ) > 0) {
      return( logOffset <- max(ZeroQ[1]) )# max height, of outputs less than or equal to zero
    }
    return( min(rating[rating[2] > 0,1]) - 0.01 ) # set logOffset to 0.01 less than ctf
  }

  logOffset <- getLogOffs(rating, logOffset)

  num <- num - logOffset
  rating[,1] <- rating[,1] - logOffset # subtract log offset from ref
  rating <- rating[rating[,1] > 0 ,] # exclude levels zero or less once log offset removed

  if(nrow(rating) == 0){

    interpvalue <- data.frame(ht = (num+logOffset), q = rep(0, length(num)))
    message("warning: zero points in rating table")

  }else{

    num[num < 0] <- 0
    rating
    f.logarea <- approxfun(log(rating, base=base))
    interpvalue <- base ^ ( f.logarea(log(num, base=base)) )

    interpvalue <- data.frame(ht = (num+logOffset), q = interpvalue)

    # this ones have choppped out any zeroes
    # if intervalue height is less than the min
    #interpvalue$q[interpvalue$ht <  (rating[,1]+logOffset) %>% min] <- 0
    interpvalue <- interpvalue %>%
      mutate(q = if_else(ht < min(rating[[1]] + logOffset), 0, q))
  }

  if( max(interpvalue %>% dplyr::select(ht)) > max(rating[[1]] + logOffset) )
  {
    message("rating table exceeded, replaced with NA")
  }

  if (length(rating) == 3)
  {
    if (nrow(rating) > 0){
      qc <- maxfun(rating[,1], rating[,3], num)
    }else{
      qc <- 151
    }

    return(data.frame(value = interpvalue$q, qc = qc))
  }else{
    return(data.frame(value = interpvalue$q) )
  }
}









