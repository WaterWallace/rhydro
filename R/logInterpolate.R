#' Lograrithmic interpolation between points
#'
#' Intpolates a set of points logarithmically
#'     Either sets its own log offset based on inputs, or as determined
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
#' rating <- data.frame(discharge = c(0.1,10,100,1000,2000,10000),
#'     level = c(2,3,4,5,6,10)
#' )
#'
#' plot(rating)
#' logInterpolate(rating, 10)
#'
#' logInterpolate(rating, 2.5, logOffset = 1.9)
#' logInterpolate(rating, 9, logOffset = 1.9)
#'
#' @export
logInterpolate <- function (rating, num, logOffset = 0, interpbase = 10)
{
  # rating[,1] = Level, i.e. reference
  # rating[,2] = Discharge i.e. lookup value

  ref <- rating[1]
  lookup <- rating[2]

  ctf <- min(rating[lookup > 0,][1])

  if (logOffset == 0) {

    zeroQ <- rating[lookup <= 0,]
    if (nrow(zeroQ) > 0)
    {
      logOffset <- max(zeroQ[1]) # max height, of outputs less than or equal to zero
    }else{
      logOffset <- min(ref)-0.01
    }
  }
  dfOffs <- ref - logOffset
  num <- num - logOffset
  f.logarea <- approxfun(log(dfOffs, base=interpbase)[,1], log(lookup, base=interpbase)[,1])
  interpvalue <- interpbase ^ ( f.logarea(log(num, base=interpbase)) )

  interpvalue <- data.frame(ht = (num+logOffset), q = interpvalue)
  interpvalue$q[interpvalue$ht < ctf] <- 0

  return(interpvalue$q)
}

log(3.5, base=10)

num <- 6
#logOffset <- 0


rating <- data.frame(level = c(2,3,4,6,10),
  discharge = c(0.1,10,100,1000,4000)
                      )
plot(rating)
level <- 2.9
logInterpolate(rating, level, logOffset = 1.9)
points(level,
       logInterpolate(rating, level, logOffset = 1.9),
       col="red",
       pch=19
       )

#plot(rating)
level <- 4.5
logInterpolate(rating, level)
points(level,
       logInterpolate(rating, level),
       col="blue",
       pch=19
)

#plot(rating)
level <- 9
logInterpolate(rating, level)
points(level,
       logInterpolate(rating, level),
       col="orange",
       pch=19
)

?plot()
