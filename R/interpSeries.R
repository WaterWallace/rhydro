#' Interpolates for xy coordinates
#'
#' To interpolate values (xaxis) at a location (yaxis), in a series of data.
#'
#' @param series A dataframe, with x,y coordinates
#' @param location Numeric, location along the line (y-axis) of which to interpolate on the x-axis
#'
#' @return dataframe
#' @importFrom(stats,approxfun)
#'
#' @examples
#' xs <- data.frame(chain = c(1,2,3,4,5,6,7),
#'     rl = c(10,9,7,4,8,9,11)
#' )
#' # Interpolate the cross section at an RL of 8.5
#'interpSeries(xs, 8.5)
#'
#' @export
#'
interpSeries <- function(series, location)
{
  # to interpolate values (xaxis) at a location (yaxis), in a series of data.
  series <- series[order(series$chain),]

  neighbs <- data.frame(rla = c(0,series[,2]),rlb=c(series[,2],0))
  neighbs <- cbind(neighbs, chaina = c(0,series[,1]),chainb=c(series[,1],0))

  neighbs <- neighbs[c(-1,-nrow(neighbs)),]
  interpFor <- neighbs[neighbs$rla > location & neighbs$rlb < location | neighbs$rlb > location & neighbs$rla < location  ,]

  if (nrow(interpFor) > 0)
  {
    interpPoints <- list()
    count <- 0
    for (i in 1:nrow(interpFor))
    {
      count <- count+1
      pointrl <- interpFor[i,1:2]
      pointchain <- interpFor[i,3:4]
      f.point <- approxfun(pointrl, pointchain)
      interpPoints[[count]] <- data.frame(chain = f.point(location), rl = location)
    }
    interpPoints <- do.call(rbind,interpPoints)
    names(interpPoints) <- names(series)
    interpPoints <- rbind(series, interpPoints)
    interpPoints <- interpPoints[order(interpPoints$chain),]
    return(interpPoints)

  }else{
    return(series)
  }

}
