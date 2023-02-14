#' Combines two timeseires together
#'
#' Either adds, subtracts, divides or multiplies
#' But also can apply transformations to each series before combining
#'
#' @param seriesA a dataframe of posixct time, and value (primary var)
#' @param seriesB (optional) a dataframe of posixct time, and value
#' @param multiA Numeric Multiplier for seriesA
#' @param offsetA Numeric Offset for seriesA
#' @param timeAdjA Numeric time adjust for seriesA (to subtract from )
#' @param multiB Numeric Multiplier for seriesB
#' @param offsetB Numeric Offset for seriesB
#' @param timeAdjB Numeric time adjust for seriesB (to subtract from )
#' @param combMethod "Add", "Subtract", "Multiply", "Divide"
#'
#' @return dataframe listing each gap start, end and duration.
#' @importFrom dplyr filter
#'
#' @examples
#'
#'  # create a df of hourly times
#'  numPoints <- 50
#'  # SeriesA
#'  randomMinutes <- rnorm(numPoints, mean = 120, sd = 30) # create 100 random intervals
#'  randomMinutes[randomMinutes < 1] <- 1 # set less than 1 minute to 1
#'  randomMinutes <- as.POSIXct(cumsum(randomMinutes)*60, origin="2021-01-01")
#'
#'  # build a data frame of times and values
#'  dfA <- data.frame(Time = randomMinutes,
#'                  data = cumsum(rnorm(numPoints, mean = 0, sd = 2))   # add a row of random numbers
#'  )
#'  # SeriesB
#'  randomMinutes <- rnorm(numPoints, mean = 120, sd = 30) # create 100 random intervals
#'  randomMinutes[randomMinutes < 1] <- 1 # set less than 1 minute to 1
#'  randomMinutes <- as.POSIXct(cumsum(randomMinutes)*60, origin="2021-01-01")
#'
#'  # build a data frame of times and values
#'  dfB <- data.frame(Time = randomMinutes,
#'                    data = cumsum(rnorm(numPoints, mean = 0, sd = 2))   # add a row of random numbers
#'  )
#'  # add two timeseries
#'  addedTS <- addTS(dfA, dfB)
#'  plot(dfA, xlim=c(min(addedTS$Date), max(addedTS$Date)),
#'       ylim=c(min(addedTS$TS), max(addedTS$TS)))
#'  points(dfB, col="red")
#'  points(addedTS, col="blue")
#'  # mulitply two timeseries
#'  multiTS <- addTS(dfA, dfB, combMethod = "Multiply")
#'  plot(dfA, xlim=c(min(multiTS$Date), max(multiTS$Date)),
#'       ylim=c(min(multiTS$TS), max(multiTS$TS)))
#'  points(dfB, col="red")
#'  points(multiTS, col="blue")
#'  # multiply a single timeseires by a factor
#'  multiTS <- addTS(dfA, multiA = 5)
#'  plot(dfA, xlim=c(min(multiTS$Date), max(multiTS$Date)),
#'       ylim=c(min(dfA$data, multiTS$TS), max(dfA$data, multiTS$TS)))
#'  points(multiTS, col="blue")
#'  # offset a single timeseires by a factor
#'  multiTS <- addTS(dfA, offsetA = 50)
#'  plot(dfA, xlim=c(min(multiTS$Date), max(multiTS$Date)),
#'       ylim=c(min(dfA$data, multiTS$TS), max(dfA$data, multiTS$TS)))
#'  points(multiTS, col="blue")
#'  # offset time of a timeseries
#'  multiTS <- addTS(dfA, timeAdjA =  240)
#'  plot(dfA, type="l", xlim=c(min(multiTS$Date), max(multiTS$Date)),
#'       ylim=c(min(dfA$data, multiTS$TS), max(dfA$data, multiTS$TS)))
#'  lines(multiTS, type="l", col="blue")
#'  multiTS <- addTS(dfA, timeAdjA =  -60)
#'  lines(multiTS, col="red")
#'
#'
#' @export
#'


addTS <- function(seriesA, seriesB=0, multiA=1, offsetA=0, timeAdjA=0, multiB=1, offsetB=0, timeAdjB=0, combMethod = "Add")
{

  seriesA[,2] <- seriesA[,2] * multiA
  seriesA[,2] <- seriesA[,2] + offsetA
  seriesA[,1] <- seriesA[,1] - ( timeAdjA * 60 )

  if( length(seriesB) != 1  ){
    seriesB[,2] <- seriesB[,2] * multiB
    seriesB[,2] <- seriesB[,2] + offsetB
    seriesB[,1] <- seriesB[,1] - ( timeAdjB * 60 )
  }else{
    # if no series B, make it same as seriesA but with no data
    seriesB <- data.frame(seriesA[,1],
                          data = switch(combMethod,
                                 "Add" = 0,
                                 "Subtract" = 0,
                                 "Multiply" = 1,
                                 "Divide" = 1
                          )
                  )
  }

  start <- max(min(seriesA[,1]), min(seriesB[,1]))
  end <- min(max(seriesA[,1]), max(seriesB[,1]))

  #
  alltimes <- c(
    seriesA[,1][seriesA[,1] >= start & seriesA[,1] <= end],
    seriesB[,1][seriesB[,1] >= start & seriesB[,1] <= end]
  )
  alltimes <- alltimes[order(alltimes)]
  alltimes <- unique(alltimes)

  #
  f.A <- approxfun(seriesA[,1], seriesA[,2])
  f.B <- approxfun(seriesB[,1], seriesB[,2])

  bothseries <- data.frame(alltimes, f.A(alltimes), f.B(alltimes))
  head(bothseries)

  return(data.frame(Date = alltimes, TS = switch(combMethod,
                                                 "Add" = bothseries[,2] + bothseries[,3],
                                                 "Subtract" = bothseries[,2] - bothseries[,3],
                                                 "Multiply" = bothseries[,2] * bothseries[,3],
                                                 "Divide" = bothseries[,2] / bothseries[,3])
                    )
        )


}


