#' Processes EMD blocks at at time, and then interpolates between
#'
#' Uses predefined ceemdan parameters, specifically for estuaries
#'
#'
#' @param tsdata dataframe, with posixct for time and values, and a defined interval i.e. hourly
#' @param tsblocks tsblocks, data.frame(Start = posixct, End=posixct)
#' @param method "emd" or "ceemdan"
#'
#' @return dataframe of filtered/decomposed data
#'
#' @importFrom dplyr filter
#' @importFrom stats approxfun
#'
#' @examples
#'
#' library(ccInterp)
#' library(ggplot2)
#'
#' # make some random data
#' randomMinutes <- rnorm(10000, mean = 60, sd = 100) # create 1000 random intervals
#' randomMinutes <- randomMinutes[randomMinutes > 30]  # cull short or negative intervals
#' randomMinutes <- as.POSIXct(cumsum(randomMinutes)*60, origin="2021-01-01")
#' # build a data frame of times and values
#' df <- data.frame(Time = randomMinutes,
#'                # add a row of random numbers
#'                data = cumsum(rnorm(length(randomMinutes), mean = 0, sd = 10))
#' )
#'
#' # change to hourly
#' hourlydf <- changeInterval(df, Interval = "Hourly", option="inst", offset = 30)
#'
#' gaps <- GapList(df, 360) # 6 hour gaps
#' blocks <- blockList(gaps, df)
#' data <- processEMD(hourlydf, blocks)
#' plot(df, col="grey")
#' lines(data$time, data$a)
#' lines(data$time, data$b, col="blue")
#' lines(data$time, data$c, col="red")
#' @export
#'
processEMD <- function(tsdata, tsblocks=0, method="ceemdan" )
{
  emdcount <- 0
  emdlist <- list()

  colnames(tsdata)[1] <- "Date"
  colnames(tsdata)[2] <- "Data"

  tsblocks <- tsblocks %>% dplyr::filter(End > tsdata$Date[1] & Start < tsdata$Date[nrow(tsdata)])

  if (nrow(tsblocks)==0)
  {
    # default is period of record
    tsblocks <- data.frame(Start=tsdata[1,1], End = tsdata[nrow(tsdata), 1] )
  }

  # subsection and EMD for each block
  for(i in 1:nrow(tsblocks) )
  {
    message(paste("Filtering:", tsblocks$Start[i], "to:", tsblocks$End[i]))
    subsection <- tsdata %>% dplyr::filter(Date >= tsblocks$Start[i], Date <= tsblocks$End[i]) # minimum duration for block, otherwise gap

    if (nrow(subsection) > 0)
    {
      ceemdanVel <- ceemdanFilter(subsection[,1], subsection[,2], method=method, output=1 )
      if (is.null(ceemdanVel))
      {
        ceemdanVel <- data.frame(time=subsection[,1], a=subsection[,2], b=0, c=0)
      }
      names(ceemdanVel) <- c("time","a","b","c")
      emdcount <- emdcount + 1
      emdlist[[emdcount]] <- ceemdanVel
    }
  }

  combinedEMD <- do.call(rbind,emdlist)
  f.a <- approxfun(combinedEMD$time, combinedEMD$a)
  f.b <- approxfun(combinedEMD$time, combinedEMD$b)
  f.c <- approxfun(combinedEMD$time, combinedEMD$c)

  df <- data.frame(time=tsdata$Date, a=f.a(tsdata$Date), b=f.b(tsdata$Date), c=f.c(tsdata$Date))
  return(df)
  #return(combinedEMD)
}
