#' Creates a list of gaps, with a start time, end time and duration
#'
#' Outputs gaps of a defined gap length
#'
#' @param tsdata A dataframe, with first column POSIXct time
#' @param minGap Numeric, number of minutes to record a gap
#'
#' @return dataframe listing each gap start, end and duration.
#' @importFrom dplyr filter
#'
#' @examples
#'
#'
#' library(dplyr)
#' # create a df of hourly times
#'
#' randomMinutes <- rnorm(1000, mean = 60, sd = 30) # create 1000 random intervals
#' randomMinutes[randomMinutes < 1] <- 1 # set less than 1 minute to 1
#'  randomMinutes <- as.POSIXct(cumsum(randomMinutes)*60, origin="2021-01-01")
#'
#' # Gaplist of a single column dataframe
#' head(GapList(data.frame(randomMinutes), 60)) # 60 minutes minimum gap
#' head(GapList(data.frame(randomMinutes), 120)) # 120 minutes minimum gap
#' head(GapList(data.frame(randomMinutes), 180)) # 180 minutes minimum gap
#'
#' # build a data frame of times and values
#' df <- data.frame(Time = randomMinutes,
#'                 data = cumsum(rnorm(1000, mean = 0, sd = 1))   # add a row of random numbers
#' )
#'
#' plot(df)
#' head(GapList(df, 120)) # Gaplist of a multi column dataframe (same result)
#'
#'
#'
#' @export
#'


GapList <- function(tsdata, minGap = 60 )
{
  # add a column of gaps
  tsdata <- cbind(tsdata, gap = difftime(c(tsdata[-1,1],  tsdata[nrow(tsdata),1]), c(tsdata[,1]), units="mins"  ))
  # create a list of gaps
  gap <- tsdata %>% dplyr::filter(gap > minGap)
  # add end date to gap list
  return( data.frame(start = gap[,1], end = gap[,1]+gap$gap, gap = gap$gap ) )
}
