#' Creates a list of blocks, with a start time, end time and duration
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
#' randomMinutes <- as.POSIXct("2021-01-01") + cumsum(randomMinutes)*60 # add to a start point
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
#'
#'
#'


blockList <- function(gaps, ts)
{


  ts <- na.omit(ts)
  ts <- ts[,1]

  gaps$start <- as.numeric(gaps$start)/60
  gaps$end <- as.numeric(gaps$end)/60
  ts <- as.numeric(ts)/60

  # convert gaps, to blocks
  blocks <- data.frame(Start = c(ts[1], gaps$end), End = c(gaps$start, ts[length(ts)]) ) # blocks in minutes
  blocks$dur <- (blocks$End - blocks$Start) # calc duration of data block
  blocks <- blocks %>% dplyr:::filter(dur > 360) # minimum duration for block, otherwise gap

  # convert from minutes to posix
  blocks$Start <- as.POSIXct(blocks$Start*60, origin="1970-01-01")
  blocks$End <- as.POSIXct(blocks$End*60, origin="1970-01-01")

  return (blocks)
}
