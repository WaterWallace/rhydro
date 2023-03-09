#' Creates a list of blocks, with a start time, end time and duration
#'
#' @param gaps Dataframe, output from GapList()
#' @param ts Dataframe, with first column POSIXct time
#' @param minblock Numeric, minimum length of a "block" in minutes
#'
#' @return dataframe listing each gap start, end and duration.
#' @importFrom dplyr filter
#'
#' @examples
#'
#' # create a df of hourly times
#'
#' randomMinutes <- rnorm(1000, mean = 60, sd = 30) # create 1000 random intervals
#' randomMinutes[randomMinutes < 1] <- 1 # set less than 1 minute to 1
#' randomMinutes <- as.POSIXct("2021-01-01") + cumsum(randomMinutes)*60 # add to a start point
#'
#' # Gaplist of a single column dataframe
#' gaps <- GapList(data.frame(randomMinutes), 120) # 180 minutes minimum gap
#'
#' # build a data frame of times and values
#' df <- data.frame(Time = randomMinutes,
#'                 data = cumsum(rnorm(1000, mean = 0, sd = 1))   # add a row of random numbers
#' )
#'
#'
#' ten <- blockList(gaps, df, minblock = 10) # at least 10 minutes per block
#' day <- blockList(gaps, df, minblock = 1440) # day long blocks only
#'
#' head(ten)
#' head(day)
#'
#' @export
blockList <- function(gaps, ts, minblock = 360)
{

  ts <- na.omit(ts)
  ts <- ts[,1]

  gaps$start <- as.numeric(gaps$start)/60
  gaps$end <- as.numeric(gaps$end)/60
  ts <- as.numeric(ts)/60

  # convert gaps, to blocks
  blocks <- data.frame(Start = c(ts[1], gaps$end), End = c(gaps$start, ts[length(ts)]) ) # blocks in minutes
  blocks$dur <- (blocks$End - blocks$Start) # calc duration of data block

  blocks <- blocks %>% dplyr::filter(dur > minblock) # minimum duration for block, otherwise gap

  # convert from minutes to posix
  blocks$Start <- as.POSIXct(blocks$Start*60, origin="1970-01-01")
  blocks$End <- as.POSIXct(blocks$End*60, origin="1970-01-01")

  return (blocks)
}

?ccInterp::StevesCoolRandomTS()

