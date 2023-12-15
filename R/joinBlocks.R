#' Join two blocks together depending on specific dates
#'
#' Used for merging datasets, or deleting data during gaps
#' Length of original and infill must be the same
#' Typekey also must be the same length if used
#'
#' @param original dataframe, timestamp, and one or more columns of data
#' @param infill dataframe, timestamp, and one or more columns of data
#' @param minGap minimum gap in the original data to infill
#' i.e. replace any gap over two hours with infill data
#' @param typekey optional, for interpolating boundaries
#' a vector descring the interpolation of the
#' respective columns.
#' 0 = linear
#' 1 = maximum neighbor
#' NULL = linear for all
#' Where not NULL, must be length of input dataframes
#'
#' @return dataframe of the original data infilled with infill data.
#'
#' @importFrom dplyr filter
#'
#' @examples
#' library(dplyr)
#'
#'
#'
#'ts <- ccInterp::StevesCoolRandomTS(smoothed = FALSE)
#'ts2 <- ccInterp::StevesCoolRandomTS(smoothed = TRUE)
#'
#'# stretch ts2 to fit ts
#'ts$Time <- as.numeric(ts$Time)
#'ts2$Time <- as.numeric(ts2$Time)
#'
#'#identify
#'ts$set <- 1
#'ts2$set <- 2
#'
#'# offset
#'ts2$Time <- ts2$Time - (min(ts2$Time) -min(ts$Time))
#'
#'# scale
#'ts2$Time <- min(ts2$Time) + ( ts2$Time - min(ts2$Time) ) *
#'  diff(range(ts$Time)) /
#'  diff(range(ts2$Time))
#'
#'ts$Time <- as.POSIXct(ts$Time, origin = "1970-01-01")
#'ts2$Time <- as.POSIXct(ts2$Time, origin = "1970-01-01")
#'
#'# infill ts1 with ts2
#'plot(ts$Time, ts$Signal)
#'joined <- joinBlocks(ts, ts2, minGap = 120)
#'points(joined$Time, joined$Signal, col="red")
#'
#'# plot which dataset was used
#'plot(joined$Time, joined$set)
#'
#' @export
#'
#'
joinBlocks <- function(original, infill, minGap = 120, minBlock = 120, typekey = NULL)
{
  stopifnot("must be equal columns" = length(original) == length(infill) )
  if (!is.null(typekey)){
    stopifnot("typekey must be length of inputs" = length(typekey) == length(infill) )
  }

  if(infill[1,1] < original[1,1]) {
    toprowinfill <- infill[1,]
    names(toprowinfill) <- names(original)
    original <- rbind(toprowinfill, original)
  }
  if(infill[nrow(infill), 1] < original[nrow(infill), 1]) {
    bottomrowinfill <- infill[nrow(infill),]
    names(bottomrowinfill) <- names(original)
    original <- rbind(bottomrowinfill, original)
  }

  gaplist <- original %>% GapList(minGap = minGap)
  #blocklist <- rhydro::blockList(gaplist, original, minblock = minBlock)

  original <- splitBlocks(gaplist, original, include = 0, typekey = typekey)
  infill <- splitBlocks(gaplist, infill, include = 1, typekey = typekey)

  names(infill) <-  names(original)
  original <- rbind(infill, original)
  original <- original[order(original[,1]),]
  return(original)
}
