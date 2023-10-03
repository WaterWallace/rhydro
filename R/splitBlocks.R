#' Omit or include data between dates
#'
#' Used for merging datasets, or deleting data during gaps
#'
#' @param blocklist output from blockList/GapList
#' @param newdata dataframe, timestamp, and one or more columns of data
#' @param typekey optional, for interpolating boundaries
#' a vector descring the interpolation of the
#' respective columns.
#' 0 = linear
#' 1 = maximum neighbor
#' NULL = linear for all
#' Where not NULL, must be length of input dataframes
#' @param include to either include the 'newdata' or exclude it
#' based on blocklist start/end.
#' i.e. when include is 1, the 'newdata' is chunked to only dates within
#' the 'blocklist', including interpolated block boundaries
#' when include is 0, 'newdata' is chunked to only dates outside the blocklist.
#'
#' @return dataframe of the original data within, or outside of blocklist.
#'
#'
#' @importFrom dplyr filter
#'
#' @examples
#' library(dplyr)
#'
#'
#' ts <- ccInterp::StevesCoolRandomTS(smoothed = FALSE, obs = 10000 )
#' plot(ts$Time, ts$Noise)
#' gaps <- rhydro::GapList(ts, minGap = 120)
#'
#' # include between gaps
#' splitTS <- splitBlocks(gaps, ts, include = 1)
#' plot(splitTS$Time, splitTS$Noise, col="red")
#' #exclude between gaps
#' splitTS <- splitBlocks(gaps, ts, include = 0)
#' points(splitTS$Time, splitTS$Noise, col="blue")
#'
#' splitTS <- blockList(gaps, ts) %>% # convert gaps to blocks of ts
#' splitBlocks(ts, include = 1) # inclide blocks only
#' plot(splitTS$Time, splitTS$Noise, col="blue")
#'
#' splitTS <- blockList(gaps, ts) %>% # convert gaps to blocks of ts
#' splitBlocks(ts, include = 0)
#' points(splitTS$Time, splitTS$Noise, col="red")
#' @export
#'

splitBlocks <- function(blocklist, newdata, typekey = NULL, include = 1)
{

  maxfun <- function(t, y, x, option = "max") # look forward and back to pick the max (or min)
  {
    # function looks to either side of point x and outputs the minimum
    f.qual0 <- approxfun(t, y, method = "constant", ties = option, f = 0, rule = 2 )
    f.qual1 <- approxfun(t, y, method = "constant", ties = option, f = 1, rule = 2 )

    if (option == "max") {
      return(pmax(f.qual0(x), f.qual1(x)))
    } else if (option == "min") {
      return(pmin(f.qual0(x), f.qual1(x)))
    }
  }

  stopifnot("include must be 1 or 0" = include == 1 | include == 0)
  # start = set to include (or not)
  # end = set to not include (or do)
  # depending on value of "include"
  tags <- rbind(
    data.frame( date = blocklist[,1], tag = include),
    data.frame( date = blocklist[,2], tag = abs(include-1) )
  )

  tags <- tags[order(tags$date),]  # sort
  dupes <- tags$date[duplicated(tags$date)]  # duplicates
  # duplicates are just single points separated by gaps
  tags <- tags[!tags$date %in% dupes,]  # remove duplicates
  f.tags <- approxfun(tags, method = "constant") # lookup function
  # boundaries to interpolate and include ( or not )
  bounds <- c( blocklist[,1], blocklist[,2] )

  # interpolate points at point boundaries
  boundsList <- list()
  boundsList[[1]] <- bounds
  # determine how to interpolate data
  for( dataset in 2:length(newdata))
  {
    if(is.null(typekey))
    {
      boundsList[[dataset]] <- approx(newdata[,1], newdata[,dataset],
                                      bounds)$y
    }else{
      # interpolate
      if( typekey[dataset] == 0 ){
        boundsList[[dataset]] <- approx(newdata[,1], newdata[,dataset],
                                        bounds)$y
        # maximum value
      }else if( typekey[dataset] == 1){
        boundsList[[dataset]] <- maxfun(newdata[,1], newdata[,dataset],
                                        bounds)
      }
    }
  }
  boundsList <- do.call(cbind, boundsList) %>% as.data.frame
  boundsList[,1] <- as.POSIXct(boundsList[,1], origin = "1970-01-01")
  names(boundsList) <- names(newdata)
  newdata$tags <- f.tags(newdata[,1]) # tag is whether to include or not
  boundsList$tags <- include # include boundaries ( or not )
  newdata <- rbind( boundsList, newdata )

  # tag boundaries to 1 or 0, 0 will exclude the boundary as well
  newdata$tags [ newdata[,1] %in% boundsList[,1] ] <- include

  # sort, remove duplicates
  newdata <- newdata[order(newdata[,1]),]
  newdata <- distinct(newdata,
                      .data[[names(newdata)[1]]], # column 1 name
                      .keep_all = TRUE)
  # omit NA from result
  newdata$tags[is.na(newdata$tags)] <- 0

  # include only those with the correct tag
  count <- 0
  splitblocks <- list()
  for ( i in split(newdata,rleid( newdata$tags ) ) )
  {
    if(unique(i$tags) == 1)
    {
      splitblocks[[count <- count+1]] <- i
    }
  }

  splitblocks <- do.call(rbind, splitblocks)
  return(splitblocks[,-length(splitblocks)])
  #return(splitblocks)
}










