#' Calculate cross sectional area for a a range of periods
#'
#' Applies a rating table to height data for a time period
#'     If the phased box is set to true, the result is based on a time factor between the
#'     current rating table and the next rating table.
#'
#' @details
#' The input parameters *RatePer*, *offsets*, *points*, are retrieved as a list
#' from an influx database retrieval script.
#'
#' @param RatePer *dataframe*, with columns *PeriodStart*, *End*, *Phased*, *TableNum*
#' \itemize{
#'     \item{*PeriodStart*}{ Posixct, start time of the rating period" }
#'     \item{*TableNum*}{ String, the section ID number to be applied for this time }
#'     \item{*Phased*}{ Boolean, TRUE of FALSE }
#'     }
#' @param points list, of dataframes, including *from*, *to*, *ratingID*, *QC*
#' \itemize{
#'     \item{*from*}{ Numeric, Reference value, i.e. height }
#'     \item{*to*}{ Numeric, Lookup value, i.e. discharge }
#'     \item{*ratingID*}{ String, ID of rating curve, i.e. "1", "Interim", "Debris1" }
#'     \item{*QC*}{ Integer, Quality code }
#'     }
#' @param stagedf dataframe, with columns for a *timestamp* and a *height*
#' \itemize{
#'     \item{*timestamp*}{ posixct, Reference value, i.e. height }
#'     \item{*height*}{ Numeric, Lookup value, i.e. discharge }
#'     }
#' @param offsets *(optional)* dataframe, with columns "PeriodStart", "TableNum", "logOffset"
#' \itemize{
#'     \item{*TableNum*}{ String, the section ID of the cross section}
#'     \item{*logOffset*}{ Numeric, logOffset of this rating table, If zero, offset will be calculated }
#'     }
#'
#' @return Vector of log interpolated data as read from the rating table,
#'     same timestep as the input height.
#'
#' @examples
#'
#' #prepared exaple
#' data(tinana)
#' out <- PhasedLoginterp(rating$Periods, rating$XS, level, rating$Offsets)
#' head(out)
#' plot(level$level.ts, out$Value+0.001, log="y")
#'
#' #Randomly generated example
#' library(dplyr)
#' set.seed(10)
#' #  random timeseries generator
#' maxht <- 5
#' ts <- ccInterp::StevesCoolRandomTS(maxht)
#' ts <- ts %>% dplyr::select(Time, Signal)
#'
#' tmin <- ts$Time[1]
#' tmax <- ts$Time[nrow(ts)]
#'
#' numperiods <- 6
#' periods <- data.frame(
#'   PeriodStart = seq(tmin, tmax, length=numperiods),
#'   TableNum = runif(numperiods, 1, 2*numperiods) %>% round %>% paste,
#'   Phased =  runif(numperiods, 0,1) %>% round %>% as.logical
#' )
#'
#' plot(periods$PeriodStart, periods$TableNum, type = "s")
#'
#' numTables <- unique(periods$TableNum) %>% length()
#' ratings <- list()
#' for(rating in 1:numTables)
#' {
#'
#'   hts <- seq ( log(runif(1, 0.01, 0.1) ), log(max(ts$Signal)), length= 10 )
#'   hts <- exp(hts)
#'
#'  ratingID <- unique(periods$TableNum)[rating]
#'  ratings[[ratingID]] <- data.frame(from = round(hts,3),
#'                                  to = round(hts^(runif(1, 2.5, 3.5)), 3),
#'                                  ratingID = ratingID,
#'                                  QC = 10 )
#' }
#'
#' offsets <- data.frame(
#' TableNum = unique(periods$TableNum),
#'   logOffset = do.call(rbind,
#'   lapply(ratings,
#'   function(rating){
#'    rating <- rating[rating$to > 0,]
#'    min(rating$from) - 0.02
#'   })))
#'
#' #calculate discharge with PhasedLoginterp()
#' ts <- cbind(ts, PhasedLoginterp(periods, ratings, ts, offsets))
#'
#' par(mar = c(5, 4, 4, 4) + 0.3)  #  Leave space for z axis
#' plot(Signal ~ Time, data = ts, type="l", col="darkgreen")
#' par(new = TRUE)
#' plot(ts$Time, ts$Value, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col="maroon")
#' axis(side=4, at = pretty(range(ts$Value)))
#' mtext("Value", side=4, line=3)
#'
#' @export
#'

# Have to check that it uses only the latest offsets for each TableNum
PhasedLoginterp <- function(RatePer, points, stagedf, offsets = NULL )
{

  #PhasedLoginterp(rating$Periods, rating$XS, level, rating$Offsets)

  #RatePer <- periods
  #offsets
  #points <- ratings
  #stagedf <- ts

  #RatePer <- rating$Periods
  #points <- rating$XS
  #stagedf <- level
  #offsets <- rating$Offsets

  #RatePer <- mrdxs$Periods
  #points <- mrdlookups
  #stagedf <- data.frame(timestamp = mrdht[[1]]$time,
  #                      height = mrdht[[1]]$value_Level )


  points <- do.call(rbind, points)
  #points <- points %>% select(!QC)

  ratingsTimesList <- function(starttimes, timestamps) # creates a list of times of models
  {
    #RatePer, stagedf[,1]
    starttimes <- RatePer
    timestamps <- stagedf[,1]

    firstDataPoint <-  timestamps[1]
    lastDataPoint <- timestamps[length(timestamps)]

    if(firstDataPoint > starttimes[nrow(starttimes),1] ) return( cbind(starttimes[nrow(starttimes),], End = lastDataPoint) )

    dfModelTimes <- data.frame( rbind(NA, starttimes),
                                End = c(starttimes[,1], starttimes[nrow(starttimes),1]) # add end times
    )
    dfModelTimes <- dfModelTimes[-1,] # exclude redundant top row

    # include rows that start after first data point
    headSelect <-  firstDataPoint < dfModelTimes$PeriodStart
    # include rows that start before last data point
    tailSelect <- lastDataPoint > dfModelTimes$PeriodStart
    # expand selection
    headSelect <- c(headSelect[-1], headSelect[length(headSelect)] )
    tailSelect <- c(tailSelect[1], tailSelect[-length(tailSelect)] )
    selection <- (headSelect & tailSelect) # head or tail TRUE

    # exclude any times outside start and end times
    dfModelTimes <- dfModelTimes[selection,]

    return(dfModelTimes)
  }

  periods <- ratingsTimesList(RatePer, stagedf[,1])
  convertedList <- list()
  lenPeriods <- nrow(periods)

  QCPresent <- 'QC' %in% names(points)

  for(periodNum in 1:lenPeriods)
  {
    # periodNum <- 2
    thisPeriod <- periods[periodNum,]
    message(paste("from: ", thisPeriod$PeriodStart, " to: ", thisPeriod$End, thisPeriod$Phased, thisPeriod$TableNum) )

    # subset heights
    if(periodNum < lenPeriods)
    {
      stagesubset <- stagedf[stagedf[,1] >= thisPeriod$PeriodStart & stagedf[,1] < thisPeriod$End,]
    }else{
      stagesubset <- stagedf[stagedf[,1] >= thisPeriod$PeriodStart,]
    }

    if(nrow(stagesubset) > 0)
    {

      #thisTable <- points[[paste(thisPeriod$TableNum)]]
      thisTable <- points[points$ratingID == thisPeriod$TableNum,] # start table

      if ( !is.null(offsets) )
      {
        thisLog <- offsets[offsets$TableNum == thisPeriod$TableNum,]$logOffset
      }else
      {
        thisLog <- 0
      }

      lookup1 <- data.frame(thisTable$from, thisTable$to)
      if (QCPresent) lookup1$QC <- thisTable$QC

      phasedareas <- logInterpolate(lookup1, stagesubset[,2], logOffset = thisLog)

      if(periodNum < lenPeriods & thisPeriod$Phased == 1)
      {
        nextPeriod <- periods[(periodNum+1),]

        #nextTable <- points[[paste(nextPeriod$TableNum)]]
        nextTable <- points[points$ratingID == nextPeriod$TableNum,] # start table

        if ( !is.null(offsets) )
        {
          nextLog <- offsets[offsets$TableNum == nextPeriod$TableNum,]$logOffset
        }else
        {
          nextLog <- 0
        }
        #nextTable <- xsdf[xsdf$n == nextPeriod$TableNum,] # end table

        #lookup2 <- areaLookup(dplyr::select(nextTable, c("CHAIN","RL"))) # get lookup table of next xs
        #lookup2 <- data.frame(nextTable$from, nextTable$to, nextTable$QC)
        lookup2 <- data.frame(nextTable$from, nextTable$to)
        if (QCPresent) lookup2$QC <- nextTable$QC

        phasedareas2 <- logInterpolate(lookup2, stagesubset[,2], logOffset = nextLog) # calculate areas

        #######
        minutesBetweenRate <- difftime( nextPeriod$PeriodStart, thisPeriod$PeriodStart, units="mins" )
        sinceBegin <- difftime( stagesubset[,1], thisPeriod$PeriodStart, units="mins" )
        ratio <- as.numeric(sinceBegin/as.numeric(minutesBetweenRate))

        phasedareas$value <- phasedareas$value*(1-ratio)+phasedareas2$value*ratio
        if (QCPresent) phasedareas$qc <- pmax(phasedareas$qc, phasedareas2$qc)

      }
      convertedList[[periodNum]] <- data.frame(Time = stagesubset[,1],
                                               Value = phasedareas$value)


      if (QCPresent)  convertedList[[periodNum]]$QC <- phasedareas$qc

    }else{
      # skip empty data
    }

  }

  convertedList <- do.call(rbind, convertedList)

  if(convertedList[1,1] > stagedf[1,1]) message("warning: areas trimmed to match lookup dates")

  head(convertedList)
  f.out <- approxfun(convertedList$Time, convertedList$Value)
  if(QCPresent){
    f.qc <- approxfun(convertedList$Time, convertedList$QC)
    return(data.frame(Value = f.out(stagedf[,1]),
                      QC = f.qc(stagedf[,1])))
  }else{
    df <- data.frame(Value = f.out(stagedf[,1]))
    return(df)
  }


}

