#' Calculate cross sectional area for a a range of periods
#'
#' Applies a rating table to height data for a time period
#'     If the phased box is set to true, the result is based on a time factor between the
#'     current rating table and the next rating table.
#'
#' @param RatePer dataframe, with columns "PeriodStart", "End", "Phased", "TableNum"
#'     PeriodStart (Posixct) start time of the rating period
#'     Phased (Numeric) 1 or 0.
#'     TableNum (Alphanumeric) the section ID number to be applied for this time
#' @param xsdf dataframe, with columns "CHAIN", "RL" and "n"
#'     CHAIN (Numeric) cross section chainages
#'     RL (Numeric) cross section Reduced levels
#'     n (Aphanumeric) a unique cross section ID number for a particuar cross section.
#'         n is same as TableNum in RatePer
#' @param stagedf dataframe, with columns for a timestamp (posixct) and a height (Numeric)
#'
#' @return Vector of areas, from a lookup table at each Stage timestamp,
#'     therefore will always merge with Stage.
#'
#' @examples
#' library(ggplot2)
#' # Set cross sections
#' xs <- data.frame(CHAIN = c(1,2,3,4,5,6,7),
#'     RL = c(10,9,7,4,8,9,11)
#' )
#' xs2 <- data.frame(CHAIN = c(1,2,3,4,5,6,7),
#'     RL = c(10,9,4,4,4,9,11)
#' )
#' xs$n <- "xs1"
#' xs2$n <- "xs2"
#' XSDF <- rbind(xs, xs2)
#' # plot each cross section
#' ggplot(XSDF, aes(CHAIN, RL, colour=n))+
#'  geom_line()+
#'  geom_point()
#'
#' # Set rating periods
#' Periods <- data.frame(
#'     PeriodStart = as.POSIXct(c("2020-01-01 00:00", "2021-01-01 00:00", "2022-01-01 00:00")),
#'     Phased = c(1,0,1),
#'     TableNum = c("xs1", "xs2", "xs1")
#'     )
#'
#' # Create a constant stage over time
#' Stage <- data.frame(
#'     timestamp = seq(as.POSIXct("2020-01-01 00:00"), as.POSIXct("2024-01-01 00:00"),
#'         by = 60*60*24), # daily timestamps
#'     height = 7
#'   )
#'
#' # Apply phased rating tables to stage
#' Stage$Area <- PhasedAreas(Periods, XSDF, Stage)
#' # Heights covering entire rating periods
#' plot(Stage$timestamp, Stage$Area)
#'
#' # Subsection Stage
#' Stage <- data.frame(
#'     timestamp = seq(as.POSIXct("2020-06-01 00:00"), as.POSIXct("2024-01-01 00:00"),
#'         by = 60*60*24), # daily timestamps
#'     height = 7
#'   )
#'
#' # Apply phased rating tables to stage
#' Stage$Area <- PhasedAreas(Periods, XSDF, Stage)
#' # Plots starting after the first rating period has started
#' plot(Stage$timestamp, Stage$Area)
#' # Subsection Stage
#' Stage <- data.frame(
#'     timestamp = seq(as.POSIXct("2019-01-01 00:00"), as.POSIXct("2024-01-01 00:00"),
#'         by = 60*60*24), # daily timestamps
#'     height = 7
#'   )
#'
#' # Apply phased rating tables to stage
#' Stage$Area <- PhasedAreas(Periods, XSDF, Stage)
#' # Heights starting before rating periods - No values before period starts.
#' plot(Stage$timestamp, Stage$Area)
#'
#' # Set rating periods to always phased change
#' Periods <- data.frame(
#'     PeriodStart = as.POSIXct(c("2020-01-01 00:00", "2021-01-01 00:00",
#'         "2022-01-01 00:00", "2024-01-01 00:00")),
#'     Phased = c(1,1,1,1),
#'     TableNum = c("xs1", "xs2", "xs1", "xs2")
#'     )
#'
#' # Apply phased rating tables to stage
#' Stage$Area <- PhasedAreas(Periods, XSDF, Stage)
#' # Heights covering entire rating periods
#' plot(Stage$timestamp, Stage$Area)
#'
#' @export
#'
PhasedAreas <- function(RatePer, xsdf, stagedf )
{


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
  areasList <- list()
  lenPeriods <- nrow(periods)
  for(periodNum in 1:lenPeriods)
  {
    # periodNum <- 1
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
      thisTable <- xsdf[xsdf$n == thisPeriod$TableNum,] # start table
      lookup1 <- areaLookup(dplyr::select(thisTable, c("CHAIN","RL")))
      phasedareas <- logInterpolate(lookup1, stagesubset[,2])

      if(periodNum < lenPeriods & thisPeriod$Phased == 1)
      {
        nextPeriod <- periods[(periodNum+1),]
        nextTable <- xsdf[xsdf$n == nextPeriod$TableNum,] # end table

        lookup2 <- areaLookup(dplyr::select(nextTable, c("CHAIN","RL"))) # get lookup table of next xs
        phasedareas2 <- logInterpolate(lookup2, stagesubset[,2]) # calculate areas

        #######
        minutesBetweenRate <- difftime( nextPeriod$PeriodStart, thisPeriod$PeriodStart, units="mins" )
        sinceBegin <- difftime( stagesubset[,1], thisPeriod$PeriodStart, units="mins" )
        ratio <- as.numeric(sinceBegin/as.numeric(minutesBetweenRate))

        phasedareas <- phasedareas*(1-ratio)+phasedareas2*ratio

      }
      areasList[[periodNum]] <- data.frame(Time = stagesubset[,1], Area = phasedareas)

    }else{
      # skip empty data
    }

  }

  areasList <- do.call(rbind, areasList)

  if(areasList[1,1] > stagedf[1,1]) message("warning: areas trimmed to match XS dates")
  f.area <- approxfun(areasList)
  return(f.area(stagedf[,1]))

}
