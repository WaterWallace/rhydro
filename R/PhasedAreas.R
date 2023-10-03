#' Calculate cross sectional area for a a range of periods
#'
#' Applies a rating table to height data for a time period
#'     If the phased box is set to true, the result is based on a time factor between the
#'     current rating table and the next rating table.
#'
#'     This is redundant since PhasedLoginterp does the same thing.
#'     However need an extra function to convert *xsdf* to *points*
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
#' @param stagedf dataframe, with columns for a timestamp (posixct)
#'      and a height (Numeric)
#'
#' @return dataframe of areas, and optional quality code,
#'     from a lookup table at each Stage timestamp,
#'     therefore will always merge with Stage.
#'
#' @examples
#'
#'
#' #'
#' #mrdxs <- getXSDB(client, "1110056", bucket = "tsdata3" )
#' #mrdht <- getTSDB(client, "1110056", bucket= "tsdata3" )
#' data(mrd)
#'
#' mrdht <- dplyr::select(mrdht[[1]], c(time, value_Level))
#' areas <- PhasedAreas(mrdxs$Periods, mrdxs$XS, mrdht)
#' plot(mrdht$time, areas$Value)
#' plot(mrdht$value_Level, areas$Value)
#'
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
#'         by = 60*60*24), #' daily timestamps
#'     height = 7
#'   )
#'
#' # Apply phased rating tables to stage
#' Stage <- cbind(Stage, PhasedAreas(Periods, XSDF, Stage))
#' #' Heights covering entire rating periods
#' plot(Stage$timestamp, Stage$Value)
#'
#'
#' # Subsection Stage
#' Stage <- data.frame(
#'     timestamp = seq(as.POSIXct("2020-06-01 00:00"), as.POSIXct("2024-01-01 00:00"),
#'         by = 60*60*24), #' daily timestamps
#'     height = 7
#'   )
#'
#' # Apply phased rating tables to stage
#' Stage <- cbind(Stage, PhasedAreas(Periods, XSDF, Stage))
#'
#' # Plots starting after the first rating period has started
#' plot(Stage$timestamp, Stage$Value)
#'
#' # Subsection Stage
#' Stage <- data.frame(
#'     timestamp = seq(as.POSIXct("2019-01-01 00:00"), as.POSIXct("2024-01-01 00:00"),
#'         by = 60*60*24), #' daily timestamps
#'     height = 7
#'   )
#'
#' # Apply phased rating tables to stage
#' Stage <- cbind(Stage, PhasedAreas(Periods, XSDF, Stage))
#' # Heights starting before rating periods - No values before period starts.
#' plot(Stage$timestamp, Stage$Value)
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
#' Stage <- cbind(Stage, PhasedAreas(Periods, XSDF, Stage))
#' # Heights covering entire rating periods
#' plot(Stage$timestamp, Stage$Value)
#'
#'
#'
#'
#' @export
#'
#'
#areas <- PhasedAreas(mrdxs$Periods, mrdxs$XS, mrdht[[1]])
#RatePer <- mrdxs$Periods
#xsdf <- mrdxs$XS
#stagedf <- mrdht[[1]]

PhasedAreas <- function(RatePer, xsdf, stagedf )
{

  #split by ID
  mrdxssplit <- split(xsdf, f=xsdf$n)

  #Convert to lookup tables
  mrdlookups <- lapply(mrdxssplit,
                       function(xs){
                         xs <- cbind(areaLookup ( dplyr::select(xs, c("CHAIN","RL"))),
                                     ratingID = xs$n[1])
                         names(xs) <- c("from","to","ratingID")
                         return(xs)
                       }
  )
  # calculate areas
  areas <- PhasedLoginterp( RatePer = RatePer,
                            points = mrdlookups,
                            stagedf = data.frame(timestamp = stagedf[,1],
                                                 height = stagedf[,2]
                            )
  )
  return(areas)

}



