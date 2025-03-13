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
#'     \item{*QC*}{ Integer, Quality code }
#'     }
#' @param offsets *(optional)* dataframe, with columns "PeriodStart", "TableNum", "logOffset"
#' \itemize{
#'     \item{*TableNum*}{ String, the section ID of the cross section}
#'     \item{*logOffset*}{ Numeric, logOffset of this rating table, If zero, offset will be calculated }
#'     }
#' @param invert Boolean, if TRUE will invert the rating table so it calculates the input from the output
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
PhasedLoginterp <- function(RatePer, points, stagedf, offsets = NULL, invert = FALSE )
{
  ## Didn't have time to work out the log conversions so just created a lookup table every millimetere.
  ## Works but is inefficient for invert = TRUE

  if(min(RatePer$PeriodStart) > min(stagedf[,1]) )  message("Warning: Height data timestamp before rating periods start")

  QCPresent <- "QC" %in% (points %>% do.call(bind_rows, .) %>% names )

  if(QCPresent & !"QC" %in% (stagedf %>% names) ) # if there are rating qc's
    # but no input data QC's, add some dummy values that will get overwritten by the rating
  {
    stagedf <- mutate(stagedf, QC = 1)
  }

  stopifnot("RatePer must be: PeriodStart TableNum Phased" =
              RatePer %>% names %in% c("PeriodStart","TableNum","Phased")
  )


  stopifnot("points must be: from to ratingID (QC)" =
              c("from","to","ratingID") %in% (points %>% do.call(rbind,.) %>% names)
  )


  if(invert)
  {
    for(ratingnum in 1:length(points))
    {
      expandedhts <- seq(
        range(points[[ratingnum]]$from)[1],
        range(points[[ratingnum]]$from)[2],
        by = 0.001
      )
      expandedtable <- logInterpolate(points[[ratingnum]] %>%
                                        as.data.frame %>%
                                        dplyr::select(from, to, QC), expandedhts)

      # overwrite with expanded table every mm
      points[[ratingnum]] <- data.frame(from = expandedhts,
                                        to = expandedtable$value,
                                        QC = expandedtable$qc,
                                        ratingID = points[[ratingnum]]$ratingID[1])
    }
  }

  #unlist
  if(is.list(points)){
    points <- do.call(bind_rows, points)
  }

  if(invert){
    names(points) <- names(points) %>%
      gsub("to", "temp", .) %>%
      gsub("from", "to", .) %>%
      gsub("temp", "from", .)
  }
  # resplit and name each item
  points <- split(points, points$ratingID)

  # Add period and and what to phase into
  periods <- RatePer %>% mutate(End = lead(PeriodStart)) %>%
    mutate(nextTable = lead(TableNum)) %>%
    mutate(nextTable = ifelse(Phased, nextTable, TableNum)) %>%
    mutate(MinutesBetween = difftime(End, PeriodStart, units = "min") %>% as.numeric)

  # make the period end equal to the end of time series
  periods[nrow(periods),]$End <- max(max(stagedf[,1]), periods[nrow(periods),]$PeriodStart)
  periods[nrow(periods),]$nextTable <- periods[nrow(periods),]$TableNum


  stageList <- list()
  # loop through periods until the second last one
  for(period in seq_len(nrow(periods)))
  {
    start <- periods %>% slice(period) %>% pull(PeriodStart)
    end <- periods %>% slice(period) %>% pull(End)

    # subset
    if(period == max(nrow(periods)))
    {
      stagesubset <- stagedf %>%
        filter(if_any(1, ~ .x >= start))
    }else{
      stagesubset <- stagedf %>%
        filter(if_any(1, ~ .x >= start & .x < end))
    }

    if(nrow(stagesubset) == 0) next    # no data in subset

    #Table names
    thistable <- periods %>% slice(period) %>% pull(TableNum)
    nexttable <- periods %>% slice(period) %>% pull(nextTable)

    if ( !is.null(offsets) )
    {
      split_list <- offsets %>%
        group_by(TableNum) %>%
        group_split()
      names(split_list) <- unique(offsets$TableNum)

      thisLog <- split_list[[thistable]] %>% pull(logOffset)
      nextLog <- split_list[[nexttable]] %>% pull(logOffset)
    }else
    {

      thisLog <- 0
      nextLog <- 0
    }

    if( ( periods %>% slice(period) %>% pull(Phased) ) & (period < nrow(periods)) ) # phased
    {
      minsbetween <- periods %>% slice(period) %>% pull(MinutesBetween)

      points[[thistable]]
      points[[nexttable]]

      stageList[[period]] <-  stagesubset %>%
        mutate(sinceBegin = difftime( .[[1]], start, units="mins" )) %>%
        mutate(ratio = as.numeric(sinceBegin/minsbetween)) %>%
        mutate(rating1 = logInterpolate(points[[thistable]] %>%  as.data.frame %>% dplyr::select(contains(c("from", "to", "QC"))), stagesubset[[2]], )) %>%
        mutate(rating2 = logInterpolate(points[[nexttable]] %>%  as.data.frame %>% dplyr::select(contains(c("from", "to", "QC"))), stagesubset[[2]], )) %>%
        mutate(Value = rating1$value * (1 - ratio) + rating2$value * ratio)

      if(QCPresent){
        stageList[[period]] <- stageList[[period]] %>%
          mutate(QC = pmax(QC, rating1$qc, rating2$qc))
      }

    }else{ # not phased
      stageList[[period]] <- stagesubset %>%
        mutate(rating1 = logInterpolate(points[[thistable]] %>%
                                          as.data.frame %>%
                                          dplyr::select(contains(c("from", "to", "QC"))),
                                        stagesubset[[2]], )) %>%
        mutate(Value  = rating1$value)

      if(QCPresent){
        stageList[[period]] <- stageList[[period]] %>%
          mutate(QC = pmax(QC, rating1$qc))
      }

    }
    stageList[[period]] <- stageList[[period]] %>%
      dplyr::select(contains(c("Time", "Value", "QC")))
  }

  return(do.call(bind_rows, stageList))




}


