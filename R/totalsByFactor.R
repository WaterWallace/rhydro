if(1 == 0)
{

library(dplyr)
library(ccInterp)
library(hydroTSM)
library(data.table)
spliceTimeFlow <- function(spliced) #input as data.frame( Time, Data, QC)
{
  tsMean <- function(df, Sum=0)
  {
    #df <- subsetdf

    TimeDiffs <- diff(as.numeric(df$Time))
    ValueDiffs <- diff(df$Data)
    ValueAvgs <- df$Data[-length(df$Data)] + ValueDiffs/2

    A <- sum(ValueAvgs*TimeDiffs) #summed valuediffs in seconds
    B <- sum(TimeDiffs) #summed timediffs in seconds

    ValueAverage <- A/B

    if (Sum==1)
    {
      return(A)
    }
    if (Sum==2)
    {
      return(B)
    }
    return(ValueAverage)

  }
  #spliced$Time

  StartTime <- spliced$Time[1]
  lineQC <- spliced$QC[1]
  EndTime <- 0
  lastTime <- 0

  QCLine <- data.frame(Start = NULL, End=NULL, QC= NULL)
  QCLines <- QCLine

  for (spliceRow in 1:(nrow(spliced)-1) )
  {

    nextRow <- spliceRow + 1
    #print("row num")
    #print(spliceRow)

    if (spliced$QC[nextRow] > lineQC)
    {
      lastTime <- EndTime
      EndTime <- spliced$Time[spliceRow]
      lineQC <- spliced$QC[spliceRow]

      #print ("next > lineqc")
      #print(StartTime)
      #print(EndTime)

    }
    if (spliced$QC[nextRow] < lineQC)
    {
      lastTime <- EndTime
      EndTime <- spliced$Time[nextRow]
      lineQC <- spliced$QC[spliceRow]


      #print ("next < lineqc")
      #print(StartTime)
      #print(EndTime)
    }


    if (EndTime != lastTime)
    {
      #write line
      #print(
      #  paste("start: ",StartTime," End: ",EndTime, " LineQC: ", lineQC )
      #)

      QCLine <- data.frame(Start = StartTime, End=EndTime, QC= lineQC)
      QCLines <- rbind(QCLines, QCLine)

      lineQC <- spliced$QC[nextRow]
      lastTime <- EndTime

      #update start time
      StartTime <- EndTime

    }


    if (nextRow == nrow(spliced)) #final row
    {
      EndTime <- spliced$Time[nextRow]

      #write line
      #print("last line")
      #print(
      #  paste("start: ",StartTime," End: ",EndTime, " LineQC: ", lineQC )
      #)

      QCLine <- data.frame(Start = StartTime, End=EndTime, QC= lineQC)
      QCLines <- rbind(QCLines, QCLine)

      break
    }


    #readline(prompt="Press [enter] to continue")

  }

  FlowRows <- NULL
  TimeRows <- NULL
  for(subsetRow in 1:(nrow(QCLines)))
  {
    subsetdf <- subset(spliced, Time >= QCLines$Start[subsetRow] & Time <= QCLines$End[subsetRow] )
    subsetqual <- QCLines$QC[subsetRow]

    #timeDiff <- as.numeric(QCLines$End[subsetRow]) - as.numeric(QCLines$Start[subsetRow])
    #tsMean(subsetdf)
    timeDiff <- tsMean(subsetdf, Sum=2)
    Megalitres <- tsMean(subsetdf, Sum=1)/1000

    avg <- Megalitres/timeDiff

    FlowRows <- rbind(FlowRows, Megalitres)
    TimeRows <- rbind(TimeRows, timeDiff)
  }

  QCLines <- cbind(QCLines, Flow = FlowRows)
  QCLines <- cbind(QCLines, Duration = TimeRows)

  aggrFlow <- aggregate(Flow ~ QC, data=QCLines, sum, na.action = na.pass )
  sum(aggrFlow$Flow, na.rm=TRUE)

  aggrFlow$Flow/sum(aggrFlow$Flow)*100

  QCLines$Duration
  aggrDur <- aggregate(Duration ~ QC, data=QCLines, sum, na.action = na.pass)
  sum(aggrDur$Duration, na.rm=TRUE)

  aggrDur$Duration/sum(aggrDur$Duration)*100
  FlowPercent <- aggrFlow$Flow/sum(aggrFlow$Flow, na.rm=TRUE)*100

  QCdf <- data.frame(aggrFlow$QC, aggrFlow$Flow, FlowPercent = FlowPercent, aggrDur$Duration, TimePercent = aggrDur$Duration/sum(aggrDur$Duration)*100 )

  return(QCdf)

}

fname <- "C:/Users/wallacesw/Documents/GitHub/DischargeCalcsRMD/1120053.Discharge.rds"
spliced <- readRDS("data/spliced.RDS")

q <- readRDS(fname) # included most recent discharge output

### Create lookup for fdc
hourly <- q$Predictors %>% dplyr::select(c(Timestamp, Event)) %>%
  changeInterval(Interval = "Hourly", option = "inst") %>%
  mutate(fdc = fdc(Inst, log = FALSE, plot = FALSE))

hourly %>% plot(Inst ~ fdc, data=., log = "y")
f.fdcMWV <- splinefun(hourly$Inst, hourly$fdc*100)

mwvpercentile <- q$Predictors %>% mutate(Percentile = f.fdcMWV(Event)) %>% na.omit

# prevent splinefun from overranging
# this will crash it if there's nothing there though
mwvpercentile[mwvpercentile$Event > hourly$Inst %>% max, ]$Percentile <- 0
mwvpercentile[mwvpercentile$Event < hourly$Inst %>% min, ]$Percentile <- 100

f.mwvpercentile <- approxfun(mwvpercentile$Timestamp, mwvpercentile$Percentile)

ranges <- q$`2015-08-18 12:00:40`$model1$Calcs %>% mutate(mwvpercentile = f.mwvpercentile(Timestamp)) %>% na.omit
#ranges$mwvpercentile

#range(ranges$mwvpercentile)

# add mwv percentile to discharge
ranges <- q$`2015-08-18 12:00:40`$model1$Calcs %>% mutate(mwvpercentile = f.mwvpercentile(Timestamp)) %>% na.omit %>%
  mutate(range = cut(mwvpercentile, c(seq(0,10,1),seq(15,100,5)) ))
unique(ranges$range)


library(dplyr)
library(data.table)
# function input
addFactorBoundaries <- function(df, bias = 1)
{
  df <- df %>% mutate(factorPlusOne = tail(factor, -1) %>% c(0) ) %>%
    mutate(factorMinsOne = c(0, head(factor, -1) ))

  if (bias == 1)
  {
  # duplicates
  dupes <- df %>% dplyr::filter(factorPlusOne > factor & factorMinsOne > factor) %>%
    mutate(factor = ( pmax(factorPlusOne, factorMinsOne)))

  plusone <- df %>% dplyr::filter(factorPlusOne > factor) %>%
    mutate(factor = factorPlusOne)

  minusone <- df %>% dplyr::filter(factorMinsOne > factor) %>%
    mutate(factor = factorMinsOne)

  }else if(bias == 0)
  {
  dupes <- df %>% dplyr::filter(factorPlusOne < factor & factorMinsOne < factor) %>%
      mutate(factor = ( pmin(factorPlusOne, factorMinsOne)))

  plusone <- df %>% dplyr::filter(factorPlusOne < factor) %>%
    mutate(factor = factorPlusOne)

  minusone <- df %>% dplyr::filter(factorMinsOne < factor) %>%
    mutate(factor = factorMinsOne)

  }

  # join data
  df <- rbind(df[!(df$ts %in% dupes$ts),],
              plusone <- plusone[!(plusone$ts %in% dupes$ts),],
              minusone <- minusone[!(minusone$ts %in% dupes$ts),],
              dupes)

  df <- df[order(df$ts),]

  # recalc plus/minus
  df <- df %>% mutate(factorPlusOne = tail(factor, -1) %>% c(0) ) %>%
    mutate(factorMinsOne = c(0, head(factor, -1) ))
  #View(df)

  #fix quality discontinuities
  dupetimes <- df$ts[duplicated(df$ts)]
  dupetimes <- df[df$ts %in% dupetimes,]


  for(dupe in df$ts[duplicated(df$ts)])
  {
    #print(dupe)
    thisdupe <- df[df$ts == dupe,]

    thisdupe <- thisdupe[thisdupe$factorPlusOne == thisdupe$factorMinsOne | thisdupe$factorPlusOne == 0 | thisdupe$factorMinsOne == 0,]
    if(nrow(thisdupe) == 2)
    {
      if(thisdupe$factor[1] == thisdupe$factorPlusOne[2])
      {
        # reverse order
        df$factor[df$ts == dupe] <- rev(df$factor[df$ts == dupe])
      }

    }

  }

  # run this twice in case another single pops up
  for(i in 1:2)
  {

    # re-add next/previous factor
    df <- df %>% mutate(factorPlusOne = tail(factor, -1) %>% c(0) ) %>%
      mutate(factorMinsOne = c(0, head(factor, -1) ))

    # split
    splitdf <- df %>% mutate( run = rleid(df$factor)) %>% group_split(run)
    for(spliti in 1:length(splitdf))
    {
      if(nrow(splitdf[[spliti]]) == 1)
      {
        if(bias == 1)
        {
          splitdf[[spliti]]$factor <- max(splitdf[[spliti]]$factorPlusOne, splitdf[[spliti]]$factorMinsOne)
        }else if (bias == 0){
          splitdf[[spliti]]$factor <- max(splitdf[[spliti]]$factorPlusOne, splitdf[[spliti]]$factorMinsOne)
        }
      }
    }
    # join list
    df <- do.call(rbind, splitdf)
    # remove duplicates
    df <- df %>% dplyr::select(ts, numeric, factor) %>% distinct(ts, factor, .keep_all = TRUE)
  }
  return(df)


}

df <- spliced

totalsByFactor <- function(df, bias = 1)
{

  stopifnot ( "dataframe must be 3 comlumn (timestamp, numeric and factor)" = length(df) == 3 )

  colnames(df) <- c("ts", "numeric", "factor")

  # bias = 1, bias toward the higher number on factor boundary
  # bias = 0, bias toward the lower number on factor boundary
  stopifnot ( "Can not have NA's in factor" = df$factor[ is.na(df$factor) ] %>% length == 0 )

  df <- df %>% mutate(factor = as.factor(factor))
  numlevels <- df$factor %>% unique %>% length
  previousLevels <- levels(df$factor)

  # convert to numeric
  df$factor <-  df$factor %>% as.numeric

  # add boundaries to original dataframe
  dfBounds <- addFactorBoundaries(df, bias)
  # split by runs
  dfBounds <- dfBounds %>% mutate(run = rleid(factor)) %>% group_split(run)

  matrix(c(1,2,3,4), ncol = 2 )
  ?matrix

  # empty matrix
  boundSums <- matrix( rep(0, 2*length(unique(df$factor))), ncol = 2 )
  rownames(boundSums) <- previousLevels

  #dfBounds[[1]]


  # accumulate flow in ML for each factor
  for(bound in dfBounds)
  {
    boundML <- pracma::trapz(bound$ts %>% as.numeric, bound$numeric) / 100+0
    boundTime <- range(bound$ts)  %>% diff() %>% as.numeric(units = "hours")

    boundSums[[bound$factor[1],1]] <- boundSums[[bound$factor[1],1]] + boundML
    boundSums[[bound$factor[1],2]] <- boundSums[[bound$factor[1],2]] + boundTime
  }

  #aggrDur <- aggregate(Duration ~ QC, data=QCLines, sum, na.action = na.pass)
  #sum(aggrDur$Duration, na.rm=TRUE)

  boundSums <- cbind (boundSums, FlowPcnt = ( boundSums[,1] / sum(boundSums[,1]) * 100 ))
  boundSums <- cbind (boundSums, TimePcnt = ( boundSums[,2] / sum(boundSums[,2]) * 100 ))

  #boundSums <- cbind (boundSums, total = ( boundSums / sum(boundSums) * 100 ))

  colnames(boundSums) <- c("Volume(ML)","Time(hours)","PercentVolume","Time(%)")

  return(boundSums %>% round(1) )
}

#View(df)

df$factor
df <- data.frame(ts = seq(Sys.time()-60*60*24, Sys.time(), length.out = 100  ),
             numeric = cumsum(rnorm(500, 0, 10)),
             factor = sample(c(10,20,30), 500, replace = TRUE))
totalsByFactor(df, bias = 1)

totalsByFactor(spliced, bias = 1)



#ranges <- q$`2015-08-18 12:00:40`$model1$Calcs %>% mutate(mwvpercentile = f.mwvpercentile(Timestamp)) %>% na.omit %>%
#  mutate(range = cut(mwvpercentile, c(0, 0.66, 2.5, 100) ))
#unique(ranges$range)

df <- tibble(ts = ranges$Timestamp, numeric = ranges$Q, factor = ranges$range)
#df[is.na(df$factor),]

totals <- totalsByFactor(df)
totals

sum(totals[,2])


sum( totals[1:4,2] )
sum( totals[1:4,2] )

totals <- totals %>% as.data.frame
totals <- cbind(totals, seq(1, nrow(totals)))

totals[1:12,2] %>% sum

totals[5:16,2] %>% sum
totals[17:20,2] %>% sum


splitted <- strsplit( row.names(totals), split = "," ) %>% do.call(rbind, .)
totals <- mutate( totals, Percentile = as.numeric(gsub(".*?([0-9]+).*", "\\1", splitted[,2]) ))
totals <- mutate( totals, Accum = cumsum(PercentVolume ))
plot(data = totals, Accum ~ Percentile, log = "xy")
f.accum <- splinefun(totals$Percentile, totals$Accum)
lines(f.accum(Percentile) ~ Percentile, data = totals)

# re add levels
#df$factor <- as.factor(df$factor)
#levels(df$factor) <- previousLevels


spliceTimeFlow <- function(spliced) #input as data.frame( Time, Data, QC)
{
  tsMean <- function(df, Sum=0)
  {
    #df <- subsetdf

    TimeDiffs <- diff(as.numeric(df$Time))
    ValueDiffs <- diff(df$Data)
    ValueAvgs <- df$Data[-length(df$Data)] + ValueDiffs/2

    A <- sum(ValueAvgs*TimeDiffs) #summed valuediffs in seconds
    B <- sum(TimeDiffs) #summed timediffs in seconds

    ValueAverage <- A/B

    if (Sum==1)
    {
      return(A)
    }
    if (Sum==2)
    {
      return(B)
    }
    return(ValueAverage)

  }
  #spliced$Time

  StartTime <- spliced$Time[1]
  lineQC <- spliced$QC[1]
  EndTime <- 0
  lastTime <- 0

  QCLine <- data.frame(Start = NULL, End=NULL, QC= NULL)
  QCLines <- QCLine

  for (spliceRow in 1:(nrow(spliced)-1) )
  {

    nextRow <- spliceRow + 1
    #print("row num")
    #print(spliceRow)

    if (spliced$QC[nextRow] > lineQC)
    {
      lastTime <- EndTime
      EndTime <- spliced$Time[spliceRow]
      lineQC <- spliced$QC[spliceRow]

      #print ("next > lineqc")
      #print(StartTime)
      #print(EndTime)

    }
    if (spliced$QC[nextRow] < lineQC)
    {
      lastTime <- EndTime
      EndTime <- spliced$Time[nextRow]
      lineQC <- spliced$QC[spliceRow]


      #print ("next < lineqc")
      #print(StartTime)
      #print(EndTime)
    }


    if (EndTime != lastTime)
    {
      #write line
      #print(
      #  paste("start: ",StartTime," End: ",EndTime, " LineQC: ", lineQC )
      #)

      QCLine <- data.frame(Start = StartTime, End=EndTime, QC= lineQC)
      QCLines <- rbind(QCLines, QCLine)

      lineQC <- spliced$QC[nextRow]
      lastTime <- EndTime

      #update start time
      StartTime <- EndTime

    }


    if (nextRow == nrow(spliced)) #final row
    {
      EndTime <- spliced$Time[nextRow]

      #write line
      #print("last line")
      #print(
      #  paste("start: ",StartTime," End: ",EndTime, " LineQC: ", lineQC )
      #)

      QCLine <- data.frame(Start = StartTime, End=EndTime, QC= lineQC)
      QCLines <- rbind(QCLines, QCLine)

      break
    }


    #readline(prompt="Press [enter] to continue")

  }

  FlowRows <- NULL
  TimeRows <- NULL
  for(subsetRow in 1:(nrow(QCLines)))
  {
    subsetdf <- subset(spliced, Time >= QCLines$Start[subsetRow] & Time <= QCLines$End[subsetRow] )
    subsetqual <- QCLines$QC[subsetRow]

    #timeDiff <- as.numeric(QCLines$End[subsetRow]) - as.numeric(QCLines$Start[subsetRow])
    #tsMean(subsetdf)
    timeDiff <- tsMean(subsetdf, Sum=2)
    Megalitres <- tsMean(subsetdf, Sum=1)/1000

    avg <- Megalitres/timeDiff

    FlowRows <- rbind(FlowRows, Megalitres)
    TimeRows <- rbind(TimeRows, timeDiff)
  }

  QCLines <- cbind(QCLines, Flow = FlowRows)
  QCLines <- cbind(QCLines, Duration = TimeRows)

  aggrFlow <- aggregate(Flow ~ QC, data=QCLines, sum, na.action = na.pass )
  sum(aggrFlow$Flow, na.rm=TRUE)

  aggrFlow$Flow/sum(aggrFlow$Flow)*100

  QCLines$Duration
  aggrDur <- aggregate(Duration ~ QC, data=QCLines, sum, na.action = na.pass)
  sum(aggrDur$Duration, na.rm=TRUE)

  aggrDur$Duration/sum(aggrDur$Duration)*100
  FlowPercent <- aggrFlow$Flow/sum(aggrFlow$Flow, na.rm=TRUE)*100

  QCdf <- data.frame(aggrFlow$QC, aggrFlow$Flow, FlowPercent = FlowPercent, aggrDur$Duration, TimePercent = aggrDur$Duration/sum(aggrDur$Duration)*100 )

  return(QCdf)

}
df <- data.frame(Time = ranges$Timestamp, Data = ranges$Q, QC = ranges$range %>% as.numeric)
totalsOld <- spliceTimeFlow(df)


}
