# Github version2 started 04/01/2023

getwd()


# uselocalcopy <- TRUE
#library(devtools)
#install_github("WaterWallace/ccInterp")

library(ccInterp)
library(ggplot2)
library(viridisLite)
library(viridis)
library("influxdbclient")
library(Rlibeemd)
library(ggplot2)
library(reshape2)
library(signal)
library(zoo)
library(MASS)
library(oce) #for despike
library(dplyr)
library(EMD)
library(pracma)
library(data.table) # for rleid
library(repmod)
# test3

# COMMAND --------

# declare functions

mergeTS <- function(oldblock, newblock, method="Append")
{
  #oldblock <- mergedBlock
  #newblock <- newBlock
  if (nrow(newblock > 0)) 
  {
    colnames(newblock) <- colnames(oldblock)
    #oldblock <- mergedBlock
    #newblock <- newBlock
    #method <- "Insert"
    
    # append new data from new bloack
    if (method == "Append")
    {
      newblock <- newblock %>% dplyr:::filter(newblock[,1] > last(oldblock[,1]) )
      mergedblock <- rbind (oldblock, newblock)
      
    }
    # replace old data with new block
    if (method == "Replace")
    {
      oldblock <- oldblock %>% dplyr:::filter(oldblock[,1] < first(newblock[,1]) )
      mergedblock <- rbind (oldblock, newblock)
    }
    # insert new block in the middle of a block
    if (method == "Insert")
    {
      
      firstblock <- oldblock %>% dplyr:::filter(oldblock[,1] < first(newblock[,1]) )
      endblock <- oldblock %>% dplyr:::filter(oldblock[,1] > last(newblock[,1]) )
      #mergedblock <- rbind (firstblock[,1:2], newblock[,1:2], endblock[,1:2])
      mergedblock <- rbind (firstblock, newblock, endblock)
      
    }
    return(mergedblock)
  }
  return (oldblock)
  
  
}
GapList <- function(tsdata, minGap = 60 )
{
  #tsdata <- data.frame(ProcessedQ$time, ProcessedQ$value_Discharge)
  # returns a dataframe listing each gap start, end and duration.
  # input is a posixct time
  
  # add a column of gaps
  tsdata <- cbind(tsdata, gap = difftime(c(tsdata[-1,1],  tsdata[nrow(tsdata),1]), c(tsdata[,1]), units="mins"  ))
  # create a list of gaps
  gap <- tsdata %>% dplyr:::filter(gap > minGap)
  # add end date to gap list
  return( data.frame(start = gap[,1], end = gap[,1]+gap$gap, gap = gap$gap ) )
  
}
blockList <- function(gaps, ts)
{  

  
  ts <- na.omit(ts)
  ts <- ts[,1]
  
  gaps$start <- as.numeric(gaps$start)/60
  gaps$end <- as.numeric(gaps$end)/60
  ts <- as.numeric(ts)/60
  #as.POSIXct( as.numeric(na.omit(ts))*60, origin="1970-01-01" )
  
  # convert gaps, to blocks
  blocks <- data.frame(Start = c(ts[1], gaps$end), End = c(gaps$start, ts[length(ts)]) ) # blocks in minutes
  blocks$dur <- (blocks$End - blocks$Start) # calc duration of data block
  blocks <- blocks %>% dplyr:::filter(dur > 360) # minimum duration for block, otherwise gap
  
  # convert from minutes to posix
  blocks$Start <- as.POSIXct(blocks$Start*60, origin="1970-01-01")
  blocks$End <- as.POSIXct(blocks$End*60, origin="1970-01-01")
  
  return (blocks)
}
f.qualmax <- function(x, t, qc) #interpolate at x, for a timeseries, t/qc
{
  f.qual0 <-  approxfun(t, qc, method = "constant" , f=0)
  f.qual1 <- approxfun(t, qc, method = "constant" , f=1)
  
  pmax(f.qual0(x),  f.qual1(x))
}
ceemdanFilter <- function(time, data, output=1, method="ceemdan", d1 = 16, d2 = 25)
{

  GetPeriods <- function(imf){
    
    #GetPeriods(imfs, avg)
    #imf <- imfs
    #avg <- 1
    power_spec = function(y,samp.freq){
      
      N <- length(y)
      fk <- fft(y)
      fk <- fk[2:length(fk)/2+1]
      fk <- 2*fk[seq(1, length(fk), by = 2)]/N
      freq <- (1:(length(fk)))* samp.freq/(2*length(fk))
      data.frame(amplitude = Mod(fk), freq = freq)
    }
    
    periods <- matrix(nrow = ncol(imf), ncol = 1 )
    ampl <- matrix(nrow = ncol(imf), ncol = 1 )
    if (length(imf) == 1)
    {
      periods[1] <- 1
      return(periods)
    }
    
    for (imfNum in 1:ncol(imf))
    {
      imfSpec <- spectrum(imf[,imfNum], plot=FALSE)
      lowpass <- butter(2, min(100/length(imfSpec$spec), 1) , type="low") #low pass filter
      filteredSpec <- filtfilt(lowpass, imfSpec$spec) #24 hour low pass filter on hourly data
      f <- imfSpec$freq[which.max(filteredSpec)] #frequency
      periods[imfNum] <- 1/f
      #ampl[imfNum] <- max(filteredSpec)
      
      signal <- imf[,imfNum]#raw signal
      x = power_spec(signal,samp.freq = f)
      ampl[imfNum] <- max(x$amplitude)
    }
    
    Periods <- data.frame(Period = periods, Amplitude = ampl)
    return(Periods)
  }
  
  addAllCols <- function(imf, periods, a=16, b=25){
    
    #periods <- Periods
    TideType <- matrix(nrow = 3, ncol = 1)
    summedcols <- matrix(nrow = nrow(imf), ncol=3)
    summedcols[,1] <- 0
    summedcols[,2] <- 0
    summedcols[,3] <- 0
    
    for (period in 1:NROW(periods))
    {
      if (periods[period] < a)
      {
        TideType[1] <- period
        summedcols[,1] <- summedcols[,1]+imf[,period]
      }
      if (periods[period] >= a && periods[period] < b) 
      {
        TideType[2] <- period 
        summedcols[,2] <- summedcols[,2]+imf[,period]
      }
      if (periods[period] >= a && periods[period] >= b)
      { 
        TideType[3] <- period
        summedcols[,3] <- summedcols[,3]+imf[,period]
      }
    }
    return (summedcols)
    
  }
  
  if(method=="ceemdan")
  {
    #imfs[,1]
    imfs <- ceemdan(data) # input an hourly vector
  }else if(method=="emd")
  {
    imfs2 <- EMD:::emd(data, as.numeric(time) )
    imfdf <- as.data.frame(imfs2$imf)
    if (nrow(imfdf) > 0)
    {
      imfdf <- cbind(imfdf,  res = imfs2$residue)
      imfs <- ts(imfdf)
    }else{
      return (NULL)
    }
  }else{
    message("method must be chosen: ceemdan or emd")
    return(0)
  }
  
  Periods <- GetPeriods(imfs)
  
  TideTypes <- addAllCols(imfs, as.vector(Periods$Period), d1, d2) #the sum of all imfs of each period of TideType
  timestamp <- as.data.frame(time)
  TypesOut <- cbind(timestamp, round(TideTypes, digits=3))
  
  if (output==1)
  {
    return(TypesOut)
  }else if(output==2)
  {
    return(Periods)
  }else if(output==3)
  {
    return(imfs)
  }
}
processEMD <- function(tsdata, tsblocks=0, method="ceemdan" )
{

  emdcount <- 0
  emdlist <- list()
 
  # input hourly dataframe, with posixct for time and values
  #tsdata, data.frame(posixtime, value)
  #tsblocks, data.frame(Start = posixctime, End=posixctime)
  #method, either "ceemdan" or "emd"
  
  # set default column names for input data
  #colnames(tsdata) <- c("Date","Data")
  colnames(tsdata)[1] <- "Date"
  colnames(tsdata)[2] <- "Data"
  
  tsblocks <- tsblocks %>% dplyr::filter(End > tsdata$Date[1] & Start < tsdata$Date[nrow(tsdata)])
  
  if (nrow(tsblocks)==0)
  {
    # default is period of record
    tsblocks <- data.frame(Start=tsdata[1,1], End = tsdata[nrow(tsdata), 1] )
  }
  
  #i <- 1
  # subsection and EMD for each block
  for(i in 1:nrow(tsblocks) )
  {
    # subsection
    message(paste("Filtering:", tsblocks$Start[i], "to:", tsblocks$End[i]))
    #
    subsection <- tsdata %>% dplyr::filter(Date >= tsblocks$Start[i], Date <= tsblocks$End[i]) # minimum duration for block, otherwise gap
    # emdfilter
    if (nrow(subsection) > 0)
    {
      #ceemdanVel <- ceemdanFilter(subsection[,1], subsection[,2], method="ceemdan" )
      ceemdanVel <- ceemdanFilter(subsection[,1], subsection[,2], method=method )
      if (is.null(ceemdanVel))
      {
        ceemdanVel <- data.frame(time=subsection[,1], a=subsection[,2], b=0, c=0)
        names(ceemdanVel) <- c("time",1,2,3)
      }
      emdcount <- emdcount + 1
      emdlist[[emdcount]] <- ceemdanVel
    }
  }
  
  combinedEMD <- do.call(rbind,emdlist)
  f.a <- approxfun(combinedEMD$time, combinedEMD$'1')
  f.b <- approxfun(combinedEMD$time, combinedEMD$'2')
  f.c <- approxfun(combinedEMD$time, combinedEMD$'3')
  f.a(tsdata$Date)
  f.b(tsdata$Date)
  f.c(tsdata$Date)
  
  df <- data.frame(time=tsdata$Date, a=f.a(tsdata$Date), b=f.b(tsdata$Date), c=f.c(tsdata$Date))
  return(df)
  #return(combinedEMD)
}
splitBlocks <- function(blocklist, newdata, include = TRUE, exclude = FALSE)
{
  
  
  if (include == exclude)
  {
    message("Either include or exclude not both or neither")
    return(0)
  }
  
  names(blocklist) <- c("start","end","gap")
  names(newdata) <- c("date","value","qc")
  
  #blocklist$start
  newdata$date <- as.POSIXct(newdata$date)
  
  starts <- data.frame(time = blocklist$start, tag = 1)
  ends <- data.frame(time = blocklist$end, tag = 0)
  fullset <- rbind(starts, ends)
  
  fullset <- fullset[order(fullset$time),]
  timedupes <- fullset[duplicated(fullset$time), ]
  
  fullset <- fullset[!duplicated(fullset$time), ]
  
  f.tags <- approxfun(fullset, method = "constant" , rule=1)  
  
  
  # split inside blocks  
  newdata$tags <- f.tags(newdata$date)
  
  # lookup for boundary value
  f.boundaries <- approxfun(newdata$date, newdata$value)
  #f.boundaries(fullset$time)
  
  # lookup for boundary quality
  #f.boundqc <- approxfun(newdata$date, newdata$qc)
  
  
  # create a df of boundaries with a tag of 1 for include
  if (include == TRUE)
  {
    setTo <- 1
  }else if ( exclude == TRUE)
  {
    setTo <- 0
  }
  
  fqcbounds <- maxminfun(newdata$date, newdata$qc,fullset$time)
  bounds <-  data.frame(date=fullset$time, value =f.boundaries(fullset$time), qc=fqcbounds$QC, tags=setTo )
  
  if (nrow(newdata[newdata$date == bounds$date,] ) > 0 )
  {
    # set boundaries in new data to 1
    newdata[newdata$date == bounds$date,]$tags <- setTo
  }
  # merge data and boundaries
  newdata <- rbind(newdata, bounds)
  # sort
  newdata <- newdata[order(newdata$date),]
  # remove duplicates

  
  newdata <- newdata[!duplicated(newdata$date), ]
  newdata <- na.omit(newdata)
  
  splitblocks <- list()
  count <- 0
  
  for ( i in split(newdata,rleid( newdata$tags ) ) )
  {
    
    # if including the blocks
    if(include == TRUE & i$tags == 1)
    {
      count <- count+1
      #i <- i[i$qc < 150,]
      splitblocks[[count]] <- i
    }
    
    # if excluding the blocks
    if(exclude == TRUE & i$tags == 0)
    {
      count <- count+1
      splitblocks[[count]] <- i
    }
    
  }
  
  return(splitblocks)


}
joinBlocks <- function(splitblocks, olddata)
{
  
  splitblocks <- do.call(rbind.data.frame, splitblocks) # merge list items
  splitblocks <- splitblocks[,-(ncol(splitblocks))] # remove tag
  
  mergedQ <- rbind(olddata, splitblocks)
  
  dupes <- duplicated(mergedQ$date)
  mergedQ[dupes, ]$date
  
  dupedates <- mergedQ[dupes, ]$date
  
  mergedQ <- mergedQ[!duplicated(mergedQ$date), ]
  mergedQ <- mergedQ[order(mergedQ$date),]
  
  
  for(i in dupedates)
  {
    dupets <- (as.POSIXct(i, origin="1970-01-01") )
    
    a <- olddata[ olddata$date == dupets, ]$qc
    b <- splitblocks[ splitblocks$date == dupets,  ]$qc
    
    mergedQ[mergedQ$date == dupets, ]$qc <- max(a,b) 
    
  }
  
  return(mergedQ)
}
addTS <- function(seriesA, seriesB=0, multiA=1, offsetA=0, timeAdjA=0, multiB=1, offsetB=0, timeAdjB=0, combMethod = "Add")
{
  # seriesA = a dataframe of posixct time, and value (primary var)
  # seriesB = a dataframe of posixct time, and value (secondary var, i.e. this is subtracted from A for "Subtract")
  # seriesA = a dataframe of posixct time, and value
  # combMethod = "Add", "Subtract", "Multiply", "Divide"
  
  seriesA[,2] <- seriesA[,2] * multiA
  seriesA[,2] <- seriesA[,2] + offsetA
  seriesA[,1] <- seriesA[,1] - ( timeAdjA * 60 )
  
  if(length(seriesB == 1)){
    seriesB[,2] <- seriesB[,2] * multiB
    seriesB[,2] <- seriesB[,2] + offsetB
    seriesB[,1] <- seriesB[,1] - ( timeAdjB * 60 )
  }else{
    seriesB <- data.frame(seriesA[,1],0)
  }
  
  start <- max(min(seriesA[,1]), min(seriesB[,1]))
  end <- min(max(seriesA[,1]), max(seriesB[,1]))
  
  #
  alltimes <- c(
    seriesA[,1][seriesA[,1] >= start & seriesA[,1] <= end], 
    seriesB[,1][seriesB[,1] >= start & seriesB[,1] <= end]
  )
  alltimes <- alltimes[order(alltimes)]
  alltimes <- unique(alltimes)
  
  #
  f.A <- approxfun(seriesA[,1], seriesA[,2])
  f.B <- approxfun(seriesB[,1], seriesB[,2])
  
  bothseries <- data.frame(alltimes, f.A(alltimes), f.B(alltimes))
  head(bothseries)
  
  return(data.frame(Date = alltimes, TS = switch(combMethod,
                                                 "Add" = bothseries[,2] + bothseries[,3],
                                                 "Subtract" = bothseries[,2] - bothseries[,3],
                                                 "Multiply" = bothseries[,2] * bothseries[,3],
                                                 "Divide" = bothseries[,2] / bothseries[,3]
      )
    )
  )
  
  
}
createModel <- function(params, offsLoc=1)
{
  
  # creates a dummy model from input parameters
  # params = dataframe, one row, named headings for names of coefficients
  # offsLoc = location of offset, default = 1
  
  params <- cbind(params[offsLoc], params[-offsLoc]) # sort offset to first position
  dummyinput <- data.frame(replicate(length(params),sample(0:10,100,rep=TRUE)))
  names(params)[1] <- "(Intercept)"
  names(dummyinput) <- names(params)
  names(dummyinput)[1] <- "y"
  dummymod <- lm(y ~  . , data=dummyinput)
  
  for(param in names(params))
  {
    dummymod$coefficients[param] <- params[param][1,1]
  }
  #if (offsLoc == 0){
  #  dummymod$coefficients[1] <- 0
  #}
  
  return (dummymod)
}
processInfluxVI <- function(vimodel) # function to create a fake rlm to use in predict()
{
  #vimodel <- thisModel[threshold,]
  # process it
  vimodel <- dplyr::select(vimodel, -c('_time', Updated, ID, Threshold, '_measurement', name, time)) # strip metadata from influx VI
  vimodel <- vimodel[, colSums(vimodel != 0) > 0 | names(vimodel) == "Offs"] # remove any with a zero coefficient
  offsloc <- match("Offs", names(vimodel)) # find location of intercept, called "Offs" in influxdb
  if (is.na(offsloc)) offsloc <- 0 # no intercept, or intercept is zero
  # create an 'R" model
  return(createModel(vimodel,offsloc)) # convert the vimodel 
}

# area calculations
interpSeries <- function(series, location)
{
  # to interpolate values (xaxis) at a location (yaxis), in a series of data.
  series <- series[order(series$chain),]
  
  neighbs <- data.frame(rla = c(0,series[,2]),rlb=c(series[,2],0))
  neighbs <- cbind(neighbs, chaina = c(0,series[,1]),chainb=c(series[,1],0))
  
  neighbs <- neighbs[c(-1,-nrow(neighbs)),]
  interpFor <- neighbs[neighbs$rla > location & neighbs$rlb < location | neighbs$rlb > location & neighbs$rla < location  ,]
  
  if (nrow(interpFor) > 0)
  {
    interpPoints <- list()
    count <- 0
    for (i in 1:nrow(interpFor))
    {
      count <- count+1
      pointrl <- interpFor[i,1:2]
      pointchain <- interpFor[i,3:4]
      f.point <- approxfun(pointrl, pointchain)
      interpPoints[[count]] <- data.frame(chain = f.point(location), rl = location)
    }
    interpPoints <- do.call(rbind,interpPoints)
    names(interpPoints) <- names(series)
    interpPoints <- rbind(series, interpPoints)
    interpPoints <- interpPoints[order(interpPoints$chain),]
    return(interpPoints)
    
  }else{
    return(series)
  }
  
}
calcArea <- function(xscalc, ght)
{
  #xscalc <- xs
  # ght <- 40
  
  if (ght <= min(xscalc[,2])) return (0)
  # Interpolate cross section at level of "ght"
  xscalc <- interpSeries(xscalc, ght)
  # Set any values higher than ght to ght
  #NROW(xscalc[xscalc$rl > ght,]$rl)
  if ( NROW(xscalc[xscalc$rl > ght,]$rl) > 0 )   xscalc[xscalc$rl > ght,]$rl <- ght
  
  # calculate offset
  offset <- max(xscalc$rl)
  if (ght > offset)
  {
    extrapolate <- ght - offset
  }else
  {
    extrapolate <- 0 
  }
  
  # invert so it measures area under curve
  xscalc$rl <- xscalc$rl*-1
  #plot(xscalc)
  # offset to positive
  xscalc$rl <- xscalc$rl+offset+extrapolate
  
  #plot(xscalc)
  
  return (trapz(xscalc$chain, xscalc$rl))
  
}
areaLookup <- function(xs, interval = 0.1, window=c(0,0))
{
  # xs <- dplyr::select(thisTable, c("CHAIN","RL"))
  names(xs) <- c("chain", "rl")
  
  if(window[1] == 0 & window[2] == 0 )
  {
    window<- c(min(xs$rl), max(xs$rl)*1.5 )
  }
  
  xsLookup <- list()
  count <- 0
  for( i in seq(window[1],window[2],by=interval) )
  {
    count <- count+1
    xsLookup[[count]] <- data.frame(height = i, area = calcArea(xs, i))
  }
  return(do.call(rbind,xsLookup))
  
}
logInterpolate <- function (rating, num, logOffset = 0) 
{
  ctf <- min(rating[rating[2] > 0,][1])
  
  if (logOffset == 0) {
    logOffset <- max(rating[rating[2] <= 0,][1])
  }
  dfOffs <- rating[, 1] - logOffset
  num <- num - logOffset
  f.logarea <- approxfun(log(dfOffs), log(rating[, 2]))
  interpvalue <- exp(f.logarea(log(num)))
  
  interpvalue <- data.frame(ht = (num+logOffset), q = interpvalue)
  interpvalue$q[interpvalue$ht < ctf] <- 0
  
  return(interpvalue$q)
}
PhasedAreas <- function(RatePer, xsdf, stagedf )#rating periods, 
{
  
  #RatePer <- RatePeriods
  #xsdf
  #stagedf <- data.frame(DataOutput$Timestamp, DataOutput$Height)

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
    
    if(1==0)
    { # error in this one, fixed below
      # exclude start rows less than first data point
      selection <- dfModelTimes$PeriodStart > firstDataPoint & dfModelTimes$End < lastDataPoint
      # expand selection
      headSelect <- c(selection[-1], selection[length(selection)] )
      tailSelect <- c(selection[1], selection[-length(selection)] )
      selection <- (headSelect | tailSelect) # head or tail TRUE
    }else
    {
      # include rows that start after first data point
      headSelect <-  firstDataPoint < dfModelTimes$PeriodStart
      # include rows that start before last data point
      tailSelect <- lastDataPoint > dfModelTimes$PeriodStart 
      # expand selection
      headSelect <- c(headSelect[-1], headSelect[length(headSelect)] )
      tailSelect <- c(tailSelect[1], tailSelect[-length(tailSelect)] )
      selection <- (headSelect & tailSelect) # head or tail TRUE
    }
    
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


# COMMAND --------




# COMMAND --------

if(1==0){
  
  #redoneVI <- doForGS(GSNumber, startdate = 0,  updateMethod = "Replace", inputVI = oldvi[[selection]])
  

  GSNumber <- "122013A"

  #lrh = prglrh,
  #HighFlowThreshold <- -99,
  #forceHT = TRUE,
  #forceEMD = TRUE,
  #CALCVI = TRUE
  #abs=FALSE

  #doForGS(GSNumber, startdate = as.Date(Sys.Date()-30), updateMethod = "Append", token=token)
  #startdate =  Sys.Date() -365
  startdate =  0
  enddate = 0
  #GSNumber <- "1080025"
  CALCVI <- FALSE
  #lrh <- 7.4
  HighFlowThreshold <- -99
  ABS <- FALSE
  Poly <- 0
  skippedemdvalues <- 0
  forceEMD <- FALSE
  forceHT <- FALSE
  xvelFile <- ""
  #uselocalcopy = TRUE
  updateMethod <- "Append"
  ignoreQEst <- FALSE
  
  getwd()
  
  #("1120053", startdate = 0,  updateMethod = "Replace", inputVI = testVI, token=token)
  
  
  inputVI <- testVI
  GSNumber <- "1110056"
  startdate = "2020-05-01"
  enddate = "2020-06-01"
  updateMethod = "Replace"
  
  #recentFlow <- doForGS(GSNumber, startdate = Sys.Date()-60, updateMethod = "Append", token=token)
  updateMethod = "Append"
  CALCVI = TRUE
  GSNumber <- "1240062"
  startdate = 0
  enddate = 0
  #HighFlowThreshold = -99
  ABS = FALSE
  ignoreQEst = TRUE
  forceEMD = TRUE
  forceHT = TRUE 
  Poly=1
  emdmethod <- "ceemdan"
  skipYvel <- FALSE
  
  # RECENT FLOW
  startdate = Sys.Date()-90
  updateMethod <- "Append"
  
  #emdmethod <- "emd"
  
}


# COMMAND --------

#setwd("C:/Users/wallacesw/Documents/R")
#GSNumber <- "1120053"
#startdate <- 0
#enddate <- 0
updateMethod <- "LocalFile"

doForGS <- function(GSNumber, CALCVI = FALSE, lrh=0, HighFlowThreshold = -99, ABS=FALSE, addGaugings="", 
                    skippedemdvalues = 0, forceEMD = FALSE, forceHT = FALSE, xvelFile = "", Poly=0, 
                    startdate= Sys.Date()-60, enddate=0, 
                    ignoreQEst = FALSE, updateMethod = "Replace",
                    inputVI = NULL,
                    skipYvel = FALSE, token = key_get("READKEY", keyring="influx"), emdmethod="ceemdan"
                    )
{
  
  
  #fUNCTIONS
  {

    
    
    fillGaps <- function(mergedBlock, gaps, method="linear")
    {
      #ts <- fillGaps(tsdata, gaplist, qc= f.dataqc(tsdata$t))
      
      if (ncol(mergedBlock) == 2){
        mergedBlock$QC <- 10
      }
      
      if (nrow(gaps) == 0) return (mergedBlock) # no gaps to fill
      
      # input numeric time as minutes
      f.gapqual <- function(gaplength, good=60, fair = 120, poor = 360, bad = 720 )
      {
        if (gaplength <= good) return (10)
        if (gaplength <= fair) return (20)
        if (gaplength <= poor) return (30)
        if (gaplength <= bad) return (60)
        return (250)
      }
      
      if (method=="linear"){
        # for each row in "gaps"
        for (i in 1:(nrow(gaps)))
        {
          # row at which the gap begins
          rowatgapstart <- which(mergedBlock[,1] == gaps$start[i] )
          # Set this qc to the qc based on the gaplength 
          mergedBlock$QC[rowatgapstart] <- f.gapqual(gaps$gap[i])
        }
        # output qc'd block
        return(mergedBlock)
      }
      
      
      f.linear <- approxfun(mergedBlock[,1], mergedBlock[,2])
      if(method=="spline"){
        # can return unpredictable results
        f.spline <- splinefun(mergedBlock[,1], mergedBlock[,2] )
      }else if(method=="NA"){
        f.spline <- function(x){return (NA)}
        f.linear <- function(x){return (NA)}
      }else {
        message("method must be linear or spline or NA")
        return(0)
      }
      
      nr <- nrow(gaps)
      # for each gap in GapList over 60 minutes, but less than 360 fill with a spline interpolation
      for(i in 1:nr)
      {
        
        #print (i)
        gaplength <- gaps$gap[i]
        qc <- f.gapqual(gaplength)
        if (qc < 200){
          ts <- seq(gaps$start[i], gaps$end[i], length=6 )
          newBlock <- data.frame(Date = ts, Point = f.spline(ts))
        }
        if (qc == 200){
          # between 9 and 12 hours linear
          ts <- seq(gaps$start[i], gaps$end[i], length=3 )
          newBlock <- data.frame(Date = ts, Point = f.linear(ts))
          qc <- 60
        }
        if (qc >= 250)
        {
          # over 12 hours is a gap
          ts <- seq(gaps$start[i], gaps$end[i], length=3 )
          newBlock <- data.frame(Date = ts, Point = 0)
        }
        # add qc to new block
        newBlock <- cbind(newBlock, QC = qc)
        # merge new block
        mergedBlock <- mergeTS(mergedBlock, newBlock, method="Insert" )
      }
      
      return(mergedBlock)
      
    }
    
    despiked <- function(data)
    {
      
      if (length(data) > 1){
        data <- despike(
          data,
          reference = c("median", "smooth", "trim"),
          n = 4,
          k = 7,
          min = NA,
          max = NA,
          replace = c("reference", "NA"),
          skip
        )
      }
      
      return (data)
    }
    
    plotCEEMD <- function (TypesOut)
    {
      
      time <- as.POSIXct(TypesOut$time, format="%d/%m/%Y %H:%M" )
      TypesOut$time <- as.numeric(time)/60
      colnames(TypesOut)[1] <- "Timestamp"
      
      #melt data frame into long format
      df <- melt(TypesOut ,  id.vars = 'Timestamp', variable.name = 'series')
      
      #create line plot for each column in data frame
      ggplot(df, aes(Timestamp, value)) +
        geom_line(aes(colour = series))
      
    }
    
    VIfromGaugings <- function(gaugings, forceEMD = FALSE, forceHT = FALSE, includeintercept = FALSE, Poly=Poly)
    {
      # includeIntercept defaults to FALSE, but will include it if the gaugings go near zero
      # unless you force it to take an intercept
      if ( min(gaugings$Xvel, na.rm=T) < 0 &  min(gaugings$Event, na.rm=T) < 0.05) includeintercept <- TRUE
      heightIsAFactor <- forceHT
      useEMD <- forceEMD

      zeroIntercept <- function(model) return(update(model, . ~ . - 1))
      includeIntercept <- function(model) return(update(model, . ~ . + 1))
      removeBE <- function(model) return(update(model, . ~ . - TideBEvent))
      
      
      #models$simple <- update(models$simple, data = gaugings)
      #head(gaugings)
      
      modelLin <- lm(Yvel ~ Xvel, data = gaugings)
      modelPoly2 <- lm(Yvel ~ stats::poly(Xvel, degree=2, raw=TRUE), data = gaugings)
      modelPoly3 <- lm(Yvel ~ stats::poly(Xvel,degree=3, raw=TRUE), data = gaugings)
      #########
      
      #Plot Xvel vs Yvel
      par(mfrow=c(3,1))
      x <- data.frame(Xvel = seq(-1, 3, 0.01))
      
      
      plot(Yvel ~ Xvel, data=gaugings,  main="Linear", xlab="Xvel", ylab="Yvel")
      lines(x$Xvel, predict(modelLin, x),col = "darkgreen", lwd = 3)
      plot(Yvel ~ Xvel, data=gaugings, main="2nd order", xlab="Xvel", ylab="Yvel")
      lines(x$Xvel ,predict(modelPoly2, x) ,col = "darkgreen", lwd = 3)
      plot(Yvel ~ Xvel, data=gaugings, main="3rd order", xlab="Xvel", ylab="Yvel")
      lines(x$Xvel ,predict(modelPoly3, x) ,col = "darkgreen", lwd = 3)
      angle <- modelLin$coefficients[2]
      
      if (Poly==0){
        fit <- menu(c("Linear", "Quadratic", "Polynomial"), title="Do you want this?")
      }else {
        if (Poly>=1  & Poly <=3)
        {
          fit <- Poly
        }else
        {
          fit <- menu(c("Linear", "Quadratic", "Polynomial"), title="Do you want this?")             
        }
      }
      
      models <- list()
      models[["simple"]] <- rlm(MeasV ~ Xvel, data = gaugings)
      models[["empirical"]] <-  rlm(MeasV ~ Xvel+XHt, data = gaugings)
      # emd
      models[["emd"]] <- rlm(MeasV ~ TideA + TideB + Event, data = gaugings)
      models[["emdHt"]] <-  rlm(MeasV ~ TideA + TideB + Event + EventHt, data = gaugings)
      models[["emdHtBE"]] <-  rlm(MeasV ~ TideA + TideB + Event + EventHt + TideBEvent, data = gaugings)
      # Yvel
      models[["emdY"]] <- rlm(MeasV ~ TideA + TideB + Event + Yvel + TideBEvent, data = gaugings) 
      models[["emdHtY"]] <- rlm(MeasV ~ TideA + TideB + Event + EventHt + Yvel + TideBEvent, data = gaugings)
      models[["emdEventY"]] <- rlm(MeasV ~ TideA + TideB + Event + EventY + TideBEvent, data = gaugings)
      models[["emdHtEventY"]] <- rlm(MeasV ~ TideA + TideB + Event + EventHt + EventY + TideBEvent, data = gaugings)
      
      models[["XHtY"]] <- rlm(MeasV ~ Xvel + XHt + Yvel , data=gaugings)
      models[["XY"]] <- rlm(MeasV ~ Xvel * Yvel, data=gaugings)
      models[["Y"]] <- rlm(MeasV ~ Xvel + Yvel, data=gaugings)
      models[["XHtXY"]] <- rlm(MeasV ~ Xvel + Xvel:Height + Xvel:Yvel, data=gaugings)
      
      # if tide b is order of magnitude above tide a
      if ( abs(coef(models[["emdHtBE"]])["TideB"] / coef(models[["emdHtBE"]])["TideA"]) > 3 | coef(models[["emdHtBE"]])["TideA"] < 0)
      {
        # remove BE as predictor from all models
        models <- lapply(models, removeBE)
      }
      
      if (includeintercept){
        models <- lapply(models, includeIntercept)
      }else{
        models <- lapply(models, zeroIntercept)
      }

      # Model stats
      EventSig <- cbind(data.frame(coefficients = coef(models[["emdHt"]])), pvalue=rob.pvals(models[["emdHt"]]))["Event","pvalue"]
      EventHtSig <- cbind(data.frame(coefficients = coef(models[["emdHt"]])), pvalue=rob.pvals(models[["emdHt"]]))["EventHt","pvalue"]
      XvelSig <- cbind(data.frame(coefficients = coef(models[["empirical"]])), pvalue=rob.pvals(models[["empirical"]]))["Xvel","pvalue"]
      XvelHtSig <- cbind(data.frame(coefficients = coef(models[["empirical"]])), pvalue=rob.pvals(models[["empirical"]]))["XHt","pvalue"]
      XHtXvelCoef <- coef(models[["empirical"]])["Xvel"]
      EventHtCoef <- coef(models[["emdHt"]])["EventHt"]
      
      if(EventHtSig < 0.05 & EventHtCoef < 0)  heightIsAFactor <- TRUE
      
      if (heightIsAFactor){
        EMDEventCoef <- coef( models[["emdHt"]])["Event"]
        EMDTideCoef <- coef( models[["emdHt"]])["TideA"]
      }else{
        EMDEventCoef <- coef(models[["emd"]])["Event"]
        EMDTideCoef <- coef(models[["emd"]])["TideA"]
      }
      if (EMDEventCoef > 1 & EMDEventCoef > EMDTideCoef) useEMD <- TRUE
      if (EventHtSig < XvelHtSig)  useEMD <- TRUE
      
      if (useEMD == FALSE & min(gaugings$Xvel < 0.1)) # if emd not in use, probably not tidal
      { # so..
        # override the non-emd models to add an offset if the xvel is close to zero
        models[["simple"]] <- includeIntercept(models[["simple"]])
        models[["empirical"]] <-   includeIntercept(models[["empirical"]])
        models[["XHtY"]] <- includeIntercept(models[["XHtY"]] )
        models[["XY"]] <- includeIntercept(models[["XY"]])
        models[["Y"]] <- includeIntercept(models[["Y"]] )
        models[["XHtXY"]] <- includeIntercept(models[["XHtXY"]]) 
      }
      
      modellistoutput <- list()
      if (heightIsAFactor)
      {
        if (useEMD){


          if (fit == 3) 
          {
            modellistoutput[["Primary"]] <- models[["emdHtEventY"]]
            modellistoutput[["Secondary"]] <- models[["emdHtY"]]
          }
          if (fit == 2)
          {
            if (abs(angle) > 0.1){
              modellistoutput[["Primary"]] <- models[["emdHtY"]]
              modellistoutput[["Secondary"]] <-models[["emdHtBE"]]
            }else{
              modellistoutput[["Primary"]] <- models[["emdHtBE"]]
              modellistoutput[["Secondary"]] <-models[["emdHtY"]]
            }
          }
          if (fit == 1)   
          {
            if (EventHtCoef > -0.2 & EventHtCoef < 0.1)
            {
              if (EMDEventCoef < 1) # event coef not realistic, so make emd secondary
              {  
                modellistoutput[["Primary"]] <- models[["empirical"]]
                modellistoutput[["Secondary"]] <- models[["emdHtBE"]]
              }else 
              {
                modellistoutput[["Primary"]] <- models[["emdHtBE"]]
                modellistoutput[["Secondary"]] <- models[["empirical"]]
              }
              
            }else{ # eventHt not realistic, so make secondary
              modellistoutput[["Primary"]] <- models[["emd"]]
              modellistoutput[["Secondary"]] <- models[["emdHtBE"]]
            }
          } 
        }else #not EMD
        {
          if (fit == 3)
          {
            modellistoutput[["Primary"]] <- models[["XHtXY"]]
            modellistoutput[["Secondary"]] <- models[["empirical"]]
          }
          if (fit == 2)
          {
            if (abs(angle)>0.1) 
            { 
              modellistoutput[["Primary"]] <- models[["XY"]]
              modellistoutput[["Secondary"]] <- models[["empirical"]]
            }else{
              modellistoutput[["Primary"]] <- models[["empirical"]]
              modellistoutput[["Secondary"]] <- models[["XHtY"]]
            }
          }
          if (fit == 1)
          {
            if (XHtXvelCoef>0){
              modellistoutput[["Primary"]] <- models[["empirical"]]
              modellistoutput[["Secondary"]] <- models[["simple"]]
            }else
            {
              modellistoutput[["Primary"]] <- models[["simple"]]
              modellistoutput[["Secondary"]] <- models[["empirical"]]
            }
          }
        }
        
      }else #height not a factor
      {
        if (useEMD)
        {
          #emdYvelVI
          if (fit==3)
          {
            modellistoutput[["Primary"]] <- models[["emdEventY"]]
            modellistoutput[["Secondary"]] <- models[["emdY"]]
          }
          if (fit==2)
          {
            modellistoutput[["Primary"]] <- models[["emdY"]]
            modellistoutput[["Secondary"]] <- models[["emd"]]
          }
          if (fit==1)
          {
            modellistoutput[["Primary"]] <- models[["emd"]]
            modellistoutput[["Secondary"]] <- models[["simple"]]
          }
        }else{ # Not EMD
          if (fit==3)
          {
            modellistoutput[["Primary"]] <- models[["XY"]]
            modellistoutput[["Secondary"]] <- models[["Y"]]
          }
          if (fit==2)
          {
            modellistoutput[["Primary"]] <- models[["Y"]]
            modellistoutput[["Secondary"]] <- models[["simple"]]
          }
          if (fit==1)
          {
            modellistoutput[["Primary"]] <- models[["simple"]]
          }
        }
      }
      
      
      return(modellistoutput)
      
      
    }
    
  }
  #END OF FUNCTIONS  
  #########################################################33
  # PART ONE
  if (token == "")
  {
    message("No influxdb token supplied")
    return (0)
  }
  
  client <- InfluxDBClient$new(url = "https://eastus-1.azure.cloud2.influxdata.com",
                               token = token,
                               org = "stephen.wallace@des.qld.gov.au")
  
  #dir.create(GSNumber)
  if (!dir.exists(GSNumber)){
    dir.create(GSNumber)
  } else {
    #print("Dir already exists!")
  }
  
  #updateMethod <- "Append"
  
  ################################################################
  # LRH EQUAL TO THE HEIGHT OF HADCP
  message("Retrieving Height from influxDB")
  #Stage <- read.csv(stageFile, skip=2)
  #head(Stage)
  
  #getwd()
  if (!file.exists( paste(GSNumber,"/",GSNumber,"Stage.rds",sep="") )) 
  {
    updateMethod <- "Replace"
  }

  if(updateMethod == "Replace") # or file doesn't exist
  {
    Stage <- getTSDB(client, GSNumber, 100, start=startdate, stop=enddate )
    saveRDS(Stage, paste(GSNumber,"/",GSNumber,"Stage.rds",sep=""))
    Stage <- Stage[[1]]
  }else if( updateMethod == "LocalFile")
  {
    Stage <- readRDS(paste(GSNumber,"/",GSNumber,"Stage.rds",sep=""))
    Stage <- Stage[[1]]
  }else if(updateMethod == "Append")
  {
    oldStage <- readRDS(paste(GSNumber,"/",GSNumber,"Stage.rds",sep=""))
    oldStage <- oldStage[[1]]
    #as.Date(Stage[nrow(oldStage),1])-1
    Stage <- getTSDB(client, GSNumber, 100, start=as.Date(oldStage[nrow(oldStage),1])-1, stop=enddate )
    # merge Stage
    Stage <- mergeTS(oldStage, Stage[[1]])
    saveRDS(list(Stage), paste(GSNumber,"/",GSNumber,"Stage.rds",sep=""))
    # chop this here after startdate 'skw to do
    
    #as.POSIXct(startdate)
    #as.POSIXct(startdate,origin="1970-01-01")
    Stage <- Stage[Stage$time >= as.POSIXct(startdate,origin="1970-01-01"),]
    
    if(enddate > startdate) Stage <- Stage[Stage$time <= as.POSIXct(enddate,origin="1970-01-01"),]
    
    
  }
  
  Stage <- data.frame(Date = Stage$time, Point = Stage$value_100, QC = Stage$QC_100 )
  
  ReadHADCP <- function (DataIn, stime, heights, lrh )
  {

    timestamps <- DataIn$Date
    lrhPeriods <- data.frame(start = c(0,lrh$Date),
                          end = c(lrh$Date, NA),
                          lrh = c(NA,lrh$lrh)
    )
    lrhPeriods <- lrhPeriods[-1,]
    lrhPeriods$start <- as.POSIXct(lrhPeriods$start, origin="1970-01-01")
    attr(lrhPeriods$start, "tzone") <- "Australia/Brisbane" 
    attr(lrhPeriods$end, "tzone") <- "Australia/Brisbane" 
    
    firstDataPoint <-  timestamps[1]
    lastDataPoint <- timestamps[length(timestamps)]
    
    lrhPeriods$start[1] <- min(firstDataPoint, lrhPeriods$start[1] )
    lrhPeriods$end[nrow(lrhPeriods)] <- max(lastDataPoint, lrhPeriods$start[nrow(lrhPeriods)] )
    
    startPer <- which(firstDataPoint >= lrhPeriods$start & firstDataPoint < lrhPeriods$end)
    endPer <- which(lastDataPoint > lrhPeriods$start & lastDataPoint <= lrhPeriods$end)
    
    lrhPeriods <- lrhPeriods[startPer:endPer,]
    f.stage <- approxfun(stime, heights)
    
    adcpData <- list()
    for(lrhPeriod in 1:nrow(lrhPeriods))
    {
      thisPeriod <- lrhPeriods[lrhPeriod,]
      adcpSub <- DataIn[DataIn[,1] > thisPeriod$start & DataIn[,1] <= thisPeriod$end,  ]
      
      svdf <- data.frame(adcpSub, ht = f.stage(adcpSub[,1]))
      svdf <- na.omit(svdf)
      svdf <- svdf[svdf$ht > thisPeriod$lrh, ]
      
      adcpData[[lrhPeriod]] <- svdf  
    }
    adcpData <- do.call(rbind, adcpData)
    gaplist <- GapList( adcpData )
    
    adcpData <- fillGaps(adcpData, gaplist) # returns a quality coded dataframe, coded for gaps
    
    # downgrade qualities to the higher of, input, and gapfilled
    if (ncol(DataIn) == 3)
    { 
      adcpData[,3] <- pmax(
        f.qualmax(adcpData[,1], DataIn[,1], DataIn[,3]), #interpolate at x, for a timeseries, t/qc
        adcpData[,3] )
      
    }
    
    return(data.frame( t = adcpData$Date, y = adcpData$Point, QC = adcpData$QC))
    
  }
  
  message("Retrieving Xvel from influxDB")
  
  oldxvelstart <- as.Date(Sys.Date())
  if(updateMethod == "Replace")
  {
    Xvel <- getTSDB(client, GSNumber, "240.70", start=startdate, stop=enddate)
    saveRDS(Xvel, paste(GSNumber,"/",GSNumber,"Xvel.rds",sep=""))
    Xvel <- Xvel[[1]]
  }else if(updateMethod == "LocalFile")
  {
    Xvel <- readRDS(paste(GSNumber,"/",GSNumber,"Xvel.rds",sep=""))
    Xvel <- Xvel[[1]]
  }else if(updateMethod == "Append")
  {
    oldXvel <- readRDS(paste(GSNumber,"/",GSNumber,"Xvel.rds",sep=""))
    oldXvel <- oldXvel[[1]]
    #as.Date(Stage[nrow(oldStage),1])-1
    oldxvelstart <- as.Date(oldXvel[nrow(oldXvel),1])-1
    Xvel <- getTSDB(client, GSNumber, "240.70", start=oldxvelstart, stop=enddate )
    # merge Stage
    Xvel <- mergeTS(oldXvel, Xvel[[1]])
    saveRDS(list(Xvel), paste(GSNumber,"/",GSNumber,"Xvel.rds",sep=""))
    # chop this here after startdate 'skw to do
    Xvel <- Xvel[Xvel$time >= as.POSIXct(startdate,origin="1970-01-01"),]
    if(enddate > startdate) Xvel <- Xvel[Xvel$time<= as.POSIXct(enddate,origin="1970-01-01"),]
  }
  Xvel <- data.frame(Date = Xvel$time, Point = Xvel$value_240.70, QC = Xvel$QC_240.70 )
  
  #attr(Stage$Date, "tzone") <- "Australia/Brisbane"
  

  if(skipYvel == FALSE)
  {
    message("Retrieving Yvel from influxDB")
    if(updateMethod == "Replace")
    {
      Yvel <- getTSDB(client, GSNumber, "240.73", start=startdate, stop=enddate)
      saveRDS(Yvel, paste(GSNumber,"/",GSNumber,"Yvel.rds",sep=""))
      Yvel <- Yvel[[1]]
    }else if(updateMethod == "LocalFile")
    {
      Yvel <- readRDS(paste(GSNumber,"/",GSNumber,"Yvel.rds",sep=""))
      Yvel <- Yvel[[1]]
    }else if(updateMethod == "Append")
    {
      oldYvel <- readRDS(paste(GSNumber,"/",GSNumber,"Yvel.rds",sep=""))
      oldYvel <- oldYvel[[1]]
      #as.Date(Stage[nrow(oldStage),1])-1
      Yvel <- getTSDB(client, GSNumber, "240.73", start=as.Date(oldYvel[nrow(oldYvel),1])-1, stop=enddate )
      # merge Stage
      Yvel <- mergeTS(oldYvel, Yvel[[1]])
      saveRDS(list(Yvel), paste(GSNumber,"/",GSNumber,"Yvel.rds",sep=""))
      # chop this here after startdate 'skw to do
      Yvel <- Yvel[Yvel$time >= as.POSIXct(startdate,origin="1970-01-01"),]
    }
    Yvel <- data.frame(Date = Yvel$time, Point = Yvel$value_240.73, QC = Yvel$QC_240.73 )

    
  }
  
  message("Retrieving local site params from influxDB") 
  if(updateMethod =="Replace" | updateMethod =="Append") # or file not there
  {
    siteparams <- getSITE(client, GSNumber)
    saveRDS(siteparams, paste(GSNumber,"/",GSNumber,"siteparams.rds",sep=""))
  }else{
    siteparams <- readRDS(paste(GSNumber,"/",GSNumber,"siteparams.rds",sep=""))
  }
  lrh <- data.frame(Date = siteparams$LRV$time, lrh = siteparams$LRV$"_value")
  
  
  #plot(Xvel$Date, Xvel$Point)
  #plot(Stage$Date,  despiked(Stage[,2]))
  message("reading xvel")
  
  ts <- ReadHADCP(Xvel, Stage$Date, despiked(Stage[,2]), lrh)
  
  # skipYvel <- FALSE
  if (skipYvel == TRUE)
  {
    tsy <- ts
    tsy$y <- 0
    tsy$QC <- 250
  }else{
    
    if (nrow(Yvel>0) )
    {
      message("reading yvel")
      tsy <- ReadHADCP(Yvel, Stage$Date, Stage[,2], lrh)
      
    }else
    {
      tsy <- ts
      tsy$y <- 0
      tsy$QC <- 250
    }
  }

  ts$QC <- pmax(
    f.qualmax(ts$t, ts[,1], ts[,3]),
    f.qualmax(ts$t, Stage$Date, Stage$QC)
  )
  
  # dygraphs::dygraph(ts)
  
  message("Converting to hourly interval for EMD")
  #ts$t <- as.POSIXct( ts$t*60 , origin="1970-01-01")
  convertedTS <- changeInterval(ts, dt=1, Interval="Hourly", offset=-30, option="inst")
  
  #large gaps don't work well with EMD
  #velocity data gets subsectioned between the gaps and runs the EMD analysis separately

  # Get data chunks for EMD, calculate gaps, convert to blocks
  naremoved <- na.omit(ts)
  #gaps <- GapList(na.omit(data.frame(ts$t), minGap = 720) # gaps in minutes
  gaps <- GapList(data.frame(naremoved$t), minGap = 720) # gaps in minutes
  blocks <- blockList(gaps,naremoved )
  
  # uselocalcopy <- FALSE
  ##############
  # PART TWO
  
  message ("Retrieving gaugings from InfluxDB")
  if(updateMethod =="Replace") # or file not there)
  {
    Gaugings <- getGauging(client, GSNumber)
    #tail(Gaugings)
    saveRDS(Gaugings, paste(GSNumber,"/",GSNumber,"Gaugings.rds",sep=""))
  }else if(updateMethod =="Append") # or file not there)
  {
    # tail(Gaugings)
    Gaugings <- readRDS(paste(GSNumber,"/",GSNumber,"Gaugings.rds",sep=""))
    #NewGaugings <- getGauging(client, GSNumber, start = as.Date("2022-11-01") )
    NewGaugings <- getGauging(client, GSNumber, start = as.Date(max(Gaugings$time)) )
    if(!is.null(NewGaugings))
    {
      Gaugings <- rbind(Gaugings, NewGaugings[NewGaugings$time > max(Gaugings$time),])
      saveRDS(Gaugings, paste(GSNumber,"/",GSNumber,"Gaugings.rds",sep=""))
    }
    #saveRDS(Gaugings, paste(GSNumber,"/",GSNumber,"Gaugings.rds",sep=""))
  }else{
    Gaugings <- readRDS(paste(GSNumber,"/",GSNumber,"Gaugings.rds",sep=""))
  }

  
  message ("Retrieving XS from InfluxDB")
  if(updateMethod =="Replace" | updateMethod =="Append") # or file not there)
  {
    xs <- getXSDB(client, GSNumber)
    saveRDS(xs, paste(GSNumber,"/",GSNumber,"xs.rds",sep=""))
  }else{
    xs <- readRDS(paste(GSNumber,"/",GSNumber,"xs.rds",sep=""))
  }
  RatePeriods <- xs$Periods
  xsdf <- xs$XS
  
  # inputVI <- NULL
  if(!is.null(inputVI))
  {
    Models <- inputVI
  }else{
    Models <- getVI(client, GSNumber)
  }
  
  message("Filtering EMD")
  # process data with EMD
  if(updateMethod == "Replace" | !file.exists( paste(GSNumber,"/",GSNumber,"_ceemdanVelDespiked.rds",sep="") ))
  {
    ceemdanVel <- processEMD(convertedTS, blocks, method = emdmethod) # input hourly dataframe, with posixct for time and values
  }else if(updateMethod == "Append")
  {
    ceemdanVel <- readRDS(paste(GSNumber,"/",GSNumber,"_ceemdanVelDespiked.rds", sep=""))
    #tail(ceemdanVel)
    # Subset blocks to only new ones ending before end data
  
    # read back from the beginning, if the start time is less than the xvel start time, start from there. 
    for (blocknum in nrow(blocks):1)
    {
      if (blocks[blocknum,]$Start < as.POSIXct( oldxvelstart) )
      {
        break # break out of for loop
      }
    }
    
    #tail(ceemdanVel)
    #newDataHours <- difftime(blocks$End[nrow(blocks)], convertedTS$Date[(nrow(convertedTS))], units='hours')
    newDataHours <- difftime(convertedTS$Date[(nrow(convertedTS))], ceemdanVel$time[nrow(ceemdanVel)], units='hours')
    if (newDataHours > 1) # if more than an hour of new data to filter
    {
      newceemdan <- processEMD(convertedTS, blocks[blocknum:nrow(blocks),], method=emdmethod) # input hourly dataframe, with posixct for time and values
      newceemdan <- newceemdan %>% dplyr::filter(time > blocks[blocknum,]$Start )
      #plot(newceemdan$time, newceemdan$a)
      # merge ceemdanVel with newceemdan, overwriting with newceemdan
      ceemdanVel <- mergeTS(ceemdanVel, newceemdan, method="Replace" )
    }else
    { # just use the rds file, don't update anything
      ceemdanVel <- ceemdanVel
    }
    
  }else if(updateMethod == "LocalFile")
  {
    ceemdanVel <- readRDS(paste(GSNumber,"/",GSNumber,"_ceemdanVelDespiked.rds", sep=""))
  }
    
  # plot EMD
  #plotCEEMD(ceemdanVel)
  #write ceemd to csv
  #write.csv(ceemdanVel,paste(GSNumber,"/",GSNumber,"_ceemdanVelDespiked.CSV", sep=""), row.names=FALSE)
  
  saveRDS(ceemdanVel,paste(GSNumber,"/",GSNumber,"_ceemdanVelDespiked.rds", sep=""))
  #tail(ceemdanVel)

  if (ignoreQEst == TRUE)
  {
    Gaugings <- Gaugings %>% dplyr::filter(Method != "ES")
  }
  #STEP ONE
  ## CALCULATE PARAMETERS FOR VELOCITY INDEX MODEL
  #use chopped, gap filled velocities
  
  
  head(ts)
  head(tsy)
  
  # fill tsy with zeroes to match up with ts
  if (tsy[1,1] > ts[1,1])
  {
    tsy <- rbind(ts[1,], tsy)
    tsy[1,2] <- 0
    tsy[1,3] <- 250
  }

  f.xvel <- approxfun(ts$t, ts$y)
  f.yvel <- approxfun(tsy$t, tsy$y)
  
  DataOutput <- cbind(ts, f.yvel(ts$t))
  names(DataOutput) <- c("Timestamp","Xvel","QC","Yvel")
  
  head(ceemdanVel)
  # subtract b and c from Xvel to get TideA
  TideB <- data.frame(ceemdanVel$time, ceemdanVel$b)
  Event <- data.frame(ceemdanVel$time, ceemdanVel$c)
  TideA <- addTS(data.frame(ts$t, ts$y), TideB, combMethod = "Subtract") # subtract TideB from Xvel
  TideA <- addTS(TideA, Event, combMethod = "Subtract") # Then subtract Event
  
  f.tideA <- approxfun(TideA)
  f.tideB <- approxfun(TideB)
  f.Event <- approxfun(Event)
  f.Stage <- approxfun(Stage$Date, Stage$Point)
  
  DataOutput <- cbind(DataOutput, TideA = f.tideA(ts$t))
  DataOutput <- cbind(DataOutput, TideB = f.tideB(ts$t))
  DataOutput <- cbind(DataOutput, Event = f.Event(ts$t))
  DataOutput <- cbind(DataOutput, Height = f.Stage(ts$t))
  
  message("calculating areas..")
  DataOutput$Area <- round(PhasedAreas(RatePeriods, xsdf, data.frame(DataOutput$Timestamp, DataOutput$Height) ),3)

  DataOutput <- cbind(DataOutput, TideBEvent = DataOutput$Event * DataOutput$TideB)  
  DataOutput <- cbind(DataOutput, EventHt = DataOutput$Event * DataOutput$Height)
  DataOutput <- cbind(DataOutput, XHt = DataOutput$Xvel * DataOutput$Height)
  DataOutput <- cbind(DataOutput, XY = DataOutput$Xvel * DataOutput$Yvel)
  DataOutput <- cbind(DataOutput, EventY = DataOutput$Event * DataOutput$Yvel)
  
  head(DataOutput)
  
  #saveRDS(DataOutput, paste("ORC_Predictors.rds"))
  

  # create viparams from gaugings
  head(Gaugings)
  # downgrade gauging qc
  # Set Gauging QC to 60
  if ( length(Gaugings) > 0)
  {
    GaugingQC <- f.qualmax(Gaugings$time, DataOutput$Timestamp, DataOutput$QC) #interpolate at x, for a timeseries, t/qc
    if ( nrow(Gaugings[Gaugings$Method == "ES",]) > 0  ) {Gaugings[Gaugings$Method == "ES",]$QC <- 60}
    Gaugings$QC <- pmax(GaugingQC, Gaugings$QC )
  }
  f.Area <- approxfun(DataOutput$Timestamp, DataOutput$Area)
    
  VIParams <- data.frame(Timestamp = Gaugings$time, MeasQ = Gaugings$Flow, QC = Gaugings$QC, Method =Gaugings$Method )
  VIParams <- cbind(VIParams, Xvel = f.xvel(Gaugings$time))
  VIParams <- cbind(VIParams, Yvel = f.yvel(Gaugings$time))
  VIParams <- cbind(VIParams, TideA = f.tideA(Gaugings$time))
  VIParams <- cbind(VIParams, TideB = f.tideB(Gaugings$time))
  VIParams <- cbind(VIParams, Event = f.Event(Gaugings$time))
  VIParams <- cbind(VIParams, Height = f.Stage(Gaugings$time))
  VIParams <- cbind(VIParams, Area = f.Area(Gaugings$time))
  

  VIParams <- cbind(VIParams, TideBEvent = VIParams$Event * VIParams$TideB)
  VIParams <- cbind(VIParams, EventHt = VIParams$Event * VIParams$Height)
  VIParams <- cbind(VIParams, XHt = VIParams$Xvel * VIParams$Height)
  VIParams <- cbind(VIParams, XY = VIParams$Xvel * VIParams$Yvel)
  VIParams <- cbind(VIParams, EventY = VIParams$Event * VIParams$Yvel)
  VIParams <- cbind(VIParams, MeasV = VIParams$MeasQ / VIParams$Area)
  
  head(VIParams)
  

  #DataOutput[is.na(DataOutput)]

  # tidy up
  rm(list = c("f.tideA","f.tideB","f.Event", "f.Stage","f.Area","f.xvel","f.yvel") )
  rm(list = c("TideB","Event","TideA") )
  
  # vi params are anything with a qc < 160
  VIParams <- VIParams %>% dplyr::filter(QC <= 160)
  head( VIParams )
  
  #saveRDS(VIParams, "ORC_Gaugings.rds")
  
  
  modelTimesList <- function(Models, timestamps) # creates a list of times of models
  {
    mergedmodels <- do.call(rbind, Models) # merge list
    modelTimes <- unique(mergedmodels$time) # get a vector of unique times
    modelTimes <- data.frame( period = seq(1:length(modelTimes)),
                              times = modelTimes
    )
    attr(modelTimes$times, "tzone") <- "Australia/Brisbane" 
    #firstDataPoint <- as.POSIXct("2020-01-01 00:00")
    firstDataPoint <-  timestamps[1]
    lastDataPoint <- timestamps[length(timestamps)]
    
    # exclude any model times that end before the lastDataPoint
    modelTimes <- modelTimes[modelTimes$times < lastDataPoint,] 
    dfModelTimes <- data.frame(period = c(NA,modelTimes$period),
                               Start = c(firstDataPoint, modelTimes$times), # dataframe of model time start and end
                               End = c(modelTimes$times, lastDataPoint)
    )
    dfModelTimes <- dfModelTimes[-1,] # exclude redundant top row
    
    # exclude any model times where the firstDataPoint is greater than the model period end
    dfModelTimes <- dfModelTimes[firstDataPoint < dfModelTimes$End,]
    return(dfModelTimes)
  }
  
  #ignoreQEst <- FALSE
  #Poly <- 2
  # CALCVI <- TRUE
  dfModelTimes <- modelTimesList(Models, DataOutput$Timestamp) # list times models are applied to/from.
  if(CALCVI)
  {
    if (is.null(Models)) 
    {
      message("NULL returned from VI, is this correct?")
      dfModelTimes <- list()
      dfModelTimes <- data.frame(period = 1, Start = DataOutput$Timestamp[1], End = DataOutput$Timestamp[nrow(DataOutput)] ) 
    }#else{
    #  dfModelTimes <- modelTimesList(Models, DataOutput$Timestamp)
    #}
    
    calcVI <- function(viperiods, viparams, HighFlowThreshold = -99, forceEMD = TRUE, forceHT = TRUE, forceOffset=TRUE, Poly = 1)
    {

      visubset <- viparams %>% dplyr:::filter(Timestamp > viperiods$Start & Timestamp < viperiods$End | QC == 60 | MeasQ > 400 )
      message("high gaugings")
      out <- VIfromGaugings(visubset, forceEMD = forceEMD, forceHT = forceHT, includeintercept=forceOffset, Poly = Poly)
      
      ModelsList <- list ("high" =  out)

      ######################
      #THEN THIS
      #Create subset for low velocities
      if (HighFlowThreshold != -99){
        message("low gaugings")
        if (ABS==FALSE){
          #LowGaugings <- subset(VIParams, VIParams$Xvel < HighFlowThreshold)
          LowGaugings <- subset(visubset, xor ( visubset$Xvel < HighFlowThreshold, visubset$Xvel > 1  ))
        }else{
          LowGaugings <- subset(visubset, xor ( abs(visubset$Xvel) < HighFlowThreshold, visubset$Xvel > 1  ))
        }
        #plot(LowGaugings$Timestamp, LowGaugings$Xvel)
        lowout <- VIfromGaugings(LowGaugings, forceEMD = forceEMD, forceHT = forceHT, includeintercept=TRUE, Poly = Poly ) #VIfromGaugings(LowGaugings) ##
        ModelsList <- c(ModelsList, list ("low" = lowout))
      }
      
      #####################
      return(ModelsList)
      
      
    }
    
    viperiodlist <- list()

    # periodNum <- 1
    for (periodNum in 1:nrow(dfModelTimes))
    {
      message(paste("VI period:",dfModelTimes[periodNum,]$Start))
      #attr(period$start, "tzone") <- "Australia/Brisbane" 
      period <- dfModelTimes[periodNum,]
      viperiodlist[[paste(period$Start)]] <- calcVI(period, VIParams, HighFlowThreshold, Poly=Poly, forceOffset = TRUE)
    }

    if (startdate == 0){
      saveRDS(viperiodlist, file = paste(GSNumber,"/",GSNumber, ".MODELLIST.rds", sep=""))
    }
    return(viperiodlist)
    
  }


  modelPeriodList <- list()
  for(modelPeriod in 1:nrow(dfModelTimes)) # apply velocity index models to DataOutput
  {
    thisPeriod <- dfModelTimes[modelPeriod,] # a df of this model period of applicability
    thisModels <- Models[[thisPeriod$period]] # select models with this id number
    subsetDataOuput <- DataOutput[DataOutput$Timestamp > thisPeriod$Start & DataOutput$Timestamp < thisPeriod$End,] # subset times for this period
    
    # group by ID, models with the same ID are a low/high version for the same velocity index calculation
    thisModels <- arrange(thisModels, ID, Threshold)
    thisModels <- split(thisModels,rleid(thisModels$ID ) ) 
    
    thisModelList <- list()
    for(thisModel in thisModels) # each model has an ID
    {
      # thisModel <- thisModels[[1]]
      modelID <- thisModel$ID[1]
      modelRows <- nrow(thisModel) # number of rows this model has (more than one row means low/high thresholds)
      thisModel$Threshold <- as.numeric(thisModel$Threshold)
      #thisModel <- thisModel[order(thisModel$Threshold),] # sort by thresholds
      
      thresholdList <- list()
      for(threshold in 1:modelRows) # for each threshold subset the dataset to between this and the next.. and apply the velocity index!
      {
        #print(threshold)
        thisthreshold <- thisModel[threshold,]$Threshold
        head(subsetDataOuput)
        
        threshSubset <- subsetDataOuput[subsetDataOuput$Xvel >= thisthreshold,] # chop anything below this threshold
        
        if (threshold < modelRows)
        {
          nextthreshold <- thisModel[threshold+1,]$Threshold
          threshSubset <- subsetDataOuput[subsetDataOuput$Xvel < nextthreshold,]
        }
        head(threshSubset)
        VI <- processInfluxVI(thisModel[threshold,])
        #vidf <- data.frame(threshSubset$Timestamp, CalcV = predict(VI, threshSubset))
        #vidf$Q <- threshSubset$Area * vidf$CalcV 
        #thresholdList[[threshold]] <- vidf 
        
        calcOut <- data.frame(Timestamp = threshSubset$Timestamp, CalcV = predict(VI, threshSubset))
        calcOut$Q <- calcOut$CalcV*threshSubset$Area
        thresholdList[[threshold]] <- calcOut
      }
      thresholdList <- do.call(rbind,thresholdList) # merge list of thresholds
      thresholdList <- thresholdList[order(thresholdList[,1]),] # reorder by date
      
      thisModelList[[paste("model",modelID,sep="")]][["Calcs"]] <- thresholdList # add velocity indexed velocity to list
      thisModelList[[paste("model",modelID,sep="")]][["VI"]] <- thisModel # add velocity index equation to list
    }
    
    modelPeriodList[[paste(thisPeriod$Start)]] <- thisModelList
    
  }
  
  modelPeriodList[["Predictors"]] <- DataOutput
  modelPeriodList[["VIParams"]] <- VIParams

  #modelPeriodList$`2018-12-06 12:06:00`$model1$VI
  #modelPeriodList$`2018-12-06 12:06:00`$model2$VI
  
  
  if (startdate == 0){
    saveRDS(modelPeriodList, file = paste(GSNumber,"/",GSNumber, ".Discharge.rds", sep=""))
  }else{
    saveRDS(modelPeriodList, file = paste(GSNumber,"/",startdate,"_",GSNumber, ".Discharge.rds", sep=""))
  }
  
    
  return(modelPeriodList)
  # end of function
  

}



library(lattice)
library(ggallin)
library(gridExtra)
library(ggpubr)

doResidualPlots <- function(model, Gaugings, Predictors)
{
  get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  asinh_trans <- scales::trans_new(
    "inverse_hyperbolic_sine",
    transform = function(x) {asinh(x)},
    inverse = function(x) {sinh(x)}
  )
  
  df <- data.frame(Predicted = predict(model, Gaugings), 
                   Residual = Gaugings$MeasV- predict(model, Gaugings), 
                   Magnitude = Gaugings$EventHt, 
                   Xvel = Gaugings$Xvel,
                   Method = Gaugings$Method
  )
  
  df$RawResid <- Gaugings$MeasV - Gaugings$Xvel
  
  yrange <- c(
    min(df$RawResid, df$Residual),
    max(df$RawResid, df$Residual)
    
  )
  xrange <- c(
    min(df$Xvel, df$Predicted),
    max(df$Xvel, df$Predicted)
  )
  
  
  Pred_Plot <- ggplot( df, aes(Predicted, Residual, shape=Method)) + 
    geom_point(aes( colour = Magnitude), size=3) +
    scale_color_viridis(option = "plasma")+
    geom_hline( yintercept = 0)
  
  Raw_Plot <- ggplot( df, aes(Xvel, RawResid, shape=Method)) + 
    geom_point(aes( colour = Magnitude), size=3) +
    scale_color_viridis(option = "plasma")+
    geom_hline( yintercept = 0)
  
  
  Raw_Plot <- Raw_Plot +
    scale_y_continuous(trans=asinh_trans, limits=yrange ) +
    scale_x_continuous(trans=asinh_trans )
  
  Pred_Plot <- Pred_Plot +
    scale_y_continuous(trans=asinh_trans, limits=yrange ) +
    scale_x_continuous(trans=asinh_trans )
  
  
  legend <- get_legend(Raw_Plot)
  #Raw_Plot <- Raw_Plot + theme(legend.position="none")
  #Pred_Plot <- Pred_Plot + theme(legend.position="none")
  
  #grid.arrange(Raw_Plot, Pred_Plot, legend, nrow=1, ncol=3, widths=c(2.3,2.3,0.8))
  
  
  RawResidTime <- ggplot( df, aes( seq(1,nrow(df)), RawResid, shape=Method)) + 
    geom_point(aes( colour = Magnitude), size=3) +
    scale_color_viridis(option = "plasma")
  
  ResidTime <- ggplot( df, aes( seq(1,nrow(df)), Residual, shape=Method)) + 
    geom_point(aes( colour = Magnitude), size=3) +
    scale_color_viridis(option = "plasma")
  
  RawResidTime <- RawResidTime +  scale_y_continuous(trans=asinh_trans, limits=yrange )
  ResidTime <- ResidTime +  scale_y_continuous(trans=asinh_trans, limits=yrange )
  
  legend <- ResidTime
  rightlegend <- get_legend(legend)
  legend <- ResidTime + theme(legend.position="bottom")
  bottomlegend <- get_legend(legend)
  #RawResidTime <- RawResidTime + theme(legend.position="none")
  #ResidTime <- ResidTime + theme(legend.position="none")
  
  
  FullPlot <- ggplot( Predictors, aes(Xvel, predict(model, Predictors ))) + 
    geom_point(aes( colour = EventHt)) +
    scale_color_viridis(option = "plasma")
  
  
  #grid.arrange(Raw_Plot, Pred_Plot, legend, RawResidTime, ResidTime, nrow=2, ncol=3, widths=c(2.3,2.3,0.8))
  
  plotList <- list()
  residList <- list()
  residTime <- list()
  legends <- list()
  
  legends[["bottom"]] <- bottomlegend
  legends[["right"]] <- rightlegend
  
  residList[["Raw"]] <- Raw_Plot
  residList[["Predicted"]] <- Pred_Plot
  residTime[["Raw"]] <- RawResidTime
  residTime[["Predicted"]] <- ResidTime
  
  plotList[["Residuals"]] <- residList
  plotList[["Times"]] <- residTime
  plotList[["legend"]] <- legends
  plotList[["FullPlot"]] <- FullPlot
  
  
  return(plotList)
}

AllFlow <- doForGS("1080025", updateMethod = "LocalFile", ABS=TRUE)
plot(Q ~ Timestamp, data= AllFlow$`2019-01-26 14:28:00`$model1$Calcs )



# or calculate a new one
model <- rlm(MeasV ~ TideA+TideB+Event+EventHt, data = AllFlow$VIParams)
rlmplots <- doResidualPlots(model, AllFlow$VIParams, AllFlow$Predictors)

rlmResiduals <- plots$Residuals$Predicted
rlmTime <- plots$Times$Predicted

ggarrange(rlmBEplots$FullPlot, rlmplots$FullPlot,  # + rremove("x.text")
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

ggarrange(rlmBEplots$FullPlot, rlmplots$FullPlot,  # + rremove("x.text")
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# or change it
model <- rlm(MeasV ~ TideA+TideB+Event*Height, data = AllFlow$VIParams)
plots <- doResidualPlots(model, AllFlow$VIParams, AllFlow$Predictors)

#rlmResiduals <- plots$Residuals$Predicted
#rlmTime <- plots$Times$Predicted

ggarrange(rlmBEplots$FullPlot, plots$FullPlot,  # + rremove("x.text")
          labels = c("A", "B"),
          ncol = 2, nrow = 1)




# document/examples of the functions here

########################
#
# Phased Areas

#readRDS(paste(path, "/", "dailyREDM.rds", sep=""))


