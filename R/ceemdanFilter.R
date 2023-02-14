#' Apply ceemdan to a data set
#'
#'     Applies ceemdan and merges similar period imfs,
#'     also calculates periods, amplitudes of each imfs
#'
#' @param time Vector, POSIXct time, must be even intervals, i.e. hourly
#' @param data Numeric, noisy or cyclical data to be filtered/decomposed
#' @param output 0, 1, 2 or 3
#'     0 = Output all below as a list
#'     1 = Output the grouped imfs, groupled by d1 and d2
#'     2 = Output period and amplitude of each imf
#'     3 = Ouptut each imf
#' @param method "emd" or "ceemdan"
#' @param d1 Numeric, hours to filter below
#' @param d2 Numeric, hours to filter above
#'
#' @return list or dataframe (depending on "output")
#'     either the entire list of all parameters
#' @importFrom Rlibeemd ceemdan
#' @importFrom EMD emd
#' @importFrom signal butter
#'
#' @examples
#'
#' library(ccInterp)
#' library(ggplot2)
#' library(reshape2)
#' library(ggpubr)
#'
#' # make some random data
#' randomMinutes <- rnorm(10000, mean = 60, sd = 30) # create 1000 random intervals
#' randomMinutes[randomMinutes < 1] <- 1 # set less than 1 minute to 1
#' randomMinutes <- as.POSIXct(cumsum(randomMinutes)*60, origin="2021-01-01")
#' # build a data frame of times and values
#' df <- data.frame(Time = randomMinutes,
#'                data = cumsum(rnorm(10000, mean = 0, sd = 10))   # add a row of random numbers
#' )
#' # add some noise
#' t <- as.numeric(df$Time)/60/60/24 # time in days
#' inputnoise <- 100 * sin( (12+(25/60)) * 2*pi * t) + # 12 hour frequency, amplitude 2
#' 20 * sin( 2*(12+(25/60)) * 2*pi * t) + # 24 hour frequency, amplitude 1
#' 50 * sin( ( 1/28 ) * 2*pi * t) + # monthly frequency, amplitude 0.2
#' 10 * sin( ( 1/365 ) * 2*pi * t) # yearly frequency, amplitude 0.1
#' inputdata <- df
#' inputdata <- cbind(inputdata, inputnoise)
#' df$data <- df$data + inputnoise
#'
#' # change to hourly
#' hourlydf <- changeInterval(df, Interval = "Hourly", option="inst", offset = 30)
#' # plot raw data
#' rawPlot <- ggplot(hourlydf, aes(x=Date))+
#'   geom_line(aes(y = Inst), color = "black")
#'
#' # filter out above and below 16 to 25 hours
#' emdFilteredShort <- ceemdanFilter(hourlydf$Date, hourlydf$Inst,  output = 1, d1=16, d2=25)
#' emdFilteredShortMelt <- melt(emdFilteredShort, id="time")
#' names(emdFilteredShortMelt)[2] <- "Frequency"
#' emdFilteredPlotShort <- ggplot(emdFilteredShortMelt, aes(x=time, y=value, colour=Frequency)) +
#'   geom_line()
#'
#' # filter out above and below 1000 to 3000 hours
#' emdFilteredLong <- ceemdanFilter(hourlydf$Date, hourlydf$Inst, output = 1, d1=360, d2=1500)
#' emdFilteredLongMelt <- melt(emdFilteredLong, id="time")
#' names(emdFilteredLongMelt)[2] <- "Frequency"
#' emdFilteredPlotLong <- ggplot(emdFilteredLongMelt, aes(x=time, y=value, colour=Frequency)) +
#'   geom_line()
#' RawAndAddedNoise <- ggplot(df, aes(x=Time, y=data)) +
#'   geom_line(colour="grey") +
#'   geom_line(data = inputdata, aes(x = Time, y = data),  colour="blue")
#'
#' # output raw (added noise data), with the 16/25 ceemdan, and 360/1500 ceemdan
#' ggarrange(rawPlot, emdFilteredPlotShort, emdFilteredPlotLong, nrow=3, ncol=1)
#'
#' RawAndDenoised <- ggplot(inputdata, aes(x=Time, y=data)) +
#'   geom_line(colour="grey") +
#'   geom_line(data = emdFilteredLong, aes(x = time, y = above1500),  colour="red") +
#'   geom_line(data = emdFilteredShort, aes(x = time, y = above25),  colour="blue")
#'
#' # output raw data, with the 1500 hour residual in red and 26 hour residual in blue
#' RawAndDenoised
#' # raw (added noise data), with raw input data (blue - no noise added), and ceemdan denoised (red)
#' rawPlot +
#'   geom_line(data = inputdata, aes(x = Time, y = data),  colour="blue") +
#'   geom_line(data = emdFilteredShort, aes(x = time, y = above25),  colour="red")
#' @export
#'
ceemdanFilter <- function(time, data, output=0, method="ceemdan", d1 = 16, d2 = 25)
{

  GetPeriods <- function(imf){

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
    summedcols <- as.data.frame(summedcols)
    names(summedcols) <- c(paste("below",a,sep=""), paste("within",a,"-",b,sep=""), paste("above",b,sep="")  )
    return (summedcols)

  }

  if(method=="ceemdan")
  {
    #imfs[,1]
    imfs <- ceemdan(data) # input an hourly vector
  }else if(method=="emd")
  {
    imfs2 <- EMD::emd(data, as.numeric(time) )
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
  #timestamp <- as.data.frame(time)
  TypesOut <- data.frame(time, signif(TideTypes, digits=4))

  outputList <- list()
  outputList[["Periods"]] <- Periods
  outputList[["imfs"]] <- imfs
  outputList[["TypesOut"]] <- TypesOut

  if (output==1)
  {
    return(outputList[["TypesOut"]] )
  }else if(output==2)
  {
    return(outputList[["Periods"]])
  }else if(output==3)
  {
    return(outputList[["imfs"]])
  }
  else{
    return(outputList)
  }
}
