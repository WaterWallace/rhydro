
if(1 == 0)
{
library(httr)
library(dplyr)
# Daintree village
url1 <- "http://www.bom.gov.au/fwo/IDQ65394/IDQ65394.531110.tbl.shtml"
#Barratt Creek Daintree
"http://www.bom.gov.au/fwo/IDQ65394/IDQ65394.531132.plt.shtml"


# Plane Creek at Sucrogen
url1 <- "http://www.bom.gov.au/fwo/IDQ65393/IDQ65393.533143.tbl.shtml"

#strsplit(rawToChar( res$content ), "\n")
scrapeBOM <- function(url1)
{

  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0"
  res   <- GET(url1, add_headers(`Connection` = "keep-alive", `User-Agent` = UA))

  body <- strsplit(rawToChar( res$content ), "<tbody>")
  body <- strsplit(body[[1]][[2]], "</tbody>")
  tablerows <- strsplit(body[[1]][[1]], "</tr>")

  rowdata <- list()
  rownum <- 0
  for(row in tablerows[[1]])
  {
    columns <- strsplit(row, "\n")

    cols <- strsplit(row, "<td align=left>")
    cols <- strsplit(cols[[1]][length(cols[[1]])], "<td align=right>")


    element <- 0
    data <- list()
    for(col in cols[[1]])
    {
      data[[    element <- element+1]] <- strsplit(col, "</td>\n")[[1]]
    }

    data <- do.call(cbind,data)

    if(length(data) == 2){
      rowdata[[  rownum <- rownum + 1]] <- data
    }

  }
  sitedata <- do.call(rbind, rowdata)
  sitedata <- as.data.frame(sitedata)

  sitedata$V1 <- as.POSIXct(sitedata$V1, format = "%d/%m/%Y %H:%M")
  sitedata$V2 <- as.numeric(sitedata$V2)

  plot(sitedata)

  return(distinct(sitedata, V1, .keep_all = TRUE)
  )



}

data
data <- scrapeBOM("http://www.bom.gov.au/fwo/IDQ65394/IDQ65394.531110.tbl.shtml")
data <- scrapeBOM("http://www.bom.gov.au/fwo/IDQ65393/IDQ65393.533143.tbl.shtml")
tail(data)
data$V1 <- as.POSIXct(data$V1, tz = "Australia/Sydney")



plot()

data$diff <- c(0,diff(data$V1))
data
plot(data$V1)
diff(data$V1)



scrape2influx <- function(url1, gsnumber, token = token)
{
  bomdata <- scrapeBOM(url1)

  data <- data.frame(time = bomdata$V1,
                     value = bomdata$V2,
                     QC = as.integer(140),
                     GSNumber = gsnumber,
                     name = "TSDB",
                     var = "Level")

  #token <- "" # don't save the token in this file
  client <- InfluxDBClient$new(url = "https://eastus-1.azure.cloud2.influxdata.com",
                               token = token,
                               org = "stephen.wallace@des.qld.gov.au",
                               retryOptions = TRUE)


  tail(data)
  print("influx import")
  ## Send to influx
  client$write(data,bucket = "tsdata3", precision = "s",
               measurementCol = "GSNumber",                 # key
               tagCols = c("name","var"),         # identifiers
               fieldCols = c("value", "QC"),                      # data
               timeCol = "time")                            # timestamp

}


scrape2influx("http://www.bom.gov.au/fwo/IDQ65394/IDQ65394.531132.plt.shtml", "B531132", token ) #barrat
scrape2influx("http://www.bom.gov.au/fwo/IDQ65394/IDQ65394.531110.tbl.shtml", "B531110", token ) #daintree village

"http://www.bom.gov.au/fwo/IDQ65394/IDQ65394.531132.plt.shtml"


}
