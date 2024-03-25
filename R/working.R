
if(1==0)
{

getXSSurvey <- function(client, gsnumber, bucket, latest = TRUE)
{
  outputlist <- list()

  q.points <- paste('from(bucket: "',bucket,'")
  |> range(start: 2010-01-01)
  |> drop(columns: ["_start", "_stop"])
  |> filter(fn: (r) => r["_measurement"] == "',gsnumber,'")
  |> filter(fn: (r) => r["name"] == "XSDB")
  |> filter(fn: (r) => r["xsid"] != "RP")
  |> pivot(rowKey:["_time"], columnKey:["_field"], valueColumn:"_value")
  |> group(columns: ["measurement", "xsid"])', sep="")

  q.rp <- paste('from(bucket: "',bucket,'")
  |> range(start: 2010-01-01)
  |> drop(columns: ["_start", "_stop"])
  |> filter(fn: (r) => r["_measurement"] == "',gsnumber,'")
  |> filter(fn: (r) => r["name"] == "XSDB")
  |> filter(fn: (r) => r["xsid"] == "RP")
  |> pivot(rowKey:["_time"], columnKey:["_field"], valueColumn:"_value")
  |> group(columns: ["measurement"])', sep="")

  q.points <- gsub("\r?\n|\r", " ", q.points)
  q.rp <- gsub("\r?\n|\r", " ", q.rp )

  outputlist[["RP"]] <- client$query(q.rp, POSIXctCol=NULL)
  outputlist[["points"]] <- client$query(q.points, POSIXctCol=NULL)

  if(!is.null(outputlist[["RP"]]))
  {
    outputlist[["RP"]] <- do.call(rbind, outputlist[["RP"]])
    names(outputlist[["RP"]]) <- names(outputlist[["RP"]]) %>% gsub("_time", "time", .)

    if(latest)
    {
      outputlist[["RP"]] <- outputlist[["RP"]]  %>% arrange(desc(time)) %>%
        distinct(rpid, .keep_all = TRUE)
    }

    # change y to double
    outputlist$RP$y <- outputlist$RP$y %>% as.double
  }
  # change y to double
  outputlist$points <- outputlist$points %>% lapply(function(x) {
    x$y <- x$y %>% as.double
    return(x)
  })

  return(outputlist)

}

DRL <- getXSSurvey(client, "1080025", bucket="tsdata3")
DRL$points[[1]]$rpid

DRL$RP

# writes an aligned RP to db
influxUpdateRP <- function(client, aligned, RP, bucket )
{

  bm <- aligned %>% dplyr::filter(rpid %in% RP)
  bm$xsid <- "RP"

  client$write(bm,bucket = bucket, precision = "s",
               measurementCol = "_measurement" ,
               tagCols = c("name", "xsid", "rpid","y"),
               fieldCols = c("x","z","comment"),
               timeCol = "_time")

}

influxUpdateXS <- function(client, timestamp = Sys.time(), gsnumber, xsid, xs, bucket)
{

  xs$name = "XSDB"
  xs$xsid = xsid
  xs$GSNumber = gsnumber
  xs$time <- timestamp

  rp <- xs %>% dplyr::filter(rpid != "")
  xs <- xs %>% dplyr::select(-rpid)

  xs$chainid <- seq(1, nrow(xs))*10

  head(xs)

  client$write(xs,bucket = bucket, precision = "s",
               measurementCol = "GSNumber" ,
               tagCols = c("name", "xsid", "chainid","y"),
               fieldCols = c("x","z","comment"),
               timeCol = "time")

  client$write(rp,bucket = bucket, precision = "s",
               measurementCol = "GSNumber" ,
               tagCols = c("name", "xsid", "rpid","y"),
               fieldCols = c("x","z","comment"),
               timeCol = "time")

}

xs <- data.frame(x = c(0.127,0.360,0.23,0.205,0.217,0.288,0.364,0.328,0.088,0.637,1.385),
                 y = c(1.723,2.09,2.194,4.596,4.235,3.793,3.591,3.029,2.337,1.135,-2.189),
                 z = c(0.623,1.593,0.588,-2.192,-1.569,-0.821,-0.136,0.005,0.239,0.715,0.698),
                 comment = c("mid pit",
                             "2nd d/s post",
                             "",
                             "wl@1026",
                             "timber",
                             "",
                             "",
                             "",
                             "bottom timber",
                             "",
                             "EOS rb"))
xs <- cbind(xs, rpid = c("RP1","RP2", rep("", nrow(xs)-2 )))

influxUpdateXS(client, gsnumber = "1080025", xsid = "TEST", xs = xs, bucket = "test")

# imported from influx
influxUpdateRP(client, aligned[[2]], "RP3", "tsdata3")

testxs <- getXSSurvey(client, "1080025", bucket = "test")
testxs$points
DRL$RP

is.null(testxs$RP)

}


if(1 == 0)
{
  #shinyapp

  xs # xs to import
  influxUpdateXS(client, "1080025", xsid = "TEST", xs = xs, bucket = "test")

  #get recently imported survey
  newxs <- getXSSurvey(client, "1080025", "test")

  # align
  aligned <- xsAlignment(newxs$points[[1]], newxs$RP, "RP1", "RP2", angle = 180, origin = c(0,1500,10) )

  # import RP's
  aligned <- aligned %>% dplyr::filter(rpid != "")

  # split
  aligned <- aligned %>% split(aligned$rpid)

  # import each RP
  for(rp in aligned){
    print(rp)
    influxUpdateRP(client, rp, rp$rpid, "test" )
  }


}

