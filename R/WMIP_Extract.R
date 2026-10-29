#' Extract time-series data from the Queensland Water Monitoring Information Portal (WMIP)
#'
#' Retrieves time-series observations from the Queensland Water Monitoring
#' Information Portal (WMIP) web service and returns the result as a tibble.
#'
#' The function is a convenience wrapper around the WMIP
#' `get_ts_traces` API endpoint, supporting common hydrologic and water
#' quality variables, including water level, discharge, rainfall,
#' conductivity, temperature, pH, and turbidity. The WMIP API provides
#' both archived and telemetry data, with optional aggregation over
#' configurable reporting intervals. The function returns only values with
#' quality codes less than 150; all other values are replaced with `NA`. 【1-ee8cee】
#'
#' @param WMIPID Character string containing a WMIP site identifier.
#'   For example, `"143001C"`.
#'
#' @param START Character string specifying the start date/time in WMIP
#'   format (`YYYYMMDDHHMMSS` or `YYYYMMDD`).
#'
#' @param param Character string specifying the variable to retrieve.
#'   Supported values are:
#'   \describe{
#'     \item{`"level"`}{Gauge height (stage), variable 100.00.}
#'     \item{`"discharge"`}{Rated stream discharge derived from stage
#'       (100.00 \eqn{\rightarrow} 140.00), returned in m³/s.}
#'     \item{`"preservedQ"`}{Stored discharge values (140.00).}
#'     \item{`"rainfall"`}{Rainfall (10.00), mm.}
#'     \item{`"temperature"`}{Water temperature (2080.00), °C.}
#'     \item{`"conductivity"`}{Electrical conductivity (2010.00), µS/cm.}
#'     \item{`"pH"`}{pH (2100.00).}
#'     \item{`"turbidity"`}{Turbidity (2030.00), NTU.}
#'   }
#'   Variable mappings are based on WMIP standard time-series variables. 【1-ee8cee】
#'
#' @param datasource Character string specifying the WMIP datasource.
#'   Default is `"AT"`.
#'   Supported WMIP datasource types include:
#'   \describe{
#'     \item{`"A"`}{Archive (quality-controlled historical data).}
#'     \item{`"TE"`}{Telemetry data.}
#'     \item{`"AT"`}{Combined archive and telemetry data.}
#'     \item{`"ATQ"`}{Combined archive and telemetry discharge data.}
#'   } 【1-ee8cee】
#'
#' @param END Character string specifying the end date/time in WMIP
#'   format (`YYYYMMDDHHMMSS` or `YYYYMMDD`).
#'   Defaults to tomorrow's date.
#'
#' @param type Aggregation method applied to the retrieved data.
#'   One of:
#'   `"max"`, `"min"`, `"mean"`, `"end"`, `"tot"`, `"cum"`,
#'   `"inst"`, or `"point"`.
#'
#' @param interval Aggregation interval. One of:
#'   `"year"`, `"month"`, `"day"`, `"hour"`, `"minute"`,
#'   or `"second"`.
#'
#' @param report Determines whether timestamps for aggregated values
#'   represent the start or end of the reporting interval.
#'   Either `"start"` or `"end"`. WMIP reports aggregated values at
#'   the start of a period by default. 【1-ee8cee】
#'
#' @param httpTimeout Numeric HTTP request timeout in seconds.
#'   Default is `30`.
#'
#' @return A tibble containing:
#'   \describe{
#'     \item{site}{WMIP site identifier.}
#'     \item{varname}{Human-readable variable name returned by WMIP.}
#'     \item{var}{WMIP variable code.}
#'     \item{time}{Observation timestamp as `POSIXct`
#'       (`Etc/GMT-10`).}
#'     \item{value}{Observed or aggregated value.}
#'     \item{quality}{WMIP quality code.}
#'   }
#'
#'   If the request fails or a non-200 HTTP response is received,
#'   a single-row tibble containing `"error"` in `var` is returned.
#'
#' @details
#' WMIP site identifiers correspond to monitoring stations published by the
#' Queensland Government. For example, site `"143001C"` represents the
#' Rockhampton Gauge on the Fitzroy River.
#'
#' The function constructs a WMIP `get_ts_traces` request and parses the
#' JSON response into a tidy tabular format. Aggregation behaviour depends
#' on the selected combination of `type`, `interval`, and `report`
#' parameters. 【1-ee8cee】
#'
#' @examples
#' \donttest{
#' # Hourly water level data
#' level_data <- WMIP_Extract(
#'   WMIPID = "143001C",
#'   START = "20250101"
#' )
#'
#' # Hourly discharge (m3/s)
#' discharge_data <- WMIP_Extract(
#'   WMIPID = "143001C",
#'   START = "20250101",
#'   param = "discharge"
#' )
#'
#' # Daily rainfall totals
#' rainfall_data <- WMIP_Extract(
#'   WMIPID = "143001C",
#'   START = "20250101",
#'   param = "rainfall",
#'   type = "tot",
#'   interval = "day"
#' )
#'
#' # Daily mean water temperature
#' temp_data <- WMIP_Extract(
#'   WMIPID = "143001C",
#'   START = "20250101",
#'   param = "temperature",
#'   type = "mean",
#'   interval = "day"
#' )
#' }
#' @references
#' Queensland Government Water Monitoring Information Portal (WMIP) API
#' Documentation, Version 2.1 (3 October 2025). 【1-ee8cee】
#'
#' @export
WMIP_Extract <- function (WMIPID, START, param = "level", datasource = "AT",
                          END = format(Sys.Date() + 1, "%Y%m%d"),
                          type = "mean", interval = "hour", report = "start", httpTimeout = 30) {

  stopifnot("type must be max, min, mean, end, tot, cum, inst, point" =
              type %in% c( "max", "min", "mean", "end", "tot", "cum", "inst", "point" ) )

  stopifnot("interval must be year, month, day, hour, minute, second" =
              interval %in% c( "year", "month", "day", "hour", "minute", "second" ) )

  stopifnot("report must be start or end" =
              report %in% c( "start", "end") )

  param <- dplyr::case_when(
    param == "level" ~ "varfrom=100.00&varto=100.00",
    param == "discharge" ~ "varfrom=100.00&varto=140.00",
    param == "preservedQ" ~ "varfrom=140.00&varto=140.00",
    param == "rainfall" ~ "varfrom=10.00&varto=10.00",
    param == "temperature" ~ "varfrom=2080.00&varto=2080.00",
    param == "conductivity" ~ "varfrom=2010.00&varto=2010.00",
    param == "pH" ~ "varfrom=2100.00&varto=2100.00",
    param == "turbidity" ~ "varfrom=2030.00&varto=2030.00"
  )

  WMIP_URL <- paste("https://water-monitoring.information.qld.gov.au/cgi/webservice.pl?function=get_ts_traces&site_list=",
                    WMIPID, "&datasource=", datasource, "&",
                    param, "&start_time=", START, "&end_time=",
                    END, "&data_type=",type,"&interval=",interval,
                    "&report_time=",report,"&multiplier=1", sep = "")


  #jsondata$content

  jsondata <- tryCatch(
    {
      httr::GET(url = WMIP_URL, timeout = httpTimeout)
    },
    error = function(e){
      message("Error:")
      print(e)
      return(NULL)
    },
    finally = {

    }
  )



  if ( is.null(jsondata) ) {

    WMIPData <- tibble::tibble("site" = WMIPID,
                               "varname" = param,
                               "var" = "error",
                               "time" = NA,
                               "value" = NA,
                               "quality" = NA,
    )
    return(WMIPData)

  }

  if(jsondata$status_code != 200) {
    message(paste("Bad status code:", jsondata$status_code ) )
    WMIPData <- tibble::tibble("site" = WMIPID,
                               "varname" = param,
                               "var" = "error",
                               "time" = NA,
                               "value" = NA,
                               "quality" = NA,
    )
    return(WMIPData)
  }

  jsonout <- jsonlite::fromJSON(jsondata$content %>% rawToChar)

  WMIPData <- tibble::tibble("site" = WMIPID,
                             "varname" = jsonout$return$traces$varto_details$name,
                             "var" = jsonout$return$traces$varto_details$variable,
                             "time" = jsonout$return$traces$trace[[1]]$t  %>% format(scientific = FALSE) %>% as.POSIXct(format="%Y%m%d%H%M%S", tz="Etc/GMT-10"),
                             "value" = jsonout$return$traces$trace[[1]]$v,
                             "quality" = jsonout$return$traces$trace[[1]]$q,
  )

  WMIPData <- WMIPData %>% mutate(value = as.numeric(value),
                                  quality = as.numeric(quality)
  )

  return( WMIPData %>%
            mutate(value = ifelse(quality < 150, value, NA)) )
}
