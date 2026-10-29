#' Extract historic time-series data from eagle.io
#'
#' Retrieves historic observations from an eagle.io Parameter node using the
#' eagle.io Historic API and returns the results as a tibble.
#'
#' The function requests data from the `/historic` endpoint, extracts the
#' parameter values and quality codes, and converts timestamps to
#' `POSIXct` objects. Timestamps are requested in UTC and returned in the
#' `Australia/Brisbane` timezone. The eagle.io Historic API supports
#' extraction of time-series data over a specified time range using a node
#' identifier and API key authentication. 【1-7646b6】【2-62cc09】
#'
#' @param APIKEY Character string containing a valid eagle.io API key.
#'   The key is supplied in the `X-Api-Key` HTTP header for authentication.
#'   【2-62cc09】
#'
#' @param param Character string containing the eagle.io Parameter node ID.
#'   This may be either an eagle.io node identifier or a custom identifier
#'   supported by the Historic API. 【1-7646b6】
#'
#' @param START POSIXct start date/time for data extraction.
#'   Converted internally to ISO8601 format (`YYYY-MM-DDTHH:MM:SSZ`).
#'
#' @param END POSIXct end date/time for data extraction.
#'   Converted internally to ISO8601 format (`YYYY-MM-DDTHH:MM:SSZ`).
#'   Defaults to the current system time.
#'
#' @return A tibble containing:
#'   \describe{
#'     \item{ts}{Timestamp of the observation.}
#'     \item{Value}{Recorded value.}
#'     \item{Quality}{eagle.io quality code, where available.}
#'     \item{name}{Parameter name from eagle.io metadata.}
#'     \item{units}{Engineering units associated with the parameter.}
#'   }
#'
#' @details
#' This function is a wrapper around the eagle.io Historic API endpoint:
#'
#' `https://api.eagle.io/api/v1/historic/`
#'
#' Data are requested using:
#'
#' * `startTime`
#' * `endTime`
#' * `qualityExcluded=NONE`
#'
#' The response metadata is used to populate the `name` and `units`
#' columns in the returned tibble. Historic API requests require
#' ISO8601 timestamps and authenticated requests using an API key.
#' 【1-7646b6】【2-62cc09】
#'
#' @examples
#' \donttest{
#' # Extract one day of data from a parameter node
#' eio_data <- EIO_Extract(
#'   APIKEY = "your_api_key",
#'   param = "eio_node",
#'   START = as.POSIXct(
#'     "2025-01-01 00:00:00",
#'     tz = "Australia/Brisbane"
#'   ),
#'   END = as.POSIXct(
#'     "2025-01-02 00:00:00",
#'     tz = "Australia/Brisbane"
#'   )
#' )
#'
#' head(eio_data)
#'
#' # Extract the last 7 days of data
#' weekly_data <- EIO_Extract(
#'   APIKEY = "your_api_key",
#'   param = "eio_node",
#'   START = Sys.time() - 7 * 24 * 60 * 60
#' )
#' }
#'
#' @references
#' eagle.io HTTP API Documentation:
#' https://docs.eagle.io/en/latest/api/
#' 【1-7646b6】【2-62cc09】
#'
#' @export
EIO_Extract <- function (APIKEY, param, START, END = Sys.time() )
{
  attr(START, "tzone") <- "UTC"
  attr(END, "tzone") <- "UTC"

  START <- START %>% format("%Y-%m-%dT%H:%M:%SZ")
  END <- END %>% format("%Y-%m-%dT%H:%M:%SZ")

  URLData <- paste("https://api.eagle.io/api/v1/historic/?params=",
                   param, "&startTime=", START, "&endTime=", END, "&qualityExcluded=NONE",
                   sep = "")
  APIData <- httr::GET(URLData, httr::add_headers(`X-Api-Key` = APIKEY,
                                                  `Content-Type` = "application/json"))
  Node_content = jsonlite::fromJSON(rawToChar(APIData$content))

  vartype <- Node_content$header$columns$`0`$name
  varunits <- Node_content$header$columns$`0`$units

  Data <- bind_cols(
    ts = Node_content[["data"]][["ts"]],
    Node_content[["data"]][["f"]][["0"]]
  ) %>%
    mutate(v = ifelse ("v" %in% names(.),v,NA),
           q = ifelse ("q" %in% names(.),q,NA)) %>%
    rename('Value' = 'v', 'Quality' = 'q') %>%
    mutate(name = vartype,
           units = varunits
    )

  Data$ts <- as.POSIXct(Data$ts, format = "%Y-%m-%dT%H:%M:%S",
                        tz = "UTC")
  attr(Data$ts, "tzone") <- "Australia/Brisbane"


  return(Data)
}
