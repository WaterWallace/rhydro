#' Calculate cross section stats for a range of heights
#'
#' Area, wetted perimeter, hydraulic radius
#'
#' @param xs dataframe, with columns "chain" and "rl".
#' @param from numeric, start gauge height range
#' @param to numeric, end gauging height range
#' @param by numeric, step increment for summary range
#'
#' @return dataframe of gauge heights and cross section stats
#'
#' @examples
#' xs <- data.frame(chain = c(1,2,3,4,5,6,7),
#'     rl = c(10,9,7,4,8,9,11)
#' )
#' xsSummary(xs)
#' xsSummary(xs, by = 1)
#' xsSummary(xs, from = 6, to = 9, by = 0.05)
#'
#' @export
#'
xsSummary <- function(xs, from = -1, to = -1, by = 0.1 )
{

  if (from == -1) from <- min(xs[,2])
  if (to == -1) to <- max(xs[,2])

  levels <- seq(from, to, by=by)

  xssummary <- list()
  for(i in 1:length(levels))
  {
    level <- levels[i]
    perim <- wettedPerim(xs, level)
    area <- calcArea(xs, level)

    level <- round(level,3)
    perim <- round(perim,3)
    area <- round(area,3)

    xssummary[[i]] <- data.frame(Stage = level, Area = area, W.Perim = perim, H.Rad = round(area/perim,3) )
  }
  xssummary <- do.call(rbind,xssummary)
  return(xssummary)

}
