#' Applies Mannings equation across a range of heights
#'
#' Adds discharge to a discharge summary
#'
#' @param xssummary datamframe, output from xsSummary()
#' @param slope numeric, flood slope of river
#' @param n numeric, roughness coefficient of river
#'
#' @return dataframe of xs summary with added discharge
#'
#' @examples
#' xs <- data.frame(chain = c(0,20,30,40,50,60,70),
#'     rl = c(10,9,7,4,8,9,11)
#' )
#' summary <- xsSummary(xs, by = 1)
#' manningsEqn(summary, n=0.05, s=0.0015)
#'
#' @export
#'
manningsEqn <- function(xssummary, slope=0.0015, n=0.05)
{
  xssummary <- na.omit(xssummary)
  a <- xssummary[,2]
  r23 <- xssummary[,4]^(2/3)
  s05 <- slope^0.5

  return(xssummary %>% mutate(q = (a*r23*s05)/n))
}
