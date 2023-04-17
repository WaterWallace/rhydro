#' Calculate wetted perimeter for a particular height
#'
#' @param xs dataframe, with columns "chain" and "rl".
#' @param level Numeric
#'
#' @return value
#'
#' @examples
#' xs <- data.frame(chain = c(1,2,3,4,5,6,7),
#'     rl = c(10,9,7,4,8,9,11)
#' )
#' wettedPerim(xs, 1)
#' wettedPerim(xs, 8)
#' wettedPerim(xs, 11)
#' wettedPerim(xs, 20)
#'
#' @export
#'
wettedPerim <- function(xs, level)
{

  if(level < min(xs[,2])) return(0)
  xs <- interpSeries(xs,level)
  #xs <- xs[!(xs[,2] > level),]
  xs[(xs[,2] > level),] <- NA

  chain <- xs[,1]
  rl <- xs[,2]

  chain2 <- c(NA,chain)
  rl2 <- c(NA,rl)
  chain <- c(chain,NA)
  rl <- c(rl,NA)

  df <- data.frame(chain2,rl2,chain,rl)
  df <- na.omit(df)

  wettedperim <- sum(sqrt(
    abs(df$chain2-df$chain)^2+
      abs(df$rl2-df$rl)^2)
  )
  return(wettedperim)

}
