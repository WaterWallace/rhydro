#' Calculate cross sectional area for a a range of heights
#'
#' Applies calcArea at intervals to produce a lookup table
#'
#' @param xs dataframe, with columns "chain" and "rl".
#' @param interval Numeric for the interval between height steps
#' @param window vector with a length of 2 with the minimum and maximum values
#'
#' @return dataframe of height and area
#'
#' @examples
#' xs <- data.frame(chain = c(1,2,3,4,5,6,7),
#'     rl = c(10,9,7,4,8,9,11)
#' )
#' # default
#' areaLookup(xs)
#' # specify window
#' areaLookup(xs, window = c(7,9))
#' # 1m interval
#' areaLookup(xs, interval = 1)
#' @export
#'
areaLookup <- function(xs, interval = 0.1, window=c(0,0))
{

  names(xs) <- c("chain", "rl")

  if(window[1] == 0 & window[2] == 0 )
  {
    window<- c(min(xs$rl), max(xs$rl)*1.5 )
  }

  if(interval == 0)
  {
    rlseq <-  unique( c(window[1], window[2], xs$rl ) )
  }else{
    rlseq <- unique( c( window[1],
                        window[2],
                        seq(window[1],window[2],by=interval),
                        xs$rl)
    )

    rlseq <- rlseq [ rlseq %>% between(window[1], window[2]) ] # limit by range window
  }



  xsLookup <- list()
  count <- 0
  for( i in rlseq )
  {
    count <- count+1
    xsLookup[[count]] <- data.frame(height = i, area = calcArea(xs, i))
  }
  xsLookup <- do.call(rbind,xsLookup)
  xsLookup <- xsLookup[order(xsLookup$height),]

}
