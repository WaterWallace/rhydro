#' Translate x,y,z coordinates by offset
#'
#' @param m dataframe, with columns "x", "y", "z"
#' @param translation vector, length 3, 1=x, 2=y, 3=z
#'
#' @return dataframe, translated input dataframe
#'
#' @examples
#' m <- data.frame(x = c(2.5,2,3,1), y = c(3,5,10,15), z = c(3,2,1,3))
#' # plotting y ( chainage ) against x (alignment)
#' plot(y ~ x, m, xlim = c( min(m$x, m$y) - max( m$x, m$y ) - 10 , max(m$x, m$y)*2 + 10 ) ,
#'    ylim =c( min(m$x, m$y) - max( m$x, m$y ) - 10 , max(m$x, m$y)*2 + 10 ))
#' abline (1,0)
#' abline (v = 0)
#' points(y ~ x, matrixTranslate(m, c(10,5,0)), col="red")
#'
#' @export
#'
matrixTranslate <- function(m, translation = c(0,0,0))
{

  stopifnot("translation should be length 3" = length(translation) == 3 )
  translation <- unlist(translation)

  meta <- m %>% dplyr::select(-c(x,y,z))
  m <- m %>% dplyr::select(c(x,y,z))
  n <- names(m)
  m <- as.matrix(t(m))
  m <- rbind(m,a = 1)

  translate <- matrix(c(1,0,0,0,
                        0,1,0,0,
                        0,0,1,0,
                        translation[1],translation[2],translation[3],1), nrow=4)

  m <- translate %*% m
  m <- m %>% t %>% as.data.frame
  names(m) <- n
  return( m[,-4] %>% cbind(meta))

}
