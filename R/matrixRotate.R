#' Rotate x,y,z coordinates around an axis
#'
#' @param m dataframe, with columns "x", "y", "z"
#' @param deg degrees to rotate (anticlockwise)
#' @param rotateaxis char, axis to rotate around, "x", "y" or "z"
#' @param origin vector (or dataframe), length 3, origin to rotate around, default 0,0,0
#'
#' @return dataframe, rotated
#'
#' @examples
#' m <- data.frame(x = c(2.5,2,3,1), y = c(3,5,10,15), z = c(3,2,1,3))
#' # plotting y ( chainage ) against x (alignment)
#' plot(y ~ x, m, xlim = c( min(m$x, m$y) - max( m$x, m$y ) - 10 , max(m$x, m$y)*2 + 10 ) ,
#'      ylim =c( min(m$x, m$y) - max( m$x, m$y ) - 10 , max(m$x, m$y)*2 + 10 ))
#' abline (1,0)
#' abline (v = 0)
#'
#' # points are rotated around the z (elevation) axis
#' rotated <- matrixRotate(m,15, rotateaxis = "z")
#' points(y ~ x, rotated, col="orange")
#' rotated <- matrixRotate(m,45, rotateaxis = "z")
#' points(y ~ x, rotated, col="red")
#' rotated <- matrixRotate(m,180, rotateaxis = "z")
#' points(y ~ x, rotated, col="blue")
#'
#'
#' # plotting z (elevation) against y (chainage)
#' plot(z ~ y, m, xlim = c( min(m$x, m$z) - max( m$x, m$z ) - 10 , max(m$x, m$z)*2 + 10 ) ,
#'      ylim =c( min(m$x, m$z) - max( m$x, m$z ) - 10 , max(m$x, m$z)*2 + 10 ))
#' abline (1,0)
#' abline (v = 0)
#'
#' rotated <- matrixRotate(m,5, rotateaxis = "x")
#' points(z ~ y, rotated, col="orange")
#' rotated <- matrixRotate(m,10, rotateaxis = "x")
#' points(z ~ y, rotated, col="red")
#' rotated <- matrixRotate(m,15, rotateaxis = "x")
#' points(z ~ y, rotated, col="blue")
#'
#'
#' # plotting z (elevation) against y (chainage)
#' plot(y ~ x, m, xlim = c( min(m$x, m$z) - max( m$x, m$z ) - 10 , max(m$x, m$z)*2 + 10 ) ,
#'      ylim =c( min(m$x, m$z) - max( m$x, m$z ) - 10 , max(m$x, m$z)*2 + 10 ))
#' abline (1,0)
#' abline (v = 0)
#'
#' # Rotate around a specific origin, rather than default (0,0,0)
#' rotated <- matrixRotate(m,15, rotateaxis = "z", origin = m[1,])
#' points(y ~ x, rotated, col="orange")
#' @export
#'
#'
matrixRotate <- function(m, deg = 0, rotateaxis = "z", origin = c(0,0,0))
{
  stopifnot("origin should be length 3" = length(origin) == 3 )
  stopifnot("rotateaxis should be x, y or z" = (rotateaxis == "x" |rotateaxis == "y" |rotateaxis == "z"))
  stopifnot("deg can't be a vector" = (length(deg) > 1))

  meta <- m %>% select(-c(x,y,z))
  origin <- unlist(origin)
  m <- m %>% select(c(x,y,z))
  m <- m %>% matrixTranslate(-origin) %>%
    t %>%
    as.matrix
  rads <- deg2rad(deg)

  #Get transformation matrix
  m <- switch(rotateaxis,
              "x" = matrix(c(1,0,0,0,cos(rads),sin(rads),0,-sin(rads),cos(rads)), nrow=3),
              "y" = matrix(c(cos(rads),0,-sin(rads),0,1,0,sin(rads),0,cos(rads)), nrow=3),
              "z" = matrix(c(cos(rads),sin(rads),0, -sin(rads),cos(rads),0,0,0,1), nrow=3)
  ) %*% m %>% t %>% as.data.frame # Multily transform by input matrix
  names(m) <- c("x","y","z")

  return( matrixTranslate(m, origin) %>%  cbind(meta) )
}





