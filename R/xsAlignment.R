#' Rotate x,y,z coordinates around an axis
#'
#' @param xs dataframe, columns required x, y, z, rpid (+optional meta)
#' Where multiple lines for the same rpid exist i.e. closing a cross section, it gets the mean
#' @param rp data.frame, columns required x, y, z, rpid (+optional meta)
#' @param originRP string, name of primary reference point (that new xs is shifted to)
#' @param alignRP string, name of secondary reference point ( that determiens alignment angle )
#' @param angle float, override angle
#' @param origin vector, length 3, override origin, 1=x, 2=y, 3=z
#'
#' @return dataframe, rotated
#'
#' @examples
#'
#' # NOT RUN #
#' #DRL <- getXSSurvey(client, "1080025", bucket = "tsdata3")
#' #
#' data(DRLXS) # example site
#'
#' head(DRL$RP)
#' head(DRL$points[[2]])
#'
#' # align DRL points to RP's RP1 and RP2 for a specific cross section
#' aligned <- xsAlignment(DRL$points[[2]],
#'                        DRL$RP,
#'                        originRP = "RP1",
#'                        alignRP = "RP2")
#'
#' head(aligned)
#'
#' # align all cross sections
#' aligned <- lapply(DRL$points, xsAlignment,
#'                   rp = DRL$RP,
#'                   originRP = "RP1",
#'                   alignRP = "RP2")
#'
#' @export
#'
#'
xsAlignment <- function(xs, rp = NULL, originRP, alignRP, angle = NULL, origin = NULL)
{
  meanCoords <- function(newCoords)
  {
    meta <- newCoords %>% dplyr::select(-c(x,y,z))
    coords <- newCoords %>% dplyr::select(c(x,y,z))
    cbind(meta[1,], coords %>% colMeans %>% t)
  }

  origin.new <- xs %>% dplyr::filter(rpid == originRP) %>% meanCoords # surveyed primary benchmark
  align.new <- xs %>% dplyr::filter(rpid == alignRP) %>% meanCoords # surveyd second point to adjust angle
  if(is.null(rp))
  {
    origin.RP <- xs %>% dplyr::filter(rpid == originRP) # stored primary benchmark
    align.RP <- xs %>% dplyr::filter(rpid == alignRP) # stored second benchmark
  }else{
    stopifnot("Not enough RP's" = nrow(rp) > 1 )
    origin.RP <- rp %>% dplyr::filter(rpid == originRP) %>% arrange(time) %>% tail(1) # stored primary benchmark
    align.RP <- rp %>% dplyr::filter(rpid == alignRP) %>% arrange(time) %>% tail(1) # stored second benchmark
  }

  #if no override angle
  if(is.null(angle))
  {
    # get adjustment angle against RP's
    angle <- rpAlign(origin.new,
                     align.new,
                     origin.RP,
                     align.RP)
  }

  # rotate to original alignment
  rotated <- matrixRotate(xs, angle)

  # override origin
  if(!is.null(origin)){
    origin.RP <- data.frame(x = origin[1], y = origin[2], z = origin[3])
  }

  # translate to stored primary benchmark
  rotated.origin <- rotated %>% dplyr::filter(rpid == originRP) %>% meanCoords()
  matrixTranslate(rotated, translation = c(
    origin.RP$x - rotated.origin$x,
    origin.RP$y - rotated.origin$y,
    origin.RP$z - rotated.origin$z)
  )

}
