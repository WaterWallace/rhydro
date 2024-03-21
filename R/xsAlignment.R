#' Rotate x,y,z coordinates around an axis
#'
#' @param xs dataframe, columns required x, y, z, rpid (+optional meta)
#' @param rp data.frame, columns required x, y, z, rpid (+optional meta)
#' @param originRP string, name of primary reference point (that new xs is shifted to)
#' @param alignRP string, name of secondary reference point ( that determiens alignment angle )
#'
#' @return dataframe, rotated
#'
#' @examples
#'
#' #DRL <- getXSSurvey(client, "1080025", bucket = "tsdata3")
#' load("data/DRLXS.rda") # example site
#'
#' # align DRL points to RP's RP1 and RP2 for a specific cross section
#' aligned <- xsAlignment(DRL$points[[2]],
#'                        DRL$RP,
#'                        originRP = "RP1",
#'                        alignRP = "RP2")
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
xsAlignment <- function(xs, rp, originRP, alignRP)
{
  origin.new <- xs %>% dplyr::filter(rpid == originRP) # surveyed primary benchmark
  align.new <- xs %>% dplyr::filter(rpid == alignRP) # surveyd second point to adjust angle
  origin.RP <- rp %>% dplyr::filter(rpid == originRP) # stored primary benchmark
  align.RP <- rp %>% dplyr::filter(rpid == alignRP) # stored second benchmark

  # get adjustment angle against RP's
  angle <- rpAlign(origin.new,
                   align.new,
                   origin.RP,
                   align.RP)

  # rotate to original alignment
  rotated <- matrixRotate(xs, angle)

  # translate to stored primary benchmark
  rotated.origin <- rotated %>% dplyr::filter(rpid == originRP)
  matrixTranslate(rotated, translation = c(
    origin.RP$x - rotated.origin$x,
    origin.RP$y - rotated.origin$y,
    origin.RP$z - rotated.origin$z)
  )

}



