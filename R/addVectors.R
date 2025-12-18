if(1==0)
{
deg2rad <- function(n) n/180*pi
rad2deg <- function(n) n/pi*180  #n/180*pi

mag2coord <- function(magnitude, direction) {
  x = magnitude * cos(direction)
  y = magnitude * sin(direction)
  return(data.frame(x, y))
}
coord2mag <- function(x, y)
{
  theta <- atan2(y, x)
  magnitude <-  x / cos(theta)
  data.frame(mag = magnitude, dir = rad2deg(theta) %% 360 )
}
addVectors <- function(vec1, vec2)
{
  polar1 <- mag2coord(vec1$mag, deg2rad(vec1$dir))
  polar2 <- mag2coord(vec2$mag, deg2rad(vec2$dir))
  coord2mag(x = polar1$x+polar2$x, y = polar1$y+polar2$y)
}

switchBearingMath <- function(bearing_deg) {
  (90 - bearing_deg) %% 360
}


library(dplyr)

# up the street
vec <- addVectors(vec1 = data.frame(mag = (20.002-4.67+4+16.4+19), dir = switchBearingMath( (227+07/60) - 180 ) ),
           vec2 = data.frame(mag = (20.925-5.96), dir = switchBearingMath( (300+01/60) - 180 )))
vec %>% mutate(dir = switchBearingMath(dir))


# mid point
vec <- addVectors(vec1 = data.frame(mag = 4.67, dir = switchBearingMath( 227+07/60) ),
                  vec2 = data.frame(mag = 4+24.423, dir = switchBearingMath( (317+59/60) )))

midpoint <- vec %>% mutate(dir = switchBearingMath(dir))
midpoint

# del corner
vec <- addVectors(vec1 = data.frame(mag = midpoint$mag, dir = switchBearingMath( midpoint$dir) ),
                  vec2 = data.frame(mag = 1.638, dir = switchBearingMath( (332+29/60) )))
delcorner <- vec %>% mutate(dir = switchBearingMath(dir))
delcorner

# missing nail
vec <- addVectors(vec1 = data.frame(mag = delcorner$mag, dir = switchBearingMath( delcorner$dir) ),
                  vec2 = data.frame(mag = 6.037, dir = switchBearingMath( (227+07/60) - 180  )))
missingnail <- vec %>% mutate(dir = switchBearingMath(dir))
missingnail

# lorrikeet corner
vec <- addVectors(vec1 = data.frame(mag = 4.67, dir = switchBearingMath( 227+07/60) ),
                  vec2 = data.frame(mag = 4, dir = switchBearingMath( (317+59/60) )))

vec %>% mutate(dir = switchBearingMath(dir))


# kingfisher corner
kfishercnr <- addVectors(vec1 = data.frame(mag = 4.67, dir = switchBearingMath( 227+07/60) ),
                  vec2 = data.frame(mag = 4, dir = switchBearingMath( 227+07/60) ))
kfishercnr <- kfishercnr %>% mutate(dir = switchBearingMath(dir))
kfishercnr

# 6m truncation, kingfisher
sixmtrunc <- addVectors(vec1 = data.frame(mag = kfishercnr$mag, dir = switchBearingMath( kfishercnr$dir ) ),
                         vec2 = data.frame(mag = 2, dir = switchBearingMath( 227+07/60) ))
sixmtrunc <- sixmtrunc %>% mutate(dir = switchBearingMath(dir))

# 6m truncation, mid circle
sixmtrunc <- addVectors(vec1 = data.frame(mag = sixmtrunc$mag, dir = switchBearingMath( sixmtrunc$dir ) ),
                        vec2 = data.frame(mag = 6, dir = switchBearingMath( 227+07/60+90) )) # turn 90 degrees and go 6 metres
sixmtrunc <- sixmtrunc %>% mutate(dir = switchBearingMath(dir))

# Distance to first chord
sixmtrunc <- addVectors(vec1 = data.frame(mag = sixmtrunc$mag, dir = switchBearingMath( sixmtrunc$dir ) ),
                        vec2 = data.frame(mag = 6, dir = switchBearingMath( 227+07/60+90+90+60) )) # turn towards first chord
sixmtrunc %>% mutate(dir = switchBearingMath(dir))



# merv corner
vec <- addVectors(vec1 = data.frame(mag = 4.67, dir = switchBearingMath( 227+07/60) ),
                  vec2 = data.frame(mag = 4+16.383, dir = switchBearingMath( 227+07/60) ))

vec %>% mutate(dir = switchBearingMath(dir))

24.9-25.053





vec <- addVectors(vec1 = data.frame(mag = (20.002+4+16.4+19), dir = switchBearingMath(227+07/60)),
                  vec2 = data.frame(mag = 20.925, dir = switchBearingMath(300+01/60)))
switchBearingMath(vec$dir)

addVectors(vec1 = data.frame(mag = 4.7, dir = switchBearingMath(227+07/60)),
           vec2 = data.frame(mag = 20.925, dir = switchBearingMath(300+01/60)))
switchBearingMath(vec$dir)


180 - switchBearingMath(vec$dir)
47+180
64+180

zeroup <-addVectors(vec1 = data.frame(mag = (20.002+4+16.4+19), dir = 0),
                  vec2 = data.frame(mag = 20.925, dir = (300+01/60) - (227+07/60) ))
(227+07/60) - 180 + zeroup$dir


( ( vec$dir - 90)   ) %% 360
64+180

360 - (vec$dir - 270)

vec <- addVectors(vec1 = data.frame(mag = 28, dir = switchBearingMath(317) ),
                  vec2 = data.frame(mag = 20, dir = switchBearingMath(227) ))

vec


( (90 - vec$dir) + 180 )  %% 360


addVectors(vec1 = data.frame(mag = 1, dir = 45),
           vec2 = data.frame(mag = 1, dir = -45))



convert_bearing_to_math <- function(bearing_deg) {
  (90 - bearing_deg) %% 360
}

# Inputs from real estate plan (bearings)
mag1 <- 20.002 + 4 + 16.4 + 19
dir1_bearing <- 227 + 7/60

mag2 <- 20.925
dir2_bearing <- 300 + 1/60

# Convert to math angles
dir1_rad <- convert_bearing_to_math(dir1_bearing) * pi / 180
dir2_rad <- convert_bearing_to_math(dir2_bearing) * pi / 180

# Convert to Cartesian
x1 <- mag1 * cos(dir1_rad)
y1 <- mag1 * sin(dir1_rad)
x2 <- mag2 * cos(dir2_rad)
y2 <- mag2 * sin(dir2_rad)

# Add components
x_total <- x1 + x2
y_total <- y1 + y2

# Resulting vector
mag_result <- sqrt(x_total^2 + y_total^2)
dir_result_math_deg <- atan2(y_total, x_total) * 180 / pi
dir_result_bearing <- (90 - dir_result_math_deg) %% 360



# Output
mag_result
dir_result_bearing
}
