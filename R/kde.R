
#' Produce a 2D kernel density estimate based on some POINTS geometry
#'
#' Note: Taken from https://github.com/r-spatial/sf/issues/1201
#'
#' @param points sf object with POINT geometry
#' @param cellsize
#' @param bandwith
#' @param extent allows cropping to a specific subregion of the map
#'
#' @return raster
#' @export
#'
#' @examples \dontrun{
#' librarry(sf)
#' libary(here)
#' library(magrittr)
#' library(dplyr)
#'
#' shpBuildings <- st_read(dsn=here::here('Data', 'GeoData', 'QuebecCityOpenData', 'Buildings','vdq-batiments.shp'))
#' shpCentroids <- shpBuildings %>% st_centroid
#'
#' rasterKDECentroids <- st_kde(shpCentroids,0.01,0.01)
#' plot(rasterKDECentroids)
#' }
st_kde <- function(points,cellsize, bandwith, extent = NULL){

  if(is.null(extent)){
    extent_vec <- sf::st_bbox(points)[c(1,3,2,4)]
  } else{
    extent_vec <- sf::st_bbox(extent)[c(1,3,2,4)]
  }

  n_y <- ceiling((extent_vec[4]-extent_vec[3])/cellsize)
  n_x <- ceiling((extent_vec[2]-extent_vec[1])/cellsize)

  extent_vec[2] <- extent_vec[1]+(n_x*cellsize)-cellsize
  extent_vec[4] <- extent_vec[3]+(n_y*cellsize)-cellsize

  coords <- sf::st_coordinates(points)
  matrix <- MASS::kde2d(coords[,1],coords[,2],h = bandwith,n = c(n_x,n_y),lims = extent_vec)
  raster::raster(matrix)
}


