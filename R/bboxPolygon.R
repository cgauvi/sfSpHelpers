
#' Create a st_polygon from a bounding box
#'
#' Really useful function to define bounding box enveloppe
#'
#' @param shp sf or sfc object that we want to bound within some box
#' @param buffer dist to add around the bounding box to avoid the weird boundary effects
#'
#' @export
#'
#' @return st_polygon forming a box
#' @details See https://stackoverflow.com/questions/45719790/create-voronoi-polygon-with-simple-feature-in-r
#'
#' @examples
#' \dontrun{
#' shp_trees <-  st_read('https://www.donneesquebec.ca/recherche/dataset/bc5afddf-9439-4e96-84fb-f91847b722be/resource/bbdca0dd-82df-42f9-845b-32348debf8ab/download/vdq-arbrepotentielremarquable.geojson')
#' shp_trees %>% bbox_polygon()
#' }
#'
bbox_polygon <- function(shp, buffer=0) {
  bb <- sf::st_bbox(shp)

  #Alternatively, using sp syntax
  #p <- x %>% as("Spatial") %>% sp::bbox() %>% splancs::bboxx() #Bounding box with all 4 extreme points
  #p %<>% rbind(mat[1, ] ) #Close the polygon

  p <- matrix(
    c(bb["xmin"], bb["ymin"], #Create the bounding box => rectangle with 4 extreme points
      bb["xmin"], bb["ymax"],
      bb["xmax"], bb["ymax"],
      bb["xmax"], bb["ymin"],
      bb["xmin"], bb["ymin"]), #Close the polygon
    ncol = 2, byrow = T
  )

  if(buffer == 0 ) polyReturn <- sf::st_polygon(list(p)) #Convert list of points to polygon (need list here)
  else polyReturn <- sf::st_polygon(list(p)) %>% sf::st_buffer(dist=buffer)

  #Cast to sf and set crs
  polyReturn %<>% sf::st_sfc() %>% sf::st_as_sf(crs=sf::st_crs(shp))

  #Final check
  bbox_validation(polyReturn)

  return(polyReturn)
}




#' Create an sf bounding box from a list of longitude and latitudes
#'
#' Can work with up to 2 points
#'
#' @param list_lng
#' @param list_lat
#' @param crs
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(ggplot2)
#' lats <- c(45.6,44.2)
#' lngs <- c(-71.1,-73.1)
#' shp_bbox <- bbox_from_points(lats, lngs)
#' shp_points <- st_as_sf( data.frame(lat=lats, lng=lngs), coords=c('lat', 'lng'), crs=4326)
#' ggplot() + geom_sf(data=shp_points) + geom_sf(data=shp_bbox)
#' }
bbox_from_points <- function(list_lng, list_lat, crs=4326){

  require(sf)
  require(magrittr)

  stopifnot(length(list_lng) == length(list_lat))

  sp_box <- as(raster::extent(list(x=list_lng,y=list_lat )), "SpatialPolygons")

  shp_bbox <-  sf::st_as_sf(sp_box)

  sf::st_crs(shp_bbox) <- crs

  #Final check
  bbox_validation(shp_bbox)

  return(shp_bbox)
}




#' Create an sf bounding box from a vector with (xmin,ymin,ymax,ymax)
#'
#' This works automatically with raster extents or sf::bbox
#'
#' @param v vector with names (xmin,ymin,ymax,ymax)
#' @param crs
#' @param x_name_min character
#' @param y_name_min character
#' @param x_name_max character
#' @param y_name_max character
#'
#' @return sf object representing bbox
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' shp_trees <-  st_read('https://www.donneesquebec.ca/recherche/dataset/bc5afddf-9439-4e96-84fb-f91847b722be/resource/bbdca0dd-82df-42f9-845b-32348debf8ab/download/vdq-arbrepotentielremarquable.geojson')
#' rasterKDECentroids <- st_kde(shp_trees , cellsize = 0.001, bandwith =c(.001, .001 ) )
#' bbox_from_vector(extent(rasterKDECentroids) , crs=4326)
#'
#' }
bbox_from_vector <- function(v,
                        crs,
                        x_name_min="xmin" ,
                        y_name_min="ymin",
                        x_name_max="xmax" ,
                        y_name_max="ymax"){

  require(magrittr)
  require(sf)
  require(sp)


  #IF already produced by sf::bbox will work as identity. If raster extent , then will convert to bbox
  v <- st_bbox(v)

  #Check names. Order can differ, but the xmin, xmax,... should all be there (and nothing else)
  names_extent <- c(x_name_min,y_name_min,x_name_max,y_name_max)
  assertthat::assert_that(all( names_extent %in% names(v) ) & all( names(v) %in%  names_extent),
                          msg='v object must contain names of the coordinates to use for bbox')
  assertthat::assert_that(!is.na(crs))

  #Raster extent
  rast_extent <- raster::extent( v[[x_name_min]],v[[x_name_max]],
                                 v[[y_name_min]],v[[y_name_max]] )

  #Convert to sf
  shp_bbox <- as(rast_extent, "SpatialPolygons") %>%
    st_as_sf() %>%
    st_set_crs(crs)

  #Final check
  bbox_validation(shp_bbox)

  return(shp_bbox)

}


#' Minimal wrapper over bbox_from_vector that takes a character representing a city/osm place and returns the bbox as a sf polygon
#'
#' Need to slipt the lng and lat for osm
#'
#' e.g. the lat,lng are when compared to sf::st_boox
#'
#' osmdata::opq(bbox = 'Atlanta')$bbox
#' vs
#' sf::st_bbox(osmdata::osmdata_sf(osmdata::opq(bbox = 'Atlanta')))
#'
#' @param osm_place
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library (mapview)
#' shp_bbox_disney_osm <- bbox_from_osm('Disney')
#' mapview(shp_bbox_disney_osm)
#' }
bbox_from_osm <- function(osm_place){


  opq <- osmdata::opq(bbox = osm_place)

  bbox <- stringr::str_split(opq$bbox, pattern = ',' )[[1]] %>% as.double()
  names(bbox) <- c( "ymin", "xmin" , "ymax", "xmax" )

  bbox_from_vector(bbox,
                   crs = 4326)
}


#' Util to check if the bbox polygon produced is valid or not
#'
#' @param shp_bbox
#'
#' @return logical/boolean T for ok F for warning
#'
bbox_validation <- function(shp_bbox){

  #Make valid & check
  shp_bbox %<>% st_make_valid()

  if(units::drop_units(st_area(shp_bbox)) == 0){
    warning('Waring! The bounding box has 0 area: are points unique? is affine dimension 2?')
    return(F)
  }

  return(T)

}
