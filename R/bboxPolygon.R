
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
#' @examples   shpUnionedSchoolds %>% st_centroid() %>% bbox_polygon()
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

  return(polyReturn)
}




#' Create an sf bounding box from a list of longitude and latitudes
#'
#' @param list_lng
#' @param list_lat
#' @param crs
#'
#' @return
#' @export
#'
#' @examples
#'
#' bbox_from_points(c(45.6,44.2), c(-71.1,-73.1))
bbox_from_points <- function(list_lng, list_lat,crs=4326){

  stopifnot(length(list_lng) == length(list_lat))

  sp_box <- as(raster::extent(list(x=list_lng,y=list_lat )), "SpatialPolygons")

  shp_bbox <-  sf::st_as_sf(sp_box)

  sf::st_crs(shp_bbox) <- crs

  return(shp_bbox)
}
