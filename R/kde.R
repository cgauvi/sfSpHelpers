
#' Produce a 2D kernel density estimate based on some POINTS geometry
#'
#' Note: Taken from https://github.com/r-spatial/sf/issues/1201
#'
#' @param points sf object with POINT geometry
#' @param cellsize size of cell
#' @param bandwith bandwith for 2D kde, larger: smoother, smaller: spikier
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
#' shpBuildings <- SfSpHelpers::get_zipped_remote_shapefile ("https://data.montreal.ca/dataset/4ad6baea-4d2c-460f-a8bf-5d000db498f7/resource/866a3dbc-8b59-48ff-866d-f2f9d3bbee9d/download/uniteevaluationfonciere.geojson.zip")
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



#' Create polygons that represent a 2D density out of points or centroid of polygons
#'
#' If using a sf object, will use st_centroids
#' Otherwise, need to ensure lng and lat columns are available
#'
#' @param df data.frame ish or sf
#' @param crs_epsg crs of the newly create polygon, default 4326 if inputing a df and not explicitely set
#' @param bw bandwidth  (same for both dimesions)
#' @param gsize gid size (same for both dimesions)
#'
#' @return
#' @export
#'
#' @examples
get_polygon_heatmap <- function(df,  crs_epsg=4326, bw=.001, gsize=500 ){

  require(sp)
  require(sf)
  require(magrittr)
  require(dplyr)

  if(any( grepl('sf', class(df)))){
    df %<>% SfSpHelpers::getCentroids()
    crs_epsg <- st_crs(df)
    df <- df %>% st_set_geometry(NULL)

  }

  #Make sure the lng and lat columns are present
  stopifnot( all(c("lng","lat") %in% colnames(df) ))

  df_lng_lat <- df %>%
    dplyr::select(lng,lat) %>%
    as.data.frame() %>%
    mutate_at(vars("lng","lat"), as.numeric )

  #Compute the 2D kernel density estimate
  kde <- KernSmooth::bkde2D(df_lng_lat,
                bandwidth=c(bw, bw), gridsize = c(gsize,gsize))

  #Get contour lines
  CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
  ## EXTRACT CONTOUR LINE LEVELS
  LEVS <- as.factor(sapply(CL, `[[`, "level"))
  NLEV <- length(levels(LEVS))
  ## CONVERT CONTOUR LINES TO POLYGONS
  pgons <- lapply(1:length(CL), function(i)
    Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
  spgons = SpatialPolygons(pgons)
  shp_polygons <- spgons %>% st_as_sf() %>% st_set_crs( st_crs(crs_epsg))
  shp_polygons$colors <- heat.colors(NLEV, NULL)[LEVS]
  shp_polygons$level <- LEVS %>% as.numeric()


  return(shp_polygons)
}

