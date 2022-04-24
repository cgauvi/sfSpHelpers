



#' Get the long/lat of centroids of sf object (adds 2 new columns if they do not already exist)
#'
#' Similar to cbind( shp , st_coordinates(st_centroid(shp)))
#'
#' @param shp, sf object
#'
#' @export
#'
#' @return shp with 2 new lng and lat columns
#'
getCentroids <- function(shp){

  #Check if columns already exist
  if(all(c("lng","lat") %in% colnames(shp))) {
    warning('Warning! shp already has lat lng which could be centroids')
    return(shp)
  }

  #For reference: st_centroid(shpSchools) %>% pull(geometry) is equiv to st_geometry(st_centroid(shpSchools))
  centroidsCols <-  do.call( rbind, sf::st_geometry(sf::st_centroid(shp))) %>%
    dplyr::as_tibble() %>%
    setNames(c("lng","lat"))

  if(!any(c("lng","lat") %in% colnames(shp))) shp %<>% dplyr::bind_cols(centroidsCols)


  return(shp)


}
