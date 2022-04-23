


#' Add start and end points to a sf linestring object
#'
#' @param shp_segments , sf object
#'
#' @return shp_segments , sf object with 4 new columns start_x,start_y, end_x,end_y
#' @export
#'
st_linestring_add_endpoints <- function(shp_segments){

  #Make sure sf object and geometry is linestring
  stopifnot( any(grepl('sf', class(shp_segments) ) ) )
  stopifnot( any( grepl('LINESTRING' , class(shp_segments$geometry)) ))

  #Get the start and endpoints + convert to matrix
  mat_start <- do.call(rbind, shp_segments$geometry %>% lwgeom::st_startpoint())
  colnames(mat_start) <- c('start_x','start_y')

  mat_end <- do.call(rbind, shp_segments$geometry %>% lwgeom::st_endpoint())
  colnames(mat_end) <- c('end_x','end_y')

  #Column bind to existing shp
  shp_segments  %<>% cbind( mat_start )
  shp_segments  %<>% cbind( mat_end )


  return(shp_segments)

}



#' Uses st_linestring_add_endpoints to take a potentiall complicated linestring and convert it into a simple linestring based only on the endpoints of the initial linestring
#'
#' @param shp_segments
#'
#' @return shp_segments_endpoints
#' @export
#'
#' @examples
st_linestring_to_sf_linestring_endpoints <- function(shp_segments){

  assertthat::assert_that(nrow(shp_segments)>0,msg='Fatal error! no segments passed')

  shp_segments %<>% st_linestring_add_endpoints()

  #Create a sfc object using the endpoints
  list_lines <- purrr::map ( 1:nrow(shp_segments),
                             ~sf::st_linestring(x = matrix(  c(shp_segments$start_x[[.x]], shp_segments$end_x[[.x]], shp_segments$start_y[[.x]], shp_segments$end_y[[.x]]), nrow = 2, ncol = 2) )
  )

  #Convert to sf
  shp_segments_endpoints <- sf::st_as_sf( st_sfc(coords=list_lines),
                                       crs=st_crs(shp_segments) ) %>%
    rename(geometry=x)


  return(shp_segments_endpoints)

}
