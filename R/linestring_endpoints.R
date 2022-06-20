


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



#' Convenience function over  st_linestring_add_endpoints and st_sf_linestring_from_points
#' Takes a potentially complicated linestring and converts it into a simple linestring based only on the *endpoints* of the initial linestring
#' (keeps the geometry as LINESTRING)
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

  #Convert to sf (new sf object)
  crs <- st_crs(shp_segments)
  shp_segments_endpoints <- st_sf_linestring_from_points(shp_segments %>% sf::st_drop_geometry(),
                                                         col_start_x = 'start_x',
                                                         col_start_y = 'start_y',
                                                         col_end_x = 'start_x',
                                                         col_end_y = 'start_y',
                                                         crs=crs)


  return(shp_segments_endpoints)

}


#' From a df where each row represents an edge in 2D of the form (lng_1,lat_1),  (lng_2,lat_2), create an sf object with LINESTRING geometry
#'
#' @param df_segments
#' @param col_start_x start represents the first endpoint (start is a misnomer there is no edge orientation)
#' @param col_start_y
#' @param col_end_x
#' @param col_end_y
#'
#' @return shp_segments_endpoints sf object
#' @export
#'
#' @examples
st_sf_linestring_from_points <- function(df_segments,
                                         col_start_x = 'start_x',
                                         col_start_y = 'start_y',
                                         col_end_x = 'start_x',
                                         col_end_y = 'start_y',
                                         crs=4326){

  assert_that(!any(class(df_segments) %in% 'sf'),
              msg='st_sf_linestring_from_points takes a dataframe as input! Using an sf object creates ambiguity')

  # Make sure columns are present
  cols_endpoints <- c(col_start_x,col_start_y,col_end_x,col_end_y)
  str_cols <- paste0(cols_endpoints, collapse = ", ")
  assert_that(all(cols_endpoints %in% colnames(df_segments)),
              msg=glue('Error! columns names indicating the endpoints {str_cols} are absent! Set appropriate names '))

  # Create the linestring geometry
  list_lines <- purrr::map ( 1:nrow(df_segments),
               ~sf::st_linestring(x = matrix(  c(df_segments[[col_start_x]][[.x]], df_segments[[col_start_y]][[.x]],
                                                 df_segments[[col_end_x]][[.x]], df_segments[[col_end_y]][[.x]]),
                                               nrow = 2, ncol = 2) )
  )

  #Convert to sf
  shp_segments_endpoints <- sf::st_as_sf( st_sfc(coords=list_lines),
                                          crs=crs) %>%
    rename(geometry=x)

  # Add back the original data
  shp_segments_endpoints %<>% cbind(df_segments)

  return(shp_segments_endpoints)

}
