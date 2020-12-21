


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
