
#' Scale an individual sfc
#'
#' @param sf_to_scale
#' @param scale_factor
#'
#' @return
#'
#' @examples
scale_geom_sfc <- function(sf_to_scale,
                           scale_factor=0.75){

  sf_to_scale_centroid <- sf::st_centroid(sf_to_scale)
  sf_scaled <- (sf_to_scale - sf_to_scale_centroid) * scale_factor + sf_to_scale_centroid

  return(sf_scaled)
}




#' Scale an sf object with *POLYGON geometry
#'
#' Convert to projected coordinates before performing the affine transformation
#'
#' Inspired by Robin Lovelace's https://geocompr.robinlovelace.net/geometric-operations.html
#'
#' @param df
#' @param scale_factor
#'
#' @return
#' @export
#'
#' @examples
scale_geom_df <- function(df,
                          scale_factor=0.75,
                          proj_new = NULL
){


  if (is.null(proj_new)) {
    print('No projection selected.. using web mercator')
    proj_new <- 3857
  }


  # Check projection
  if( is.null(sf::st_crs(df) [[1]])){
    stop('Fatal error, no projection set!')
  } else{

    #Save the original projection for later
    orig_proj <- sf::st_crs(df)

    #Project
    print(glue::glue('Converting to projection {proj_new}'))
    df %<>% sf::st_transform(crs=proj_new)

  }

  #Make sure we have a geom of geometry column
  if( any ( 'geometry' == colnames(df) ) ){
    geom_col_str <- 'geometry'
  }else if (any ( 'geom' == colnames(df) )){
    geom_col_str <- 'geom'
  }else{
    stop('Fatal error! no geom columns')
  }

  #Check if polygon
  if (!any(grepl('POLYGON', class(df[[geom_col_str]])))){
    print('Warning! scale_geom_df only works for polygons! Not doing anything' )
    return (df)
  }

  #We want the centoid of EACH polygon, otherwise this gives weird results
  is_multi_poly <- F
  if( any(grepl('MULTIPOLYGON', class(df[[geom_col_str]]))) ){
    print('Casting to POLYGON...')
    is_multi_poly <- T
    df %<>% sf::st_cast('POLYGON')
  }

  #Scale the geometry column
  print(glue::glue('Using {geom_col_str} as geom column'))
  df[[geom_col_str]] <- scale_geom_sfc(  df[[geom_col_str]], scale_factor ) %>% sf::st_set_crs(proj_new)

  #Reset the proj
  df %<>% sf::st_transform(crs=orig_proj)

  #Return the same structure as what was initially input
  if(is_multi_poly){
    df %<>% sf::st_union() %>% sf::st_sf()
  }

  return(df)
}


