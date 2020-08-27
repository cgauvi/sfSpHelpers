


#' Generic fct to filter a shp using a spatial predicate (e.g. st_intersect) and another shp
#'
#' @param shpToFilter initial sf object to filter
#' @param shpToCompare sf object use for filtering
#' @param st_predicate spatial predicate - eg sf::st_within
#'
#' @return shpFiltered
#' @export
#'
#' @examples
st_generic_filter <- function(shpToFilter,
                              shpToCompare,
                              st_predicate){

  #Quick checks
  stopifnot( any( 'sf' %in% class(shpToFilter) ) )
  stopifnot( any( 'sf' %in% class(shpToCompare) ) )
  stopifnot( nrow(shpToFilter) > 0 )
  stopifnot( nrow(shpToCompare) > 0 )

  #Get the features that intersect/touch/within, etc
  idxIntersects <-  purrr::map_lgl( st_predicate(shpToFilter, shpToCompare),
                                    ~length(.x) >0)

  #Print a message and identify the type of spatial predicate
  strFctName <- paste( as.character(substitute(st_predicate)), sep="", collapse = "")
  print(glue::glue('Shape {strFctName} {sum(idxIntersects)} features'))

  shpFiltered <- shpToFilter[idxIntersects, ]

  return(shpFiltered)
}




#' Convenience wrapper that calls st_generic_filter with st_intersects
#'
#' @param shpToFilter
#' @param shpToCompare
#' @param st_predicate
#'
#' @return
#' @export
#'
#' @examples
st_intersects_filter <- function(shpToFilter,
                              shpToCompare){

  return(
    st_generic_filter (shpToFilter,
                       shpToCompare,
                       sf::st_intersects)
    )
}




#' Convenience wrapper that calls st_generic_filter with st_within
#'
#' @param shpToFilter
#' @param shpToCompare
#' @param st_predicate
#'
#' @return
#' @export
#'
#' @examples
st_within_filter <- function(shpToFilter,
                                 shpToCompare){

  return(
    st_generic_filter (shpToFilter,
                       shpToCompare,
                       sf::st_within)
  )
}


