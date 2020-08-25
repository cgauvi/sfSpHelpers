

#' Get a piecewise constant inerpolation based on the voronoi diagram
#'
#' @param shp, sf object
#' @param varToInterpolate, chr variable to interpolate
#' @param fctToAggregate fct used e.g. mean, median, max or other statistic
#'
#' @return voronoi, sf object with col varToInterpolate
#' @export
#'
#' @examples
getVoronoiInterpolation <- function(shp,
                                    varToInterpolate,
                                    fctToAggregate=mean){


  #Get a df with centroids
  dfCentroids <- shp %>%
    getCentroids() %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(c(lng,lat))

  #Get the voronoi polygon as sp object
  voronoi <- ggvoronoi::voronoi_polygon(dfCentroids,x='lng',y='lat')

  #Set the projection
  sp::proj4string(voronoi) <- sp::proj4string (shp %>% as_Spatial())

  #Convert to sf and set crs
  voronoi %<>% sf::st_as_sf(crs=sf::st_crs(shp))
  voronoi %<>% sf::st_set_crs(st_crs(shp))

  #Add an id to each polygon
  voronoi$id <- 1:nrow(voronoi)

  #Spatial join to assign a voronoi cell to each property point
  shpWithVoronoiCell <- sf::st_join(shp %>%  dplyr::select(one_of(varToInterpolate)),
                                voronoi,
                                join=sf::st_intersects, #make sure we assign a *single* voronoi cell to *each* point
                                largest=T)

  #Quick check that all
  if(shpWithVoronoiCell$id %>% is.na %>% sum){
    print(glue::glue("Warning! there are {shpWithVoronoiCell$id %>% is.na %>% sum} points without a cell assigned -- use a projected crs for better results"))
  }

  #Group the points by voronoi cell and take the average difference from median DA assessment
  shpByCell <- shpWithVoronoiCell %>%
    dplyr::group_by(id) %>%
    dplyr::summarise_at( vars(varToInterpolate),
                         .funs=function(x) fctToAggregate(x, na.rm=T))

  #Quick check for cells that were not interpolated
  if(dplyr::n_distinct(shpByCell$id) != dplyr::n_distinct(voronoi$id ) ){
    print(glue::glue("Warning! there are {dplyr::n_distinct(voronoi$id) - dplyr::n_distinct(shpByCell$id)} missing voronoi cells "))
  }

  #Add back the average to the original polygons
  voronoi %<>% dplyr::left_join(shpByCell %>% sf::st_set_geometry(NULL),
                                by='id')

  return(voronoi)

}
