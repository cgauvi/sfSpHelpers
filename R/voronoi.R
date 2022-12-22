

#' Get a piecewise constant inerpolation based on the voronoi diagram
#'
#' @param shp, sf object
#' @param varToInterpolate, chr variable to interpolate
#' @param fctToAggregate fct used e.g. mean, median, max or other statistic
#'
#' @return voronoi, sf object with col varToInterpolate
#' @export
#'
getVoronoiInterpolation <- function(shp,
                                    varToInterpolate,
                                    fctToAggregate=mean){

  require(dplyr)

  #Get a df with centroids + make sure they are distinct - otherwise ggvoronoi::voronoi_polygon crashes
  dfCentroids <- shp %>%
    getCentroids() %>%
    distinct(lng,lat)

  #Get the voronoi polygon as sp object
  voronoi <- voronoi_polygon(dfCentroids,x='lng',y='lat')

  #Set the projection
  sp::proj4string(voronoi) <- sp::proj4string (shp %>% sf::as_Spatial())

  #Convert to sf and set crs
  voronoi %<>% sf::st_as_sf(crs=sf::st_crs(shp))
  voronoi %<>% sf::st_set_crs(sf::st_crs(shp))

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



#' Voronoi Diagram from Data Frame
#'
#' Taken from https://cran.r-project.org/src/contrib/Archive/ggvoronoi/ggvoronoi_0.8.4.tar.gz directly since it is a standalone function and the dependency on ggvoronoi
#' is messing up R CMD CHECK
#'
#' Create a Voronoi diagram for analysis or plotting with \code{\link[ggplot2]{geom_polygon}}.
#' @param data \code{data.frame} containing a set of points to make voronoi regions and any additional desired columns.
#' @param x numeric vector (for example longitude).
#' @param y numeric vector (for example latitude).
#' @param outline \code{data.frame} with first column x/longitude, second column y/latitude, and a group column denoting islands or pieces.
#' @param data.frame output as \code{data.frame}? You will lose information if you do this. For use in \code{\link[ggvoronoi]{StatVoronoi}}.
#' @keywords voronoi, choropleth
#' @import ggplot2 sp deldir rgeos
#' @importFrom methods slot
#' @export
#' @examples
#' \dontrun{
#' set.seed(45056)
#' x <- sample(1:200,100)
#' y <- sample(1:200,100)
#' points <- data.frame(x, y,
#'                      distance = sqrt((x-100)^2 + (y-100)^2))
#' circle <- data.frame(x = 100*(1+cos(seq(0, 2*pi, length.out = 2500))),
#'                      y = 100*(1+sin(seq(0, 2*pi, length.out = 2500))),
#'                      group = rep(1,2500))
#'
#' vor_spdf <- voronoi_polygon(data=points,x="x",y="y",outline=circle)
#' vor_df <- fortify_voronoi(vor_spdf)
#'
#' ggplot(vor_df) +
#'     geom_polygon(aes(x=x,y=y,fill=distance,group=group))
#'}
voronoi_polygon = function(data, x = 'x', y = 'y', outline = NULL, data.frame=FALSE)
{
  require(sp)
  require(deldir)

  if(class(data) != "data.frame"){
    stop('"data" must be of class data.frame.')
  }
  if(nrow(data)==0){
    stop('"data" must not be empty.')
  }
  if(sum(duplicated(data[,c(x,y)]))>0){
    stop('"data" must not contain duplicate (x,y) points.')
  }
  xname = x
  yname = y
  x = data[,x]
  y = data[,y]
  if(!is.null(outline)){
    if(class(outline) != "data.frame" & class(outline) != "SpatialPolygonsDataFrame" & class(outline) != "SpatialPolygons"){
      outline = NULL
      warning("Outline must be of class data.frame or SpatialPolygonsDataFrame. No outline will be used.")
    }
    else if(class(outline) == "data.frame"){
      if(nrow(outline)==0){
        stop('"outline" must not be empty.')
      }
      if(!is.numeric(outline[,1]) | !is.numeric(outline[,2])){
        warning("Columns 1 and 2 of Outline must be numeric. No outline will be used.")
      }
      if(is.null(outline$group)){
        outline$group = rep(1, nrow(outline))
      }
      outline_polygons = lapply(unique(outline$group), function(group){
        poly = Polygon(outline[outline$group == group,1:2])
        Polygons(list(poly), ID = as.character(group))
      })
      outline_spdf = SpatialPolygonsDataFrame(SpatialPolygons(outline_polygons),
                                              data = data.frame(group = unique(outline$group),
                                                                row.names = unique(outline$group)))
    }else if(class(outline) == "SpatialPolygonsDataFrame" | class(outline)=="SpatialPolygons"){
      outline_spdf = outline
    }
  }
  if(!is.null(outline)){
    extent = extent(outline_spdf)
    rw = c(min(extent@xmin, min(x)),
           max(extent@xmax, max(x)),
           min(extent@ymin, min(y)),
           max(extent@ymax, max(y)))
  }else{
    rw = NULL
  }
  pts = SpatialPointsDataFrame(cbind(x, y), data, match.ID = T)
  vor_desc = tile.list(deldir(pts@coords[, 1], pts@coords[, 2],
                              rw = rw,suppressMsge = TRUE))
  vor_polygons <- lapply(1:(length(vor_desc)), function(i) {
    tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
    tmp <- rbind(tmp, tmp[1, ])
    Polygons(list(Polygon(tmp)), ID = i)
  })
  rownames(pts@data) = sapply(slot(SpatialPolygons(vor_polygons),
                                   "polygons"), slot, "ID")
  vor_spdf = SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons),
                                      data = pts@data)
  if(!is.null(outline)){
    vor_spdf = rgeos::intersect(gBuffer(vor_spdf, byid=TRUE, width=0), gBuffer(outline_spdf, byid=TRUE, width=0))
  }

  if(data.frame==FALSE){
    return(vor_spdf)
  }else{

    voronoi = suppressMessages(ggplot2::fortify(vor_spdf, data))

    names(voronoi)[1:2] = names(data[,c(xname, yname)])

    for(name in names(data)){
      if(!(name %in% names(voronoi))){
        voronoi[,name] = unlist(lapply(1:length(vor_spdf@polygons), function(i){
          unlist(lapply(1:length(slot(vor_spdf@polygons[[i]], 'Polygons')), function(j){
            rep(vor_spdf@data[i,name], nrow(slot(slot(vor_spdf@polygons[[i]], 'Polygons')[[j]], 'coords')))
          }))
        }))
      }
    }

    return(voronoi)
  }
}
