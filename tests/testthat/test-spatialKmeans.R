test_that("",{


  shpBuildings <- sf::st_read('https://data.montreal.ca/dataset/d26fad0f-2eae-44d5-88a0-2bc699fd2592/resource/1c02ead8-f680-495f-9675-6dd18bd3cad0/download/logsoc_donneesouvertes_20191231.geojson')


  #No need to get the centroids, spatialKMeans does it automatically
  shpBuildingsAgg <- spatialKMeans (shpBuildings,
                                    numCentroids = 500,
                                    numClosestPoints = 10,
                                    var="an_orig",
                                    aggFct=median)

  assertthat::are_equal(shpBuildingsAgg$an_orig %>% is.na %>% sum() ,0)

})


#
# test_that("raw num centroids + median heightmax - villeray", {
#
#   library(ggplot2)
#   library(sf)
#   library(here)
#
#
#   mypwd <- keyringr::decrypt_gk_pw("user charles")
#
#   #mtl_buildings
#   conn <- DBI::dbConnect( drv = RPostgreSQL::PostgreSQL(),
#                        host="localhost",
#                        user="postgres",
#                        password=mypwd,
#                        dbname="nrCan_buildings")
#
#   #This is also to test out st_transform and make sure that the srid of public.quartiers_sociologiques_2014_2950 is really 2950
#   queryStr <- 'SELECT *
#               FROM (
#               	SELECT q_socio,  ST_Transform(geom,4326) as geom
#               	FROM public.quartiers_sociologiques_2014_2950
#               	WHERE q_socio LIKE \'Villeray\'
#               	) as neigh
#               JOIN
#               	(SELECT *
#               	FROM mtl_buildings
#               	WHERE  St_isvalid (geom)
#               	 )as buildings
#               ON st_intersects(neigh.geom, buildings.geom)'
#
#   shpBuildings <- sf::st_read(conn,
#                           query = queryStr)
#
#
#   #No need to get the centroids, spatialKMeans does it automatically
#   shpBuildingsAgg <- spatialKMeans (shpBuildings,
#                             numCentroids = 10**3,
#                             numClosestPoints = 10,
#                             var="heightmax",
#                             aggFct=median)
#
#   #Check same crs
#   assertthat::are_equal(st_crs(shpBuildingsAgg), st_crs(shpBuildings))
#
#   #Number of centroids
#   assertthat::are_equal(nrow(shpBuildingsAgg) , 10**3)
#
#   #Plot to see the difference
#   shpBuildingsAgg$id <- 'aggregated'
#   shpBuildings$id <- 'raw'
#   shpBuildings %<>% dplyr::rename(geometry=geom)
#
#   shpBoth <- rbind(shpBuildingsAgg,
#                    shpBuildings %>% dplyr::select( colnames(shpBuildingsAgg)))
#
#   mapBoxToken <- keyringr::decrypt_gk_pw('token mapToken')
#   Sys.setenv(MAPBOX_ACCESS_TOKEN=mapBoxToken)
#
#   ggplot() +
#     snapbox::layer_mapbox( map_style = stylebox::mapbox_light(),  sf::st_bbox(shpBuildingsAgg, crs=st_crs(shpBoth)), scale_ratio = 0.25) +
#     geom_sf(data=shpBoth, aes(col=heightmax) ) +
#     facet_wrap(~id, ncol=1)
#
#
#   tmap::tmap_mode("view")
#   tmap::tm_shape(shpBuildings  ) +
#     tmap::tm_polygons("heightmax")
#
# })
#
#
# test_that("raw num centroids + median heightmin - villeray", {
#
#   library(ggplot2)
#   library(sf)
#   library(here)
#
#
#   mypwd <- keyringr::decrypt_gk_pw("user charles")
#
#   #mtl_buildings
#   conn <- DBI::dbConnect( drv = RPostgreSQL::PostgreSQL(),
#                           host="localhost",
#                           user="postgres",
#                           password=mypwd,
#                           dbname="nrCan_buildings")
#
#   #This is also to test out st_transform and make sure that the srid of public.quartiers_sociologiques_2014_2950 is really 2950
#   queryStr <- 'SELECT *
#               FROM (
#               	SELECT q_socio,  ST_Transform(geom,4326) as geom
#               	FROM public.quartiers_sociologiques_2014_2950
#               	WHERE q_socio LIKE \'Villeray\'
#               	) as neigh
#               JOIN
#               	(SELECT *
#               	FROM mtl_buildings
#               	WHERE  St_isvalid (geom)
#               	 )as buildings
#               ON st_intersects(neigh.geom, buildings.geom)'
#
#   shpBuildings <- st_read(conn,
#                           query = queryStr)
#
#
#   #No need to get the centroids, spatialKMeans does it automatically
#   shpBuildingsAgg <- spatialKMeans (shpBuildings,
#                                     numCentroids = 10**3,
#                                     numClosestPoints = 10,
#                                     var="heightmin",
#                                     aggFct=median)
#
#   #Check same crs
#   assertthat::are_equal(st_crs(shpBuildingsAgg), st_crs(shpBuildings))
#
#   #Number of centroids
#   assertthat::are_equal(nrow(shpBuildingsAgg) , 10**3)
#
#   #Plot to see the difference
#   shpBuildingsAgg$id <- 'aggregated'
#   shpBuildings$id <- 'raw'
#   shpBuildings %<>% dplyr::rename(geometry=geom)
#
#   shpBoth <- rbind(shpBuildingsAgg,
#                    shpBuildings %>% dplyr::select( colnames(shpBuildingsAgg)))
#
#   mapBoxToken <- keyringr::decrypt_gk_pw('token mapToken')
#   Sys.setenv(MAPBOX_ACCESS_TOKEN=mapBoxToken)
#
#   ggplot() +
#     snapbox::layer_mapbox( map_style = stylebox::mapbox_light(),  st_bbox(shpBuildingsAgg, crs=st_crs(shpBoth)), scale_ratio = 0.25) +
#     geom_sf(data=shpBoth, aes(col=heightmin) ) +
#     facet_wrap(~id, ncol=1)
#
#
#   #tmap::tmap_mode("view")
#   #tmap::tm_shape(shpBuildings  ) +
#   #  tmap::tm_polygons("heightmin")
#
# })
#
#
#
#
# test_that("proportion of centroids + median - villeray", {
#
#   library(ggplot2)
#   library(sf)
#   library(here)
#
#
#   mypwd <- keyringr::decrypt_gk_pw("user charles")
#
#   #mtl_buildings
#   conn <- DBI::dbConnect( drv = RPostgreSQL::PostgreSQL(),
#                           host="localhost",
#                           user="postgres",
#                           password=mypwd,
#                           dbname="nrCan_buildings")
#
#
#   queryStr <- 'SELECT *
#               FROM (
#               	SELECT q_socio,geom
#               	FROM public.quartiers_sociologiques_2014
#               	WHERE q_socio LIKE \'Villeray\'
#               	) as neigh
#               JOIN
#               	(SELECT *
#               	FROM mtl_buildings
#               	WHERE  St_isvalid (geom)
#               	 )as buildings
#               ON st_intersects(neigh.geom, buildings.geom)'
#
#   shpBuildings <- st_read(conn,
#                           query = queryStr)
#
#
#   #No need to get the centroids, spatialKMeans does it automatically
#   shpBuildingsAgg <- spatialKMeans (shpBuildings,
#                                     propToKeep = 0.1,
#                                     numClosestPoints = 10,
#                                     var="heightmax",
#                                     aggFct=median)
#   #Check same crs
#   assertthat::are_equal(st_crs(shpBuildingsAgg), st_crs(shpBuildings))
#
#   #Number of centroids
#   assertthat::are_equal(nrow(shpBuildingsAgg) , ceiling(0.1*nrow(shpBuildings)))
#
#   #Plot to see the difference
#   shpBuildingsAgg$id <- 'aggregated'
#   shpBuildings$id <- 'raw'
#   shpBuildings %<>% dplyr::rename(geometry=geom)
#
#   shpBoth <- rbind(shpBuildingsAgg,
#                    shpBuildings %>% dplyr::select( colnames(shpBuildingsAgg)))
#
#   ggplot(shpBoth) +
#     geom_sf(aes(col=heightmax) ) +
#     facet_wrap(~id, ncol=1)
# })
#
#
#
#
# test_that("proportion of centroids + median heightmin - villeray", {
#
#   library(ggplot2)
#   library(sf)
#   library(here)
#
#
#   mypwd <- keyringr::decrypt_gk_pw("user charles")
#
#   #mtl_buildings
#   conn <- DBI::dbConnect( drv = RPostgreSQL::PostgreSQL(),
#                           host="localhost",
#                           user="postgres",
#                           password=mypwd,
#                           dbname="nrCan_buildings")
#
#
#   queryStr <- 'SELECT *
#               FROM (
#               	SELECT q_socio,geom
#               	FROM public.quartiers_sociologiques_2014
#               	WHERE q_socio LIKE \'Villeray\' OR q_socio LIKE \'Peter McGill\'
#               	) as neigh
#               JOIN
#               	(SELECT *
#               	FROM mtl_buildings
#               	WHERE  St_isvalid (geom)
#               	 )as buildings
#               ON st_intersects(neigh.geom, buildings.geom)'
#
#   shpBuildings <- st_read(conn,
#                           query = queryStr)
#
#
#   #No need to get the centroids, spatialKMeans does it automatically
#   shpBuildingsAgg <- spatialKMeans (shpBuildings,
#                                     propToKeep = 0.1,
#                                     numClosestPoints = 10,
#                                     var="heightmin",
#                                     aggFct=median)
#   #Check same crs
#   assertthat::are_equal(st_crs(shpBuildingsAgg), st_crs(shpBuildings))
#
#   #Number of centroids
#   assertthat::are_equal(nrow(shpBuildingsAgg) , ceiling(0.1*nrow(shpBuildings)))
#
#   #Plot to see the difference
#   shpBuildingsAgg$id <- 'aggregated'
#   shpBuildings$id <- 'raw'
#   shpBuildings %<>% dplyr::rename(geometry=geom)
#
#   shpBoth <- rbind(shpBuildingsAgg,
#                    shpBuildings %>% dplyr::select( colnames(shpBuildingsAgg)))
#
#   ggplot(shpBoth) +
#     geom_sf(aes(col=heightmin) ) +
#     facet_wrap(~id, ncol=1)
# })
