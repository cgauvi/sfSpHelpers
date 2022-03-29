
test_that("voronoi interpolation on points", {

  require(dplyr)

  shpBuildings <- sf::st_read('https://data.montreal.ca/dataset/d26fad0f-2eae-44d5-88a0-2bc699fd2592/resource/1c02ead8-f680-495f-9675-6dd18bd3cad0/download/logsoc_donneesouvertes_20191231.geojson')
  shpBuildings_sample <-shpBuildings %>%  sample_n(200)

  shpVoronoi <- getVoronoiInterpolation( shpBuildings_sample,
                                          var="an_orig",
                                          fctToAggregate =  median) #nas are removed by default in getVoronoiInterpolation

  assertthat::assert_that( sum(is.na(shpVoronoi$an_orig)) ==0 )
}
)


test_that("voronoi interpolation on polygons", {

  file_name <- system.file("shape/nc.shp", package="sf")
  nc <- sf::st_read(file_name)

  shpVoronoi <- getVoronoiInterpolation( nc,
                                         var="BIR79",
                                         fctToAggregate = median ) #nas are removed by default in getVoronoiInterpolation

  assertthat::assert_that( sum(is.na(shpVoronoi$BIR79)) ==0 )

}
)

#
# test_that("voronoi interpolation based on heightmax for 2 discontiguous neighbourhoods", {
#
#
#   library(ggplot2)
#   library(here)
#
#
#   mypwd <- keyringr::decrypt_gk_pw("user charles")
#   mapBoxToken <- keyringr::decrypt_gk_pw('token mapToken')
#   Sys.setenv(MAPBOX_ACCESS_TOKEN=mapBoxToken)
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
#   shpBuildings <- sf::st_read(conn,
#                           query = queryStr)
#
#
#   shpVoronoi <- getVoronoiInterpolation(shpBuildings,
#                           'heightmax')
#
#   p <- ggplot() +
#     snapbox::layer_mapbox( map_style = stylebox::mapbox_light(),  area = st_bbox(shpVoronoi), scale_ratio = 0.25) +
#     geom_sf(data=shpVoronoi, aes(fill=heightmax), alpha=0.2, , lwd=0.1) +
#     geom_sf(data=shpBuildings %>% sf::st_centroid(), lwd=0.2, alpha=0.2) +
#     viridis::scale_fill_viridis() +
#     ggtitle('Building maximum height') +
#     labs(subtitle="Villeray and Peter McGill",
#          caption = 'Source: NrCan and City of Montreal Lidar buildings dataset.\nMapbox base layer from {snapbox}')
#
#   ggsave(here("Figures", "voronoi_interpolate_villeray_petermcgill_heightmax_4326.png"),
#          p,
#          width = 10,
#          height = 10)
#
#
# })
#
#
#
# test_that("voronoi interpolation based on elev for only 1 neighbourhoods", {
#
#
#   library(ggplot2)
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
#   shpBuildings <- sf::st_read(conn,
#                               query = queryStr)
#
#
#   shpVoronoi <- getVoronoiInterpolation(shpBuildings,
#                                         'elevmax')
#
#   p <- ggplot() +
#     geom_sf(data=shpVoronoi, aes(fill=elevmax)) +
#     geom_sf(data=shpBuildings %>% sf::st_centroid(), lwd=0.5) +
#     viridis::scale_fill_viridis() +
#     ggtitle('Villeray - max elevation interpolation') +
#     labs(caption = 'Voronoi interpolation - SRID 4326')
#
#   ggsave(here("Figures", "voronoi_interpolate_villeray_elevmax_4326.png"),
#          p,
#          width = 10,
#          height = 10)
#
#
#   p
# })
#
#
#
# test_that("voronoi interpolation based on elevmin for only 1 neighbourhoods", {
#
#
#   library(ggplot2)
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
#   shpBuildings <- sf::st_read(conn,
#                               query = queryStr)
#
#
#   shpVoronoi <- getVoronoiInterpolation(shpBuildings,
#                                         'elevmin')
#
#   p <- ggplot() +
#     geom_sf(data=shpVoronoi, aes(fill=elevmin)) +
#     geom_sf(data=shpBuildings %>% sf::st_centroid(), lwd=0.5) +
#     viridis::scale_fill_viridis() +
#     ggtitle('Villeray - min elevation interpolation') +
#     labs(caption = 'Voronoi interpolation - SRID 4326')
#
#   ggsave(here("Figures", "voronoi_interpolate_villeray_elevmin_4326.png"),
#          p,
#          width = 10,
#          height = 10)
#
#   p
# })
#
#
#
#
#
# test_that("voronoi interpolation based on heightmax for only 1 neighbourhoods with projected coordinates", {
#
#
#   library(ggplot2)
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
#   #Transform to 2950
#   queryStr <- 'SELECT *
#               FROM (
#               	SELECT q_socio, st_transform(geom, 2950) as geom
#               	FROM public.quartiers_sociologiques_2014
#               	WHERE q_socio LIKE \'Villeray\'
#               	) as neigh
#               JOIN
#               	(SELECT heightmax, st_transform(geom, 2950) as geom
#               	FROM mtl_buildings
#               	WHERE  St_isvalid (geom)
#               	 )as buildings
#               ON st_intersects(neigh.geom, buildings.geom)'
#
#   shpBuildings <- sf::st_read(conn,
#                               query = queryStr)
#
#
#   shpVoronoi <- getVoronoiInterpolation(shpBuildings,
#                                         'heightmax')
#
#
#   p <- ggplot() +
#     snapbox::layer_mapbox( map_style = stylebox::mapbox_light(),  area = st_bbox(shpVoronoi), scale_ratio = 0.25) +
#     geom_sf(data=shpVoronoi, aes(fill=heightmax), alpha=0.2, lwd=0.1) +
#     geom_sf(data=shpBuildings %>% sf::st_centroid(), lwd=0.1, alpha=0.2 ) +
#     viridis::scale_fill_viridis() +
#     ggtitle('Building maximum height') +
#     labs(subtitle="Villeray, Montreal",
#          caption = 'Source: NrCan and City of Montreal Lidar buildings dataset.\nMapbox base layer from {snapbox}\nInterpolation based on voronoi diagram of centroids')
#
#   ggsave(here("Figures", "voronoi_interpolate_villeray_heightmax_2950.png"),
#          p,
#          width = 10,
#          height = 10)
#
#
# })

