test_that("voronoi interpolation based on heightmax for 2 discontiguous neighbourhoods", {


  library(ggplot2)
  library(here)


  mypwd <- keyringr::decrypt_gk_pw("user charles")

  #mtl_buildings
  conn <- DBI::dbConnect( drv = RPostgreSQL::PostgreSQL(),
                          host="localhost",
                          user="postgres",
                          password=mypwd,
                          dbname="nrCan_buildings")


  queryStr <- 'SELECT *
              FROM (
              	SELECT q_socio,geom
              	FROM public.quartiers_sociologiques_2014
              	WHERE q_socio LIKE \'Villeray\' OR q_socio LIKE \'Peter McGill\'
              	) as neigh
              JOIN
              	(SELECT *
              	FROM mtl_buildings
              	WHERE  St_isvalid (geom)
              	 )as buildings
              ON st_intersects(neigh.geom, buildings.geom)'

  shpBuildings <- sf::st_read(conn,
                          query = queryStr)


  shpVoronoi <- getVoronoiInterpolation(shpBuildings,
                          'heightmax')

  ggplot() +
    geom_sf(data=shpVoronoi, aes(fill=heightmax)) +
    geom_sf(data=shpBuildings %>% sf::st_centroid(), lwd=0.5) +
    viridis::scale_fill_viridis()
})



test_that("voronoi interpolation based on elev for only 1 neighbourhoods", {


  library(ggplot2)
  library(here)


  mypwd <- keyringr::decrypt_gk_pw("user charles")

  #mtl_buildings
  conn <- DBI::dbConnect( drv = RPostgreSQL::PostgreSQL(),
                          host="localhost",
                          user="postgres",
                          password=mypwd,
                          dbname="nrCan_buildings")


  queryStr <- 'SELECT *
              FROM (
              	SELECT q_socio,geom
              	FROM public.quartiers_sociologiques_2014
              	WHERE q_socio LIKE \'Villeray\'
              	) as neigh
              JOIN
              	(SELECT *
              	FROM mtl_buildings
              	WHERE  St_isvalid (geom)
              	 )as buildings
              ON st_intersects(neigh.geom, buildings.geom)'

  shpBuildings <- sf::st_read(conn,
                              query = queryStr)


  shpVoronoi <- getVoronoiInterpolation(shpBuildings,
                                        'elevmax')

  ggplot() +
    geom_sf(data=shpVoronoi, aes(fill=elevmax)) +
    geom_sf(data=shpBuildings %>% sf::st_centroid(), lwd=0.5) +
    viridis::scale_fill_viridis()
})



test_that("voronoi interpolation based on elevmin for only 1 neighbourhoods", {


  library(ggplot2)
  library(here)


  mypwd <- keyringr::decrypt_gk_pw("user charles")

  #mtl_buildings
  conn <- DBI::dbConnect( drv = RPostgreSQL::PostgreSQL(),
                          host="localhost",
                          user="postgres",
                          password=mypwd,
                          dbname="nrCan_buildings")


  queryStr <- 'SELECT *
              FROM (
              	SELECT q_socio,geom
              	FROM public.quartiers_sociologiques_2014
              	WHERE q_socio LIKE \'Villeray\'
              	) as neigh
              JOIN
              	(SELECT *
              	FROM mtl_buildings
              	WHERE  St_isvalid (geom)
              	 )as buildings
              ON st_intersects(neigh.geom, buildings.geom)'

  shpBuildings <- sf::st_read(conn,
                              query = queryStr)


  shpVoronoi <- getVoronoiInterpolation(shpBuildings,
                                        'elevmin')

  ggplot() +
    geom_sf(data=shpVoronoi, aes(fill=elevmin)) +
    geom_sf(data=shpBuildings %>% sf::st_centroid(), lwd=0.5) +
    viridis::scale_fill_viridis()
})





test_that("voronoi interpolation based on elevmin for only 1 neighbourhoods with projected coordinates", {


  library(ggplot2)
  library(here)


  mypwd <- keyringr::decrypt_gk_pw("user charles")

  #mtl_buildings
  conn <- DBI::dbConnect( drv = RPostgreSQL::PostgreSQL(),
                          host="localhost",
                          user="postgres",
                          password=mypwd,
                          dbname="nrCan_buildings")

  #Transform to 2950
  queryStr <- 'SELECT *
              FROM (
              	SELECT q_socio, st_transform(geom, 2950) as geom
              	FROM public.quartiers_sociologiques_2014
              	WHERE q_socio LIKE \'Villeray\'
              	) as neigh
              JOIN
              	(SELECT elevmin, st_transform(geom, 2950) as geom
              	FROM mtl_buildings
              	WHERE  St_isvalid (geom)
              	 )as buildings
              ON st_intersects(neigh.geom, buildings.geom)'

  shpBuildings <- sf::st_read(conn,
                              query = queryStr)


  shpVoronoi <- getVoronoiInterpolation(shpBuildings,
                                        'elevmin')

  ggplot() +
    geom_sf(data=shpVoronoi, aes(fill=elevmin)) +
    geom_sf(data=shpBuildings %>% sf::st_centroid(), lwd=0.5) +
    viridis::scale_fill_viridis()
})

