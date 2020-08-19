test_that("raw num centroids + median - villeray", {

  library(ggplot2)
  library(sf)
  library(here)


  mypwd <- keyringr::decrypt_gk_pw("user charles")

  #mtl_buildings
  conn <- DBI::dbConnect( drv = RPostgreSQL::PostgreSQL(),
                       host="localhost",
                       user="postgres",
                       password=mypwd,
                       dbname="statCan_buildings")


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

  shpBuildings <- st_read(conn,
                          query = queryStr)


  #No need to get the centroids, spatialKMeans does it automatically
  shpBuildingsAgg <- spatialKMeans (shpBuildings,
                            numCentroids = 10**3,
                            numClosestPoints = 10,
                            var="heightmax",
                            aggFct=median)
  #Check same crs
  assertthat::are_equal(st_crs(shpBuildingsAgg), st_crs(shpBuildings))

  #Number of centroids
  assertthat::are_equal(nrow(shpBuildingsAgg) , 10**3)

  #Plot to see the difference
  shpBuildingsAgg$id <- 'aggregated'
  shpBuildings$id <- 'raw'
  shpBuildings %<>% dplyr::rename(geometry=geom)

  shpBoth <- rbind(shpBuildingsAgg,
                   shpBuildings %>% dplyr::select( colnames(shpBuildingsAgg)))

  ggplot(shpBoth) +
    geom_sf(aes(col=heightmax) ) +
    facet_wrap(~id, ncol=1)
})



test_that("proportion of centroids + median - villeray", {

  library(ggplot2)
  library(sf)
  library(here)


  mypwd <- keyringr::decrypt_gk_pw("user charles")

  #mtl_buildings
  conn <- DBI::dbConnect( drv = RPostgreSQL::PostgreSQL(),
                          host="localhost",
                          user="postgres",
                          password=mypwd,
                          dbname="statCan_buildings")


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

  shpBuildings <- st_read(conn,
                          query = queryStr)


  #No need to get the centroids, spatialKMeans does it automatically
  shpBuildingsAgg <- spatialKMeans (shpBuildings,
                                    propToKeep = 0.1,
                                    numClosestPoints = 10,
                                    var="heightmax",
                                    aggFct=median)
  #Check same crs
  assertthat::are_equal(st_crs(shpBuildingsAgg), st_crs(shpBuildings))

  #Number of centroids
  assertthat::are_equal(nrow(shpBuildingsAgg) , ceiling(0.1*nrow(shpBuildings)))

  #Plot to see the difference
  shpBuildingsAgg$id <- 'aggregated'
  shpBuildings$id <- 'raw'
  shpBuildings %<>% dplyr::rename(geometry=geom)

  shpBoth <- rbind(shpBuildingsAgg,
                   shpBuildings %>% dplyr::select( colnames(shpBuildingsAgg)))

  ggplot(shpBoth) +
    geom_sf(aes(col=heightmax) ) +
    facet_wrap(~id, ncol=1)
})




test_that("proportion of centroids + median - villeray", {

  library(ggplot2)
  library(sf)
  library(here)


  mypwd <- keyringr::decrypt_gk_pw("user charles")

  #mtl_buildings
  conn <- DBI::dbConnect( drv = RPostgreSQL::PostgreSQL(),
                          host="localhost",
                          user="postgres",
                          password=mypwd,
                          dbname="statCan_buildings")


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

  shpBuildings <- st_read(conn,
                          query = queryStr)


  #No need to get the centroids, spatialKMeans does it automatically
  shpBuildingsAgg <- spatialKMeans (shpBuildings,
                                    propToKeep = 0.1,
                                    numClosestPoints = 10,
                                    var="heightmax",
                                    aggFct=median)
  #Check same crs
  assertthat::are_equal(st_crs(shpBuildingsAgg), st_crs(shpBuildings))

  #Number of centroids
  assertthat::are_equal(nrow(shpBuildingsAgg) , ceiling(0.1*nrow(shpBuildings)))

  #Plot to see the difference
  shpBuildingsAgg$id <- 'aggregated'
  shpBuildings$id <- 'raw'
  shpBuildings %<>% dplyr::rename(geometry=geom)

  shpBoth <- rbind(shpBuildingsAgg,
                   shpBuildings %>% dplyr::select( colnames(shpBuildingsAgg)))

  ggplot(shpBoth) +
    geom_sf(aes(col=heightmax) ) +
    facet_wrap(~id, ncol=1)
})
