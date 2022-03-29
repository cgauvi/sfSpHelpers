test_that("kde", {

  library(raster)

  shpBuildings <- st_read('https://www.donneesquebec.ca/recherche/dataset/bc5afddf-9439-4e96-84fb-f91847b722be/resource/bbdca0dd-82df-42f9-845b-32348debf8ab/download/vdq-arbrepotentielremarquable.geojson')
  shpCentroids <- shpBuildings %>% st_centroid

  rasterKDECentroids <- st_kde(shpCentroids,0.01,0.01)
  plot(rasterKDECentroids$layer)
})




test_that("polygons heatmap with sf", {


  shp_trees <- sf::st_read('https://www.donneesquebec.ca/recherche/dataset/bc5afddf-9439-4e96-84fb-f91847b722be/resource/bbdca0dd-82df-42f9-845b-32348debf8ab/download/vdq-arbrepotentielremarquable.geojson')


  shp_polyons <- get_polygon_heatmap(shp_trees , bw=.001, gsize=500 )

  #Make sure there are associated features and at least 2 levels
  assertthat::assert_that(nrow( shp_polyons ) > 1 )
  assertthat::assert_that(n_distinct( shp_polyons$level ) > 1 )
  assertthat::are_equal( sf::st_crs(shp_polyons)$input,  "EPSG:4326" )

  #library(gplot2)
  #ggplot(shp_polyons) + geom_sf(aes(fill=colors),lwd=0) + scale_fill_viridis_d()
}

)




test_that("polygons heatmap with dataframe", {



  csv_url <-"https://data.montreal.ca/dataset/f170fecc-18db-44bc-b4fe-5b0b6d2c7297/resource/c7d0546a-a218-479e-bc9f-ce8f13ca972c/download/localisation_des_compteurs_velo.csv"
  shp_bike_counting_stations <- st_read_remote( csv_url , dirToDownload = "")

  shp_bike_counting_stations %<>% dplyr::mutate(lat=Latitude)
  shp_bike_counting_stations %<>% dplyr::mutate(lng=Longitude)

  shp_polyons <- get_polygon_heatmap(shp_bike_counting_stations,   bw=.001, gsize=500 )

  #Make sure there are associated features and at least 2 levels
  assertthat::assert_that(nrow( shp_polyons ) > 1 )
  assertthat::assert_that(n_distinct( shp_polyons$level ) > 1 )
  assertthat::are_equal( sf::st_crs(shp_polyons)$input,  "EPSG:4326" )

}

)
