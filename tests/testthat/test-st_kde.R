test_that("kde", {

  shpBuildings <- sf::st_read('https://www.donneesquebec.ca/recherche/dataset/bc5afddf-9439-4e96-84fb-f91847b722be/resource/bbdca0dd-82df-42f9-845b-32348debf8ab/download/vdq-arbrepotentielremarquable.geojson')
  shpCentroids <- shpBuildings %>% sf::st_centroid()

  rasterKDECentroids <- st_kde(shpCentroids,0.01,0.01)

  expect_equal(dim(rasterKDECentroids), c(20,26,1))

})




test_that("polygons heatmap with sf", {

  shp_trees <- sf::st_read('https://www.donneesquebec.ca/recherche/dataset/bc5afddf-9439-4e96-84fb-f91847b722be/resource/bbdca0dd-82df-42f9-845b-32348debf8ab/download/vdq-arbrepotentielremarquable.geojson')

  shp_polyons <- get_polygon_heatmap(shp_trees, bw=.001, gsize=500)

  #Make sure there are associated features and at least 2 levels
  expect_true(nrow(shp_polyons) > 1)
  expect_true(n_distinct( shp_polyons$level ) > 1)
  expect_true(grepl(sf::st_crs(shp_polyons)$input, "WGS 84|EPSG:4326"))

}

)




test_that("polygons heatmap with dataframe", {

  csv_url <-"https://data.montreal.ca/dataset/f170fecc-18db-44bc-b4fe-5b0b6d2c7297/resource/c7d0546a-a218-479e-bc9f-ce8f13ca972c/download/localisation_des_compteurs_velo.csv"
  shp_bike_counting_stations <-  read.csv(csv_url)

  shp_bike_counting_stations %<>% dplyr::mutate(lat=Latitude)
  shp_bike_counting_stations %<>% dplyr::mutate(lng=Longitude)

  shp_polyons <- get_polygon_heatmap(shp_bike_counting_stations,   bw=.001, gsize=500 )

  #Make sure there are associated features and at least 2 levels
  expect_true(nrow( shp_polyons ) > 1 )
  expect_true(n_distinct( shp_polyons$level ) > 1 )
  expect_equal( sf::st_crs(shp_polyons)$input,  "EPSG:4326" )

}

)
