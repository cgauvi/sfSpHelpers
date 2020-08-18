test_that("get centroids works", {

  shpBuildings <- st_read(dsn=here::here('Data', 'GeoData', 'QuebecCityOpenData', 'Buildings','vdq-batiments.shp'))
  shpBuildingsWithLngLat <- shpBuildings %>% getCentroids

  assertthat::assert_that( all( c('lng','lat') %in% colnames(shpBuildingsWithLngLat)) )
})
