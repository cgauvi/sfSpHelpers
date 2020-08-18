test_that("kde", {

  shpBuildings <- st_read(dsn=here::here('Data', 'GeoData', 'QuebecCityOpenData', 'Buildings','vdq-batiments.shp'))
  shpCentroids <- shpBuildings %>% st_centroid

  rasterKDECentroids <- st_kde(shpCentroids,0.01,0.01)
  plot(rasterKDECentroids)
})
