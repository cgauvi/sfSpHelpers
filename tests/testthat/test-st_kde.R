test_that("kde", {

  library(raster)

  shpBuildings <- st_read(dsn=here::here('Data', 'GeoData', 'QuebecCityOpenData', 'Trees','vdq-arbrepotentielremarquable.shp'))
  shpCentroids <- shpBuildings %>% st_centroid

  rasterKDECentroids <- st_kde(shpCentroids,0.01,0.01)
  plot(rasterKDECentroids$layer)
})
