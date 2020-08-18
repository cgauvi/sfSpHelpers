test_that("buffer", {

  shpTrees <- st_read(dsn=here::here('Data', 'GeoData', 'QuebecCityOpenData', 'Trees','vdq-arbrepotentielremarquable.shp'))
  shpTrees %<>% st_transform(crs=32198)

  shpTreesBuffered <- shpTrees %>% unionShpBuffer(dist = 1000)

  plot(shpTreesBuffered$geometry)

  assertthat::are_equal(nrow(shpTreesBuffered),13)
})
