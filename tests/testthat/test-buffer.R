test_that("buffer", {

  shpTrees <- st_read('https://www.donneesquebec.ca/recherche/dataset/bc5afddf-9439-4e96-84fb-f91847b722be/resource/bbdca0dd-82df-42f9-845b-32348debf8ab/download/vdq-arbrepotentielremarquable.geojson')
  shpTrees %<>% st_transform(crs=32198)

  shpTreesBuffered <- shpTrees %>% unionShpBuffer(dist = 1000)

  plot(shpTreesBuffered$geometry)

  assertthat::are_equal(nrow(shpTreesBuffered),13)
})
