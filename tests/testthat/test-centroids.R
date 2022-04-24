test_that("get centroids works", {

  shp_neigh <- st_read( "https://www.donneesquebec.ca/recherche/dataset/5b1ae6f2-6719-46df-bd2f-e57a7034c917/resource/436c85aa-88d9-4e57-9095-b72b776a71a0/download/vdq-quartier.geojson")
  shpBuildingsWithLngLat <- shp_neigh %>% getCentroids

  expect_true( all( c('lng','lat') %in% colnames(shpBuildingsWithLngLat)) )
})
