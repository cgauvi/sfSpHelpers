test_that("geojson download", {

  geojsonUrl <- 'http://donnees.ville.montreal.qc.ca/dataset/71433534-694b-4538-8cd7-86530790ab0c/resource/7e2dc3a5-4e92-4aa1-9da3-377d992a9b19/download/fleuve_montagne_attractions.geojson'
  shp <- st_read_remote( geojsonUrl , dirToDownload = "")

  assertthat::assert_that( any(grepl('sf', class(shp))))
  assertthat::assert_that( nrow(shp) >= 100)

})


test_that("zipped shp", {

  zippedShpFileUrl <- 'http://donnees.ville.montreal.qc.ca/dataset/71433534-694b-4538-8cd7-86530790ab0c/resource/e6dcc90d-c39a-4223-bb1f-307c24a426ed/download/fleuve_montagne_polylines.zip'
  shp <- get_zipped_remote_shapefile( zippedShpFileUrl , dirToDownload = here("Data"))

  assertthat::assert_that( any(grepl('sf', class(shp))))
  assertthat::assert_that(nrow(shp) >=4)

})

