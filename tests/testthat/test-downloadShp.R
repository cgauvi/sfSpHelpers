test_that("geojson download", {

  geojsonUrl <- 'https://www.donneesquebec.ca/recherche/dataset/bc5afddf-9439-4e96-84fb-f91847b722be/resource/bbdca0dd-82df-42f9-845b-32348debf8ab/download/vdq-arbrepotentielremarquable.geojson'
  shp <- st_read_remote( geojsonUrl , dirToDownload = "")

  assertthat::assert_that( any(grepl('sf', class(shp))))
  assertthat::assert_that( nrow(shp) >= 100)

})


test_that("zipped shp", {

  zippedShpFileUrl <- 'https://data.montreal.ca/dataset/4ad6baea-4d2c-460f-a8bf-5d000db498f7/resource/866a3dbc-8b59-48ff-866d-f2f9d3bbee9d/download/uniteevaluationfonciere.geojson.zip'
  shp <- get_zipped_remote_shapefile( zippedShpFileUrl , dirToDownload = here::here("Data"))

  assertthat::assert_that( any(grepl('sf', class(shp))))
  assertthat::assert_that(nrow(shp) >=4)

})

