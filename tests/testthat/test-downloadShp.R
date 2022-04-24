

test_that("zipped shp", {

  zippedShpFileUrl <- 'https://data.montreal.ca/dataset/4ad6baea-4d2c-460f-a8bf-5d000db498f7/resource/866a3dbc-8b59-48ff-866d-f2f9d3bbee9d/download/uniteevaluationfonciere.geojson.zip'
  shp <- get_zipped_remote_shapefile( zippedShpFileUrl , dirToDownload = here::here("data"))

  expect_true( any(grepl('sf', class(shp))))
  expect_true(nrow(shp) >=4)

})

