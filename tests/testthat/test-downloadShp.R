

test_that("zipped geojson - tmp dir", {

  existing_dir_to_dl <- "data_dl"
  if(dir.exists(existing_dir_to_dl)) unlink(existing_dir_to_dl, recursive = T)
  dir.create(existing_dir_to_dl)

  zippedShpFileUrl <- "https://data.montreal.ca/dataset/4ad6baea-4d2c-460f-a8bf-5d000db498f7/resource/866a3dbc-8b59-48ff-866d-f2f9d3bbee9d/download/uniteevaluationfonciere.geojson.zip"
  shp <- get_zipped_remote_shapefile(zippedShpFileUrl, existing_dir_to_dl)

  expect_true( any(grepl('sf', class(shp))))
  expect_true(nrow(shp) >=4)

})


#
# # Following should be tested, but is just too long to run each time ..
# test_that("zipped files in same existing (non temp) directory", {
#
#   existing_dir_to_dl <- "data_dl"
#   if(dir.exists(existing_dir_to_dl)) unlink(existing_dir_to_dl, recursive = T)
#   dir.create(existing_dir_to_dl)
#
#   # special case since this is a zipped dir of shp files (so we extract a dir and not a list of files)
#   zippedShpFileUrl <- "https://donneesouvertes.affmunqc.net/role/ROLE2022_SHP.zip"
#   shp <- get_zipped_remote_shapefile(zippedShpFileUrl)
#
#   zippedShpFileUrl2 <- "https://data.montreal.ca/dataset/8a1d7d54-c297-46fe-b670-bb205641b13e/resource/467a5bc6-f31f-4831-884f-ecb3abbbce15/download/depots_deneigement_saison_2022-2023.zip"
#   shp_2 <- get_zipped_remote_shapefile(zippedShpFileUrl2, existing_dir_to_dl)
#
#   # dont overwrite
#   expect_false(any(dim(shp) == dim(shp_2)))
#
#   # both files are persisted
#   dir_1  <- gsub(x= basename(zippedShpFileUrl), pattern = '.tgz$|.tar.gz$|.zip$', replacement =  '')
#   file_2 <- paste0(gsub(x= basename(zippedShpFileUrl2), pattern = '.tgz$|.tar.gz$|.zip$', replacement =  ''), ".shp")
#
#   expect_true(dir.exists(file.path(existing_dir_to_dl, dir_1)))
#   expect_true(file.exists(file.path(existing_dir_to_dl, file_2)))
#
# })
#

