#Make sure we turn off the pesky spherical coordinates
check_turn_off_s2()

test_that("qc cd", {
  shp_cd_qc <- download_cd(PR = '24',use_cartographic = F)


  expect_true(nrow(shp_cd_qc) == 98)

})

test_that("qc cd - after caching", {
  shp_cd_qc_0 <- download_cd(PR = '24',use_cartographic = F)
  shp_cd_qc <- download_cd(PR = '24',use_cartographic = F)

  expect_true(nrow(shp_cd_qc) == 98)
  expect_true(sum(is.na(shp_cd_qc$CDUID)) == 0)
})


test_that("on cd", {
  shp_cd_on <- download_cd(PR = '35',use_cartographic = F)

  # see https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=131938&CVD=138862&CPV=35&CST=01012006&CLV=1&MLV=4
  expect_true(nrow(shp_cd_on) == 49)
  expect_true('Thunder Bay' %in% shp_cd_on$CDNAME)

})
