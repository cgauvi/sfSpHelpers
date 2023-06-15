#Make sure we turn off the pesky spherical coordinates
check_turn_off_s2()

test_that("qc adm regions <-> econ regions", {
  shp_er_qc <- download_econ_regions(PR = '24',use_cartographic = F)

  expect_true(nrow(shp_er_qc) == 17)

})

test_that("qc adm regions <-> econ regions - after caching", {
  shp_er_qc_0 <- download_econ_regions(PR = '24',use_cartographic = F)
  shp_er_qc <- download_econ_regions(PR = '24',use_cartographic = F)

  expect_true(nrow(shp_er_qc) == 17)
  expect_true(sum(is.na(shp_er_qc$ERUID)) == 0)
})


test_that("on econ regions", {
  shp_er_on <- download_econ_regions(PR = '35',use_cartographic = F)

  # see https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=131938&CVD=138862&CPV=35&CST=01012006&CLV=1&MLV=4
  expect_true(nrow(shp_er_on) == 11)

})
