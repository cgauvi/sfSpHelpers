
#' Download a (zipped) shp file from a url
#'
#' @param url
#' @param dirToDownload
#'
#' @return shp downloaded shp file
#' @export
get_zipped_remote_shapefile <- function(url, dirToDownload=here::here("data")){

  assertthat::assert_that(grepl('.tgz$|.tar.gz$|.zip$',url))

  temp_file <- tempfile()
  temp_dir <- tempdir()
  download.file(url,temp_file)


  if(grepl('.tgz$|.tar.gz$',url)){
    utils::untar(temp_file, exdir = temp_dir ) #overwrites by default: https://stackoverflow.com/questions/51050502/what-does-tar-overwrite-actually-do-or-not-do
  } else if(grepl('.zip$',url)){
    utils::unzip(temp_file, exdir =temp_dir,overwrite = T)
  } else{
    stop('Unsupported filetype')
  }


  shp <- sf::st_read(temp_dir)

  unlink(temp_file)
  unlink(temp_dir)


  return(shp)

}




