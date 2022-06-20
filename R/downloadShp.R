
#' Download a (zipped) shp file from a url
#'
#' @param url
#' @param dirToDownload
#'
#' @return shp downloaded shp file
#' @export
get_zipped_remote_shapefile <- function(url, dirToDownload=NA){

  assertthat::assert_that(grepl('.tgz$|.tar.gz$|.zip$',url))

  delete_tmp_dir <- F
  if(is.na(dirToDownload) | is.null(dirToDownload) ){
    print('Downloading to tmp directory')
    dir_dl <- tempdir()
    delete_tmp_dir <- T
  }else{
    dir_dl <- dirToDownload
  }
  temp_file <- tempfile()
  
  download.file(url,temp_file)


  if(grepl('.tgz$|.tar.gz$',url)){
    utils::untar(temp_file, exdir = dir_dl ) #overwrites by default: https://stackoverflow.com/questions/51050502/what-does-tar-overwrite-actually-do-or-not-do
  } else if(grepl('.zip$',url)){
    utils::unzip(temp_file, exdir =dir_dl,overwrite = T)
  } else{
    stop('Unsupported filetype')
  }


  shp <- sf::st_read(dir_dl)

  unlink(temp_file)
  if(delete_tmp_dir) unlink(dir_dl)

  return(shp)

}




