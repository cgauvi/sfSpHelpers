
#' Download a (zipped) shp file from a url
#'
#' @param url_download
#' @param dirToDownload
#'
#' @return shp downloaded shp file
#' @export
get_zipped_remote_shapefile <- function(url_download, dirToDownload=NA){

  assertthat::assert_that(grepl('.tgz$|.tar.gz$|.zip$',url_download))

  # Point to correct directory: might be temp
  delete_tmp_dir <- F
  if(is.na(dirToDownload) | is.null(dirToDownload) ){
    print('Downloading to tmp directory')
    dir_dl <- tempdir()
    delete_tmp_dir <- T
  }else{
    dir_dl <- dirToDownload
  }
  temp_file <- tempfile()

  # make sure files exist
  if(!file.exists(temp_file)){
    warning("Warning! fuckup with tmp files: forcewriting to data dir")
    temp_file <- here::here('data', 'fallback_temp_file_fuckup')
  }

  if(!dir.exists(dir_dl)){
    warning("Warning! fuckup with tmp dir: forcewriting to data dir")
    dir_dl <- here::here('data', 'fallback_temp_dir_fuckup')
  }

  # Download zipped file
  download.file(url_download,temp_file)

  # Unzip
  if(grepl('.tgz$|.tar.gz$',url_download)){
    utils::untar(temp_file, exdir = dir_dl) #overwrites by default: https://stackoverflow.com/questions/51050502/what-does-tar-overwrite-actually-do-or-not-do
  } else if(grepl('.zip$',url_download)){
    utils::unzip(temp_file, exdir = dir_dl, overwrite = T)
  } else{
    stop('Unsupported filetype')
  }


  shp <- tryCatch({
    # Read the directory (e.g. shp folder)
    sf::st_read(dir_dl)
  },error = function(e){
    # Read a single file (e.g. geojson)
    file_donwloaded <-  basename(url_download) # basename
    file_donwloaded <- gsub(x= file_donwloaded, pattern = '.tgz$|.tar.gz$|.zip$', replacement =  '')# remove the .zip at the end
    sf::st_read(file.path(dir_dl,file_donwloaded))
  })


  #Clean up: important, otherwise we might read in files we downloaded in the past
  unlink(temp_file)
  if (delete_tmp_dir)  tryCatch(unlink(dir_dl, recursive = T), error = function(e) print("Error when deleting tmp dir") ) #dont delete existing dir


  return(shp)

}




