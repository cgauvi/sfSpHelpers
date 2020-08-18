
#' Fct that wraps st_read with a curl call
#'
#' Can read geojsons for instance
#'
#' Tries to download the file to dirToDownload/nameOfFile for persistance.
#' If this fails, it just downloads it to a tmp file.
#' If dirToDownload/nameOfFile already exists, then it just reads back the shp file.
#' nameOfFile is created from the url path
#'
#' @param url
#' @param dirToDownload
#'
#' @return
#' @export
#'
#' @examples
st_read_remote <- function(url, dirToDownload= here('Data')){


  destFile <- tryCatch({
    urlPath <- httr::parse_url(url)$path

    splitUrl <- strsplit(urlPath, split='/')
    destFile <- file.path(dirToDownload, splitUrl[[1]][[length(splitUrl[[1]])]])
  },error=function(e){
    print(glue::glue("Fatal error with url {url}\n{e}\nsaving to root: {here::here()}"))
    return( tempfile()  )
  })

  #Check if file already exists
  if(file.exists(destFile)){
    print('Shp file already exists...')
    return( sf::st_read(destFile) )
  }

  print('Downloading shp file...')

  #If not download it
  curl::curl_download(url = url,
                destfile = destFile )



  return( st_read(destFile) )


}

get_zipped_remote_shapefile <- function(path, dirToSave=here("Data")){

  require(glue)
  require(here)
  require(sf)
  require(httr)
  require(curl)

  h <- new_handle()
  handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer=0)


  dest_dir = here::here("Data")
  pathRootNoExt <- tools::file_path_sans_ext( basename(path)  )

  if(grepl('http',path)){

    #Create the Data/subdir if required
    GeneralHelpers::createDirIfNotExistentRecursive(file.path(dirToSave,pathRootNoExt))

    #Download the file
    dest <- file.path(dest_dir, pathRootNoExt, glue("{pathRootNoExt}.zip") )

    #Check if the shp exists
    shpPath <- file.path(dest_dir, pathRootNoExt, glue("{pathRootNoExt}.shp|geojson") )
    if ( file.exists( shpPath  )){
      return (  sf::st_read(shpPath,quiet=TRUE) )
    }

    curl::curl_download(url= path,
                  destfile = dest ,
                  handle = h)
    path <- dest
  }

  if(grepl('.tgz$|.tar.gz$',path)){
    utils::untar(path, exdir = file.path(dest_dir, pathRootNoExt))
  } else if(grepl('.zip$',path)){
    utils::unzip(path, exdir = file.path(dest_dir, pathRootNoExt))
  } else{
    stop('Unsupported filetype')
  }

  file.remove(path)

  shape_name = grep('.shp|geojson',list.files(dest_dir),value=T)
  setwd(dest_dir)
  sf::st_read(shape_name,quiet=TRUE)
}



