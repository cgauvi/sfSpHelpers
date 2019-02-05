##### Main fcts to read in a .shp file as an sp or sf object  #####

#' Read the shp file with rgdal::readOGR
#'
#' Data taken from https://www.donneesquebec.ca/recherche/fr/dataset/vque_9
#'
#' @export
#'
#' @return shp, spatial polygons/point object
#'
#' @examples
readShpFile <- function(pathName,layerName,readSP){


  tryCatch({
    if(readSP){
      print("Reading in shp as SP object")
      shp <-  rgdal::readOGR(file.path(pathName,paste0(layerName,".shp")),
                             layer = layerName)
    } else {
      print("Reading in shp as SF object")

      shp <- sf::st_read(dsn=pathName,
                         layer=layerName)  #Make sure the layer name has no extension
      #make the shape valid
      shp %<>% makeSfValidProjRmZ

    }



  },
  error=function(e) stop(paste0("Error reading layer ",
                                layerName, " from folder ",
                                pathName, " -- ", e))
  )

  return (shp)

}


readShpFileWrapper <- function(rdataPath, rdataName, pathName,layerName,readSP=F){

  shp <- GeneralHelpers::wrapperRdataWrite( readShpFile,
                                                          rdataPath,
                                                          rdataName,
                                                          pathName,
                                                          layerName,
                                                          readSP)

  return(shp)
}


##### Utils/helper fcts  #####



#' Calls auxiliary GeneralHelpers functions to find a shp file in dir
#'
#' @param abspath
#'
#' @return
#' @export
#'
#' @examples
getShpLayerNameInDir <- function(abspath){

  abspath %<>% GeneralHelpers::convertAbsPath
  stopifnot(dir.exists(abspath))

  listShpFiles <- GeneralHelpers::getListFilesExt(dirPath = abspath, ".shp") %>%
    GeneralHelpers::getFileNameNoExt(. ,".shp")

  stopifnot(length(listShpFiles)>=1)

  return(listShpFiles[[1]])

}



