#' Split lines into smaller segments
#'
#' Taken from https://gist.github.com/dblodgett-usgs/cf87392c02d73f1b7d16153d2b66a8f3
#'
#'
#' @title split lines
#' @description Splits lines longer than a given threshold into the minimum number of pieces to all be under the given threshold.
#'
#' @param max_length maximum segment length to return
#' @param input_lines
#' @param id name of ID column in data.frame
#'
#' @return only the split lines.
#' @importFrom dplyr group_by ungroup filter select mutate
#' @export
#'
split_lines <- function(input_lines, max_length, id = "ID") {


  geom_column <- attr(input_lines, "sf_column")

  input_crs <- sf::st_crs(input_lines)

  input_lines[["geom_len"]] <- sf::st_length(input_lines[[geom_column]])

  attr(input_lines[["geom_len"]], "units") <- NULL
  input_lines[["geom_len"]] <- as.numeric(input_lines[["geom_len"]])

  too_long <- filter(select(input_lines, id, geom_column, geom_len), geom_len >= max_length)

  rm(input_lines) # just to control memory usage in case this is big.

  too_long <- mutate(too_long,
                     pieces = ceiling(geom_len / max_length),
                     fID = 1:nrow(too_long)) %>%
    select(-geom_len)

  split_points <- sf::st_set_geometry(too_long, NULL)[rep(seq_len(nrow(too_long)), too_long[["pieces"]]),] %>%
    select(-pieces)

  split_points <- mutate(split_points, split_fID = row.names(split_points)) %>%
    group_by(fID) %>%
    mutate(piece = 1:n()) %>%
    mutate(start = (piece - 1) / n(),
           end = piece / n()) %>%
    ungroup()

  new_line <- function(i, f, t) {
    lwgeom::st_linesubstring(x = too_long[[geom_column]][i], from = f, to = t)[[1]]
  }

  split_lines <- apply(split_points[c("fID", "start", "end")], 1,
                       function(x) new_line(i = x[["fID"]], f = x[["start"]], t = x[["end"]]))

  rm(too_long)

  split_lines <- st_sf(split_points[c(id, "split_fID")], geometry = st_sfc(split_lines, crs = input_crs))

  return(split_lines)
}



#' Splits line into segments using SfSpHelpers::split_lines and then removes the first and last segments (if possible)
#'
#' @param shp_line
#' @param id_col
#' @param min_distance_m_to_remove
#'
#' @return
#'
#' @examples
split_lines_remove_line_endpoints <- function(shp_line,id_col, min_distance_m_to_remove=10){

  #Quick tests
  assertthat::assert_that(any('sf' %in% class(shp_line)))
  assertthat::assert_that(nrow(shp_line)==1, msg='Fatal error in split_lines_remove_line_endpoints! need to sed in sf object with a single row')

  #Reproject and split/segmentize
  shp_line  %<>% st_transform(crs=3857)
  split_lines <- split_lines(shp_line, min_distance_m_to_remove, id = id_col)

  #Inspect results and try to throw out the endpoints (first and last segments from the new split linestring)
  if(nrow(split_lines) == 1){
    print('Warning! only a single line created! returning it')
    lines_return <- split_lines
  }else if(nrow(split_lines) == 2){
    print('Warning! only 2 lines created! returning the first endpoint')
    lines_return <- split_lines[1:2, ]
  }else{
    lines_return <- split_lines[2:nrow(split_lines)-1, ]
  }

  assertthat::assert_that(nrow(lines_return) <= nrow(split_lines) )

  return(lines_return)
}


#' Splits line into segments using SfSpHelpers::split_lines and then removes the first and last segments (if possible)
#' and then groups the line string by id
#'
#' Not exported, only kept as reference as it peforms the same function as remove_line_endpoints_lwgeom which
#' is preferable
#'
#' @param shp_line
#' @param id_col
#' @param min_distance_m_to_remove
#'
#' @return
#'
#' @examples
#'
#' @param shp_line
#' @param id_col
#' @param min_distance_m_to_remove
#'
#' @return
#'
#' @examples
remove_line_endpoints_from_split <- function(shp_line,id_col, min_distance_m_to_remove=10){

  lines_return <- split_lines_remove_line_endpoints(shp_line,id_col, min_distance_m_to_remove=10)

  lines_return_single_multilinestring <- lines_return %>%
    group_by(vars(id_col)) %>%
    summarise()

  return(lines_return_single_multilinestring)

}

#' Get a substring from a linestring
#'
#' Wrapper over lwgeom::st_linesubstring with possibility to either use meters or
#'
#' Not exported: remove_line_endpoints wraps this function and is the function to use
#'
#' remove_line_endpoints_lwgeom performs the same functionality as remove_line_endpoints_from_split but is
#' faster, more robust, and allows for fractions to be inputed, not just distances in meters
#'
#' @param shp_line
#' @param id_col
#' @param min_distance_m_to_remove distance to remove EACH SIDE: so 2X for total removed
#' @param min_proportion_remove proportion to remove EACH SIDE: so 2X for total removed
#'
#' @return
#'
#' @examples
remove_line_endpoints_lwgeom <- function(shp_line,
                                         id_col,
                                         min_distance_m_to_remove=NULL,
                                         min_proportion_remove=NULL){

  assertthat::assert_that(any(id_col %in% colnames(shp_line)))
  assertthat::assert_that( sum(sapply(list(min_distance_m_to_remove,min_proportion_remove), purrr::is_null)) %% 2 ==1,
                           msg='Fatal error! input exaclty one of min_distance_m_to_remove or min_proportion_remove')

  #Convert to a proportion of total segment length when meters are used
  if(!is.null(min_distance_m_to_remove)){
    assertthat::assert_that( 0 < min_distance_m_to_remove , msg='Fatal error! use a distance to remove > 0')
    from <- min_distance_m_to_remove/units::drop_units(st_length(shp_line))
  }else{
    assertthat::assert_that( 0 < min_proportion_remove & min_proportion_remove <1 , msg='Fatal error! use a proportion in (0,1)')
    from <- min_proportion_remove
  }

  #Quick check that we cannot remove more than the total segment length
  if(from >= 0.5){
    stop(paste0('Cannot remove such a large segment! Would remove (for both sides): ',  2*from*100 , ' % of the segment' ))
  }

  to <- 1-from

  #This will still run if from > to, but we should be more careful
  lines_return <- lwgeom::st_linesubstring(x = shp_line, from = from, to = to)

  return(lines_return)

}




#' Removes the endpoints for a collection of LINESTRINGS
#'
#' @param id_col
#' @param min_distance_m_to_remove
#' @param min_proportion_remove
#' @param shp_lines sf object with multiple rows
#'
#' @return
#' @export
#'
#' @examples
remove_line_endpoints <- function(shp_lines,
                                  id_col,
                                  min_distance_m_to_remove=NULL,
                                  min_proportion_remove=NULL){

  #Remove endpoints from all rows
  list_lines_no_endpoints <- lapply(1:nrow(shp_lines),
                                    function(x) remove_line_endpoints_lwgeom(shp_lines [x, ],
                                                                             id_col,
                                                                             min_distance_m_to_remove,
                                                                             min_proportion_remove )
  )

  shp_lines_no_endpoints <- do.call(rbind, list_lines_no_endpoints)

  #Reproject
  shp_lines_no_endpoints %<>% st_transform(crs=st_crs(shp_lines))

  assertthat::assert_that(nrow(shp_lines_no_endpoints) == nrow(shp_lines))


  return(shp_lines_no_endpoints)
}


