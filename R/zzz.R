
#' Code to check if s2 should be used and turn off UNLESS `USE_S2_SPHERICAL` is set
#'
check_turn_off_s2 <- function(){

  # Turn off spherical computing - always messes up computations - polygons not designed with this framework in mind
  # By default use_s2 is F since Sys.getenv('USE_S2_SPHERICAL') = '' unless explicitely set and accessed (e.g. in .Renviron file)
  use_s2 <- (Sys.getenv('USE_S2_SPHERICAL') == 'True')

  if(!use_s2){
    warning("Warning USE_S2_SPHERICAL env var is not set! Defaulting to False")
  }else{
    warning("Using spherical coordinates since USE_S2_SPHERICAL = True")
  }

  sf::sf_use_s2(use_s2)
}

check_turn_off_s2()

