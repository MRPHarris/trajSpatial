# Miscellaneous exported functions


#' Convert a trajectory data frame into openair format
#'
#' @description Convert a standard trajectory dataframe from splitR to a format compatible with the openair package
#'
#' @param trajdata a trajectory dataframe returned from splitr::trajectory_read
#'
#' @importFrom dplyr rename
#'
#' @export
#'
convert_openair <- function(trajdata) {
  x <- trajdata # var pass
  drop_columns_openair = c(
    "lat_i","lon_i","theta","rainfall","mixdepth","rh","sp_humidity",
    "h2o_mixrate","terr_msl","sun_flux", "air_temp") # init columns for dropping
  x <- x[ , !(names(x) %in% drop_columns_openair)] # drop columns
  x$year = 2000 + x$year # year conversion
  x$receptor = 1 # receptor fudge
  x <- x %>% rename(hour.inc = hour_along, date = traj_dt_i, date2 = traj_dt) # various renaming
  return(x)
}

#' Add a unique identifier to each trajectory
#'
#' @description Add a unique numerical identifier to each trajectory for straightforward parsing
#'
#' @param trajdata a trajectory dataframe returned from splitr::trajectory_read.
#' @param ntraj_1 TRUE/FALSE is there only one trajectory per day?
#' @param direction hysplit trajectory model run direction. One of either 'forward' or 'backward'.
#' @param openair_compliant TRUE/FALSE Has the trajectory dataframe already been converted to openair format with convert_openair? Set to FALSE if not.
#'
#' @importFrom magrittr %>%
#' @importFrom chron times
#'
#' @export
#'
add_traj_identifier <- function(trajdata, ntraj_1 = FALSE, direction = "forward", openair_compliant = TRUE){
  x <- trajdata # legacy var pass
  # convert to openair if specified. Required for the function to work, but can be performed in advanced.
  if(!isTRUE(openair_compliant)){
    x <- x %>% convert_openair()
  }
  # This function assumes that convert_openair() has been used on the dataset.
  x_new <- as.data.frame(x)
  # Compute trajectory number. Starts at 1.
  if(!isTRUE(ntraj_1)){
    x_new$start.time.minutes <- substr(x_new[,12],12,19)
    x_new$start.time.minutes <- 60*24*as.numeric(times(x_new$start.time.minutes))
    x_new$trajectory <- cumsum(c(0, as.numeric(diff(x_new$start.time.minutes)) != 0)) + 1
  } else {
    x_new$diffs <- 1
    x_new$diffs[2:nrow(x_new)] <- diff(x_new$hour.inc)
    if(direction == "forward"){
      x_new$trajectory <- cumsum(c(0,as.numeric(x_new$diffs[2:nrow(x_new)]) != 1)) + 1
    } else if(direction == "backward"){
      x_new$trajectory <- cumsum(c(0,as.numeric(x_new$diffs[2:nrow(x_new)]) != -1)) + 1
    }
  }
  x_new
}
