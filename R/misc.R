# Miscellaneous exported functions


#' Convert a trajectory data frame into openair format
#'
#' @description Convert a standard trajectory dataframe from splitR to a format compatible with the openair package
#'
#' @param trajdata a trajectory dataframe returned from splitr::trajectory_read
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
