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

#' Find endpoint files that do not match.
#'
#' @description Experimental function. Take two trajectory endpoint filename vectors (old and new) and
#'      find those that are not present in the former. Intended for use where new
#'      endpoint files are added to a pre-existing folder containing e.g., endpoint
#'      files used for a previous cluster analysis.
#'
#' @param prev_endpt_filenames a vector of filenames to check against.
#' @param new_endpt_filenames a vector of filenames to identify new files within (those not in the prev_endpt_filenames).
#' @param exclude_date_bounds TRUE/FALSE to simply identify new trajectories based on whether they occur after the last date in the previous trajectories.
#' @param skip A character vector used to generate a date sequence, e.g. "2 days". This is equivalent to the skip field in the HYSPLIT cluster program. Note that there are some differences - for some years, the HYSPLIT cluster program seems to assume that September has 31 days, for instance.
#'
#' @importFrom magrittr %>%
#' @importFrom rlist list.rbind
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr row_number
#' @importFrom dplyr filter
#'
#' @export
#'
find_new_endpts <- function(prev_endpt_filenames,
                            new_endpt_filenames,
                            exclude_date_bounds = TRUE,
                            skip = "2 days"){
  # NB for sequence: the HYSPLIT date selection is a bit quirky, and doesn't seem
  # to perfectly select for every other day. This ultimately doesn't matter that
  # much but is the reason why, if you really dive into trying to match a sequence of
  # dates with those from a skip = e.g. 2 cluster run, you'll run into differences. E.g.,
  # skipping the first of October every now and then. Maybe it's leap years breaking it or
  # something, I don't know.
  ## test vars
  # prev_endpt_filenames = cluslist_21$V8
  # new_endpt_filenames = list.files(path = "E:/DATA/HYSPLIT FILES/Endpoint files/PH_1500m_240hr_reanalysis_endpoints_norecurse/",
  #                                  full.names = T)
  # exclude_date_bounds = TRUE
  # skip = "2 days"
  # seq_offset = 1
  ## Var pass
  endpt_seq = skip
  ## Trim path
  prev_endpt_filenames_conc <- prev_endpt_filenames %>%
    trim_path_int()
  new_endpt_filenames_conc <- new_endpt_filenames %>%
    trim_path_int()
  ## Format
  endpt_dates_pre <- lapply(strsplit(prev_endpt_filenames_conc,"[-]"),"[",c(6,7,8)) %>%
    lapply(., paste, collapse = "-") %>%
    list.rbind() %>% as.data.frame() %>% 'colnames<-'(c('date')) %>%
    mutate(date_fmt = as.Date(date, "%y-%m-%d")) %>%
    arrange(date_fmt) %>% select(-c(date))
  # New endpoint filenames. This can be a list of filenames from a folder containing new and old; find the difference!
  n_endpt_dates_pre <- lapply(strsplit(new_endpt_filenames_conc,"[-]"),"[",c(6,7,8)) %>%
    lapply(., paste, collapse = "-") %>%
    list.rbind() %>% as.data.frame() %>% 'colnames<-'(c('date')) %>%
    mutate(date_fmt = as.Date(date, "%y-%m-%d")) %>%
    arrange(date_fmt) %>% dplyr::select(-c(date)) %>%
    mutate(rn = row_number())
  # Find min and max in pre
  pre_date_min = min(endpt_dates_pre$date_fmt)
  pre_date_max = max(endpt_dates_pre$date_fmt)
  # Adjust max if seq
  if(!is.null(endpt_seq)){
    num_from_seq = unlist(lapply(strsplit(endpt_seq," "),"[[",1)) %>% as.numeric()
    pre_date_max = as.Date(pre_date_max) + (num_from_seq)
  }
  # Assumes the new trajectories are after the former; band-aid solution
  n_endpt_dates <- n_endpt_dates_pre %>%
    filter(date_fmt >= as.Date(pre_date_max))
  # Sequence to match
  if(!is.null(endpt_seq)){
    # generate sequence
    dateseq <- seq((min(n_endpt_dates$date_fmt)), max(n_endpt_dates$date_fmt), by = "2 days")
    # Trim
    n_endpt_dates <- n_endpt_dates %>%
      filter(date_fmt %in% dateseq)
  }
  # Now get the filenames
  new_fnames <- new_endpt_filenames[n_endpt_dates$rn]
}
