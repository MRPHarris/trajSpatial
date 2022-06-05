# Non-exported utility functions.

#' Remove the file path from a given file or folder name. Borrowed from SampleQueue package.
#'
#' @description Removes the file or folder path from a given file/folder name by splitting the path at every "/", and getting rid of all but the last set of characters. If given a short filename (no path), the same string will just be spat out the other end unchanged.
#'
#' @param filnames a string containing the full file path.
#'
#' @noRd
#'
trim_path_int <- function(filenames){
  if(length(filenames) > 1){
    it_list <- vector(mode = "list", length = length(filenames))
    trimmed_filenames <- vector(mode = "character", length = length(filenames))
    for(f in seq_along(filenames)){
      filename_trimmed <- unlist(strsplit(filenames[f],"/"))[length(unlist(strsplit(filenames[f],"/")))]
      trimmed_filenames[f] <- filename_trimmed
    }
    trimmed_filenames
  } else if(length(filenames) == 1){
    filename <- filenames
    filename_trimmed <- unlist(strsplit(filename,"/"))[length(unlist(strsplit(filename,"/")))]
    filename_trimmed
  } else{
    message("Empty object; no path to trim. Filenames may be missing.")
  }
}

#' Add vars to trajectory file used for temporal analysis
#'
#' @description Adds a number of columns used for time-based analysis of endpoints.
#'
#' @param traj_data a dataframe containing trajectory endpoints.
#' @param verbose TRUE/FALSE to generate init message.
#'
#' @noRd
#'
format_trajdata_timevars <- function(traj_data, verbose = FALSE){
  if(isTRUE(verbose)){
    message("Formatting trajectory data. This may take some time for larger tables.")
  }
  formatted_traj_data <- traj_data %>%
    mutate(hour_start = lubridate::hour(date.start)) %>%
    mutate(day_start = lubridate::day(date.start)) %>%
    mutate(month_start = lubridate::month(date.start)) %>%
    mutate(year_start = lubridate::year(date.start)) %>%
    mutate(hour_seq = cumsum(c(0, as.numeric(diff(lubridate::hour(date.start))) != 0)) + 1) %>%
    mutate(day_seq = cumsum(c(0, as.numeric(diff(lubridate::day(date.start))) != 0)) + 1) %>%
    mutate(month_seq = cumsum(c(0, as.numeric(diff(lubridate::month(date.start))) != 0)) + 1) %>%
    mutate(year_seq = cumsum(c(0, as.numeric(diff(lubridate::year(date.start))) != 0)) + 1)
  formatted_traj_data
}
