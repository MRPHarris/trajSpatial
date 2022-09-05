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

#' Wrapper for `grepl()` where `x` is the first argument
#'
#' Direct rip of the tidy_grepl function from splitR. This function is
#' wrapper for `grepl()` that uses default argument values and
#' rearranges first three arguments for better pipelining.
#' @param x,pattern Select arguments from the `grepl()` function.
#'
#' @noRd
#'
tidy_grepl_int <- function(x, pattern) {
  vapply(
    pattern,
    FUN = function(pattern) {
      grepl(pattern = pattern, x = x)
    },
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
}

#' Wrapper for `gsub()` where `x` is the first argument
#'
#' Direct rip of the tidy_grepl function from splitR. This function is
#' wrapper for `gsub()` that uses default argument values and
#' rearranges first three arguments for better pipelining.
#' @param x,pattern,replacement,fixed Select arguments from the `gsub()` function.
#'
#' @noRd
#'
tidy_gsub_int <- function(x, pattern, replacement, fixed = FALSE) {
  gsub(pattern, replacement, x, fixed = fixed)
}

#' Check endpt file for extended met information
#'
#' @description Read in an ASCII file and check if it contains extended meteorological information.
#'         Files containing extended met may cause the clustering function in HYSPLIT to fail.
#'         Adapted directly from splitR's `trajectory_read()`
#'
#' @param file full file-path to a single trajectory endpt file output by the HYSPLIT model.
#'
#' @noRd
#'
endpt_metcheck_single <- function(file){
  file_path <- file
  file_one_line <- readr::read_file(file_path)
  if(file_one_line %>% tidy_grepl_int("AIR_TEMP")){
    checkmet <- TRUE
  } else {
    checkmet <- FALSE
  }
  return(checkmet)
}

#' Remove extended met information from an endpoint file
#'
#' @description Read in and remove extended data from a single endpoint file.
#'
#' @param endpt_file full file-path to a single trajectory endpt file output by the HYSPLIT model.
#' @param hour_limit numeric or character for the hour limit for endpoints (e.g. "120" will trim each endpoint file to <=120 hours). Fewer hours = faster clustering. Use a negative for backwards trajectories.
#'
#' @importFrom stringr str_count
#' @importFrom stringr str_split
#' @importFrom stringr str_pad
#' @importFrom stringr str_detect
#'
#' @noRd
#'
format_endpt_forcluster <- function(endpt_file,
                                    hour_limit = -120){
  # Var passing to match original splitr syntax
  file_i_path <- endpt_file
  file_lines <- readLines(file_i_path, encoding = "UTF-8",
                          skipNul = TRUE)
  ## Pre-allocate new file
  file_lines_new <- file_lines
  ## Get number of spaces each line has to be padded with
  wsps_sizes <- lapply(file_lines, function(x){
    str_count(x, "\\G ")
  }) %>%
    unlist(recursive = F)
  ## Get header line
  header_line <- file_lines %>% vapply(FUN.VALUE = logical(1),
                                       USE.NAMES = FALSE, function(x) grepl("PRESSURE", x)) %>%
    which()
  ## Pre-header lines. to be preserved.
  pre_header_lines <- file_lines[1:header_line-1]
  ## HEADER LINE MODIFICATION.
  elements <- unlist(str_split(file_lines[header_line]," +"))
  elements_keep <- elements[which(lapply(elements,function(el){
    t <- nchar(el)
    t
  }) %>% unlist() > 0)[1:2]]
  headerbasic <- paste(elements_keep, collapse = " ", sep = ' ')
  headerbasic2 <- unlist(str_split(headerbasic," "))
  headerbasic2[1] <- "1"
  headerbasic3 <- paste(headerbasic2,collapse = ' ')
  # headerbasic_element_2 <- unlist(str_split(headerbasic," "))
  header_line_new <- str_pad(string = headerbasic3,
                             side = 'left',
                             width = nchar(headerbasic3) + wsps_sizes[11], pad = " ")
  # file_lines_new[header_line] <- header_line_new
  ## Get data lines
  file_lines_datacheck <- file_lines[(header_line + 1):(length(file_lines))] %>%
    tidy_gsub_int("\\s\\s*", " ") %>% tidy_gsub_int("^ ","")
  file_lines_data <- file_lines[(header_line + 1):(length(file_lines))]
  ## Identify over-flow lines
  file_lines_data_02 <- file_lines_datacheck %>% vapply(FUN.VALUE = logical(1),
                                                        USE.NAMES = FALSE, function(x) {
                                                          tidy_grepl_int(x, paste0("^", rep("[0-9\\.-]*?",
                                                                                            2) %>% paste(collapse = " "), "$"))
                                                        })
  #
  file_lines_data2 <- file_lines_data[which(!file_lines_data_02)]
  ## Remove cluster-incompatible data
  new_line_elements <- lapply(file_lines_data2, function(line){
    new_line <- substr(line,1,92)
    new_line
  }) %>% unlist()
  ## hour adjustments
  if(!is.null(hour_limit)){
    hour_limit <- as.character(hour_limit)
    if(!str_detect(hour_limit,"[.]0")){
      hour_limit <- paste0(hour_limit,".0")
    }
    hrind <- which(unlist(lapply(strsplit(new_line_elements,"\\s+"),"[[",10)) == hour_limit)
    new_line_elements <- new_line_elements[1:hrind]
  }
  file_lines_data3 <- new_line_elements
  new_file <- c(pre_header_lines,header_line_new,file_lines_data3)
  # write(new_file,paste0(test_dir,"Modified_bad_file"))
  new_file
}

