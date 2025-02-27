# Non-exported utility functions.

#' Ensure a character string ends in a forward slash
#'
#' @description Ensure a characters string (e.g., a filepath) ends in a forward slash so directories and files are obtained/written correctly by R.
#'
#' @param string a character string.
#'
#' @noRd
#'
ensure_path_slash <- function(string, os = "win"){
  if(os == "win"){
    if(substr(string, nchar(string)-1+1, nchar(string)) != "/"){
      newstring <- paste0(string,"/")
    } else {
      newstring <- string
    }
  } else {
    stop("Oops, OS support only includes windows at this stage.")
  }
  newstring
}

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
    message("Empty object; no path to trim. Files or filenames may be missing.")
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
                             width = nchar(headerbasic3) + wsps_sizes[header_line], pad = " ")
  # file_lines_new[header_line] <- header_line_new
  ## Get data lines
  file_lines_datacheck <- file_lines[(header_line + 1):(length(file_lines))] %>%
    tidy_gsub_int("\\s\\s*", " ") %>% tidy_gsub_int("^ ","")
  file_lines_data <- file_lines[(header_line + 1):(length(file_lines))]
  ## Identify over-flow lines
  if(any(grepl("NaN",file_lines_datacheck))){
    file_lines_data_02 <- file_lines_datacheck %>% vapply(FUN.VALUE = logical(1),
                                                          USE.NAMES = FALSE, function(x) {
                                                            tidy_grepl_int(x, "NaN")
                                                          })
  } else {
    file_lines_data_02 <- file_lines_datacheck %>% vapply(FUN.VALUE = logical(1),
                                                          USE.NAMES = FALSE, function(x) {
                                                            tidy_grepl_int(x, paste0("^", rep("[0-9\\.-]*?",
                                                                                              2) %>% paste(collapse = " "), "$"))
                                                          })
  }
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
    # nle_1 <- strsplit(new_line_elements,"\\s+")
    # nle_ex10 <- nle_1[which(lapply(nle_1,length) > 10)]
    # hrind(which(unlist(lapply(nle_ex10,"[[",10)) == hour_limit))
    hrind <- which(unlist(lapply(strsplit(new_line_elements,"\\s+"),"[[",10)) == hour_limit)
    if(!isempty(hrind)){
      new_line_elements <- new_line_elements[1:hrind]
    }
  }
  file_lines_data3 <- new_line_elements
  new_file <- c(pre_header_lines,header_line_new,file_lines_data3)
  # write(new_file,paste0(test_dir,"Modified_bad_file"))
  new_file
}

#' Shorten endpoint filenames.
#'
#' @description Adaptively shorten a set of supplied filenames by removing the supplied characters.
#'     Special handling of "lat" and "lon" if supplied. Returns the filenames in 'short' format (without preceding path).
#'
#' @param filenames_in a set of filenames to be shortened
#' @param drop_chars a character vector of any number of characters to be removed.
#'
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#'
#' @noRd
#'
shorten_endpt_filenames <- function(filenames_in,
                                    drop_chars = c("traj","YSH","lon","lat")){
  # Constrict filenames.
  if(grep("/",filenames_in[1])){
    filenames_in <- trim_path_int(filenames_in)
  }
  if(is.null(drop_chars)){
    drop_chars <- c("traj","YSH","lon","lat")
  } else if(!is.character(drop_chars)){
    stop("Please provide drop_chars as a character vector.")
  }
  # Now we do the dropping, for each of the character elements.
  if("lat" %in% drop_chars){
    # Lat special - uses regex to get rid of the coordinates too.
    filenames_in <- gsub("lat*[_]([^_]+)[_]", replacement = "_",x = filenames_in)
    drop_chars <- drop_chars[-which(drop_chars == 'lat')]
  }
  if("lon" %in% drop_chars){
    filenames_in <- gsub("lon*[_]([^_]+)[_]", replacement = "_",x = filenames_in)
    drop_chars <- drop_chars[-which(drop_chars == 'lon')]
  }
  # General removal of specified drop chars
  filenames_in <- str_replace_all(filenames_in,paste(drop_chars, collapse = "|"),"")
  # Final checks: repeated underscores, leading and trailing dashes.
  filenames_in <- str_replace(filenames_in,"__","-")
  filenames_in <- str_replace(filenames_in,"--","-")
  filenames_in <- gsub('^\\-|\\-$', '', filenames_in) # Remove leading or trailing dashes
  # Final var pass
  filenames_out <- filenames_in
  filenames_out
}

#' Return the file path component of a given filename
#'
#' @description Given a filename including a full path, return the path.
#'
#' @param filename_full A filename, including path, as a character object.
#'
#' @noRd
#'
return_path <- function(filename_full){
  path_split <- unlist(strsplit(filename_full,"/"))
  path_comb = paste0(paste(path_split[c(1:length(path_split)-1)], collapse = "/"),"/")
  path_comb
}

#' Replace the path of a file with another
#'
#' @description Given a filename including a path, replace this path with another whilst preserving the file name components not from the path.
#'
#' @param filename_full A full filename including path
#' @param new_path the path that will be used in place of the existing one.
#'
#' @noRd
#'
replace_path <- function(filename_full, new_path){
  filename_nopath <- trim_path_int(filename_full)
  filename_new = paste0(ensure_path_slash(new_path),filename_nopath)
  filename_new
}


