# Functions for collating endpoint files for cluster analysis.


#' Archive all endpoints currently in the cluster endpoints directory
#'
#' @description Creates a folder within the specified archive directory, and moves all non-default files currently in the
#'    cluster endpt directory to that folder.
#'
#' @param cluster_endpoints_dir character vecotor. Full file path to the cluster endpoints directory.
#' @param cluster_endpoints_archive_dir character vector. Full file path to the desired archive directory.
#' @param archive_name character vector to be added to the folder containing the archived endpoints.
#' @param stopifempty TRUE or FALSE to stop the operation if there are no endpoints to archive.
#' @param append_timestamp TRUE or FALSE to add a timestamp of the archive operation.
#'
#' @export
#'
archive_cluster_endpoints <- function(cluster_endpoints_dir = "C:/hysplit/cluster/endpts/",
                                      cluster_endpoints_archive_dir = "C:/hysplit/cluster/endpts/archive/",
                                      archive_name = "endpts",
                                      stopifempty = FALSE,
                                      append_timestamp = TRUE){
  # First, list all files in the endpts folder.
  endpts_filelist <- list.files(path = cluster_endpoints_dir, full.names = TRUE)
  # Files to keep.
  keep_files <- c("C:/hysplit/cluster/endpts/archive", "C:/hysplit/cluster/endpts/test", "C:/hysplit/cluster/endpts/readme_endpts.txt")
  endpts_filelist <- endpts_filelist[-match(keep_files,endpts_filelist)] # remove the files to keep from the file list.
  # Are there endpoints to archive?
  if(length(endpts_filelist) == 0){
    # No.
    if(isTRUE(stopifempty)){
      stop("No files in ",cluster_endpoints_dir," to archive.")
    } else {
      message("No files in ",cluster_endpoints_dir," to archive.")
      # pass_stop = FALSE
    }
  } else if(length(endpts_filelist) != 0){
    # Yes: proceed.
    # Folder renaming.
    if(!is.null(append_timestamp)){
      hour <- substr(Sys.time(),12,13) # get current time
      min <- substr(Sys.time(),15,16)
      sec <- substr(Sys.time(),18,19)
      archive_folder_name <- paste0("archive-",archive_name,"-date-",Sys.Date(),"-hms-",hour,"-",min,"-",sec)
    } else {
      archive_folder_name <- paste0("archive-",archive_name)
    }
    dir.create(path = paste0(cluster_endpoints_archive_dir,archive_folder_name)) # create archive folder
    file.copy(endpts_filelist,paste0(cluster_endpoints_archive_dir,archive_folder_name)) # move all files in endpts_filelist to that directory.
    file.remove(endpts_filelist) # remove files from endpts directory
    if(!file.exists(endpts_filelist[1])){
      message("Archiving successful. Endpoint files in ",cluster_endpoints_dir," archived to ",paste0(cluster_endpoints_archive_dir,archive_folder_name))
    } else{
      message("Oops! There may have been an issue with the archiving process. Check directory paths and file status.")
    }
  }# check that there are actually endpoint files present. If not, archive them.
}

#' Prepare date-matched endpoint files for cluster analysis.
#'
#' @description Moves endpoints matching a supplied vector of dates to the specified directory. Options for filename shortening.
#'
#' @param from_dir full path to the folder where the endpoints are currently stored.
#' @param recurse TRUE or FALSE to recurse into sub-directories of the specified folder
#' @param to_dir full file path to the destination folder. C drive default hysplit install cluster folder by default
#' @param date_vec Optional; a vector of formatted dates in the YYYY-MM-DD format.
#' @param hour_vec Optional; a numeric vector of double-digit hour intervals matching the desired day-hours starting from 0. E.g. "12" for midday. If supplied, will be combined with the date vector - thus, there should be an hour for every date entry if you choose to specify hours.
#' @param rename_long_files Optional; a character vector of strings to remove from filenames. See the internal 'shorten_endpt_filenames'. The HYSPLIT clustering will fail if any filenames exceed 54 characters.
#'
#' @export
#'
collate_endpts <- function(from_dir,
                                    recurse = FALSE,
                                    to_dir = "C:/hysplit/cluster/endpts/",
                                    date_vec = NULL,
                                    hour_vec = NULL,
                                    rename_long_files = c("traj","YSH","lon","lat")){
  # Format datevec for indexing
  if(!is.null(date_vec)){
    datevec_char <- format(date_vec, "%y-%m-%d")
    if(!is.null(hour_vec)){
      datevec_char <- paste0(datevec_char,"-",hour_vec)
    }
  }
  # Get files
  all_endpoints <- list.files(from_dir, full.names = TRUE, recursive = recurse)[grep('traj',list.files(from_dir, full.names = FALSE))]
  # Extract the files matching the datevec.
  if(!is.null(date_vec)){
    filematch <- grep(paste(datevec_char,collapse="|"),
                      all_endpoints, value=TRUE)
  } else {
    filematch <- all_endpoints
  }
  # Get longest length of filematches.
  char_check <- max(unique(nchar(filematch)), na.rm = T)
  if(char_check > 54){
    if(is.null(rename_long_files)){
      message("At least of the target endpoints has a filename longer than 54 characters. Clustering will probably fail as a result.")
    }
  }
  # Optional renaming of files in case they exceed 54 characters.
  if(!is.null(rename_long_files)){
    to_files <- paste0(to_dir,shorten_endpt_filenames(filematch, drop_chars = rename_long_files))
    char_check2 <- max(unique(nchar(trim_path_int(to_files))), na.rm = T)
    if(char_check2 > 54){
      message("After shortening, at least of the target endpoints still has a filename longer than 54 characters. Consider supplying more characters to remove.")
    }
  } else {
    to_files <- to_dir
  }
  # Do the file copy
  file.copy(filematch,to_files)
  if(file.exists(to_files)){
    message("Endpoint files transferred to specified directory.")
  }
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


