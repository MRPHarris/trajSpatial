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
