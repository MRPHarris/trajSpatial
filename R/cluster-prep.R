# Functions used for organising trajectory data for cluster analysis in the GUI.
# Listed in order of typical use during a cluster workflow.

#' Move all files in one directory to another
#'
#' @description Performs the 'archive' function from the HYSPLIT cluster GUI if used on the cluster working directory. All files are moved from the from_dir to the to_dir. By default, existing files in the archive are themselves archived.
#'
#' @param from_dir full path to the folder containing the files. Windows cluster working directory by default.
#' @param to_dir full file path to the destination folder. Windows cluster archive by default.
#' @param archive_to_dir TRUE/FALSE to archive files currently in the to_dir. They will be moved to a timestamped folder within a new subfolder named 'previous'.
#' @param archive_name NULL or character string. If specified, adds the character to the timestamped subfolder used to archive existing to_dir contents.
#' @param overwrite TRUE/FALSE to overwrite existing to_dir folder contents if no archiving is performed.
#' @param verbose TRUE/FALSE to print status messages.
#'
#' @importFrom pracma isempty
#' @importFrom magrittr %>%
#'
#' @export
#'
archive_cluster_wd <- function(from_dir = "C:/hysplit/cluster/working/",
                      to_dir = "C:/hysplit/cluster/archive/",
                      archive_to_dir = TRUE,
                      archive_name = NULL,
                      overwrite = TRUE,
                      verbose = TRUE){
  # Checks
  from_dir <- from_dir %>% ensure_path_slash()
  to_dir <- to_dir %>% ensure_path_slash()
  # List files without directories
  files <- list.files(from_dir, full.names = TRUE)
  files <- files[!file.info(files)$isdir]
  if(isempty(files)){
    # That's it folks
    if(verbose){message("No files in the specified directory. It's clear!")}
  } else {
    # Carry on.
    # First, prep the to_dir. Do files need archiving?
    if(isTRUE(archive_to_dir)){
      # Get the files in the archive dir
      archive_existing_files <- list.files(to_dir, full.names = TRUE, recursive = FALSE)
      archive_existing_files <- archive_existing_files[!file.info(archive_existing_files)$isdir]
      if(isempty(archive_existing_files)){
        if(verbose){message("No files in ",to_dir," to archive. Continuing to copy operation.")}
      } else {
        # Archive the previous directory
        # Create the top-level archive-archive
        if(!dir.exists(paste0(to_dir,"previous/"))){
          dir.create(paste0(to_dir,"previous/"))
        }
        # Now prep this current previous dir.
        hour <- substr(Sys.time(),12,13) # get current time
        min <- substr(Sys.time(),15,16)
        sec <- substr(Sys.time(),18,19)
        if(!is.null(archive_name)){
          archive_folder_name <- paste0("previous-",archive_name,"-",Sys.Date(),"-hms-",hour,"-",min,"-",sec,"/")
        } else {
          archive_folder_name <- paste0("previous-",Sys.Date(),"-hms-",hour,"-",min,"-",sec,"/")
        }
        archive_folder_name_long <- paste0(to_dir,"previous/",archive_folder_name)
        # Now create the clear folder
        dir.create(path = archive_folder_name_long) # create archive folder
        # Copy the files
        file.copy(archive_existing_files,archive_folder_name_long) # move all files in endpts_filelist to that directory.
        # remove previous
        file.remove(archive_existing_files)
      }
    }
    # Now, do the main file moving.
    file.copy(from = files,to = to_dir, overwrite = overwrite)
    file.remove(files)
    if(verbose){message("Files moved successfully to ",to_dir)}
  }
}

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
#' @param target_traj NULL or a string that filenames must contain.
#' @param to_dir full file path to the destination folder. C drive default hysplit install cluster folder by default
#' @param date_vec Optional; a vector of formatted dates in the YYYY-MM-DD format. Note that to match dates, trajectory endpoint file names must somewhere contain a timestamp in YY-MM-DD-HH format.
#' @param hour_vec Optional; a numeric vector of double-digit hour intervals matching the desired day-hours starting from 0. E.g. "12" for midday. If supplied, will be combined with the date vector - thus, there should be an hour for every date entry if you choose to specify hours.
#' @param rename_long_files Optional; a character vector of strings to remove from filenames. See the internal 'shorten_endpt_filenames'. The HYSPLIT clustering will fail if any filenames exceed 54 characters.
#' @param format_endpts TRUE or FALSE to check each endpoint for extended met, and remove this information before transferring to the specified directory.
#' @param verbose TRUE or FALSE to print progress every 100 files. Useful when moving large datasets.
#'
#' @importFrom stringr str_sub
#' @importFrom stringr str_extract
#' @importFrom magrittr %>%
#'
#' @export
#'
collate_endpts <- function(from_dir,
                           recurse = FALSE,
                           target_traj = NULL,
                           to_dir = "C:/hysplit/cluster/endpts/",
                           date_vec = NULL,
                           hour_vec = NULL,
                           rename_long_files = c("traj","YSH","lon","lat"),
                           format_endpts = TRUE,
                           verbose = TRUE){
  # Checks
  from_dir <- from_dir %>% ensure_path_slash()
  to_dir <- to_dir %>% ensure_path_slash()
  # Format datevec for indexing
  if(!is.null(date_vec)){
    datevec_char <- format(date_vec, "%y-%m-%d")
    if(!is.null(hour_vec)){
      datevec_char <- paste0(datevec_char,"-",hour_vec)
    }
  }
  # Get files
  if(is.null(target_traj)){
    all_endpoints <- list.files(from_dir, full.names = TRUE, recursive = recurse)
  } else {
    if(!is.character(target_traj)){
      stop("Please supply 'target_traj' as either NULL or a string.")
    }
    all_endpoints <- list.files(from_dir, full.names = TRUE, recursive = recurse)[grep(target_traj,list.files(from_dir, full.names = FALSE, recursive = recurse))]
  }
  # Extract the files matching the datevec.
  if(!is.null(date_vec)){
    files_short <- trim_path_int(all_endpoints)
    # Get the dates from filenames in YY-MM-DD format
    dates_in_endpts <- str_extract(files_short,'(\\d+)-(\\d+)-(\\d+)-(\\d+)') %>%
      str_sub(., end = -4)
    # Find matches
    filematch <- all_endpoints[grepl(paste0(datevec_char, collapse = "|"),
                                     dates_in_endpts)]
  } else {
    filematch <- all_endpoints
  }
  # Check if they exceed 8
  if(length(filematch) < 16){
    warning("Clustering requires at least 16 trajectories.")
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
  # Do the file copy. format if necessary
  if(isTRUE(format_endpts)){
    filelist <- as.list(filematch)
    # Is there evidence of a mismatch?
    for(f in seq_along(filelist)){
      endpt_file_it <- filelist[[f]]
      filecheck <- endpt_metcheck_single(endpt_file_it)
      if(isTRUE(filecheck)){
        # Read in and modify endpoint.
        newfile <- format_endpt_forcluster(endpt_file_it)
        write(newfile, to_files[f])
      } else if(!isTRUE(filecheck)){
        # No modification needed. Copy file unmodified.
        file.copy(filematch[f],to_files[f])
      } else {
        message("File check for ",endpt_file_it," returned neither TRUE or FALSE. Check file format.")
      }
      if(verbose){
        if(f %% 100 == 0){
          message(f," files transferred.")
        }
      }
    }
  } else {
    file.copy(filematch,to_files)
  }
  # Check last file.
  if(file.exists(to_files[length(to_files)])){
    message("Endpoint files transferred to specified directory.")
  }
}

#' Make an INFILE for a cluster analysis.
#'
#' @description The INFILE is created during the clustering setup and lists all the endpoints to be supplied to the clustering program.
#'
#' @param endpoint_dir A full path to the directory containing the endpoints to be used for clustering.
#' @param cluster_wd The cluster working directory. Defaults to the standard location for a windows default install.
#' @param base_name A string used to subset endpoint files. NULL by default, so all endpoints in the supplied directory are used.
#' @param flag_longfilenames TRUE/FALSE to print message if any endpoints have a filename exceeding 54 characters.
#' @param disallowed_flags character strings supplied as a character vector to be used to match files to be excluded from the INFILE.
#' @param export TRUE/FALSE to write the INFILE to the supplied cluster working directory.
#' @param verbose TRUE/FALSE to print status message
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
make_INFILE <- function(endpoint_dir = "C:/hysplit/cluster/endpts/",
                        cluster_wd = "C:/hysplit/cluster/working/",
                        base_name = NULL,
                        flag_longfilenames = FALSE,
                        disallowed_flags = "readme",
                        export = TRUE,
                        verbose = FALSE){
  # Ensure paths end in slashes
  endpoint_dir <- endpoint_dir %>% ensure_path_slash()
  cluster_wd <- cluster_wd %>% ensure_path_slash()
  if(!dir.exists(endpoint_dir)){
    stop("The supplied endpoint directory does not exist.")
  }
  if(!dir.exists(cluster_wd)){
    stop("The supplied cluster working directory does not exist.")
  }
  # Get them names
  fdat <- list.files(path = endpoint_dir, full.names = T)
  fdat <- fdat[!file.info(fdat)$isdir]
  # Remove the specified files, if it's there.
  forbidden_matches <- grep(paste(disallowed_flags,collapse="|"),
                            fdat)
  fdat <- fdat[-c(forbidden_matches)]
  # Check for the specified name.
  fdat_short = fdat %>% trim_path_int()
  if(is.null(base_name)){
    fdat_base = fdat
  } else {
    fdat_base = fdat[grep(base_name,fdat_short)]
  }
  # flag if req
  # This will probably be moved elsewhere but useful here for now.
  if(isTRUE(flag_longfilenames)){
    lengths <- unlist(lapply(fdat %>% trim_path_int(), nchar))
    if(any(lengths > 54)){
      message("Some endpoint filenames are longer than 54 characters. Clustering will fail as a result.")
    }
  }
  if(isTRUE(export)){
    write.table(fdat,
                file = paste0(cluster_wd,"INFILE"),
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    if(verbose){
      message("INFILE created.")
    }
  }
}
