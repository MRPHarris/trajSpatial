# Functions for collating endpoint files for cluster analysis, and for reading, parsing, and analysing the resulting outputs.
# Some of these will be directly useable, and others are from a chapter in my thesis (e.g. cluster classes and related computations) and
# will be less easy to apply directly.

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
#' @param date_vec Optional; a vector of formatted dates in the YYYY-MM-DD format.
#' @param hour_vec Optional; a numeric vector of double-digit hour intervals matching the desired day-hours starting from 0. E.g. "12" for midday. If supplied, will be combined with the date vector - thus, there should be an hour for every date entry if you choose to specify hours.
#' @param rename_long_files Optional; a character vector of strings to remove from filenames. See the internal 'shorten_endpt_filenames'. The HYSPLIT clustering will fail if any filenames exceed 54 characters.
#' @param format_endpts TRUE or FALSE to check each endpoint for extended met, and remove this information before transferring to the specified directory.
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
                               format_endpts = TRUE){
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
    all_endpoints <- list.files(from_dir, full.names = TRUE, recursive = recurse)[grep(target_traj,list.files(from_dir, full.names = FALSE))]
  }
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
    }
  } else {
    file.copy(filematch,to_files)
  }
  # Check last file.
  if(file.exists(to_files[length(to_files)])){
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

#' Read in cluster change-in-TSV file.
#'
#' @description A HYSPLIT clustering attempt will output a file containing data on the
#'      percent change in total spatial variance or 'change in TSV %'. This file is called
#'      'DELPCT' and can be found in the cluster working directory after a given clustering attempt.
#'      This function reads in this file, and optionally trims it to above a given n_clusters.
#'
#' @param cluster_wd full file path to the clustering working directory. It is set to 'C:/hysplit/cluster/working/' by default.
#' @param max_clusters NULL or numeric to trim the returned data frame to only contain less or equal cluster quantities than the number specified.
#'
#' @importFrom dplyr slice
#' @importFrom magrittr %>%
#'
#' @export
#'
read_DELPCT <- function(cluster_wd = "C:/hysplit/working/cluster/",
                        max_clusters = NULL,
                        verbose = F){
  if(!file.exists(paste0(cluster_wd,"DELPCT"))){
    stop("No DELPCT file found. Run clusters or check specified cluster working directory")
  } else {
    f <- read.delim(paste0(cluster_wd,"DELPCT"), sep = "") %>%
      'colnames<-'(c('n_calculation','n_clusters','TSV_change_pct'))
    if(!is.null(max_clusters)){
      f <- f %>%
        slice(which(n_clusters <= max_clusters))
    }
    if(isTRUE(verbose)){
      message("DELPCT file extracted. Note that HYSPLIT quantifies the change in TSV between calculations/nclusters rather than the actual TSV value for that calculation.")
    }
    f
  }
}

#' Plot change in TSV across clusters using a DELPCT table
#'
#' @description Create a plot that mirrors HYSPLIT's 'Display change in total variance'
#'       plotting function, with change-in-percent thresholds to indicate where a good
#'       n_clusters might be. Returns a ggplot2 plotting object.
#'
#' @param DELPCT_table an output from read_DELPCT()
#' @param threshold percentage change-in-variance threshold to mark where decent n-cluster specifications might be found. HYSPLIT gives 20 and 30 as options.
#' @param max_clusters numeric, the right limit of the x-axis.
#' @param max_change numeric, the upper limit of the y-axis.
#' @param lab_x_offset numeric, the x-axis offset of threshold exceeding point-labels from their respective point.
#' @param lab_y_offset numeric, the y-axis offset of threshold exceeding point-labels from their respective point.
#'
#' @importFrom magrittr %>%
#' @importFrom cowplot theme_cowplot
#' @importFrom dplyr mutate
#'
#' @export
#'
plot_DELPCT <- function(DELPCT_table,
                        threshold = 20,
                        max_clusters = 30,
                        max_change = 100,
                        lab_x_offset = 1,
                        lab_y_offset = 5){
  ## Formatting
  newtable <- DELPCT_table
  newtable <- newtable %>%
    dplyr::mutate(pct_change = as.numeric((.[[3]]/lag(.[[3]]) * 100)-100))
  ## Create pct change threshold values
  xvals_pct_change <- newtable$n_clusters[which(newtable$pct_change > threshold)] + 1
  yvals_pct_change <- newtable$TSV_change_pct[which(newtable$pct_change > threshold)-1]
  lines_pct_change <- data.frame(x = rep(xvals_pct_change,2),y = c(yvals_pct_change, rep(0,length(yvals_pct_change))),
                                 np = rep(seq(from = 1,
                                              to = as.numeric(length(xvals_pct_change)),
                                              by = as.numeric(1)),2))
  ## Plot
  plt <- ggplot() +
    geom_line(data = newtable, aes(x = n_clusters, y = TSV_change_pct)) +
    geom_point(data = newtable, aes(x = n_clusters, y = TSV_change_pct)) +
    scale_x_continuous(expand = c(0,0), limits = c(0,max_clusters), breaks = seq(0,max_clusters,2.5),
                       labels = seq(0,max_clusters,2.5)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,max_change), breaks = seq(0,max_change,10)) +
    ## change points
    geom_line(data = lines_pct_change, aes(x,y, group = np), linetype = 'dashed', colour = 'red') +
    geom_point(data = lines_pct_change[1:(nrow(lines_pct_change)/2),], aes(x,y), colour = 'red') +
    geom_label(data = lines_pct_change[1:(nrow(lines_pct_change)/2),], aes(x + lab_x_offset, y + lab_y_offset, label = x),
               fill = NA, colour = 'red', label.size = NA) +
    labs(y = "Change in TSV (%)", x = "Number of clusters") +
    theme_cowplot(12) +
    theme(axis.text.x = element_text(colour = c("black","NA")),
          axis.text.y = element_text(colour = c("black","NA")))
  plt
}

#' Read a single cluster tdump file
#'
#' @description Read a single cluster tdump file. The cluster tdump file is generated in the HYSPLIT clustering GUI when the
#'      user presses "Display Means" for a given n clusters after clustering computations are performed. It contains information
#'      that can be used to plot or analyse the mean cluster trajectories.
#'
#' @param meanclus_tdump a full filepath to a cluster tdump file
#' @param header_string character vector used to target the header line within the tdump file
#' @param type experimental; one of either 'hysplit' or 'analogue' to specify either a hysplit program-generated file or a custom cluster generated in R using other functions in the trajSpatial package.
#'
#' @importFrom dplyr rename
#' @importFrom dplyr select
#'
#' @export
#'
read_meanclus_tdump <- function(meanclus_tdump,
                                header_string = "RESSURE",
                                type = "hysplit"){
  ## borrowed from splitr and other trajSpatial functions
  # file_i_path <- meanclus_tdump
  if(type == "hysplit"){
    file_lines <- readLines(meanclus_tdump, encoding = "UTF-8", skipNul = TRUE)
    ## Get header line
    header_line <- file_lines %>% vapply(FUN.VALUE = logical(1), USE.NAMES = FALSE, function(x) grepl(header_string, x)) %>% which()
    tab <- read.delim(meanclus_tdump, sep = "", skip = header_line, header = F) %>%
      dplyr::rename(cluster = V1,
                    lat = V10,
                    lon = V11,
                    height = V12,
                    hour.inc = V9) %>%
      dplyr::select(-contains("V"))
    tab
  } else if(type == "analogue"){
    file <- read.table(meanclus_tdump) %>%
      dplyr::select(c(cluster,hour.inc,lat,lon,height))
    file
  } else {
    stop("type not recognised. Please supply one of either 'hysplit' for GUI-generated clusmeans, or 'analogue' for analogue files.")
  }
}

#' Read a single cluslist file
#'
#' @description Read a single cluslist file. The cluslist file is generated by the HYSPLIT clustering program when a clustering attempt is performed
#'      for a given n cluster value, and contains a table listing each individual endpoint file and the cluster they have been assigned to. This function
#'      is a simple wrapper for read.table().
#'
#' @param CLUSLIST_file a full file path to the desired cluslist file.
#'
#' @importFrom dplyr rename
#' @importFrom dplyr select
#'
#' @export
#'
read_cluslist <- function(CLUSLIST_file){
  cluslist <- read.table(file = CLUSLIST_file)
  cluslist
}

#' Read a set of cluster tdump files
#'
#' @description Read in all cluster tdump files (i.e. mean cluster trajectory information) in the specified directory. This can be used
#'      to collate all mean cluster information after a set of clustering runs for analysis or display.
#'
#' @param cluster_dir a full filepath to the cluster working directory. Defaults to "C:/hysplit/cluster/working/".
#' @param type experimental; one of either 'hysplit' or 'analogue' to specify either a hysplit program-generated file or a custom cluster generated in R using other functions in the trajSpatial package.
#' @param header_string character vector used to target the header line within the tdump file
#'
#' @importFrom dplyr rename
#' @importFrom dplyr select
#'
#' @export
#'
get_clusmeans_tdump <- function(cluster_dir = "C:/hysplit/cluster/working/",
                                type = "hysplit",
                                header_string = "RESSURE"){
  tdumps_full <- list.files(cluster_dir, full.names = TRUE)
  tdumps_short <- list.files(cluster_dir, full.names = FALSE)
  tdump_means <- tdumps_full[which(stringr::str_detect(tdumps_short,"Cmean"))]
  tdump_means_short <- tdumps_short[which(stringr::str_detect(tdumps_short,"Cmean"))]
  if(type == "analogue"){
    tdump_means <- tdump_means[which(stringr::str_detect(tdump_means_short,"_anlg"))]
    tdump_means_short <- tdump_means_short[which(stringr::str_detect(tdump_means_short,"_anlg"))]
  }
  mean_tabs <- vector('list', length = length(tdump_means))
  for(mn in seq_along(mean_tabs)){
    mean_tabs[[mn]] <- read_meanclus_tdump(tdump_means[mn],
                                           type = type,
                                           header_string = header_string)
  }
  names(mean_tabs) <- unlist(lapply(str_split(tdump_means_short,"[.]"),"[[",1))
  mean_tabs
}

#' Read a set of cluslist files
#'
#' @description Read a set of cluslist files. Individual cluslist files are generated by the HYSPLIT clustering program when a clustering attempt is performed
#'      for a given n cluster value, and contains a table listing each individual endpoint file and the cluster they have been assigned to.
#'
#' @param cluster_wd a full file path to the cluster working directory. Defaults to "C:/hysplit/cluster/working/".
#'
#' @importFrom dplyr rename
#' @importFrom dplyr select
#'
#' @export
#'
get_cluslists <- function(cluster_wd){
  files <- list.files(cluster_wd, full.names = TRUE)
  files_short <- list.files(cluster_wd, full.names = FALSE)
  cluslist_files <- files[which(stringr::str_detect(files_short,"CLUSLIST"))]
  cluslist_files_short <- files_short[which(stringr::str_detect(files_short,"CLUSLIST"))]
  cluslist_tabs <- vector('list', length = length(cluslist_files))
  for(cl in seq_along(cluslist_tabs)){
    cluslist_tabs[[cl]] <- read_cluslist(cluslist_files[cl])
  }
  names(cluslist_tabs) <- cluslist_files_short
  cluslist_tabs
}

#' Calculate the percentage of endpoints in each cluster
#'
#' @description Using a supplied cluslist file, derives the percentage of endpoints contained in each cluster, providing a means
#'      to determine their relative importance.
#'
#' @param cluslist a single cluslist file, such as a direct output from read_cluslist() or an individual cluslist from get_cluslists().
#'
#' @importFrom dplyr rename
#' @importFrom dplyr select
#'
#' @export
#'
get_cluster_percentages <- function(cluslist){
  if(!'cluster' %in% colnames(cluslist)){
    cluslist <- cluslist %>%
      dplyr::rename(cluster = V1)
  }
  ntrajectories <- nrow(cluslist)
  nclusters <- max(cluslist$cluster)
  # loop to get percentages
  cluster_pc_df <- data.frame(matrix(NA,nrow = nclusters+1, ncol = 3))
  colnames(cluster_pc_df) <- c("cluster", "percent","n")
  it_list <- vector(mode = "list", length = nclusters)
  for(i in seq_along(it_list)){
    # get cluster nrows
    ntrajectories_it <- nrow(cluslist[which(cluslist$cluster == i),])
    cluster_pc_df[i,1] <- paste0("Cluster ",i)
    cluster_pc_df[i,2] <- (ntrajectories_it/ntrajectories)*100
    cluster_pc_df[i,3] <- ntrajectories_it
  }
  cluster_pc_df[nclusters+1,1] <- "Total"
  cluster_pc_df[nclusters+1,2] <- sum(cluster_pc_df[1:nclusters,2])
  cluster_pc_df[nclusters+1,3] <- ntrajectories
  cluster_pc_df
}

#' Calculate percentage importance of clusters grouped into classes
#'
#' @description Experimental function. A simple table of each cluster alongside columns containing 'class' and 'class_num' can be used
#'      to group clusters into classes for grouped analysis. See Markle et al. (2012) for an example of this type of analysis.
#'
#' @param cluslist a single cluslist file, such as a direct output from read_cluslist() or an individual cluslist from get_cluslists().
#' @param assignments a data.frame containing the columns: 'new_clust' containing the cluster number from 1 to n clusters, 'class' containing the names of the classes, and 'class_num' containing numeric numbers for each class.
#'
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr inner_join
#' @importFrom dplyr arrange
#'
#' @export
#'
get_class_percentages <- function(cluslist,
                                  assignments){
  if(!'cluster' %in% colnames(cluslist)){
    cluslist <- cluslist %>%
      rename(cluster = V1)
  }
  assignments = assignments %>%
    dplyr::select(c(new_clust,class,class_num)) %>%
    rename(cluster = new_clust)
  cluslist <- cluslist %>%
    inner_join(assignments)
  unique_assignments <- assignments %>%
    dplyr::select(c(class,class_num)) %>%
    distinct() %>%
    arrange(class_num)
  ntrajectories <- nrow(cluslist)
  nclass<- max(cluslist$class_num)
  # loop to get percentages
  class_pc_df <- data.frame(matrix(NA,nrow = nclass+1, ncol = 4))
  colnames(class_pc_df) <- c("class","class number", "percent","n")
  it_list <- vector(mode = "list", length = nclass)
  for(i in seq_along(it_list)){
    # get cluster nrows
    ntrajectories_it <- nrow(cluslist[which(cluslist$class_num == i),])
    class_pc_df[i,1] <- unique_assignments$class[i]
    class_pc_df[i,2] <- unique_assignments$class_num[i]
    class_pc_df[i,3] <- (ntrajectories_it/ntrajectories)*100
    class_pc_df[i,4] <- ntrajectories_it
  }
  class_pc_df[nclass+1,1] <- "Total"
  class_pc_df[nclass+1,2] <- NA
  class_pc_df[nclass+1,3] <- sum(class_pc_df[1:nclass,3])
  class_pc_df[nclass+1,4] <- ntrajectories
  class_pc_df
}

#' Create an analogue cluster mean trajectory from one or more supplied cluslists.
#'
#' @description Experimental function. Takes cluslist files and a set of corresponding trajectories,
#'      and attempts to create a set of clusmean files. The mean calculation uses geosphere::geomean, but
#'      produces outputs that differ slightly from HYSPLIT's trajmean program. I could not diagnose
#'      the differences, but they seem to be minor. Bears further investigation another time!
#'
#' @param clustlists one or more cluslist files, such as a direct output from read_cluslist() or get_cluslists().
#' @param export_directory NULL or full file path to a directory. If !NULL, mean trajectory tables will be exported as .csvs.
#' @param endpoint_folder full file path to a folder containing the endpoints used in the cluster calculations.
#' @param traj_total_duration_hrs numeric; how long were the trajectories run for, in hours?
#'
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom rlist list.rbind
#' @importFrom dplyr arrange
#'
#' @export
#'
create_clusmeans_analogue <- function(clustlists,
                                      export_directory = NULL,
                                      endpoint_folder,
                                      traj_total_duration_hrs = 120){
  if(is.null(export_directory)){
    message("No export directory specified. Clusmean analogues will be collated into a nested list. This can use a lot of memory for large trajectory clustering datasets.")
  }
  # test if one or more cluslists were supplied
  if(!isTRUE(inherits(clustlists,"list"))){
    clustlists <- list(clustlists) # Assuming a data frame was supplied containing a single cluslist, this will just cause the function to iterate over a one-element list.
  }
  clustlists_compiled <- vector('list',length = length(clustlists))
  for(clist in seq_along(clustlists)){
    ## Order doesn't really matter - just move through the iterations.
    message("Starting clusmean creation for: ",names(clustlists)[clist])
    clustlist_it <- clustlists[[clist]]
    if(!'cluster' %in% colnames(clustlist_it)){
      clustlist_it <- clustlist_it %>%
        rename(cluster = V1)
    }
    # Determine number of clusters
    nclusters <- as.data.frame(clustlist_it$cluster) %>%
      distinct() %>% nrow() %>% as.numeric()
    clust_itlist <- vector('list', length = nclusters)
    for(clust in seq_along(clust_itlist)){
      message("Starting cluster ",clust," of ", length(clust_itlist))
      ## Iterate along clusters
      ## Get trajectories associated with the 1st cluster of the three
      cluster_traj_list <- as.list(clustlist_it$V8[which(clustlist_it$cluster == clust)]) %>%
        lapply(function(file){
          file <- read_endpoint_file(file)
          file
        })
      ## HEIGHTS
      red2hghts <- lapply(cluster_traj_list, function(f){
        f <- f$height
        f
      })
      heights <- colMeans(do.call(rbind,red2hghts))
      ## MEAN LATS AND LONS
      red2coords <- lapply(cluster_traj_list, function(f1){
        f1 <- f1 %>%
          dplyr::select(c(lon,lat))
      })
      endpt_list <- vector('list', length = nrow(cluster_traj_list[[1]]))
      for(ep in seq_along(endpt_list)){
        # For each endpoint, extract the rows at that index into a single data frame.
        # From each list, extract the first row and combine into a single data frame.
        endpt_list[[ep]] <- list.rbind(lapply(red2coords, function(f2){
          f2 <- f2[ep,] %>%
            'colnames<-'(c('x','y'))
          f2
        }))
      }
      # Means
      endpt_means <- lapply(endpt_list, function(ept){
        round(geosphere::geomean(xy = ept),3)
      }) %>% list.rbind() %>% data.frame() %>% 'colnames<-'(c('lon','lat')) %>%
        mutate(hour.inc = rev(seq(-(traj_total_duration_hrs),0,1))) %>%
        dplyr::select(c(hour.inc,lat,lon)) %>%
        mutate(height = heights) %>%
        mutate(cluster = clust) %>%
        dplyr::select(c(cluster,hour.inc,lat,lon,height))
      clust_itlist[[clust]] <- endpt_means
    }
    # Compile this cluslist
    # Export this clusmean
    if(!is.null(export_directory)){
      clusmean_combined_it <- list.rbind(clust_itlist)
      write.table(clusmean_combined_it, file = paste0(export_directory,"Cmean1_",nclusters,"_anlg.tdump"))
      message(paste0("Cmean1_",nclusters,"_anlg.tdump exported"))
    } else if(is.null(export_directory)){
      clustlists_compiled[[clist]] <- list.rbind(clust_itlist)
      message(paste0("Cmean1_",nclusters," analog added to complist"))
    }
  }
}

#' Read a single endpoint file
#'
#' @description Read and format a single endpoint (trajectory) file. An endpoint file is generated for every run of the hysplit model, and contains
#'      information for individual trajectories or releases.
#'
#' @param file a full file path to the desired endpoint file.
#' @param header_string a character string used to target the header line in the file.
#'
#' @importFrom dplyr rename
#'
#' @export
#'
read_endpoint_file <- function(file, header_string = "PRESSURE"){
  ## Column names after https://www.ready.noaa.gov/hysplitusersguide/S263.htm
  file_lines <- readLines(file, encoding = "UTF-8", skipNul = TRUE)
  header_line <- file_lines %>% vapply(FUN.VALUE = logical(1), USE.NAMES = FALSE, function(x) grepl(header_string, x)) %>% which()
  tab <- read.delim(file, sep = "", skip = header_line, header = F) %>%
    dplyr::rename(trajnum = V1,
                  metgrid = V2,
                  year = V3,
                  month = V4,
                  day = V5,
                  hour = V6,
                  minute = V7,
                  fc_hour = V8,
                  hour.inc = V9,
                  lat = V10,
                  lon = V11,
                  height = V12,
                  pressure = V13)
  tab
}

## Format a DELPCT file for plotting
# lobotomised initial bits from trajSpatial::plot_DELPCT
## bit of a mess but these are probably important

#' Format a DELPCT file for plotting
#'
#' @description Tore initial bits out of trajSpatial::plot_DELPCT. Useful for more refined line plotting.
#'
#' @param DELPCT_table an output from read_DELPCT()
#' @param threshold numeric value for % change used to calculate whether an n cluster decrease exceeds a threshold value for reduced total spatial variance.
#'
#' @importFrom dplyr mutate
#'
#' @export
#'
format_obj_DELPCT <- function(DELPCT_table,
                              threshold = 20){
  listvars <- vector('list', length = 2)
  newtable <- DELPCT_table
  newtable <- newtable %>%
    mutate(pct_change = as.numeric((.[[3]]/lag(.[[3]]) * 100)-100))
  # Create pct change threshold values
  xvals_pct_change <- newtable$n_clusters[which(newtable$pct_change > threshold)] + 1
  yvals_pct_change <- newtable$TSV_change_pct[which(newtable$pct_change > threshold)-1]
  lines_pct_change <- data.frame(x = rep(xvals_pct_change,2),
                                 y = c(yvals_pct_change, rep(0,length(yvals_pct_change))),
                                 np = rep(seq(from = 1,
                                              to = as.numeric(length(xvals_pct_change)),
                                              by = as.numeric(1)),2))
  listvars[[1]] <- newtable
  listvars[[2]] <- lines_pct_change
  return(listvars)
}

#' Recoding cluslist table number assignments
#'
#' @description The numbering of clusters is determine by the algorithm, and as such can be a bit of a nightmare when plotting.
#'      This function offers a simple way to recode the number of each cluster in a cluslist file.
#'
#' @param cluslist a single cluslist file, such as a direct output from read_cluslist() or an individual cluslist from get_cluslists().
#' @param old_clust numeric vector of the old cluster numbers, in the order they appear.
#' @param new_clust numeric vector of the new desired cluster numbers, in the order they will replace the old_clust values.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr recode
#'
#' @export
#'
recode_cluslist <- function(cluslist,
                            old_clust,
                            new_clust){
  if(!'cluster' %in% colnames(cluslist)){
    cluslist <- cluslist %>%
      rename(cluster = V1)
  }
  cluslist <- cluslist %>%
    mutate(cluster = recode(cluster, !!!setNames(new_clust, old_clust)))
  cluslist
}

#' Recoding clusmean table number assignments
#'
#' @description The numbering of clusters is determined by the algorithm, and as such can be a bit of a nightmare when plotting.
#'      This function offers a simple way to recode the number of each cluster in a clusmean file.
#'
#' @param clusmean a single clusmean file, such as a direct output from read_meanclus_tdump().
#' @param old_clust numeric vector of the old cluster numbers, in the order they appear.
#' @param new_clust numeric vector of the new desired cluster numbers, in the order they will replace the old_clust values.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr recode
#'
#' @export
#'
recode_clusmean <- function(clusmean,
                            old_clust,
                            new_clust){
  clusmean <- clusmean %>%
    mutate(cluster = recode(cluster, !!!setNames(new_clust, old_clust)))
  clusmean
}

#' Assign trajectories to their clusters within a trajectory data table.
#'
#' @description Attach cluster numbers to trajectories within a trajectory data table, such as that obtained with splitr::trajectory_read().
#'
#' @param trajectory_data a data.frame of trajectory data, such as that obtained via splitr::trajectory_read().
#' @param cluslist a single cluslist file, such as a direct output from read_cluslist() or an individual cluslist from get_cluslists().
#' @param new_assignments NULL; Experimental; new numbers for recoding.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr recode
#' @importFrom dplyr arrange
#' @importFrom dplyr inner_join
#'
#' @export
#'
format_trajdata_clusterfreqs <- function(trajectory_data,
                                         cluslist,
                                         new_assignments = NULL){
  ## Extract date grob that can be used to match trajectories
  cluslist_it <- cluslist
  cluster_fnames <- cluslist_it$V8
  ## Just do this for all clusters
  cluster_fnames_short <- unlist(lapply(str_split(cluster_fnames,"[/]"), tail, n = 1))
  endpt_years <- unlist(lapply(str_split(cluster_fnames_short,"[-]"),"[[",6))
  endpt_months <- unlist(lapply(str_split(cluster_fnames_short,"[-]"),"[[",7))
  endpt_days <- unlist(lapply(str_split(cluster_fnames_short,"[-]"),"[[",8))
  all_endpt_dates <- as.data.frame(matrix(paste(endpt_years, endpt_months,endpt_days, sep = "-"))) %>%
    mutate(cluster = cluslist_it$cluster) %>%
    'colnames<-'(c('just_date','cluster')) %>%
    mutate(just_date = as_date(just_date)) %>%
    arrange(just_date)
  ## Ok, now, given a set of trajectories:
  trajectories <- trajectory_data
  # 1) are they the sames size?
  # nrow(trajectories)/121 == nrow(all_endpt_dates)
  # 2) match the dates to the trajectories
  trajectories <- trajectories %>%
    mutate(just_date = as_date(just_date)) %>%
    arrange(just_date) %>%
    inner_join(all_endpt_dates) %>%
    add_traj_identifier() %>%
    mutate(start_year = year(date))
  if(!is.null(new_assignments)){
    trajectories <- trajectories %>%
      inner_join(new_assignments %>% dplyr::rename(cluster = new_clust))
  }
  trajectories
}

## Hashed code below here is tba sorted/integrated within package

# ## Format a set of cluster-associated trajectories for a stacked inter-cluster frequency plot.
# # Takes an output from format_trajdata_clusterfreqs
# # CLUSTER FNS
# cluster_stacked_frequencies_year <- function(ca_trajectories,
#                                              stepfmt = F,
#                                              year_range = seq(1980,2020,1)){
#   ## Get the frequency of events per year
#   year_event_freq <- ca_trajectories %>%
#     group_by(start_year) %>%
#     summarise(total_events = n()/121)
#   ## Get the frequency of each cluster for a given year relative to the max number of occurences.
#   yearly_clus <- ca_trajectories %>%
#     group_by(start_year, cluster) %>%
#     summarise(n = n()) %>%
#     mutate(freq = n / sum(n))
#   fillyears <- data.frame(matrix(year_range)) %>%
#     'colnames<-'(c('start_year'))
#   nclus <- vector('list', length = length(unique(ca_trajectories$cluster)))
#   for(clus in seq_along(nclus)){
#     clus_it <- clus
#     frame_it <- yearly_clus %>%
#       filter(cluster == clus_it) %>%
#       full_join(year_event_freq) %>%
#       arrange(start_year) %>%
#       ungroup() %>%
#       mutate(cluster = clus_it) %>%
#       replace(is.na(.), 0) %>%
#       mutate(n_occurrences = n/121) #%>%
#     # mutate(freq_scaled = total_events*freq)
#     if(clus == 1){
#       frame_it <- frame_it %>%
#         mutate(stacked_freq = freq) %>%
#         mutate(stacked_freq_base = 0)
#       if(isTRUE(stepfmt)){
#         nclus[[clus]] <- stepformat(frame_it, trail = F)
#       } else if(isFALSE(stepfmt)) {
#         nclus[[clus]] <- frame_it
#       } else {
#         stop("Please provide stepfmt as logical TRUE or FALSE.")
#       }
#     } else {
#       # Sum the frequencies of previous groups
#       its <- seq(1,(clus_it-1),1)
#       summed_freq_total <- lapply(nclus[its], function(x){
#         freq <- x$freq
#       }) %>% Reduce('+',.)
#       # summed_freq_base <- nclus[[clus_it-1]]$stacked_freq
#       if(isTRUE(stepfmt)){
#         frame_it <- stepformat(frame_it, trail = F)
#       }
#       # frame_it <- stepformat(frame_it, trail = F)
#       frame_it <- frame_it %>%
#         mutate(stacked_freq = freq + summed_freq_total) %>%
#         mutate(stacked_freq_base = summed_freq_total)
#       nclus[[clus]] <- frame_it
#     }
#     # nclus[[clus]] <- frame_it
#   }
#   allclus_list <- rlist::list.rbind(nclus)
#   allclus_list
# }
#
#
# cluster_stacked_frequencies_daily_movav <- function(ca_trajectories,
#                                                     stepfmt = F,
#                                                     year_range = seq(1980,2020,1),
#                                                     winwidth = 90){
#
#   # ca_trajectories = trajectories
#   # stepfmt = F
#   # year_range = seq(1980,2020,1)
#   # winwidth = 90
#   ## Days in sequence
#   tot_days = length(unique(lubridate::as_date(ca_trajectories$date.start)))
#   day_sequence = seq(1,tot_days,1)
#   ## Fill necessary columns where possible
#   if(!'cluster' %in% colnames(ca_trajectories)){
#     stop("Please add a cluster column by inner_joining with the class-cluster assignment table.")
#   }
#   if(!'day_seq' %in% colnames(ca_trajectories)){
#     ca_trajectories <- ca_trajectories %>%
#       mutate(day_seq = cumsum(c(0, as.numeric(diff(lubridate::day(date.start))) != 0)) + 1)
#   }
#
#   # ca_trajectories <- ca_trajectories %>%
#   #   mutate(class_num = as.numeric(forcats::fct_inorder(class)))
#   ## Get the frequency of events per year
#   ## Add daily sequence field to trajectories
#   daily_event_freq <- ca_trajectories %>%
#     group_by(day_seq) %>%
#     summarise(daily_events = n()/121)
#
#   # year_event_freq <- ca_trajectories %>%
#   #   group_by(start_year) %>% clas
#   #   summarise(total_events = n()/121)
#   ## Get the frequency of each class_num for a given year relative to the max number of occurences.
#   daily_clus <- ca_trajectories %>%
#     group_by(day_seq, cluster) %>%
#     summarise(n = n()) %>%
#     mutate(freq = n / sum(n))
#   # https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
#   # daily_movfreqs <- daily_clas %>%
#   #   group_by(class_num,day_seq) %>%
#   #   summarise(mov_count_test = zoo::rollapply(freq, 91,function(x){sum(x)/91}, align = 'center', fill = NA))
#
#   filldays <- data.frame(matrix(day_sequence)) %>%
#     'colnames<-'(c('day_seq'))
#   nclus <- vector('list', length = length(unique(ca_trajectories$cluster)))
#   for(clus in seq_along(nclus)){
#     clus_it <- clus
#     frame_it <- daily_clus %>%
#       filter(cluster == clus_it) %>%
#       full_join(daily_event_freq) %>%
#       arrange(day_seq) %>%
#       ungroup() %>%
#       mutate(cluster = clus_it) %>%
#       replace(is.na(.), 0) %>%
#       mutate(n_occurrences = n/121) %>%
#       mutate(mov_freq = zoo::rollapply(n_occurrences, winwidth,sum, align = 'center', fill = NA)) %>%
#       mutate(mov_freq = mov_freq/winwidth)
#     # mutate(freq_scaled = total_events*freq)
#     if(clus == 1){
#       frame_it <- frame_it %>%
#         mutate(stacked_freq = mov_freq) %>%
#         mutate(stacked_freq_base = 0)
#       if(isTRUE(stepfmt)){
#         nclus[[clus]] <- stepformat(frame_it, trail = F)
#       } else if(isFALSE(stepfmt)) {
#         nclus[[clus]] <- frame_it
#       } else {
#         stop("Please provide stepfmt as logical TRUE or FALSE.")
#       }
#     } else {
#       # Sum the frequencies of previous groups
#       its <- seq(1,(clus_it-1),1)
#       summed_freq_total <- lapply(nclus[its], function(x){
#         mov_freq <- x$mov_freq
#       }) %>% Reduce('+',.)
#       # summed_freq_base <- nclus[[clas_it-1]]$stacked_freq
#       if(isTRUE(stepfmt)){
#         frame_it <- stepformat(frame_it, trail = F)
#       }
#       # frame_it <- stepformat(frame_it, trail = F)
#       frame_it <- frame_it %>%
#         mutate(stacked_freq = mov_freq + summed_freq_total) %>%
#         mutate(stacked_freq_base = summed_freq_total)
#       nclus[[clus]] <- frame_it
#     }
#     # nclas[[clas]] <- frame_it
#   }
#   allclus_list <- rlist::list.rbind(nclus)
#   allclus_list
# }
# ## CLASS FNS
# class_stacked_frequencies_year <- function(ca_trajectories,
#                                            stepfmt = F,
#                                            year_range = seq(1980,2020,1)){
#
#   # ca_trajectories <- trajectories
#   # stepfmt = F
#   # year_range = seq(1980,2020,1)
#
#   # ca_trajectories = trajectories
#   # stepfmt = F
#   # year_range = seq(1980,2020,1)
#
#   if(!'class_num' %in% colnames(ca_trajectories)){
#     stop("Please add a class and class_num column by inner_joining with the class-cluster assignment table.")
#   }
#   # ca_trajectories <- ca_trajectories %>%
#   #   mutate(class_num = as.numeric(forcats::fct_inorder(class)))
#   ## Get the frequency of events per year
#   year_event_freq <- ca_trajectories %>%
#     group_by(start_year) %>%
#     summarise(total_events = n()/121)
#   ## Get the frequency of each class_num for a given year relative to the max number of occurences.
#   yearly_clas <- ca_trajectories %>%
#     group_by(start_year, class_num) %>%
#     summarise(n = n()) %>%
#     mutate(freq = n / sum(n))
#   fillyears <- data.frame(matrix(year_range)) %>%
#     'colnames<-'(c('start_year'))
#   nclas <- vector('list', length = length(unique(ca_trajectories$class_num)))
#   for(clas in seq_along(nclas)){
#     clas_it <- clas
#     frame_it <- yearly_clas %>%
#       filter(class_num == clas_it) %>%
#       full_join(year_event_freq) %>%
#       arrange(start_year) %>%
#       ungroup() %>%
#       mutate(class_num = clas_it) %>%
#       replace(is.na(.), 0) %>%
#       mutate(n_occurrences = n/121) #%>%
#     # mutate(freq_scaled = total_events*freq)
#     if(clas == 1){
#       frame_it <- frame_it %>%
#         mutate(stacked_freq = freq) %>%
#         mutate(stacked_freq_base = 0)
#       if(isTRUE(stepfmt)){
#         nclas[[clas]] <- stepformat(frame_it, trail = F)
#       } else if(isFALSE(stepfmt)) {
#         nclas[[clas]] <- frame_it
#       } else {
#         stop("Please provide stepfmt as logical TRUE or FALSE.")
#       }
#     } else {
#       # Sum the frequencies of previous groups
#       its <- seq(1,(clas_it-1),1)
#       summed_freq_total <- lapply(nclas[its], function(x){
#         freq <- x$freq
#       }) %>% Reduce('+',.)
#       # summed_freq_base <- nclus[[clas_it-1]]$stacked_freq
#       if(isTRUE(stepfmt)){
#         frame_it <- stepformat(frame_it, trail = F)
#       }
#       # frame_it <- stepformat(frame_it, trail = F)
#       frame_it <- frame_it %>%
#         mutate(stacked_freq = freq + summed_freq_total) %>%
#         mutate(stacked_freq_base = summed_freq_total)
#       nclas[[clas]] <- frame_it
#     }
#     # nclas[[clas]] <- frame_it
#   }
#   allclus_list <- rlist::list.rbind(nclas)
#   allclus_list
# }
#
# class_stacked_frequencies_month <- function(ca_trajectories,
#                                             stepfmt = F,
#                                             year_range = seq(1980,2020,1)){
#
#   # ca_trajectories <- trajectories
#   # stepfmt = F
#   # year_range = seq(1980,2020,1)
#
#   tot_months = (max(year_range) - min(year_range)) * 12 + 12
#   month_sequence = seq(1,tot_months,1)
#
#   # ca_trajectories = trajectories
#   # stepfmt = F
#   # year_range = seq(1980,2020,1)
#
#   if(!'class_num' %in% colnames(ca_trajectories)){
#     stop("Please add a class and class_num column by inner_joining with the class-cluster assignment table.")
#   }
#   if(!'month_seq' %in% colnames(ca_trajectories)){
#     ca_trajectories <- ca_trajectories %>%
#       mutate(month_seq = cumsum(c(0, as.numeric(diff(lubridate::month(date.start))) != 0)) + 1)
#   }
#   # ca_trajectories <- ca_trajectories %>%
#   #   mutate(class_num = as.numeric(forcats::fct_inorder(class)))
#   ## Get the frequency of events per year
#   ## Add daily sequence field to trajectories
#   month_event_freq <- ca_trajectories %>%
#     group_by(month_seq) %>%
#     summarise(monthly_events = n()/121)
#
#   # year_event_freq <- ca_trajectories %>%
#   #   group_by(start_year) %>%
#   #   summarise(total_events = n()/121)
#   ## Get the frequency of each class_num for a given year relative to the max number of occurences.
#   monthly_clas <- ca_trajectories %>%
#     group_by(month_seq, class_num) %>%
#     summarise(n = n()) %>%
#     mutate(freq = n / sum(n))
#   fillmonths <- data.frame(matrix(month_sequence)) %>%
#     'colnames<-'(c('month_seq'))
#   nclas <- vector('list', length = length(unique(ca_trajectories$class_num)))
#   for(clas in seq_along(nclas)){
#     clas_it <- clas
#     frame_it <- monthly_clas %>%
#       filter(class_num == clas_it) %>%
#       full_join(month_event_freq) %>%
#       arrange(month_seq) %>%
#       ungroup() %>%
#       mutate(class_num = clas_it) %>%
#       replace(is.na(.), 0) %>%
#       mutate(n_occurrences = n/121) #%>%
#     # mutate(freq_scaled = total_events*freq)
#     if(clas == 1){
#       frame_it <- frame_it %>%
#         mutate(stacked_freq = freq) %>%
#         mutate(stacked_freq_base = 0)
#       if(isTRUE(stepfmt)){
#         nclas[[clas]] <- stepformat(frame_it, trail = F)
#       } else if(isFALSE(stepfmt)) {
#         nclas[[clas]] <- frame_it
#       } else {
#         stop("Please provide stepfmt as logical TRUE or FALSE.")
#       }
#     } else {
#       # Sum the frequencies of previous groups
#       its <- seq(1,(clas_it-1),1)
#       summed_freq_total <- lapply(nclas[its], function(x){
#         freq <- x$freq
#       }) %>% Reduce('+',.)
#       # summed_freq_base <- nclus[[clas_it-1]]$stacked_freq
#       if(isTRUE(stepfmt)){
#         frame_it <- stepformat(frame_it, trail = F)
#       }
#       # frame_it <- stepformat(frame_it, trail = F)
#       frame_it <- frame_it %>%
#         mutate(stacked_freq = freq + summed_freq_total) %>%
#         mutate(stacked_freq_base = summed_freq_total)
#       nclas[[clas]] <- frame_it
#     }
#     # nclas[[clas]] <- frame_it
#   }
#   allclus_list <- rlist::list.rbind(nclas)
#   allclus_list
# }
#
# class_stacked_frequencies_monthtot <- function(ca_trajectories,
#                                                stepfmt = F){
#
#   # ca_trajectories <- trajectories
#   # stepfmt = F
#   # year_range = seq(1980,2020,1)
#   #
#   #   tot_months = (max(year_range) - min(year_range)) * 12 + 12
#   #   month_sequence = seq(1,tot_months,1)
#
#   # ca_trajectories = trajectories
#   # stepfmt = F
#   # year_range = seq(1980,2020,1)
#
#   if(!'class_num' %in% colnames(ca_trajectories)){
#     stop("Please add a class and class_num column by inner_joining with the class-cluster assignment table.")
#   }
#   if(!'start_month' %in% colnames(ca_trajectories)){
#     ca_trajectories <- ca_trajectories %>%
#       mutate(start_month = month(date.start))
#   }
#   # ca_trajectories <- ca_trajectories %>%
#   #   mutate(class_num = as.numeric(forcats::fct_inorder(class)))
#   ## Get the frequency of events per year
#   ## Add daily sequence field to trajectories
#   month_event_freq <- ca_trajectories %>%
#     group_by(start_month) %>%
#     summarise(monthly_events = n()/121)
#
#   # year_event_freq <- ca_trajectories %>%
#   #   group_by(start_year) %>%
#   #   summarise(total_events = n()/121)
#   ## Get the frequency of each class_num for a given year relative to the max number of occurences.
#   monthly_clas <- ca_trajectories %>%
#     group_by(start_month, class_num) %>%
#     summarise(n = n()) %>%
#     mutate(freq = n / sum(n))
#   fillmonths <- data.frame(matrix(seq(1,12,1))) %>%
#     'colnames<-'(c('start_month'))
#   nclas <- vector('list', length = length(unique(ca_trajectories$class_num)))
#   for(clas in seq_along(nclas)){
#     clas_it <- clas
#     frame_it <- monthly_clas %>%
#       filter(class_num == clas_it) %>%
#       full_join(month_event_freq) %>%
#       arrange(start_month) %>%
#       ungroup() %>%
#       mutate(class_num = clas_it) %>%
#       replace(is.na(.), 0) %>%
#       mutate(n_occurrences = n/121) #%>%
#     # mutate(freq_scaled = total_events*freq)
#     if(clas == 1){
#       frame_it <- frame_it %>%
#         mutate(stacked_freq = freq) %>%
#         mutate(stacked_freq_base = 0)
#       if(isTRUE(stepfmt)){
#         nclas[[clas]] <- stepformat(frame_it, trail = F)
#       } else if(isFALSE(stepfmt)) {
#         nclas[[clas]] <- frame_it
#       } else {
#         stop("Please provide stepfmt as logical TRUE or FALSE.")
#       }
#     } else {
#       # Sum the frequencies of previous groups
#       its <- seq(1,(clas_it-1),1)
#       summed_freq_total <- lapply(nclas[its], function(x){
#         freq <- x$freq
#       }) %>% Reduce('+',.)
#       # summed_freq_base <- nclus[[clas_it-1]]$stacked_freq
#       if(isTRUE(stepfmt)){
#         frame_it <- stepformat(frame_it, trail = F)
#       }
#       # frame_it <- stepformat(frame_it, trail = F)
#       frame_it <- frame_it %>%
#         mutate(stacked_freq = freq + summed_freq_total) %>%
#         mutate(stacked_freq_base = summed_freq_total)
#       nclas[[clas]] <- frame_it
#     }
#     # nclas[[clas]] <- frame_it
#   }
#   allclus_list <- rlist::list.rbind(nclas)
#   allclus_list
# }
#
# class_stacked_frequencies_daily_movav <- function(ca_trajectories,
#                                                   stepfmt = F,
#                                                   year_range = seq(1980,2020,1),
#                                                   winwidth = 90){
#
#   # ca_trajectories = trajectories
#   # stepfmt = F
#   # year_range = seq(1980,2020,1)
#   # winwidth = 90
#   ## Days in sequence
#   tot_days = length(unique(lubridate::as_date(ca_trajectories$date.start)))
#   day_sequence = seq(1,tot_days,1)
#   ## Fill necessary columns where possible
#   if(!'class_num' %in% colnames(ca_trajectories)){
#     stop("Please add a class and class_num column by inner_joining with the class-cluster assignment table.")
#   }
#   if(!'day_seq' %in% colnames(ca_trajectories)){
#     ca_trajectories <- ca_trajectories %>%
#       mutate(day_seq = cumsum(c(0, as.numeric(diff(lubridate::day(date.start))) != 0)) + 1)
#   }
#
#   # ca_trajectories <- ca_trajectories %>%
#   #   mutate(class_num = as.numeric(forcats::fct_inorder(class)))
#   ## Get the frequency of events per year
#   ## Add daily sequence field to trajectories
#   daily_event_freq <- ca_trajectories %>%
#     group_by(day_seq) %>%
#     summarise(daily_events = n()/121)
#
#   # year_event_freq <- ca_trajectories %>%
#   #   group_by(start_year) %>%
#   #   summarise(total_events = n()/121)
#   ## Get the frequency of each class_num for a given year relative to the max number of occurences.
#   daily_clas <- ca_trajectories %>%
#     group_by(day_seq, class_num) %>%
#     summarise(n = n()) %>%
#     mutate(freq = n / sum(n))
#   # https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
#   # daily_movfreqs <- daily_clas %>%
#   #   group_by(class_num,day_seq) %>%
#   #   summarise(mov_count_test = zoo::rollapply(freq, 91,function(x){sum(x)/91}, align = 'center', fill = NA))
#
#   filldays <- data.frame(matrix(day_sequence)) %>%
#     'colnames<-'(c('day_seq'))
#   nclas <- vector('list', length = length(unique(ca_trajectories$class_num)))
#   for(clas in seq_along(nclas)){
#     clas_it <- clas
#     frame_it <- daily_clas %>%
#       filter(class_num == clas_it) %>%
#       full_join(daily_event_freq) %>%
#       arrange(day_seq) %>%
#       ungroup() %>%
#       mutate(class_num = clas_it) %>%
#       replace(is.na(.), 0) %>%
#       mutate(n_occurrences = n/121) %>%
#       mutate(mov_freq = zoo::rollapply(n_occurrences, winwidth,sum, align = 'center', fill = NA)) %>%
#       mutate(mov_freq = mov_freq/winwidth)
#     # mutate(freq_scaled = total_events*freq)
#     if(clas == 1){
#       frame_it <- frame_it %>%
#         mutate(stacked_freq = mov_freq) %>%
#         mutate(stacked_freq_base = 0)
#       if(isTRUE(stepfmt)){
#         nclas[[clas]] <- stepformat(frame_it, trail = F)
#       } else if(isFALSE(stepfmt)) {
#         nclas[[clas]] <- frame_it
#       } else {
#         stop("Please provide stepfmt as logical TRUE or FALSE.")
#       }
#     } else {
#       # Sum the frequencies of previous groups
#       its <- seq(1,(clas_it-1),1)
#       summed_freq_total <- lapply(nclas[its], function(x){
#         mov_freq <- x$mov_freq
#       }) %>% Reduce('+',.)
#       # summed_freq_base <- nclus[[clas_it-1]]$stacked_freq
#       if(isTRUE(stepfmt)){
#         frame_it <- stepformat(frame_it, trail = F)
#       }
#       # frame_it <- stepformat(frame_it, trail = F)
#       frame_it <- frame_it %>%
#         mutate(stacked_freq = mov_freq + summed_freq_total) %>%
#         mutate(stacked_freq_base = summed_freq_total)
#       nclas[[clas]] <- frame_it
#     }
#     # nclas[[clas]] <- frame_it
#   }
#   allclus_list <- rlist::list.rbind(nclas)
#   allclus_list
# }
#
# ## takes an output from cluster_stacked_frequencies_year. Converts it into a suitable format for filled-in steps.
# plot_stacked_freq_steps <- function(cluster_stckd_freq_year = NULL,
#                                     plot = T){
#   ## Generate lagged stacked frequency ceilings
#   testdat_ceil1 <- cluster_stckd_freq_year %>%
#     # dplyr::filter(cluster == 2) %>%
#     dplyr::select(c(start_year, cluster, stacked_freq)) %>%
#     group_by(cluster)
#   testdat_ceil2 <- bind_rows(old = testdat_ceil1,
#                              new = testdat_ceil1 %>% mutate(stacked_freq = lag(stacked_freq)),
#                              .id = "source") %>%
#     arrange(start_year, source)
#   ## Generate lagged frequency floors
#   testdat_floor1 <- cluster_stckd_freq_year %>%
#     # dplyr::filter(cluster == 2) %>%
#     dplyr::select(c(start_year, cluster, stacked_freq_base)) %>%
#     group_by(cluster)
#   testdat_floor2 <- bind_rows(old = testdat_floor1,
#                               new = testdat_floor1 %>% mutate(stacked_freq_base = lag(stacked_freq_base)),
#                               .id = "source") %>%
#     arrange(start_year, source)
#   ## Combine
#   testdat2 <- merge(testdat_ceil2,testdat_floor2)
#   if(isTRUE(plot)){
#     p <- ggplot() +
#       geom_ribbon(data = testdat2, aes(x = start_year - 0.5, ymin = stacked_freq_base, ymax = stacked_freq, group = as.factor(cluster), fill = as.factor(cluster))) +
#       scale_y_continuous(expand = c(0,0)) +
#       scale_x_continuous(expand = c(0,0), limits = c(2004.5,2020.5), breaks = seq(2005,2020,1)) +
#       theme_cowplot(12) +
#       theme(axis.text.x = element_text(colour = c("black","NA","NA","NA","NA"))) +
#       labs(x = "Year", y = "Frequency",
#            fill = "Cluster") +
#       geom_vline(xintercept = seq(2005.5,2019.5,1), linetype = 'dashed') #+
#     # geom_step(data = testdat, aes(x = start_year-0.5, y = stacked_freq, group = as.factor(cluster)), colour = 'black')
#     p
#   } else {
#     return(testdat2)
#   }
# }
#
# ## Extract data from a trajectory list matching a specific cluster
# extract_cluster_data <- function(cluslist,
#                                  trajectories,
#                                  which_cluster = 1){
#   if(!'cluster' %in% colnames(cluslist)){
#     cluslist <- cluslist %>%
#       dplyr::rename(cluster = V1)
#   }
#   # First, remove the long filenames
#   cluster_rows <- cluslist %>%
#     dplyr::filter(cluster == which_cluster)
#   cluster_fnames <- cluster_rows$V8
#   cluster_fnames_short <- unlist(lapply(str_split(cluster_fnames,"[/]"), tail, n = 1))
#   endpt_years <- unlist(lapply(str_split(cluster_fnames_short,"[-]"),"[[",6))
#   endpt_months <- unlist(lapply(str_split(cluster_fnames_short,"[-]"),"[[",7))
#   endpt_days <- unlist(lapply(str_split(cluster_fnames_short,"[-]"),"[[",8))
#   all_endpt_dates <- as.data.frame(matrix(paste(endpt_years, endpt_months,endpt_days, sep = "-")))
#   ## Date matching column
#   if(!"date.start.yymmdd" %in% colnames(trajectories)){
#     if(!"date.start" %in% colnames(trajectories)){
#       trajectories$date.start.yymmdd <- substr(trajectories$date,3,10)
#     } else {
#       trajectories$date.start.yymmdd <- substr(trajectories$date.start,3,10)
#     }
#   }
#   cluster_trajdata <- trajectories[trajectories$date.start.yymmdd %in% (all_endpt_dates[,]),]
#   cluster_trajdata
# }
#
