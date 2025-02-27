# Functions for reading in outputs from the HYSPLIT cluster program.

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
read_DELPCT <- function(cluster_dir = "C:/hysplit/working/cluster/",
                        max_clusters = NULL,
                        verbose = F){
  if(!file.exists(paste0(cluster_dir,"DELPCT"))){
    stop("No DELPCT file found. Run clusters or check specified cluster working directory")
  } else {
    f <- read.delim(paste0(cluster_dir,"DELPCT"), sep = "") %>%
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

#' Read a single cluster tdump mean trajectory file
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
#' @importFrom tidyselect contains
#' @importFrom magrittr %>%
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
      rename(cluster = V1,
                    lat = V10,
                    lon = V11,
                    height = V12,
                    hour.inc = V9) %>%
      select(-contains("V"))
    tab
  } else if(type == "analogue"){
    file <- read.table(meanclus_tdump) %>%
      select(c(cluster,hour.inc,lat,lon,height))
    file
  } else {
    stop("type not recognised. Please supply one of either 'hysplit' for GUI-generated clusmeans, or 'analogue' for trajSpatial-produced clusmean analogue files.")
  }
}

#' Read a set of cluster tdump files
#'
#' @description Read in all cluster tdump files (i.e. mean cluster trajectory information) in the specified directory, using an
#'  iterative application of `read_meanclus_tdump()`. This can be used to collate all mean cluster information after a set of clustering runs for analysis or display.
#'
#' @param cluster_dir a full filepath to the cluster working directory. Defaults to "C:/hysplit/cluster/working/".
#' @param type One of either 'hysplit' or 'analogue' to specify either a hysplit program-generated file or a cluster mean tdump file generated in R using `create_clusmeans_analogue()`.
#' @param header_string character vector used to target the header line within the tdump file
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#'
#' @export
#'
get_clusmeans_tdump <- function(cluster_dir = "C:/hysplit/cluster/working/",
                                type = "hysplit",
                                header_string = "RESSURE"){
  tdumps_full <- list.files(cluster_dir, full.names = TRUE)
  tdumps_short <- list.files(cluster_dir, full.names = FALSE)
  tdump_means <- tdumps_full[which(str_detect(tdumps_short,"Cmean"))]
  tdump_means_short <- tdumps_short[which(str_detect(tdumps_short,"Cmean"))]
  if(type == "analogue"){
    tdump_means <- tdump_means[which(str_detect(tdump_means_short,"_anlg"))]
    tdump_means_short <- tdump_means_short[which(str_detect(tdump_means_short,"_anlg"))]
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

#' Read a single cluslist file
#'
#' @description Read a single cluslist file. The cluslist file is generated by the HYSPLIT clustering program when a clustering attempt is performed
#'      for a given n cluster value, and contains a table listing each individual endpoint file and the cluster they have been assigned to. This function
#'      is a simple wrapper for read.table().
#'
#' @param CLUSLIST_file a full file path to the desired cluslist file.
#'
#' @export
#'
read_cluslist <- function(CLUSLIST_file){
  read.table(file = CLUSLIST_file)
}


#' Read a set of cluslist files
#'
#' @description Read a set of cluslist files. Individual cluslist files are generated by the HYSPLIT clustering program when a clustering attempt is performed
#'      for a given n cluster value, and contains a table listing each individual endpoint file and the cluster they have been assigned to.
#'
#' @param cluster_wd a full file path to the cluster working directory. Defaults to "C:/hysplit/cluster/working/".
#'
#' @importFrom stringr str_detect
#'
#' @export
#'
get_cluslists <- function(cluster_dir = "C:/hysplit/cluster/working/"){
  files <- list.files(cluster_dir, full.names = TRUE)
  files_short <- list.files(cluster_dir, full.names = FALSE)
  cluslist_files <- files[which(str_detect(files_short,"CLUSLIST"))]
  cluslist_files_short <- files_short[which(str_detect(files_short,"CLUSLIST"))]
  cluslist_tabs <- vector('list', length = length(cluslist_files))
  for(cl in seq_along(cluslist_tabs)){
    cluslist_tabs[[cl]] <- read_cluslist(cluslist_files[cl])
  }
  names(cluslist_tabs) <- cluslist_files_short
  cluslist_tabs
}

#' Read a single endpoint file
#'
#' @description Read and format a single endpoint (trajectory) file. An endpoint file is generated for every run of the hysplit model, and contains
#'      information for individual trajectories or releases. This is a very simple alternative to `splitr::trajectory_read()`
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
    rename(trajnum = V1,
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


