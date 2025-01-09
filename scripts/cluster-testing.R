# Running Ward variance clustering with splitr, trajSpatial and hysplit.

# This is essentially a script version of the tutorial.

# Last updated 2023-08-11
# Created by Matt Harris
# matthew.harris@unsw.edu.au
# https://www.github.com/MRPHarris

# Written and run in:
#   Rstudio v2022.02.3
#   R v4.2.2
#   Windows 10 (Education)

#### SETUP ####

## Load needed relevant packages
library(pacman)
pacman::p_load(trajSpatial,
               splitr,
               tidyverse,
               dplyr,
               magrittr,
               pracma,
               data.table,
               signal,
               cowplot)

## Source internal package functions
# Directories
proj_dir <- "C:/Users/matth/Desktop/University Files/mh phd 2020/software_and_coding/rstudio/MyPackages/trajSpatial/"
scripts_dir <- paste0("C:/Users/matth/Desktop/University Files/mh phd 2020/software_and_coding/rstudio/MyPackages/trajSpatial/","R/")
# Source all functions that are a part of the package.
files <- list.files(scripts_dir, full.names = TRUE)
flist <- vector(mode = "list", length = length(files))
for(i in seq_along(flist)){
  source(files[i])
}

## Directories used to get data
base_dir <- "C:/Users/matth/Desktop/Work/software-and-coding/trajSpatial package testing/" # Base directory: the parent folder you're working in.
date_dir <- paste0(base_dir,"date vectors/") # Date directory: store date vectors in here.
cluster_export_dir <- paste0(base_dir,"cluster outputs/") # export destination for cluster data
target_dir <- "D:/DATA/HYSPLIT Files/Endpoint files/PH_1500m_240hr_endpoints/" # location of the trajectory endpoint files to be clustered
run_name <- "PH_1500m_GDAS1_120hr_2d12h" # what's the name of this trajectory endpoint set?

##### Preparing a cluster run #####

## Setting directories

## SUPPLYING ENDPOINTS
# When clustering, the program needs to be supplied with a set of endpoints.
# These are, by default, stored in the "~/hysplit/cluster/endpts/"
# First, make sure the endpts directory is clear of any endpoint files.
archive_cluster_endpoints(cluster_endpoints_dir = "C:/hysplit/cluster/endpts/",
                          cluster_endpoints_archive_dir = "C:/hysplit/cluster/endpts/archive/",
                          archive_name = "deception-test",
                          stopifempty = FALSE,
                          append_timestamp = TRUE)

# Now, move a set of trajectories to the cluster endpoints directory. You can supply directly from their original folder to
# the cluster algorithm, but I find that partitioning can avoid unintentional errors (e.g., modification of the precious original endpoint files).
# It is also a chance to make changes to files that avoid errors.
# Specifically, (1) ensure filenames are <54 characters long, and (2) remove extended meteorological information from endpoint files. Both these will cause the clustering operation to fail.

# This package includes a 'collate endpoints' function that can be used to move trajectories to the directory.
# Move deception endpoints from hysplit outputs to the cluster wd.
collate_endpts(from_dir = "C:/hysplit/working/1/deception_test/",
               to_dir = "C:/hysplit/cluster/endpts/",
               rename_long_files = c("traj","test","lon","lat"))

# Alright, now the endpoints are in the correct directory. Next, clear out the rest of the stuff in the working folder.
# This is equivalent to the 'archive' button in the hysplit cluster GUI.
# In this case, we can use the defaults.
clear_dir(from_dir = "C:/hysplit/cluster/working/",
          to_dir = "C:/hysplit/cluster/archive/")

# Next, make the INFILE.
# For this we can use the function defaults.
make_INFILE()




## Failing.

collate_endpts <- function(from_dir,
                           recurse = FALSE,
                           target_traj = NULL,
                           to_dir = "C:/hysplit/cluster/endpts/",
                           date_vec = NULL,
                           hour_vec = NULL,
                           rename_long_files = c("traj","YSH","lon","lat"),
                           format_endpts = TRUE){
  # Test vars
  from_dir = "C:/hysplit/working/1/deception_test/"
  recurse = FALSE
  target_traj = NULL
  to_dir = "C:/hysplit/cluster/endpts/"
  date_vec = NULL
  hour_vec = NULL
  rename_long_files = c("traj","lon","lat")
  format_endpts = TRUE
  # end test vars


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



format_endpt_forcluster <- function(endpt_file,
                                    hour_limit = -120){
  # var testing
  endpt_file = endpt_file_it
  hour_limit = -120

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
    if(!isempty(hrind)){
      new_line_elements <- new_line_elements[1:hrind]
    }
  }
  file_lines_data3 <- new_line_elements
  new_file <- c(pre_header_lines,header_line_new,file_lines_data3)
  # write(new_file,paste0(test_dir,"Modified_bad_file"))
  new_file
}


## When preparing a cluster run, first prepare the folder.
archive_cluster_wd <- function(cluster_wd = "C:/hysplit/cluster/working/",
                               archive_dir = "C:/hysplit/cluster/archive/")


archive_cluster_endpoints()

# Test
make_INFILE(endpoint_dir = "C:/hysplit/working/1/deception_test/",
            clus_working_dir = "C:/hysplit/cluster/working/test/",
            flag_longfilenames = FALSE,
            export = TRUE)

endpoint_dir <- "C:/hysplit/working/1/deception_test/"
clus_working_dir <- "C:/hysplit/cluster/working/test/"


#### STEP 1: DATE VECTORS

## Step 1: collate endpoints using the supplied date vectors.
type = 'GDAS1'
# # date_dir <- era5_dir
# dates_precip_1pct <- readRDS(file = paste0(date_dir,"precip_1pct_days.rds"))
# if(type == 'reanalysis'){
#   date_vec <- dates_precip_1pct %>%
#     dplyr::filter(date >= as_date("1980-01-01"))
#   date_vec <- date_vec$date
# } else {
#   date_vec <- dates_precip_1pct %>%
#     dplyr::filter(date >= as_date("2005-01-01"))
#   date_vec <- date_vec$date
# }
# archive_cluster_endpoints(stopifempty = F)
# collate_endpts(from_dir = target_dir,
#                recurse = TRUE)

## S2: Run clustering
# Go into HYSPLIT, and run the clustering

## Step 3: get the DELPCT file
# This is now a part of trajSpatial::read_DELPCT()
library(trajSpatial)
DELPCT_it <- trajSpatial::read_DELPCT(max_clusters = 100)
# saveRDS(DELPCT_it, file = paste0(cluster_export_dir,"DELPCT outputs/",run_name,"_DELPCT.rds"))
# write.csv(DELPCT_it,file = paste0(cluster_export_dir,"DELPCT outputs/",run_name,"_DELPCT.csv"))

## Optional plotting
plot_DELPCT(DELPCT_it, threshold = 20)

## Step 4: export the mean files when they are created.
# After identifying appropriate numbers of trajectories, put them into their means.
# To do this, 'assign trajectories to means', then hit 'display means'. You don't have to actually plot them,
# but this second step creates the tdump files.

tdumps_full <- list.files("C:/hysplit/cluster/working/", full.names = TRUE)
tdumps_short <- list.files("C:/hysplit/cluster/working/", full.names = FALSE)
tdump_means <- tdumps_full[which(stringr::str_detect(tdumps_short,"Cmean"))]
tdump_means_short <- tdumps_short[which(stringr::str_detect(tdumps_short,"Cmean"))]
read_meanclus_tdump <- function(meanclus_tdump,
                                header_string = "RESSURE"){
  ## borrowed from splitr and other trajSpatial functions
  # file_i_path <- meanclus_tdump
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
}

mean_tabs <- vector('list', length = length(tdump_means))
for(mn in seq_along(mean_tabs)){
  mean_tabs[[mn]] <- read_meanclus_tdump(tdump_means[mn])
}
names(mean_tabs) <- unlist(lapply(str_split(tdump_means_short,"[.]"),"[[",1))

# Save means
saveRDS(object = mean_tabs, file = paste0(cluster_export_dir,"Mean cluster files/",run_name,"_means.rds"))
