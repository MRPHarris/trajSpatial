# Functions used to analyse and manipulate data derived from a clustering run.





#' Generate annual frequencies and stacked frequencies of clusters
#'
#' @description Generate annual frequencies (%) for each cluster within a cluster analysis,
#'      along with stacked cluster data that allows for plotting as a single unit.
#'
#' @param trajdata_frame A dataframe containing trajectories to be associated with those in the cluslist. A multi-trajectory output from trajectory_read() or a similar function. See example package data for an example data frame.
#' @param cluslist A cluslist - an output from `read_cluslist()`.
#' @param stepfmt TRUE/FALSE to format data for a geom_step() in ggplot. Will add an additional observation at the tail of each cluster's frequencies.
#' @param year_range an optional sequence of years (e.g., seq(2005,2020,1) or c(2005,2020)) to subset the trajdata.
#' @param verbose TRUE/FALSE to print messages as the function proceeds.
#'
#' @export
#'
cluster_annual_freqs <- function(trajdata_frame,
                                 cluslist,
                                 stepfmt = FALSE,
                                 year_range = NULL,
                                 verbose = TRUE){
  ## test vars
  # trajdata_frame = trajdata_RA_full
  # cluslist = cluslists[["CLUSLIST_21_retro2023"]] %>%
  #   rename(cluster = V1)
  # stepfmt = F
  # year_range = seq(2005,2020,1)
  # verbose = TRUE

  # var check
  if(!'cluster' %in% colnames(cluslist)){
    cluslist <- cluslist %>% rename(cluster = V1)
  }

  ## Obtain dates for days associated with clusters
  # First, remove the long filenames
  cluster_fnames <- cluslist$V8
  # Why am I doing this with fnames?
  all_endpt_dates <- as.data.frame(matrix(paste(cluslist$V3,
                                                str_pad(cluslist$V4,2,'left',0),
                                                str_pad(cluslist$V5,2,'left',0), sep = "-")))  %>%
    mutate(cluster = cluslist$cluster) %>%
    'colnames<-'(c('just_date','cluster')) %>%
    mutate(just_date = as_date(just_date)) %>%
    arrange(just_date)

  # Modify trajectory data frame
  ca_trajectories <- trajdata_frame %>%
    mutate(just_date = as_date(date.start)) %>%
    arrange(just_date) %>%
    inner_join(all_endpt_dates, by = "just_date") %>%
    add_traj_identifier() %>%
    mutate(start_year = year(as.Date(date.start)))

  # Year constraint
  if(!is.null(year_range)){
    if(verbose){message("Conforming trajectory data to specified year range")}
    ca_trajectories <- ca_trajectories %>%
      filter(year(as.Date(date.start)) >= min(year_range) & year(as.Date(date.start)) <= max(year_range))
  }
  yearly_clus <- ca_trajectories %>%
    filter(hour.inc == 0) %>%
    group_by(start_year, cluster) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))
  test <- data.frame(matrix(year_range)) %>%
    'colnames<-'(c('start_year'))
  nclus <- vector('list', length = length(unique(ca_trajectories$cluster)))
  for(clus in seq_along(nclus)){
    if(verbose){message(clus)}
    clus_it <- clus
    frame_it <- yearly_clus %>%
      filter(cluster == clus_it) %>%
      full_join(test, by = 'start_year') %>%
      arrange(start_year) %>%
      ungroup() %>%
      mutate(cluster = clus_it) %>%
      replace(is.na(.), 0) %>%
      mutate(n_occurrences = n/sum(n))
    if(clus == 1){
      frame_it <- frame_it %>%
        mutate(stacked_freq = freq) %>%
        mutate(stacked_freq_base = 0)
      if(isTRUE(stepfmt)){
        nclus[[clus]] <- stepformat(frame_it, trail = F)
      } else if(isFALSE(stepfmt)) {
        nclus[[clus]] <- frame_it
      } else {
        stop("Please provide stepfmt as logical TRUE or FALSE.")
      }
    } else {
      # Sum the frequencies of previous groups
      its <- seq(1,(clus_it-1),1)
      summed_freq_total <- lapply(nclus[its], function(x){
        freq <- x$freq
      }) %>% Reduce('+',.)
      # summed_freq_base <- nclus[[clus_it-1]]$stacked_freq
      if(isTRUE(stepfmt)){
        frame_it <- stepformat(frame_it, trail = F)
      }
      # frame_it <- stepformat(frame_it, trail = F)
      frame_it <- frame_it %>%
        mutate(stacked_freq = freq + summed_freq_total) %>%
        mutate(stacked_freq_base = summed_freq_total)
      nclus[[clus]] <- frame_it
    }
    # nclus[[clus]] <- frame_it
  }
  allclus_list <- rlist::list.rbind(nclus)
  allclus_list
}

#' Create an analogue cluster mean trajectory from one or more supplied cluslists.
#'
#' @description Takes cluslist files and a set of corresponding trajectories,
#'      and creates a set of clusmean files analogous to those output by the HYSPLIT cluster program.
#'      The calculation uses geosphere::geomean, but produces outputs that differ slightly from HYSPLIT's trajmean program.
#'
#' @param cluslists one or more cluslist files, such as a direct output from read_cluslist() or get_cluslists().
#' @param export_directory NULL or full file path to a directory. If !NULL, mean trajectory tables will be exported as .csvs.
#' @param endpoint_folder full file path to a folder containing the endpoints used in the cluster calculations. If this is different to the file paths found in the cluslist, the endpoint folder will be used instead.
#' @param traj_total_duration_hrs numeric; how long were the trajectories run for, in hours?
#' @param add_file_identifier optional character to add to the exported file name, if an export directory is provided.
#'
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom rlist list.rbind
#' @importFrom dplyr arrange
#' @importFrom geosphere geomean
#'
#' @export
#'
create_clusmeans_analogue <- function(cluslists,
                                      export_directory = NULL,
                                      endpoint_folder,
                                      traj_total_duration_hrs = 120,
                                      add_file_identifier = NULL){
  if(is.null(export_directory)){
    message("No export directory specified. Clusmean analogues will be collated into a nested list. This can use a lot of memory for large trajectory clustering datasets.")
  }
  ## test if one or more cluslists were supplied
  clustlists = cluslists # lazy var pass to legacy code
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
    # Check path and update if different to endpoint folder
    paths <- sapply(clustlist_it$V8, return_path)
    if(length(unique(paths)) > 1){stop("There are multiple file paths in the current cluslist. This shouldn't be possible.")}
    if(unique(paths) != endpoint_folder){
      clustlist_it$V8 <- sapply(clustlist_it$V8,replace_path, new_path = endpoint_folder)
    }
    ## Determine number of clusters
    nclusters <- as.data.frame(clustlist_it$cluster) %>%
      distinct() %>% nrow() %>% as.numeric()
    clust_itlist <- vector('list', length = nclusters)
    for(clust in seq_along(clust_itlist)){
      # clust = 1
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
        round(geomean(xy = ept),3)
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
      if(is.null(add_file_identifier)){
        clusmean_combined_it <- list.rbind(clust_itlist)
        write.table(clusmean_combined_it, file = paste0(export_directory,"Cmean1_",nclusters,"_anlg.tdump"))
        message(paste0("Cmean1_",nclusters,"_anlg.tdump exported"))
      } else {
        clusmean_combined_it <- list.rbind(clust_itlist)
        write.table(clusmean_combined_it, file = paste0(export_directory,"Cmean1_",nclusters,"_",add_file_identifier,"_anlg.tdump"))
        message(paste0("Cmean1_",nclusters,"_",add_file_identifier,"_anlg.tdump exported"))
      }
    } else if(is.null(export_directory)){
      clustlists_compiled[[clist]] <- list.rbind(clust_itlist)
      message(paste0("Cmean1_",nclusters," analog added to complist"))
    }
  }
  if(is.null(export_directory)){
    clustlists_compiled
  }
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
#' @importFrom magrittr %>%
#'
#' @export
#'
get_cluster_percentages <- function(cluslist){
  if(!'cluster' %in% colnames(cluslist)){
    cluslist <- cluslist %>%
      rename(cluster = V1)
  }
  ntrajectories <- nrow(cluslist)
  nclusters <- max(cluslist$cluster)
  # loop to get percentages. This could definitely be done through dplyr but oh well
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
#' @importFrom magrittr %>%
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
    select(c(new_clust,class,class_num)) %>%
    rename(cluster = new_clust)
  cluslist <- cluslist %>%
    inner_join(assignments)
  unique_assignments <- assignments %>%
    select(c(class,class_num)) %>%
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
#' @importFrom magrittr %>%
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
#' @importFrom magrittr %>%
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

## Unused functions
#' Assign trajectories to their clusters within a trajectory data table. Currently incorporated into cluster_stacked_freqs.
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
#' @noRd
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

