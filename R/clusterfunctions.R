# Functions for collating endpoint files for cluster analysis, and for reading, parsing, and analysing the resulting outputs.
# Some of these will be directly useable, and others are from a chapter in my thesis (e.g. cluster classes and related computations) and
# will be less easy to apply directly.


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
