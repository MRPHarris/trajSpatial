# Metrics of trajectory data trimmed to a shapefile.

#' Calculate various metrics for trajectory data portions by trimming to a shapefile.
#'
#' @description Trims a trajectory data frame to only those points lying within the
#'     bounds of a shapefile polygon, and then calculates the mean and standard deviation
#'     of various endpoint parameters: height, pressure, lat, lon.
#'
#' @param shapefile an ARCGIS shapefile (.shp) loaded into R with e.g. readOGR
#' @param trajdata a data frame of trajectory endpoints containing lat/lon data of each endpoint
#' @param xy character vector of the column names containing the x (lon) and y (lat) data within the trajdata data frame.
#' @param type character; one of 'month', 'month_seq', or 'year'
#' @param verbose TRUE or FALSE to display progress messages to help with error diagnostics.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr count
#' @importFrom dplyr ungroup
#' @importFrom dplyr semi_join
#' @importFrom dplyr anti_join
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#'
#' @export
#'
trim_trajdata_shp_metrics <- function(shapefile,
                                      trajdata,
                                      xy = c('lon','lat'),
                                      type = "month",
                                      verbose = TRUE){
  # Input type checks
  if((type != "month") && (type != "month_seq") && (type != "year")){
    stop("Error in calculating zonal metrics. Please supply a type of 'month','month_seq', or 'year'.")
  }
  # Trajectory data secondary variables formatting.
  if(!"day_seq" %in% colnames(trajdata)){
    if(isTRUE(verbose)){
      message("Formatting trajectory data. Quicker to do this ahead of time with format_trajdata().")
    }
    trajdata <- format_trajdata_timevars(trajdata, verbose = FALSE) # if not, add them.
    if(isTRUE(verbose)){
      message("Formatting successful.")
    }
  }
  # Trim the data.
  if(isTRUE(verbose)){
    message("Trimming data with supplied shapefile.")
  }
  trimmed_data <- trim_trajdata_shp(shapefile = shapefile,
                                    trajdata = trajdata,
                                    xy = c('lon','lat'),
                                    verbose = verbose)
  if(isTRUE(verbose)){
    message("Trimming successful. Calculating metrics.")
  }
  # Now for the metrics! Way forward depends on 'type' param.
  if((type == "month") || (type == "month_seq")){
    ## Frequencies
    months_seq <- data.frame(unique(trajdata$month_seq)) %>% 'colnames<-'(c('month_seq')) %>%
      arrange(month_seq)
    freqs_long <- trimmed_data %>%
      group_by(month_seq) %>%
      count() %>% ungroup()
    match_long <- semi_join(freqs_long,months_seq, by = c("month_seq"))
    nomatch_long <- anti_join(months_seq, freqs_long, by = c("month_seq")) %>%
      mutate(n = 0)
    freqs_long <- rbind(match_long,nomatch_long) %>%
      mutate(n = as.numeric(n)) %>% arrange(month_seq)
    freqs_long$month <- rep(seq(1,12,1),nrow(freqs_long)/12) # Can't mutate this for some reason.
    freqs_long <- freqs_long %>%
      mutate(id = row_number())
    ## Metrics
    metrics_long <- trimmed_data %>%
      group_by(month_seq) %>%
      summarise(mean_pressure = mean(pressure, na.rm = TRUE),
                sd_pressure = sd(pressure, na.rm = TRUE),
                mean_height = mean(height, na.rm = TRUE),
                sd_height = sd(height, na.rm = TRUE),
                mean_lat = mean(lat, na.rm = TRUE),
                sd_lat = sd(lat, na.rm = TRUE),
                mean_lon = mean(lon, na.rm = TRUE),
                sd_lon = sd(lon, na.rm = TRUE))
    metrics_match <- semi_join(metrics_long,months_seq, by = c("month_seq"))
    metrics_nomatch <- anti_join(months_seq, metrics_match, by = c("month_seq")) %>%
      mutate(mean_pressure = NA) %>% mutate(sd_pressure = NA) %>%
      mutate(mean_height = NA) %>% mutate(sd_height = NA) %>%
      mutate(mean_lat = NA) %>% mutate(sd_lat = NA) %>%
      mutate(mean_lon = NA) %>% mutate(sd_lon = NA)
    metrics_long <- rbind(metrics_match,metrics_nomatch) %>%
      arrange(month_seq)
    metrics_long$month <- rep(seq(1,12,1),nrow(metrics_long)/12)
    metrics_long <- metrics_long %>%
      mutate(id = row_number())
    ## Basic parsing done - now, determine level of monthly analysis.
    if(type == "month_seq"){
      # Finalise and export long format
      metrics_collated_long <- base::merge(freqs_long,metrics_long, sort = FALSE)
      return(metrics_collated_long)
    } else if(type == "month"){
      # Finalise and export short format
      freqs_short <- freqs_long %>%
        group_by(month) %>%
        summarize(
          mean_n = mean(n, na.rm = TRUE),
          sd_n = sd(n, na.rm = TRUE))
      metrics_short <- metrics_long %>%
        group_by(month) %>%
        summarize(
          mean_monthly_pressure = mean(mean_pressure, na.rm = TRUE),
          sd_monthly_pressure = sd(mean_pressure, na.rm = TRUE),
          mean_monthly_height = mean(mean_height, na.rm = TRUE),
          sd_monthly_height = sd(mean_height, na.rm = TRUE),
          mean_monthly_lat = mean(mean_lat, na.rm = TRUE),
          sd_monthly_lat = sd(mean_lat, na.rm = TRUE),
          mean_monthly_lon = mean(mean_lon, na.rm = TRUE),
          sd_monthly_lon = sd(mean_lon, na.rm = TRUE))
      metrics_collated_short <- base::merge(freqs_short, metrics_short, sort = FALSE)
      return(metrics_collated_short)
    }
  } else if(type == "year"){
    ## Frequencies
    years_seq <- data.frame(unique(trajdata$year_seq)) %>% 'colnames<-'(c('year_seq'))
    test_mnth_freqs <- trimmed_data %>%
      group_by(year_seq)
    freqs <- test_mnth_freqs %>% count() %>% ungroup()
    match <- semi_join(freqs,years_seq, by = c("year_seq"))
    nomatch <- anti_join(years_seq, freqs, by = c("year_seq")) %>%
      mutate(n = 0)
    freqs_full <- rbind(match,nomatch) %>%
      mutate(n = as.numeric(n)) %>%
      arrange(year_seq)
    freqs_full <- freqs_full %>%
      mutate(id = row_number())
    ## Metrics
    metrics_long <- trimmed_data %>%
      group_by(year_seq) %>%
      summarise(mean_pressure = mean(pressure, na.rm = TRUE),
                sd_pressure = sd(pressure, na.rm = TRUE),
                mean_height = mean(height, na.rm = TRUE),
                sd_height = sd(height, na.rm = TRUE),
                mean_lat = mean(lat, na.rm = TRUE),
                sd_lat = sd(lat, na.rm = TRUE),
                mean_lon = mean(lon, na.rm = TRUE),
                sd_lon = sd(lon, na.rm = TRUE)) %>% ungroup()
    metrics_match <- semi_join(metrics_long,years_seq, by = c("year_seq"))
    metrics_nomatch <- anti_join(years_seq, metrics_match, by = c("year_seq")) %>%
      mutate(mean_pressure = NA) %>% mutate(sd_pressure = NA) %>%
      mutate(mean_height = NA) %>% mutate(sd_height = NA) %>%
      mutate(mean_lat = NA) %>% mutate(sd_lat = NA) %>%
      mutate(mean_lon = NA) %>% mutate(sd_lon = NA)
    metrics_long <- rbind(metrics_match,metrics_nomatch) %>%
      arrange(year_seq)
    metrics_long$year <- rep(seq(min(trimmed_data$year_start, na.rm = TRUE),max(trimmed_data$year_start, na.rm = TRUE),1))
    metrics_long <- metrics_long %>%
      dplyr::mutate(id = row_number())
    metrics_collated <- base::merge(freqs_full, metrics_long, sort = FALSE)
    return(metrics_collated)
  }
}
