# Functions for plotting results from a cluster analysis.

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
#' @importFrom ggplot2 ggplot
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
    mutate(pct_change = as.numeric((.[[3]]/lag(.[[3]]) * 100)-100))
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
format_DELPCT <- function(DELPCT_table,
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
