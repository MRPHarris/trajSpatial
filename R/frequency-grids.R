# Functions for creating lat-lon frequency grids from trajectory endpoint data

# The method here involves a few steps
# 1. The first is to coerce the endpoint data into gridded information. This is achieved
#   by rounding the lat/lon coordinates for each endpoint to the nearest halfway point between
#   lat-lon grid cells (effectively 'centering' them within a 1 degree grid cell).
# 2. The second is to build polygon squares for plotting within ggplot.
# 3. Finally, we ensure the information is all plotted in the correct projection.
# With some luck, this means that we can 'seamlessly' plot gridded frequency data for
# trajectory endpoints within ggplot

#' Produce gridded frequency data from hysplit endpoint latitude and longitude coordinates.
#'
#' @description Using gridded trajectory model endpoint data lat/lon coordinates, produce 1-degree
#'      frequency polygons in a grid. Output can be plotted in ggplot. At present, only a
#'      one-degree grid size is supported.
#'
#' @param endpoints_lat numerical data containing the latitude coordinates of hysplit endpoints.
#' @param endpoints_lon numerical data containing the longitude coordinates of hysplit endpoints.
#' @param grid_min_lat The minimum polygon grid latitude.
#' @param grid_max_lat The maximum polygon grid latitude.
#' @param grid_min_lon The minimum polygon grid longitude.
#' @param grid_max_lon The maximum polygon grid longitude.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr count
#' @importFrom dplyr semi_join
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom magrittr %>%
#'
#' @export
#'
create_freq_polygrid <- function(endpoints_lat,
                              endpoints_lon,
                              grid_min_lat = -90,
                              grid_max_lat = -40,
                              grid_min_lon = -180,
                              grid_max_lon = 180){
  ## Format provided lat-lon pairs
  latlon = data.frame(endpoints_lat,endpoints_lon) %>% 'colnames<-'(c('lat','lon'))
  # latlon <- endpoints %>%
  #   dplyr::select(lat, lon)
  ## Coerce to integers
  latlon_rnd1 <- latlon %>%
    mutate(lat = as.integer(lat)) %>%
    mutate(lon = ifelse(test = ((lon > -1) & (lon < 0)),
                        yes = (-0.5),
                        no = (as.integer(lon))))
  ## Coerce into a halfgrid format
  latlon_rnd2 <- latlon_rnd1 %>%
    mutate(lat = ifelse(test = sign(lat) == -1,
                        yes = ifelse(lat == -90, lat + 0.5, lat - 0.5),
                        no = ifelse(lat == 90, lat - 0.5, lat + 0.5))) %>%
    # Additional exception: if the sign is -1, and the value
    mutate(lon = ifelse(test = sign(lon) == -1,
                        yes = ifelse(test = lon == -180,
                                     yes = lon + 0.5,
                                     ifelse(test = lon == -0.5,
                                            yes = (lon*1),
                                            no = (lon - 0.5))),
                        no = ifelse(lon == 180, lon - 0.5, lon + 0.5)))
  ## Generate frequency data (counts per lat-lon pair)
  freqs <- latlon_rnd2 %>% count(lat,lon)
  ## Generate an empty halfgrid
  # Create a halfgrid that reflects the supplied values.
  grid = generate_latlon_halfgrid(grid_min_lat,
                                  grid_max_lat,
                                  grid_min_lon,
                                  grid_max_lon)
  # g <- latlon_halfgrid
  ## Merge to create frequency data-containing halfgrid
  match <- semi_join(freqs,g, by = c("lat","lon"))
  nomatch <- anti_join(g, freqs, by = c("lat","lon")) %>%
    mutate(n = 0)
  grid_freqs <- rbind(match,nomatch) %>%
    mutate(n = as.numeric(n)) %>%
    arrange(lat,lon)
  ## Create the polygon grid.
  polygrid <- generate_latlon_gridpoly(grid_freqs)
  ## Return
  return(polygrid)
}

#' Generate a grid of polygons for plotting.
#'
#' @description use a previously generated lat-lon halfgrid to produce a grid of polygons that can be plotted in ggplot. This can take a little while for larger grids. Progress is shown by default.
#'
#' @param halfgrid an output from generate_latlon_halfgrid()
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#'
#' @noRd
#'
generate_latlon_gridpoly <- function(halfgrid,
                                     verbose = TRUE){
  ## Iteration list and dummy data
  it_list <- vector('list',length = nrow(halfgrid))
  new_dat <- matrix(data = NA, nrow = nrow(halfgrid)*4, ncol = 4) %>% as.data.frame() %>%
    'colnames<-'(c('lat','lon','ID','freq'))
  for(hg in seq_along(it_list)){
    ## Get row indices
    rows <- seq((4*hg)-3,4*hg,1)
    ## halfpoint calcs
    halfpoint <- as.numeric(halfgrid[hg,])
    lat <- halfpoint[1]
    lon <- halfpoint[2]
    freqv <- halfpoint[3]
    ## Need to invert this if the lon sign is negative.
    lonsign <- sign(lon)
    if(lonsign == 1){
      combs <- data.frame(lat = c(ceiling(lat),ceiling(lat),
                                  floor(lat),floor(lat)),
                          lon = c(ceiling(lon), floor(lon),
                                  floor(lon), ceiling(lon)),
                          ID = hg,
                          freq = freqv)
    } else {
      combs <- data.frame(lat = c(ceiling(lat),ceiling(lat),
                                  floor(lat),floor(lat)),
                          lon = c(floor(lon), ceiling(lon),
                                  ceiling(lon), floor(lon)),
                          ID = hg,
                          freq = freqv)
    }
    new_dat[c(rows),] <- combs
    if(hg %% 100 == 0){
      if(verbose){
        message("Row ", hg,"/",length(it_list), " complete")
      }
    }
  }
  ## Reorder
  new_dat %>%
    arrange(lat,lon)
  ## Return
  return(new_dat)
}

#' Generate a lattitude-longitude halfgrid.
#'
#' @description Generate a grid of halfway points between latitude/longitude whole integers within the specified bounding box.
#'
#' @param min_lat The minimum latitude. Lower edge of the bounding box.
#' @param max_lat The maximum latitude. Upper edge of the bounding box.
#' @param min_lon The minimum longitude. Left edge of the bounding box.
#' @param max_lon The maximum longitude. Right edge of the bounding box.
#'
#' @importFrom dplyr relocate
#' @importFrom magrittr %>%
#'
#' @noRd
#'
generate_latlon_halfgrid <- function(min_lat,max_lat,min_lon,max_lon){
  ## Code here based on: https://stackoverflow.com/questions/43612903/how-to-properly-plot-projected-gridded-data-in-ggplot2
  ## test vars
  # min_lat = -90
  # max_lat = -40
  # min_lon = -180
  # max_lon = 180
  # Issue: max values are going to be rounded up.
  ## Create integer sequences, accounting for negatives.
  min_lat_int <- ifelse(sign(min_lat) == -1,
                        yes = as.numeric(as.integer(min_lat)) + 0.5,
                        no = as.numeric(as.integer(min_lat)) - 0.5)
  max_lat_int <- ifelse(sign(max_lat) == -1,
                        yes = as.numeric(as.integer(max_lat)) + 0.5,
                        no = as.numeric(as.integer(max_lat)) - 0.5)
  min_lon_int <- ifelse(sign(min_lon) == -1,
                        yes = as.numeric(as.integer(min_lon))  + 0.5,
                        no = as.numeric(as.integer(min_lon)) - 0.5)
  max_lon_int <- ifelse(sign(max_lon) == -1,
                        yes = as.numeric(as.integer(max_lon))  + 0.5,
                        no = as.numeric(as.integer(max_lon)) - 0.5)
  ## Create the grid
  lat_seq <- seq(min_lat_int,max_lat_int,1)
  lon_seq <- seq(min_lon_int,max_lon_int,1)
  grid <- expand.grid(lon_seq,lat_seq) %>%
    'colnames<-'(c("lon","lat")) %>%
    relocate(lon,.after = lat)
}




