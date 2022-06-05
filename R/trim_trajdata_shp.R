# Functions for parsing hysplit trajectory data with shapefiles

#' Archive all endpoints currently in the cluster endpoints directory
#'
#' @description Trims a trajectory data frame to only those points lying within the
#'     bounds of a shapefile polygon.
#'
#' @param shapefile an ARCGIS shapefile (.shp) loaded into R with e.g. readOGR
#' @param trajdata a data frame of trajectory endpoints containing lat/lon data of each endpoint
#' @param xy character vector of the column names containing the x (lon) and y (lat) data within the trajdata data frame.
#'
#' @importFrom raster crs
#' @importFrom sp coordinates
#' @importFrom sf st_as_sf
#' @importFrom sf st_join
#' @importFrom sp spTransform
#'
#' @export
#'
trim_trajdata_shp <- function(shapefile,
                              trajdata,
                              xy = c('lon','lat'),
                              verbose = TRUE){
  # Get started.
  if(isTRUE(verbose)){
    message("Attempting to extract matching trajectory endpoint entries...")
  }
  targetdat <- trajdata %>%
    relocate(xy[1], .before = 1) %>%
    relocate(xy[2], .before = 1) %>%
    rename(Longitude = 2, Latitude = 1)
  if(!"X" %in% colnames(targetdat)){
    targetdat <- targetdat %>%
    mutate(X = row_number())
  }
  # Convert to spatialpoints
  coordinates(targetdat) <- ~Longitude+Latitude
  # This essentially pushes it into EPSG 4326 (default lat/long WGS 84)
  crs(targetdat) = "+proj=longlat +datum=WGS84 +no_defs"
  # Now reproject it into whatever the shapefile is using.
  traj_projected <- spTransform(targetdat, proj4string(shapefile))
  ## Old, clunky transformation: into st.
  # message("Moving to sf object for CRS purposes...")
  # targetdat <- targetdat %>%
  #   st_as_sf(coords = c("Longitude","Latitude"))
  # targetdat <- targetdat %>%
  #   st_set_crs(4326) %>% st_transform(crs=3976)
  # targetdat2 <- targetdat %>%
  #   mutate(Longitude = unlist(map(targetdat$geometry,1)),
  #          Latitude = unlist(map(targetdat$geometry,2)))
  # st_geometry(targetdat2) <- NULL
  ## I could not get over() to work, so the objects are converted to st.
  if(isTRUE(verbose)){
    message("Converting sp objects to st and performing trim")
  }
  newbounds <- st_as_sf(shapefile)
  newpoints <- st_as_sf(traj_projected)
  # Trimming done with a join.
  result <- st_join(newpoints, newbounds, left = F)
  indices <- result$X
  trimmed_trajdata <- trajdata[indices,]
  if(isTRUE(verbose)){
    message("Trimming complete.")
  }
  return(trimmed_trajdata)
}
