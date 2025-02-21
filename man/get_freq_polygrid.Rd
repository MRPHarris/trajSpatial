% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequency_grids.R
\name{get_freq_polygrid}
\alias{get_freq_polygrid}
\title{Produce gridded frequency data from hysplit endpoint latitude and longitude coordinates.}
\usage{
get_freq_polygrid(
  endpoints_lat,
  endpoints_lon,
  grid_min_lat = -90,
  grid_max_lat = -40,
  grid_min_lon = -180,
  grid_max_lon = 180
)
}
\arguments{
\item{endpoints_lat}{numerical data containing the latitude coordinates of hysplit endpoints.}

\item{endpoints_lon}{numerical data containing the longitude coordinates of hysplit endpoints.}

\item{grid_min_lat}{The minimum polygon grid latitude.}

\item{grid_max_lat}{The maximum polygon grid latitude.}

\item{grid_min_lon}{The minimum polygon grid longitude.}

\item{grid_max_lon}{The maximum polygon grid longitude.}
}
\description{
Using gridded trajectory model endpoint data lat/lon coordinates, produce 1-degree frequency polygons in a grid. Output can be plotted in ggplot.
}
