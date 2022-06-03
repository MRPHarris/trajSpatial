
# Intuitive spatial analysis of hysplit endpoints in R

This package is a work-in-progress framework for the spatial analysis of
hysplit trajectory endpoints.

The current github repo is <https://github.com/MRPHarris/trajSpatial>.

## Introduction

The trajSpatial package provides a number of routes for the spatial
analysis of trajectory data. It is primarily designed for trajectory
endpoints output by the NOAA HYSPLIT model, and preferably those
generated using splitr package in R.

## Spatial/geographic analysis of endpoint data

Two routes are provided for area-based spatial analysis: simple
grid-squares, and shapefile/spatialdata trimming.

The former is a simple grid-square framework, where the user defines one
or more lat-lon grid squares, and various functions will calculate
temporal metrics/statistics for those regions - endpoint frequency over
time, mean pressure over time, etc. Temporal scales exist from seasonal
(binned months over the full time period spanned by the trajectory
dataset), months-over-time (sequential months for the dataset duration),
and yearly. The benefit of the grid-square approach is that it is
projection-free; the user simply designs the grids using whatever
coordinates they like.

In the second case, shapefiles are used as polygons to define geographic
regions. This is significantly more precise but can be more finnicky -
the user must content with all the joys of working with shapefiles in R
(projections, etc.).

In the future, I hope to add more complex features such as a potential
source contribution function (PSCF) after e.g. Sinclair et al., 2013,
but for now the spatial frequencies produced by the package are
unweighted.

## Facilitating HYSPLIT cluster analysis

The HYSPLIT GUI has a clustering function that enables spatial
[cluster](https://www.ready.noaa.gov/documents/Tutorial/html/traj_clus.html)
analysis of supplied endpoints. A series of function in this package
provide a straightforward means to collate and archive clusters,
allowing relatively fast use of the clustering GUI.

To collate a series of endpoints for cluster analysis, use the
`collate_endpts()` function. This will move all supplied endpoints to
the supplied directory (e.g., the cluster endpts directory). A vector of
dates can also be supplied to match endpoint files. Options for
recursion. Something to be aware of is that the cluster analysis fails
if any endpoint filenames are \<54 characters - the package incorporates
adaptive file renaming to prevent this.

Once the endpoints have been moved to the endpt directory, perform the
cluster analysis. Once it is complete, you can move the endpoint files
from the endpt directory to an archive with
`archive_cluster_endpoints()`.

Repeat as desired.
