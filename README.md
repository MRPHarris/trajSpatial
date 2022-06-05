
# Spatial analysis of hysplit endpoints in R

This package is a work-in-progress (!!) framework for the spatial
analysis of hysplit trajectory endpoints.

The current github repo is <https://github.com/MRPHarris/trajSpatial>.

## Introduction

The trajSpatial package provides a number of routes for the spatial
analysis of trajectory data. It is primarily designed for trajectory
endpoints output by the NOAA HYSPLIT model, and preferably those
generated using splitr package in R.

At present, the functions have been tested on trajectory data subject to
intermediate formatting steps that have not yet been added. I.e. some or
all may not work when applied to data output directly from the model via
GUI or splitr. Feel free to browse the code, though!

## Spatial/geographic analysis of endpoint data

At the moment, the package provides a straightforward means to trim
trajectory files to endpoints only falling within the area bounded by a
supplied shapefile via the function `trim_trajdata_shp()`. One can go a
step further and calculate the metrics of trajectories falling within a
supplied shapefile by using `trim_trajdata_shp_metrics()`. This will
return a data frame with the mean and sd of height, pressure, lat and
lon of all endpoints falling within the shapefile. Three types of
metrics are available, corresponding to different ways of temporally
parsing the data: ‘month’ for bulk analysis of the months (i.e. seasonal
analysis), ‘month_seq’ for months in sequence over the duration of the
parent trajectory data, and ‘year’ for each year for the duration of the
parent trajectory data.

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
