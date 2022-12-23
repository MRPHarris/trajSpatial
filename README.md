
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
supplied shapefile (imported using `rgdal::readOGR()`) via the function
`trim_trajdata_shp()`.

A note on projection handling. In order to ensure the trajectory data
conforms to whatever projection the shapefile is using, the data is
first coverted to a spatial points data frame using and defined as using
the EPSG 4326 projection (setting the proj4 string to “+proj=longlat
+datum=WGS84 +no_defs”). The data is then transformed to the same
projection as the shapefile.

One can go a step further and calculate the metrics of trajectories
falling within a supplied shapefile by using
`trim_trajdata_shp_metrics()`. This will return a data frame with the
mean and sd of height, pressure, lat and lon of all endpoints falling
within the shapefile. Three types of metrics are available,
corresponding to different ways of temporally parsing the data: ‘month’
for bulk analysis of the months (i.e. seasonal analysis), ‘month_seq’
for months in sequence over the duration of the parent trajectory data,
and ‘year’ for each year for the duration of the parent trajectory data.

In the future, I hope to add more complex features such as a potential
source contribution function (PSCF) after e.g. Sinclair et al., 2013,
but for now the spatial frequencies produced by the package are
unweighted.

## Facilitating HYSPLIT cluster analysis

The HYSPLIT GUI has a clustering function that enables spatial
[cluster](https://www.ready.noaa.gov/documents/Tutorial/html/traj_clus.html)
analysis of supplied endpoints. A series of functions in this package
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

## Parsing and analysis of cluster files produced by HYSPLIT

The HYSPLIT clustering program produces a range of files (‘Cmean’,
‘cluslist’) at various points in a clustering analysis. These files are
variously used to store information on which endpoint files
(i.e. trajectories) are associated with which clusters, and to plot the
mean trajectories of said clusters. These files can be read into R and
analysed outside of the GUI. A suite of functions are provided in this
package to read, analyse, and plot the various outputs of the clustering
program. These can be used to, for example, circumvent the
[5000-trajectory hard-coded
limit](https://hysplitbbs.arl.noaa.gov/viewtopic.php?f=3&t=2466) of the
HYSPLIT cluster mean-assignment program (‘trajmean’), that prevents any
visualising of mean trajectories in large analyses. At present the mean
trajectory calculation uses a simple geometric mean, which produces
slight differences in mean trajectory paths to the HYSPLIT program.
