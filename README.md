
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

A common way to spatially analyse HYSPLIT endpoints is by clustering
endpoints - a feature offered by the HYSPLIT GUI itself. I have not yet
found a preferable way to perform clustering outside of the GUI - in any
event doing clusters through the GUI has the advantage of being the
‘done thing’, and you can lean on all the other work using the same
method.

The trajSpatial has a range of functions for collating trajectory
endpoint files to facilitate speedy cluster analysis. The HYSPLIT GUI
will only accept endpoint files output directly by the model - thus, a
file handling framework of sorts is needed. A typical workflow using
these functions involves identifying trajectories matching certain
cluster criteria - e.g. ‘trajectories arriving at x site on y dates’.
Files matching the desired dates are fetched from a common directory,
and sent to the cluster directory. The clustering is then performed
through the GUI, and the output files can be imported back into R.
