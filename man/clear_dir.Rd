% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusterfunctions.R
\name{clear_dir}
\alias{clear_dir}
\title{Move all files in one directory to another}
\usage{
clear_dir(
  from_dir = "C:/hysplit/cluster/working/",
  to_dir = "C:/hysplit/cluster/archive/",
  archive_to_dir = TRUE,
  archive_name = NULL,
  overwrite = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{from_dir}{full path to the folder containing the files. Windows cluster working directory by default.}

\item{to_dir}{full file path to the destination folder. Windows cluster archive by default.}

\item{archive_to_dir}{TRUE/FALSE to archive files currently in the to_dir. They will be moved to a timestamped folder within a new subfolder named 'previous'.}

\item{archive_name}{NULL or character string. If specified, adds the character to the timestamped subfolder used to archive existing to_dir contents.}

\item{overwrite}{TRUE/FALSE to overwrite existing to_dir folder contents if no archiving is performed.}

\item{verbose}{TRUE/FALSE to print status messages.}
}
\description{
Performs the 'archive' function from the HYSPLIT cluster GUI if used on the cluster working directory. All files are moved from the from_dir to the to_dir. By default, existing files in the archive are themselves archived.
}
