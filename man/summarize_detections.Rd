% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summ-summarize_detections.r
\name{summarize_detections}
\alias{summarize_detections}
\title{Summarize detections by animal, location, or both}
\usage{
summarize_detections(
  det,
  location_col = "glatos_array",
  receiver_locs = NULL,
  animals = NULL,
  summ_type = "animal"
)
}
\arguments{
\item{det}{A \code{glatos_detections} object (e.g., produced by
\link{read_glatos_detections}).

\emph{OR} a data frame containing detection
data with four columns described
below and one column containing a location grouping variable, whose name is
specified by \code{location_col} (see below).

The following four columns must appear in \code{det},
except \code{deploy_lat} and \code{deploy_lon} are not needed
if \code{receiver_locs} is specified:
\describe{
\item{\code{animal_id}}{Individual animal
identifier; character.}
\item{\code{detection_timestamp_utc}}{ Timestamps for
the detections (MUST be of class 'POSIXct').}
\item{\code{deploy_lat}}{Latitude of receiver deployment in decimal
degrees, NAD83.}
\item{\code{deploy_long}}{Longitude of receiver deployment in decimal
degrees, NAD83.}
}}

\item{location_col}{A character string indicating the column name in
\code{det} (and \code{receiver_locs} if specified) that will be used as the
location grouping variable (e.g. "glatos_array"), in quotes.}

\item{receiver_locs}{An optional data frame containing receiver data with the
two columns ('deploy_lat', 'deploy_long') described below and one column
containing a location grouping variable, whose name is specified by
\code{location_col} (see above).
The following two columns must appear in \code{receiver_locs}:
\itemize{
\item \code{deploy_lat} Latitude of receiver deployment in decimal
degrees, NAD83.
\item \code{deploy_long} Longitude of receiver deployment in decimal
degrees, NAD83.
}}

\item{animals}{A character vector with values of 'animal_id' that will be
included in summary. This allows (1) animals \emph{not} detected (i.e.,
not present in \code{det}) to be included in the summary and/or (2)
unwanted animals in \code{det} to be excluded from the summary.}

\item{summ_type}{A character string indicating the primary focus of
the summary. Possible values are \code{"animal"} (default),
\code{"location"}, and \code{"both"}. See Details below.}
}
\value{
If \code{summ_type = "animal"} (default): A data frame, data.table, or
tibble containing six columns:
\itemize{
\item{\code{animal_id}: described above.}
\item{\code{num_locs}: number of locations.}
\item{\code{num_dets}: number of detections.}
\item{\code{first_det}: first detection timestamp.}
\item{\code{last_det}: last detections timestamp.}
\item{\code{locations}: character string with
locations detected, separated by spaces.}
}
If \code{summ_type = "location"}: A data frame, data.table, or
tibble containing eight columns:
\itemize{
\item{\code{LOCATION_COL}: defined by \code{location_col}.}
\item{\code{num_fish}: number of unique animals detected.}
\item{\code{num_dets}: number of detections.}
\item{\code{first_det}: first detection timestamp.}
\item{\code{last_det}: last detections timestamp.}
\item{\code{mean_lat}: mean latitude of receivers at this location.}
\item{\code{mean_lon}: mean longitude of receivers at this location.}
\item{\code{animals}: character string with animal_ids detected,
separated by spaces.}
}
If \code{summ_type = "both"}: A data frame, data.table, or tibble
containing seven columns:
\itemize{
\item{\code{animal_id}: described above.}
\item{\code{LOCATION_COL}: defined by \code{location_col}.}
\item{\code{num_dets}: number of detections.}
\item{\code{first_det}: first detection timestamp.}
\item{\code{last_det}: last detections timestamp.}
\item{\code{mean_lat}: mean latitude of receivers at this location.}
\item{\code{mean_lon}: mean longitude of receivers at this location.}
}
}
\description{
Calculate number of fish detected, number of detections, first and last
detection timestamps, and/or mean location of receivers or groups,
depending on specific type of summary requested.
}
\details{
Input argument \code{summ_type} determines which of three possible
summaries is conducted. If \code{summ_type = "animal"} (default), the
output summary includes the following for each unique value of
\code{animal_id}: number of unique locations (defined by unique values of
\code{location_col}), total number of detections across all locations,
timestamp of first and last detection across all locations, and a
space-delimited string showing all locations where each animal was
detected. If \code{summ_type = "location"}, the output summary includes the
following for each unique value of \code{location_col}: number of animals
(defined by unique values of \code{animal_id}), total number of detections
across all animals, timestamp of first and last detection across all
animals, mean latitude and longitude of each location group, and a
space-delimited string of each unique animal that was detected. If
\code{summ_type = "both"}, the output summary includes the following for
each unique combination of \code{location_col} and \code{animal_id}: total
number of detections, timestamp of first and last detection, and mean
latitude and longitude.

If \code{receiver_locs = NULL} (default), then mean latitude and
longitude of each location (\code{mean_lat} and \code{mean_lon} in
output data frame) will be calculated from data in \code{det}. Therefore,
mean locations in the output summary may not represent the mean among
all receiver stations in a particular group if detections did not occur
on all receivers in each group. However, when actual receiver locations
are specified by \code{receiver_locs}, then \code{mean_lat} and
\code{mean_lon} will be calculated from \code{receiver_locs}. Also, if mean
location is not desired or suitable, then \code{receiver_locs} can
be used to pass a single user-specified \code{deploy_lat} and
\code{deploy_long} for each unique value of \code{location_col}, whose
values would then represent \code{mean_lat} and \code{mean_lon} in
the output summary.
}
\examples{

# get path to example detection file
det_file <- system.file("extdata", "walleye_detections.csv",
  package = "glatos"
)
det <- read_glatos_detections(det_file)

# Basic summaries

# by animal
ds <- summarize_detections(det)

# by location
ds <- summarize_detections(det, summ_type = "location")

# by animal and location
ds <- summarize_detections(det, summ_type = "both")


# Include user-defined location_col

# by animal
det$some_place <- ifelse(grepl("^S", det$glatos_array), "s", "not_s")

ds <- summarize_detections(det, location_col = "some_place")

# by location
ds <- summarize_detections(det,
  location_col = "some_place",
  summ_type = "location"
)

# by animal and location
ds <- summarize_detections(det,
  location_col = "some_place",
  summ_type = "both"
)


# Include locations where no animals detected

# get example receiver data
rec_file <- system.file("extdata", "sample_receivers.csv",
  package = "glatos"
)
rec <- read_glatos_receivers(rec_file)

ds <- summarize_detections(det, receiver_locs = rec, summ_type = "location")


# Include animals that were not detected
# get example animal data from walleye workbook
wb_file <- system.file("extdata", "walleye_workbook.xlsm",
  package = "glatos"
)
wb <- read_glatos_workbook(wb_file)

ds <- summarize_detections(det, animals = wb$animals, summ_type = "animal")

# Include by animals and locations that were not detected
ds <- summarize_detections(det,
  receiver_locs = rec, animals = wb$animals,
  summ_type = "both"
)

}
\author{
T. R. Binder and C. Holbrook
}
