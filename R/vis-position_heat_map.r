#' Position Heat Maps
#'
#' Create heat maps to display the spatial distribution of acoustic telemetry
#' positions. Most useful when used on data with high spatial resultion, such as
#' VPS positional telemetry data.
#'
#' @param positions A dataframe containing detection data with at least the
#'  following 4 columns: \describe{ \item{`DETECTEDID`}{Individual animal
#'  identifier; character.} \item{`DATETIME`}{Date-time stamps for the
#'  positions (MUST be of class 'POSIXct')} \item{`LAT`}{Position
#'  latitude.} \item{`LON`}{Position longitude.} }
#'
#' @param projection A character string indicating if the coordinates in the
#'  'positions' dataframe are geographic (`projection = "LL"`) or
#'  projected/Cartesian(`projection = "UTM"`). Used to convert coordinates
#'  between latitude/longitude in decimal degrees ("LL"; e.g., 45.98753) and
#'  UTM. Valid arguments are "LL" (latitude/longitude) and "UTM". If
#'  projection=="UTM", then `utm_zone` and '`hemisphere` arguments
#'  must also be supplied.
#'
#' @param fish_pos_int A character string indicating whether output will display
#'  number of fish or number of positions occuring in each cell of the grid.
#'  Valid arguments are c("fish", "positions", "intervals"). Default is "fish".
#'  If fish_pos_interval == "intervals", then argument "interval" must be
#'  supplied.
#'
#' @param abs_or_rel A character string indicating whether output will display
#'  values as absolute value (i.e, the actual number of fish, positions, or
#'  intervals) or as relative number (relative to total number of fish
#'  detected). Valid arguments are c("absolute", "relative"). Default is
#'  "absolute".
#'
#' @param  resolution A numeric value indicating the spatial resolution (in
#'  meters) of the grid system used to make the heat maps. Default is 10 m.
#'
#' @param interval A numeric value indicating the duration (in seconds) of time
#'  bin (in seconds) for use in calculating number of intervals fish were
#'  resident in a grid cell (i.e., a surrogate for amount of time spent in each
#'  cell of the grid). If interval==NULL (default), than raw number of positions
#'  is calculated. This value is only used when fish_pos_int == "intervals'.
#'
#' @param x_limits An optional 2-element numeric containing limits of x axis. If
#'  x_limits == NULL (default), then it is determined from the extents of the
#'  data.
#'
#' @param y_limits An optional 2-element numeric containing limits of y axis. If
#'  y_limits == NULL (default), then it is determined from the extents of the
#'  data.
#'
#' @param utm_zone An interger value between 1 and 60 (inclusive) indicating the
#'  primary UTM zone of the detection data. Required and used only when
#'  projection == "UTM". Default is NULL (i.e. assumes detection data are in
#'  projection == LL by default).
#'
#' @param hemisphere A character string indicating whether detection data are in
#'  the northern or southern hemisphere. Required and used only when projection
#'  == "UTM". Valid values are c("N", "S"). Default is "N".
#'
#' @param legend_gradient A character string indicating the orientation of the
#'  color legend; "y" = vertical, "x" = horizontal, "n" indicates that no legend
#'  should be drawn. Default is "y".
#'
#' @param legend_pos A numeric vector indicating the location of the color legend
#'  as a portion of the total plot area (i.e., between 0 and 1). Only used if
#'  'legend_gradient" in not "n". Default is c(0.99, 0.2, 1.0, 0.8), which puts
#'  the legend along the right hand side of the plot.
#'
#' @param output An optional character string indicating how results will be
#'  displayed visually. Options include: 1) a plot in the R device window
#'  ("plot"), 2) a .png image file ("png"), or 3) a .kmz file ("kmz") for
#'  viewing results as an overlay in Google Earth. Accepted values are c("plot",
#'  "png", "kmz"). Default value is "plot".
#'
#' @param folder A character string indicating the output folder. If path is not
#'  specified then `folder` will be created in the working directory.
#'  Default is "position_heat_map".
#'
#' @param out_file A character string indicating base name of output files (if
#'  `output = "png" or "kmz"`). If `out_file` is a path, all but last
#'  part is ignored (via `basename`). Any file extension is also ignored
#'  (via `tools::file_path_sans_ext`).
#'
#' @details When an 'interval' argument is supplied, the number of unique fish x
#'  interval combinations that occurred in each grid cell is calculated instead
#'  of raw number of positions. For example, in 4 hours there are a total of 4
#'  1-h intervals. If fish 'A' was positioned in a single grid cell during 3 of
#'  the 4 intervals, than the number of intervals for that fish and grid
#'  combination is 3. Intervals are determined by applying the
#'  [findInterval][base::findInterval] function (base R) to a sequence of timestamps
#'  (class: POSIXct) created using seq(from = min(positions[, DATETIME]), to =
#'  min(positions[, DATETIME]), by = interval), where interval is the
#'  user-assigned interval duration in seconds. Number of intervals is a more
#'  robust surrogate than number of positions for relative time spent in each
#'  grid in cases where spatial or temporal variability in positioning
#'  probability are likely to significantly bias the distribution of positions
#'  in the array.

#' @details Calculated values (i.e., fish, positions, intervals) can be returned
#'  as absolute or relative, which is specified using the abs_or_rel argument;
#'  "absolute" is the actual value, "relative" is the absolute value divided by
#'  the total number of fish appearing in the 'positions' dataframe. Units for
#'  plots: fish = number of unique fish (absolute) or \% of total fish in
#'  'positions' dataframe (relative); positions = number of positions (absolute)
#'  or mean number of positions per fish in 'positions' dataframe (relative);
#'  intervals = number of unique fish x interval combinations (absolute) or
#'  mean number of unique fish x interval combinations per fish in 'positions'
#'  dataframe (relative).
#'
#' @return A list object containing 1) a matrix of the calculated values (i.e.,
#'  fish, positions, intervals), with row and column names indicating location
#'  of each grid in UTM, 2) a character string specifying the UTM zone of the
#'  data in the matrix, 3) the bounding box of the data in UTM, 4) and the
#'  bounding box of the data in latitude (Y) and longitude (X), 5) a character
#'  string displaying the function call (i.e., a record of the arguments passed
#'  to the function).
#'
#' @return In addition, the user specifies an image output for displaying the
#'  heat map. Options are a "plot" (displayed in R), "png" (png file saved to
#'  specified folder), and "kmz" for viewing the png image as an overlay in
#'  Google Earth (kmz file saved to specified folder).
#'
#' @author Thomas R. Binder
#'
#' @examples
#' data(lamprey_tracks)
#' phm <- position_heat_map(lamprey_tracks)
#'
#' @export

position_heat_map <- function(positions,
                              projection = "LL",
                              fish_pos_int = "fish",
                              abs_or_rel = "absolute",
                              resolution = 10,
                              interval = NULL,
                              x_limits = NULL,
                              y_limits = NULL,
                              utm_zone = NULL,
                              hemisphere = "N",
                              legend_gradient = "y",
                              legend_pos = c(0.99, 0.2, 1.0, 0.8),
                              output = "plot",
                              folder = "position_heat_map",
                              out_file = NULL) {
  # Perform checks on supplied data and arguiments ---------------------------

  # Check that the required columns appear in the detections dataframe
  if (sum(c("DETECTEDID", "DATETIME", "LAT", "LON") %in%
    names(positions)) != 4) {
    stop(
      "The columns 'DETECTEDID', 'DATETIME', 'LAT', and ",
      "'LON' must appear in the positions dataframe."
    )
  }

  # Check that DATETIME is of class 'POSIXct'
  if (!("POSIXct" %in% class(positions$DATETIME))) {
    stop(paste0(
      "Column 'DATETIME' in the positions dataframe ",
      "must be of class 'POSIXct'."
    ))
  }

  # Check that projection is %in% c("LL", "UTM")
  if (!projection %in% c("LL", "UTM")) {
    stop(paste0("Argument 'projection' must be one of 'LL' or 'UTM'."))
  }

  # Check that utm_zone is supplied if projection argument is UTM
  if (projection == "UTM" & is.null(utm_zone)) {
    stop(paste0("Argument 'utm_zone' must be supplied when projection='UTM'."))
  }

  # Check that output is %in% c("plot", "png", "kmz")
  if (!output %in% c("plot", "png", "kmz")) {
    stop(paste0("Argument 'output' must be one of 'plot', 'png', or 'kmz'."))
  }

  # Check that hemisphere is %in% c("N", "S")
  if (!hemisphere %in% c("N", "S")) {
    stop(paste0("Argument 'hemisphere' must be one of 'N' or 'S'."))
  }

  # Check that abs_or_rel is %in% c("N", "S")
  if (!abs_or_rel %in% c("absolute", "relative")) {
    stop(paste0("Argument 'abs_or_rel' must be one of 'absolute' or 'relative'."))
  }

  # Check that hemisphere is %in% c("N", "S")
  if (!fish_pos_int %in% c("fish", "positions", "intervals")) {
    stop(paste0("Argument 'fish_pos_int' must be one of 'fish', 'positions', or 'intervals'."))
  }

  # Check that interval argument is supplied if fish_pos_int == "intervals".
  if (fish_pos_int == "intervals" & is.null(interval)) {
    stop(paste0("Argument 'interval' must be supplied when fish_pos_int == 'intervals'."))
  }

  # Determine x and y limits -------------------------------------------------
  # Set x and y extents based on data range or user-defined limits
  if (is.null(x_limits)) x_limits <- range(positions$LON)
  if (is.null(y_limits)) y_limits <- range(positions$LAT)

  # If projection is 'LL', convert x and y limits to UTM
  if (projection == "LL") {
    range_xylim <- data.frame(Y = c(
      y_limits[1], y_limits[1], y_limits[2],
      y_limits[2]
    ), X = c(
      x_limits[1], x_limits[2],
      x_limits[1], x_limits[2]
    ))
    attr(range_xylim, which = "projection") <- "LL"

    range_xylim <- lonlat_to_utm(range_xylim)
    x_limits <- range(range_xylim$X)
    y_limits <- range(range_xylim$Y)
  }


  # Coerce positions to data.frame
  positions <- as.data.frame(positions)


  # Prepare the positions dataframe ------------------------------------------
  # Remove columns in original dataframe called "X" and "Y" - do this because
  # convUL function requires the LON and LAT data to be stored in columns
  # named X and Y
  positions <- positions[, !names(positions) %in% c("X", "Y")]

  # Rename LON and LAT columns to X and Y for convUL function
  names(positions)[match(c("LON", "LAT"), names(positions))] <- c("X", "Y")

  # Create a projection attribute for the positions dataframe. If projection is
  # UTM, also create a zone attribute for the positions dataframe.
  attr(positions, which = "projection") <- projection
  if (attr(positions, which = "projection") == "UTM") {
    attr(positions, which = "zone") <- "utm_zone"
  }

  # If projection=="LL", convert positions to UTM coordinates
  if (attr(positions, which = "projection") == "LL") {
    positions <- lonlat_to_utm(positions)
    # Change X and Y column names back to original LON and LAT
    names(positions)[match(c("X", "Y"), names(positions))] <- c("LON", "LAT")
  }


  if (output %in% c("png", "kmz")) {
    # Create new directory for results based on user-defined 'folder' ------
    dir.create(folder, showWarnings = FALSE)
    file_path <- normalizePath(folder)
    folder <- basename(folder)
  }


  # Determine the total number of fish in the data set -----------------------
  # Total number of unique transmitters positioned - used for determining
  # relative values
  totalFish <- length(unique(positions$DETECTEDID))


  # Define grid locations ----------------------------------------------------
  seq_longitude <- seq(floor(x_limits[1]), x_limits[2],
    by = resolution
  )
  seq_latitude <- seq(floor(y_limits[1]), y_limits[2],
    by = resolution
  )



  # Define bounding box for output -------------------------------------------
  b_box_UTM <- data.frame(
    corner = c("SW", "SE", "NE", "NW"),
    X = c(
      seq_longitude[1],
      seq_longitude[length(seq_longitude)] + resolution,
      seq_longitude[length(seq_longitude)] + resolution,
      seq_longitude[1]
    ),
    Y = c(
      seq_latitude[1],
      seq_latitude[1],
      seq_latitude[length(seq_latitude)] + resolution,
      seq_latitude[length(seq_latitude)] + resolution
    )
  )

  # Convert b_box to LL for kmz output
  attr(b_box_UTM, which = "projection") <- "UTM"
  attr(b_box_UTM, which = "zone") <- ifelse(projection == "LL", attr(positions, which = "zone"), utm_zone)
  b_box_LL <- utm_to_lonlat(b_box_UTM, hemisphere)

  # Calculate the values to be displayed -------------------------------------
  # Determines in which grid number each position resides.
  positions$BinLon <- findInterval(positions$LON, seq_longitude)
  positions$BinLat <- findInterval(positions$LAT, seq_latitude)

  if (fish_pos_int == "positions") {
    # Calculate num positions ------------------------------------------

    # Create a matrix of the number of positions in each grid.
    # - used to plot NumPositions when abs_or_rel=="absolute"
    results <- as.matrix(unclass(table(
      factor(positions$BinLat, levels = length(seq_latitude):1),
      factor(positions$BinLon, levels = 1:length(seq_longitude))
    )))

    # Sets cells with no positions to NA so they will be transparent in
    # png file
    results[results == 0] <- NA

    # Convert NumPositions to NumPositions/fish.
    # - used to plot NumPositions when abs_or_relPos == "relative"
    if (abs_or_rel == "relative") results <- results / totalFish
  }

  if (fish_pos_int == "intervals") {
    # Calculate num intervals ------------------------------------------
    # Create a sequence from user-defined 'interval' and first and last
    # position times in the positions dataframe.
    interval_seq <- seq(
      from = as.POSIXct(as.Date(min(positions$DATETIME))),
      to = as.POSIXct(as.Date(max(positions$DATETIME) + 86400)),
      by = interval
    )

    # Identify time interval in which each position occurred.
    positions$Interval <- findInterval(positions$DATETIME, interval_seq)

    # Create a new dataframe containing only no-duplicated combinations of
    # DETECTEDID, BinLat, BinLon, and Interval
    # - required to determine the number of unique fish positioned in each grid
    positions2 <- positions[!(duplicated(
      positions[, c("DETECTEDID", "BinLat", "BinLon", "Interval")]
    )), ]

    # Create a matrix containing the number of unique fish x interval
    # combinations in each grid
    # - Used to plot NumIntervals when abs_or_rel == "absolute"
    results <- as.matrix(unclass(table(
      factor(positions2$BinLat, levels = length(seq_latitude):1),
      factor(positions2$BinLon, levels = 1:length(seq_longitude))
    )))
    results[results == 0] <- NA

    # Convert Results matrix to NumIntervals per fish
    # (i.e., divide by the total number of fish detected)
    # - used to plot NumIntervals when abs_or_rel == "relative"
    if (abs_or_rel == "relative") results <- results / totalFish
  }

  if (fish_pos_int == "fish") {
    # Calculate num fish ---------------------------------------------------

    # Create a new dataframe containing only no-duplicated combinations of
    # DETECTEDID, BinLat, and BinLon
    # - required for determining the number of unique fish positioned in
    # each grid
    positions2 <- positions[!(duplicated(
      positions[, c("DETECTEDID", "BinLat", "BinLon")]
    )), ]

    # Create a matrix of the number of unique transmitters positioned in each grid
    # - used to plot NumFish when abs_or_relFish == "absolute"
    results <- as.matrix(unclass(table(
      factor(positions2$BinLat, levels = length(seq_latitude):1),
      factor(positions2$BinLon, levels = 1:length(seq_longitude))
    )))

    # Set cells with no positions to NA so they will be transparent in png file
    results[results == 0] <- NA

    # Convert NumFish matrix to %NumFish, relative to the total number of
    # unique transmitters positioned in the system
    # - used to plot NumFish when abs_or_relFish == "relative"
    if (abs_or_rel == "relative") {
      results <- results / totalFish * 100
    }
  }

  # Output -------------------------------------------------------------------
  if (output %in% c("png", "kmz")) {
    # make output file name (without extension)
    if (is.null(out_file)) {
      out_file <- paste0(fish_pos_int, "_", abs_or_rel)
    } else {
      out_file <- tools::file_path_sans_ext(basename(out_file))
    }

    png_file <- file.path(file_path, paste0(out_file, ".png"))

    kml_file <- file.path(file_path, paste0(out_file, ".kml"))

    kmz_file <- file.path(file_path, paste0(out_file, ".kmz"))


    png(
      file = file.path(png_file),
      bg = "transparent",
      height = 2000,
      width = 2000 * (ncol(results) / nrow(results)),
      pointsize = 38
    )
  }

  par(mar = c(0, 0, 0, 0))
  rast <- raster::raster(results) # coerce to raster

  raster::image(rast, col = c(rev(rainbow(100, end = 0.7))), axes = FALSE)

  if (legend_gradient != "n") {
    plotrix::color.legend(legend_pos[1], legend_pos[2], legend_pos[3], legend_pos[4],
      round(seq(min(results, na.rm = TRUE),
        max(results, na.rm = TRUE),
        by = (max(results, na.rm = TRUE) -
          min(results, na.rm = TRUE)) / 4
      ), 0),
      rev(rainbow(100, end = 0.7)),
      gradient = legend_gradient, font = 2, family = "sans", cex = 1
    )
  }

  if (output %in% c("png", "kmz")) {
    grDevices::dev.off(grDevices::dev.cur())
  }



  # Change row and column names for summary data to corresponding
  # longitudes and latitudes for the corresponding grids.
  rownames(results) <- rev(seq_latitude)
  colnames(results) <- seq_longitude

  # Create a kmz file -------------------------------------------------------
  if (output == "kmz") {
    # Create a text string containing the information required to output a
    # kmz file. This is where the N, S, E, and W bounds of the image are
    # defined for rendering the image in Google Earth.
    kml <- paste0(
      '<?xml version="1.0" encoding="UTF-8"?>',
      '<kml xmlns="http://www.opengis.net/kml/2.2"
  	                    xmlns:gx="http://www.google.com/kml/ext/2.2"
  	                    xmlns:kml="http://www.opengis.net/kml/2.2"
  	                    xmlns:atom="http://www.w3.org/2005/Atom">',
      "<Folder>",
      "<name>", out_file, "</name>",
      "<open>", 1, "</open>",
      "<LookAt>",
      "<longitude>",
      mean(b_box_LL[, "X"]),
      "</longitude>",
      "<latitude>",
      mean(b_box_LL[, "Y"]),
      "</latitude>",
      "<altitude>",
      0,
      "</altitude>",
      "<range>",
      max(diff(b_box_UTM$X), diff(b_box_UTM$Y)),
      "</range>",
      "<tilt>",
      0,
      "</tilt>",
      "<heading>",
      0,
      "</heading>",
      "</LookAt>",
      "<GroundOverlay>",
      paste0("<name>", out_file, "</name>"),
      "<Icon>",
      paste0(
        "<href>", basename(png_file),
        "</href>"
      ),
      "<viewBoundScale>0.75</viewBoundScale>",
      "</Icon>",
      "<gx:LatLonQuad>",
      "<coordinates>",
      paste0(b_box_LL[1, "X"], ",", b_box_LL[1, "Y"], ",", 0, ","),
      paste0(b_box_LL[2, "X"], ",", b_box_LL[2, "Y"], ",", 0, ","),
      paste0(b_box_LL[3, "X"], ",", b_box_LL[3, "Y"], ",", 0, ","),
      paste0(b_box_LL[4, "X"], ",", b_box_LL[4, "Y"], ",", 0, ","),
      "</coordinates>",
      "</gx:LatLonQuad>",
      "</GroundOverlay>",
      "</Folder>",
      "</kml>"
    )

    # Write the kml object to kml text file and places it in the folder
    # containing the three png files.

    write.table(kml,
      file = kml_file,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
    )

    zip::zip(
      zipfile = basename(kmz_file),
      files = c(kml_file, png_file),
      mode = "cherry-pick",
      root = dirname(kmz_file)
    )


    # Delete the kml and png files.
    file.remove(kml_file)
    file.remove(png_file)
  }
  if (output %in% c("png", "kmz")) {
    message(paste0("Output file are located in: ", file_path))
  }


  return(list(
    values = results,
    utm_zone = paste(
      ifelse(projection == "LL",
        attr(positions, which = "zone"), utm_zone
      ),
      hemisphere
    ),
    bbox_UTM = b_box_UTM,
    bbox_LL = b_box_LL,
    function_call = sys.call()
  ))
}


#' Convert geographic positions to UTM
lonlat_to_utm <- function(lonlat) {
  # Calculate UTM zone
  utm_zone <- (floor((lonlat[["X"]] + 180) / 6) %% 60) + 1

  # Add first three digits of EPSG based on hemisphere
  utm_epsg <- rep(NA_real_, length(utm_zone))

  # southern hemisphere
  utm_epsg[lonlat[["Y"]] < 0] <- utm_zone + 32700

  # northern hemisphere
  utm_epsg[lonlat[["Y"]] >= 0] <- utm_zone + 32600

  # if multiple epsg, choose zone with most obs; in case of tie, choose first
  utm_epsg_freq <- table(utm_epsg)
  utm_epsg <- as.integer(names(utm_epsg_freq)[which.max(utm_epsg_freq)][1])

  # Convert to UTM
  lonlat_sf <- sf::st_as_sf(lonlat, coords = c("X", "Y"), crs = 4326)
  utm_sf <- sf::st_transform(lonlat_sf, crs = utm_epsg)

  # Return format consistent with PBSMapping::convUL
  lonlat[, c("Y", "X")] <-
    as.data.frame(sf::st_coordinates(utm_sf))[, c("Y", "X")]

  attr(lonlat, "projection") <- "UTM"
  attr(lonlat, "zone") <- utm_epsg %% 100

  return(lonlat)
}

#' Convert UTM positions to lonlat
utm_to_lonlat <- function(utm, hemisphere) {
  # Define EPSG
  utm_epsg <- ifelse(hemisphere == "N",
    32600 + attr(utm, "zone"),
    32700 + attr(utm, "zone")
  )

  # Convert to longlat
  utm_sf <- sf::st_as_sf(utm, coords = c("X", "Y"), crs = utm_epsg)
  lonlat_sf <- sf::st_transform(utm_sf, crs = 4326)

  # Return format consistent with PBSMapping::convUL
  lonlat_df <- cbind(
    corner = lonlat_sf$corner,
    as.data.frame(sf::st_coordinates(lonlat_sf))[, c("X", "Y")]
  )

  attr(lonlat_df, "projection") <- "LL"
  attr(lonlat_df, "zone") <- attr(utm, "zone")

  return(lonlat_df)
}
