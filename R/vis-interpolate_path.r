#' Interpolate new positions within a spatiotemporal path data
#'
#' Interpolate new positions within a spatiotemporal path data set
#'   (e.g., detections of tagged fish) at regularly-spaced time intervals
#' 	 using linear or non-linear interpolation.
#'
#' @param det An object of class `glatos_detections` or data frame
#'   containing spatiotemporal data with at least 4 columns containing
#'   'animal_id', 'detection_timestamp_utc', 'deploy_long', and
#'   'deploy_lat' columns.
#'
#' @param start_time specify the first time bin for interpolated data.
#'     If not supplied, default is first timestamp in the input data
#'     set.  Must be a character string that can be coerced to
#'     'POSIXct' or an object of class 'POSIXct'.  If character string
#'     is supplied, timezone is automatically set to UTC.
#'
#' @param out_class Return results as a data.table or tibble.  Default
#'   returns results as data.frame.  Accepts `data.table` or `tibble`.
#'
#' @param int_time_stamp The time step size (in seconds) of interpolated
#'   positions. Default is 86400 (one day).
#'
#' @param trans An optional transition matrix with the "cost" of
#'   moving across each cell within the map extent. Must be of class
#'   `TransitionLayer`. A transition layer may be
#'   created from a polygon shapefile using [make_transition].
#'
#' @param lnl_thresh A numeric threshold for determining if linear or
#'   non-linear interpolation shortest path will be used.
#'
#' @param show_progress Logical. Progress bar and status messages will be
#'  shown if TRUE (default) and not shown if FALSE.
#'
#' @details Non-linear interpolation uses the `gdistance` package
#'   to find the shortest pathway between two locations (i.e.,
#'   receivers) that avoid 'impossible' movements (e.g., over land for
#'   fish). The shortest non-linear path between two locations is
#'   calculated using a transition matrix layer that represents the
#'   'cost' of an animal moving between adjacent grid cells.  The
#'   transition matrix layer (see [gdistance]) is created from
#'   a polygon shapefile using [make_transition] or from a
#'   `RasterLayer` object using [transition][gdistance::transition]. In
#'   `make_transition`, each cell in the output transition layer
#'   is coded as water (1) or land (0) to represent possible (1) and
#'   impossible (0) movement paths.
#'
#' @details Linear interpolation is used for all points when
#'   `trans` is not supplied.  When `trans` is supplied,
#'   then interpolation method is determined for each pair of
#'   sequential observed detections. For example, linear interpolation
#'   will be used if the two geographical positions are exactly the
#'   same and when the ratio (linear distance:non-linear distance)
#'   between two positions is less than `lnl_thresh`.  Non-linear
#'   interpolation will be used when ratio is greater than
#'   `lnl_thresh`.  When the ratio of linear distance to
#'   non-linear distance is greater than `lnl_thresh`, then the
#'   distance of the non-linear path needed to avoid land is greater
#'   than the linear path that crosses land.  `lnl_thresh` can be
#'   used to control whether non-linear or linear interpolation is
#'   used for all points. For example, non-linear interpolation will
#'   be used for all points when `lnl_thresh` > 1 and linear
#'   interpolation will be used for all points when `lnl_thresh`
#'   = 0.
#'
#' @details All linear interpolation is done by [stats::approx()] with
#'   argument `ties = "ordered"` controlling how tied `x` values
#'   are handled. See [stats::approxfun()].
#'
#' @return A dataframe with animal_id, bin_timestamp,
#'   latitude, longitude, and record_type.
#'
#'
#' @author Todd Hayden, Tom Binder, Chris Holbrook
#'
#' @examples
#'
#' #--------------------------------------------------
#' # EXAMPLE #1 - simple interpolate among lakes
#'
#' # get polygon of the Great Lakes
#' data(great_lakes_polygon) # glatos example data
#' plot(sf::st_geometry(great_lakes_polygon), xlim = c(-92, -76))
#'
#' # make sample detections data frame
#' pos <- data.frame(
#'   animal_id = 1,
#'   deploy_long = c(-87, -82.5, -78),
#'   deploy_lat = c(44, 44.5, 43.5),
#'   detection_timestamp_utc = as.POSIXct(c(
#'     "2000-01-01 00:00",
#'     "2000-02-01 00:00", "2000-03-01 00:00"
#'   ), tz = "UTC")
#' )
#'
#' # add to plot
#' points(deploy_lat ~ deploy_long, data = pos, pch = 20, cex = 2, col = "red")
#'
#' # interpolate path using linear method
#' path1 <- interpolate_path(pos)
#' nrow(path1) # now 61 points
#' sum(path1$record_type == "interpolated") # 58 interpolated points
#'
#' # add linear path to plot
#' points(latitude ~ longitude, data = path1, pch = 20, cex = 0.8, col = "blue")
#'
#' # load a transition matrix of Great Lakes
#' # NOTE: This is a LOW RESOLUTION TransitionLayer suitable only for
#' #       coarse/large scale interpolation only. Most realistic uses
#' #       will need to create a TransitionLayer; see ?make_transition.
#' data(greatLakesTrLayer) # glatos example data; a TransitionLayer
#'
#' # interpolate path using non-linear method (requires 'trans')
#' path2 <- interpolate_path(pos, trans = greatLakesTrLayer)
#'
#' # add non-linear path to plot
#' points(latitude ~ longitude,
#'   data = path2, pch = 20, cex = 1,
#'   col = "green"
#' )
#'
#' # can also force linear-interpolation with lnlThresh = 0
#' path3 <- interpolate_path(pos, trans = greatLakesTrLayer, lnl_thresh = 0)
#'
#' # add new linear path to plot
#' points(latitude ~ longitude,
#'   data = path3, pch = 20, cex = 1,
#'   col = "magenta"
#' )
#'
#' #--------------------------------------------------
#' # EXAMPLE #2 - walleye in western Lake Erie
#' \dontrun{
#'
#'
#' # get example walleye detection data
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'   package = "glatos"
#' )
#' det <- read_glatos_detections(det_file)
#'
#' # take a look
#' head(det)
#'
#' # extract one fish and subset date
#' det <- det[det$animal_id == 22 &
#'   det$detection_timestamp_utc > as.POSIXct("2012-04-08") &
#'   det$detection_timestamp_utc < as.POSIXct("2013-04-15"), ]
#'
#' # get polygon of the Great Lakes
#' data(great_lakes_polygon) # glatos example data; an sf object
#'
#' # convert polygon to terra::spatVector
#' great_lakes_polygon <- terra::vect(great_lakes_polygon)
#'
#' # crop polygon to western Lake Erie
#' maumee <- terra::crop(great_lakes_polygon,
#'   y = terra::ext(-83.7, -82.5, 41.3, 42.4)
#' )
#'
#'
#' plot(maumee, col = "grey")
#' points(deploy_lat ~ deploy_long,
#'   data = det, pch = 20, col = "red",
#'   xlim = c(-83.7, -80)
#' )
#'
#'
#' # make transition layer object
#' tran <- make_transition(sf::st_as_sf(maumee), res = c(0.1, 0.1))
#'
#' # plot to check output
#' plot(tran$rast, xlim = c(-83.7, -82.0), ylim = c(41.3, 42.7))
#' plot(maumee, add = TRUE)
#'
#' # not high enough resolution- bump up resolution, will take some time
#' tran1 <- make_transition(sf::st_as_sf(maumee), res = c(0.001, 0.001))
#'
#' # plot to check resolution- much better
#' plot(tran1$rast, xlim = c(-83.7, -82.0), ylim = c(41.3, 42.7))
#' plot(maumee, add = TRUE)
#'
#'
#' # add fish detections to make sure they are "on the map"
#' # plot unique values only for simplicity
#' foo <- unique(det[, c("deploy_lat", "deploy_long")])
#' points(foo$deploy_long, foo$deploy_lat, pch = 20, col = "red")
#'
#' # call with "transition matrix" (non-linear interpolation), other options
#' # note that it is quite a bit slower than linear interpolation
#' pos2 <- interpolate_path(det,
#'   trans = tran1$transition,
#'   out_class = "data.table"
#' )
#'
#' plot(maumee, col = "grey")
#' points(latitude ~ longitude, data = pos2, pch = 20, col = "red", cex = 0.5)
#' }
#'
#' @export



interpolate_path <- function(det,
                             trans = NULL,
                             start_time = NULL,
                             int_time_stamp = 86400,
                             lnl_thresh = 0.9,
                             out_class = NULL,
                             show_progress = TRUE) {
  ##  Declare global variables for NSE & R CMD check
  detection_timestamp_utc <- record_type <- num_rows <- animal_id <- bin <-
    bin_stamp <- i_lat <- deploy_lat <- i_lon <- deploy_long <-
    bin_timestamp <- num <- start_dtc <- x.animal_id <-
    x.detection_timestamp_utc <- x.deploy_lat <- x.deploy_long <-
    x.record_type <- x.num_rows <- x.bin <- gcd <- i.start <-
    lcd <- crit <- t_lat <- t_lon <- t_timestamp <- coord <- grp <-
    nln_longitude <- nln_latitude <- seq_count <- i_time <- latitude_lead <-
    longitude_lead <- cumdist <- NULL


  # stop if out_class is not NULL, data.table, or tibble
  if (!is.null(out_class)) {
    if (!(out_class %in% c("data.table", "tibble"))) {
      stop('out_class is not a "data.table" or "tibble"')
    }
  }

  # check to see that trans is a transition layer or transition stack
  if (!is.null(trans) &
    inherits(trans, c("TransitionLayer", "TransitionStack")) == FALSE) {
    stop(
      paste0(
        "Supplied object for 'trans' argument is not class ",
        "TransitionLayer or TransitionStack."
      ),
      call. = FALSE
    )
  }

  # check start_time
  if (is.null(start_time)) {
    start_time <- min(det$detection_timestamp_utc)
  }
  if (is.na(start_time) & length(start_time) > 0) {
    stop("start_time cannot be coerced to 'POSIXct' or 'POSIXt' class")
  }

  if (is.character(start_time)) {
    start_time <- as.POSIXct(start_time, tz = "UTC")
  }

  # make sure start_time < largest timestamp in dataset
  if (start_time > max(det$detection_timestamp_utc)) {
    stop("start_time is larger than last detection.  No data to interpolate!", call. = FALSE)
  }

  # make copy of detections for function
  dtc <- data.table::as.data.table(det)

  # subset only columns for function and rows >= start_time:
  dtc <- dtc[detection_timestamp_utc >= start_time, c(
    "animal_id",
    "detection_timestamp_utc",
    "deploy_lat",
    "deploy_long"
  )]

  dtc[, record_type := "detection"]

  # count number of rows- single observations are not interpolated
  dtc[, num_rows := nrow(.SD), by = animal_id]

  # Sort detections by transmitter id and then by detection timestamp
  data.table::setkey(dtc, animal_id, detection_timestamp_utc)

  # save original dataset to combine with interpolated data in the end
  det <- data.table::copy(dtc)
  data.table::setnames(det, c(
    "animal_id", "bin_stamp", "i_lat", "i_lon",
    "record_type", "num_rows"
  ))

  # remove any fish with only one detection
  dtc <- dtc[num_rows != 1]

  # error if only fish with one observation.
  if (nrow(dtc) == 0) stop("must have two observations to interpolate")

  # extract and determine start time
  t_seq <- seq(
    start_time, max(dtc$detection_timestamp_utc),
    int_time_stamp
  )

  # bin data by time interval and add bin to dtc
  dtc[, bin := t_seq[findInterval(detection_timestamp_utc, t_seq)]]

  # make all combinations of animals and detection bins
  dtc <- dtc[data.table::CJ(bin = t_seq, animal_id = unique(animal_id)),
    on = c("bin", "animal_id")
  ]

  data.table::setkey(dtc, animal_id, bin, detection_timestamp_utc)

  # if only need to do linear interpolation:
  if (is.null(trans) | lnl_thresh == 0) {
    dtc[, bin_stamp := detection_timestamp_utc][
      is.na(detection_timestamp_utc),
      bin_stamp := bin
    ]

    dtc[, i_lat := approx(detection_timestamp_utc, deploy_lat,
      xout = bin_stamp, ties = "ordered"
    )$y, by = animal_id]
    dtc[, i_lon := approx(detection_timestamp_utc, deploy_long,
      xout = bin_stamp, ties = "ordered"
    )$y, by = animal_id]

    dtc[is.na(deploy_long), record_type := "interpolated"]
    dtc <- dtc[, c("animal_id", "bin_stamp", "i_lat", "i_lon", "record_type")]
    det <- det[num_rows == 1, c(
      "animal_id", "bin_stamp", "i_lat", "i_lon",
      "record_type"
    )]
    out <- data.table::rbindlist(list(dtc, det), use.names = TRUE)
    data.table::setkey(out, animal_id, bin_stamp)
    out[, bin_stamp := t_seq[findInterval(bin_stamp, t_seq)]]
    out <- na.omit(out, cols = "i_lat")
    data.table::setnames(out, c(
      "animal_id", "bin_timestamp", "latitude",
      "longitude", "record_type"
    ))
    out <- unique(out)
    out <- data.table::setorder(out, animal_id, bin_timestamp, -record_type)

    # If out_class == NULL, then return data as data.table
    if (is.null(out_class)) {
      out <- as.data.frame(out)
      return(out)
    }

    # if out_class == "tibble", then return tibble object
    if (out_class == "tibble") {
      out <- dplyr::as_tibble(out)
      return(out)
    }

    # if out_class == NULL, then return data.frame object
    return(out)
  }

  # routine for combined nln and ln interpolation

  # identify start and end rows for observations before and after NA
  ends <- dtc[!is.na(deploy_lat), .(start = .I[-nrow(.SD)], end = .I[-1]),
    by = animal_id
  ][end - start > 1]

  # identify observations that are both start and ends
  dups <- c(ends$start, ends$end)[ends[, duplicated(c(start, end))]]

  # create and append duplicate rows for observations
  # that are both start and end.
  # This is so each observation can be in only one group

  # identifies rows and duplicate rows that need duplicated
  dtc[, c("rep", "num") := list(1L, 1:.N)][dups, rep := 2L]
  dtc <- dtc[rep(num, rep)]
  dtc[, rep := NULL]
  dtc[, num := NULL]

  # recalculate first and last rows- no duplicate rows this time...
  new_ends <- dtc[!is.na(deploy_lat), .(start = .I[-nrow(.SD)], end = .I[-1]),
    by = animal_id
  ][end - start > 1]

  # create row index
  dtc[, start_dtc := 1:.N]

  # extract rows that need interpolated
  dtc <- dtc[new_ends, .(
    animal_id = x.animal_id,
    detection_timestamp_utc = x.detection_timestamp_utc,
    deploy_lat = x.deploy_lat, deploy_long = x.deploy_long,
    record_type = x.record_type, num_rows = x.num_rows,
    bin = x.bin, i.start = start
  ),
  on = .(start_dtc >= start, start_dtc <= end)
  ]

  # calculate great circle distance between coords
  # dtc[, gcd := geosphere::distHaversine(as.matrix(
  #  .SD[1, c("deploy_long", "deploy_lat")]),
  #  as.matrix(.SD[.N, c("deploy_long", "deploy_lat")])), by = i.start]

  # remove geosphere dependency and use geodist.  geosphere relies on sp and raster.
  dtc[, gcd := geodist::geodist_vec(
    x1 = .SD[[1, "deploy_long"]],
    y1 = .SD[[1, "deploy_lat"]],
    x2 = .SD[[.N, "deploy_long"]],
    y2 = .SD[[.N, "deploy_lat"]],
    measure = "haversine"
  ),
  by = i.start
  ]

  # calculate least cost (non-linear) distance between points
  message("Calculating least-cost (non-linear) distances... (step 1 of 3)")

  grpn <- data.table::uniqueN(dtc$i.start)

  if (show_progress) pb <- txtProgressBar(min = 0, max = grpn, style = 3)

  dtc[, lcd := {
    if (show_progress) setTxtProgressBar(pb, value = .GRP)
    gdistance::costDistance(trans,
      fromCoords = as.matrix(
        .SD[1, c("deploy_long", "deploy_lat")]
      ),
      toCoords = as.matrix(.SD[.N, c("deploy_long", "deploy_lat")])
    )
  },
  by = i.start
  ]

  # calculate ratio of gcd:lcd
  dtc[, crit := gcd / lcd]

  # create keys for lookup
  dtc[!is.na(detection_timestamp_utc),
    t_lat := data.table::shift(deploy_lat, type = "lead"),
    by = i.start
  ]
  dtc[!is.na(detection_timestamp_utc),
    t_lon := data.table::shift(deploy_long, type = "lead"),
    by = i.start
  ]
  dtc[!is.na(detection_timestamp_utc),
    t_timestamp := data.table::shift(detection_timestamp_utc, type = "lead"),
    by = i.start
  ]

  # extract rows that need non-linear interpolation
  # based on gcd:lcd distance
  nln <- dtc[crit < lnl_thresh]

  land_chk <- dtc[is.infinite(lcd)][
    !is.na(deploy_lat),
    c("deploy_lat", "deploy_long")
  ]

  # stop execution and display offending receivers if any receivers are on land.

  capture <- function(x) paste(capture.output(print(x)), collapse = "\n")

  if (nrow(land_chk) > 0) {
    stop("Some coordinates are on land or beyond extent.
    Interpolation impossible! Check receiver locations or extents of transition
    layer:\n", capture(as.data.table(land_chk)), call. = FALSE)
  }

  # extract data for linear interpolation
  # check to make sure that all points to be interpolated
  # are within the tranition layer is needed before any interpolation.

  ln <- dtc[crit >= lnl_thresh | is.nan(crit)]
  if (nrow(ln) == 0) {
    ln <- data.table::data.table(
      animal_id = character(), i_lat = numeric(),
      i_lon = numeric(),
      bin_stamp = as.POSIXct(character()),
      record_type = character()
    )
  } else {
    message("Starting linear interpolation... (step 2 of 3)")
    # linear interpolation
    grpn <- uniqueN(ln$i.start)
    if (show_progress) pb <- txtProgressBar(min = 0, max = grpn, style = 3)
    ln[, bin_stamp := detection_timestamp_utc][
      is.na(detection_timestamp_utc),
      bin_stamp := bin
    ]
    ln[, i_lat := {
      if (show_progress) setTxtProgressBar(pb, .GRP)
      tmp <- .SD[
        c(1, .N),
        c("detection_timestamp_utc", "deploy_lat")
      ]
      approx(c(tmp$detection_timestamp_utc),
        c(tmp$deploy_lat),
        xout = c(bin_stamp),
        ties = "ordered"
      )$y
    },
    by = i.start
    ]
    ln[, i_lon := {
      tmp <- .SD[
        c(1, .N),
        c("detection_timestamp_utc", "deploy_long")
      ]
      approx(c(tmp$detection_timestamp_utc),
        c(tmp$deploy_long),
        xout = c(bin_stamp),
        ties = "ordered"
      )$y
    },
    by = i.start
    ]
    ln[is.na(deploy_long), record_type := "interpolated"]
  }

  # extract records to lookup
  nln_small <- nln[!is.na(detection_timestamp_utc)][!is.na(t_lat)]

  if (nrow(nln_small) == 0) {
    nln <- data.table(
      animal_id = character(), i_lat = numeric(),
      i_lon = numeric(),
      bin_stamp = as.POSIXct(character()),
      record_type = character()
    )
  } else {
    # nln interpolation
    # create lookup table
    data.table::setkey(nln_small, deploy_lat, deploy_long, t_lat, t_lon)
    lookup <- unique(nln_small[, .(deploy_lat, deploy_long, t_lat, t_lon),
      allow.cartesian = TRUE
    ])

    message("\nStarting non-linear interpolation... (step 3 of 3)")
    grpn <- nrow(lookup)
    if (show_progress) pb <- txtProgressBar(min = 0, max = grpn, style = 3)
    # calculate non-linear interpolation for all unique movements in lookup (old version with sp::coordinates)
    ## lookup[, coord := { if(show_progress) setTxtProgressBar(pb, value = .GRP);
    ##   sp::coordinates(
    ##           gdistance::shortestPath(trans, as.matrix(
    ##           .SD[, c("deploy_long", "deploy_lat")]), as.matrix(
    ##             .SD[, c("t_lon", "t_lat")]), output = "SpatialLines")
    ##   )
    ## },
    ##   by = 1:nrow(lookup)]

    # alternative using sf (needs tested!)
    # st_as_sf(gdistance::shortestPath(trans, as.matrix(lookup[, c("deploy_long", "deploy_lat")]), as.matrix(lookup[, c("t_lon", "t_lat")]), output = "SpatialLines"), crs = 4326)

    lookup[, coord := {
      if (show_progress) setTxtProgressBar(pb, value = .GRP)
      sf::st_as_sf(
        gdistance::shortestPath(trans, as.matrix(
          .SD[, c("deploy_long", "deploy_lat")]
        ), as.matrix(
          .SD[, c("t_lon", "t_lat")]
        ), output = "SpatialLines"),
        crs = 4326
      )
    },
    by = 1:nrow(lookup)
    ]

    message("\nFinalizing results.")

    lookup[, grp := 1:.N]

    # extract interpolated points from coordinate lists...
    res <- lookup[, .(
      nln_longitude = lookup$coord[[.I]][, 1],
      nln_latitude = lookup$coord[[.I]][, 2]
    ), by = grp]

    # set keys, join interpolation and original data
    data.table::setkey(lookup, grp)
    data.table::setkey(res, grp)
    lookup <- lookup[res]
    lookup[, coord := NULL]

    # added first/last rows, number sequence for groups
    lookup[lookup[, .I[1], by = grp]$V1, nln_longitude := deploy_long]
    lookup[lookup[, .I[.N], by = grp]$V1, nln_longitude := t_lon]
    lookup[lookup[, .I[1], by = grp]$V1, nln_latitude := deploy_lat]
    lookup[lookup[, .I[.N], by = grp]$V1, nln_latitude := t_lat]
    lookup[, seq_count := 1:.N, by = grp]

    # lookup interpolated values for original dataset
    data.table::setkey(lookup, deploy_lat, deploy_long, t_lat, t_lon)
    nln_small <- lookup[nln_small, allow.cartesian = TRUE]
    data.table::setkey(nln_small, i.start, seq_count)

    # add timeseries for interpolating nln movements
    nln_small[
      nln_small[, .I[1], by = i.start]$V1,
      i_time := detection_timestamp_utc
    ]
    nln_small[nln_small[, .I[.N], by = i.start]$V1, i_time := t_timestamp]

    # arch <- nln_small
    # nln_small <- nln_small[i.start == 163]

    nln_small[, latitude_lead := data.table::shift(nln_latitude, type = "lag", fill = NA), by = i.start]
    nln_small[, longitude_lead := data.table::shift(nln_longitude, type = "lag", fill = NA), by = i.start]

    # Switch from geosphere to geodist
    # nln_small[, cumdist :=  geosphere::distGeo(
    #    .SD[, c("nln_longitude", "nln_latitude")],
    #    .SD[,c("longitude_lead", "latitude_lead")]), by = i.start]

    nln_small[, cumdist := geodist::geodist_vec(
      x1 = .SD[["nln_longitude"]],
      y1 = .SD[["nln_latitude"]],
      x2 = .SD[["longitude_lead"]],
      y2 = .SD[["latitude_lead"]],
      measure = "geodesic"
    ),
    by = .I
    ]

    nln_small[is.na(cumdist), cumdist := 0]
    nln_small[, cumdist := cumsum(cumdist), by = i.start]

    nln_small[, latitude_lead := NULL][, longitude_lead := NULL]

    # calculate cumdist
    ## nln_small[, cumdist := cumsum(c(0, sqrt(diff(nln_longitude) ^ 2 +
    ##                                         diff(nln_latitude) ^ 2))),
    ##           by = i.start]

    # interpolate missing timestamps for interpolated coordinates
    nln_small[, i_time := as.POSIXct(
      approx(cumdist, i_time,
        xout = cumdist,
        ties = "ordered"
      )$y,
      origin = "1970-01-01 00:00:00",
      tz = attr(nln_small$i_time, "tzone")
    ),
    by = i.start
    ]

    # create timestamp vector to interpolate on.
    nln[, bin_stamp := detection_timestamp_utc]
    nln[is.na(detection_timestamp_utc), bin_stamp := bin]
    nln[, grp := i.start]

    # interpolate timestamps
    data.table::setkey(nln_small, i.start)
    data.table::setkey(nln, i.start)
    nln[, i_lat := {
      tmp <- nln_small[
        .(.SD[1, "i.start"]),
        c("i_time", "nln_latitude")
      ]
      approx(tmp$i_time, tmp$nln_latitude,
        xout = bin_stamp,
        ties = "ordered"
      )$y
    },
    by = grp
    ]

    nln[, i_lon := {
      tmp <- nln_small[
        .(.SD[1, "i.start"]),
        c("i_time", "nln_longitude")
      ]
      approx(tmp$i_time, tmp$nln_longitude,
        xout = bin_stamp,
        ties = "ordered"
      )$y
    },
    by = grp
    ]

    nln[is.na(deploy_long), record_type := "interpolated"]
  }

  # combine into a single data.table
  out <- data.table::rbindlist(list(
    ln[
      record_type == "interpolated",
      c("animal_id", "bin_stamp", "i_lat", "i_lon", "record_type")
    ],
    nln[
      record_type == "interpolated",
      c(
        "animal_id", "bin_stamp", "i_lat", "i_lon",
        "record_type"
      )
    ],
    det[, c(
      "animal_id", "bin_stamp", "i_lat", "i_lon",
      "record_type"
    )]
  ), use.names = TRUE)

  out[, !c("animal_id")]
  data.table::setkey(out, animal_id, bin_stamp)
  out[, bin_stamp := t_seq[findInterval(bin_stamp, t_seq)]]
  data.table::setnames(out, c(
    "animal_id", "bin_timestamp", "latitude",
    "longitude", "record_type"
  ))
  out <- na.omit(out, cols = "latitude")
  out <- unique(out)
  data.table::setorder(out, animal_id, bin_timestamp, -record_type)

  # If out_class == NULL, then return data as data.table
  if (is.null(out_class)) {
    out <- as.data.frame(out)
    return(out)
  }

  # if out_class == "tibble", then return tibble object
  if (out_class == "tibble") {
    out <- dplyr::as_tibble(out)
    return(out)
  }

  # if out_class == NULL, then return data.frame object
  return(out)
}
