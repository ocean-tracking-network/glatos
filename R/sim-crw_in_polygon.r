#' Simulate a correlated random walk inside a polygon
#'
#' Uses [crw()] to simulate a random walk as series of equal-length steps
#' with turning angles drawn from a normal distribution inside a polygon.
#'
#' @param polyg A spatial polygon object of class [sf::sf()] or
#'   [sf::sfc()] containing `POLYGON` or `MULTIPOLYGON`
#'   features (but `SpatialPolygonsDataFrame` and `SpatialPolygons`
#'   are also accepted); \cr *OR* \cr A polygon defined as data frame or
#'   matrix with numeric columns x and y.
#'
#' @param theta A 2-element numeric vector with turn angle parameters (theta[1]
#'   = mean; theta[2] = sd), in degrees, from normal distribution.
#'
#' @param stepLen A numeric scalar with total distance moved in each step, in
#'   meters.
#'
#' @param initPos A 2-element numeric vector with initial position
#'   (initPos[1]=x, initPos[2]=y) in same coordinate reference system as
#'   `polyg`.
#'
#' @param initHeading A numeric scalar with initial heading in degrees. E.g., 0
#'   = North; 90 = East, 180 = South, 270 = West; etc.
#'
#' @param nsteps A numeric scalar with number of steps to simulate.
#'
#' @param inputCRS A `crs` object or numeric EPSG code of coordinate system
#'   of input `polyg`. Only used if `polyg` does not contain a
#'   `crs`. If missing, then `polyg` is assumed in an arbitrary Cartesian
#'   (projected) system with base unit of one meter.
#'
#' @param cartesianCRS Coordinate reference system used for simulations. Must be
#'   a Cartesian (projected) coordinate system. Must be given when input CRS
#'   is non-Cartesian (e.g., long-lat); optional otherwise. See Note.
#'
#' @param sp_out Logical. If TRUE (default) then output is an `sf` object.
#'   If FALSE, then output is a `data.frame`.
#'
#' @param show_progress Logical. Progress bar and status messages will be shown
#'   if TRUE (default) and not shown if FALSE.
#'
#' @details If initPos = NA, then a starting point is randomly selected within
#'   the polygon boundary. A path is simulated forward using [crw()].
#'   Initial heading is also randomly selected if `initHeading = NA`. When
#'   a step crosses the polygon boundary, a new heading for that step is drawn
#'   and the turn angle standard deviation is enlarged slightly for each
#'   subsequent point that lands outside the polygon.
#'
#' @details If input `polyg` object is a data.frame with x and y columns and
#'   `sp_out = TRUE`, then output object
#'   coordinate system is defined by `inputCRS`. Coordinate system on output
#'   will be same as input if `polyg` contains a valid CRS.
#'
#'
#' @return When `sp_out = TRUE`, an `sf` object containing one
#'   `POINT` feature for each vertex in the simulated path. \cr *OR*
#'   \cr When `sp_out = FALSE`, a two-column data frame containing:
#'   \item{x}{x coordinates} \item{y}{y coordinates} in the same units as
#'   `polyg`.
#'
#' @author C. Holbrook \email{cholbrook@@usgs.gov}
#'
#' @seealso [crw], [transmit_along_path], [detect_transmissions]
#'
#' @note The path is constructed in segments based on the minimum distance
#'   between the previous point and the closest polygon boundary.
#'
#'   Simulations are conducted within the coordinate system specified by
#'   argument `cartesianCRS`.
#'
#' @note  EPSG 3175 (`cartesianCRS = 3175`) is recommended projected
#'   coordinate system for the North American Great Lakes Basin and St. Lawrence
#'   River system.
#'   <https://spatialreference.org/ref/epsg/nad83-great-lakes-and-st-lawrence-albers/>.
#'
#' @examples
#'
#' # Example 1 - data.frame input
#' mypolygon <- data.frame(x = c(-50, -50, 50, 50), y = c(-50, 50, 50, -50))
#'
#' path_df <- crw_in_polygon(mypolygon,
#'   theta = c(0, 20), stepLen = 10,
#'   initPos = c(0, 0), initHeading = 0, nsteps = 50, sp_out = FALSE
#' )
#'
#' class(path_df) # note object is data.frame
#'
#' plot(path_df,
#'   type = "o", pch = 20, asp = c(1, 1),
#'   xlim = range(mypolygon$x), ylim = range(mypolygon$y)
#' )
#'
#' polygon(mypolygon, border = "red")
#'
#'
#' # Example 2 - data.frame input; input CRS specified
#' mypolygon <- data.frame(
#'   x = c(-84, -85, -85, -84),
#'   y = c(45, 44, 45, 45)
#' )
#' path_df <- crw_in_polygon(mypolygon,
#'   theta = c(0, 20),
#'   stepLen = 1000,
#'   initPos = c(-84.75, 44.75),
#'   initHeading = 0,
#'   nsteps = 50,
#'   inputCRS = 4326,
#'   cartesianCRS = 3175,
#'   sp_out = FALSE
#' )
#' plot(path_df,
#'   type = "o", pch = 20, asp = c(1, 1),
#'   xlim = range(mypolygon$x), ylim = range(mypolygon$y)
#' )
#' class(path_df) # note object is data.frame
#' polygon(mypolygon, border = "red")
#'
#'
#' # Example 3 - sf POLYGON input
#' data(great_lakes_polygon)
#'
#' # simulate in great lakes polygon
#' path_sf <- crw_in_polygon(great_lakes_polygon,
#'   theta = c(0, 25),
#'   stepLen = 10000,
#'   initHeading = 0,
#'   nsteps = 100,
#'   cartesianCRS = 3175
#' )
#'
#' # plot
#' plot(sf::st_geometry(great_lakes_polygon),
#'   col = "lightgrey",
#'   border = "grey"
#' )
#' points(sf::st_coordinates(path_sf), type = "o", pch = 20, col = "red")
#'
#' # zoom in
#' plot(sf::st_geometry(great_lakes_polygon),
#'   col = "lightgrey",
#'   xlim = sf::st_bbox(path_sf)[c("xmin", "xmax")],
#'   ylim = sf::st_bbox(path_sf)[c("ymin", "ymax")]
#' )
#' points(sf::st_coordinates(path_sf), type = "o", pch = 20, col = "red")
#'
#'
#' # Example 4 - SpatialPolygonsDataFrame input
#' data(greatLakesPoly)
#'
#' # simulate in great lakes polygon
#' path_sp <- crw_in_polygon(greatLakesPoly,
#'   theta = c(0, 25),
#'   stepLen = 10000,
#'   initHeading = 0,
#'   nsteps = 100,
#'   cartesianCRS = 3175,
#'   sp_out = TRUE
#' )
#'
#' # plot
#' plot(sf::st_as_sfc(greatLakesPoly), col = "lightgrey", border = "grey")
#' points(sf::st_coordinates(sf::st_as_sf(path_sp)),
#'   type = "o", pch = 20,
#'   col = "red"
#' )
#'
#' # zoom in
#' plot(sf::st_as_sfc(greatLakesPoly),
#'   col = "lightgrey", border = "grey",
#'   xlim = sf::st_bbox(path_sp)[c("xmin", "xmax")],
#'   ylim = sf::st_bbox(path_sp)[c("ymin", "ymax")]
#' )
#' points(sf::st_coordinates(sf::st_as_sf(path_sp)),
#'   type = "o", pch = 20,
#'   col = "red"
#' )
#'
#' @export

crw_in_polygon <- function(polyg, theta = c(0, 10), stepLen = 100,
                           initPos = c(NA, NA), initHeading = NA, nsteps = 30,
                           inputCRS = NA, cartesianCRS = NA, sp_out = TRUE,
                           show_progress = TRUE) {
  # Check input class
  if (!inherits(polyg, c(
    "data.frame", "sf", "sfc", "SpatialPolygonsDataFrame",
    "SpatialPolygons"
  ))) {
    stop(
      "Input 'polyg' must be of class 'data.frame', 'sf', 'sfc', ",
      "'SpatialPolygonsDataFrame', or 'SpatialPolygons'."
    )
  }

  # Get input CRS and use CRS arg if missing
  crs_in <- sf::st_crs(polyg)

  if (is.na(crs_in)) crs_in <- sf::st_crs(inputCRS)

  # Get or set Cartesian CRS
  crs_cartesian <- sf::st_crs(cartesianCRS)

  # Set crs_cartesian = crs_input if Cartesian and cartesianCRS missing
  if (is.na(crs_cartesian) & isTRUE(crs_in$IsGeographic)) crs_cartesian <- crs_in


  # Check for Cartesian CRS
  if (isTRUE(crs_in$IsGeographic) & is.na(cartesianCRS)) {
    stop(
      "Coordinate ",
      "reference system of input 'polyg' must be Cartesian ",
      "(projected) \nor 'cartesianCRS' must be specified."
    )
  }


  if (isTRUE(crs_cartesian$IsGeographic)) {
    stop(
      "Coordinate reference system ",
      "specified by 'cartesianCRS' is not ",
      "Cartesian/projected."
    )
  }


  # Check that sf geometry is POLYGON
  if (inherits(polyg, c("sf", "sfc"))) {
    if (!any(c("MULTIPOLYGON", "POLYGON") %in% sf::st_geometry_type(polyg))) {
      stop(
        "Input object 'polyg' must contain geometry of type 'POLYGON' when ",
        "class is 'sf' or 'sfc'."
      )
    }
    polyg_sf <- polyg
  } else if (inherits(polyg, c("data.frame", "matrix"))) {
    # Check names
    if (!all(c("x", "y") %in% colnames(polyg))) {
      stop(
        "Input 'polyg' must have ",
        "columns named 'x' and 'y'."
      )
    }

    # Close polyg if needed (first and last point must be same)
    if (!identical(polyg[1, ], utils::tail(polyg, 1))) polyg <- rbind(polyg, polyg[1, ])

    # Make sf object
    polyg_sf <- sf::st_polygon(list(as.matrix(polyg[c("x", "y")])))
    polyg_sf <- sf::st_sfc(polyg_sf, crs = sf::st_crs(crs_in))
    polyg_sf <- sf::st_sf(ID = 1:length(polyg_sf), geom = polyg_sf)
  }


  # Convert to sf_polygon if SpatialPolygonsDataFrame or SpatialPolygons
  if (inherits(polyg, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
    polyg_sf <- sf::st_as_sf(polyg)
  }

  # Set or change CRS for calculations
  if (!is.na(crs_cartesian)) {
    polyg_sf <- sf::st_transform(polyg_sf, crs = crs_cartesian)
  }

  # If any initPos were not given
  # randomly select one point in the study area
  if (any(is.na(initPos))) init <- sf::st_sample(polyg_sf, size = 1)


  # If initPos are both given, check to see if in polyg
  if (all(!is.na(initPos))) {
    init <- sf::st_as_sf(
      data.frame(
        x = initPos[1],
        y = initPos[2]
      ),
      coords = c("x", "y"),
      crs = crs_in
    )
    if (!is.na(crs_in)) {
      init <- sf::st_transform(init,
        crs = sf::st_crs(crs_cartesian)
      )
    }
    inPoly <- any(sf::st_contains(polyg_sf, init, sparse = FALSE))
    if (!inPoly) stop("initPos is outside polygon boundary.")
  } # end if


  # randomly select heading if not given
  if (is.na(initHeading)) initHeading <- runif(1, 0, 360)

  # preallocate
  path_fwd <- data.frame(x = rep(NA, nsteps + 1), y = NA)
  path_fwd[1, ] <- sf::st_coordinates(init)

  # create lines object from polyg (for distance measurement)
  xl <- sf::st_cast(polyg_sf, "MULTILINESTRING")

  rows_i <- 1
  init_i <- init
  dist_i <- min(sf::st_distance(init_i, xl, sparse = TRUE), na.rm = TRUE)
  nsteps_i <- (as.numeric(dist_i) %/% stepLen) + 1
  rows_i <- 1 + 1:nsteps_i
  rows_i <- rows_i[rows_i <= (nsteps + 1)]

  # initalize counter for boundary failures
  k <- 0

  # initialize progress bar
  if (show_progress) {
    message("Simulating tracks...")
    pb <- txtProgressBar(min = 0, max = nsteps + 2, initial = 0, style = 3)
  }

  while (length(rows_i) > 0) {
    # calculate theta based on k (failed boundary attempts)
    theta_i <- c(theta[1], theta[2] * (1 + 0.1 * k^2))

    # operate on temporary object for ith window
    path_fwd_i <- crw(
      theta = theta_i, stepLen = stepLen,
      initPos = as.vector(sf::st_coordinates(init_i)),
      initHeading, nsteps = length(rows_i)
    )

    # replace check_in_polygon with check_cross_boundary
    #  otherwise, paths can jump peninsulas etc
    # inPoly <- check_in_polygon(path_fwd_i, polyg_sf,
    #                            EPSG = crs_cartesian)

    # combine init and path_fwd_i
    path_mat <- rbind(
      unname(sf::st_coordinates(init_i)),
      unname(as.matrix(path_fwd_i))
    )

    inPoly <- check_cross_boundary(
      path = path_mat,
      boundary = xl,
      EPSG = crs_cartesian
    )

    if (all(!inPoly)) {
      k <- k + 1 # counter
      next # repeat this iteration if all outside polygon
    }
    if (any(!inPoly)) rows_i <- rows_i[inPoly] # retain only rows inside

    k <- 0

    # update path.fwd
    path_fwd[rows_i, ] <- path_fwd_i[inPoly, ]


    # simulate track forward
    init_i <- sf::st_as_sf(path_fwd[max(rows_i), ],
      coords = c("x", "y"),
      crs = crs_cartesian
    )

    # smallest distance to boundary
    dist_i <- min(sf::st_distance(init_i, xl, sparse = TRUE), na.rm = TRUE)

    # calculate heading at end (start of next)
    initHeading <- vector_heading(
      path_fwd$x[max(rows_i) - 1:0],
      path_fwd$y[max(rows_i) - 1:0]
    )


    # update progress bar
    if (show_progress) {
      setTxtProgressBar(pb, max(rows_i))
      if (max(rows_i) > (nsteps + 1)) close(pb)
    }

    # conservative estimate of the number of rows/steps to simulate
    # i.e., without reaching barrier
    nsteps_i <- (as.numeric(dist_i) %/% stepLen) + 1
    rows_i <- max(rows_i) + 1:nsteps_i
    rows_i <- rows_i[rows_i < nsteps + 2]
  } # end while

  if (show_progress) message("Done.")

  # Set output CRS
  if (!is.na(crs_in)) {
    crs_out <- crs_in
  } else {
    crs_out <- sf::st_crs(NA)
  }

  # Coerce to sf
  path_fwd_sf <- sf::st_as_sf(path_fwd,
    coords = c("x", "y"),
    crs = crs_cartesian
  )

  if (!is.na(crs_cartesian)) {
    path_fwd_sf <- sf::st_transform(
      path_fwd_sf,
      crs = crs_out
    )
  }

  if (sp_out) {
    return(path_fwd_sf)
  }

  path_fwd_df <- as.data.frame(sf::st_coordinates(path_fwd_sf))[, c("X", "Y")]
  names(path_fwd_df) <- c("x", "y")
  return(path_fwd_df)
}

#' Check if in polygon
check_in_polygon <- function(points, polygon, EPSG) {
  points_sf <- sf::st_as_sf(points,
    coords = c("x", "y"),
    crs = EPSG
  )
  # identify points contains in any polygon
  inPoly <- apply(sf::st_contains(polygon, points_sf, sparse = FALSE), 2, any)
  return(inPoly)
}


#' Check if track crosses polygon boundary
check_cross_boundary <- function(path, boundary, EPSG) {
  # Make line segment objects of sequential point-pairs in path

  segs_mat <- cbind(
    utils::head(path, -1),
    utils::tail(path, -1)
  )

  in_poly <-
    unname(
      apply(segs_mat, 1,
        function(x) {
          !any(sf::st_intersects(boundary,
            sf::st_linestring(rbind(x[1:2], x[3:4])),
            sparse = FALSE
          ))
        },
        simplify = TRUE
      )
    )


  # segs_list <- apply(segs_mat, 1,
  #                function(x) sf::st_linestring(rbind(x[1:2], x[3:4])),
  #                simplify = FALSE)
  #
  # segs_sfc <- do.call(sf::st_sfc, segs_list)
  # sf::st_crs(segs_sfc) <- EPSG
  #
  # segs_sf <- sf::st_as_sf(segs_sfc)
  #
  # #identify line segments that cross polygon boundary
  # in_poly <- !apply(sf::st_crosses(boundary, segs_sf, sparse = FALSE), 2, any)
  # #in_poly <- !colSums(sf::st_crosses(boundary, segs_sf, sparse = FALSE))
  #
  return(in_poly)
}
