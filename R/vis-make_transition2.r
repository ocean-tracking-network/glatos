#' Create transition layer from a spatial polygon
#'
#' Create transition layer for [interpolate_path] from
#' [SpatialPolygonsDataFrame][SpatialPolygons].
#'
#' @param poly A spatial polygon object of class
#' [SpatialPolygonsDataFrame][SpatialPolygons].
#'
#' @param res two element vector that specifies the x and y dimension
#'   of output raster cells.  Units of res are same as input
#'   polygon.
#'
#' @param extent_out An optional `Extent` object
#'   (see [extent][raster::extent]) that determines the extent of the
#'   output objects. Output extent will default to extent of input object
#'   `poly` if `extent_out`, and `x_lim`/`y_lim`
#'   are NULL (default).
#'
#' @param x_lim An optional two-element vector with extents of x axis.
#'
#' @param y_lim An optional two-element vector with extents of x axis.
#'
#' @details `make_transition` uses [rasterize][raster::rasterize] to convert a
#'   [SpatialPolygonsDataFrame][SpatialPolygons] into a raster layer, and
#'   geo-corrected transition layer [transition][gdistance::transition].  Raster cell
#'   values on land = 0 and water = 1.
#'
#' @details output transition layer is corrected for projection
#'   distortions using [geoCorrection][gdistance::geoCorrection].  Adjacent
#'   cells are connected by 16 directions and transition function
#'   returns 0 (land) for movements between land and water and 1 for
#'   all over-water movements.
#'
#' @return A list with two elements:
#' \describe{
#'    \item{transition}{a geo-corrected transition raster layer where land = 0
#'       and water=1
#'   (see `gdistance`)}
#'    \item{rast}{rasterized input layer of class `raster`}}
#'
#' @seealso [make_transition]
#'
#' @author Todd Hayden, Tom Binder, Chris Holbrook
#'
#' @examples
#'
#' library(sp) #for loading greatLakesPoly
#' library(raster) # for plotting rasters
#'
#' # get polygon of the Great Lakes
#' data(greatLakesPoly) #glatos example data; a SpatialPolygonsDataFrame
#'
#' # make_transition layer
#' tst <- make_transition2(greatLakesPoly, res = c(0.1, 0.1))
#'
#' # plot raster layer
#' # notice land = 1, water = 0
#' plot(tst$rast)
#'
#' # plot transition layer
#' plot(raster(tst$transition))
#' 
#' \dontrun{
#' # increase resolution- this may take some time...
#' tst1 <- make_transition2(greatLakesPoly, res = c(0.01, 0.01))
#'
#' # plot raster layer
#' plot(tst1$rast)
#'
#' # plot transition layer
#' plot(raster(tst1$transition))
#' }
#'
#' @export

make_transition2 <- function(poly, res = c(0.1, 0.1), extent_out = NULL,
                             x_lim = NULL, y_lim = NULL){
  
  message("Making transition layer...")
  
  if(sum(is.null(x_lim), is.null(y_lim)) == 1) stop(paste0("You must specify ",
    "'x_lim' and 'y_lim' or neither."))
  if(!is.null(x_lim) & length(x_lim) != 2) stop("'x_lim' must be a vector ",
    "with exactly two elements.")
  if(!is.null(y_lim) & length(y_lim) != 2) stop("'y_lim' must be a vector ",
    "with exactly two elements.")

  if(is.null(x_lim) & is.null(extent_out)){ 
    extent_out <- raster::extent(poly) 
    } else if (!is.null(x_lim)) { 
      extent_out <- raster::extent(c(x_lim[1], x_lim[2],
                                      y_lim[1], y_lim[2]))
  }
    
  burned = raster::rasterize(poly, y = raster::raster(res = res, ext = extent_out), 
    field = 1, background = 0)
  
  tran <- function(x) if(x[1] * x[2] == 0){ return(0) } else { return(1) }
  tr1 <- gdistance::transition(burned, transitionFunction = tran, 
    directions = 16)
  tr1 <- gdistance::geoCorrection(tr1, type="c")
  
  message("Done.")
  return(list(transition = tr1, rast = burned))
  }
    
