#' Create transition layer from a spatial polygon
#'
#' Create transition layer for \link{interpolate_path} from
#' \link[=SpatialPolygons]{SpatialPolygonsDataFrame}.
#'
#' @param poly A spatial polygon object of class
#'   \link[=SpatialPolygons]{SpatialPolygonsDataFrame} or a
#'   \link[sf:sf]{sf::sf()} object with a geometry column of polygon and/or
#'   multipolygon objects.
#'
#' @param res two element vector that specifies the x and y dimension
#'   of output raster cells.  Units of res are same as input
#'   polygon.
#'
#' @param x_lim An optional two-element vector with extents of x axis.
#'     Default returns object with extent of input object.
#'
#' @param y_lim An optional two-element vector with extents of x axis.
#'     Default returns object with extent of input objects
#' 
#' @details \code{make_transition2} uses \link[fasterize]{fasterize} to
#'   convert a \link[=SpatialPolygons]{SpatialPolygonsDataFrame} or a
#'   \link[sf:sf]{sf::sf()} object with a geometry column of polygon and/or
#'   multipolygon into
#'   a raster layer, and geo-corrected transition layer
#'   \link[gdistance]{transition}.  Raster cell values on land = 0 and
#'   water = 1.
#'
#' @details output transition layer is corrected for projection
#'   distortions using \link[gdistance]{geoCorrection}.  Adjacent
#'   cells are connected by 16 directions and transition function
#'   returns 0 (land) for movements between land and water and 1 for
#'   all over-water movements.
#' 
#' @return A list with two elements:
#' \describe{
#'    \item{transition}{a geo-corrected transition raster layer where land = 0
#'       and water=1
#'   (see \code{gdistance})}
#'    \item{rast}{rasterized input layer of class \code{raster}}}
#'
#' @seealso \link{make_transition}
#'
#' @author Todd Hayden, Tom Binder, Chris Holbrook
#'
#' @examples
#'
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

make_transition2 <- function (poly, res = c(0.1, 0.1), x_lim = NULL,
                              y_lim = NULL){
    message("Making transition layer...")
    if (inherits(poly,  c("SpatialPolygonsDataFrame", "SpatialPolygons", "sf")) == FALSE) {
        stop(paste0("Supplied object for 'poly' argument is not class ",
                    "SpatialPolygonsDataFrame, SpatialPolygons or a sf polygon object"), call. = FALSE)}        
    if (sum(is.null(x_lim), is.null(y_lim)) == 1) 
        stop(paste0("You must specify ", "'x_lim' and 'y_lim' or neither."))
    if (!is.null(x_lim) & length(x_lim) != 2) 
        stop("'x_lim' must be a vector ", "with exactly two elements.")
    if (!is.null(y_lim) & length(y_lim) != 2) 
        stop("'y_lim' must be a vector ", "with exactly two elements.")
    if (is.null(x_lim)) {
        extent_out <- raster::extent(poly)
    }
    else if (!is.null(x_lim)) {
        extent_out <- raster::extent(c(x_lim[1], x_lim[2], y_lim[1], 
                                       y_lim[2]))
    }

    if (inherits(poly, c("SpatialPolygonsDataFrame", "SpatialPolygons")) == TRUE){
        poly <- sf::st_as_sf(poly)
    }
    
    if (sf::st_crs(poly) == sf::NA_crs_){
        warning(paste0("poly object does not have a coordinate reference system defined!"))
    }
    burned = fasterize::fasterize(sf = poly,
                                  raster = raster::raster(res = res,
                                                          ext = extent_out),
                                  field = NULL, fun = "last", background = 0,
                                  by = NULL)

    tran <- function(x) if (x[1] * x[2] == 0) {
                            return(0)
                        }
                        else {
                            return(1)
                        }
    tr1 <- gdistance::transition(burned, transitionFunction = tran,
                                 directions = 16)
    tr1 <- gdistance::geoCorrection(tr1, type = "c")
    message("Done.")
    return(list(transition = tr1, rast = burned))
}
