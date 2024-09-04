#' Make a KML or KMZ file of receiver and animal release locations
#'
#' Convert standard GLATOS receiver location and animal release data to a
#' KML (or optionally KMZ) file (e.g., for viewing in Google Earth).
#' (NOTE: EARLY DEVELOPMENT VERSION).
#'
#' @param wb A `glatos_workbook` object created by
#' [read_glatos_workbook].
#'
#' @param wb_file A character string with path and name of workbook in standard
#'   GLATOS format (*.xlsm). If only file name is given, then the file must be
#'   located in the working directory. File must be a standard GLATOS file
#'   (e.g., *xxxxx_GLATOS_YYYYMMDD.xlsm*) submitted via GLATOSWeb Data
#'   Portal <http://glatos.glos.us>.
#'
#' @param receiver_locs **not yet implemented**
#'
#' @param animals **not yet implemented**
#'
#' @param kmz logical; If TRUE, a KMZ file (zipped KML file) will
#'   be created. Default value is FALSE.
#'
#' @param show_ongoing_recs Indicates if ongoing stations
#'   (missing recovery timestamp) should be included in result.
#'
#' @param end_date End date (e.g. "YYYY-MM-DD") to be used for any ongoing
#'   stations (if showOngoing == T). Defaults to current system time.
#'
#' @param out_file File name (path optional) of output file. If path not
#'  specified then file will be written to working directory. Extension
#'  is not checked against `kmz`. Required if `wb_file` is NULL.
#'  If not specified and `wb_file` is given, then file will be written
#'  to file with name matching `wb_file`.
#'
#' @param wb_version An optional character string with the workbook version
#'   number. Passed to [read_glatos_workbook] when input is
#'   `wb_file`.
#'
#' @param ... optional arguments that influence kml/kmz features. Curently
#' only two options:
#' \describe{
#'   \item{`labelSize`}{A numeric scalar with the size of placemark labels
#'     (only shown when placemark is highlighted by user).}
#'   \item{`iconSize`}{A numeric scalar with the size of placemark icons.}
#' }
#'
#' @details
#' Receiver locations will be visible between deployment and recovery
#' timestamps at each location. Release locations will be displayed when the
#' display window includes the date of release.
#'
#' @return A KML (and optionally, KMZ) file, written to the directory that
#'   contains the input GLATOS workbook, or `out_file` otherwise.
#'   Path to output file is returned.
#'
#' @author C. Holbrook \email{cholbrook@usgs.gov}
#'
#' @examples
#' \dontrun{
#' # get path to example GLATOS Data Workbook
#' wb_file <- system.file("extdata",
#'   "walleye_workbook.xlsm",
#'   package = "glatos"
#' )
#'
#' # read workbook directly
#' kml_workbook(wb_file = wb_file)
#'
#' # now with bigger label and point and out_file
#' kml_workbook(
#'   wb_file = wb_file, labelSize = 20, iconSize = 1,
#'   out_file = "bigger.kml"
#' )
#'
#' # read workbook directly; output kmz
#' kml_workbook(wb_file = wb_file, kmz = TRUE)
#'
#' # get path to example GLATOS Data Workbook
#' wb <- read_glatos_workbook(wb_file)
#' kml_workbook(wb = wb, kmz = TRUE, out_file = "bigger.kmz")
#' }
#'
#' @export
kml_workbook <- function(
    wb = NULL, 
    wb_file = NULL, 
    receiver_locs = NULL,
    animals = NULL, 
    kmz = FALSE, 
    show_ongoing_recs = TRUE, 
    end_date = NULL,
    out_file = NULL, 
    wb_version = NULL, 
    ...) {
  
  # check for features not yet supported
  if (!is.null(animals)) stop("use of 'animals' input not yet supported.")
  if (!is.null(receiver_locs)) stop("use of 'receiver_locs' input not yet supported.")

  # check for correct handling of wb and wb_file
  if (!is.null(wb) & !is.null(wb_file)) {
    stop(paste0(
      "You cannot specify both ",
      "'wb' and 'wb_file'. Specify only one."
    ))
  }

  # check for correct handling of workbook data and animals and receivers
  if ((!is.null(wb) | !is.null(wb_file)) &
    (!is.null(animals) & !is.null(receiver_locs))) {
    stop(paste0(
      "You cannot ",
      "specify 'wb' or 'wb_file' if both 'animals' and 'receiver_locs' \n  ",
      "are specified."
    ))
  }

  # check for outfile is input is wb
  if (is.null(wb_file) & is.null(out_file)) {
    stop(paste0(
      "'out_file' must be ",
      "specified when 'wb_file = NULL.'"
    ))
  }

  ##  Declare global variables for NSE & R CMD check
  Folder <- NULL

  # set default and get optional kml arguments
  kml_args <- list(labelSize = 0.6, iconSize = 0.6)
  args_in <- if(interactive()) list() else list(...)
  if (length(args_in) > 0) kml_args[names(args_in)] <- args_in

  # get data from workbook
  if (!is.null(wb_file)) {
    # if workbook is given, check if object or file path/name
    if (file.exists(wb_file)) {
      wb <- glatos::read_glatos_workbook(wb_file, wb_version = wb_version)
    } else {
      stop("Input file '", wb_file, "' does not exist or cannot be accessed.")
    }
  }


  if (!is.null(wb) | !is.null(wb_file)) {
    rec_loc <- wb$receivers
    anim <- wb$animals
  }
  # get data from receiver_locs if given
  if (!is.null(receiver_locs)) rec_loc <- receiver_locs

  # get data from animals if given
  if (!is.null(animals)) anim <- animals

  # check for receiver_locs and animals
  if (!exists("anim")) {
    stop(paste0(
      "This function requires animal data. ",
      "Ensure that animal data exist \n  in an input workbook('wb' or 'wb_file')",
      " or separate input 'animals'."
    ))
  }

  if (!exists("rec_loc")) {
    stop(paste0(
      "This function requires receiver data. ",
      "Ensure that receiver data exist \n  in an input workbook('wb' or 'wb_file')",
      " or separate input 'receiver_locs'."
    ))
  }

  # remove recovery timestamps if show_ongoing_recs = FALSE
  missing_recov <- is.na(rec_loc$recover_date_time)
  if (show_ongoing_recs == FALSE) {
    # set end timestamp to current time if absent
    if (is.null(end_date)) {
      end_date <- Sys.time()
    } else {
      end_date <- as.POSIXct(end_date, tz = "UTC")
    }
    rec_loc <- rec_loc[!missing_recov, ] # omit receivers with no recoveries
  } else {
    rec_loc$recover_date_date[missing_recov] <- end_date
  }

  rec_pos <- data.frame(
    Folder = "Receivers",
    Name = with(rec_loc, paste0(
      glatos_project, "-", glatos_array, "-",
      station_no, " (", water_body, ")"
    )),
    TimeSpan_start = paste0(
      gsub(" ", "T", rec_loc$deploy_date_time),
      "-00:00"
    ),
    TimeSpan_end = paste0(
      gsub(" ", "T", rec_loc$recover_date_time),
      "-00:00"
    ),
    Longitude = rec_loc$deploy_long,
    Latitude = rec_loc$deploy_lat,
    stringsAsFactors = FALSE
  )

  rec_pos$Altitude <- 0
  rec_pos$Description <- ""


  # Fish releases

  # check for missing release_group
  missing_relgrp <- is.na(anim$release_group)
  if (any(missing_relgrp)) {
    warning(paste0(
      "Some or all values in column '",
      "release_group' are missing values and have been assigned release date ",
      "instead."
    ), call. = FALSE)

    anim$release_group <- format(as.Date(anim$utc_release_date_time))
  }

  # make table of counts
  rel_loc <- as.data.frame(table(anim$release_group))
  names(rel_loc)[1] <- "release_group"
  rel_loc <- merge(rel_loc,
    unique(anim[, c(
      "release_group", "release_location", "release_latitude",
      "release_longitude", "utc_release_date_time"
    )]),
    by = "release_group"
  )

  rel_pos <- data.frame(
    Folder = "Animal releases",
    Name = with(rel_loc, paste0(release_location, " (", release_group, ")")),
    TimeSpan_start = paste0(
      gsub(" ", "T", rel_loc$utc_release_date_time),
      "-00:00"
    ),
    TimeSpan_end = paste0(
      gsub(" ", "T", rel_loc$glatos_release_date_time),
      "-00:00"
    ),
    Longitude = rel_loc$release_longitude,
    Latitude = rel_loc$release_latitude,
    stringsAsFactors = FALSE
  )

  rel_pos$Altitude <- 0
  rel_pos$Description <- ""


  # make KML

  #-kml-specific values
  if (!is.null(wb_file)) {
    kmlName <- gsub(".xlsm$|.xlsx$", ".kml", basename(wb_file))
  } else if (!is.null(out_file)) {
    kmlName <- basename(out_file)
  }



  kmlHead <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">',
    ",<Document>",
    paste0("<name>", kmlName, ".kml</name>"),
    '<StyleMap id="msn_placemark_circle">',
    "<Pair>",
    "<key>normal</key>",
    "<styleUrl>#sn_placemark_circle</styleUrl>",
    "</Pair>",
    "<Pair>",
    "<key>highlight</key>",
    "<styleUrl>#sh_placemark_circle_highlight</styleUrl>",
    "</Pair>",
    "</StyleMap>",
    ',<Style id="sh_placemark_circle_highlight">',
    "<IconStyle>",
    "<color>ff0000ff</color>",
    "<scale>0.709091</scale>",
    "<Icon>",
    "<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle_highlight.png</href>",
    "</Icon>",
    "</IconStyle>",
    "<LabelStyle>",
    paste0("<scale>", kml_args$labelSize, "</scale>"),
    "</LabelStyle>",
    "<ListStyle>",
    "</ListStyle>",
    "</Style>",
    '<Style id="sn_placemark_circle">',
    "<IconStyle>",
    "<color>ff0000ff</color>",
    paste0("<scale>", kml_args$iconSize, "</scale>"),
    "<Icon>",
    "<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>",
    "</Icon>",
    "</IconStyle>",
    "<LabelStyle>",
    "<scale>0</scale>",
    "</LabelStyle>",
    "<ListStyle>",
    "</ListStyle>",
    "</Style>",
    '<StyleMap id="msn_placemark_circle_rel">',
    "<Pair>",
    "<key>normal</key>",
    "<styleUrl>#sn_placemark_circle_rel</styleUrl>",
    "</Pair>",
    "<Pair>",
    "<key>highlight</key>",
    "<styleUrl>#sh_placemark_circle_highlight_rel</styleUrl>",
    "</Pair>",
    "</StyleMap>",
    ',<Style id="sh_placemark_circle_highlight_rel">',
    "<IconStyle>",
    "<color>ff00ffff</color>",
    "<scale>0.709091</scale>",
    "<Icon>",
    "<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle_highlight.png</href>",
    "</Icon>",
    "</IconStyle>",
    "<LabelStyle>",
    paste0("<scale>", kml_args$labelSize, "</scale>"),
    "</LabelStyle>",
    "<ListStyle>",
    "</ListStyle>",
    "</Style>",
    '<Style id="sn_placemark_circle_rel">',
    "<IconStyle>",
    "<color>ff00ffff</color>",
    paste0("<scale>", kml_args$iconSize, "</scale>"),
    "<Icon>",
    "<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>",
    "</Icon>",
    "</IconStyle>",
    "<LabelStyle>",
    "<scale>0</scale>",
    "</LabelStyle>",
    "<ListStyle>",
    "</ListStyle>",
    "</Style>"
  )

  # two style maps; one for receivers, second for releases
  stylemaps <- c("msn_placemark_circle", "msn_placemark_circle_rel")

  makeKMLBody <- function(myFolderName, stylemap, myPoints) {
    folderHead <- c(
      "<Folder>",
      paste0("<name>", myFolderName, "</name>")
    )

    for (i in 1:nrow(myPoints)) {
      if (i == 1) pmBody <- vector() # empty vector

      pmBody <- c(
        pmBody,
        "<Placemark>",
        paste0("<name>", myPoints$Name[i], "</name>"),
        paste0("<styleUrl>#", stylemap, "</styleUrl>"),
        "<Point>",
        with(myPoints, paste("<coordinates>", Longitude[i], ",", Latitude[i], ",0</coordinates>", sep = "")),
        "</Point>",
        "<TimeSpan>",
        paste0("<begin>", myPoints$TimeSpan_start[i], "</begin>"),
        paste0("<end>", myPoints$TimeSpan_end[i], "</end>"),
        "</TimeSpan>",
        "</Placemark>"
      )
    }

    folderBody <- c(folderHead, pmBody, "</Folder>")
    return(folderBody)
  }

  # identify number of unique folders
  folders <- sort(unique(rec_pos$Folder))

  for (i in 1:length(folders)) {
    if (i == 1) folderBody <- vector()

    folderBody <- c(folderBody, makeKMLBody(
      folders[i], stylemaps[1],
      subset(rec_pos, Folder == folders[i])
    ))
  }

  # add releases
  folderBody <- c(folderBody, makeKMLBody(
    rel_pos$Folder[i], stylemaps[2],
    rel_pos
  ))

  kmlFoot <- c("</Document>", "</kml>")

  kmlOut <- c(kmlHead, folderBody, kmlFoot)

  kmlFullName <- ifelse(!is.null(out_file), file.path(
    gsub("\\.", getwd(), dirname(out_file)), basename(out_file)
  ),
  file.path(dirname(wb_file), kmlName)
  )

  # change ext to kml if kmz
  if(kmz & grepl("\\.kmz$", kmlFullName, ignore.case = TRUE)){
    kmzFullName <- kmlFullName
    kmlFullName <- gsub("z$", "l", kmlFullName) 
    kmlFullName <- gsub("Z$", "L", kmlFullName) 
  }
    
  write.table(kmlOut, kmlFullName,
    col.names = FALSE, row.names = FALSE,
    quote = FALSE
  )

  if (kmz) {
    utils::zip(kmzFullName, files = kmlFullName, flags = "-q")
    return(kmzFullName)
  }

  return(kmlFullName)
}
