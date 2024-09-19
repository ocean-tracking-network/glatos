#' ```{r child = "ReadMe.md"}
#' ```
#' @docType package
#' @name glatos
#' 
#' @import data.table sp
#' @importFrom graphics abline axis box legend lines mtext par points symbols
#'   text
#' @importFrom stats approx dnorm ecdf end fivenum median na.omit rbinom rnorm
#'   runif start step
#' @importFrom utils capture.output read.csv packageVersion setTxtProgressBar
#'   txtProgressBar unzip write.csv write.table zip
#' @importFrom grDevices bmp colorRampPalette dev.new dev.off jpeg png rainbow
#'   tiff
#' @importFrom dplyr %>%
"_PACKAGE"

# avoid R CMD check note
globalVariables(".")

# current nickname
nickname <- function() "very-refreshing-lemonade"

# package startup message
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "version ", utils::packageVersion("glatos"),
    " ('", nickname(), "')"
  ))
}
