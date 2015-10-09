#' Run the shiny server for hemaClass.
#' 
#' Run a local version of the server running on \url{http://hemaclass.org}. 
#' Provides an easy-to-used and web-based interface for the \pkg{hemaClass} 
#' package.
#' 
#' @aliases runHemaClass runhemaclass
#' @param debug.help logical. Should the code be displayed next to or beneath
#'   the web application. Useful when debugging or showcasing the app.
#' @param launch.browser logical. If \code{TRUE}, the web browser is used.
#'   Default is \code{TRUE}. If \code{FALSE}, the default from 
#'   \code{\link{runApp}} is used.
#' @param \dots Arguments passed to \code{\link{runApp}}.
#' @return Starts up a local Shiny server instance of the 
#'   \url{http://hemaclass.org} website for faster analysis.
#' @note If the Rstudio viewer is used, only a single \code{.CEL} file can be 
#'   uploaded, please use your browser instead.
#' @references \url{http://hemaclass.org}
#' @seealso \code{\link{runApp}}
#' @author 
#'   Steffen Falgreen Larsen <falgreen (at) me.com>\cr
#'   Anders Ellern Bilgrau \cr
#'   Jonas Have
#' @export
#' @import tools gdata WriteXLS
#' @importFrom shinysky busyIndicator
#' @importFrom shiny runApp
#' @importFrom RLumShiny jscolorInput
#' @examples
#' \dontrun{
#' runHemaClass()
#' 
#' # For a showcase/debug help mode:
#' runHemaClass(debug.help = TRUE)
#' 
#' # To use the Rstudio viewer:
#' runHemaClass(launch.browser = FALSE) 
#' }
runHemaClass <- function(launch.browser = TRUE, debug.help = FALSE, ...) {
  if (!isTRUE(launch.browser)) {
    launch.browser <- getOption("shiny.launch.browser", interactive())
  }
  shiny::runApp(system.file("website/", package = 'hemaClass'),
                launch.browser = launch.browser,
                display.mode = ifelse(debug.help, "showcase", "auto"))
}
