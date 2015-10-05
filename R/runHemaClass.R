#' Run the shiny server for hemaClass.
#' 
#' Run a local version of the server running on \code{hemaclass.com}. Provides
#' an easy web-based interface for the \pkg{hemaClass} package.
#' 
#' @aliases 
#'   runHemaClass
#' @return Starts a shiny server for the analysis
#' @author 
#'   Steffen Falgreen Larsen \cr
#'   Anders Ellern Bilgrau
#' @export
#' @import tools gdata WriteXLS
#' @importFrom shinysky busyIndicator
#' @importFrom shiny runApp
#' @importFrom RLumShiny jscolorInput
#' @examples
#' \dontrun{
#' library("gdata")
#' library("survival")
#' library("shinysky")
#' runHemaClass()
#' }
runHemaClass <- function() {
  shiny::runApp(system.file("website/", package = 'hemaClass'))
}
