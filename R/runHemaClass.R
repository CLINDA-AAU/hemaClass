#' Run the shiny server for hemaClass.
#' 
#' Run a local version of the server running on \code{hemaclass.com}. Provides
#' an easy web-based interface for the \pkg{hemaClass} package.
#' 
#' @aliases 
#'   runHemaClass
#' @param Version What version of hemaClass should be run.
#' @return Starts a shiny server for the analysis
#' @author 
#'   Steffen Falgreen Larsen \cr
#'   Anders Ellern Bilgrau
#' @export
#' @import tools shiny gdata WriteXLS RLumShiny
#' @importFrom shinysky busyIndicator
#' @examples
#' \dontrun{
#' library("gdata")
#' library("survival")
#' library("shinysky")
#' runHemaClass()
#' }
runHemaClass<- function(Version = 0.1){
  shiny::runApp(system.file(paste0("homepage", "/Version_", Version), 
                            package='hemaClass'))
}
