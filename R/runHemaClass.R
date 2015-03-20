#' Run the shiny server for hemaClass.
#' 
#' Run a local version of the server running on \code{hemaclass.com}.
#' 
#' @aliases 
#'   runHemaClass
#' @param Version What version of hemaClass should be run.
#' @return Starts a shiny server for the analysis
#' @author 
#'   Steffen Falgreen Larsen
#'   Anders Ellern Bilgrau
#' @export
#' @import tools
#' @import shiny
#' @import shinysky
#' @import gdata
#' @import WriteXLS
#' @examples
#' \dontrun{
#' require(gdata)
#' require(survival)
#' require(shinysky)
#' runHemaClass()
#' }
runHemaClass<- function(Version = 0.1){
  shiny::runApp(system.file(paste0("homepage", "/Version_", Version), 
                            package='hemaClass'))
}
