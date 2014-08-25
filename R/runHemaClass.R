#' Run the shiny server for hemaClass.
#' 
#' @return Starts a shiny server for the analysis
#' @author 
#'   Steffen Falgreen Larsen
#'   Anders Ellern Bilgrau
#' @details This is a local version of the server running on hemaclass.com
#' @export
runHemaClass<- function(){
  shiny::runApp(system.file('shiny', package='hemaClass'))
}