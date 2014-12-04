#' Run the shiny server for hemaClass.
#' 
#' @return Starts a shiny server for the analysis
#' @author 
#'   Steffen Falgreen Larsen
#'   Anders Ellern Bilgrau
#' @details This is a local version of the server running on hemaclass.com
#' @export
#' @import tools
#' @import shiny
#' @import shinysky
runHemaClass<- function(){
  shiny::runApp(system.file('Shiny', package='hemaClass'))
}

#' @export
runHemaClass2<- function(){
  shiny::runApp(system.file('Shiny2', package='hemaClass'))
}