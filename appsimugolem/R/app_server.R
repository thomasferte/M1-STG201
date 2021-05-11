#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_texte_server("texte_ui_1")
  mod_simulation_server("simulation_ui_1")
  mod_plot_server("plot_ui_1")
}
