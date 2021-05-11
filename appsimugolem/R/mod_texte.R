#' texte UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_texte_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Comment faire la simulation :"),
    p("Jouez sur les interventions dans la colonne de gauche/droite pour faire changer les courbes épidémiologiques à droite/gauche.")
  )
}
    
#' texte Server Functions
#'
#' @noRd 
mod_texte_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_texte_ui("texte_ui_1")
    
## To be copied in the server
# mod_texte_server("texte_ui_1")
