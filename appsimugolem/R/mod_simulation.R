#' simulation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#' @import deSolve
#' 
#' 
mod_simulation_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Choisissez les interventions que vous voulez mettre en place."),
    div(
      id = "formulaire_simu",
      
      checkboxInput("want_l1", "Confinement 1", FALSE),
      numericInput("nb_l1", "Nombre", value = 1, min = 0, max = 389, step = 1),
      actionButton("val_nb_l1", "OK", class = "btn-secondary"),
      dateInput("date_l1_1", "Date début :", value = "2020-03-02", min = "2020-03-02", max = "2021-04-01", format = "dd-mm-yyyy", weekstart = 1, language = "fr"),
      dateInput("date_l1_1", "Date fin :", value="2021-04-01", min="2020-03-02", max="2021-04-01", format = "dd-mm-yyyy", weekstart = 1, language = "fr"), # il faudrait que min == date_debut
      
      checkboxInput("want_l2", "Confinement 2", FALSE),
      checkboxInput("want_curf", "Couvre-feu 18h", FALSE),
      checkboxInput("want_mask", "Masques", FALSE),
      checkboxInput("want_cs", "Écoles fermées", FALSE),
      checkboxInput("want_caf", "Cafés/Restaurants fermés", FALSE),
      checkboxInput("want_v", "Variants", FALSE),
      
      actionButton("validate", "Valider mon choix", class="btn-primary")
    )
    
  )
}
    
#' simulation Server Functions
#'
#' @noRd 
mod_simulation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}
    
## To be copied in the UI
# mod_simulation_ui("simulation_ui_1")
    
## To be copied in the server
# mod_simulation_server("simulation_ui_1")
