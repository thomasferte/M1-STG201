#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      h1("Simulation intéractive COVID-19"),
      mod_texte_ui("texte_ui_1"),
      sidebarLayout(
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Graphique", mod_plot_ui("plot_ui_1")),
                      tabPanel("Tuto", "tutoriel"),
                      tabPanel("Article", "article"),
                      tabPanel("Sources", "sources"),
                      tabPanel("À propos", "à propos")
          ) # fin tabsetPanel
        ), # fin mainPanel
        sidebarPanel(
          mod_simulation_ui("simulation_ui_1")
        ) # fin sidebarPanel
      ) # fin sidebarLayout
    ) # fin fluidPage
  ) # fin taglist
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'appsimugolem'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

