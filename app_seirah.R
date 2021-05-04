
# ================================================================================================================================= #
# ============                                                LIBRAIRIES                                               ============ #
# ================================================================================================================================= #

setwd("C:/Users/cleme/Documents/Etudes/MASTER SANTE PUBLIQUE/M1/S8/STG201 - Stage d'insertion professionnel en santé publique/code")

library(cli)
library(deSolve)
library(dplyr)
library(ggplot2)
library(ggridges)
library(golem)
library(gridExtra)
library(lattice)
library(lme4)
library(plotly)
library(png)
library(readr)
library(saemix)
library(scales)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(stringr)
library(tidyr)


# ================================================================================================================================= #
# ============                                                 DONNÉES                                                 ============ #
# ================================================================================================================================= #

        # Copier la partie 'données' du fichier R 'data_graphs_regression'.


# ================================================================================================================================= #
# ============                                                FONCTIONS                                                ============ #
# ================================================================================================================================= #

        # Copier les parties nécessaires du fichier R 'simu_seirah'.


# ================================================================================================================================= #
# ============                                                APPLI : UI                                               ============ #
# ================================================================================================================================= #

ui <- dashboardPage(
    dashboardHeader(title = "Application SEIRAH"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Présentation", tabName ="dashboard", icon=icon("info")),
            menuItem("Méthodes", icon = icon("chart-bar"), tabName ="widgets",
                     menuSubItem("Introduction", tabName = "method1"),
                     menuSubItem("Filtre de Kalman", tabName = "method2"),
                     menuSubItem("Régression", tabName = "method3")),
            menuItem("Figures & Simulations", icon = icon("chart-bar"), tabName ="widgets",
                     menuSubItem("Figures", tabName = "simu1"),
                     menuSubItem("Simulations interventions", tabName="simu2")),
            menuItem("Informations supplémentaires", icon = icon("th"), tabName ="widgets2"))),
    
    dashboardBody(
        tags$head(tags$style(HTML('.skin-blue .main-header .logo {background-color: #20355c;}
                              .skin-blue .main-header .logo:hover {background-color: #18387a;}
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: #66738c;}
                              .skin-blue .main-header .navbar{ background-color : #85a1d6;}
                              #sidebar { background-color : #040d21; }
                              #titre { font-weight : bold }
                              #txt { text-align : justify }
                              #txt { color: #00000'))),
        tabItems(
            tabItem(tabName = "dashboard",
                    h3("Lorem ipsum", style="color:#426096"),
                    p(id = "txt", "ici présentation du sujet"),
                    h3("Objectifs", style="color:#426096"),
                    p(id = "txt", "ici objectifs."),
                    h3("[...]", style="color:#426096"),
                    p(id = "txt", "qqch d'autre ?")
            ),
            
            tabItem(tabName = "method1",
                    h3("Introduction", style="color:#426096"),
                    p(id = "txt", "ici l'introduction du sujet, des variables utilisées.")
            ),
            
            tabItem(tabName = "method2",
                    h3("Filtre de Kalman", style="color:#426096"),
                    p(id = "txt", "ici l'explication de la méthode mise en place avec le filtre de Kalman.")
            ),
            
            tabItem(tabName = "method3",
                    h3("Régression", style="color:#426096"),
                    p(id = "txt", "ici l'explication du modèle de régression utilisé.")
            ),
            
            tabItem(tabName = "simu1",
                    h3("Figures", style="color:#426096"),
                    p(id = "txt", "ici mettre les différents graphiques statiques.")
            ),
            
            tabItem(tabName = "simu2",
                    h3("Simulation Interventions", style="color:#426096"),
                    p(id = "txt", "ici mettre (à gauche ?) les spécifications d'interventions désirées et (à droite ?) le graphique dynamique de : b ? logbOnNonVaccine ? les différentes courbes du modèle SEIRAH ? qqch d'autre ? ou choisir le graphique que l'on veut afficher ?")
            ),
            
            tabItem(tabName = "widgets2",
                    h3("Informations générales sur cette application", style="color:#426096"),
                    p(id = "txt","Ce site a été réalisé dans le cadre de [...]"),
                    p(id = "txt","Données utilisées : "),
                    a(href="", "site1"),
                    a(href="", "site2"),
                    a(href="", "site3"),
                    h3("Sources - Bibliographie", style="color:#426096"),
                    p(id = "txt","[1] Source 1 .")
            )
        ) # fin tabitems
    ) # fin dashboardBody
) # fin dashboardPage


# ================================================================================================================================= #
# ============                                              APPLI : SERVER                                             ============ #
# ================================================================================================================================= #

server <- function(input, output) {
    
    # 
    
}


# ================================================================================================================================= #
# ============                                                 LANCEMENT                                               ============ #
# ================================================================================================================================= #

shinyApp(ui = ui, server = server)
