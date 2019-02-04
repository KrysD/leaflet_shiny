source("global.R")
# ----- Title:

header <- dashboardHeader(title = "TP Shiny")

# ----- SideBar:
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Leaflet", tabName = "leaflet", icon = icon("map",lib="font-awesome"))
  ))


# ----- Dashboard:
body <- dashboardBody(
  # ----- Introduction menu item 
  tabItems(
    # ----- Fin Map INSEE
    # ----- Fin water access map
    tabItem(tabName="leaflet",
            fluidRow(
              box(title = "Carte leaflet", solidHeader = T, status = "success",width = 5,
                  leafletOutput('mapBM')),
              box(title = "Courbe Dygraph", solidHeader = T, status = "success",width = 7,
                  dygraphOutput("courbeBM"))
            ),
            fluidRow(
              uiOutput("infoBoxBM")
            )
    )
    # ----- Fin water access map
  )
)


ui <- dashboardPage(header, sidebar, body,skin="green")


