source("global.R")

server <- function(input, output){
  
  # ----- Reactive Values
  
  # ----- End Reactive Values
  
  # ----- Leaflet Map
  output$mapBM <- renderLeaflet({
    # ----- Bornes
    bins <- c(0, 1, 2, 3, 4, 5, 50, Inf)
    # ----- Palette sur les données
    pal <- colorBin("Blues", domain = contourIris$CMJ, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g m<sup>3</sup>/j",
      contourIris$NOM_IRIS, round(contourIris$CMJ,2)
    ) %>% lapply(htmltools::HTML)
    
    leaflet(contourIris) %>%
      addTiles() %>%
      addPolygons(layerId = contourIris$DCOMIRIS,
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  fillColor = ~pal(CMJ),
                  dashArray = "3",
                  fillOpacity = 0.7,
                  group = "background",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
  })
  # ----- End Leaflet Map
  
  # ----- Dygraph
  output$courbeBM <- renderDygraph({

      dy <- dygraph(CMJ_bdx,main = "Consommation moyenne journalière de Bordeaux")%>% 
        dyAxis("y", label = "Conso (m3)") %>%
        dySeries("CMJ", axis = 'y', label = "Bordeaux", color="#00aedb")  %>%
        dyLegend(width = 400)
  })
  # ----- End Dygraph
  
  # ----- Box
  
  output$infoBoxBM <- renderUI({
      infobox <- box(width = 12, title = "Info conso :", solidHeader = T, status = "success",
          infoBox("CMJ moyenne période sélectionnée",0,icon = icon("tint"),fill = T),
          infoBox("CMJ minimale :",0,icon = icon("arrow-down"),fill = T,color = "green"),
          infoBox("CMJ maximale :",0,icon = icon("arrow-up"),fill = T,color = "red")
      )
      return(infobox)
  })
  
  # ----- End Box

}