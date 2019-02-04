source("global.R")

server <- function(input, output){
  
  # ----- Reactive Values
  reactVal <- reactiveValues(idShapeSelected = NULL,nameShapeSelected=NULL,dateWindow = NULL)
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
  
  observeEvent(input$mapBM_shape_click,{
    
    if(input$mapBM_shape_click$id == "selected"){
      reactVal$idShapeSelected <- NULL
      reactVal$nameShapeSelected <- NULL
      
      leafletProxy("mapBM")%>%
        clearGroup("selected")
    }else{
      reactVal$idShapeSelected <- input$mapBM_shape_click$id
      reactVal$nameShapeSelected <- as.character(contourIris@data$NOM_IRIS[contourIris@data$DCOMIRIS==reactVal$idShapeSelected])
      
      leafletProxy("mapBM")%>%clearGroup("selected")
      
      leafletProxy("mapBM")%>%
        addPolygons(data = contourIris[contourIris$DCOMIRIS==reactVal$idShapeSelected,],
                    layerId = "selected",
                    group = "selected",
                    color = "#DB4437",
                    opacity = 1,
                    fillOpacity = 0)
    }


    
  })
  # ----- End Leaflet Map
  
  # ----- Dygraph
  output$courbeBM <- renderDygraph({
    if(!is.null(reactVal$idShapeSelected)){
      TLRV_selected <- filter(TLRV_bdx,ID%in%Base_client$ID[Base_client$DCOMIRIS==reactVal$idShapeSelected])
      CMJ_selected <- TLRV_selected %>% group_by(J) %>% dplyr::summarise(CMJ=mean(vol, na.rm = T))
      CMJ_selected <- xts(CMJ_selected[,-1], order.by = CMJ_selected$J)
      
      CMJ_both <- cbind(CMJ_bdx,CMJ_selected)
      names(CMJ_both) <- c("Bordeaux",reactVal$nameShapeSelected)
      
      main <- sprintf("Consommation moyenne journalière Bordeaux VS %s",reactVal$nameShapeSelected)
      
      dy <- dygraph(CMJ_both,main = main)%>% 
        dyAxis("y", label = "Conso (m3)") %>%
        dySeries("Bordeaux", axis = 'y', label = "Bordeaux", color="#00aedb")  %>%
        dySeries(reactVal$nameShapeSelected, axis = 'y', label = reactVal$nameShapeSelected , color="#DB4437")  %>%
        dyLegend(width = 400)
      
    }else{
      dy <- dygraph(CMJ_bdx,main = "Consommation moyenne journalière de Bordeaux")%>% 
        dyAxis("y", label = "Conso (m3)") %>%
        dySeries("CMJ", axis = 'y', label = "Bordeaux", color="#00aedb")  %>%
        dyLegend(width = 400)
    }
    return(dy)
  })
  
  observeEvent(input$courbeBM_date_window,{
    reactVal$dateWindow <- as.POSIXct(substr(input$courbeBM_date_window,1,10),tz = "GMT")

    leafletProxy("mapBM")%>%clearGroup("background")
    
    leafletProxy("mapBM")%>%clearGroup("selected")
    
    TLRVfilter <- filter(TLRV_bdx,(J>=reactVal$dateWindow[1])&(J<=reactVal$dateWindow[2]))
    TLRVfilter$DCOMIRIS <- Base_client$DCOMIRIS[match(TLRVfilter$ID, Base_client$ID)]
    CMJfilter <- TLRVfilter %>% group_by(DCOMIRIS) %>% dplyr::summarise(CMJ=mean(vol, na.rm = T))
    contourIris@data$CMJ <- CMJfilter$CMJ[match(contourIris@data$DCOMIRIS, CMJfilter$DCOMIRIS)]
    
    
    # ----- Bornes
    bins <- c(0, 1, 2, 3, 4, 5, 50, Inf)
    # ----- Palette sur les données
    pal <- colorBin("Blues", domain = contourIris$CMJ, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g m<sup>3</sup>/j",
      contourIris$NOM_IRIS, round(contourIris$CMJ,2)
    ) %>% lapply(htmltools::HTML)
    
    
    leafletProxy("mapBM")%>%
    addPolygons(layerId = contourIris$DCOMIRIS,
                data = contourIris,
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
                  direction = "auto"))%>%
      addPolygons(data = contourIris[contourIris$DCOMIRIS==reactVal$idShapeSelected,],
                  layerId = "selected",
                  group = "selected",
                  color = "#DB4437",
                  opacity = 1,
                  fillOpacity = 0)
  })
  # ----- End Dygraph
  
  # ----- Box
  
  output$infoBoxBM <- renderUI({
    
    if(!is.null(reactVal$idShapeSelected)){
      from = reactVal$dateWindow[1]
      to = reactVal$dateWindow[2]
      
      TLRVfilter <- filter(TLRV_bdx,ID%in%Base_client$ID[Base_client$DCOMIRIS==reactVal$idShapeSelected])
      TLRVfilter <- filter(TLRVfilter,(J>=from)&(J<=to))
      
      id.min <- TLRVfilter$ID[which.min(TLRVfilter$vol)]
      conso.min <- round(min(TLRVfilter$vol,na.rm=T),4)
      
      id.max <- TLRVfilter$ID[which.max(TLRVfilter$vol)]
      conso.max <- round(max(TLRVfilter$vol,na.rm=T),4)
      
      
      infobox <- box(width = 12, title = HTML(sprintf("Info conso : <b>%s</b> Du <b>%s</b> au <b>%s</b>",
                                                      reactVal$nameShapeSelected,
                                                      reactVal$dateWindow[1],
                                                      reactVal$dateWindow[2])),
                     solidHeader = T, status = "success",
          infoBox("CMJ moyenne période sélectionnée",round(mean(TLRVfilter$vol,na.rm=T),4),icon = icon("tint"),fill = T),
          infoBox(sprintf("CMJ minimale : %s",id.min),conso.min,icon = icon("arrow-down"),fill = T,color = "green"),
          infoBox(sprintf("CMJ maximale : %s ",id.max),conso.max,icon = icon("arrow-up"),fill = T,color = "red")
      )
      return(infobox)
    }

  })
  
  # ----- End Box

}