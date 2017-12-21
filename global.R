# ----- Load and install missing packages
packages<-c("shiny","shinydashboard","leaflet","dygraphs","highcharter","xts","dplyr")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, require, character.only = TRUE)

load("data/data.RData")


# ----- Calcul CMJ BDX
CMJ_bdx <- TLRV_bdx %>% group_by(J) %>% dplyr::summarise(CMJ=mean(vol, na.rm = T))
CMJ_bdx <- xts(CMJ_bdx[,-1],order.by = CMJ_bdx$J)

