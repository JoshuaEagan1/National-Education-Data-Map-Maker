#a function to use the inputs from the districts section of the shiny app to 
#generate the leaflet output
appDistrictMapper<-function(districtData, colorVariable, colorVariableTitle="", colorPalette, 
                            popupVectorVariables="", popupVectorNames="", subset=NULL, districts_sf,
                            crosswalk_name){
  
  #creating the popupVector
  popupVector<-popupVectorVariables
  names(popupVector)<-popupVectorNames
  
  #filtering districtData:
  if(!is.null(subset)){districtData<-districtData %>% filter(subset)}
  
  districts<-merge(districts_sf, districtData, by=crosswalk_name)
  
  #making the html popups
  if(popupVector!=""){
  cater<-districts %>% as_tibble() %>% select(unname(popupVector))
  cater[] <- lapply(cater, as.character)
  cater[is.na(cater)]<-" "
  
  
  district_popup <- rep("", nrow(cater))
  for(i in 1:nrow(cater)){
    for(j in 1:ncol(cater)){
      district_popup[i] <-paste0(district_popup[i], 
                                 paste0("<strong>", names(popupVector)[j], ": </strong>", cater[i,j], "<br>"
                                 ))
      }
    }
  }
  
  #assembling the leaflet
  l<-leaflet(data = districts) %>% addProviderTiles(providers$Stamen.Toner)
  if(popupVector!=""){
  l<-l %>%
    addPolygons(color = "#444444", 
                weight=1,
                fillColor = colorPalette(as.vector(t(st_drop_geometry(districts[,colorVariable])))), 
                popup=district_popup,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)
    )}
  else {
    l<-l %>%
      addPolygons(color = "#444444", 
                  weight=1,
                  fillColor = colorPalette(as.vector(t(st_drop_geometry(districts[,colorVariable])))), 
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
                  
  )}
  l %>% addLegend(position="bottomright", 
                  pal=colorPalette,
                  values=as.vector(t(st_drop_geometry(districts[,colorVariable]))),
                  title = colorVariableTitle)
}