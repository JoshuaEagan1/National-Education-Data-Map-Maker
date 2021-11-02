#a function to use the inputs from the schools section of the shiny app to 
#generate the leaflet output

appSchoolMapper<-function(schoolData, colorVariable, colorVariableTitle=NULL, colorPalette, 
                            popupVectorVariables="", popupVectorNames="", subset=NULL,
                          sizeVariable, schools_pts, crosswalk_name){
 
  #creating the popupVector
  popupVector<-popupVectorVariables
  names(popupVector)<-popupVectorNames
  
  #filtering schoolData:
  if(!is.null(subset)){schoolData<-schoolData %>% filter(subset)}
  
  schools<-merge(schoolData, schools_pts, by=crosswalk_name)
  
  #making the html popups
  if(popupVector!=""){
  cater<-schools %>% as_tibble() %>% select(unname(popupVector))
  cater[] <- lapply(cater, as.character)
  cater[is.na(cater)]<-" "
  
  school_popup <- rep("", nrow(cater))
  for(i in 1:nrow(cater)){
    for(j in 1:ncol(cater)){
      school_popup[i] <-paste0(school_popup[i], 
                                 paste0("<strong>", names(popupVector)[j], ": </strong>", cater[i,j], "<br>"
                                 ))
      }
    }
  }
  
  #transforming the size variable
  if(sizeVariable==""){
    log_trans<-rep(2, nrow(schools))
  } else{
    log_trans<-log(schoolData[,sizeVariable])
    log_trans[log_trans<0]<-0
    log_trans<-log_trans+1
  }

  
  #assembling the leaflet
  l<-leaflet(data = schools) %>% addProviderTiles(providers$Stamen.Toner)
  
  if(popupVector!=""){
  l<-l %>%
    addCircleMarkers( lng = ~Lon, lat = ~Lat,  opacity = 0.75, fillOpacity = 0.4,
                      color = colorPalette(as.vector(schools[,colorVariable])),
                      popup = school_popup,
                      radius = log_trans)}
  else {
  l<-l %>%
    addCircleMarkers( lng = ~Lon, lat = ~Lat,  opacity = 0.75, fillOpacity = 0.4,
                      color = colorPalette(as.vector(schools[,colorVariable])),
                      radius = log_trans)
  }
  l %>% addLegend(position="bottomright", 
                  pal=colorPalette,
                  values=as.vector(schools[,colorVariable]),
                  title = colorVariableTitle)
}
