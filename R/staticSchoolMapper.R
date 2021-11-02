#making a function to produce a ggplot map of school districts

staticSchoolMapper<-function(schoolData, colorVariable, colorPalette, sizeVariable, schools_pts, contOrCat, color_names){
  schools<-merge(schoolData, schools_pts, all.x=T, by="SCHOOL_ID")
  
  #transforming the size variable
  if(sizeVariable==""){
    log_trans<-rep(2, nrow(schools))
  } else{
    log_trans<-log(schoolData[,sizeVariable])
    log_trans[log_trans<0]<-0
    log_trans<-log_trans+1
  }
  
  ###MAKING THE CHOROPLETH MAP
  if(contOrCat=="Continuous"){
    ggplot(schools, aes(size=log_trans)) + 
      geom_sf(aes_string(color=colorVariable)) +
      scale_color_gradient(low = colorPalette[1], high = colorPalette[2])+
      theme(legend.position="none", panel.grid.major = element_line(colour = "transparent"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(fill = 'white', colour = 'white'))   
    
  } else if (contOrCat=="Discrete"){
    names(colorPalette)<-color_names
    
    ggplot(schools, aes(size=log_trans)) + 
      geom_point(aes_string(color=colorVariable)) +
      scale_color_manual(values=colorPalette)+
      theme(legend.position="none", panel.grid.major = element_line(colour = "transparent"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(fill = 'white', colour = 'white'))}
  ###
}
