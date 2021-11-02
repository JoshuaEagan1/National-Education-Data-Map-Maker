#making a function to produce a ggplot map of school districts

staticDistrictMapper<-function(districtData, colorVariable, colorPalette, 
                               districts_sf, contOrCat, color_names, crosswalk_name){
  districts<-merge(districts_sf, districtData, by=crosswalk_name)
  
  #making the static map
  names(districts)[c(1:ncol(districts))[names(districts)==colorVariable]]<-"color_variable_internal"
  
  ###MAKING THE CHOROPLETH MAP
  if(contOrCat=="Continuous"){
    ggplot(districts) + 
      geom_sf(aes(fill=color_variable_internal)) +
      scale_fill_gradient(low = colorPalette[1], high = colorPalette[2])+
      theme(legend.position="none", panel.grid.major = element_line(colour = "transparent"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(fill = 'white', colour = 'white'))   
    
  } else if (contOrCat=="Discrete"){
    names(colorPalette)<-color_names
    
    ggplot(districts) + 
      geom_sf(aes(fill=color_variable_internal)) +
      scale_fill_manual(values=colorPalette)+
      theme(legend.position="none", panel.grid.major = element_line(colour = "transparent"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(fill = 'white', colour = 'white'))}
  ###
}
