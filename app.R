#creating a shiny app to implement the districtMapper function

#https://shiny.rstudio.com/gallery/navbar-example.html

#loading necessary packages
library(tidyverse)
library(leaflet)
library(leaflet.providers)
library(sf)
library(RColorBrewer)
library(shiny)
library(openxlsx)
library(colourpicker)
library(foreign)
library(haven)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(htmlwidgets)
library(mapview)
library(shinyhelper)
library(DataEditR)
library(ggthemes)

#Calder Color Pallet: 
#green: #B5C093
#purple: #66609F
#black: #4D4D4E
#off-white: #E2DEDA 
#white: #FFFFFF

ui <-fluidPage(tags$head(HTML("<title>National Education Data Map Maker</title>")),
        navbarPage(
  title= tagList(tags$img(src = "CALDER logo.png"),
  tags$h2("National Education Data Map Maker")),
  
  # Create Right Side Logo/Image with Link       
  tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><img src=\"AIR logo.png\" style=\"float:right;padding-top:10px;\"></div>');
    console.log(header)")
  ),
  
  tags$head(includeCSS("www/styling.css")), 
  
     navbarMenu("Create Map",
            tabPanel("District Mapper", districtMapper_UI("districtMapperModule"))
            ,tabPanel("School Mapper", schoolMapper_UI("schoolMapperModule"))
            #, tabPanel("Data Editor", tags$h1("THIS WILL CONTAIN THE DATA EDITOR WIDGET"))
                ),
                
            tabPanel("About", #This tab creates the `about` tab
                     fluidRow( 
                       style="background-image: url('benjamin-child-0sT9YhNgSEs-unsplash (1).JPG');
      height: 375px;
      background-position: center;
      background-size: cover;",
                       tags$h1(style="color: #FFFFFF;
              padding-top: 200px;
              background: rgba(128, 128, 128, .5);", 
                               "The National Education Data Map Maker")
                     ),
                     fluidRow(tags$br()),
                     fluidRow(
                     style="background-color:#B5C093;",
                     column(width=5,
                            tags$h2("Purpose"), offset=1)),
  
  fluidRow(
    style="background-color:#B5C093;",
    column(width=5,
           tags$h4("For most people, it's natural to think in terms of geography.
                   Whether they are parents trying to decide which school district
                   to move their family to or policy makers trying to understand
                   the climate of equity between schools in their state, people
                   like to see their data mapped geographically. However, 
                   software for this purpose tends to be difficult to use or
                   prohibitively expenesive. The National Education Data Map 
                   Maker is intended to empower people to make informed
                   decisions in their relationships with the US education system
                   by providing a free, interactive environment for people to
                   explore education data geographically."), offset=1),
    
            ),
  
  fluidRow(
    column(width=5, tags$h2("How Does it Work?"), offset=6)),
  fluidRow(
    column(width=5, 
           tags$h4("This application lets users: "),
                   tags$ul( #create a bulleted list
                     tags$li(tags$h4("upload their own data about schools or school districts")), 
                     tags$li(tags$h4("customize how they want their map to look and behave")), 
                     tags$li(tags$h4("create an interactive leaflet style map that allows for
                     zoom and click functionality and has the ability to display
                     information using color ", tags$a(href="https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html", "(see this online example leaflet map.)"))),
                     tags$li(tags$h4("export either their leaflet map or a 'static'
                     version of the map as a .png image file."))
                     ),
           tags$h4("Click on the 'tutorial' tab for more information.")
           , offset=6)),
  fluidRow(style="background-color:#4d4d4d; color:#FFFFFF;",
    column(width=5, tags$h2("Who Made This?"), offset=1)),
  fluidRow(
    style="background-color:#4d4d4d; color:#FFFFFF;",
    column(width=5, tags$h4("All the code for this app was written by 
                             Joshua Eagan as a part of the Missouri Data Fellows
                             Program, a collaboration between the University of
                             Missouri Economics Department and Missouri's 
                             Department of Elementary and Secondary Education. 
                             To see more of Joshua's projects, visit his ", tags$a(href="https://github.com/JoshuaEagan1", "Github"),
                             tags$br(), tags$br(), "This project would not have been 
                             possible without the developers at R Studio and the 
                             authors of the handful of R packages used to create this
                             application. Also, special thanks to Zoe Rich for 
                             the graphic design advice." 

        ), offset=1)), fluidRow(style="background-color:#4d4d4d; color:#FFFFFF;", tags$br(), tags$br()),

  ),

            tabPanel("Tutorial", #This tab creates the `Tutorial` tab
                     fluidRow( 
                       style="background-image: url('campaign-creators-gMsnXqILjp4-unsplash.jpg');
      height: 375px;
      background-position: center;
      background-size: cover;",
                       tags$h1(style="color: #FFFFFF;
              padding-top: 200px;
              background: rgba(128, 128, 128, .5);", 
                               "Tutorial")
                     ),
                     fluidRow(tags$br()),
                     fluidRow(
                       style="background-color:#B5C093;",
                       column(width=5,
                              tags$h2("Preparing the Data"), offset=1)),
                     
                     fluidRow(
                       style="background-color:#B5C093;",
                       column(width=5,
                              tags$h4("Before you can make your first map, you 
                                      need to have some data in a format that you
                                      can upload to this app. Don't worry, it's 
                                      not as difficult as it sounds, this app good
                                      at figuring out how to work with your data.
                                      There are only a few requirements about the file you upload:"),
                                      tags$br(), 
                                      tags$ul( #create a bulleted list
                                        tags$li(tags$h4("It needs to be an Excel file (.xlsx), comma separated values file (.csv), STATA data file (.dta), or SAS data file (.sasbdat7).")), 
                                        tags$li(tags$h4("It has to have a correctly named identifier column. You have two options for identifier columns: the NCES assigned school/district identifier or a state's internal-use identifier.",  tags$b("If you are mapping school districts,"), " your data needs to contain a column titled `NCES_District_Identifier` (case sensitive) containing the NCES code for each district OR a column titled `Internal_District_Identifier` (also case sensitive) containing the capitalized two letter state abbreviation and internal use district code separated by a dash-e.g., `MO-123456`. ",  tags$b("If you are mapping schools,"), "you must have one of the following- a column called 'NCES_Identifier' (case sensitive) containing the NCES assigned identifier for each school OR two columns titled `Internal_School_Identifier` and `Internal_District_Identifier` (each case sensitive) containing a state education agency's internal ID number for each school and district, respectively. Again, in the case of schools, the `Internal_District_Identifier` should begin with the capitalized two letter state abbreviation followed by `-`. If both the NCES and internal identifiers are present, the NCES identifier will be used.")),
                                        tags$li(tags$h4("There can only be one row for each district if you are mapping districts and there can only be one row for each school if you are mapping schools.")) 
                                        ),
                                      tags$br(), 
                              tags$h4("If these are all true, you can upload your data successfully. One last thing: it is stongly reccomended that you only map data for one or two states at a time. Trying to visualize more states than this will lead to a very long wait to generate your map."), offset=1),
                     column(width=5, tags$iframe(src="https://www.youtube.com/embed/BHS42Rh_dlo", width = "90%", height = "400", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA), offset=1)
                       ),
                     fluidRow(
                       column(width=5,
                              tags$h2("Customizing Map Features" ), offset=6)),
                     fluidRow(
                       column(width=5, tags$iframe(src="https://www.youtube.com/embed/ghr7gaFGdSY", width = "90%", height = "400", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA), tags$br(), offset=1),
                       column(width=5,
                              tags$h4("Once you upload your data, you have a few
                                      options you can set to customize your map.
                                      Firstly, can select a variable that you want to 
                                      represent using color. Since some variables
                                      are better represented on a spectrum (from 
                                      low to high) and others would be best displayed 
                                      by picking unique colors for every value they 
                                      could take (think variables that can only take
                                      a few possible values), this app will let you do either.
                                      In the app, select `continuous` to do the former
                                      and `discrete` to do the latter. If you're 
                                      confused by this, there will be a help button
                                      icon to help you. After you select your variable,
                                      choose colors for the range of different values
                                      your variable can take." ),
                              tags$br(), 
                              tags$h4("Next you'll be prompted to select some 
                                      variables to pop up whenever you click on
                                      a district or school. This is optional,
                                      but setting this is a great way to make 
                                      your app interactive explore your data 
                                      across multiple dimensions. Be sure to 
                                      name your variables accurately! This is
                                      how they will be labeled when a district
                                      is clicked on."),
                              tags$br(), 
                              tags$h4("If you're using the school mapper, you can
                                      associate the size of the points (representing
                                      schools) on your map with another one of your
                                      variables, much like you did with your `color variable`.
                                      This variable has to be continuous, so it 
                                      can not be a variable that has non-numeric
                                      characters such as letters or @, #, $, etc.
                                      This feature, like the previous feature, is optional.
                                      If you do choose to select a size variable, it 
                                      should contain only positive values. It's alright
                                      if there are a few negative values, but in the
                                      context of this app, negative size does not make
                                      sense, so schools with negative values of the 
                                      size variable will show up as very small dots
                                      for convenience.
                                      "),
                              tags$br(), 
                              tags$h4("Lastly, keep in mind that if the columns you selected
                              for the color or size variables contain missing values,
                              the associated schools or districits will be dropped-i.e.,
                              not included in the map output.")
                              )
                       ),
                     fluidRow(style="background-color:#4d4d4d; color:#FFFFFF;", 
                       column(width=5,
                              tags$h2("Rendering and Exporting the Map" ), offset=1)),
                     fluidRow(style="background-color:#4d4d4d; color:#FFFFFF;", 
                       column(width=5,
                              tags$h4("When you're done choosing the settings to
                                      customize your map, click the `Run App` button
                                      to generate the map. You'll be able to veiw
                                      your map in the app itsself or download it.
                                      If you would like to 
                                      save the leaflet map to reference it later, you can
                                      save it as a .html file. Any web browser 
                                      such as Google Chrome, Microsoft Edge, or
                                      Mozilla Firefox will be able to open the
                                      file to let you veiw your map again. All 
                                      you have to do to reopen it is double click
                                      on the file in your file explorer."),
                              tags$br(),
                              tags$h4("If you would perfer to save an image of your
                                      map instead of the interactive HTML leaflet 
                                      file, you can save an image file (.png) of 
                                      your map by clicking the `Save Map Screenshot`
                                      button."), offset=1),
                       column(width=5, tags$iframe(src="https://www.youtube.com/embed/XWSskIFiBe8", width = "90%", height = "400", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA), offset=1)
                     ),
                     
                     fluidRow(style="background-color:#4d4d4d; color:#FFFFFF;", tags$br(), tags$br())
                     
                     
                     
                     
                     )
            #, tabPanel("Documentation", tags$h1("THIS WILL CONTAIN DOCUMENTATION"))
                     
                     ,
                
  #style="background-color:#CB4C4E;",
  #theme = shinytheme("journal"),
  selected="District Mapper",
  
  tags$footer(column(3, "Produced by Joshua Eagan"), column(3, "Powered by R Shiny"), 
              column(2, tags$a(href="mailto:jmemq7@mail.missorui.edu", tags$b("Contact me!"), 
                               class="externallink", style = "color: white; text-decoration: none")), 
              column(3, actionLink("twitter_share", label = "Share", icon = icon("twitter"),
                                   style= "color:white;", onclick = sprintf("window.open('%s')", 
                                                                            "https://twitter.com/intent/tweet?text=Check%out%ScotPHO's%profile%tool&url=https://www.r-graph-gallery.com/"))), 
              style = "
   position:fixed;
   text-align:center;
   left: 0;
   bottom:0;
   width:100%;
   z-index:1000;  
   height:30px; /* Height of the footer */
   color: white;
   padding: 10px;
   font-weight: bold;
   background-color: #4d4d4d"
  ) 
  
))

server <- function(input, output, session) {
  observe_helpers(help_dir = "help_mds")
  districtMapper_Server("districtMapperModule")
  schoolMapper_Server("schoolMapperModule")
}

shinyApp(ui, server)