#a shiny module to visualize Missouri school district data geographically

schoolMapper_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow( 
      style="background-image: url('javier-trueba-iQPr1XkF5F0-unsplash.jpg');
      height: 375px;
      background-position: center;
      background-size: cover;",
      tags$h1(style="color: #FFFFFF;
              padding-top: 200px;
              background: rgba(128, 128, 128, .5);", 
              "Create a Leaflet Style Map of Missouri Schools")
    ),
    
    fluidRow(tags$br()),
    
    fluidRow(
      style="background-color:#B5C093;",
      column(width=10,
             tags$h2("Import School Data"), offset=1)),
    
    fluidRow(
      style="background-color:#B5C093;", column(width=10,
      column(width=6,
             tags$h4("First, upload an Comma Separated Values (.csv), Excel (.xslx), 
                   STATA (.dta) or SAS (.sas7bdat) data file with the
                   data you would like to use to build a map using the prompt 
                   to the right. This data table 
                   MUST contain a column called 'NCES_Identifier' containing 
                   the NCES assigned identifier for each school or two columns 
                   titled `Internal_School_Identifier` and `Internal_District_Identifier`
                   containing a state education agency's internal ID number for
                   each school and district, respectively.")),
      column(width=3),
      column(width=3,
             fileInput(ns("file1"), "Choose Data File",
                       multiple = FALSE,
                       accept = c(".csv", ".xlsx", ".dta", ".sas7bdat"))),
    offset=1)),
    
    fluidRow(column(uiOutput(ns("section2_1ui")), width=12)),
    fluidRow(column(uiOutput(ns("section2_2ui")), width=12)),
    
    fluidRow(
      column(width=10,
             tags$h2("Configure the Color Variable"), offset=1)),
    
    fluidRow(column(width=10, 
      column(width=3, tags$h4("Which variable should be illustrated on the map?"))
      ,column(width=3, tags$h4("What should this variable be called in the map legend?"))
      ,column(width=3, tags$h4("Should this variable be treated as continuous or discrete?")),
    offset=1)),
    
    fluidRow(column(width=10, 
      column(width=3, uiOutput(ns("selectVar")))
      ,column(width=3, uiOutput(ns("altTitleUi")))
      ,column(width=3, helper(selectInput(ns("contOrCat"), "Continuous or Discrete?", c("Continuous", "Discrete")), icon = "question-circle",
                              colour = "#66609F",
                              type = "markdown",
                              content = "cont_vs_disc_schools"
      )),
    offset=1)),
    
    #blank row to add 'whitespace'
    fluidRow(tags$br(), tags$br()),
    
    fluidRow(column(width=10, 
      uiOutput(ns("varTypeText")), offset=1)),
    
    fluidRow(column(width=10, 
      uiOutput(ns("colorPaletteInputUI")), offset=1))
    
    ,fluidRow(style="background-color:#4d4d4d; color:#FFFFFF;", column(width=10,tags$h2("Set Up Popup Text"), offset=1))
    ,fluidRow(style="background-color:#4d4d4d; color:#FFFFFF;", column(width=6, tags$h4("Which variables should display as text when a user clicks on a school?"), tags$br(), tags$br(), offset=1))
    ,fluidRow(style="background-color:#4d4d4d; color:#FFFFFF;", column(width=10, 
              column(width=3, tags$h4("Select Multiple Popup Variables:"), uiOutput(ns("selectPopupVars"))),
              column(width=9, tags$h4("How should these variables be labeled?"), uiOutput(ns("popupNameUI"))), offset=1))
    
    ,fluidRow(
      column(width=10, tags$h2("Configure the Size Variable"), offset=1))
    
    ,fluidRow(
      column(width=5, 
      helper(tags$h4("Which variable should be associated with the size
                              of each point on the map?"), icon = "question-circle",
             colour = "#66609F",
             type = "markdown",
             content = "size_var_schools"
      ),
             uiOutput(ns("sizevarwarnUI")), offset=1)
             )
    
    ,fluidRow(
      column(width=5, uiOutput(ns("selectSizeVar")), offset=1)
    )
    
    ,fluidRow(
      style="background-color:#B5C093; text-align: center; height: 700px; padding-bottom:50;",
      column(width=3, tags$br(), 
             actionButton(ns("button"), "Run App"),
             tags$br(), tags$br(), 
             uiOutput(ns("reportDownloaderUI")))
      , column(width=9, tags$br(), withSpinner(leafletOutput(ns("leaflet_in_app"), height=600),type=6, color ="#66609F"))
    )
  )
}

schoolMapper_Server <- function(id) {
  
  moduleServer(
    id,
    
    function(input, output, session){
      
      #loading school level point data
      load("./data/schools_pts.R")
      points<-points %>% mutate(Internal_Identifier=paste0(Internal_District_Identifier, Internal_School_Identifier)) %>%
        select(-c(5,6))
      
      intermediate <- reactive ({req(input$file1)
        
        if(substrRight(input$file1$datapath, 4)==".csv"){
          intermediate<-read.csv(input$file1$datapath, header=T)}
        else if (substrRight(input$file1$datapath, 5)==".xlsx"){
          intermediate<-read.xlsx(input$file1$datapath)}
        else if (substrRight(input$file1$datapath, 4)==".dta"){
          intermediate<-read.dta(input$file1$datapath)}
        else if (substrRight(input$file1$datapath, 9)==".sas7bdat"){
          intermediate<-read_sas(input$file1$datapath)}
        else {stop("Please upload a csv, xlsx, dta, or sas7bdat file")}
        intermediate})
      
      crosswalk_name<-reactive({
        #Uploading a data file that doesn't have a valid identifier:
        validate(need((("NCES_Identifier" %in% names(intermediate()))|("Internal_School_Identifier" %in% names(intermediate())&"Internal_District_Identifier" %in% names(intermediate()))), "Please upload a different file with a column titled 'NCES_identifier' or two columns titled `Internal_District_Identifier` and `Internal_School_Identifier`."))
        if((!"NCES_Identifier" %in% names(intermediate()))){
          crosswalk_name<-"Internal_Identifier"
        } else{
          crosswalk_name<-"NCES_Identifier"
        }
        crosswalk_name
      })
      
      df_in<-reactive({
        #Uploading a data file that doesn't have a proper identifier:
        validate(need((("NCES_Identifier" %in% names(intermediate()))|("Internal_School_Identifier" %in% names(intermediate())&"Internal_District_Identifier" %in% names(intermediate()))), "Please upload a different file with a column titled 'NCES_Identifier' or two columns titled `Internal_District_Identifier` and `Internal_School_Identifier`."))
        
        intermediate_2<-intermediate()
        
        if(crosswalk_name()=="NCES_Identifier"){
        #adding leading 0s if necessary to NCES_identifier in intermediate
          intermediate_2$NCES_Identifier<-sprintf("%012s", as.character(intermediate_2$NCES_Identifier))}
        else if(crosswalk_name()=="Internal_Identifier"){
          intermediate_2<-intermediate_2 %>% mutate(Internal_Identifier=paste0(Internal_District_Identifier, Internal_School_Identifier))
        }
        intermediate_2
        
      })
      
      df <- reactive ({
        #Uploading a data file that has multiple observations per school
        validate(need(length(df_in()[[crosswalk_name()]])<=length(unique(df_in()[[crosswalk_name()]])), "Please upload a file that has only one row for each school. Your file upload has multiple rows per school"))
        
        df_in() %>% filter(df_in()[[crosswalk_name()]] %in% points[[crosswalk_name()]])
      })
      
      #generating a list of variable names
      choices <- reactive({names(df())})
      
      #generating the select list
      output$selectVar <- renderUI({
        ns <- session$ns
        selectInput(ns("variable"), "Color Variable:", choices())
      })
      colorVariable<-reactive({input$variable})
      
      #creating the section of the ui to generate the colorVariableTitle argument
      output$altTitleUi<-renderUI({
        ns <- session$ns
        textInput(ns("colorVariableTitle"), "Color Variable Title", colorVariable())
      })
      
      #creating the UI to select the colors to create the color palette
      color_names<-reactive({
        validate(need(!((sum(nchar(gsub("[0-9.-]", "", paste0(df()[[colorVariable()]]))))!=0) & (input$contOrCat=="Continuous")),
                      "Error: You selected a variable containing non-numeric characters, which can not be continuous. Please select discrete or edit your variable to remove non-numeric characters."))
        if(input$contOrCat=="Continuous"){
          color_names <- paste0("color", seq_len(2))
        } else if (input$contOrCat=="Discrete"){
          #creating length(popupNames) color inputs
          color_names <- paste0("color", seq_len(length(levels(as.factor(df()[[input$variable]])))))
        }
        color_names
      })
      
      vartypetext<-reactive({ 
        if(input$contOrCat=="Continuous"){
          vartypetext<- "continuous"
        } else if (input$contOrCat=="Discrete"){
          vartypetext<- "discrete"
        }
        vartypetext})
      
      output$varTypeText<-renderUI({
        ns <- session$ns
        column(width=12, tags$h4(paste0("Which colors should be used to visualize the ", vartypetext(), " color variable?")))
      })
      
      #providing text for the color selections
      
      color_labels<-reactive({  
        if(input$contOrCat=="Continuous"){
          color_labels <- as.character(c(paste0("Minimum: ", min(df()[[input$variable]])), paste0("Maximum: ", max(df()[[input$variable]]))))
        } else if (input$contOrCat=="Discrete"){
          #creating length(popupNames) color inputs
          color_labels <- levels(as.factor(df()[[input$variable]]))
        }
        color_labels
      })
      
      output$colorPaletteInputUI <- renderUI({
        ns <- session$ns
        validate(need(color_names(), ""))
        
        map2(color_labels(), color_names(), ~ column(width=3, tagList(  
          #surpress warnings            
          tags$style(type="text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"),
          tags$h5(renderText(.x)), colourInput(ns(.y), NULL, value = isolate(input[[.y]])))))
      })
      
      colorPalette<-reactive({  
        if(input$contOrCat=="Continuous"){
          #creating the colorPalette argument
          colorPalette<-colorNumeric(map_chr(color_names(), ~ input[[.x]] %||% ""), df()[[colorVariable()]])
        } else if (input$contOrCat=="Discrete"){
          colorPalette<-colorFactor(palette = map_chr(color_names(), ~ input[[.x]] %||% ""), domain = df()[[colorVariable()]])
        }
        colorPalette
      })
      
      #creating the UI section to render the popupVector argument
      output$selectPopupVars <- renderUI({
        ns <- session$ns
        
        selectizeInput(
          ns("popup_variables"), NULL, choices(), multiple = TRUE,
          options = list(
            placeholder = 'Not Required',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
        
        #selectInput(ns("popup_variables"), NULL, choices(), multiple = TRUE)
      })
      
      #creating length(popupNames) text inputs
      col_names <- reactive(paste0("col", seq_len(length(input$popup_variables))))
      output$popupNameUI <- renderUI({
        ns <- session$ns
        map2(col_names(), input$popup_variables, ~ column(width=4, tags$h5(renderText(.y)), textInput(ns(.x), NULL, value = isolate(input[[.x]]))))
      })
      
      #creating the UI section to render the sizeVariable argument
      output$selectSizeVar <- renderUI({
        ns <- session$ns
        
        selectizeInput(
          ns("sizeVariable"), "Size Variable:", choices = choices(),
          options = list(
            placeholder = 'Not Required',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      })
      
      #prevents renderLeaflet from running when sizevariable is incorrect
      sizeVariableReactive<-reactive({validate(need((sum(nchar(gsub("[0-9.-]", "", paste0(df()[[input$sizeVariable]]))))==0), "")) 
        input$sizeVariable})
      
      #create a size variable warning message
      output$sizevarwarnUI<-renderUI({
        ns <- session$ns
        validate(need(!(sum(nchar(gsub("[0-9.-]", "", paste0(df()[[input$sizeVariable]]))))==0), "")) 
        tagList(tags$style("color: #e0e0d1;"),
        column(width=6, 
        tags$h4("Error: You selected a size variable containing non-numeric characters. Please 
                edit your variable to remove non-numeric characters.")))
        
      })
      
      #show the user their data
      output$testtable1<-renderTable({head(df())})
      
      #display the data upload and trigger a warning when incorrect districts are listed
      output$section2_1ui<-renderUI({
        ns <- session$ns
        validate(need(!is.null(df()), ""))
        tagList(
          fluidRow(style= "background-color: #B5C093; color: #66609F",
                   tags$br(),
                   column(width=10, tags$h4("Your upload was successful! Here are the first six rows."),
                          offset=1)),
          fluidRow(style= "background-color: #B5C093;",
          column(width=10, style="overflow: auto; height: 300px;", withSpinner(tableOutput(ns("testtable1")),type=6, color ="#66609F"), offset=1)
        ))})
      output$section2_2ui<-renderUI({
        ns <- session$ns
        validate(need(!is.null(df()), "")) %then%
          need(length((setdiff(df_in()[[crosswalk_name()]], points[[crosswalk_name()]]))>0), "")   
        tagList(
          fluidRow(style="background-color: #B5C093;",
            column(width=10, 
                   tags$h4(paste0("Warning: the following schools can not be spatially represented on a map either because they do not correspond with a specific location in Missouri or because they do not exist. ",
                                  
                                  #printing a list of districts that couldn't be mapped, capping out at 100...
                                  paste(paste(setdiff(df_in()[[crosswalk_name()]], points[[crosswalk_name()]])[1:min(100,length(setdiff(df_in()[[crosswalk_name()]], points[[crosswalk_name()]])))], collapse=", "), ifelse(length(setdiff(df_in()[[crosswalk_name()]], points[[crosswalk_name()]]))>100, "...", ""))))
                   
                   ,style="overflow: auto; height: 50px;"
                   , offset=1))
        )})
      
      popupVectorNames<-reactive({map_chr(col_names(), ~ input[[.x]] %||% "")})
      
      subset<-reactive({ifelse(is.na(df()[[input$sizeVariable]]) | is.na(df()[[input$variable]]), FALSE, TRUE)})
      
      l_map<-eventReactive(input$button, 
                           
      {
      if(!is.null(input$popup_variables)){
        appSchoolMapper(schoolData=df(), colorVariable=input$variable,
                        colorVariableTitle=input$colorVariableTitle, 
                        colorPalette=colorPalette(), 
                        popupVectorVariables=input$popup_variables,
                        popupVectorNames=popupVectorNames(), subset=subset(),
                        sizeVariable=sizeVariableReactive(), schools_pts=points,
                        crosswalk_name=crosswalk_name())
      } else {
        appSchoolMapper(schoolData=df(), colorVariable=input$variable,
                        colorVariableTitle=input$colorVariableTitle, 
                        colorPalette=colorPalette(), subset=NULL,
                        sizeVariable=sizeVariableReactive(), schools_pts=points,
                        crosswalk_name=crosswalk_name())
      }

      })
      
      output$leaflet_in_app<-renderLeaflet({
        
        validate(need(input$button, ""))
        l_map()
      })
      
      #configuring the html map widget output
      output$downloadReport <- downloadHandler(
        filename = paste0('School Leaflet Map_', 
                          substr(gsub(" ","_",gsub(":","",gsub("-","",Sys.time()))),1,nchar(gsub(" ","_",gsub(":","",gsub("-","",Sys.time()))))-2),
                          ".html"),
        content = function(file) {
          
          shiny::withProgress(
            message = "Downloading Map...",
            detail = "This should take around 60 seconds.",
            value = 0,
            {
              incProgress(.1)
              Sys.sleep(.75)
              incProgress(.4)
              saveWidget(l_map(), file=file)
              incProgress(.5)
              Sys.sleep(.2)
            }
          )
        }
      )
      
      #configuring the html map widget output
      output$downloadMapshot <- downloadHandler(
        filename = paste0('School Map Screenshot_', 
                          substr(gsub(" ","_",gsub(":","",gsub("-","",Sys.time()))),1,nchar(gsub(" ","_",gsub(":","",gsub("-","",Sys.time()))))-2),
                          ".png"),
        content = function(file) {
          
          shiny::withProgress(
            message = "Downloading Screenshot...",
            detail = "This should take around 60 seconds.",
            value = 0,
            {
              incProgress(.2)
              mapshot(l_map(), file=file)
              incProgress(.8)
              Sys.sleep(.2)
            }
          )
        }
      )
      
      output$reportDownloaderUI<-renderUI({
        ns <- session$ns
        validate(need(input$button, ""))
        tagList(downloadButton(ns('downloadReport'), 'Download Map'), tags$br(), tags$br(), 
                downloadButton(ns('downloadMapshot'), 'Download Map Screenshot'))
      })
      
    }
  )
}