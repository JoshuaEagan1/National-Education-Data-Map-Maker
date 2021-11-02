if (interactive()) {
  library(shiny)
  library(rhandsontable)
  library(shinyjs)
  ui <- fluidPage(
    useShinyjs(),
    dataInputUI("input1"),
    dataFilterUI("filter1"),
    rHandsontableOutput("data1")
  )
  server <- function(input,
                     output,
                     session) {
    data_input <- dataInputServer("input1")
    # list with slots data and rows (indices)
    data_filter <- dataFilterServer("filter1",
                                    data = data_input
    )
    output$data1 <- renderRHandsontable({
      if (!is.null(data_filter$data())) {
        rhandsontable(data_filter$data())
      } else if (!is.null(data_input())){
        rhandsontable(data_input())
        }
    })
  }
  
  dataOutputServer("output1",
                   data = data_input1
  )
  
  
  shinyApp(ui, server)
}