library(shiny)
shinyApp(
  server = function(input, output, session) {
    value <- ""
    output$ui <- renderUI({
      #browser()
      value <<- if(is.null(input$txt)) '' else input$txt
      print(value)
      textInput("txt", "text", value)
    })
  },
  ui = fluidPage(uiOutput("ui"))
)

