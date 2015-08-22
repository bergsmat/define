library(shiny)
shinyApp(
  server = function(input, output) {
    value <- "xoxoxo"
    output$ui <- renderUI({
      value <<- input$txt_0
      textInput("txt_0", "text", value)
    })
    output$val <- renderText(value)
  },
  ui = fluidPage( 
    fluidRow( column( 10, uiOutput("ui"))),
    fluidRow(column(10, textOutput('val'))))
)
