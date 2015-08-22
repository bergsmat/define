library(shiny)

  getwsd <- function(){
    a <- list.files()
    dir <- file_test("-d",a)
    a[dir]
  }
  
  getfiles <- function(){
    a <- list.files()
    a <- file.path(getwd(), a)
    file <- file_test("-f",a)
    a[file]
  }
  
  token <- function(x = 0, ...){
    if(x==0) return(character(0))
    pattern <- rep('',x)
    nms <- tempfile(pattern=pattern,...)
    res <- basename(nms)
    res
  }

server <- function(input, output) {
  
 # chosen <- character(0)
  collection <- data.frame(
    stringsAsFactors=FALSE,
    chosen = character(0),
    token = character(0)
  )
  foo <- data.frame(
    stringsAsFactors=FALSE,
    chosen = character(0),
    token = character(0)
  )
  
  output$ui <- renderUI({
    
    context <- if(is.null(input$subdirs)) getwd() else input$subdirs
    setwd(context)
    
    available <- getfiles()
    
    if(!is.null(input$selectionList)) collection <<- rbind( 
      collection, 
      data.frame(
        stringsAsFactors = FALSE,
        chosen = file.path(getwd(), input$selectionList),
        token = token(1)
      )
    )
    
    if(!is.null(input$txt_1)) collection[1, 'token'] <<- input$txt_1 
    
    #chosen <<- chosen[!chosen %in% input$permaList]  # dismiss chosen files per user's action
    if(!is.null(input$permaList)) collection <<- collection[!(collection$chosen %in% input$permaList),]
    row.names(collection) <<- NULL
    
    output$cl <- renderTable(collection) # temporary reference table
    
    sidebarLayout(
      
      sidebarPanel(
        radioButtons(
          inputId="subdirs", 
          label = "Directory Navigator", 
          choices = c( 
            "..", 
            getwd(), 
            getwsd()), 
          selected = getwd()
        )
      ),
      
      mainPanel( width = 8, do.call( wellPanel, c( 
        list( 
          checkboxGroupInput(
            inputId="selectionList", 
            label="Available Files", 
            choices = basename(available[!available %in% collection$chosen])
          ),
          hr(),
          
          checkboxGroupInput(
            inputId="foo", 
            label = "foo Files", 
            choices=foo$chosen),
          checkboxGroupInput(
            inputId="permaList", 
            label = "Selected Files", 
            choices=collection$chosen),
          hr()
        ),
        lapply(
          seq_len(nrow(collection)), 
          function(i) textInput(
            inputId=paste("txt", i, sep = "_"), 
            label = collection$chosen[i], 
            collection$token[i]
          )
        )
      )))
    )
  })
}

#------------------------------------------------------------------------------------------------#

ui <- fluidPage(
  
  titlePanel("File Selection"),
  
  fluidRow(
    column(10,
      uiOutput("ui")
    )
  ),
  fluidRow(column(10,tableOutput('cl')))
)

shinyApp(server = server, ui = ui)
