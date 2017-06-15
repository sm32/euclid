library(shiny)
library(data.table)

options(shiny.maxRequestSize=100*1024^2) 

shinyServer(function(input, output, session) {
  
  delimiter <- ','
  dtResult <- data.table()
  dataPath <- NULL
  exclusionPath <- NULL
  
  
  #This function is repsonsible for loading in the selected file
  baseData <- reactive({
    infile <- input$dtFilePath
    if (is.null(infile)) {
      # User has not uploaded a file yet
      dataPath <<- NULL
      return(NULL)
    }
    dataPath <<- infile$datapath
    head(data.table::fread(input=infile$datapath, sep=delimiter,stringsAsFactors = FALSE),100)
  })
  
  exclusionData <- reactive({
    infile <- input$exFilePath
    if (is.null(infile)) {
      # User has not uploaded a file yet
      exclusionPath <<- NULL
      return(NULL)
    }
    exclusionPath <<- infile$datapath
    data.table::fread(input=infile$datapath, sep=delimiter,stringsAsFactors = FALSE)
  })
  
  #This previews the CSV data file
  output$baseData <- renderTable({
    baseData()
  })
  
  output$exclusionData <- renderTable({
    exclusionData()
  })
  
  output$similarityResults <- renderTable({
    similarityResults()
    head(dtResult,100)
  })
  
  similarityResults <- eventReactive(input$getSim, {
    if (is.null(input$dtFilePath)) {
      dataPath <<- NULL
      return(NULL)
    }
    source('score_input.R')
    dt <- data.table::fread(input=dataPath, sep=delimiter,stringsAsFactors = FALSE)
    
    # Create a Progress object
    progress <- shiny::Progress$new(style = 'notification')
    progress$set(message = "Computing similarities", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a closure to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL, deno = 5) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / deno
      }
      progress$set(value = value, detail = detail)
    }
    
    dtResult <<- getCosineSimilarity(dt,exclusionData,input$topN, updateProgress)
  })
  
  observeEvent(input$getSim, {
    updateTabsetPanel(session, "tabs", selected = "Similarity")
  })
  
  observeEvent(input$dtFilePath, {
    if(is.null(input$dtFilePath)) {
      return(NULL)
    }
    updateTabsetPanel(session, "tabs", selected = "Data")
  })
  
  observeEvent(input$exFilePath, {
    if(is.null(input$exFilePath)) {
      return(NULL)
    }
    updateTabsetPanel(session, "tabs", selected = "Exclusions")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("euclid-cosim_output.csv")
    },
    content = function(file) {
      if(nrow(dtResult) > 1) {
        write.csv(dtResult,file,row.names = FALSE)  
      }
    }
  )
})