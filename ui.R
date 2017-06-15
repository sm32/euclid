library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Euclid - understand similarity better with cosine-similarity"),
  
  sidebarPanel(
    #Selector for data file upload
    fileInput('dtFilePath', 'Choose data file (CSV)*',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),

    #Selector for exclusion file upload
    fileInput('exFilePath', 'Choose exclusion file (CSV)',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
    #Slider to select top values
    sliderInput("topN", "Choose top similar values", 
                min = 1, max = 100, value = 20, step= 10),
    
    #The action button prevents an action firing before we're ready
    actionButton("getSim", "Calculate Similarity"),
    
    h6("* indicates mandatory")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Data", h6("Data Format: id, kpi, product_views, cart_adds, orders [id can be email/customer_id etc, kpi can be class/category etc]"),tableOutput("baseData")),
      tabPanel("Exclusions", h6("Data Format: kpi"), tableOutput("exclusionData")),
      tabPanel("Similarity",h6("Results Format: kpi, similar_kpi, score"),downloadButton('downloadData', 'Download'), tableOutput("similarityResults")),
      id="tabs"
    )
  )
))