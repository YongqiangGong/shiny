
library(shiny)
library(tidyverse)
library(PubMedWordcloud)
library(DT)
library(bslib)
library(shinybusy)
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(bootswatch = "journal"),
  h3("Pubmed Hotspot"),
  add_busy_gif(src = "https://jeroen.github.io/images/banana.gif", height = 70, width = 70),  # 添加加载动画
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width=4,
      column(6,
             textInput(inputId = "keys",label = "Keys",value = "breast cancer")),
      column(6,
             textInput(inputId = "journal",label = "Journal Name",value = "science")),
      sliderInput("time", label = h3("Time"), min = 1990, 
                  max = 2024, value = c(2009, 2010)),
      
      textInput(inputId = "number",label = "Max Number",value = "10000"),
      
      tags$div(style = "text-align: center;", 
               actionButton("submit_button", "Submit")),
      
      br(),
      h5("开发者：龚永强"),
      br(),
      h5("单位：南开大学医学院")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      helpText("分析速度较慢，请等待！"),
      h4("Wordcloud:"),
      plotOutput("Plot"),
      br(),
      tags$div(style = "text-align: center;", 
               downloadButton("download_table", "Download Table")),
      h4("Results:"),
      dataTableOutput("result")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  pmids <- reactive({
    PubMedWordcloud::getPMIDsByKeyWords(keys=input$keys,journal=input$journal,dFrom=input$time[1],dTo=input$time[2],n=input$number,https = T)
    
  })
  abs2 <- reactive({
    PubMedWordcloud::getAbstracts(pmids())
  })
  abs3 <- reactive({
    PubMedWordcloud::cleanAbstracts(abs2())
  })
  observeEvent(input$submit_button,{
    show_modal_spinner(spin = "cube-grid", text = "Processing...")
    on.exit(remove_modal_spinner()) # 隐藏加载动画 
    
    output$Plot <- renderPlot({
      color2=PubMedWordcloud::colSets(type="Paired")
      plotWordCloud(abs3(),min.freq = 2,colors=color2)
      
      
    })
    output$result <- renderDataTable({
      datatable(abs3())
    })
    
    
  })
  output$download_table <- downloadHandler(
    filename = function() {
      "Results_wordcloud.csv"
    },
    content = function(file) {
      write_csv(abs3(),file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)