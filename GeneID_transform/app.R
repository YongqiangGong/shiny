library(shiny)
library(tidyverse)
library(AnnotationDbi)
library(org.Hs.eg.db)
library(DT)
library(bslib)
library(shinybusy)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "journal"),
  # Somewhere in UI
  add_busy_gif(src = "https://jeroen.github.io/images/banana.gif", height = 70, width = 70),  # 添加加载动画
  # Application title
  h3("Gene-ID Transform"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput(inputId = "gene", label = h4("Gene name"), value = "TP53 APC2 TTLL6"),
      selectInput("keytype", label = h4("Input Type"), 
                  choices = list("Symbol" = "SYMBOL", "Ensembl" = "ENSEMBL", "Entrezid" = "ENTREZID"), 
                  selected = 1),
      checkboxGroupInput("columns", label = h4("Output Type"), 
                         choices = list("Symbol" = "SYMBOL", "Ensembl" = "ENSEMBL", "Entrezid" = "ENTREZID",
                                        "Gene Name" = "GENENAME"),
                         selected = c("ENSEMBL", "ENTREZID", "GENENAME")),
      tags$div(style = "text-align: center;", 
               actionButton("submit_button", "Submit")),
      br(),
      tags$div(style = "text-align: center;", 
               downloadButton("download_data", "Download")),
      br(),
      h5("Developer: Yongqiang Gong"),
      h5("Institution: School of Medicine, Nankai University")
    ),
    # Show a plot of the generated distribution "ENSEMBL", "ENTREZID"
    mainPanel(
      width = 9,
      h4("Results:"),
      dataTableOutput("result")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$submit_button, {
    show_modal_spinner(spin = "cube-grid", text = "Processing...") # 显示加载动画
    output$result <- renderDataTable({
      gene <- unlist(strsplit(input$gene, ",|\\s+"))
      p <- AnnotationDbi::select(x = org.Hs.eg.db::org.Hs.eg.db, keys = gene,
                                 columns = input$columns,
                                 keytype = input$keytype)
      datatable(p, class = "display")
    })
    on.exit(remove_modal_spinner()) # 隐藏加载动画
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      "gene_data.csv"
    },
    content = function(file) {
      gene <- unlist(strsplit(input$gene, ",|\\s+"))
      p <- AnnotationDbi::select(x = org.Hs.eg.db::org.Hs.eg.db, keys = gene,
                                 columns = input$columns,
                                 keytype = input$keytype)
      write_csv(p, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
