#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggvenn)
library(ggsci)
library(bslib)
library(shinybusy)
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(bootswatch = "journal"),
  add_busy_gif(src = "https://jeroen.github.io/images/banana.gif", height = 70, width = 70),  # 添加加载动画
  h3("Venn Diagram"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6,sliderInput("Setsize", label = "Set size", min = 0, 
                             max = 20, value =5 )),
        column(6,sliderInput("titlesize", label = "Title size", min = 0, 
                             max = 15, value = 10)),
        column(6,
               textInput("Set1", label = "Set 1", value = "Gene1 Gene2  Gene3  Gene4  Gene5  Gene6  Gene7  Gene8  Gene9  
                 Gene10 Gene11 Gene12 Gene13 Gene14 Gene15 Gene16")),
        column(6,
               textInput("title1", label = "title 1", value = "Set 1")),
        
        column(6,  textInput("Set2", label = "Set 2", value = "Gene1 Gene2  Gene3  Gene4  Gene5  Gene6  Gene7  Gene8  Gene9  
                 Gene10 Gene11 Gene12 Gene13 Gene14 Gene15 Gene16")),
        
        column(6, textInput("title2", label = "title 2", value = "Set 2")),
        
        column(6, textInput("Set3", label = "Set 3", value = "")),
        
        column(6,textInput("title3", label = "title 3", value = "")),
        
        column(6, textInput("Set4", label = "Set 4", value = "")),
        
        column(6,textInput("title4", label = "title 4", value = "")),
        
        tags$div(style = "text-align: center;", 
                 actionButton("submit_button", "Submit")),
        br(),
        br(),
      ),
      br(),
      ),
    
    mainPanel(
      h4("Venn Diagram:"),
      tags$div(style = "text-align: center;", 
               downloadButton("download", "Download")),
      br(),
      plotOutput("Plot",height = "1000px", width = "1000px"),
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$submit_button,{
    show_modal_spinner(spin = "cube-grid", text = "Processing...")
    set1 <- unlist(strsplit(input$Set1, ",|\\s+"))
    set2 <- unlist(strsplit(input$Set2, ",|\\s+"))
    set3 <- unlist(strsplit(input$Set3, ",|\\s+"))
    set4 <- unlist(strsplit(input$Set4, ",|\\s+"))
    
    # 创建一个空列表
    data <- list()
    
    # 添加键值对，其中键是输入的标题，值是对应的基因集合向量
    if (input$title1 != "") {
      data[[input$title1]] <- set1
    }
    if (input$title2 != "") {
      data[[input$title2]] <- set2
    }
    if (input$title3 != "") {
      data[[input$title3]] <- set3
    }
    if (input$title4 != "") {
      data[[input$title4]] <- set4
    }
    
    output$Plot <- renderPlot({
      ggvenn::ggvenn(
        data = data,
        columns = NULL,
        show_elements = FALSE,
        show_percentage = TRUE,
        digits = 1,
        fill_alpha = 0.5,
        stroke_color = "black",
        stroke_alpha = 1,
        stroke_size = 1,
        stroke_linetype = "solid",
        set_name_color = "black",
        set_name_size = input$titlesize,
        text_color = "black",
        text_size = input$Setsize,
        label_sep = ",",
        count_column = NULL,
        show_outside = "auto",
        auto_scale = FALSE
      ) +
        scale_fill_lancet()
    })
    output$download <- downloadHandler(
      filename = function(){
        "Venn_Diagram.png"
      },
      content = function(file){
        set1 <- unlist(strsplit(input$Set1, ",|\\s+"))
        set2 <- unlist(strsplit(input$Set2, ",|\\s+"))
        set3 <- unlist(strsplit(input$Set3, ",|\\s+"))
        set4 <- unlist(strsplit(input$Set4, ",|\\s+"))
        
        # 创建一个空列表
        data <- list()
        
        # 添加键值对，其中键是输入的标题，值是对应的基因集合向量
        if (input$title1 != "") {
          data[[input$title1]] <- set1
        }
        if (input$title2 != "") {
          data[[input$title2]] <- set2
        }
        if (input$title3 != "") {
          data[[input$title3]] <- set3
        }
        if (input$title4 != "") {
          data[[input$title4]] <- set4
        }
        
        p <- ggvenn::ggvenn(
          data = data,
          columns = NULL,
          show_elements = FALSE,
          show_percentage = TRUE,
          digits = 1,
          fill_alpha = 0.5,
          stroke_color = "black",
          stroke_alpha = 1,
          stroke_size = 1,
          stroke_linetype = "solid",
          set_name_color = "black",
          set_name_size = input$titlesize,
          text_color = "black",
          text_size = input$Setsize,
          label_sep = ",",
          count_column = NULL,
          show_outside = "auto",
          auto_scale = FALSE
        ) +
          scale_fill_lancet()
        
        # 保存绘图到文件
        png(file, width = 10000, height = 10000,res = 600)
        print(p)
        dev.off()
      }
    )
    on.exit(remove_modal_spinner()) # 隐藏加载动画 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
