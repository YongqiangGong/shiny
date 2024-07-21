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
library(DT)
library(clusterProfiler)
library(enrichplot)
library(DOSE)
library(org.Hs.eg.db)
library(AnnotationDbi)
library(bslib)
library(shinybusy)
# Define UI for application that draws a histogram
ui <- fluidPage(
  add_busy_gif(src = "https://jeroen.github.io/images/banana.gif", height = 70, width = 70),  # 添加加载动画
  
  theme = bs_theme(bootswatch = "journal"),
  h3("GO/KEGG Enrichment Analysis"),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
              column(6,selectInput("type", label = "Type", 
                                   choices = list("GO" = "GO", "KEGG" = "KEGG"), 
                                   selected = "GO")
                     ),
              
              column(6,radioButtons("plottype", label = "Plot",
                                  choices = list("柱状图" = "a", "气泡图" = "b"), 
                                  selected = "a"))),
              
              textInput("gene", label = "Gene", value = "ABI3BP	ACTA2	ADAM12	ANPEP	APLP1	AREG	BASP1	BDNF	BGN	BMP1	CADM1	CALD1	CALU	CAP2	CAPG	CCN1	CCN2	CD44	CD59	CDH11	CDH2	CDH6	COL11A1	COL12A1	COL16A1	COL1A1	COL1A2	COL3A1	COL4A1	COL4A2	COL5A1	COL5A2	COL5A3	COL6A2	COL6A3	COL7A1	COL8A2	COLGALT1	COMP	COPA	CRLF1	CTHRC1	CXCL1	CXCL12	CXCL6	CXCL8	DAB2	DCN	DKK1	DPYSL3	DST	ECM1	ECM2	EDIL3	EFEMP2	ELN	EMP3	ENO2	FAP	FAS	FBLN1	FBLN2	FBLN5	FBN1	FBN2	FERMT2	FGF2	FLNA	FMOD	FN1	FOXC2	FSTL1	FSTL3	FUCA1	FZD8	GADD45A	GADD45B	GAS1	GEM	GJA1	GLIPR1	GPC1	GPX7	GREM1	HTRA1	ID2	IGFBP2	IGFBP3	IGFBP4	IL15	IL32	IL6	INHBA	ITGA2	ITGA5	ITGAV	ITGB1	ITGB3	ITGB5	JUN	LAMA1	LAMA2	LAMA3	LAMC1	LAMC2	LGALS1	LOX	LOXL1	LOXL2	LRP1	LRRC15	LUM	MAGEE1	MATN2	MATN3	MCM7	MEST	MFAP5	MGP	MMP1	MMP14	MMP2	MMP3	MSX1	MXRA5	MYL9	MYLK	NID2	NNMT	NOTCH2	NT5E	NTM	OXTR	P3H1	PCOLCE	PCOLCE2	PDGFRB	PDLIM4	PFN2	PLAUR	PLOD1	PLOD2	PLOD3	PMEPA1	PMP22	POSTN	PPIB	PRRX1	PRSS2	PTHLH	PTX3	PVR	QSOX1	RGS4	RHOB	SAT1	SCG2	SDC1	SDC4	SERPINE1	SERPINE2	SERPINH1	SFRP1	SFRP4	SGCB	SGCD	SGCG	SLC6A8	SLIT2	SLIT3	SNAI2	SNTB1	SPARC	SPOCK1	SPP1	TAGLN	TFPI2	TGFB1	TGFBI	TGFBR3	TGM2	THBS1	THBS2	THY1	TIMP1	TIMP3	TNC	TNFAIP3	TNFRSF11B	TNFRSF12A	TPM1	TPM2	TPM4	VCAM1	VCAN	VEGFA	VEGFC	VIM	WIPF1	WNT5A
"),
              
                     sliderInput("number", label = h3("Number"), min = 0, 
                                 max = 30, value = 10)
                     ,
              tags$div(style = "text-align: center;", 
                       actionButton("submit_button", "Submit")),
              br(),
              br(),
              br(),
            fluidRow(
              column(6, 
                       downloadButton("download_table", "Download Table")),
              column(6, 
                       downloadButton("download_plot", "Download Plot")))
              
            ,
            br(),
            br(),
            br(),
            h5("Developer: Yongqiang Gong"),
            h5("Institution: School of Medicine, Nankai University")
        ),

        
        mainPanel(
          h4("Results:"),
          dataTableOutput("result"),
          h4("Plot:"),
          plotOutput("Plot",height = "1000px", width = "1000px"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  gene1 <- reactive({unlist(strsplit(input$gene, ",|\\s+"))})
  gene2 <- reactive({
    AnnotationDbi::select(x = org.Hs.eg.db::org.Hs.eg.db,keys = gene1(),
                          columns = "ENTREZID" ,
                          keytype ="SYMBOL" )
  })
  gene <- reactive({
    gene2()$ENTREZID
  })
  GO1 <- reactive({
    enrichGO(gene = gene(), #需要分析的基因的EntrezID
             OrgDb = org.Hs.eg.db, #人基因数据库
             pvalueCutoff =0.05, #设置pvalue界值
             qvalueCutoff = 0.05, #设置qvalue界值(FDR校正后的p值）
             ont="all", #选择功能富集的类型，可选BP、MF、CC，这里选择all。
             readable =T)})
  GO2 <- reactive({
    setReadable(GO1(),
                OrgDb = org.Hs.eg.db,
                keyType = "ENTREZID")
  })
  GO <- reactive({
    data.frame(GO2()@result)
  })
  
  KEGG1 <- reactive({
    enrichKEGG(gene = gene(),
               organism = "hsa", #物种Homo sapiens (智人)
               pvalueCutoff = 0.05,
               qvalueCutoff = 0.05,
               pAdjustMethod = "BH",
               minGSSize = 10,
               maxGSSize = 500)
  })
  KEGG2 <- reactive({
    setReadable(KEGG1(),
                OrgDb = org.Hs.eg.db,
                keyType = "ENTREZID")
  })
  KEGG <- reactive({
    data.frame(KEGG2()@result)
  })
  observeEvent(input$submit_button, {
    show_modal_spinner(spin = "cube-grid", text = "Processing...")
    on.exit(remove_modal_spinner()) # 隐藏加载动画 
    output$result <- renderDataTable({
      table_type <- input$type
      switch(
        table_type,
        "GO"=datatable(GO()),
        "KEGG"=datatable(KEGG())
      )
    })
    output$Plot <- renderPlot({
      plot <- input$plottype
      table_type <- input$type
      if (table_type == "GO") {
        switch(
          plot,
          "a"=barplot(GO1(), x = "GeneRatio", color = "p.adjust", #默认参数（x和color可以根据eG里面的内容更改）
                      showCategory =input$number, #只显示前10
                      split="ONTOLOGY") + #以ONTOLOGY类型分开
            facet_grid(ONTOLOGY~., scale='free') ,
          "b"=dotplot(GO1(),showCategory=input$number,orderBy="GeneRatio",split="ONTOLOGY", color = "p.adjust") + facet_grid(ONTOLOGY~., scale='free')
        )
      } else if (table_type == "KEGG") {
        switch(
          plot,
          "a"=barplot(
            KEGG1(),
            x = "GeneRatio", #or "GeneRatio"
            color = "pvalue", #or "p.adjust", "qvalue"
            showCategory = input$number, #显示pathway的数量
            font.size = 12, #字号
            title = "KEGG enrichment barplot", #标题
            label_format = 30 #pathway标签长度超过30个字符串换行
          ) ,
          "b"=dotplot(
            KEGG1(),
            x = "GeneRatio",
            color = "p.adjust",
            showCategory = input$number,
            font.size = 12,
            title = "KEGG Pathway Enrichment",
            label_format = 30
          )
        )
      }
    })
  })
  output$download_table <- downloadHandler(filename = function(){
    "Enrichment_annalysis_table.csv"
  },
  content=function(file){
    table_type <- input$type
    switch(
      table_type,
      "GO"=write_csv(GO(),file),
      "KEGG"=write_csv(GO(),file)
    )
  })
  output$download_plot <- downloadHandler(filename = function(){
    "Enrichment_analysis_plot.png"
  },
  content=function(file){
    # Save the plot based on the input$type and input$plottype
    table_type <- input$type
    plot_type <- input$plottype
    if (table_type == "GO") {
      if (plot_type == "a") {
        p <- barplot(GO1(), x = "GeneRatio", color = "p.adjust", font.size = 18,
                showCategory = input$number, split="ONTOLOGY") + 
          facet_grid(ONTOLOGY~., scale='free')
        png(file, width = 10000, height = 10000,res = 600)
        print(p)
        dev.off()
      } else if (plot_type == "b") {
        
        p <- dotplot(GO1(), showCategory = input$number, font.size = 18,
                orderBy = "GeneRatio", split = "ONTOLOGY", color = "p.adjust") + 
          facet_grid(ONTOLOGY~., scale='free')
        png(file, width = 10000, height = 10000,res = 600)
        print(p)
        dev.off()
      }
    } else if (table_type == "KEGG") {
      if (plot_type == "a") {
        
        p <- barplot(KEGG1(), x = "GeneRatio", color = "p.adjust", 
                showCategory = input$number, font.size = 18, 
                title = "KEGG enrichment barplot", label_format = 30)
        png(file, width = 10000, height = 10000,res = 600)
        print(p)
        dev.off()
      } else if (plot_type == "b") {
        
        p <- dotplot(KEGG1(), x = "GeneRatio", color = "p.adjust", 
                showCategory = input$number, font.size = 18, 
                title = "KEGG Pathway Enrichment", label_format = 30)
        png(file, width = 10000, height = 10000,res = 600)
        print(p)
        dev.off()
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
