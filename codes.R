# getwd()
setwd("/home/yqgong/shiny")
# install.packages("tidyverse")
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("sass")
# install.packages("shinybusy")
# devtools::install_github("Zaoqu-Liu/BioEnricher")
# clusterProfiler：用于功能富集分析和基因集富集分析。支持GO、KEGG、Reactome等数据库。
library(pathview)
data(gse16873.d)
pv.out <- pathview(gene.data = gse16873.d[, 1], pathway.id = "hsa04110",
                   species = "hsa",kegg.native = TRUE,)
write.csv(gse16873.d,"hello.csv")
