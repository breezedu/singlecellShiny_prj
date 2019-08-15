

## load single cell input data
## save all tables in one *.RData file

getwd()

dir <- "D:/WorkRecord/Companies/Qiagen_Sales/RShiny_prj/singcell_prj_input/"

setwd( dir )

getwd()


exp <- read.table("exp_DataShare2.txt", header = T, row.names = 1, sep = "\t")
design <- read.table("DataShare2_Design.txt", header = T, row.names = 1, sep = "\t")
annotation <- read.table("DataShare2_Annotation.txt", header = T, row.names = 1, sep = "\t")

## box.plot



umap.df <- read.table("UMAP.txt", header = T, row.names = 1, sep = "\t") 

## umap cluster plot

save(exp, design, annotation, umap.df, file = "scPrj.RData")


library(umap)
head(umap.df)

umap.scatterplot <- umap.df %>%
                      mutate(Classification = umap.df$Classifications) %>%
                      ggplot( aes(UMAP.2D.1, UMAP.2D.2, color = Classification)) + geom_point()

umap.scatterplot

run_umap_shiny(umap.scatterplot)
