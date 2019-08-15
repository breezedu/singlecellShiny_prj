


library(shiny)

library(plotly)
library(shinycssloaders)

# install.packages("scatterplot3d")
library(scatterplot3d)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("scRNA Data visulization"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the project ID to display umap plots ----
      radioButtons("dist", "Project IDs:",
                   c("sc_Project1" = "norm",
                     "sc_Project2" = "unif",
                     "sc_Project3" = "lnorm",
                     "sc_Project4" = "exp")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Select the random distribution type ----
      radioButtons("dist", "Sample Groups:",
                   c("sc_Group1" = "norm",
                     "sc_Group2" = "unif",
                     "sc_Group3" = "lnorm",
                     "sc_Group4" = "exp"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("2d_UMAP_Plot", plotOutput("plot2d")),
                  tabPanel("3d_UMAP_Plot", plotOutput("plot3d")),
                  tabPanel("Sample Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  setwd("C:/Users/Jeff/OneDrive - QIAGEN GmbH/RShiny_prj/singcell_prj_input/")
  
  
  
  # exp <- read.table("exp_DataShare2.txt", header = T, row.names = 1, sep = "\t")
  # design <- read.table("DataShare2_Design.txt", header = T, row.names = 1, sep = "\t")
  # annotation <- read.table("DataShare2_Annotation.txt", header = T, row.names = 1, sep = "\t")
  # 
  ## box.plot
  
  # load.Rdata(file.rename="scPrj.RData", all)
  # or 
  # load("scPrj.RData")
  
  umap.df <- read.table("UMAP.txt", header = T, row.names = 1, sep = "\t") 
  
  ## umap cluster plot
  
  #save(exp, design, annotation, umap.df, file = "scPrj.RData")
  
  
  library(umap)
  head(umap)
  
  umap.scatterplot <- umap.df %>%
    mutate(Classification = umap.df$Classifications) %>%
    ggplot( aes(UMAP.2D.1, UMAP.2D.2, color = Classification)) + geom_point()
  
  # umap.scatterplot
  
  
  umap.3d <- plot_ly(type="scatter3d", x = Y[,1], y = Y[,2], z = Y[,3], 
                     color = as.factor(umap.df$Classifications) )
  
  # umap.3d
  
  Y <- umap.df
  
  # Load the data
  #Y <- readRDS("./singcell_prj_input/scPrj.Rdata")
  #
  #label = Y$label
  #Y$label = NULL
  
  # Layout configuration
  #m <- list(
  #3  l = 0,
  #  r = 0,
  #  b = 0,
  #  t = 0,
  #  pad = 0
  #)
  # Axis configuration
  #ax <- list(
  #  title = "",
  #  zeroline = FALSE,
  #  showline = FALSE,
  #  showticklabels = FALSE,
  #  showgrid = FALSE,
  #  showspikes = FALSE
  #)
  # Legend configuration
  #l <- list(
  #  x = 1,
  #  y = 0.5,
  #  font = list(
  #    family = "sans-serif",
  #    size = 14,
  #    color = "#FFFFFF"
  #  ),
  #  bgcolor = "#060606",
  #  bordercolor = "#060606"
  #)
  
  output$plot3d <- renderPlotly({
    
    umap.3d
      
  }) # end output$plot3d
  
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot2d <- renderPlot({
    umap.scatterplot
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(umap.df)
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    head(umap.df)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
