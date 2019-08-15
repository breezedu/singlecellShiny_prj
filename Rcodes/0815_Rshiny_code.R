


library(shiny)

library(umap)
library(plotly)
library(shinycssloaders)

# install.packages("scatterplot3d")
library(scatterplot3d)

library(ggplot2)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("scRNA Data visulization"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the project ID to display umap plots ----
      radioButtons("project", "Project IDs:",
                   c("sc_Project1" = "project_1",
                     "sc_Project2" = "project_1",
                     "sc_Project3" = "project_1",
                     "sc_Project4" = "project_1")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Select the random distribution type ----
      radioButtons("group", "Sample Groups:",
                   c("sc_Group1" = "Group_1",
                     "sc_Group2" = "Group_1",
                     "sc_Group3" = "Group_1",
                     "sc_Group4" = "Group_1"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(#type = "tabs",
                  tabPanel("3d_UMAP_Plot", 
                           plotOutput("plot3d")),
                  tabPanel("2d_UMAP_Plot", 
                           plotOutput("plot2d")),
                  
                  tabPanel("Box_Plot", 
                           plotOutput("plotbox")),
                  tabPanel("Violin_Plot", 
                           plotOutput("plotviolin")),
                  
                  tabPanel("Sample Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)

# Define server logic for random project app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested project ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    project <- switch(input$project,
                   prj_1 = rnorm,
                   prj_2 = runif,
                   prj_3 = rlnorm,
                   prj_4 = rexp,
                   rnorm)
    
    dist(input$group)
  })
  
  
  ## read input files from onedrive 
  setwd("C:/Users/Jeff/OneDrive - QIAGEN GmbH/RShiny_prj/singcell_prj_input/")
  
  
 
  
  # exp <- read.table("exp_DataShare2.txt", header = T, row.names = 1, sep = "\t")
  # design <- read.table("DataShare2_Design.txt", header = T, row.names = 1, sep = "\t")
  # annotation <- read.table("DataShare2_Annotation.txt", header = T, row.names = 1, sep = "\t")
  # 
  ## box.plot
  # exp[ 1:5, 1:5]
  # 
  # boxplot(t(exp[ 1:5, 1:2000])) 
  
  # load.Rdata(file.rename="scPrj.RData", all)
  # or 
  # load("scPrj.RData")
  

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
    
    umap.df <- read.table("UMAP.txt", header = T, row.names = 1, sep = "\t") 
    
    ## umap cluster plot
    #save(exp, design, annotation, umap.df, file = "scPrj.RData")

    head(umap.df)
    # umap.scatterplot
    
    
    umap.3d <- plot_ly(type="scatter3d", x = Y[,1], y = Y[,2], z = Y[,3], 
                       color = as.factor(umap.df$Classifications) )
    umap.3d
      
  }) # end output$plot3d
  
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot2d <- renderPlot({
    
    umap.df <- read.table("UMAP.txt", header = T, row.names = 1, sep = "\t") 
    
    ## umap cluster plot
    
    #save(exp, design, annotation, umap.df, file = "scPrj.RData")
    # library(umap)
    # head(umap.df)
    
    umap.scatterplot <- umap.df %>%
      mutate(Classification = umap.df$Classifications) %>%
      ggplot( aes(UMAP.2D.1, UMAP.2D.2, color = Classification)) + geom_point() 
    
    umap.scatterplot
  })
  
  ## boxplot
  output$plotbox <- renderPlot({
    
    exp_counts <- exp[1, ]
    m <- list(counts = as.numeric(exp_counts), group = as.factor(design$Age))
    m <- as.tibble(m)
    
    q <- ggplot(m, aes(group, counts, color=group)) + geom_boxplot() + geom_jitter(width = 0.1)
    q <- q + labs(x = "Age Group", y = "Single Cell Counts ", title = "Expression of A1BG")
    
    boxplot <- q
    boxplot
    
  })
  
  ## violin plot
  output$plotviolin <- renderPlot({
    
    #exp_counts <- exp[1, ]
    #m <- list(counts = as.numeric(exp_counts), group = as.factor(design$Age))
    #m <- as.tibble(m)
    
    v <- ggplot(m, aes(group, counts, color=group)) + geom_violin() + geom_jitter(width = 0.1)
    v <- v + labs(x = "Age Group", y = "Single Cell Counts ", title = "Expression of A1BG")
    
    violinplot <- q
    violinplot
    
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


# install.packages('shinydashboard')

library(shinydashboard)


ui2 <- dashboardPage(skin = 'green',
                    
                    dashboardHeader( title = "sc Test", titleWidth = 280),
                    dashboardSidebar(width = 280,  
                                     sidebarMenu(
                                       menuItem(text = "plot2d", tabName = "plot2d", icon = icon("dashboard")),
                                       menuItem(text = "plot3d", tabName = "plot3d", icon = icon("th")), 
                                       menuItem(text = "boxplot", tabName = "boxplot", icon = icon("dashboard"))
                                       
                                     )),
                    
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "plot2d",
                                fluidRow(
                                  column(5, 'umap plot 2d') ), 
                                br(), 
                                
                                fluidRow(column(width = 12, plotOutput("plot2d")
                                )
                                )),
                        # Second tab content
                        tabItem(tabName = "plot3d",
                                fluidRow(
                                  column(5,'umap plot 3d')),
                                
                                br(),
                                
                                fluidRow(column(width = 6, plotOutput("plot2d")),
                                         column(width = 6, plotOutput("plot3d"))
                                        )
                                ), 
                        # third tab content
                        tabItem(tabName = "boxplot",
                                fluidRow(
                                  column(5,'boxplot plot ')),
                                
                                br(),
                                
                                fluidRow(column(width = 6, plotOutput("plot2d")),
                                         column(width = 6, plotOutput("plot3d"))
                                ),
                                br(),
                                fluidRow(column(width = 12, plotOutput("boxplot"))
                                )
                        )
                        
                      )
                    )
)






# Create Shiny app ----
shinyApp(ui, server)

shinyApp(ui2, server)
