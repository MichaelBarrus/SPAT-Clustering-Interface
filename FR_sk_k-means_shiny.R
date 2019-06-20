library(shiny)
library(ggplot2)
library(DT)

data = read.csv("FR_SK_metrics.csv")

data <- data.frame(data)

data <- na.omit(data)

# Define UI for application
ui <- pageWithSidebar(
  headerPanel('SPAT T3: K-means with Fraser sockeye'),
  sidebarPanel(
    selectInput('xcol', 'X Variable',  
                c("Recent Total" = "Recent.Total",
                                         "Lower Ratio" = "Lower.Ratio", 
                                         "Upper Ratio" = "Upper.Ratio", 
                                         "Long-Term Ratio" = "LongTerm.Ratio", 
                                         "Short-Term Trend" = "ShortTerm.Trend", 
                                         "Recent ER" = "Recent.ER"),
                selected = "Lower.Ratio"),
    selectInput('ycol', 'Y Variable', 
                c("Recent Total" = "Recent.Total",
                                        "Lower Ratio" = "Lower.Ratio", 
                                        "Upper Ratio" = "Upper.Ratio", 
                                        "Long-Term Ratio" = "LongTerm.Ratio", 
                                        "Short-Term Trend" = "ShortTerm.Trend", 
                                        "Recent ER" = "Recent.ER"),
                selected = "Recent.ER"),
    sliderInput('clusters', 'Cluster count', min = 2, max = 9, value = 2, step = 1, round = TRUE)
  ),
  mainPanel(
    plotOutput('plot1'), 
    DT::dataTableOutput("mytable")
  )
)

server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    data[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  # Extract the vector of cluster assignments from the model
  clust_vector <- reactive({clusters$cluster
  })
  
  # Build the segmented data frame
  segmented_data <- reactive({
  mutate_all(selectedData, cluster = clust_vector)
  })
  
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  #datatable
  output$mytable = DT::renderDataTable({
    data
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)