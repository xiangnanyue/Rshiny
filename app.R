#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Iris Data K-Means Illustration"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("clusters",
                     "Number of clusters (Value of K):",
                     min = 1,
                     max = 10,
                     value = 3),
         
         selectInput(inputId = "mode",
                     label = "select illustration data",
                     choices = list("iris", "mixed gaussian"),
                     selected = "iris")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
     if (input$mode == "iris") {
         # generate clusters based on input$clusters from ui.R
         x    <- iris[, 3:4] 
         irisCluster <- kmeans(x, centers = input$clusters, iter.max = 10, trace = TRUE)
         irisCluster$cluster <- as.factor(irisCluster$cluster)
         # draw the histogram with the specified number of bins
         ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()
     }
    else {
        N <- 1000
        p <- 2  ## we want p-dimensional multivariate normal
        set.seed(0)
        X <- matrix(runif(p * p), p, p) 
        X2 <- matrix(runif(p * p), p, p) 
        COV <- crossprod(X) # get a covariance matrix
        COV2 <- crossprod(X2)
        mu1 <- rep(0, p)
        mu2 <- rep(3, p)
        library(MASS)   ## no need to install
        x <- mvrnorm(N/2, mu, COV)
        x2 <- mvrnorm(N/2, mu2, COV2)
        # generate samples according to input
        #components <- sample(1:2, prob=c(0.5,0.5), size=N, replace=TRUE)
        #mus <- matrix(c(mu1, mu2), p, p, byrow = TRUE)
        #sds <- matrix(c(COV, COV2), 2*p, p, byrow = TRUE)
        x <- matrix(c(x, x2), N, p, byrow = TRUE)
      
        gaussianCluster <- kmeans(x, centers = input$clusters, iter.max = 10, trace = FALSE)
        gaussianCluster$cluster <- as.factor(gaussianCluster$cluster)
        # draw the histogram with the specified number of clusters
        ggplot(data.frame(x), aes(X1, X2, color = gaussianCluster$cluster)) + geom_point()
     }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

