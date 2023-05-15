# basic-machine-learning
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("shiny")

# Load necessary libraries
library(caret)
library(ggplot2)
library(shiny)

# Load iris dataset
data(iris)

# Define UI
ui <- fluidPage(
  titlePanel("Iris Species Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("feature1", "Select first feature:", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
      selectInput("feature2", "Select second feature:", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
    ),
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)

# Define server
server <- function(input, output) {
  # Split dataset into training and testing sets
  set.seed(123)
  training.samples <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
  train.data <- iris[training.samples, ]
  test.data <- iris[-training.samples, ]
  
  # Train model using decision tree algorithm
  model <- train(Species ~ ., data = train.data, method = "rpart")
  
  # Create scatterplot of selected features, colored by species
  output$scatterplot <- renderPlot({
    ggplot(data = iris, aes_string(x = input$feature1, y = input$feature2, color = "Species")) +
      geom_point() +
      labs(title = paste(input$feature1, "vs.", input$feature2),
           x = input$feature1, y = input$feature2)
  })
}

# Run app
shinyApp(ui = ui, server = server)
