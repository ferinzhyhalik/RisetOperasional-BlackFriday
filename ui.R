library(shiny)
library(shinythemes)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  theme = shinytheme("slate"),
  # App title ----
  titlePanel("BlackFriday"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("Plot", "Please Select What Plot to Show", 
                  choices = c("Normal", "Top Product", "Purchase", 
                              "Gender", "City Category", "Marital Status", 
                              "Age", "Product Category 1 Gender", "Price/Category")),
      
      sliderInput("Confidence", "Please Adjust The COnfidence",
                  min = 0.1, max = 0.9, step = 0.05, value = 0.8),
      
      sliderInput("Support", "Please Adjust The Support",
                  min = 0.006, max = 0.01, step = 0.0002, value = 0.008),
      
      conditionalPanel(condition = "input.Plot == 'Normal'",
                       # Input: Slider for the number of bins ----
                       sliderInput(inputId = "bins", label = "Number of bins:",
                                   min = 1, max = 50, value = 30)),
      
      conditionalPanel(condition ="input.Plot == 'Top Product'",
                       # Input : Slider for the number of Top Product
                       sliderInput("Product", "Number of Product:",
                                   min = 1, max = 100, value = 10, step = 1)),
      
      conditionalPanel(condition ="input.Plot == 'Purchase'",
                       # Input : Slider for the number of Top Product
                       sliderInput("User", "Number of User:",
                                   min = 1, max = 100, value = 10, step = 1)),
      
      conditionalPanel(condition ="input.Plot == 'Gender'",
                       # Input : select input the type
                       selectInput("Gender", "Which City",
                                   choices = c("All", "City A", "City B", "City C"))
      ),
      conditionalPanel(condition ="input.Plot == 'City Category'",
                       # Input : select input the type
                       selectInput("City", "Which City",
                                   choices = c("All", "Product City A", "Product City B", "Product City C"))
      ),
      conditionalPanel(condition ="input.Plot == 'City Category'",
                       # Input : select input the type
                       selectInput("MarStatus", "Which Type",
                                   choices = c("All", "Product Marital Status 1", "Product Marital Status 0", "Summary Product")),
                       sliderInput("Top_Product", "Number_of_Product", 
                                   min = 1, max = 100, value = 10, step = 1)
      ),
      conditionalPanel(condition ="input.Plot == 'Age'",
                       # Input : select input the type
                       selectInput("Age", "Which Type",
                                   choices = c("All", "All Male", "All Female"))
      )
    ),
    # Main panel for displaying outputs ----
    mainPanel(# Output: Histogram ----
              tabsetPanel(
                tabPanel("Data Exploration", plotOutput("distPlot")),
                tabPanel("Prediction Table", tableOutput("predTab")),
                tabPanel('Grouped', value='grouped', plotOutput("groupedPlot", width='100%', height='100%')),
                tabPanel('Graph', value='graph', plotOutput("graphPlot", width='100%', height='100%')),
                tabPanel('Scatter', value='scatter', plotOutput("scatterPlot", width='100%', height='100%')),
                tabPanel('Parallel Coordinates', value='paracoord', plotOutput("paracoordPlot", width='100%', height='100%')),
                tabPanel('Matrix', value='matrix', plotOutput("matrixPlot", width='100%', height='100%'))
              )
              )
  )
)