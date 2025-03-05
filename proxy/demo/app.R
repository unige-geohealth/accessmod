# app.R
library(shiny)
library(ggplot2)

addResourcePath("x", "./www")
port <- as.integer(commandArgs(trailingOnly = TRUE)[[1]])

# Define UI
ui <- fluidPage(
  # Include external JavaScript file in the header
  tags$head(
    tags$script(src = "x/test.js")
  ),

  # App title
  titlePanel("Simple Shiny App with JS Integration"),

  # Layout with buttons
  sidebarLayout(
    sidebarPanel(
      # Button 1: Create a blocking event for 5 seconds
      actionButton("blockButton", "Block for 5 seconds"),

      # Button 2: Create a random plot
      actionButton("plotButton", "Generate Random Plot"),

      # Button 3: Non-reactive button that calls JavaScript
      tags$button(
        id = "btnTest",
        class = "btn btn-default action-button",
        "Log to Console",
      ),

      # Button 4: Restart button
      tags$button(
        id = "btnRestart",
        class = "btn btn-warning action-button",
        "Restart App",
      )
    ),

    # Main panel for displaying plot
    mainPanel(
      plotOutput("randomPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Blocking event handler
  observeEvent(input$blockButton, {
    # Show a message that we're blocking
    showNotification("Blocking for 5 seconds...", type = "message")

    # Simulate a blocking operation
    Sys.sleep(5)

    # Show a message when done
    showNotification("Blocking complete!", type = "message")
  })

  # Random plot generator
  observeEvent(input$plotButton, {
    output$randomPlot <- renderPlot({
      # Generate random data
      set.seed(as.numeric(Sys.time()))
      df <- data.frame(
        x = rnorm(100),
        y = rnorm(100),
        category = sample(letters[1:5], 100, replace = TRUE)
      )

      # Create a random plot
      plot_type <- sample(1:3, 1)

      if (plot_type == 1) {
        # Scatter plot
        ggplot(df, aes(x, y, color = category)) +
          geom_point(size = 3, alpha = 0.7) +
          theme_minimal() +
          ggtitle("Random Scatter Plot")
      } else if (plot_type == 2) {
        # Histogram
        ggplot(df, aes(x, fill = category)) +
          geom_histogram(bins = 20, alpha = 0.7) +
          theme_minimal() +
          ggtitle("Random Histogram")
      } else {
        # Boxplot
        ggplot(df, aes(category, y, fill = category)) +
          geom_boxplot(alpha = 0.7) +
          theme_minimal() +
          ggtitle("Random Boxplot")
      }
    })
  })
}

# Run the application
runApp(list(ui = ui, server = server), launch.browser = FALSE, port = port, host = "0.0.0.0")
