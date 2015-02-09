library(shinydashboard)


skin="black"

ui <- dashboardPage(
  skin="black",
  dashboardHeader(title = "AccessMod 5"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabsetPanel(
      tabItem('test')
      ),
    box(
      fileInput('testfi','upload files',accept = 'shp'),  
      actionButton('test','mein button'),
      actionButton('test1','mein button'),
      selectInput('test','select!',choices=c('a','b')),
      actionButton('test2','mein button')

      ),
    fluidRow(
      box(        
        plotOutput("plot1", height = 250)),

      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
        )
      )
    )
  )

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)
