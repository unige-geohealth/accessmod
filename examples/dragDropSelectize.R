library(htmltools)

addUIDep <- function(x) {
  jqueryUIDep <- htmlDependency("jqueryui", "1.10.4", c(href="shared/jqueryui/1.10.4"),
                                script = "jquery-ui.min.js",
                                stylesheet = "jquery-ui.min.css")
  
  attachDependencies(x, c(htmlDependencies(x), list(jqueryUIDep)))
}

shinyApp(
  ui = fluidPage(
    addUIDep(selectizeInput("variables", "", c('road_main','barrier_main','road_secondary_long_list_of_tag_really','landcover_main'), c("landcover_main","road_main"), TRUE, 
                            options = list(plugins = list("drag_drop", "remove_button")),width='100%')),
    textOutput("variables")
  ), 
  server = function(input, output) {
    output$variables <- renderText(input$variables)
  }
)