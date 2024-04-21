library(readr)
library(shiny)
library(DT)
library(dplyr)

hit_data <- read_csv("clean_hit.csv")
ext_hit_data <- read_csv("clean_hit_ext.csv")
pitch_data <- read_csv("clean_pitch.csv")
field_data <- read_csv("clean_field.csv")

ui <- fluidPage(
  titlePanel("JUCO Stats Database"),
  
  tabsetPanel(
    tabPanel("Hit Data", 
             selectInput("teamName", "Choose a Team:",
                         choices = c("All Teams" = "All Teams", sort(unique(hit_data$TeamName)))),
             DTOutput("hitTable")),
    tabPanel("Extended Hit Data",
             selectInput("extTeamName", "Choose a Team:",
                         choices = c("All Teams" = "All Teams", sort(unique(ext_hit_data$TeamName)))),
             DTOutput("extHitTable")),
    tabPanel("Pitch Data",
             selectInput("pitchTeamName", "Choose a Team:",
                         choices = c("All Teams" = "All Teams", sort(unique(pitch_data$TeamName)))),
             DTOutput("pitchTable")),
    tabPanel("Field Data",
             selectInput("fieldTeamName", "Choose a Team:",
                         choices = c("All Teams" = "All Teams", sort(unique(field_data$TeamName)))),
             DTOutput("fieldTable"))
  )
)

server <- function(input, output) {

  filteredHitData <- reactive({
    if (input$teamName == "All Teams") {
      hit_data
    } else {
      hit_data %>% filter(TeamName == input$teamName)
    }
  })
  
  filteredExtHitData <- reactive({
    if (input$extTeamName == "All Teams") {
      ext_hit_data
    } else {
      ext_hit_data %>% filter(TeamName == input$extTeamName)
    }
  })
  
  filteredPitchData <- reactive({
    if (input$pitchTeamName == "All Teams") {
      pitch_data
    } else {
      pitch_data %>% filter(TeamName == input$pitchTeamName)
    }
  })
  
  filteredFieldData <- reactive({
    if (input$fieldTeamName == "All Teams") {
      field_data
    } else {
      field_data %>% filter(TeamName == input$fieldTeamName)
    }
  })
  
  output$hitTable <- renderDT({
    datatable(filteredHitData(), options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 25, 50), pageLength = 10),
              filter = 'top', rownames = FALSE)
  })
  
  output$extHitTable <- renderDT({
    datatable(filteredExtHitData(), options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 25, 50), pageLength = 10),
              filter = 'top', rownames = FALSE)
  })
  
  output$pitchTable <- renderDT({
    datatable(filteredPitchData(), options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 25, 50), pageLength = 10),
              filter = 'top', rownames = FALSE)
  })
  
  output$fieldTable <- renderDT({
    datatable(filteredFieldData(), options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 25, 50), pageLength = 10),
              filter = 'top', rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)
