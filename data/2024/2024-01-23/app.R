library(tidyverse)
library(here)
library(fs)
library(withr)
library(shiny)


english_education <- read_csv("english_education.csv")

# library(shiny)


if (any(is.na(english_education$coastal))
    | any(is.na(english_education$income_flag)))
{
  english_ed <- na.omit(english_education)
}


english_ed <- english_ed |> 
  dplyr::rename(
    "Town/city" = town11nm,
    "Travel to work area" = ttwa11nm,
    "Education score" = education_score
  )

edu_names <- setNames(english_ed$coastal, english_ed$income_flag)

ui <- fluidPage(
  titlePanel("Educational attainment of young people in English towns"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "region", label = "Region", choices = unique(english_ed$rgn11nm)),
      selectInput(inputId = "coastal", label = "Type of area", choices = NULL),
      selectInput(inputId = "income", label = "Income deprivation", choices = NULL, size = 5, selectize = FALSE),
      em("This app was created for week 4 of #TidyTuesday 2024.\n
      The dataset is from the UK Office for National Statistics. It was explored in the July 2023 article 'Why do children and young people in smaller towns do better academically than those in larger towns?'."),
      br(),
      br(),
      p("Created by: @tangandhara")
  ),
  
  mainPanel(
    tableOutput("data"))
  )
)
  
server <- function(input, output, session) {
  region <- reactive({
    filter(english_ed, rgn11nm == input$region)
    })
  observeEvent(region(), {
    choices <- unique(region()$coastal)
    updateSelectInput(inputId = "coastal", choices = choices)
  })
  
  coastal <- reactive({
    req(input$coastal)
    filter(region(), coastal == input$coastal)
  })
  observeEvent(coastal(), {
    choices <- unique(coastal()$income_flag)
    updateSelectInput(inputId = "income", choices = choices)
  })
  

  
  output$data <- renderTable({
    req(input$income)
    coastal() |> 
      filter(income_flag == input$income) |> 
      select("Town/city", "Travel to work area", "Education score") |> 
      arrange(`Travel to work area`, desc(`Education score`))
  })
}

shinyApp(ui, server)
