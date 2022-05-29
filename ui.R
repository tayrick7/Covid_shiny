library(shiny)
library(shinyWidgets)
library(shinythemes)

ui <- fluidPage(
    theme = shinytheme("superhero"),
    span(titlePanel("COVID-19 Visualisation"),style ="color:yellow; font-style: italic; font.family: arial" ),
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "country",
                           "Countries/Regions:", 
                           choices = unique(cleand_covid$country),
                           selected = "United States",
                           multiple = FALSE),
            dateRangeInput("daterange", "Date Range:",
                           start = min(cleand_covid$time),
                           end = max(cleand_covid$time),
                           min = min(cleand_covid$time),
                           max = max(cleand_covid$time),
                           format ="mm/dd/yy", separator = "-" ),
            checkboxGroupButtons(
                inputId = "death_most",
                label = "First 5 countries with high incidence:", 
                choices = c("United States","India","Brazil","United Kingdom","Russia")),
            checkboxGroupButtons(
                inputId = "death_fewest",
                label = "Last 5 countries with high incidence:", 
                choices = c("Mexico","Ukraine","South Africa","Netherlands","Philippines")),
            checkboxGroupButtons(
                inputId = "most_strengthened",
                label = "First 5 countries with most strengthened policies:", 
                choices = c("Philippines","Turkey","Italy","South Africa","Mexico")),
            checkboxGroupButtons(
                inputId = "least_strengthened",
                label = "Last 5 countries with most strengthened policies:", 
                choices = c("United Kingdom","Argentina","Indonesia","Iran","Poland"))
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Read Me", div(img(src = 'myImage2.png',height = '250px', width = '350px'), style="text-align: left"), span(textOutput("info"), style="color:red;font-size: 25px;font-style: italic"),span(textOutput("Info"),style = "color:white"),
                                 br(),span(textOutput("data"), style="color:red;font-size: 20px;font-style: italic"), span(textOutput("Data"),style = "color:white"), 
                                 tags$div(
                                     span("For detailed information please ",style ="color:white"),
                                     tags$a(href="https://github.com/owid/covid-19-data/tree/master/public/data", 
                                            "click here")),
                                 br(),span(textOutput("ai"), style="color:red; font-size: 20px;font-style: italic"), span(textOutput("Ai_1"),style = "color:white"),span(textOutput("Ai_2"),style = "color:white"),span(textOutput("Ai_3"),style = "color:white"),
                                 br(),span(textOutput("name"), style="color:red;font-size: 15px;font-style: italic"),span(textOutput("name_1"),style = "color:white"),span(textOutput("name_2"),style = "color:white"),span(textOutput("name_3"),style = "color:white"),span(textOutput("name_4"),style = "color:white"),span(textOutput("name_5"),style = "color:white")
                        ),
                        tabPanel("First difference", plotlyOutput("fd"), plotlyOutput("fd_2")),
                        tabPanel("New case trend", plotlyOutput("new_case")),
                        tabPanel("Stringency trend",plotlyOutput("stringency")),
                        tabPanel("World Map", plotlyOutput("World_map_cases")),
                        tabPanel("New Cases vs Stringency Index", plotOutput("cluster"))
                        
                        
            )
        )
    )
)


