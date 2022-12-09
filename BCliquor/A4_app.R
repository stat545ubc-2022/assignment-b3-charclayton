library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(rsconnect)

bcl <- read_csv("bcl-data.csv") #load data

ui <- fluidPage(theme = shinytheme("flatly"), #change theme
                img(src = "bcliq.png", height = 100, width =800), #add logo image from BC liquor stores
                titlePanel(title = "Welcome to the BC Liquor Store App!"),#adds title at the top of the page
                h5("Filter by your drink preferences to see what we have in stock for you!"),
                br(), #adds a break
                sidebarLayout(
                  sidebarPanel( #filter by type of beverage, country it is from, and price

                    radioButtons(inputId = "typeInput", label = "Type",
                                 choices = c(unique(bcl$Type))),
                    pickerInput(inputId = "countryInput",
                                label = "Country",
                                choices=c(unique(bcl$Country)), #countries to select from from all Bcl stocks
                                selected = "CANADA", #default country selected is Canada
                                options = list(`actions-box` = TRUE, `none-selected-text` = "Please select a country"), #allow user to select all or none
                                multiple = TRUE),

                    sliderInput(inputId = "priceInput", label = "Price",
                                min = 0, max = 100,
                                value = c(25, 40), pre = "$"),
                    br() ,
                    h5("Please drink responsibly")
                  )
                  ,
                  mainPanel(DT::dataTableOutput("data_table") #only output interactive table with users preferences
                  )
                ),
                a(href="https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv>", "Find all our stocks here!") #reference dataset used and all stocks available
)

server <- function(input, output) {

  filtered_data<- #need to wrap this variable to a reactive function, when you call this reactive variabel later in the code you need a new suntax (add round brackets as if it was a fucntion ())
    reactive({
      bcl%>%
        filter(Price > input$priceInput[1] &
                 Price < input$priceInput[2] &
                 Type == input$typeInput &
                 Country %in% input$countryInput)
    }) #reactive makes the code reactive and user input dependent


  output$data_table <-
    DT::renderDataTable({ #make output table interactive based on user drink preferences
      filtered_data()
    })
}


shinyApp(ui = ui, server = server)
