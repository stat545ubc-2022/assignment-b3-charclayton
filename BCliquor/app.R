library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)

bcl <- read_csv("bcl-data.csv") #load data

ui <- fluidPage(theme = shinytheme("flatly"), #change theme
  img(src = "bcliq.png", height = 100, width =700), #add logo image from BC liquor stores
  titlePanel(title = "Welcome to the BC Liquor Store App!"),#this adds the title at the top of the page
  h5("Filter by your preferences to see what we have in stock for you!"),
  br(), #adds a break
  sidebarLayout(
    sidebarPanel(
    sliderInput(inputId = "priceInput", label = "Price",
                min = 0, max = 100,
                value = c(25, 40), pre = "$"), #this puts the slideer above the plot
    radioButtons(inputId = "typeInput", label = "Type",
                 choices = c("BEER", "REFRESHMENT", "SPIRITS", 'WINE')),
    selectInput(inputId = "countryInput", label = "Country", #select country that drink is from
                choices = c("CANADA", "UNITED STATES OF AMERICA", "FRANCE", "IRELAND", "ITALY", "BRAZIL",
                "UNITED KINGDOM", "SPAIN","GERMANY" , "PORTUGAL",  "ARGENTINA", "ISRAEL", "CZECH REPUBLIC",
                "BELGIUM" , "MEXICO", "AUSTRALIA" ,"SOUTH AFRICA","CHINA","CHILE","NETHERLANDS",
                "JAMAICA","JAPAN","CUBA","GREECE" ,"BULGARIA","DOMINICAN REPUBLIC","NEW ZEALAND","POLAND",
                "AUSTRIA","TRINIDAD AND TOBAGO","BERMUDA","ANTIGUA AND BARBUDA", "MOROCCO","GUYANA","SWEDEN",
                "DENMARK",  "LATVIA","GEORGIA","FINLAND","BARBADOS", "NICARAGUA","INDIA","KOREA - SOUTH","LEBANON",
                "HUNGARY","TAIWAN","TURKEY","SWITZERLAND", "SINGAPORE","RUSSIA (USSR)","ICELAND","VENEZUELA",
                "CROATIA","RUSSIA","PUERTO RICO","THAILAND", "FRENCH POLYNESIA (TAHITI)" ,"GUATEMALA","PHILIPPINES",
                "MONTENEGRO", "ST. CROIX","PANAMA","VIETNAM","PERU")),
    br() ,
    h5("Please drink responsibly")
    )
    ,
    mainPanel(
    plotOutput("alcohol_hist"),  #this puts the plot in the main panel of the sheet - this is a placeholder for the plot
    DT::dataTableOutput("data_table")
    )
  ),
  a(href="https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv>", "Find all our stocks here!")
)

server <- function(input, output) {

  filtered_data<- #need to wrap this variable to a reactive function, when you call this reactive variabel later in the code you need a new suntax (add round brackets as if it was a fucntion ())
    reactive({
      bcl%>%
        filter(Price > input$priceInput[1] &
                 Price < input$priceInput[2] &
                 Type == input$typeInput &
                 Country == input$countryInput)
      }) #reactive makes the code reactive and user input dependant

  output$alcohol_hist <-
    renderPlot({ #makes a reactive plot based on the user input, if the call is dependent on the user input than it needs to be in render function {}
      filtered_data() %>% #this is a reactive variable that is based on the user input - need to add the round brackets after the variable as if it were a function
      ggplot(aes(Alcohol_Content))+
      geom_histogram(fill = "darkred")+
      theme_bw()+
        labs(y = "Count",
             x = "Alcohol Price")

      })

  output$data_table <-
    DT::renderDataTable({ #make output table interactive
      filtered_data()
    })
}


shinyApp(ui = ui, server = server)

