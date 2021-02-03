#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(data.table)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(gridExtra)


# turns off scientific notation

options(scipen = 999)


# loads static data

static_data <- data.table(name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "District of Columbia"),
                          code = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"), 
                          pop = c(4908620, 734002, 7378490, 3039000, 39937500, 5845530, 3563080, 982895, 21993000, 10736100, 1412690, 1826160, 12659700, 6745350, 3179850, 2910360, 4499690, 4645180, 1345790, 6083120, 6976600, 10045000, 5700670, 2989260, 6169270, 1086760, 1952570, 3139660, 1371250, 8936570, 2096640, 19440500, 10611900, 761723, 11747700, 3954820, 4301090, 12820900, 1056160, 5210100, 903027, 6897580, 29472300, 3282120, 628061, 8626210, 7797100, 1778070, 5851750, 567025, 705749),
                          area = c(50645, 570641, 113594, 52035, 155779, 103642, 4842, 1949, 53625, 57513, 6423, 82643, 55519, 35826, 55857, 81759, 39486, 43204, 30843, 9707, 7800, 56539, 79627, 46923, 68742, 145546, 76824, 109781, 8953, 7354, 121298, 47126, 48618, 69001, 40861, 68595, 95988, 44743, 1034, 30061, 75811, 41235, 261232, 82170, 9217, 39490, 66456, 24038, 54158, 97093, 68.3),
                          region = c("South", "Sky", "Southwest", "Middle South", "West Coast", "Southwest", "Northeast", "Northeast", "South", "South", "West Coast", "Sky", "Midwest", "Midwest", "Midwest", "Sky", "Middle South", "South", "Northeast", "Northeast", "Northeast", "Midwest", "Midwest", "South", "Midwest", "Sky", "Sky", "Southwest", "Northeast", "Northeast", "Southwest", "Northeast", "South", "Sky", "Midwest", "Sky", "West Coast", "Northeast", "Northeast", "South", "Sky", "Middle South", "Southwest", "Sky", "Northeast", "South", "West Coast", "Middle South", "Midwest", "Sky", "Northeast"))[, c("pop", "pop_dens") := list(as.integer(pop), pop / area)]


# Three predictive models
est_casepos_case <- function(test_tbl, lead3_death, case, lag1_pos) { lm(lead3_death ~ 0 + case + lag1_pos, test_tbl)[1][1] }
est_casepos_case <- function(test_tbl, lead3_death, case, lag1_pos) { lm(lead3_death ~ 0 + case + lag1_pos, test_tbl)[2][1] }
est_case <- function(test_tbl, lead3_death, case) { lm(lead3_death ~ 0 + case + lag1_pos, test_tbl)[1][1] }
est_pos <- function(test_tbl, lead3_death, lag1_pos) { lm(lead3_death ~ 0 + case + lag1_pos, test_tbl)[1][1] }




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
