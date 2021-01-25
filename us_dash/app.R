

library(shiny)
library(jsonlite)
library(data.table)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(gridExtra)

options(scipen = 999)


u <- shinyUI(fluidPage(
  titlePanel("U.S. COVID-19 dashboard"),
  sidebarLayout(position = "left",
                sidebarPanel("Options",
                             selectInput(inputId = "geography", label = "Time series geography:", choices = list("U.S.", "Illinois", selected = "U.S.")),
                             selectInput(inputId = "metric", label = "Snapshot rank metric:", choices = list("Deaths", "Cases", selected = "Deaths")),
                             actionButton("refresh_button", "Refresh data")
                ),
                mainPanel("main panel",
                          column(6,plotOutput(outputId="plotgraph", width="500px",height="400px"))
                ))))

s <- shinyServer(function(input, output) 
{
  
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    qplot(rnorm(500),fill=I("red"),binwidth=0.2,main="plotgraph1")
  })
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    qplot(rnorm(500),fill=I("blue"),binwidth=0.2,main="plotgraph2")
  })

  output$plotgraph = renderPlot({
    ptlist <- list(pt1(),pt2(),pt3())
    wtlist <- c(input$wt1,input$wt2,input$wt3)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
  })
})
shinyApp(u,s)