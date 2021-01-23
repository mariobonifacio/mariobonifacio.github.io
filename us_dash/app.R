library(shiny)
library(jsonlite)
library(data.table)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(gridExtra)

options(scipen = 999)

all_data <- (fromJSON("https://covidtracking.com/api/v1/states/daily.json")) %>% 
  select(as_of = date, state, positive, totalTestResults, death, positiveIncrease, totalTestResultsIncrease, deathIncrease) %>%
  data.table() %>%
  .[!is.na(positive) & !is.na(totalTestResults) & !is.na(death),] %>% 
  .[,as_of := ymd(as_of)]
  



wk_data <- all_data[,wk_ending := case_when(wday(as_of) < wday(max(as_of, na.rm = TRUE)) ~ as_of + days(wday(max(as_of, na.rm = TRUE)) - wday(as_of)),
                                            wday(as_of) > wday(max(as_of, na.rm = TRUE)) ~ as_of + days(7 + wday(max(as_of, na.rm = TRUE)) - wday(as_of)),
                                            TRUE ~ as_of)][
  ,positive := if_else(is.finite(max(positive, na.rm = TRUE)) & !is.na(max(positive, na.rm = TRUE)), max(positive, na.rm = TRUE), 0L), by = list(wk_ending, state)][
    ,totalTestResults := if_else(is.finite(max(totalTestResults, na.rm = TRUE)), max(totalTestResults, na.rm = TRUE), 0L) , by = list(wk_ending, state)][
      ,death := if_else(is.finite(max(death, na.rm = TRUE)), max(death, na.rm = TRUE), 0L), by = list(wk_ending, state)][
        ,positiveIncrease := sum(positiveIncrease, na.rm = TRUE), by = list(wk_ending, state)][
          ,totalTestResultsIncrease := sum(totalTestResultsIncrease, na.rm = TRUE), by = list(wk_ending, state)][
            ,deathIncrease := sum(deathIncrease, na.rm = TRUE), by = list(wk_ending, state)][
              wk_ending == as_of,list(wk_ending, state, positive, totalTestResults, death, positiveIncrease, totalTestResultsIncrease, deathIncrease)
            ]

lead3 <- wk_data[,c(1:2, 8)] %>% 
  setnames(c("wk_ending", "deathIncrease"), c("lead3_wk_ending", "lead3_deathIncrease")) %>% 
  .[lead3_deathIncrease > 0,] %>% 
  .[, wk_ending := lead3_wk_ending - weeks(3)]

lag1 <- wk_data[,c(1:2, 6:7)][, lag1_pos_rate := positiveIncrease / totalTestResultsIncrease] %>% 
  setnames("wk_ending", "lag1_wk_ending") %>% 
  .[,wk_ending := lag1_wk_ending + weeks(1)] %>% 
  .[between(lag1_pos_rate, 0.0001, 0.9999), c(1:2,5:6)]

test_table <- lead3[lag1, on = c("wk_ending", "state")][
  wk_data[wk_ending >= ymd(20200601) & is.finite(positiveIncrease) & positiveIncrease > 0, c(1,2,6)], on = c("wk_ending", "state")]
  
  

state_static <- data.table(state = c('AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY'),
                           density = c(1.2863, 96.9221, 58.403, 64.9549, 256.3728, 56.4012, 735.8695, 11515, 504.3073, 410.1259, 186.6726, 219.9424, 56.9284, 22.097, 228.0246, 188.2809, 35.5968, 113.9566, 107.5174, 894.4359, 626.6735, 43.6336, 177.665, 71.5922, 89.7453, 63.7056, 7.4668, 218.271, 11.0393, 25.4161, 153.161, 1215.1985, 17.285, 28.5993, 412.5218, 287.504, 57.6546, 44.8086, 286.5454, 1021.4313, 173.3176, 11.9116, 167.2749, 112.8204, 39.943, 218.4404, 68.1416, 117.3273, 108.0496, 73.9691, 5.84),
                           pop = c(734002, 4908620, 3039000, 7378490, 39937500, 5845530, 3563080, 702455, 982895, 21993000, 10736100, 1412690, 3179850, 1826160, 12659700, 6745350, 2910360, 4499690, 4645180, 6976600, 6083120, 1345790, 10045000, 5700670, 6169270, 2989260, 1086760, 10611900, 761723, 1952570, 1371250, 8936570, 2096640, 3139660, 19440500, 11747700, 3954820, 4301090, 12820900, 1056160, 5210100, 903027, 6897580, 29472300, 3282120, 8626210, 628061, 7797100, 5851750, 1778070, 567025),
                           region = c('Sky', 'South', 'Middle South', 'Southwest', 'West Coast', 'Southwest', 'Northeast', 'Northeast', 'Northeast', 'South', 'South', 'West Coast', 'Midwest', 'Sky', 'Midwest', 'Midwest', 'Sky', 'Middle South', 'South', 'Northeast', 'Northeast', 'Northeast', 'Midwest', 'Midwest', 'Midwest', 'South', 'Sky', 'South', 'Sky', 'Sky', 'Northeast', 'Northeast', 'Southwest', 'Southwest', 'Northeast', 'Midwest', 'Sky', 'West Coast', 'Northeast', 'Northeast', 'South', 'Sky', 'Middle South', '', 'Sky', 'Northeast', 'Northeast', 'West Coast', 'Midwest', 'Middle South', 'Sky'),
                           t_2016 = c(0.1473, 0.2773, 0.2692, 0.035, -0.3011, -0.0491, -0.1364, -0.8678, -0.1137, 0.012, 0.0513, -0.3218, 0.0941, 0.3177, -0.1706, 0.1917, 0.206, 0.2984, 0.1964, -0.272, -0.2642, -0.0296, 0.0023, -0.0152, 0.1864, 0.1783, 0.2042, 0.0366, 0.3573, 0.2505, -0.0037, -0.141, -0.0821, -0.0242, -0.2249, 0.0813, 0.3708, -0.1098, 0.0072, -0.1551, 0.1427, 0.2979, 0.2601, 0.0899, 0.1808, -0.0532, -0.2641, -0.1571, 0.0077, 0.4207, 0.463),
                           t_2020 = c(0.1006, 0.2546, 0.2762, -0.0031, -0.2916, -0.135, -0.2003, -0.8675, -0.1897, 0.0336, -0.0024, -0.2946, 0.082, 0.3077, -0.1699, 0.1606, 0.1465, 0.2594, 0.1861, -0.3346, -0.3321, -0.0907, -0.0278, -0.0711, 0.1539, 0.1654, 0.1637, 0.0135, 0.3336, 0.1906, -0.0735, -0.1594, -0.1079, -0.0239, -0.2311, 0.0803, 0.3309, -0.1608, -0.0116, -0.2077, 0.1168, 0.2616, 0.2321, 0.0558, 0.2048, -0.1011, -0.3541, -0.192, -0.0063, 0.3893, 0.4338))

best_fit1 <- function(test_state){ lm(data = test_table[state == test_state], formula = lead3_deathIncrease ~ 0 + lag1_pos_rate + positiveIncrease)[[1]][[1]] }
best_fit2 <- function(test_state){ lm(data = test_table[state == test_state], formula = lead3_deathIncrease ~ 0 + lag1_pos_rate + positiveIncrease)[[1]][[2]] }
best_fitX <- function(test_state){ lm(data = test_table[state == test_state], formula = lead3_deathIncrease ~ 0 + positiveIncrease)[[1]][[1]] }

coeff_tbl <- data.table(state = character(), coeff_pos_rate = double(), coeff_cases = double(), coeff_cases_only = double())
  
for(i in state_static[,state]) coeff_tbl <- rbind(coeff_tbl, data.table(state = i, coeff_pos_rate = best_fit1(i), coeff_cases = best_fit2(i), coeff_cases_only = best_fitX(i))[coeff_pos_rate < 0, coeff_cases := coeff_cases_only][, coeff_pos_rate := (coeff_pos_rate + abs(coeff_pos_rate)) / 2])

us_data <- lead3[lag1, on = c("wk_ending", "state")][
  wk_data[wk_ending >= ymd(20200601) & is.finite(positiveIncrease) & positiveIncrease > 0, c(1,2,6)], on = c("wk_ending", "state")][
    coeff_tbl, on = "state"][
      is.na(lead3_wk_ending), proj := TRUE][
      ,lead3_wk_ending := wk_ending + weeks(3)][
        ,lead3_deathIncrease := as.double(lead3_deathIncrease)][
          (is.na(lead3_deathIncrease)) & lead3_wk_ending > max(wk_ending), lead3_deathIncrease := lag1_pos_rate * coeff_pos_rate + positiveIncrease * coeff_cases][
            !is.na(lead3_deathIncrease),][
              state_static, on = "state"][
                ,lead3_deathIncrease_pm := lead3_deathIncrease / pop * 1000000][
                  ,positiveIncrease_pm := positiveIncrease / pop * 1000000]


# Define UI for app that draws a col chart ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("U.S. COVID-19 dashboard"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Dropdown ----
      selectInput(inputId = "geography",
                  label = "Geography to display data for:",
                  choices = list("U.S.",
                                 "Arizona",
                                 "Deaths per capita last week",
                                 "Cases per capita last week"),
                  selected = "U.S.")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: column ----
      plotOutput(outputId = "cols")
      
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$cols <- renderPlot({
    data <- switch(input$geography, 
                   "U.S." = us_data %>%
                     group_by(lead3_wk_ending, proj) %>% 
                     summarize(x_var = max(lead3_wk_ending, na.rm = TRUE),
                               y_var = sum(lead3_deathIncrease, na.rm = TRUE)),
                   
                   "Illinois" = us_data[state == "IL",],
                   
                   "Deaths per capita last week" = us_data[lead3_wk_ending == max(wk_ending, na.rm = TRUE)][
                     lead3_deathIncrease_pm = lead3_deathIncrease / pop * 1000000] %>% 
                     slice_max(order_by = lead3_deathIncrease_pm, n = 20) %>% 
                     mutate(x_var = reorder(state, desc(lead3_deathIncrease_pm)),
                            y_var = lead3_deathIncrease_pm,
                            title_var = "States with most COVID-19 deaths per capita over the past week"),
                   
                   "Cases per capita last week" = us_data[wk_ending == max(wk_ending, na.rm = TRUE)] %>%
                     mutate(positiveIncrease_pm = positiveIncrease / pop * 1000000) %>% 
                     slice_max(order_by = positiveIncrease_pm, n = 20) %>% 
                     mutate(x_var = reorder(state, desc(positiveIncrease_pm)), y_var = positiveIncrease_pm, lab_var = "States with most new COVID-19 cases per capita over the past week"))
    


  ggplot(data = data, mapping = aes(x = x_var)) +
    geom_col(mapping = aes(y = y_var, alpha = proj, linetype = !is.na(proj), color = is.na(proj)), size = 1)  +
    scale_colour_discrete(type = c("lightgray", "dodgerblue")) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL) +
    scale_fill_gradient2(low = "white", mid = "white", high = "dodgerblue") +
    scale_alpha_discrete(range = c(0.4, 1))
  

    

    
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)