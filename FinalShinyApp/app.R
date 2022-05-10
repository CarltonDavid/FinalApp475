library(fpp3)

# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv("multiTimeline (2).csv", skip = 2)
# Rename columns
names(g_trends) <- c("Month", "Groundhog")

g_trends[g_trends == "<1"] <- 0

class(g_trends$Groundhog)
# Convert Month to date
g_trends$Month <- yearmonth(g_trends$Month)
# Convert to tsibble
g_trends <- tsibble(g_trends)


# Data starts in 3rd row, skip first 2 rows
g_trends2 <- read.csv("multiTimeline (3).csv", skip = 2)
# Rename columns
names(g_trends2) <- c("Month", "Punxsutawney Phil")

g_trends2[g_trends2 == "<1"] <- 0
g_trends2$`Punxsutawney Phil` <- as.integer(g_trends2$`Punxsutawney Phil`)
# Convert Month to date
g_trends2$Month <- yearmonth(g_trends2$Month)
# Convert to tsibble
g_trends2 <- tsibble(g_trends2)


# Data starts in 3rd row, skip first 2 rows
g_trends3 <- read.csv("multiTimeline (4).csv", skip = 2)
# Rename columns
names(g_trends3) <- c("Month", "Milltown Mel")

g_trends3[g_trends3 == "<1"] <- 0
g_trends3$`Milltown Mel` <- as.integer(g_trends3$`Milltown Mel`)
# Convert Month to date
g_trends3$Month <- yearmonth(g_trends3$Month)
# Convert to tsibble
g_trends3 <- tsibble(g_trends3)


# Data starts in 3rd row, skip first 2 rows
g_trends4 <- read.csv("multiTimeline (5).csv", skip = 2)
# Rename columns
names(g_trends4) <- c("Month", "Essex Ed")

g_trends4[g_trends4 == "<1"] <- 0
# Convert Month to date
g_trends4$Month <- yearmonth(g_trends4$Month)
# Convert to tsibble
g_trends4 <- tsibble(g_trends4)


# Data starts in 3rd row, skip first 2 rows
g_trends5 <- read.csv("multiTimeline (6).csv", skip = 2)
# Rename columns
names(g_trends5) <- c("Month", "Chattanooga Chuck")
# Convert Month to date
g_trends5$Month <- yearmonth(g_trends5$Month)
# Convert to tsibble
g_trends5 <- tsibble(g_trends5)

# autoplot(g_trends)



overall_data1 <- inner_join(g_trends,g_trends2, by = "Month")

overall_data2 <- inner_join(overall_data1,g_trends3, by = "Month")

overall_data3 <- inner_join(overall_data2,g_trends4, by = "Month")

overall_data <- inner_join(overall_data3,g_trends5, by = "Month")

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Groundhog Cities"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("App Instructions", tabName = "Tab1"),
            menuItem("Groundhog Search Patterns", tabName = "Tab2"),
            menuItem("Analysis", tabName = "Tab3")
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "Tab1",
                    h3("Steps for using this app:", align = "center"),
                    h3("1. Choose for which term you would like to view Google search trends (additional feature)", align = "center"),
                    h3("2. Select which variety of graph you would like to be displayed (predictive graphs show forecasts for 12 periods)", align = "center"),
            ),
            
            tabItem(tabName = "Tab2",
                    fluidPage(
                        box(
                            uiOutput("dropdown")),
                            box(
                                uiOutput("dropdown2")),
                                box(
                                    plotOutput(
                                        "hogs",
                                        width = 800,
                                        click = NULL,
                                        dblclick = NULL,
                                        hover = NULL,
                                        brush = NULL,
                                        inline = FALSE
                                    )),
                                           
                                           # Copy the line below to make a slider bar 
                                    sliderInput("slider1", label = h3("ARIMA - p"), min = 0, 
                                                       max = 10, value = 0),
                        sliderInput("slider2", label = h3("ARIMA - d"), min = 0, 
                                    max = 10, value = 0),
                    sliderInput("slider3", label = h3("ARIMA - q"), min = 0, 
                                max = 10, value = 0)
                    )    
                    ),
            tabItem(tabName = "Tab3",
                    h3("Trend Interpretation: Trend varies for each of the terms. \"Groundhog\" and \"Punxsutawney Phil\" have positive trends, while the others see increases and decreases in popularity. ", align = "center"),
                    h3("Seasonality: Clear seasonality amongst the terms \"Groundhog\" and \"Punxsutawney Phil\" - not as much seasonality amongst other terms.", align = "center"),
                    h3("Rationale: The groundhog comes out once per year (Febuary 2nd), thus searches for \"Groundhog\" and \"Punxsutawney Phil\" (the most popular groundhog) are higher around this time.", align = "center"),
                    h3("Autocorrelation: The terms \"Groundhog\" and \"Punxsutawney Phil\" have very clear 12 month lags, meaning that values are correlated heavily with values from 12 months ago", align = "center"))
        ),
    ))


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$dropdown <- renderUI(
        pickerInput(
            inputId = "select",
            # multiple = T,
            label = h3("Choose Your Term"),
            choices = c("Groundhog", "Punxsutawney Phil", "Milltown Mel", "Essex Ed", "Chattanooga Chuck"),
            selected = "Groundhog",
        )
    )
    
    output$dropdown2 <- renderUI(
        pickerInput(
            inputId = "select2",
            # multiple = T,
            label = h3("Choose Your Plot"),
            choices = c("Time Series", "Seasonality", "Autocorrelation", "Decomposition", "Naive", "Seasonal Naive", "Mean", "Drift", "Exponential Smoothing - Holts", "Exponential Smoothing - Holts/Winters", "ARIMA - Auto", "ARIMA - Choose Parameters"),
            selected = "Time Series",
        )
    )
    
    output$hogs <- renderPlot({
        if (input$select2 == "Time Series") {
            
            overall_data %>% 
                select(Month, input$select) %>%
                autoplot()
        }else if (input$select2 == "Seasonality"){
            overall_data %>% 
                select(Month, input$select) %>%
                gg_season()
        }else if (input$select2 == "Autocorrelation" && input$select == "Groundhog") {
            overall_data %>%
                select(Month, input$select)%>%
                ACF(Groundhog) %>%
                autoplot()
        }else if (input$select2 == "Autocorrelation" && input$select == "Punxsutawney Phil") {
            overall_data %>%
                select(Month, input$select)%>%
                ACF(overall_data$`Punxsutawney Phil`) %>%
                autoplot()
        }else if (input$select2 == "Autocorrelation" && input$select == "Milltown Mel") {
            overall_data %>%
                select(Month, input$select)%>%
                ACF(overall_data$`Milltown Mel`) %>%
                autoplot()
        }else if (input$select2 == "Autocorrelation" && input$select == "Essex Ed") {
            overall_data %>%
                select(Month, input$select)%>%
                ACF(overall_data$`Essex Ed`) %>%
                autoplot()
        }else if (input$select2 == "Autocorrelation" && input$select == "Chattanooga Chuck") {
            overall_data %>%
                select(Month, input$select)%>%
                ACF(overall_data$`Chattanooga Chuck`) %>%
                autoplot()
        }else if (input$select2 == "Decomposition"&& input$select == "Groundhog") {
            overall_data %>%
                model(
                    classical_decomposition(Groundhog, type = "multiplicative") 
                ) %>%
                components() %>%
                autoplot()
        }else if (input$select2 == "Decomposition" && input$select == "Punxsutawney Phil") {
            overall_data %>%
                model(
                    classical_decomposition(`Punxsutawney Phil`, type = "multiplicative") 
                ) %>%
                components() %>%
                autoplot()
        }else if (input$select2 == "Decomposition" && input$select == "Milltown Mel") {
            overall_data %>%
                model(
                    classical_decomposition(`Milltown Mel`, type = "multiplicative") 
                ) %>%
                components() %>%
                autoplot()
        }else if (input$select2 == "Decomposition" && input$select == "Essex Ed") {
            overall_data %>%
                model(
                    classical_decomposition(`Essex Ed`, type = "multiplicative") 
                ) %>%
                components() %>%
                autoplot()    
        }else if (input$select2 == "Decomposition" && input$select == "Chattanooga Chuck") {
            overall_data %>%
                model(
                    classical_decomposition(`Chattanooga Chuck`, type = "multiplicative") 
                ) %>%
                components() %>%
                autoplot()    
        }else if (input$select2 == "Naive" && input$select == "Groundhog") {
            overall_data %>%
                model(
                    NAIVE(`Groundhog`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Naive" && input$select == "Punxsutawney Phil") {
            overall_data %>%
                model(
                    NAIVE(`Punxsutawney Phil`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Naive" && input$select == "Milltown Mel") {
            overall_data %>%
                model(
                    NAIVE(`Milltown Mel`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Naive" && input$select == "Essex Ed") {
            overall_data %>%
                model(
                    NAIVE(`Essex Ed`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Naive" && input$select == "Chattanooga Chuck") {
            overall_data %>%
                model(
                    NAIVE(`Chattanooga Chuck`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Seasonal Naive" && input$select == "Groundhog") {
            overall_data %>%
                model(
                    SNAIVE(`Groundhog`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Seasonal Naive" && input$select == "Punxsutawney Phil") {
            overall_data %>%
                model(
                    SNAIVE(`Punxsutawney Phil`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Seasonal Naive" && input$select == "Milltown Mel") {
            overall_data %>%
                model(
                    SNAIVE(`Milltown Mel`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Seasonal Naive" && input$select == "Essex Ed") {
            overall_data %>%
                model(
                    SNAIVE(`Essex Ed`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Seasonal Naive" && input$select == "Chattanooga Chuck") {
            overall_data %>%
                model(
                    SNAIVE(`Chattanooga Chuck`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Mean" && input$select == "Groundhog") {
            overall_data %>%
                model(
                    MEAN(`Groundhog`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Mean" && input$select == "Punxsutawney Phil") {
            overall_data %>%
                model(
                    MEAN(`Punxsutawney Phil`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Mean" && input$select == "Milltown Mel") {
            overall_data %>%
                model(
                    MEAN(`Milltown Mel`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Mean" && input$select == "Essex Ed") {
            overall_data %>%
                model(
                    MEAN(`Essex Ed`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Mean" && input$select == "Chattanooga Chuck") {
            overall_data %>%
                model(
                    MEAN(`Chattanooga Chuck`) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Drift" && input$select == "Groundhog") {
            overall_data %>%
                model(
                    RW(`Groundhog`~drift()) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Drift" && input$select == "Punxsutawney Phil") {
            overall_data %>%
                model(
                    RW(`Punxsutawney Phil`~drift()) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Drift" && input$select == "Milltown Mel") {
            overall_data %>%
                model(
                    RW(`Milltown Mel`~drift()) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Drift" && input$select == "Essex Ed") {
            overall_data %>%
                model(
                    RW(`Essex Ed`~drift()) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Drift" && input$select == "Chattanooga Chuck") {
            overall_data %>%
                model(
                    RW(`Chattanooga Chuck`~drift()) 
                ) %>%
                forecast(h=12) %>%
                autoplot(overall_data)
            
        }else if (input$select2 == "Exponential Smoothing - Holts" && input$select == "Groundhog") {
            overall_data %>%
                model(
                    ETS(`Groundhog` ~ error("A") + trend("N") + season("N")) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Exponential Smoothing - Holts" && input$select == "Punxsutawney Phil") {
            overall_data %>%
                model(
                    ETS(`Punxsutawney Phil` ~ error("A") + trend("N") + season("N")) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Exponential Smoothing - Holts" && input$select == "Milltown Mel") {
            overall_data %>%
                model(
                    ETS(`Milltown Mel` ~ error("A") + trend("N") + season("N")) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Exponential Smoothing - Holts" && input$select == "Essex Ed") {
            overall_data %>%
                model(
                    ETS(`Essex Ed` ~ error("A") + trend("N") + season("N")) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Exponential Smoothing - Holts" && input$select == "Chattanooga Chuck") {
            overall_data %>%
                model(
                    ETS(`Chattanooga Chuck` ~ error("A") + trend("N") + season("N")) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Exponential Smoothing - Holts/Winters" && input$select == "Groundhog") {
            overall_data %>%
                model(
                    ETS(`Groundhog` ~ error("A") + trend("A") + season("A")) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Exponential Smoothing - Holts/Winters" && input$select == "Punxsutawney Phil") {
            overall_data %>%
                model(
                    ETS(`Punxsutawney Phil` ~ error("A") + trend("A") + season("A")) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Exponential Smoothing - Holts/Winters" && input$select == "Milltown Mel") {
            overall_data %>%
                model(
                    ETS(`Milltown Mel` ~ error("A") + trend("A") + season("A")) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Exponential Smoothing - Holts/Winters" && input$select == "Essex Ed") {
            overall_data %>%
                model(
                    ETS(`Essex Ed` ~ error("A") + trend("A") + season("A")) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "Exponential Smoothing - Holts/Winters" && input$select == "Chattanooga Chuck") {
            overall_data %>%
                model(
                    ETS(`Chattanooga Chuck` ~ error("A") + trend("A") + season("A")) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "ARIMA - Auto" && input$select == "Groundhog") {
            overall_data %>%
                model(
                    ARIMA(`Groundhog`) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "ARIMA - Auto" && input$select == "Punxsutawney Phil") {
            overall_data %>%
                model(
                    ARIMA(`Punxsutawney Phil`) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "ARIMA - Auto" && input$select == "Milltown Mel") {
            overall_data %>%
                model(
                    ARIMA(`Milltown Mel`) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "ARIMA - Auto" && input$select == "Essex Ed") {
            overall_data %>%
                model(
                    ARIMA(`Essex Ed`) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "ARIMA - Auto" && input$select == "Chattanooga Chuck") {
            overall_data %>%
                model(
                    ARIMA(`Chattanooga Chuck`) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "ARIMA - Choose Parameters" && input$select == "Groundhog") {
            overall_data %>%
                model(
                    ARIMA(`Groundhog`~ pdq(input$slider1, input$slider2, input$slider3)) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "ARIMA - Choose Parameters" && input$select == "Punxsutawney Phil") {
            overall_data %>%
                model(
                    ARIMA(`Punxsutawney Phil`~ pdq(input$slider1, input$slider2, input$slider3)) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "ARIMA - Choose Parameters" && input$select == "Milltown Mel") {
            overall_data %>%
                model(
                    ARIMA(`Milltown Mel`~ pdq(input$slider1, input$slider2, input$slider3)) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "ARIMA - Choose Parameters" && input$select == "Essex Ed") {
            overall_data %>%
                model(
                    ARIMA(`Essex Ed`~ pdq(input$slider1, input$slider2, input$slider3)) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }else if (input$select2 == "ARIMA - Choose Parameters" && input$select == "Chattanooga Chuck") {
            overall_data %>%
                model(
                    ARIMA(`Chattanooga Chuck`~ pdq(input$slider1, input$slider2, input$slider3)) 
                )%>%
                forecast(h=12) %>%
                autoplot(overall_data)
        }
        
})
}
# Run the application 
shinyApp(ui = ui, server = server)
