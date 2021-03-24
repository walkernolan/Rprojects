# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(magrittr)
library(tidyr)
library(ggplot2)

                        download.file(url = "https://drive.google.com/file/d/1j9tAGZABsooceGiq6h9D3VBG9dlBFZqm/view?usp=sharing", destfile =
                                        "~/test.csv")
                        trend_data <- read.csv(file = "~/test.csv")
                        trend_data <- trend_data[-1, ]
                        df <- trend_data[, c("time", "station", "temperature")]
                        #dft <-as.Date(df[,c("time")])
                        df[, c("time")] <- as.Date(df[, c("time")])
                        df[, c("temperature")] <- as.double(df[, c("temperature")])
                        df[is.na(df)] <- 0

                        df2 <- df %>%
                          group_by(time, station) %>%
                          select(temperature) %>%
                          summarise(temp = mean(temperature, na.rm = TRUE))

                        df2 <- data.frame(df2)


                        Q <- quantile(df2$temp, probs = c(.25, .75), na.rm = FALSE)
                        iqr <- IQR(df2$temp)
                        up <-  Q[2] + 1.5 * iqr # Upper Range
                        low <- Q[1] - 1.5 * iqr # Lower Range
                        df2 <-
                          df2[!(df2$temp < (Q[1] - 1.5 * iqr) | df2$temp > (Q[2] + 1.5 * iqr)), ]

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Water Temperature"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput('station', 'Station ID', c(df2$station)),
                    sliderInput('time', 'Start Date', min(df2$time), max(df2$time), as.Date("2009-01-01"))
                  ),          

                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput('plot'),
                    textOutput('text')
                  )
                  
                )
              )


# Define server function
server <- function(input, output, session) {

  output$plot <- renderPlot({
    df <- df2 %>% filter(station == input$station)  %>% filter(time > input$time)

    ggplot(df, aes(time, temp, colour = station))+geom_point()
  })

  output$text <- renderText({
    stat <- input$station
  })
  
}
shinyApp(ui = ui, server = server)