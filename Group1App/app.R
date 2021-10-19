library(shiny)
library(shinydashboard)
library(tidyverse)



header <- dashboardHeader(title = "Body Fat Predictor")

# Input
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Inputs", tabName = "model", icon = icon("bar-chart-o"),
                 numericInput("wrist", h4("Wrist Circumference (cm)"), value = 15, min = 0, max = 100),
                 numericInput("abdomen", h4("Abdomen Circumference (cm)"), value = 80, min = 0, max = 1000),
                 radioButtons("age", h4("Age"),
                              choices = list("22-30" = 0, "30-60" = 3.8, "60+" = 1.4),
                              selected =0),
                 startExpanded = TRUE
        )
    )
    
)

ui <- dashboardPage(
    dashboardHeader(title = "Predicted body fat:"),
    sidebar,
    dashboardBody(
        fluidRow(
            valueBoxOutput("approvalBox",width = 8)
        ),
        fluidRow(
            valueBoxOutput("approvalBox1",width = 8)
        ),
        # Output the image
        imageOutput("msg"),
        # Output the information
        

        box(
            title = "Contact Information", width = 8, solidHeader = TRUE, status = "warning",
            "Qizheng Xia, qxia25@wisc.edu; JINGHAO LIU, jliu975@wisc.edu; MARIT BEVERLIE MCQUAIG, mcquaig@wisc.edu"
        )
    )
)

server <- function(input, output) {
    output$approvalBox <- renderValueBox({
        value <- -155  + as.numeric(input$age) + 62.9*log(as.numeric(input$abdomen)) - 38.3*log(as.numeric(input$wrist))
        value <- round(value,2)
        if (as.numeric(input$abdomen) <= 0 |as.numeric(input$wrist<= 0)) {
            result <- "Nan"
        }
        else{
            if (value <= 0) {
                result <- paste0(value,"%")
            }
            else{
                result <- paste0(value,"%")
            }
        }
        valueBox(
            result, "Your Body Fat", icon = icon("child",  lib="font-awesome"),
            color = "yellow"
        )
    })
    output$approvalBox1 <- renderValueBox({
        value <- -155  + as.numeric(input$age) + 62.9*log(as.numeric(input$abdomen)) - 38.3*log(as.numeric(input$wrist))
        if (as.numeric(input$abdomen) <= 0 |as.numeric(input$wrist<= 0)) {
            result <- ("Warnings: You must input the postive numbers for abdomen and wrist")
        }
        else{
            if (value <= 0) {
                result <- "Warnings: You may input some wrong numbers"
            }
            else{
                result <- NULL
            }
        }
        valueBox(
            "", result,
            color = "red"
        )
    })
    
    output$msg <- renderImage({
        list(src = 'img/ref.png',deleteFile=TRUE)
    })
    output$inf <- renderText({
        (cat("I LOVE R", "\n", "R is GREAT"))
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
