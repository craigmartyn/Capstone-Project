library(shiny)

shinyUI(navbarPage("Word Prediction Model",
        tabPanel("Model", pageWithSidebar(
                headerPanel("Word Prediction Model"),
                sidebarPanel(
                        textInput("text", "Enter Text:"),
                        sliderInput("freq",
                                    "Words to Display:",
                                    min = 1,  max = 50, value = 50),
                        submitButton("Update!"),
                        h3(" "),
                        strong("Predicted Next Word:"),
                        h3(textOutput("nextword"))
                ),
                mainPanel(
                        plotOutput("plot")
                )
        )),
        tabPanel("Documentation", fluidPage(
                ##Show documentation
                htmlOutput('documentation'))
##                h5("Documentation is posted at: ", a("www.yahoo.com", href="http://www.yahoo.com")))
        )
))