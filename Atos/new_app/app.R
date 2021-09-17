#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram

ui <- dashboardPage(
    dashboardHeader(title = "Risk Assessment"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Info", tabName = "Info", icon = icon("th"))
        )
    ),
    dashboardBody(tabItems(
        
        tabItem(tabName = "dashboard",
                fluidRow(
                    box(title = "Outcom", width = 12, solidHeader = T, status = "primary", 
                        selectInput("var6", 
                                    label = "Outcom",
                                    choices = c("HEROIN.USE_1_year"=1, "HEROIN.USE_5_year"=2, "HEROIN.USE_10_year"=3, 
                                                "SHORT.TERM.ABSTINENCE_1_year"=4,  "SHORT.TERM.ABSTINENC_5_year"=5,  "SHORT.TERM.ABSTINENCE_10_year"=6, 
                                                "MEDIUM.TERM.ABSTINENCE_5_year"=7,"MEDIUM.TERM.ABSTINENCE_10_year"=8,
                                                "LONG.TERM.ABSTINENCE_10_year"=9,
                                                " OVERDOSE_1_year"=10, " OVERDOSE_5_year"=11, " OVERDOSE_10_year"=12,
                                                "MORTALITY_10_year"=13, "MORTALITY_15_year"=14)
                                    
                        )),
                    box(title = "Risk factors", width = 4, solidHeader = T, status = "primary", 
                        selectInput("var2", 
                                    label = "Treatment",
                                    choices = c("No"=0,
                                                "Yes"=1
                                    )),
                        selectInput("var", 
                                    label = "Sexual trauma",
                                    choices = c("No"=0,
                                                "Yes"=1
                                    )),
                        selectInput("var1", 
                                    label = "Prison history",
                                    choices = c("No"=0,
                                                "Yes"=1
                                    )),
                        selectInput("var3", 
                                    label = "Past month alcohol use",
                                    choices = c("No"=0,
                                                "Yes"=1
                                    )),
                        selectInput("var4", 
                                    label = "Ever overdosed",
                                    choices = c("No"=0,
                                                "Yes"=1
                                    )),
                        selectInput("var5", 
                                    label = "Drug used for first high",
                                    choices = c("A"=0,
                                                "B"=1,
                                                "C"=2
                                    )),
                        ),
                    
                   
                    
                    box(title = "Age-related factors", width = 4, solidHeader = T, status = "primary", 
                        sliderInput("range3", 
                                    label = "Age:",
                                    min = 18, max = 60, value = 30),
                        sliderInput("range", 
                                    label = "Age first got high:",
                                    min = 12, max = 60, value = 25),
                        sliderInput("range2", 
                                    label = "Age first heroin :",
                                    min = 12, max = 60, value = 25),
                        sliderInput("range1", 
                                    label = "Age treatment :",
                                    min = 12, max = 60, value = 25),
                        sliderInput("range4", 
                                    label = "Years of school completed:",
                                    min = 1, max = 12, value = 6)),
                    tabBox(
                        title = "InfoBox", width = 4, 
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", height = "250px",
                        tabPanel("Risk estimate", solidHeader = T,status = "primary",  valueBoxOutput("Box0")),
                        tabPanel("Risk factors", solidHeader = T,status = "warning",  valueBoxOutput("Box1"))
                    ),
                    tabBox(
                        title = "InfoBox2", width = 4, 
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", height = "250px",
                        tabPanel("Risk estimate", solidHeader = T,status = "primary",  valueBoxOutput("Box0")),
                        tabPanel("Risk factors", solidHeader = T,status = "warning",  valueBoxOutput("Box1"))
                    )
                    
                ),
                fluidRow(
                    box(title = "Main risk factors", solidHeader = T, status = "primary", textOutput("selected_var")),
                    box(title = "Risk plot", solidHeader = T, status = "primary", plotOutput("plot"))
                )   
        )
        

    ))
)


server <- function(input, output) {}
shinyApp(ui = ui, server = server)
