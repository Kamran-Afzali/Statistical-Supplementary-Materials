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
library('fastDummies')
library(tidyverse)
library(haven)
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
                        selectInput("var0", 
                                    label = "Treatment",
                                    choices = c("No"=1,
                                                "Yes"=0
                                    )),
                        selectInput("var1", 
                                    label = "Sexual trauma",
                                    choices = c("No"=0,
                                                "Yes"=1
                                    )),
                        selectInput("var2", 
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
                                    choices = c("Alcohol"=1,
                                                "Cannabis"=2,
                                                "Alcohol/Cannabis"=12,
                                                "Heroin"=8,
                                                "Other"=999
                                    )),
                        ),
                    
                   
                    
                    box(title = "Age-related factors", width = 4, solidHeader = T, status = "primary", 
                        sliderInput("range1", 
                                    label = "Age:",
                                    min = 18, max = 60, value = 30),
                        sliderInput("range2", 
                                    label = "Age first got high:",
                                    min = 12, max = 60, value = 25),
                        sliderInput("range3", 
                                    label = "Age first heroin :",
                                    min = 12, max = 60, value = 25),
                        sliderInput("range4", 
                                    label = "Age when first injected any drug :",
                                    min = 12, max = 60, value = 25),
                        sliderInput("range5", 
                                    label = "Years of school completed:",
                                    min = 1, max = 12, value = 6)),
                    tabBox(
                        title = "InfoBox", width = 4, 
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", height = "250px",
                        tabPanel("Risk estimate", solidHeader = T,status = "primary",  htmlOutput("Box0")),
                        tabPanel("Risk factors", solidHeader = T,status = "warning",  tableOutput("Box1"))
                    ),
                    tabBox(
                        title = "InfoBox2", width = 4, 
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", height = "250px",
                        tabPanel("Risk estimate2", solidHeader = T,status = "primary",  textOutput("Box2")),
                        tabPanel("Risk factors2", solidHeader = T,status = "warning",  textOutput("Box3"))
                    )
                    
                ),
                fluidRow(
                    box(title = "Main risk factors", solidHeader = T, status = "primary", tableOutput("selected_var")),
                    box(title = "Risk plot", solidHeader = T, status = "primary", plotOutput("plot"))
                )   
        )
        

    ))
)


server <- function(input, output) {
    data <- reactive({c(
        Treatment=(as.numeric(as.character(input$var0))),
        Trauma=(as.numeric(as.character(input$var1))),
        Prison=(as.numeric(as.character(input$var2))),
        Alcohol_use=(as.numeric(as.character(input$var3))),
        OD=(as.numeric(as.character(input$var4))),
        Drug_Type=(as.numeric(as.character(input$var5))),
        Age =(input$range1),
        Age_high=(input$range2),
        Age_heroin=(input$range3),
        Age_injected=(input$range4),
        School=(input$range5)
    )})
    
    

    output$Box0 <- renderUI({
        dat=data()
        vect=c( dat[1]==0,
            dat[2]>0,
            dat[3]>0,
            dat[4]>0,
            dat[5]>0,
            dat[6]>0,
            dat[7]<30,
            dat[8]<13,
            dat[9]<20,
            dat[10]<17,
            dat[11]<10
        )
        HTML(paste0(name_reacts[vect], sep = '<br/>', collapse = ' '))
    })
    
    output$Box1 <- renderTable({
        dat=data()
        dat=t(dat)
        colnames(dat)=reacts
        dat=dummy_cols(dat, select_columns = "h0101b")
        dat=dat[,colnames(dat)!='h0101b']
        dat["first_inj_cat"]=1*(dat["first_inj_cat"]>17)
        dat
    })
    
    output$Box2 <- renderText({
        dat=data()
        dat=t(dat)
        colnames(dat)=reacts
        #dat["first_inj_cat"]=1*(dat["first_inj_cat"]>17)
        dat=dummy_cols(dat, select_columns = "h0101b")
        dat=dat[,colnames(dat)!='h0101b']
        dat["first_inj_cat"]=1*(dat["first_inj_cat"]>17)
        reacts2=colnames(dat)
        reacts2
    })
    
    output$Box3 <- renderText({
        dat=data()
        dat=t(dat)
        colnames(dat)=reacts
        dat=dummy_cols(dat, select_columns = "h0101b")
        dat=dat[,colnames(dat)!='h0101b']
        dat["first_inj_cat"]=as.numeric(1*(dat["first_inj_cat"]>17))
        reacts2=colnames(dat)
        sign[reacts2]=dat
        signl[reacts2]=dat
        signu[reacts2]=dat
        
        low=cleaned_mod1b%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)
        medd=cleaned_mod1b%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)
        high=cleaned_mod1b%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)

        
       paste(low,medd,high)
        
    })

    output$selected_var <- renderTable({
        dat=data()
        dat=t(dat)
        colnames(dat)=reacts
        dat=dummy_cols(dat, select_columns = "h0101b")
        dat=dat[,colnames(dat)!='h0101b']
        dat["first_inj_cat"]=1*(dat["first_inj_cat"]>17)
        reacts2=colnames(dat)
        sign[reacts2]=dat
        signl[reacts2]=dat
        signu[reacts2]=dat
        
        mattt=rbind(sign,signl,signu)
        
        return(mattt)
        })
    output$plot <- renderPlot({
        dat=data()
        vect=c( dat[1]==0,
                dat[2]>0,
                dat[3]>0,
                dat[4]>0,
                dat[5]>0,
                dat[6]>0,
                dat[7]<30,
                dat[8]<13,
                dat[9]<20,
                dat[10]<17,
                dat[11]<10
        )
        lenn=length(name_reacts[vect])
        
        dat=data()
        dat=t(dat)
        colnames(dat)=reacts
        dat=dummy_cols(dat, select_columns = "h0101b")
        dat=dat[,colnames(dat)!='h0101b']
        dat["first_inj_cat"]=as.numeric(1*(dat["first_inj_cat"]>17))
        reacts2=colnames(dat)
        sign[reacts2]=dat
        signl[reacts2]=dat
        signu[reacts2]=dat
        
        high=cleaned_mod1b%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)%>%pluck(1)
        
        
        
        p+geom_point(aes(lenn, high), shape = 23, colour = "black", fill = "white", size = 5, stroke = 5)
        
        
        
        
    })
    
}
shinyApp(ui = ui, server = server)
