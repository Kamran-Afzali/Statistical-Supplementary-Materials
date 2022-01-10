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
load("~/OneDrive - Universite de Montreal/Usydney/shiny_LR_data.RData")


mod_ls=list(mod_HU_1YR,mod_HU_5YR,mod_HU_10YR,
            mod_STA_1YR,mod_STA_5YR,mod_STA_10YR,
            mod_MTA_5YR,mod_MTA_10YR,
            mod_LTA_10yr,
            mod_OD_upto1YR,mod_OD_upto5YR,mod_OD_upto10YR)

jj=list()
jj[[1]]=c(1,2,3)
jj[[2]]=c(4,5,6)
jj[[3]]=c(7,8)
jj[[4]]=c(9)
jj[[5]]=c(10,11,12)
jj[[6]]=c(13,14)
kk=c("HEROIN.USE_1_year","HEROIN.USE_5_year","HEROIN.USE_10_year","SHORT.TERM.ABSTINENCE_1_year","SHORT.TERM.ABSTINENCE_5_year","SHORT.TERM.ABSTINENCE_10_year","MEDIUM.TERM.ABSTINENCE_5_year","MEDIUM.TERM.ABSTINENCE_10_year","LONG.TERM.ABSTINENCE_10_year","OVERDOSE_1_year","OVERDOSE_5_year","OVERDOSE_10_year","MORTALITY_10_year","MORTALITY_15_year")
ss=c("Risk","Protection","Protection","Protection","Risk","Risk")
ttt=c("red","green","green","green","red","red")


nfc=function(x,lenng,inpt){ nj=jj[[x]]
kkk=c()
p2=p
for (i in nj) {
    ii=(mean(c(nj))-i)/10
    mod1_app= mod_ls[[i]]
    l=mod1_app%>% predict(inpt, type="prob")%>%select(.pred_1)%>% round(.,4)%>%pluck(1)%>%mean()
    ll=mod1_app%>% predict(inpt, type="prob")%>%select(.pred_1)%>% round(.,4)%>%pluck(1)%>%sd()
    t=kk[i]
    p2=p2+geom_point(aes_(lenng+ii, l), shape = 23, colour = "black", fill = i, size = 5, stroke = 5) + 
         geom_errorbar(aes_(x=lenng+ii, ymin=l-ll, ymax=l+ll), width=.2) +
         geom_text(aes_(x=lenng+1.75, y=l,label =t),parse = TRUE,check_overlap = TRUE)
}
p2
}

mfc=function(x,inpt){ 
    nj=jj[[x]]
    kkk=c()
    p2=p
    for (i in nj) {
        mod1_app= mod_ls[[i]]
        l=mod1_app%>% predict(inpt, type="prob")%>%select(.pred_1)%>% round(.,4)%>%pluck(1)%>%mean()
        t=kk[i]
        s=paste("risk for the outcome", t,"is", round(l*100,3), "%")
        kkk=append(kkk,s)
    }
    kkk
}

lft=function(patt,dtff){
    nnames=names(patt)
    srs=dtff%>%select(nnames)
    pat=c(patt%>%as.matrix())
    coss=apply(srs, 1, function(x) {cosine(c(as.matrix(x)), pat)}) %>% as.data.frame()
    cos_min=coss%>% top_n(5)%>% min()
    dtff_cos=dtff[which(coss>=cos_min),]
    dtff_cos[,nnames]=patt
    dtff_cos
}


ui <- dashboardPage(
    dashboardHeader(title = "Risk Assessment"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Disclaimer", tabName = "Disclaimer", icon = icon("balance-scale"))
        )
    ),
    dashboardBody(tabItems(
        
        tabItem(tabName = "dashboard",
                fluidRow(
                    box(title = "Outcom", width = 12, solidHeader = T, status = "primary", 
                        selectInput("var6", 
                                    label = "Outcom",
                                    choices = c("HEROIN.USE"=1, 
                                                "SHORT.TERM.ABSTINENCE"=2,  
                                                "MEDIUM.TERM.ABSTINENCE"=3,
                                                "LONG.TERM.ABSTINENCE"=4,
                                                " OVERDOSE"=5, 
                                                "MORTALITY"=6)
                                    
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
                        tabPanel("Risk or Protection", solidHeader = T,status = "primary",  valueBoxOutput("Box0", width = 6))
                        #,tabPanel("Risk factors", solidHeader = T,status = "warning",  tableOutput("Box1"))
                    ),
                    tabBox(
                        title = "Estimate Box", width = 4, 
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", height = "250px",
                        tabPanel("Estimate", solidHeader = T,status = "primary",  valueBoxOutput("Box2"))
                        #<tabPanel("Risk factors2", solidHeader = T,status = "warning",  textOutput("Box3"))
                    )
                    
                ),
                fluidRow(
                    tabBox(
                        title = "Report and main risk factors", 
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset3", 
                        tabPanel(title = "report", solidHeader = T, status = "primary", htmlOutput("report")),
                        tabPanel(title = "Main risk factors", solidHeader = T, status = "primary", htmlOutput("selected_var"))
                    ),
                    box(title = "Risk plot", solidHeader = T, status = "primary", plotOutput("plot"))
                )   
        )
        
        ,
        
        tabItem(tabName = "Disclaimer",
                h1("Disclaimer"),
                br(),
                h2("Disclaimer text goes here"),
                
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
    
    

    output$Box0 <- renderValueBox({
        
        dat=data()
        dat=t(dat)
        colnames(dat)=reacts
        dat=dummy_cols(dat, select_columns = "h0101b")
        dat=dat[,colnames(dat)!='h0101b']
        dat["first_inj_cat"]=as.numeric(1*(dat["first_inj_cat"]>17))
        reacts2=colnames(dat)
        # sign[reacts2]=dat
        # signl[reacts2]=dat
        # signu[reacts2]=dat
        # mod1_app=mod_ls[[as.numeric(input$var6)]]
        # high=mod1_app%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)%>%pluck(1)
        datt2=as.data.frame(dat)
        fff=lft(datt2,df)
        tixt=mfc(as.numeric(input$var6),fff)
        valueBox(
            paste0( ss[as.numeric(input$var6)]), "is being evaluated",
            color = ttt[as.numeric(input$var6)]
        )
        

    })
    
    output$Box1 <- renderTable({
        # dat=data()
        # dat=t(dat)
        # colnames(dat)=reacts
        # dat=dummy_cols(dat, select_columns = "h0101b")
        # dat=dat[,colnames(dat)!='h0101b']
        # dat["first_inj_cat"]=1*(dat["first_inj_cat"]>17)
        # dat
        dat=data()
        dat=t(dat)
        colnames(dat)=reacts
        dat=dummy_cols(dat, select_columns = "h0101b")
        dat=dat[,colnames(dat)!='h0101b']
        dat["first_inj_cat"]=1*(dat["first_inj_cat"]>17)
        reacts2=colnames(dat)
        datt2=as.data.frame(dat)
        fff=lft(datt2,df)
        return(fff)
        # sign[reacts2]=dat
        # signl[reacts2]=dat
        # signu[reacts2]=dat
        # 
        # mattt=rbind(sign,signl,signu)
        #
        
    })
    
    output$Box2 <- renderValueBox({
        dat=data()
        

        dat=t(dat)
        colnames(dat)=reacts
        #dat["first_inj_cat"]=1*(dat["first_inj_cat"]>17)
        dat=dummy_cols(dat, select_columns = "h0101b")
        dat=dat[,colnames(dat)!='h0101b']
        dat["first_inj_cat"]=1*(dat["first_inj_cat"]<17)
        
        vect=c( dat[1]==1,
                dat[2]>0,
                dat[3]>0,
                dat[4]>0,
                dat[5]>0,
                dat[6]<30,
                dat[7]<17,
                dat[8]<20,
                dat[9]>0,
                dat[10]<10,
                dat[11]>0
        )
        reacts2=colnames(dat)
        reacts3=reacts2[vect]
        mod1_app=mod_ls[[as.numeric(input$var6)]]
        kmk=as.matrix(mod1_app$fit$beta)
        kmj=exp(sum(kmk[rownames(kmk)%in%reacts3,ncol(kmk)]))
        valueBox(
            paste0( round (kmj,2)), "Odds of the outcome",
            color = "yellow"
        )
        #paste(c(reacts2,"________________________",reacts3,"________________________",kmj))
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
        
        mod1_app=mod_ls[[as.numeric(input$var6)]]
        
        low=mod1_app%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)
        medd=mod1_app%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)
        high=mod1_app%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)

        
       paste(low,medd,high)
        
    })
    
    
    output$report <- renderUI({
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
        
        mod1_app=mod_ls[[as.numeric(input$var6)]]
        
        # low=mod1_app%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)
        # medd=mod1_app%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)
        # high=mod1_app%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)
        datt2=as.data.frame(dat)
        fff=lft(datt2,df)
        rep_cont=c()
        for (i in 1:5) {
            rep_cont=append(rep_cont,mfc(i,fff))
        }
        HTML(paste0(c("Please read the disclaimer",rep_cont)  , sep = '<br/>', collapse = ' '))
        
    })
    
    
    output$selected_var <- renderUI({
        dat=data()
        vect=c( dat[1]==1,
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


    output$plot <- renderPlot({
        dat=data()
        vect=c( dat[1]==1,
                dat[2]>0,
                dat[3]>0,
                dat[4]>0,
                dat[5]>0,
                dat[6]>0,
                dat[7]<30,
                dat[8]<17,
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
        # sign[reacts2]=dat
        # signl[reacts2]=dat
        # signu[reacts2]=dat
        datt2=as.data.frame(dat)
        fff=lft(datt2,df)
        nfc(as.numeric(input$var6),lenn,fff)
        # mod1_app=mod_ls[[as.numeric(input$var6)]]
        # mod2_app=mod_ls[[as.numeric(input$var6)+1]]
        # mod3_app=mod_ls[[as.numeric(input$var6)+2]]
        # high=mod1_app%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)%>%pluck(1)
        # high2=mod2_app%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)%>%pluck(1)
        # high3=mod3_app%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)%>%pluck(1)
        # 
        # 
        # 
        # p+geom_point(aes(lenn, high), shape = 23, colour = "black", fill = "white", size = 5, stroke = 5)+
        # geom_point(aes(lenn, high2), shape = 23, colour = "blue", fill = "white", size = 5, stroke = 5)+
        # geom_point(aes(lenn, high3), shape = 23, colour = "red", fill = "white", size = 5, stroke = 5)
        
        
        
        
    })
    
}
shinyApp(ui = ui, server = server)
