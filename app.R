library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
#library(highcharter)
library(ggmap)
library(lubridate)
library(shinyjs)
appCSS <- "
#loader {
  position: absolute;
  left: 50%;
top: 50%;
z-index: 1;
width: 150px;
height: 150px;
margin: -75px 0 0 -75px;
border: 16px solid #f3f3f3;
border-radius: 50%;
border-top: 16px solid #3498db;
width: 120px;
height: 120px;
-webkit-animation: spin 2s linear infinite;
animation: spin 2s linear infinite;
}

@-webkit-keyframes spin {
0% { -webkit-transform: rotate(0deg); }
100% { -webkit-transform: rotate(360deg); }
}

@keyframes spin {
0% { transform: rotate(0deg); }
100% { transform: rotate(360deg); }
}
"

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 10,colour = "black",hjust=0.5),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=8),
    axis.text = element_text(size=8),
    axis.title.x = element_text(hjust=1),
    axis.title.y = element_text(hjust=1),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "bold"),
    legend.text = element_text(colour = "black", face = "bold"),
    axis.text.x = element_text(vjust=-1,angle=90),
    legend.position = "bottom")
}

mel <- get_map("melbourne",zoom=11,maptype="terrain")
house <- read.csv("house.csv",header = T,stringsAsFactors = F)
house$Lattitude <- as.numeric(house$Lattitude)
house$Longtitude <- as.numeric(house$Longtitude)
house <- house %>% rename(lat=Lattitude) %>% rename(lon=Longtitude)

nums <- sapply(house, is.numeric)
num_cols <- house[,nums]
num_cols$Suburb <- house$Suburb
num_cols_new <- num_cols %>% select(-Postcode)

house$Date <- dmy(house$Date)
house$Day <- wday(house$Date,label=T)
house$Year <- year(house$Date)
house$Month <- month(house$Date,label=T)

#Type: br - bedroom(s); h - house,cottage,villa, semi,terrace; u - unit, duplex; t - townhouse; dev site - development site; o res - other residential.

house$Type <- ifelse(house$Type=="h","House,Villa,Semi,Terrace",house$Type)
house$Type <- ifelse(house$Type=="u","Unit,Duplex",house$Type)
house$Type <- ifelse(house$Type=="t","Townhouse",house$Type)
house$Type <- ifelse(house$Type=="o res","Other Residential",house$Type)


mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    strip.text = element_text(size=12),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic",vjust=-1,angle=90))
}





ui <- dashboardPage(
  dashboardHeader(title="Melbourne Housing Prices"),
  dashboardSidebar(
    disable=T
  ),
  dashboardBody(
    useShinyjs(),
    inlineCSS(appCSS),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    # Loading message
    div(
      id = "loader",
      h2("")
    ),hidden(
      div(
        id = "app-content",
        fluidRow(column(width = 4,
                        box(width = NULL, solidHeader = TRUE,
                            plotOutput("map", height = 500,width="100%"),
                            plotlyOutput("hist",height=200)
                        ),
                        box(width = NULL,
                            selectInput("suburb","Council Area",choices=unique(house$CouncilArea),selected="Bayside")
                        )),
                 column(width=4,
                        h3("Simple Linear Model"),
                        box(width=NULL,solidHeader=TRUE,
                            plotlyOutput("model",height = 500)),
                        uiOutput("dependent1"),
                        uiOutput("dependent2"),
                        box(width=NULL,
                            uiOutput("rsq")),
                        box(width=NULL,
                            uiOutput("cbd"))),
                 column(width = 4,
                        box(width=NULL,solidHeader = TRUE,
                            plotlyOutput("bar",height=500),
                            #selectInput("time","Time",choices=c("Day","Month","Year"),selected="Day"),
                            uiOutput("sub"),
                            plotlyOutput("histtype",height=200)
                            
                        ))
        )
        
      )
    )
    
    
)
)

server <- function(input, output) {
  
  
  #Sys.sleep(1.5)
  
  # Hide the loading message when the rest of the server function has executed
  
  #Sys.sleep(1.5)
  
  #hide(id = "loading-content", anim = TRUE, animType = "fade")    
  
  show("app-content")
  
  output$map <- renderPlot({
    show(id="loader", anim = TRUE, animType = "fade")
    Sys.sleep(1.5)
    
    hide(id = "loader", anim = TRUE, animType = "fade")  
    temp <- house %>% filter(CouncilArea==input$suburb)
    temp <- na.omit(temp)
    temp <- temp %>% group_by(lat,lon) %>%
      summarise(n=mean(Price,na.rm=T))

    
    #g1 <- ggmap(mel)+stat_density2d(na.rm=TRUE,data=temp, aes(x=lon, y=lat, alpha=..level..),geom="polygon",fill="purple")+ggtitle("Density of Meteor Impacts") +
      #ggtitle(paste("House Price Density in",input$suburb))+theme(legend.position = "bottom")+scale_fill_gradient()+mapTheme()
    #try(g1,silent=T)
    g1 <- ggmap(mel) + 
      geom_point(data = temp, aes(x = lon, y = lat, color = n), alpha = 0.4,size=4) +
      scale_size_area(max_size=100)+ theme(legend.position = "right") +
      scale_color_gradientn("Sale Price", 
                            colors = c("#0DA3A0","#2D97AA","#4D8CB4","#6E81BF","#8E76C9","#AF6BD4","#CF60DE","#F055E9"),
                            labels = scales::dollar_format(prefix = "$")) +
      labs(title="Distribution of Melbourne home prices",
           subtitle="Prices in AUD",
           caption="Source: Kaggle")
    tryCatch(g1,message="Loading....")

    
    #g1 <- qmplot(lon, lat, data = temp, geom = "blank", zoom = 15, maptype = "toner-background", darken = .7, legend = "topleft") +
     # stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
      #scale_fill_gradient2("Price", low = "white", mid = "yellow", high = "red", midpoint = 650)
    
    
    
    
  })
  
  output$cbd <- renderUI({
    
    
    temp <- house %>% filter(CouncilArea==input$suburb)
    
    HTML("Mean Distance From Central Business District is ",round(mean(temp$Distance,na.rm=T),0),"KMs and the mean price is AUD ",round(mean(temp$Price,na.rm=T),0)," in ",input$suburb)
    
    
  })
  
  output$sub <- renderUI({
    
    temp <- house %>% filter(CouncilArea==input$suburb)
    selectInput("sub","Suburb",choices=unique(temp$Suburb),selected=unique(temp$suburb)[1])
    
  })
  
  
  output$dependent1 <- renderUI({
    
    show(id = "loader", anim = TRUE, animType = "fade")
    Sys.sleep(1.5)
    
    hide(id = "loader", anim = TRUE, animType = "fade")     
    selectInput("vars1","Independent",choices = names(num_cols_new),selected = "Distance",multiple = F)
    
    
  })
  output$dependent2 <- renderUI({
    
        
    selectInput("vars2","Dependent",choices = names(num_cols_new),selected = "Distance",multiple = F)
    
    
  })
  output$model <- renderPlotly({
    show(id = "loader", anim = TRUE, animType = "fade")
    Sys.sleep(1.5)
    
    hide(id = "loader", anim = TRUE, animType = "fade")    
    temp <- house %>% filter(CouncilArea==input$suburb)
    temp <- temp[,c(input$vars1,input$vars2)]
    names(temp)<- c(input$vars1,input$vars2)
    correlation <- cor(temp[,1],temp[,2])
    model<- lm(temp[,2]~temp[,1])
    est <- broom::tidy(model)$estimate
    #r2 <- broom::glance(model)$r.squared
    temp$yhat <- temp[,2]*est[2]+est[1]
    #print(head(temp))
    
  
    
    p<-temp %>%
      ggplot(aes(x=temp[,1],y=temp[,2]))+geom_point()+geom_line(aes(x=temp[,1],y=yhat))+xlab(input$vars1)+ylab(input$vars2)+ggtitle(paste("Correlation <br> between",input$vars1,"and",input$vars2,":",correlation))+plotTheme()
    tryCatch(ggplotly(p) %>%
      config(modeBarButtonsToRemove = list("sendDataToCloud","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"), displaylogo = FALSE, doubleClick = "reset"))  
    
    
    
    
    
    
    
    
    
  })
  
  output$rsq <- renderUI({
    temp <- house %>% filter(CouncilArea==input$suburb)
    temp <- temp[,c(input$vars1,input$vars2)]
    names(temp)<- c(input$vars1,input$vars2)
    correlation <- cor(temp[,1],temp[,2])
    model<- lm(temp[,1]~temp[,2])
    r2 <- broom::glance(model)$r.squared
    est <- broom::tidy(model)$estimate
    
    
    HTML("The linear model <b>",input$vars2,"=",input$vars1,"*",round(est[2],2),"+",round(est[1],2),"</b><br> Explains",round(r2*100,2),"% variation in ",input$vars2)
    
    
    
    
    
  })
  output$hist <- renderPlotly({
        
    temp <- house %>% filter(CouncilArea==input$suburb)
    
   p <-  temp %>%
      ggplot(aes(x=Distance))+geom_histogram(fill="orange")+ggtitle(paste("Distribution of Distance from CBD in:",input$suburb))+plotTheme()
   ggplotly(p) %>%
     config(modeBarButtonsToRemove = list("sendDataToCloud","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"), displaylogo = FALSE, doubleClick = "reset")  
   
    
    
    
  })
  output$bar <- renderPlotly({
    show(id = "loader", anim = TRUE, animType = "fade")
    Sys.sleep(1.5)
    
    hide(id = "loader", anim = TRUE, animType = "fade")     
    temp <- house %>% filter(CouncilArea==input$suburb)
    
    temp <- temp %>% filter(Suburb==input$sub)
    options(scipen=999)
    p <- temp %>%
      group_by(Date) %>%
      summarise(n=mean(Price,na.rm=T))%>%
      ggplot(aes(Date,y=n))+theme(axis.text.x = element_text(vjust=-1,angle=90))+geom_line()+geom_point()+ggtitle(paste(" Mean Prices Across Time in ",input$suburb,"and",input$sub,"Suburb"))+plotTheme()+ylab("$")
    
    ggplotly(p) %>%
      config(modeBarButtonsToRemove = list("sendDataToCloud","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"), displaylogo = FALSE, doubleClick = "reset")  
    
     
    
    
    
  })
  output$histtype <- renderPlotly({
    show(id = "loader", anim = TRUE, animType = "fade")
    Sys.sleep(1.5)
    
    hide(id = "loader", anim = TRUE, animType = "fade")     
    temp <- house %>% filter(CouncilArea==input$suburb)
    
    temp <- temp %>% filter(Suburb==input$sub)
    options(scipen=999)
    p <- temp %>%
      ggplot(aes(x=Price,fill=Type))+geom_histogram(alpha=0.5)+plotTheme()
    ggplotly(p) %>%
      config(modeBarButtonsToRemove = list("sendDataToCloud","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"), displaylogo = FALSE, doubleClick = "reset")  
    
    
    
    
  })
  
 
  
  
  
  
  
  
}

shinyApp(ui, server)