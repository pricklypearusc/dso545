library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggmap)
load("~/Desktop/DSO 545/project 311 calls/workspace.RData")
## monthly request + daily/weekly peak

ui <- shinyUI(fluidPage(    

  titlePanel("Interactive Plots (Date Range: 11/28/15-11/26/16)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month:",
                  choices=c('Whole Year',month.name),selectize=FALSE),
      selectInput("source1","Request Source:",
                  choices=c("Call","Mobile App","Self Service"),selectize=FALSE),
      hr(),
      helpText("Call/Mobile App/Self Service Request in specific month")
    ),
    mainPanel(
      plotOutput("heatmap")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("source", "Request Source:",
                  choices=c('Overall',levels(data$RequestSource)),selectize=FALSE),
      hr(),
      helpText("Distribution of Request by each source")
    ),
    mainPanel(
      plotOutput("requestPlot")
    )
  ),


  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Request Type:",
                  choices=c('Overall',levels(data$RequestType)),selectize=FALSE),
      hr(),
      helpText("Distribution of Request Type submitted via Call and Mobile App")
    ),
    mainPanel(
      plotOutput("mapdistribution_plot")
    )
  )
))


server <- shinyServer(function(input, output) {
  ###call, mobile app, self service
  output$requestPlot <- renderPlot({
    # Render a barplot
    if (input$source=='Overall'){
      tmp=data %>% group_by(RequestType) %>%
        summarize(TotalNumRequest=n())
    }
    else {
      tmp=data %>% filter(RequestSource==input$source) %>%
        group_by(RequestType) %>%
        summarize(TotalNumRequest=n())
    }
    ggplot(tmp,aes(x=reorder(RequestType,-TotalNumRequest),y=TotalNumRequest))+
      geom_bar(aes(fill=RequestType),stat='identity')+
      ylab('Number of Request')+xlab("")+
      guides(fill=F)+
      ggtitle(paste("Distribution of Request by",input$source))+
      theme_minimal()+
      theme(plot.title = element_text(hjust=0.5,face='bold'),
            axis.text.x = element_text(angle = 30, hjust = 1))+
      geom_text(aes(label=TotalNumRequest),vjust=-1,size=3)
  })
  
  output$heatmap <- renderPlot({
    # Render a barplot
    if (input$month=='Whole Year'){
      tmp=data %>% filter(RequestSource ==input$source1) %>%
        mutate(day=wday(CreatedDate,label=T,abbr=F),hour=hour(CreatedDate)) %>%
        group_by(day,hour) %>%
        summarize(count=n())
    }
    else {
      tmp=data %>% filter(RequestSource==input$source1 & month(CreatedDate,label=T,abbr=F)==input$month) %>%
        mutate(day=wday(CreatedDate,label=T,abbr=F),hour=hour(CreatedDate)) %>%
        group_by(day,hour) %>%
        summarize(count=n())
    }
    ggplot(tmp,aes(day,factor(hour),fill=count)) +
      geom_tile()+
      scale_fill_gradient(low="white",high='darkred')+
      xlab("")+ylab("Hour of day")+theme_minimal()+
      theme(plot.title = element_text(hjust=0.5,face='bold'))+
      ggtitle(paste(input$month,'request by',input$source1))
  })
  
  
  
  
  output$mapdistribution_plot <- renderPlot({
    if (input$type=='Overall'){
      tmp=data %>% filter(RequestSource %in% c("Call","Mobile App"))
    }
    else {
      tmp=data %>% filter(RequestType==input$type,
                          RequestSource %in% c("Call","Mobile App"))
    }
    qmap("Los Angeles",maptype="roadmap",zoom=10)+
      stat_density2d(data=tmp,
                     aes(x=Longitude,y=Latitude,fill=..level..,alpha=..level..),
                     geom="polygon",bins=5) +
      scale_fill_gradient(guide=F)+
      scale_alpha(guide='none')+
      facet_wrap(~RequestSource)
  })
})



shinyApp(ui = ui, server = server)


