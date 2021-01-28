library(shiny)
library(agridat)
library(tidyverse)
library(sortable)
library(DT)
library(shinyjqui)


data1<-cox.stripsplit
data2<-cox.stripsplit

colnames(data2) <- c("rep", "seed", "pesticide", "nitrogen", "length")



datasets <- list(
  `Study 1` = data1,
  `Study 2` = data2
)

ui <- fluidPage(
    titlePanel("Factorial Experiments Presentation"),
         sidebarLayout(
                 sidebarPanel(
                   selectInput("dataset", "Select Dataset", choices = names(datasets)),
                       selectInput("outcome",
                                   "Select Outcome Variable", choices = colnames(datasets[[1]])[sapply(datasets[[1]],class)%in%c("numeric","integer")]),
                       selectInput("x","Select x-axis variable",choices= colnames(datasets[[1]])[!sapply(datasets[[1]],class)%in%c("numeric","integer")]),
                       selectInput("colour","Select colour variable", choices = colnames(datasets[[1]])[!sapply(datasets[[1]],class)%in%c("numeric","integer")]),
                       selectInput("facet","Select facet variable", colnames(datasets[[1]])[!sapply(datasets[[1]],class)%in%c("numeric","integer")]),
                       
                       selectInput("order","Arrange x-axis by:",choices=c("Data Order"="data",
                                                                          "Increasing y"="increase",
                                                                          "Decreasing y"="decrease",
                                                                          "Custom"="custom")),
                        conditionalPanel(
                         condition = "input.order == 'custom'",
                         custom_sort <- orderInput(
                           label  = "Drag the items in desired x axis order",
                           items = NULL,
                           inputId = "custom_sort"))
                   ),
                   mainPanel(span(htmlOutput("error_message"),style = "color:red"),
                             plotOutput("plot")
                     )
                   )
                 )

    


server<-function(input, output,session){

  data <- reactive({
    data <- datasets[input$dataset][[1]]
    
    data$none <- "1"
    
     return(data)
    
  })
  
  classes <- reactive({
    data <- data()
    
    classes <- NULL
    
    for(i in 1:ncol(data)){
      classes$col[i] <- colnames(data)[i]
      classes$class[i] <- class(data[,i]) 
    }
    
    classes <- data.frame(classes)%>%
      arrange(col)
    
    return(classes)
  })
  
  
  choices <- reactiveValues(outcome_choices = NULL,
                            colour_choices = NULL,
                            facet_choices = NULL,
                            x_choices = NULL,
                            rank_choices = NULL)
  
  observeEvent(input$dataset,{
    
    updateSelectInput(session = session, inputId = "outcome",choices = colnames(data())[sapply(data1,class)%in%c("numeric","integer")])
    updateSelectInput(session = session, inputId = "x",choices = colnames(data())[!sapply(data1,class)%in%c("numeric","integer")])
    updateSelectInput(session = session, inputId = "colour",choices = colnames(data())[!sapply(data1,class)%in%c("numeric","integer")])
    updateSelectInput(session = session, inputId = "facet",choices = colnames(data())[!sapply(data1,class)%in%c("numeric","integer")])
    
      })
  
  
  observeEvent(input$x,{
    if(input$x!="none"){
      d1<-data()
      d1[,input$x] <- as.character(d1[,input$x])
      choices<-unique(d1[,input$x])
    updateOrderInput(session = session, inputId = "custom_sort",items = choices)
}
    
  })
  
  output$error_message <- renderText({
    if((input$colour == input$facet | input$colour == input$x | input$x == input$facet) &
       (input$colour != "none" & input$facet != "none")){
      HTML("<b>Please do not choose the same variable for more than one option</b>")
      
      print(input$custom_sort)
    }
  })
  
  
  
  output$plot <- renderPlot({
    
    data <- data()
    
    if(input$order=="increase"){
      data[,input$x] <- reorder(data[,input$x], data[,input$outcome])
    }
    if(input$order=="decrease"){
      data[,input$x] <- reorder(data[,input$x], -1*data[,input$outcome])
    }
    if(input$order == "custom"){
      data[,input$x] <- fct_relevel(data[,input$x],input$custom_sort)
    }
    
    ggplot(data=data,aes_string(x=input$x,colour=input$colour,group=input$colour,y=input$outcome))+
      stat_summary(geom="line")+
      stat_summary(geom="point")+
      facet_wrap(input$facet)+
      theme_classic()
  })
  
  }





shinyApp(ui = ui, server = server)
