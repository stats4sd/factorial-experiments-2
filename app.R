library(shiny)
library(agridat)
library(tidyverse)
library(DT)
library(shinyjqui)
library(shinyjs)


data1<-cox.stripsplit
data2<-read.csv("factorial_data.csv")
data3<-crowder.seeds
data4<-diggle.cow
data5<-gomez.splitplot.subsample

data4[81,4] <- 300

data2$Farmer <- as.factor(data2$Farmer)
data2$Inputs <- as.factor(data2$Inputs)

datasets <- list(
    `Study 1: Barley yield` = data1,
    `Study 2: Example data (yield)` = data2,
    `Study 3: Seed germination` = data3,
    `Study 4: Cow bodyweight` = data4,
    `Study 5: Rice` = data5
)

ui <- fluidPage(
    useShinyjs(),
    tags$head(tags$style(
        HTML('
         #sidebar {
            background-color: #A9D8E4;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
    )),
    titlePanel("Factorial Experiments Presentation"),
    sidebarLayout(position = "right",
        sidebarPanel(id = "sidebar",
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
                  actionButton("show_des", "See data description"),
                  hidden(
                      div(id = "des_div",
                      verbatimTextOutput("data_descrip")
                      )
                      ),
                  plotOutput("plot",height = "800px")
        )
    )
)




server<-function(input, output,session){
    
    data <- reactive({
        data <- datasets[input$dataset][[1]]
        
        data$none <- "1"
        
        return(data)
        
    })
    
    observeEvent(input$show_des, {
        
        toggle(id = "des_div")
        
        output$data_descrip <- renderPrint({
            str(data())
        })
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
        
        updateSelectInput(session = session, inputId = "outcome",choices = colnames(data())[sapply(data(),class)%in%c("numeric","integer")])
        updateSelectInput(session = session, inputId = "x",choices = colnames(data())[!sapply(data(),class)%in%c("numeric","integer")])
        updateSelectInput(session = session, inputId = "colour",choices = colnames(data())[!sapply(data(),class)%in%c("numeric","integer")],
                          selected = "none")
        updateSelectInput(session = session, inputId = "facet",choices = colnames(data())[!sapply(data(),class)%in%c("numeric","integer")],
                          selected = "none")
        
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
        if((input$colour == input$facet | input$x == input$colour | input$x == input$facet) & (input$colour != "none"  | input$facet != "none")){ #Having some trouble 
            HTML("<b>Please do not choose the same variable for more than one option</b>")
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
        
        if((input$colour == input$facet | input$x == input$colour | input$x == input$facet) & 
           (input$colour != "none"  | input$facet != "none")){
            ggplot()
            
        }else if(input$colour == "none" & input$facet == "none"){
            
            ggplot(data=data,aes_string(x=input$x,y=input$outcome, colour = input$colour,group=input$colour))+
                stat_summary(geom="line", width = 1, show.legend = FALSE)+
                stat_summary(geom="point", size = 3, show.legend = FALSE)+
                theme(axis.title = element_text(size = 15),
                      axis.text = element_text(size = 12),
                      strip.text = element_text(size = 15),
                      panel.background = element_rect(fill = "gray100", colour = "gray100"),
                      panel.grid.major = element_line(linetype = "solid", size = 0.25, colour = "gray80"),
                      panel.grid.minor = element_line(colour = "gray100"),
                      strip.background = element_rect(fill = "white",
                                                      colour = "darksalmon"),
                      plot.background = element_rect(colour = "black"))
            
        }else if(input$facet == "none"){
            
            ggplot(data=data,aes_string(x=input$x,colour=input$colour,group=input$colour,y=input$outcome))+
                stat_summary(geom="line", width = 1)+
                stat_summary(geom="point", size = 3)+
                theme(axis.title = element_text(size = 15),
                      axis.text = element_text(size = 12),
                      strip.text = element_text(size = 15),
                      panel.background = element_rect(fill = "gray100", colour = "gray100"),
                      panel.grid.major = element_line(linetype = "solid", size = 0.25, colour = "gray80"),
                      panel.grid.minor = element_line(colour = "gray100"),
                      strip.background = element_rect(fill = "white",
                                                      colour = "darksalmon"),
                      plot.background = element_rect(colour = "black"))
            
        }else if(input$colour == "none"){
            
            ggplot(data=data,aes_string(x=input$x,y=input$outcome, colour = input$colour,group=input$colour))+
                stat_summary(geom="line", width = 1, show.legend = FALSE)+
                stat_summary(geom="point", size = 3, show.legend = FALSE)+
                facet_wrap(input$facet)+
                theme(axis.title = element_text(size = 15),
                      axis.text = element_text(size = 12),
                      strip.text = element_text(size = 15),
                      panel.background = element_rect(fill = "gray100", colour = "gray100"),
                      panel.grid.major = element_line(linetype = "solid", size = 0.25, colour = "gray80"),
                      panel.grid.minor = element_line(colour = "gray100"),
                      strip.background = element_rect(fill = "white",
                                                      colour = "darksalmon"),
                      plot.background = element_rect(colour = "black"))
            
        }else{
            
            ggplot(data=data,aes_string(x=input$x,colour=input$colour,group=input$colour,y=input$outcome))+
                stat_summary(geom="line", width = 1)+
                stat_summary(geom="point", size = 3)+
                facet_wrap(input$facet)+
                theme(axis.title = element_text(size = 15),
                      axis.text = element_text(size = 12),
                      strip.text = element_text(size = 15),
                      panel.background = element_rect(fill = "gray100", colour = "gray100"),
                      panel.grid.major = element_line(linetype = "solid", size = 0.25, colour = "gray80"),
                      panel.grid.minor = element_line(colour = "gray100"),
                      strip.background = element_rect(fill = "white",
                                                      colour = "darksalmon"),
                      plot.background = element_rect(colour = "black")
                )
        }
    })
    
}

shinyApp(ui = ui, server = server)
