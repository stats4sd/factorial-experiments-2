library(shiny)
library(agridat)
library(tidyverse)

data1<-cox.stripsplit

classes <- NULL

for(i in 1:ncol(data1)){
    
    classes$col[i] <- colnames(data1)[i]
    classes$class[i] <- class(data1[,i]) 
}

classes <- data.frame(classes)%>%
    arrange(col)%>%
    add_row(col = "none", class = "character")

data1$none<-"1"

data1$x<-data1$soil

ui <- fluidPage(
    
    
    titlePanel("Factorial Experiments Presentation"),
    
    
    sidebarLayout(
        sidebarPanel(
            selectInput("outcome",
                        "Select Outcome Variable",choices = classes$col[classes$class == "numeric"|classes$class == "integer"]
            ),
            selectInput("x","Select x-axis variable",choices= classes$col[(classes$class == "factor" | classes$class =="character") & classes$col != "none"]),
            selectInput("colour","Select colour variable",choices=classes$col[classes$class == "factor" | classes$class =="character"]),
            selectInput("facet","Select facet variable",choices=classes$col[classes$class == "factor" | classes$class =="character"]),
            
            selectInput("order","Arrange x-axis by:",choices=c("Data Order"="data",
                                                               "Increasing y"="increase",
                                                               "Decreasing y"="decrease",
                                                               "Custom"="custom")),
            htmlOutput("bucketlist")
        ),
        
        
        mainPanel(
            plotOutput("Plot1")
        )
    )
)


server <- function(input, output) {
    
    output$Plot1 <- renderPlot({
        
        data1$x<-data1[,input$x]
        
        if(input$order=="increase"){
            data1$x<-reorder(data1$x,data1$yield,mean)
        }
        if(input$order=="decrease"){
            data1$x<-reorder(data1$x,-1*data1$yield,mean)
        }
        
        
        ggplot(data=data1,aes_string(x="x",colour=input$colour,group=input$colour,y=input$outcome))+
            stat_summary(geom="line")+
            stat_summary(geom="point")+
            facet_wrap(input$facet)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
