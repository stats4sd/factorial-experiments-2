library(shiny)
library(agridat)
library(tidyverse)
library(sortable)

data1<-cox.stripsplit
data2<-cox.stripsplit

colnames(data2) <- c("rep", "seed", "pesticide", "nitrogen", "length")

datasets <- list(
    `Study 1` = data1,
    `Study 2` = data2
)

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

labels <- levels(data1$calcium)

ui <- fluidPage(
    
    
    titlePanel("Factorial Experiments Presentation"),
    
    
    sidebarLayout(
        sidebarPanel(
            selectInput("dataset", "Select Dataset", choices = names(datasets)),
            selectInput("outcome",
                        "Select Outcome Variable",choices = classes$col[classes$class == "numeric"|classes$class == "integer"]
            ),
            selectInput("x","Select x-axis variable",choices= classes$col[(classes$class == "factor" | classes$class =="character") & classes$col != "none"]),
            selectInput("colour","Select colour variable",classes$col[classes$class == "factor" | classes$class =="character"]),
            selectInput("facet","Select facet variable",choices=classes$col[classes$class == "factor" | classes$class =="character"]),
            
            selectInput("order","Arrange x-axis by:",choices=c("Data Order"="data",
                                                               "Increasing y"="increase",
                                                               "Decreasing y"="decrease",
                                                               "Custom"="custom")),
            conditionalPanel(
                condition = "input.order == 'custom'",
            custom_order <- rank_list(
                text = "Drag the items in desired x axis order",
                labels = labels,
                input_id = "custom_sort"
            )
        )
        ),
        
        
        mainPanel(
            plotOutput("Plot1")
        )
    )
)

server <- function(input, output, session) {
    
    output$Plot1 <- renderPlot({
        
        data1$x<-data1[,input$x]
        
        if(input$order=="increase"){
            data1$x<-reorder(data1$x,data1$yield,mean)
        }
        if(input$order=="decrease"){
            data1$x<-reorder(data1$x,-1*data1$yield,mean)
        }
        if(input$order == "custom"){
            data1$x <- fct_relevel(data1$x, input$custom_order)
        }
        
        ggplot(data=data1,aes_string(x="x",colour=input$colour,group=input$colour,y=input$outcome))+
            stat_summary(geom="line")+
            stat_summary(geom="point")+
            facet_wrap(input$facet)+
            theme_classic()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
