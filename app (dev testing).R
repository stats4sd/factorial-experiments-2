library(shiny)
library(agridat)
library(tidyverse)
library(sortable)
library(DT)

data1<-cox.stripsplit
data2<-cox.stripsplit

colnames(data2) <- c("rep", "seed", "pesticide", "nitrogen", "length")

datasets <- list(
  `Study 1` = data1,
  `Study 2` = data2
)

ui <- shinyUI(
  fluidPage(
  titlePanel("Factorial Experiments Presentation"),
  tabsetPanel(
    tabPanel("Dataset",
      sidebarLayout(
      sidebarPanel(
        selectInput("dataset", "Select Dataset", choices = names(datasets)),
        actionButton("update", "Update")
      ),
      mainPanel(
        DT::dataTableOutput("datatable"),
        DT::dataTableOutput("classes")
      )
      )),
    tabPanel("Graphs",
             uiOutput("app"))  
  )
  )
    )

server<-shinyServer(function(input, output,session){
  

  data <- reactive({
    data <- data.frame(datasets[input$dataset])
    
    data$none <- "1"
      
    return(data)

  })
  
  output$datatable <- DT::renderDataTable({
   data()
  }
  )
  
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
  }
  )
  
  output$classes <- DT::renderDataTable({
    classes()
  })
  
  choices <- reactiveValues(outcome_choices = NULL,
                            colour_choices = NULL,
                            facet_choices = NULL,
                            x_choices = NULL,
                            rank_choices = NULL)
  
  observeEvent(input$update,{
    
    choices$outcome_choices <-
    subset(classes(),class == "numeric"|class == "integer")$col
  
  
  choices$colour_choices <- 
    subset(classes(),class == "factor"|class == "character")$col

  
  choices$facet_choices <- 
    subset(classes(),class == "factor"|class == "character")$col

  choices$x_choices <-
    subset(classes(),class == "factor"|class == "character")$col
  
  })
  
  observeEvent(input$x, {
    choices$rank_choices <-
      as.character(unique(subset(data(),select = input$x)))
  })
 
  output$app = renderUI(
    fluidPage(
      
      
      titlePanel("Factorial Experiments Presentation"),
      
      
      sidebarLayout(
        sidebarPanel(
          selectInput("outcome",
                      "Select Outcome Variable", choices = choices$outcome_choices),
          selectInput("x","Select x-axis variable",choices= choices$x_choices),
          selectInput("colour","Select colour variable", choices = choices$colour_choices),
          selectInput("facet","Select facet variable", choices = choices$facet_choices),
          
          selectInput("order","Arrange x-axis by:",choices=c("Data Order"="data",
                                                             "Increasing y"="increase",
                                                             "Decreasing y"="decrease",
                                                             "Custom"="custom")),
            ),
        mainPanel(
          conditionalPanel(
            condition = "input.order == 'custom'",
            custom_sort <- rank_list(
              text = "Drag the items in desired x axis order",
              labels = choices$rank_choices,
              input_id = "custom_sort")),
          plotOutput("plot")
        )
        )
      )
  )
  
  output$plot <- renderPlot({
  ggplot()
  })
}
)

shinyApp(ui = ui, server = server)
