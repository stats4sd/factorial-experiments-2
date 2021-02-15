library(shiny)
library(agridat)
library(tidyverse)
library(DT)
library(shinyjqui)
library(shinyjs)
library(shinydashboard)
library(shinyAce)

init = "data"

data1<-cox.stripsplit
data2<-read.csv("factorial_data.csv")
data3<-crowder.seeds
data4<-diggle.cow
data5<-gomez.splitplot.subsample

data4[81,4] <- 300

data2$Farmer <- as.factor(data2$Farmer)
data2$Inputs <- as.factor(data2$Inputs)
data2 <- select(data2, - ranef)

Oudatasets <- list(
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
        }
        
        body, label, input, button, select { 
          font-size: 12px;
        }
        
        .form-group {
            margin-bottom: 0 !important;
        }
          
        ')
    )),
    titlePanel("Exploring Presentation of Factorial Experiments"),
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
        mainPanel(
            tabsetPanel(
                tabPanel("Instructions",
                htmlOutput("instructions")),
                tabPanel("App",
                    span(htmlOutput("error_message"),style = "color:red"),
                  actionButton("show_des", "See data description"),
                  hidden(
                      div(id = "des_div",
                      verbatimTextOutput("data_descrip")
                      )
                      ),
                  actionButton("show_code", "See plotting code"),
                  hidden(
                      div(id = "code_div",
                          aceEditor(outputId = "ace",
                                    selectionId = "selection",
                                    value = init,
                                    fontSize = 12,
                                    height = "75px")
                          )
                      ),
                  plotOutput("plot",height = "600px")
                )
        )
    )
)
)

server<-function(input, output,session){
    
    
    output$instructions <- renderText({
        HTML("<head>
                 <title>Instructions</title>
                 </head>
                 <body>
                 
                 <h1>Instructions</h1>
                 
                 <p>This app is designed to help you explore how you can visualise data from factorial experiments. You will be able to choose an example dataset and then use the drop down lists to select which variables you would like to plot onto the graph which will appear on the page.</p>
                 
                 <p>To show the graph, please click onto the 'App' button above.</p>
                 
                 <h2>Data</h2>
                 
                 <p>This app has been loaded with 5 example datasets</p>
                 
                 <p>Study 1: Barely yield - A study looking at yields of barley by different levels of replication, soil type, fertiliser and calcium levels.</p>
                 <p>Study 2: Barely yield - A  simulated study looking at yields by different levels of inputs, famers, farmer gender and sites.</p>
                 <p>Study 3: Seed germination - A study looking at germination of Orobanche seeds by different levels of replication, genotype, and extract (bean or cucumber).</p>
                 <p>Study 4: Cow bodyweight - A study looking at weught if cows by different levels of iron and infection.</p>
                 <p>Study 5: Rice - A study (split-plot) looking at heights of riceplants by different levels of time, management, and rep/block.</p>
                 
                 <h2>How to create your plot</h2>
                 
                 <p></p>You can use the dropdown menus along the side to build your plot.</p>
                 
                 <ul>
                 <li>Dataset - select from one of 5 datasets</li>
                 <li>Outcome - This will be a numerical variable that has been measured such as yield. This will be plotted on the y axis</li>
                 <li>X - A categorical vaiable that will be plotted on the x axis</li>
                 <li>Colour - A categorical variable to be used to split the data with each level being a different colour</li>
                 <li>Facet - A categorical variable which will split the data into different subsets that will be plotted on smaller individual plots.</li>
                 <li>Order - Used to order the x axis</li>
                 <ul>
                 <li>Data order - The order that the data appears in R. Either alphabetical or by pre-defined order of factors</li>
                 <li>Increasing y - x axis ordered by increasing value of the outcome variable</li>
                 <li>Decreasing y - x axis ordered by descreasing value of the outcome variable</li>
                 <li>Custom - A custom order that you can define.</li>
                 </ul>
                 </ul>
                 
                 <p>You can also see the data structure by clicking on the 'See data description' button.</p>
                 
                 <p>You can see the basic R code by clicking on the 'See plotting code' button.</p>
                 
                 <p>You can select 'none'; for colour or facet to not make use of these options.</p>
                 
                 <p>Please do NOT choose the same variable for more than one option except for 'none'.</p>
                 
                 </body>")
    })
    
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
   
   observeEvent(input$show_code, {
       
       toggle("code_div")}
   )   
   
   observeEvent(c(input$dataset, input$facet, input$x, input$outcome, input$colour),
                  {
       if(input$colour == "none" & input$facet == "none"){
           
           code <- paste0("ggplot(data=`", input$dataset,"`, aes_string(x=", input$x ,", y= ", input$outcome, ", colour = "
           , input$colour, ", group = ", input$colour, "))+
            stat_summary(geom='line', width = 1, show.legend = FALSE)+
            stat_summary(geom='point', size = 3, show.legend = FALSE))")
       }else if(input$facet == "none"){
           code <- paste0("ggplot(data=`", input$dataset,"`, aes_string(x=", input$x ,", y= ", input$outcome, ", colour = "
                         , input$colour, ", group = ", input$colour, "))+
            stat_summary(geom='line', width = 1)+
            stat_summary(geom='point', size = 3)")
       }else if (input$colour == "none"){
           code <- paste0("ggplot(data=`", input$dataset,"`, aes_string(x=", input$x ,", y= ", input$outcome, ", colour = "
                         , input$colour, ", group = ", input$colour, "))+
            stat_summary(geom='line', width = 1, show.legend = FALSE)+
            stat_summary(geom='point', size = 3, show.legend = FALSE)+
            facet_wrap(~", input$facet, ")")
       }else{
           code <- paste0("ggplot(data=`", input$dataset,"`, aes_string(x=", input$x ,", y= ", input$outcome, ", colour = "
                         , input$colour, ", group = ", input$colour, "))+
            stat_summary(geom='line', width = 1)+
            stat_summary(geom='point', size = 3)+
            facet_wrap(~", input$facet, ")")
       }
       
       updateAceEditor(session, editorId = "ace", value = code)
       
       
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
