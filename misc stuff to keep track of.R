observe({
  
  classes1 <- NULL
  
  dataX <- datasets[input$dataset]
  
  for(i in 1:ncol(dataX)){
    
    classes$col[i] <- colnames(dataX)[i]
    classes$class[i] <- class(dataX[,i]) 
  }
  
  classes <- data.frame(classes)%>%
    arrange(col)%>%
    add_row(col = "none", class = "character")
  
  dataX$none<-"1"
  
  updateSelectInput(session,"outcome",choices=classes$col[classes$class == "numeric"|classes$class == "integer"])
  
  updateSelectInput(session,"x",choices=classes$col[(classes$class == "factor" | classes$class =="character") & classes$col != "none"])
  
  updateSelectInput(session,"colour",choices=classes$col[classes$class == "factor" | classes$class =="character"])
  
  updateSelectInput(session,"facet",choices=classes$col[classes$class == "factor" | classes$class =="character"])
})

classes1 <- data.frame(
  col = colnames(data1),
  class = 
