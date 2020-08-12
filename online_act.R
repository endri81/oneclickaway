library(shiny)
library(ggplot2)

linkedScatterUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, plotOutput(ns("plot1"), brush = ns("brush"))),
    column(6, plotOutput(ns("plot2"), brush = ns("brush")))
  )
}

linkedScatter <- function(input, output, session, data, left, right) {
  # Yields the data frame with an additional column "selected_"
  # that indicates whether that observation is brushed

    output$plot1 <- renderPlot({
p <- ggplot(lim_internet, aes(x=Reason, y=Total, fill=Total)) +
        +     geom_bar(stat="identity")+theme_minimal()
print(p)  

})
  
  output$plot2 <- renderPlot({
    p < - ggplot(lim_internet, aes(x=Male, y=Female, fill=Reason)) +
      geom_bar(stat="identity")+
      geom_text(aes(y=label_ypos, label=len), vjust=1.6, 
                color="white", size=3.5)+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()
    print(p)  
  })
  
  return(dataWithSelection)
}

scatterPlot <- function(data, cols) {
  ggplot(data, aes_string(x = cols[1], y = cols[2])) +
    geom_point(aes(color = selected_)) +
    scale_color_manual(values = c("black", "#66D65C"), guide = FALSE)
}