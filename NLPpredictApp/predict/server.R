#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tokenizers)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  word.predict <- function(x,data){
    data.new <- paste(data,collapse = " ")
    x <- tolower(x)
    s <- strsplit(x,split = " ")
    temp <- s[[1]]
    len <- length(temp)
    data_gram <- tokenize_ngrams(data.new,n=len+1)[[1]]
    match1 <- c()
    for (i in 1:length(data_gram)){
      if (grepl(x,minusone(data_gram[i]))){
        match1 <- c(match1,last_word(data_gram[i]))
      }
    }
    match1
  }
  
  output$value <- renderText({ word.predict(input$source,data1) })
  })

