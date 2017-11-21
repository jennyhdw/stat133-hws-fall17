# ===================================================================
# Title: gradevis.R
# Description:
#   This script creates interactive Shiny apps with 3 different tabs
# Author: Jenny Huang
# Date: 11-14-2017
# ===================================================================
library(shiny)
library(ggplot2)
library(ggvis)
source('../code/functions.R')

cleanscores <- read.csv("../data/cleandata/cleanscores.csv")

freq_table <- data.frame(table(cleanscores$Grade))
freq_table$Prop <- freq_table$Freq/334
names(freq_table)[1] <- paste("Grade")
freq_table <- freq_table[c(3,1,2,6,4,5,9,7,8,10,11),]
freq_table$Prop <- round(freq_table$Prop,2)


vec <- colnames(cleanscores)
vec <- vec[1:22]

ui <- fluidPage(
  
  titlePanel("Grade Visualizer"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1",
                       h3("Grades Distribution"),
                       tableOutput('table')
      ),
      conditionalPanel(condition = "input.tabselected==2",
                       selectInput("HW", "X-axis variable", vec, 
                                   selected = "HW1"),
                       sliderInput("BinWidth", "Bin Width", 
                                   min = 1, max = 10, value = 10)
      ),
      conditionalPanel(condition = "input.tabselected==3",
                       selectInput("X", "X-axis variable", vec, 
                                   selected = "Test1"),
                       selectInput("Y", "Y-axis variable",  vec, 
                                   selected = "Overall"),
                       sliderInput("Opacity", "Opacity", 
                                   min = 0, max = 1, value = 0.5))
    )
    ,
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel('Barchart',value = 1,
                           ggvisOutput("bar")),
                  tabPanel("Histogram",value = 2,
                           ggvisOutput("hist"),
                           textOutput("text1"),
                           verbatimTextOutput("Summary")),
                  tabPanel("Scatterplot",value = 3,
                           ggvisOutput("scatter"),
                           textOutput("text2"),
                           verbatimTextOutput("corr")),
                  id = "tabselected")
    )
  )
)


server <- function(input, output){
  output$table <- renderTable({freq_table})
  
  vis_bar <- reactive({
    fac_grade <- factor(cleanscores$Grade)
    fac_grade <- factor(fac_grade,levels(fac_grade)[c(3,1,2,6,4,5,9,7,8,10,11)])
    fac_grade <- data.frame(table(fac_grade))
    names(fac_grade)[1] <- paste("Grade")
    layer_bars(ggvis(fac_grade, ~Grade,~Freq)) 
  })
  vis_bar %>% bind_shiny("bar")
  
  vis_hist <- reactive({
    hw <- prop("x", as.symbol(input$HW))
    cleanscores %>% 
      ggvis(x = hw, fill := "#abafb5") %>% 
      layer_histograms(stroke := 'white',
                       width = input$BinWidth) 
  })
  vis_hist %>% bind_shiny("hist")
  
  output$text1 <- renderText({
    "Summary Statistics"
  })
  
  output$Summary <- renderPrint({
    hw <- which(names(cleanscores) == input$HW)
    print_stats(cleanscores[,hw])
  })
  
  
  vis_scatter <- reactive({
    colmx <- which(names(cleanscores) == input$X)
    colmy <- which(names(cleanscores) == input$Y)
    cleanscores %>%
      ggvis(x = ~cleanscores[,colmx], y = ~cleanscores[,colmy], opacity := input$Opacity) %>%
      layer_points() %>%
      add_axis("x", title = names(cleanscores)[colmx]) %>%
      add_axis("y", title = names(cleanscores)[colmy]) 
  })
  vis_scatter %>% bind_shiny("scatter")
  
  output$text2 <- renderText({
    "Correlation:"
  })
  
  output$corr <- renderPrint({
    colmx <- which(names(cleanscores) == input$X)
    colmy <- which(names(cleanscores) == input$Y)
    cat(cor(cleanscores[,colmx],cleanscores[,colmy]))
  })
}


shinyApp(ui = ui, server = server)