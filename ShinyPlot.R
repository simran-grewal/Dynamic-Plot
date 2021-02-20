library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(readxl)
library(ggpmisc)
library(ggthemes)


#Loading Data
Revenue <-
  read_excel(
    './Documents/Data Visualization/rev.xlsx',
    sheet = 'Dynamic_Chart',
    skip = 4,
    col_names = F
  )

Games <- read_excel(
  './Documents/Data Visualization/games.xlsx',
  sheet = 'Scatter Plots')

Games <- data.frame(Games)


col_names <- c(
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December"
)

#Storing the First Column which will be used as row_names later
Apps <- Revenue$...1

#Removing the first column
Revenue[, 1] <- NULL

#Setting the Column names
colnames(Revenue) <- col_names

df <- as.data.frame(Revenue)

#Setting the row_names
row.names(df)  <- Apps

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "Dynamic Plot",
                  tabPanel("Column Chart",
                           sidebarPanel(
                             tags$h3("Select Month"),
                             selectInput("month", "Months:",col_names)
                             
                           ),
                           mainPanel(
                             h1("Monthly Revenue by Apps"),
                             h2(textOutput("result"))
                           ),
                           mainPanel(
                             plotOutput("my_plot")
                           ),
                           mainPanel(
                             dataTableOutput("my_table")
                           )
                           
                  ),
                  tabPanel("Scatter Plot",
                           mainPanel(
                             h1("Wins  Vs Salary")
                           ),
                           mainPanel(
                             plotOutput("wins_salary")
                           ),
                           mainPanel(
                             h1("Wins  Vs Homeruns")
                           ),
                           mainPanel(
                             plotOutput("wins_homeruns")
                           )
                  )
                  
                )
) 


# Define server function  
server <- function(input, output) {
  
  output$result <- renderText({
    paste( input$month)
  })
  
  output$my_plot <- renderPlot({
    ggplot(df, aes(x = Apps, y = df[,input$month], fill = Apps)) +
      geom_col() +
      geom_hline(aes(yintercept = mean(df[,input$month])),
                 linetype = "dashed",
                 color = "grey25") + 
      labs(y = "Revenue")
  })
  
  output$my_table <- renderDataTable(cbind(Apps, df[,input$month], Average = mean(df[,input$month])))
  
  output$wins_salary <- renderPlot({
    ggplot(Games, aes(x = Salary, y = Wins)) + 
      geom_point(color = "red") + 
      stat_smooth(method="lm",formula=y~log(x),fill="red", se = F) + 
      stat_poly_eq(formula = y~log(x), 
                   aes(label = paste(..rr.label.., sep = "~~~")), 
                   parse = TRUE) + theme_dark()
    
  })
  
  output$wins_homeruns <- renderPlot({
    ggplot(Games, aes(x = Homeruns, y = Wins)) + 
      geom_point(color = "red") + 
      stat_smooth(method="lm",formula=y~log(x),fill="red", se = F) + 
      stat_poly_eq(formula = y~log(x), 
                   aes(label = paste(..rr.label.., sep = "~~~")), 
                   parse = TRUE)+ theme_dark()
  })
}


# Create Shiny object
shinyApp(ui = ui, server = server)