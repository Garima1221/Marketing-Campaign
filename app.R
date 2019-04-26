#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(caret)
library(plotly)
library(curl)
library(RCurl)
library(e1071)
library(rpart)
library(ranger)
load("RandomeForestModel.rda")

ui <- dashboardPage(
                    dashboardHeader(title = "Marketing Campaign"
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Profile Details", tabName = "Profile Details", icon = icon("dashboard"))
                      )
                    ),
                    dashboardBody(
                        tabItem(tabName = "Profile Details",
                                h2("Details"),
                                fluidRow(
                                  box(title = "Age",
                                      height = 120,width = 3,
                                      background = "light-blue",
                                      numericInput("age", label = h6(""), value = 18)
                                  ),
                                  box(title = "Job",
                                      height = 120,width = 3,background = "light-blue",
                                      selectInput(inputId = "job_cat",label=h6(""),
                                                  choices = c("management","technician","entrepreneur","blue-collar","unknown","retired","admin.","services","self-employed","unemployed","housemaid","student"),selected = "management")
                                  ),
                                  box(title = "Marital Status",
                                      height = 120,width = 3,background = "light-blue",
                                      selectInput(inputId = "marital",label=h6(""),
                                                  choices = c("married","single","divorced"),
                                                              selected = "single")
                                  ),
                                  box(title = "Education",
                                      height = 120,width = 3,background = "light-blue",
                                      selectInput(inputId = "education",label=h6(""),
                                                  choices = c("primary","secondary","tertiary","unknown"
                                                    ),selected = "primary")
                                  ),
                                  box(title = "Has Defaulted",
                                      height = 120,width = 3,background = "light-blue",
                                      selectInput(inputId = "default",label=h6(""),
                                                  choices = c("yes","no"),selected = "yes")
                                  ),
                                  box(title = "Has Existing Loan",
                                      height = 120,width = 3,background = "light-blue",
                                      selectInput(inputId = "loan",label=h6(""),
                                                  choices = c("yes","no"),selected = "yes")
                                  ),
                                  box(title = "Mode of contact",
                                      height = 120,width = 3,background = "light-blue",
                                      selectInput(inputId = "contact",label=h6(""),
                                                  choices = c("cellular","telephone"),selected = "cellular")
                                  ),
                                  box(title = "Month",
                                      height = 120,width = 3,background = "light-blue",
                                      selectInput(inputId = "month",label=h6(""),
                                                  choices = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),selected = "jan")
                                  ),
                                  box(title = "Day(1-31)",
                                      height = 120,width = 3,background = "light-blue",
                                      numericInput("day", label = h6(""), value = 1)
                                  ),
                                  box(title = "Duration",
                                      height = 120,width = 3,background = "light-blue",
                                      numericInput("duration", label = h6(""), value = 0)
                                  ),
                                  box(title = "Outcome",
                                      height = 120,width = 3,background = "light-blue",
                                      selectInput(inputId = "poutcome",label=h6(""),
                                                  choices = c("failure","success","other","unknown"),selected = "jan")
                                  ),
                                  box(title = "Previous",
                                      height = 120,width = 3,background = "light-blue",
                                      numericInput("previous", label = h6(""), value = 0)
                                  ),
                                  box(
                                      
                                      submitButton("Submit")
                                      
                                                                  
                                ),
                                tableOutput("value") 
                        )
                        
                                )
                        )
                      
                    
)

server <- function(input, output) {
prediction = reactive({
temp=10
test = data.frame(input$age,input$job_cat,input$marital,input$education,input$default,input$loan,input$contact,input$month,input$day,input$duration,input$outcome,input$previous)
colnames(test) = c("age","job_cat","marital","education","default","loan","contact","month","day","duration","outcome","previous")
for(i in c(categorical_features)){
  if(class(test[,i]) == 'factor'){
    test[,i] = factor(test[,i],labels = 1:length(levels(factor(test[,i]))))
  }
}
output_prediction = predict(model.RF,test)
output_prediction
})

#output$value = renderTable({
  #test = data.frame(input$age,input$job_cat,input$marital,input$education,input$default,input$loan,input$contact,input$month,input$day,input$duration,input$outcome,input$previous)
  #test
  prediction()
  #input$age
  
#})
output$value <- renderText({paste("selected", prediction)})
}
shinyApp(ui, server)