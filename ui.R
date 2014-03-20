library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Harrison's Facebook Map"),
  
  sidebarPanel(selectInput("variable", "Variable:",
                           list("Single" = "Single", 
                                "In a relationship" = "In a relationship", 
                                "Engaged" = "Engaged",
                                "Married" = "Married",
                                "In a domestic partnerhsip" = "In a domestic partnerhsip",
                                "In an open relationship" = "In an open relationship",
                                "It's complicated" = "It's complicated"))),
  
  mainPanel()
))