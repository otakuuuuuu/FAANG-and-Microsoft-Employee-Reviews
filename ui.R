# Student name: Xue Chen
# Student ID: 30100763
# Last modified: 10 June, 2019

library(shiny)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(markdown)
library(memoise)
library(leaflet)
library(dplyr)
library(plotly)

table <- read.csv('employee.csv')
loc <- read.csv('usemployee.csv')

distemp <- data.frame(
  name = c("Amazon", "Apple", "Facebook", "Google", "Microsoft", "Netflix"),
  employees = c(24247, 10476, 1239, 6345, 12553, 462)
)

rate <- data.frame(
  CompanyName = c("Amazon", "Apple", "Facebook", "Google", "Microsoft", "Netflix"),
  OverallRating = c(6,3,1,2,4,5),
  CompBenefits = c(6,4,1,2,5,3),
  WorkLifeBalance = c(6,5,2,1,3,4),
  CareerOppotunities = c(4,5,1,2,3,6),
  Rankof6 = c(6,4,1,2,3,5)
)

companynames <- list("Amazon" = 'az', 
                     "Apple" = 'ap', 
                     "Facebook" = 'fb',
                     "Google" = 'go',
                     "Microsoft" = 'ms',
                     "Netflix" = 'nf')

Amazon <- table %>% filter(Company == "Amazon")
Apple <- table %>% filter(Company == "Apple")
Facebook <- table %>% filter(Company == "Facebook")
Google <- table %>% filter(Company == "Google")
Microsoft <- table %>% filter(Company == "Microsoft")
Netflix <- table %>% filter(Company == "Netflix")

Amazonloc <- loc %>% filter(Company == "Amazon")
Appleloc <- loc %>% filter(Company == "Apple")
Facebookloc <- loc %>% filter(Company == "Facebook")
Googleloc <- loc %>% filter(Company == "Google")
Microsoftloc <- loc %>% filter(Company == "Microsoft")
Netflixloc <- loc %>% filter(Company == "Netflix")



shinyUI(navbarPage("FAANG and Microsoft Employee Reviews",
           tabPanel("Distribution",
                    sidebarLayout(
                      sidebarPanel(
                        checkboxGroupInput("names", "Company Names:", 
                                           choices = distemp$name,
                                           selected = distemp$name)
                      ),
                      mainPanel(
                        plotlyOutput("pieplot")
                      )
                    )
           ),
           
           tabPanel("Map",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("mapname", "Choose a company:",
                                     choices = companynames, selected = 'az')),
                      mainPanel(
                        leafletOutput("mymap")
                      )
                    )),
           
           tabPanel("Ranking",
                    sidebarLayout(
                      sidebarPanel(
                        checkboxGroupInput("show_vars", "Catagories:", 
                                           names(rate), selected = (names(rate)))
                      ),
                      mainPanel(
                        tabPanel("rate", DT::dataTableOutput("mytable1"))
                      )
                    )
           ),
                    
           tabPanel("Pros&Cons",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("selection", "Choose a company:",
                                    choices = companynames, selected = "Amazon"),
                        radioButtons("procon", label = h5(""),
                                     choices = list("Pros" = 1, "Cons" = 2), 
                                     selected = 1),
                        hr(),
                        sliderInput("max",
                                    "Maximum Number of Words:",
                                    min = 1,  max = 50,  value = 20)
                      ),
                      
                      # Show Word Cloud
                      mainPanel(
                        plotOutput("woldcloudplot")
                      )
                    )
           )
)
)