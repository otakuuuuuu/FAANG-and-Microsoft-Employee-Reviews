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

shinyServer(function(input, output) {
  distemp <- data.frame(
    name = c("Amazon", "Apple", "Facebook", "Google", "Microsoft", "Netflix"),
    employees = c(24247, 10476, 1239, 6345, 12553, 462)
  )
  companynames <- list("Amazon" = 'az', 
                       "Apple" = 'ap', 
                       "Facebook" = 'fb',
                       "Google" = 'go',
                       "Microsoft" = 'ms',
                       "Netflix" = 'nf')
  output$pieplot <- renderPlotly({
    df <- data.frame()
    if(any(input$names == "Amazon")){
      df <- distemp[1,]}
    if(any(input$names == "Apple")){
      df <- rbind(df, distemp[2,])}
    if(any(input$names == "Facebook")){
      df <- rbind(df, distemp[3,])}
    if(any(input$names == "Google")){
      df <- rbind(df, distemp[4,])}
    if(any(input$names == "Microsoft")){
      df <- rbind(df, distemp[5,])}
    if(any(input$names == "Netflix")){
      df <- rbind(df, distemp[6,])}
    
    plot_ly(distemp, labels = df$name, values = df$employees, type = 'pie') %>%
      layout(title = 'Distribution of Employees',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  })

  output$woldcloudplot <- renderPlot({
    Amazon <- table %>% filter(Company == "Amazon")
    Apple <- table %>% filter(Company == "Apple")
    Facebook <- table %>% filter(Company == "Facebook")
    Google <- table %>% filter(Company == "Google")
    Microsoft <- table %>% filter(Company == "Microsoft")
    Netflix <- table %>% filter(Company == "Netflix")
    
    if((input$selection == 'az') & (input$procon == 1)) {
      txt <- Amazon$Pros}
    if((input$selection == 'az') & (input$procon == 2)) {
      txt <- Amazon$Cons}
    if((input$selection == 'ap') & (input$procon == 1)) {
      txt <- Apple$Pros}
    if((input$selection == 'ap') & (input$procon == 2)) {
      txt <- Apple$Cons}
    if((input$selection == 'fb') & (input$procon == 1)) {
      txt <- Facebook$Pros}
    if((input$selection == 'fb') & (input$procon == 2)) {
      txt <- Facebook$Cons}
    if((input$selection == 'go') & (input$procon == 1)) {
      txt <- Google$Pros}
    if((input$selection == 'go') & (input$procon == 2)) {
      txt <- Google$Cons}
    if((input$selection == 'ms') & (input$procon == 1)) {
      txt <- Microsoft$Pros}
    if((input$selection == 'ms') & (input$procon == 2)) {
      txt <- Microsoft$Cons}
    if((input$selection == 'nf') & (input$procon == 1)) {
      txt <- Netflix$Pros}
    if((input$selection == 'nf') & (input$procon == 2)) {
      txt <- Netflix$Cons}
    
      myCorpus = Corpus(VectorSource(txt))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      myCorpus <- tm_map(myCorpus, toSpace, "/")
      myCorpus <- tm_map(myCorpus, toSpace, "@")
      myCorpus <- tm_map(myCorpus, toSpace, "\\|")
      myCorpus = tm_map(myCorpus, tolower)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords,
                        c(stopwords("SMART"), "the", "and", "but"))
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, stripWhitespace)
      
      myDTM = TermDocumentMatrix(myCorpus)
      m = as.matrix(myDTM)
      v = sort(rowSums(m), decreasing = TRUE)
      d <- data.frame(word = names(v),freq=v)
      wordcloud(words = d$word, freq = d$freq,
                       min.freq = 50, max.words=input$max,
                       colors=brewer.pal(8, "Dark2"))
      })
  
  output$mytable1 <- DT::renderDataTable({
    rate <- data.frame(
      CompanyName = c("Amazon", "Apple", "Facebook", "Google", "Microsoft", "Netflix"),
      OverallRating = c(6,3,1,2,4,5),
      CompBenefits = c(6,4,1,2,5,3),
      WorkLifeBalance = c(6,5,2,1,3,4),
      CareerOppotunities = c(4,5,1,2,3,6),
      Rankof6 = c(6,4,1,2,3,5)
    )
    DT::datatable(rate[, input$show_vars, drop = FALSE])
  })

  output$mymap <- renderLeaflet({ # create leaflet map
    Amazonloc <- loc %>% filter(Company == "Amazon")
    Appleloc <- loc %>% filter(Company == "Apple")
    Facebookloc <- loc %>% filter(Company == "Facebook")
    Googleloc <- loc %>% filter(Company == "Google")
    Microsoftloc <- loc %>% filter(Company == "Microsoft")
    Netflixloc <- loc %>% filter(Company == "Netflix")
    
    if(input$mapname == 'az') { marker = Amazonloc }
    if(input$mapname == 'ap') { marker = Appleloc }
    if(input$mapname == 'fb') { marker = Facebookloc }
    if(input$mapname == 'go') { marker = Googleloc }
    if(input$mapname == 'ms') { marker = Microsoftloc }
    if(input$mapname == 'nf') { marker = Netflixloc }
    
    leaflet(marker) %>% addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       radius = (sqrt(marker$Count/10)),
                       color = "red",
                       popup = ~as.character(marker$Location))
  })

})