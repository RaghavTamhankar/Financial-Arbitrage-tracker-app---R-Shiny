Show_Me_The_Money = function(Date1,Currency){
  if(as.Date(Date1) %in% R_bitrage$Date){
    A = subset(R_bitrage,Date==Date1)
    ########GBP########GBP########GBP########GBP########GBP########GBP########GBP###
    ########GBP########GBP########GBP########GBP########GBP########GBP########GBP###
    ########GBP########GBP########GBP########GBP########GBP########GBP########GBP###
    if (Currency == "GBP")  {
      Profit=c(
        A$`GBP to USD`*A$`USD to GBP`,
        A$`GBP to USD`*A$`USD to EUR`*A$`EUR to GBP`,
        A$`GBP to USD`*A$`USD to EUR`*A$`EUR to USD`*A$`USD to GBP`,
        A$`GBP to EUR`*A$`EUR to GBP`,
        A$`GBP to EUR`*A$`EUR to USD`*A$`USD to GBP`,
        A$`GBP to EUR`*A$`EUR to USD`*A$`USD to EUR`*A$`EUR to GBP`
      )
      Path=c(
        "GBP to USD to GBP",
        "GBP to USD to EUR to GBP",
        "GBP to USD to EUR to USD to GBP",
        "GBP to EUR to GBP",
        "GBP to EUR to USD to GBP",
        "GBP to EUR to USD to EUR to GBP"
      )}
    ########USD########USD########USD########USD########USD########USD########USD####
    ########USD########USD########USD########USD########USD########USD########USD####
    ########USD########USD########USD########USD########USD########USD########USD####
    else if (Currency == "USD") {
      Profit=c(
        A$`USD to GBP`*A$`GBP to USD`,
        A$`USD to GBP`*A$`GBP to EUR`*A$`EUR to USD`,
        A$`USD to GBP`*A$`GBP to EUR`*A$`EUR to GBP`*A$`GBP to USD`,
        A$`USD to EUR`*A$`EUR to USD`,
        A$`USD to EUR`*A$`EUR to GBP`*A$`GBP to USD`,
        A$`USD to EUR`*A$`EUR to GBP`*A$`GBP to EUR`*A$`EUR to USD`
      )
      Path=c(
        "USD to GBP to USD",
        "USD to GBP to EUR to USD",
        "USD to GBP to EUR to GBP to USD",
        "USD to EUR to USD",
        "USD to EUR to GBP to USD",
        "USD to EUR to GBP to EUR to USD"
      )} 
    ########EUR########EUR########EUR########EUR########EUR########EUR########EUR####
    ########EUR########EUR########EUR########EUR########EUR########EUR########EUR#### 
    ########EUR########EUR########EUR########EUR########EUR########EUR########EUR####
    else if (Currency == "EUR"){
      Profit=c(
        A$`EUR to USD`*A$`USD to EUR`,
        A$`EUR to USD`*A$`USD to GBP`*A$`GBP to EUR`,
        A$`EUR to USD`*A$`USD to GBP`*A$`GBP to USD`*A$`USD to EUR`,
        A$`EUR to GBP`*A$`GBP to EUR`,
        A$`EUR to GBP`*A$`GBP to USD`*A$`USD to EUR`,
        A$`EUR to GBP`*A$`GBP to USD`*A$`USD to GBP`*A$`GBP to EUR`
      )
      Path=c(
        "EUR to USD to EUR",
        "EUR to USD to GBP to EUR",
        "EUR to USD to GBP to USD to EUR",
        "EUR to GBP to EUR",
        "EUR to GBP to USD to EUR",
        "EUR to GBP to USD to GBP to EUR"
      )}
    #################################################################################  
    #################################################################################  
    data2=data.frame(Path,Profit)
    SortedData=data2[order(data2$Profit,decreasing =TRUE),]
    BestOption = SortedData[1,]
    money = BestOption$Profit*1000000-1000000
    
    library(ggplot2)
    return(
      ggplot(data2, aes(x=data2$Path, y=data2$Profit*1000000-1000000))+geom_point(size=5)+
        geom_point(data=BestOption, aes(x=BestOption$Path, y=BestOption$Profit*1000000-1000000), colour="red", size=8)+
        xlab("Path") + ylab("Profit")+
        ggtitle(paste("If you had one million",Currency, "'s on",Date1, "and converted it from","\n",BestOption$Path,"\n", "you would earn a profit of", money,"in",Currency,"'s")) +
        coord_flip())}
  else{return(
    ggplot()+geom_blank()+
      ggtitle("Oops, looks like the FOREX market was closed that day, go ahead and try another!"))}
}
#################################################################################  
#################################################################################
#Show_Me_The_Money("1999-09-25","GBP")

Show_Me_The_Money2 = function(Date2,Amount,CurrencyFrom,CurrencyTo){
  Currency= paste(CurrencyFrom,"to",CurrencyTo)
  if(as.Date(Date2) %in% R_bitrage$Date){
    A = subset(R_bitrage,Date==Date2)
    ########GBP########GBP########GBP########GBP########GBP########GBP########GBP###
    ########GBP########GBP########GBP########GBP########GBP########GBP########GBP###
    ########GBP########GBP########GBP########GBP########GBP########GBP########GBP###
    if (Currency == "GBP to USD")  {
      Profit=c(
        A$`GBP to USD`,
        A$`GBP to USD`*A$`USD to EUR`*A$`EUR to USD`,
        A$`GBP to EUR`*A$`EUR to GBP`*A$`GBP to USD`,
        A$`GBP to EUR`*A$`EUR to USD`
      )
      Path=c(
        "GBP to USD",
        "GBP to USD to EUR to USD",
        "GBP to EUR to GBP to USD",
        "GBP to EUR to USD"
      )}
    else if (Currency == "GBP to EUR") {
      Profit=c(
        A$`GBP to EUR`,
        A$`GBP to EUR`*A$`EUR to USD`*A$`USD to EUR`,
        A$`GBP to USD`*A$`USD to GBP`*A$`GBP to EUR`,
        A$`GBP to USD`*A$`USD to EUR`
      )
      Path=c(
        "GBP to EUR",
        "GBP to EUR to USD to EUR",
        "GBP to USD to GBP to EUR",
        "GBP to USD to EUR"
      )}
    ########USD########USD########USD########USD########USD########USD########USD####
    ########USD########USD########USD########USD########USD########USD########USD####
    ########USD########USD########USD########USD########USD########USD########USD####
    else if (Currency == "USD to GBP") {
      Profit=c(
        A$`USD to GBP`,
        A$`USD to GBP`*A$`GBP to EUR`*A$`EUR to GBP`,
        A$`USD to EUR`*A$`EUR to USD`*A$`USD to GBP`,
        A$`USD to EUR`*A$`EUR to GBP`
      )
      Path=c(
        "USD to GBP",
        "USD to GBP to EUR to GBP",
        "USD to EUR to USD to GBP",
        "USD to EUR to GBP"
      )}
    else if (Currency == "USD to EUR") {
      Profit=c(
        A$`USD to EUR`,
        A$`USD to EUR`*A$`EUR to GBP`*A$`GBP to EUR`,
        A$`USD to GBP`*A$`GBP to USD`*A$`USD to EUR`,
        A$`USD to GBP`*A$`GBP to EUR`
      )
      Path=c(
        "USD to EUR",
        "USD to EUR to GBP to EUR",
        "USD to GBP to USD to EUR",
        "USD to GBP to EUR"
      )}
    ########EUR########EUR########EUR########EUR########EUR########EUR########EUR####
    ########EUR########EUR########EUR########EUR########EUR########EUR########EUR#### 
    ########EUR########EUR########EUR########EUR########EUR########EUR########EUR####
    else if (Currency == "EUR to GBP") {
      Profit=c(
        A$`EUR to GBP`,
        A$`EUR to GBP`*A$`GBP to USD`*A$`USD to GBP`,
        A$`EUR to USD`*A$`USD to EUR`*A$`EUR to GBP`,
        A$`EUR to USD`*A$`USD to GBP`
      )
      Path=c(
        "EUR to GBP",
        "EUR to GBP to USD to GBP",
        "EUR to USD to EUR to GBP",
        "EUR to USD to GBP"
      )}
    else if (Currency == "EUR to USD") {
      Profit=c(
        A$`EUR to USD`,
        A$`EUR to USD`*A$`USD to GBP`*A$`GBP to USD`,
        A$`EUR to GBP`*A$`GBP to EUR`*A$`EUR to USD`,
        A$`EUR to GBP`*A$`GBP to USD`
      )
      Path=c(
        "EUR to USD",
        "EUR to USD to GBP to USD",
        "EUR to GBP to EUR to USD",
        "EUR to GBP to USD"
      )}
    #################################################################################  
    #################################################################################  
    data2=data.frame(Path,Profit)
    SortedData=data2[order(data2$Profit,decreasing =TRUE),]
    BestOption = SortedData[1,]
    money = BestOption$Profit*Amount
    return(paste("If you had", Amount, CurrencyFrom, "'s on",Date2, "and wanted to convert it to",CurrencyTo,",your most efficient route would be to convert from",BestOption$Path, ",providing you with", money, CurrencyTo,"'s"))}
  else{return("Oops, looks like the FOREX market was closed that day, go ahead and try another!")}
}
#################################################################################  
#################################################################################
#Show_Me_The_Money2("1999-09-21",10000,"GBP","USD")
DataGrabber = function(currency, dateRange1, dateRange2){
  B = subset(R_bitrage, Date >= min(dateRange1,dateRange2) & R_bitrage$Date <= max(dateRange2,dateRange1))
  C = B[,as.numeric(c(1,currency))]
  return(plot(C))
}
#################################################################################  
#################################################################################
#DataGrabber(6,"1999-01-04","2000-01-05")
library(shiny)
ui <- fluidPage(
  titlePanel("R_bitrage"),
  tabsetPanel(
############################################################################################
      tabPanel("Arbitrage Optimizer",
############################################################################################
            sidebarLayout(
              sidebarPanel(  
               dateInput('Date',
                         label = 'Date input: yyyy-mm-dd',
                         value = "2000-01-10",
                         max = "2017-07-27",
                         min = "1999-01-04"
               ),
############################################################################################    
               radioButtons(inputId = "Currency",
                            label = "Select Currency",
                            choices = c(USD = "USD",
                                        GBP = "GBP",
                                        EUR = "EUR"),
                            selected = "USD"
               )),
############################################################################################ 
               mainPanel(    
               plotOutput("Plot")
      ))),
############################################################################################
      tabPanel("Exchange Rate Optimizer",
############################################################################################               
          mainPanel(
                 dateInput('Date2',
                           label = 'Date input: yyyy-mm-dd',
                           value = "2016-03-18",
                           max = "2017-07-27",
                           min = "1999-01-04"
                 ),
############################################################################################    
                 radioButtons(inputId = "CurrencyFrom",
                              label = "Select Currency You Have",
                              choices = c(USD = "USD",
                                          GBP = "GBP",
                                          EUR = "EUR"),
                              selected = "USD"
                 ),
############################################################################################
                 numericInput(inputId = "Amount",
                              label = "Amount You Want to Exchange",
                              value = 100000
                 ),
############################################################################################
                 conditionalPanel(
                   condition = "input.CurrencyFrom == 'USD'",
                   selectInput(inputId = "CurrencyTo1",
                               label = "Select Currency You Want",
                               list("GBP","EUR")),
                   textOutput("Description2")
                 ),
############################################################################################
                 conditionalPanel(
                   condition = "input.CurrencyFrom == 'GBP'",
                   selectInput(inputId = "CurrencyTo2",
                               label = "Select Currency You Want",
                               list("USD","EUR")),
                   textOutput("Description3")
                 ),
############################################################################################
                 conditionalPanel(
                   condition = "input.CurrencyFrom == 'EUR'",
                   selectInput(inputId = "CurrencyTo3",
                               label = "Select Currency You Want",
                               list("GBP","USD")),
                   textOutput("Description4")
                 )
               )),
############################################################################################
      tabPanel("Trend Viewer",
############################################################################################
            mainPanel(
                dateRangeInput(inputId = 'DateRange',
                     label = 'Date Range: yyyy-mm-dd',
                     start = "2004-09-12",
                     end= "2016-02-04"
                ),
############################################################################################
                radioButtons(inputId ='currencyE', 
                             label = 'Select your conversion:',
                             choices = c('USD to GBP'=3,
                                         'GBP to USD'=2,
                                         'EUR to GBP'=7,
                                         'GBP to EUR'=6,
                                         'EUR to USD'=5,
                                         'USD to EUR'=4) 
                             ),
############################################################################################
      plotOutput("Summary")
  ))))
############################################################################################
server <- function(input, output) {
  output$Plot <- renderPlot({
    (Show_Me_The_Money(input$Date,input$Currency))
  })
  output$Description2 <- renderText({
    (Show_Me_The_Money2(input$Date2,input$Amount,input$CurrencyFrom,input$CurrencyTo1))
  })
  output$Description3 <- renderText({
    (Show_Me_The_Money2(input$Date2,input$Amount,input$CurrencyFrom,input$CurrencyTo2))
  })
  output$Description4 <- renderText({
    (Show_Me_The_Money2(input$Date2,input$Amount,input$CurrencyFrom,input$CurrencyTo3))
  }) 
  output$Summary <- renderPlot({
    DataGrabber(input$currencyE,input$DateRange[1],input$DateRange[2])
  })
}
shinyApp(ui = ui, server = server)
