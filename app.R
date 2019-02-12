# sample R + Shiny example for CS 424 Spring 2019 UIC - Andy Johnson
# www.evl.uic.edu/aej/424

# This is a sample dashboard making use of the evl room temperature data and displaying
# it in a variery of ways to show off some of the different capabilities of R and Shiny
# and the Shiny Dashboard.

#libraries to include

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)


# assume all of the csv files in this directory are data of the same kind that I want to visualize

files = list.files(pattern="*.csv")

allData = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

aqs_sites <- read.csv(file="map/aqs_sites.csv", header=TRUE, sep=",")

# Create the menu items to select the different years 
years<-c(1980:2018)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            wellPanel(
            selectInput("Year", "Select the year to visualize", years),
            uiOutput("State"),
            uiOutput("County")),
            fluidRow(
              tabBox(width=4,
                     side = "left", height = "1200",id = "tabset1",
                     selected = "AQI Index",
                     tabPanel("Pie-AQI",plotOutput("pie", height = 1200)
                     ),
                     tabPanel("Bar-AQI", plotOutput("bar", height = 1200)
                     ),
                     tabPanel("Table-AQI",dataTableOutput("table", height = 1200)
                     )
              ),
              tabBox(width=4,
                     title = "Main Pollutant ",
                     side = "left", height = "1200",id = "tabset2",
                     selected = "PieChart",
                     tabPanel("Pie-CO",plotOutput("pieCO", height = 1200)
                     ),
                     tabPanel("Pie-NO2",plotOutput("pieNO2", height = 1200)
                     ),
                     tabPanel("Pie-Ozone",plotOutput("pieOzone", height = 1200)
                     ),
                     tabPanel("Pie-SO2",plotOutput("pieSO2", height = 1200)
                     ),
                     tabPanel("Pie-PM2.5",plotOutput("piePM2.5", height = 1200)
                     ),
                     tabPanel("Pie-PM10",plotOutput("piePM10", height = 1200)
                     ),
                     tabPanel("BarChart",plotOutput("bar2", height = 1200)
                     ),
                     tabPanel("Table",dataTableOutput("table2", height = 1200)
                     )
              ),
              tabBox(width=4,
                     title = "AQI/Pollutant across years",
                     side = "left", height = "400px",id = "tabset3",
                     selected = "LineGraph1",
                     tabPanel("AQI-Graph",plotOutput("line1", height = 1200)
                     ),
                     tabPanel("Pollutant-Graph",plotOutput("line2", height = 1200)
                     ),
                     tabPanel("Map",leafletOutput("leaf", height = 1200)
                     ),
                     tabPanel("Pollutant-CO",dataTableOutput("tableCO", height = 1200)
                     ),
                     tabPanel("Pollutant-NO2",dataTableOutput("tableNO2", height = 1200)
                     ),
                     tabPanel("Pollutant-Ozone",dataTableOutput("tableOzone", height = 1200)
                     ),
                     tabPanel("Pollutant-SO2",dataTableOutput("tableSO2", height = 1200)
                     ),
                     tabPanel("Pollutant-PM2.5",dataTableOutput("tablePM2.5", height = 1200)
                     ),
                     tabPanel("Pollutant-PM10",dataTableOutput("tablePM10", height = 1200)
                     )
                  )
                )
              ),
      tabItem(tabName = "subitem1",
              fluidRow(
                box(width=4,
                    wellPanel(
                      selectInput("Year1", "Select the year to visualize", years),
                      uiOutput("State1"),
                      uiOutput("County1")
                    )
                ),
                box(width=4,
                wellPanel(
                    selectInput("Year2", "Select the year to visualize", years),
                    uiOutput("State2"),
                    uiOutput("County2"))
                ),
                box(width=4,
                wellPanel(
                  selectInput("Year3", "Select the year to visualize", years),
                  uiOutput("State3"),
                  uiOutput("County3"))
                )
              ),
              fluidRow(
                tabBox(width=4,
                       side = "left", height = "1200",id = "tabsetCmp11",
                       selected = "AQI Index",
                       tabPanel("Pie-AQI",plotOutput("pieCmp1", height = 1200)
                       ),
                       tabPanel("Bar-AQI", plotOutput("barCmp1", height = 1200)
                       ),
                       tabPanel("Table-AQI",dataTableOutput("tableCmp1", height = 1200)
                       )
                ),
                tabBox(width=4,
                       side = "left", height = "1200",id = "tabsetCmp2",
                       selected = "AQI Index",
                       tabPanel("Pie-AQI",plotOutput("pieCmp2", height = 1200)
                       ),
                       tabPanel("Bar-AQI", plotOutput("barCmp2", height = 1200)
                       ),
                       tabPanel("Table-AQI",dataTableOutput("tableCmp2", height = 1200)
                       )
                ),
                tabBox(width=4,
                       side = "left", height = "1200",id = "tabsetCmp3",
                       selected = "AQI Index",
                       tabPanel("Pie-AQI",plotOutput("pieCmp3", height = 1200)
                       ),
                       tabPanel("Bar-AQI", plotOutput("barCmp3", height = 1200)
                       ),
                       tabPanel("Table-AQI",dataTableOutput("tableCmp3", height = 1200)
                       )
                  )
                )
              ),
              tabItem(tabName = "subitem2", 
                      fluidRow(
                        box(width=4,
                            wellPanel(
                              selectInput("Year1", "Select the year to visualize", years),
                              uiOutput("Item2State1"),
                              uiOutput("Item2County1")
                            )
                        ),
                        box(width=4,
                            wellPanel(
                              selectInput("Year2", "Select the year to visualize", years),
                              uiOutput("Item2State2"),
                              uiOutput("Item2County2"))
                        ),
                        box(width=4,
                            wellPanel(
                              selectInput("Year3", "Select the year to visualize", years),
                              uiOutput("Item2State3"),
                              uiOutput("Item2County3"))
                        )
                      ),
                      fluidRow(
                        
                      )
              ),
    tabItem(tabName = "subitem3", 
            fluidRow(
              box(width=4,
                  wellPanel(
                    selectInput("Year1", "Select the year to visualize", years),
                    uiOutput("Item3State1"),
                    uiOutput("Item3County1")
                  )
              ),
              box(width=4,
                  wellPanel(
                    selectInput("Year2", "Select the year to visualize", years),
                    uiOutput("Item3State2"),
                    uiOutput("Item3County2"))
              ),
              box(width=4,
                  wellPanel(
                    selectInput("Year3", "Select the year to visualize", years),
                    uiOutput("Item3State3"),
                    uiOutput("Item3County3"))
              )
            )        
    )
    
    
                  
            )
      )


sidebar <-dashboardSidebar(disable = FALSE, collapsed = FALSE,
                 sidebarMenu(
                   menuItem("Single County", tabName = "dashboard", icon = icon("dashboard")),
                   menuItem("Multiple County", icon = icon("dashboard"), tabName = "dashboard2",menuSubItem("AQI Analysis", tabName = "subitem1"),
                            menuSubItem("Main Pollutant", tabName = "subitem2"),menuSubItem("AQI/Pollutant over years", tabName = "subitem3"),
                            badgeColor = "green"))
)

# Create the shiny dashboard
ui <- dashboardPage(
    dashboardHeader(title = "Project 1 -- Just Breathe"),
    sidebar,
    body
)
server <- function(input, output) {

# increase the default font size
theme_set(theme_grey(base_size = 18)) 

# calculate the values one time and re-use them in multiple charts to speed things up
listStates <- reactive(unique(allData[allData$Year==input$Year,]['State']))
output$State <- renderUI({selectInput("State", "Select the State to visualize", listStates())})
listCounties <- reactive(unique(allData[allData$State==input$State & allData$Year==input$Year,]['County']))
output$County <- renderUI({selectInput("County", "Select the county to visualize", listCounties())})


listStatesCmp1 <- reactive(unique(allData[allData$Year==input$Year1,]['State']))
output$State1 <- renderUI({selectInput("State1", "Select the first State to visualize", listStatesCmp1())})
listCountiesCmp1 <- reactive(unique(allData[allData$State==input$State1 & allData$Year==input$Year1,]['County']))
output$County1 <- renderUI({selectInput("County1", "Select the first county to visualize", listCountiesCmp1())})

listStatesCmp2 <- reactive(unique(allData[allData$Year==input$Year2,]['State']))
output$State2 <- renderUI({selectInput("State2", "Select the second State to visualize", listStatesCmp2())})
listCountiesCmp2 <- reactive(unique(allData[allData$State==input$State2 & allData$Year==input$Year2,]['County']))
output$County2 <- renderUI({selectInput("County2", "Select the second county to visualize", listCountiesCmp2())})

listStatesCmp3 <- reactive(unique(allData[allData$Year==input$Year3,]['State']))
output$State3 <- renderUI({selectInput("State3", "Select the third State to visualize", listStatesCmp3())})
listCountiesCmp3 <- reactive(unique(allData[allData$State==input$State3 & allData$Year==input$Year3,]['County']))
output$County3 <- renderUI({selectInput("County3", "Select the third county to visualize", listCountiesCmp3())})

justOneCountyReactive <- reactive(subset(allData, (allData$Year) == input$Year & allData$State==input$State & (allData$County== input$County)))

justOneCountyReactive2 <- reactive(subset(allData, (allData$State==input$State & (allData$County== input$County))))

justOneCountyReactiveCmp1 <- reactive(subset(allData, (allData$Year) == input$Year1 & allData$State==input$State1 & (allData$County== input$County1)))

justOneCountyReactive2Cmp1 <- reactive(subset(allData, (allData$State==input$State1 & (allData$County== input$County1))))

justOneCountyReactiveCmp2 <- reactive(subset(allData, (allData$Year) == input$Year2 & allData$State==input$State2 & (allData$County== input$County2)))

justOneCountyReactive2Cmp2 <- reactive(subset(allData, (allData$State==input$State2 & (allData$County== input$County2))))

justOneCountyReactiveCmp3 <- reactive(subset(allData, (allData$Year) == input$Year3 & allData$State==input$State3& (allData$County== input$County3)))

justOneCountyReactive2Cmp3 <- reactive(subset(allData, (allData$State==input$State3 & (allData$County== input$County3))))


# show a pie chart of the percentage of days for a given county for a given year
output$pie <- renderPlot({
  newCounty <-  justOneCountyReactive()
  
  numGDays <- newCounty$Good.Days
  numMDays <- newCounty$Moderate.Days
  numUhSDays <- newCounty$Unhealthy.for.Sensitive.Groups.Days
  numUhDays <- newCounty$Unhealthy.Days
  numVUhDays <- newCounty$Very.Unhealthy.Days
  numHDays <- newCounty$Hazardous.Days
  
  numAQIDays <- newCounty$Days.with.AQI
  
  perGDays <- round((numGDays/numAQIDays)*100,2)
  perMDays <- round((numMDays/numAQIDays)*100,2)
  perUhSDays <- round((numUhSDays/numAQIDays)*100,2)
  perUhDays <- round((numUhDays/numAQIDays)*100,2)
  perVUhDays <- round((numVUhDays/numAQIDays)*100,2)
  perHDays <- round((numHDays/numAQIDays)*100,2)
  
  df_percentage <- data.frame(group = c( "good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"),
                              value = c(perGDays,perMDays, perUhSDays, perUhDays, perVUhDays,perHDays)
  )
  lbls <- c("")
  lbls <- paste(lbls, df_percentage$value) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  colors <- c("yellow2","olivedrab3","orangered3")
  pie(df_percentage$value, main = "Pie Chart for AQI " ,labels = lbls, radius = 0.8,col=colors)
  legend(.9, .1, c("good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"), cex = 0.7, fill = colors)
  
  })  

# show a pie chart of the percentage of days for a given county for a given year(Comparison)
output$pieCmp1 <- renderPlot({
  newCounty <-  justOneCountyReactiveCmp1()
  
  numGDays <- newCounty$Good.Days
  numMDays <- newCounty$Moderate.Days
  numUhSDays <- newCounty$Unhealthy.for.Sensitive.Groups.Days
  numUhDays <- newCounty$Unhealthy.Days
  numVUhDays <- newCounty$Very.Unhealthy.Days
  numHDays <- newCounty$Hazardous.Days
  
  numAQIDays <- newCounty$Days.with.AQI
  
  perGDays <- round((numGDays/numAQIDays)*100,2)
  perMDays <- round((numMDays/numAQIDays)*100,2)
  perUhSDays <- round((numUhSDays/numAQIDays)*100,2)
  perUhDays <- round((numUhDays/numAQIDays)*100,2)
  perVUhDays <- round((numVUhDays/numAQIDays)*100,2)
  perHDays <- round((numHDays/numAQIDays)*100,2)
  
  df_percentage <- data.frame(group = c( "good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"),
                              value = c(perGDays,perMDays, perUhSDays, perUhDays, perVUhDays,perHDays)
  )
  lbls <- c("")
  lbls <- paste(lbls, df_percentage$value) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  colors <- c("yellow2","olivedrab3","orangered3")
  pie(df_percentage$value, main = "Pie Chart for AQI " ,labels = lbls, radius = 0.8,col=colors)
  legend(.9, .1, c("good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"), cex = 0.7, fill = colors)
  
})  
# show a pie chart of the percentage of days for a given county for a given year (Comparison)
output$pieCmp2 <- renderPlot({
  newCounty <-  justOneCountyReactiveCmp2()
  
  numGDays <- newCounty$Good.Days
  numMDays <- newCounty$Moderate.Days
  numUhSDays <- newCounty$Unhealthy.for.Sensitive.Groups.Days
  numUhDays <- newCounty$Unhealthy.Days
  numVUhDays <- newCounty$Very.Unhealthy.Days
  numHDays <- newCounty$Hazardous.Days
  
  numAQIDays <- newCounty$Days.with.AQI
  
  perGDays <- round((numGDays/numAQIDays)*100,2)
  perMDays <- round((numMDays/numAQIDays)*100,2)
  perUhSDays <- round((numUhSDays/numAQIDays)*100,2)
  perUhDays <- round((numUhDays/numAQIDays)*100,2)
  perVUhDays <- round((numVUhDays/numAQIDays)*100,2)
  perHDays <- round((numHDays/numAQIDays)*100,2)
  
  df_percentage <- data.frame(group = c( "good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"),
                              value = c(perGDays,perMDays, perUhSDays, perUhDays, perVUhDays,perHDays)
  )
  lbls <- c("")
  lbls <- paste(lbls, df_percentage$value) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  colors <- c("yellow2","olivedrab3","orangered3")
  pie(df_percentage$value, main = "Pie Chart for AQI " ,labels = lbls, radius = 0.8,col=colors)
  legend(.9, .1, c("good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"), cex = 0.7, fill = colors)
  
})  
# show a pie chart of the percentage of days for a given county for a given year(Comparison)
output$pieCmp3 <- renderPlot({
  newCounty <-  justOneCountyReactiveCmp3()
  
  numGDays <- newCounty$Good.Days
  numMDays <- newCounty$Moderate.Days
  numUhSDays <- newCounty$Unhealthy.for.Sensitive.Groups.Days
  numUhDays <- newCounty$Unhealthy.Days
  numVUhDays <- newCounty$Very.Unhealthy.Days
  numHDays <- newCounty$Hazardous.Days
  
  numAQIDays <- newCounty$Days.with.AQI
  
  perGDays <- round((numGDays/numAQIDays)*100,2)
  perMDays <- round((numMDays/numAQIDays)*100,2)
  perUhSDays <- round((numUhSDays/numAQIDays)*100,2)
  perUhDays <- round((numUhDays/numAQIDays)*100,2)
  perVUhDays <- round((numVUhDays/numAQIDays)*100,2)
  perHDays <- round((numHDays/numAQIDays)*100,2)
  
  df_percentage <- data.frame(group = c( "good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"),
                              value = c(perGDays,perMDays, perUhSDays, perUhDays, perVUhDays,perHDays)
  )
  lbls <- c("")
  lbls <- paste(lbls, df_percentage$value) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  colors <- c("yellow2","olivedrab3","orangered3")
  pie(df_percentage$value, main = "Pie Chart for AQI " ,labels = lbls, radius = 0.8,col=colors)
  legend(.9, .1, c("good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"), cex = 0.7, fill = colors)
  
})  

# show a bar chart of the number of days  where the AQI was good / moderate / unhealthy for sensitive / unhealthy / very unhealthy / hazardous 
# for a given county for a given year

output$bar <- renderPlot({
  newCounty <-  justOneCountyReactive()
  
  numGDays <- newCounty$Good.Days
  numMDays <- newCounty$Moderate.Days
  numUhSDays <- newCounty$Unhealthy.for.Sensitive.Groups.Days
  numUhDays <- newCounty$Unhealthy.Days
  numVUhDays <- newCounty$Very.Unhealthy.Days
  numHDays <- newCounty$Hazardous.Days
  
  numAQIDays <- newCounty$Days.with.AQI
  
  df_num_days <- data.frame(group = c( "good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"),
                              value = c(numGDays,numMDays, numUhSDays, numUhDays, numVUhDays,numHDays)
  )
  df_num_days <- df_num_days[order(df_num_days$value),]
  df_num_days$group <- factor(df_num_days$group, levels = df_num_days$group[order(-df_num_days$value)])
  
  ggplot(df_num_days, aes(x=df_num_days$group, y=df_num_days$value)) + geom_bar(stat="identity", fill="steelblue") +
    labs(x="AQI quality", y = "Number of days with the quality")+  ggtitle("Bar Chart for AQI")+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x = element_text(angle = 90, hjust = 1))

  
})  
# show a bar chart of the number of days  where the AQI was good / moderate / unhealthy for sensitive / unhealthy / very unhealthy / hazardous 
# for a given county for a given year(Comparison)

output$barCmp1 <- renderPlot({
  newCounty <-  justOneCountyReactiveCmp1()
  
  numGDays <- newCounty$Good.Days
  numMDays <- newCounty$Moderate.Days
  numUhSDays <- newCounty$Unhealthy.for.Sensitive.Groups.Days
  numUhDays <- newCounty$Unhealthy.Days
  numVUhDays <- newCounty$Very.Unhealthy.Days
  numHDays <- newCounty$Hazardous.Days
  
  numAQIDays <- newCounty$Days.with.AQI
  
  df_num_days <- data.frame(group = c( "good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"),
                            value = c(numGDays,numMDays, numUhSDays, numUhDays, numVUhDays,numHDays)
  )
  df_num_days <- df_num_days[order(df_num_days$value),]
  df_num_days$group <- factor(df_num_days$group, levels = df_num_days$group[order(-df_num_days$value)])
  
  ggplot(df_num_days, aes(x=df_num_days$group, y=df_num_days$value)) + geom_bar(stat="identity", fill="steelblue") +
    labs(x="AQI quality", y = "Number of days with the quality")+  ggtitle("Bar Chart for AQI")+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
})  
# show a bar chart of the number of days  where the AQI was good / moderate / unhealthy for sensitive / unhealthy / very unhealthy / hazardous 
# for a given county for a given year(Comparison)

output$barCmp2 <- renderPlot({
  newCounty <-  justOneCountyReactiveCmp2()
  
  numGDays <- newCounty$Good.Days
  numMDays <- newCounty$Moderate.Days
  numUhSDays <- newCounty$Unhealthy.for.Sensitive.Groups.Days
  numUhDays <- newCounty$Unhealthy.Days
  numVUhDays <- newCounty$Very.Unhealthy.Days
  numHDays <- newCounty$Hazardous.Days
  
  numAQIDays <- newCounty$Days.with.AQI
  
  df_num_days <- data.frame(group = c( "good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"),
                            value = c(numGDays,numMDays, numUhSDays, numUhDays, numVUhDays,numHDays)
  )
  df_num_days <- df_num_days[order(df_num_days$value),]
  df_num_days$group <- factor(df_num_days$group, levels = df_num_days$group[order(-df_num_days$value)])
  
  ggplot(df_num_days, aes(x=df_num_days$group, y=df_num_days$value)) + geom_bar(stat="identity", fill="steelblue") +
    labs(x="AQI quality", y = "Number of days with the quality")+  ggtitle("Bar Chart for AQI")+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
})  
# show a bar chart of the number of days  where the AQI was good / moderate / unhealthy for sensitive / unhealthy / very unhealthy / hazardous 
# for a given county for a given year(Comparsion)

output$barCmp3 <- renderPlot({
  newCounty <-  justOneCountyReactiveCmp3()
  
  numGDays <- newCounty$Good.Days
  numMDays <- newCounty$Moderate.Days
  numUhSDays <- newCounty$Unhealthy.for.Sensitive.Groups.Days
  numUhDays <- newCounty$Unhealthy.Days
  numVUhDays <- newCounty$Very.Unhealthy.Days
  numHDays <- newCounty$Hazardous.Days
  
  numAQIDays <- newCounty$Days.with.AQI
  
  df_num_days <- data.frame(group = c( "good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"),
                            value = c(numGDays,numMDays, numUhSDays, numUhDays, numVUhDays,numHDays)
  )
  df_num_days <- df_num_days[order(df_num_days$value),]
  df_num_days$group <- factor(df_num_days$group, levels = df_num_days$group[order(-df_num_days$value)])
  
  ggplot(df_num_days, aes(x=df_num_days$group, y=df_num_days$value)) + geom_bar(stat="identity", fill="steelblue") +
    labs(x="AQI quality", y = "Number of days with the quality")+  ggtitle("Bar Chart for AQI")+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
})  

  # show a pie chart of the percentage of days as the main pollutant for a given county for a given year
  output$pieCO <- renderPlot({
    newCounty <-  justOneCountyReactive()
    mPCO <- newCounty$Days.CO
    numAQIDays <- newCounty$Days.with.AQI
    per_mPCO <- round((mPCO/numAQIDays)*100,2)

    df_per_main_pollutant <- data.frame(group = c("CO"),
                                        value = c(per_mPCO,100-per_mPCO))
    lbls <- c("CO  ")
    lbls <- paste(lbls, per_mPCO) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    
    pie(df_per_main_pollutant$value, main = "CO as main pollutant" , labels = lbls, radius = 0.8)
  })
  # show a pie chart of the percentage of days as the main pollutant for a given county for a given year
  output$pieNO2 <- renderPlot({
    newCounty <-  justOneCountyReactive()
    mPNO2 <- newCounty$Days.NO2
    numAQIDays <- newCounty$Days.with.AQI
    per_mPNO2 <-round((mPNO2/numAQIDays)*100,5)
    df_per_main_pollutant <- data.frame(group = c( "NO2"),
                                        value = c(per_mPNO2,100-per_mPNO2))
    lbls <- c("NO2  ")
    lbls <- paste(lbls, per_mPNO2) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    
    pie(df_per_main_pollutant$value, main = "NO2 as main pollutant",labels = lbls, radius = 0.8)
  })
  # show a pie chart of the percentage of days as the main pollutant for a given county for a given year
  output$pieOzone <- renderPlot({
    newCounty <-  justOneCountyReactive()
    mPOzone <- newCounty$Days.Ozone
    numAQIDays <- newCounty$Days.with.AQI
    per_mPOzone <- round((mPOzone/numAQIDays)*100,5)
    df_per_main_pollutant <- data.frame(group = c( "Ozone" ),
                                        value = c(per_mPOzone,100-per_mPOzone))
    
    lbls <- c("Ozone  ")
    lbls <- paste(lbls, per_mPOzone) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    
    pie(df_per_main_pollutant$value,main = "Ozone as main pollutant", labels = lbls, radius = 0.8)
  })
  # show a pie chart of the percentage of days as the main pollutant for a given county for a given year
  output$pieSO2 <- renderPlot({
    newCounty <-  justOneCountyReactive()
    mPSO2 <- newCounty$Days.SO2
    numAQIDays <- newCounty$Days.with.AQI
    per_mPSO2 <- round((mPSO2/numAQIDays)*100,5)
    
    df_per_main_pollutant <- data.frame(group = c( "SO2"),
                                        value = c(per_mPSO2,100-per_mPSO2))
    
    lbls <- c("SO2  ")
    lbls <- paste(lbls, per_mPSO2) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    
    pie(df_per_main_pollutant$value,main = "SO2 as main pollutant", labels = lbls, radius = 0.8)
  })
  # show a pie chart of the percentage of days as the main pollutant for a given county for a given year
  output$piePM2.5 <- renderPlot({
    newCounty <-  justOneCountyReactive()
    mP_PM2.5<- newCounty$Days.PM2.5    
    numAQIDays <- newCounty$Days.with.AQI
    per_mP_PM2.5 <- round((mP_PM2.5/numAQIDays)*100,5)
    
    df_per_main_pollutant <- data.frame(group = c("PM2.5" ),
                                        value = c(per_mP_PM2.5,100-per_mP_PM2.5))
    lbls <- c("PM2.5  ")
    lbls <- paste(lbls, per_mP_PM2.5) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    
    pie(df_per_main_pollutant$value, main = "PM2.5 as main pollutant",labels = lbls, radius = 0.8)
  })
  # show a pie chart of the percentage of days as the main pollutant for a given county for a given year
  output$piePM10 <- renderPlot({
    newCounty <-  justOneCountyReactive()
    mP_PM10 <- newCounty$Days.PM10
    numAQIDays <- newCounty$Days.with.AQI
    per_mP_PM10 <- round((mP_PM10/numAQIDays)*100)
    
    lbls <- c("PM10  ")
    lbls <- paste(lbls, per_mP_PM10) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    
    df_per_main_pollutant <- data.frame(group = c("PM10"),
                                        value = c(per_mP_PM10,100-per_mP_PM10))
    
    pie(df_per_main_pollutant$value, main = "PM10 as main pollutant",labels = lbls, radius = 0.8)

  })
  
  # show a bar chart of the number of days as the main pollutant for a given county for a given year
  output$bar2 <- renderPlot({
    newCounty <-  justOneCountyReactive()
    mPCO <- newCounty$Days.CO
    mPNO2 <- newCounty$Days.NO2
    mPOzone <- newCounty$Days.Ozone
    mPSO2 <- newCounty$Days.SO2
    mP_PM2.5<- newCounty$Days.PM2.5    
    mP_PM10 <- newCounty$Days.PM10
    
    
    df_num_days <- data.frame(group = c( "CO" , "NO2" , "Ozone", "SO2", "PM2.5" ,"PM10"),
                                        value = c(mPCO,mPNO2, mPOzone, mPSO2, mP_PM2.5,mP_PM10))
    df_num_days <- df_num_days[order(df_num_days$value),]
    df_num_days$group <- factor(df_num_days$group, levels = df_num_days$group[order(-df_num_days$value)])
    
  ggplot(df_num_days, aes(x=df_num_days$group, y=df_num_days$value)) + geom_bar(stat="identity", fill="steelblue") +
    labs(x="Pollutant name ", y = "Number of days as main pollutant")+  ggtitle("Bar Chart for Pollutants")+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  })
  # use DT to help out with the tables - https://datatables.net/reference/option/
  output$table <- DT::renderDataTable(
    DT::datatable({ 
      newCounty <-  justOneCountyReactive()
      numGDays <- newCounty$Good.Days
      numMDays <- newCounty$Moderate.Days
      numUhSDays <- newCounty$Unhealthy.for.Sensitive.Groups.Days
      numUhDays <- newCounty$Unhealthy.Days
      numVUhDays <- newCounty$Very.Unhealthy.Days
      numHDays <- newCounty$Hazardous.Days
      
      df_num_days <- data.frame(AQI = c( "good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"),
                                NumberOfDays = c(numGDays,numMDays, numUhSDays, numUhDays, numVUhDays,numHDays))
      },
    options = list(searching = FALSE, pageLength = 6, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  output$tableCmp1 <- DT::renderDataTable(
    DT::datatable({ 
      newCounty <-  justOneCountyReactiveCmp1()
      numGDays <- newCounty$Good.Days
      numMDays <- newCounty$Moderate.Days
      numUhSDays <- newCounty$Unhealthy.for.Sensitive.Groups.Days
      numUhDays <- newCounty$Unhealthy.Days
      numVUhDays <- newCounty$Very.Unhealthy.Days
      numHDays <- newCounty$Hazardous.Days
      
      df_num_days <- data.frame(AQI = c( "good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"),
                                NumberOfDays = c(numGDays,numMDays, numUhSDays, numUhDays, numVUhDays,numHDays))
    },
    options = list(searching = FALSE, pageLength = 6, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  output$tableCmp2 <- DT::renderDataTable(
    DT::datatable({ 
      newCounty <-  justOneCountyReactiveCmp2()
      numGDays <- newCounty$Good.Days
      numMDays <- newCounty$Moderate.Days
      numUhSDays <- newCounty$Unhealthy.for.Sensitive.Groups.Days
      numUhDays <- newCounty$Unhealthy.Days
      numVUhDays <- newCounty$Very.Unhealthy.Days
      numHDays <- newCounty$Hazardous.Days
      
      df_num_days <- data.frame(AQI = c( "good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"),
                                NumberOfDays = c(numGDays,numMDays, numUhSDays, numUhDays, numVUhDays,numHDays))
    },
    options = list(searching = FALSE, pageLength = 6, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  output$tableCmp3 <- DT::renderDataTable(
    DT::datatable({ 
      newCounty <-  justOneCountyReactiveCmp3()
      numGDays <- newCounty$Good.Days
      numMDays <- newCounty$Moderate.Days
      numUhSDays <- newCounty$Unhealthy.for.Sensitive.Groups.Days
      numUhDays <- newCounty$Unhealthy.Days
      numVUhDays <- newCounty$Very.Unhealthy.Days
      numHDays <- newCounty$Hazardous.Days
      
      df_num_days <- data.frame(AQI = c( "good" , "moderate" , "unhealthy for sensitive", "unhealthy", "very unhealthy" ,"hazardous"),
                                NumberOfDays = c(numGDays,numMDays, numUhSDays, numUhDays, numVUhDays,numHDays))
    },
    options = list(searching = FALSE, pageLength = 6, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$table2 <- DT::renderDataTable(
    DT::datatable({ 
      
      newCounty <-  justOneCountyReactive()
      mPCO <- newCounty$Days.CO
      mPNO2 <- newCounty$Days.NO2
      mPOzone <- newCounty$Days.Ozone
      mPSO2 <- newCounty$Days.SO2
      mP_PM2.5<- newCounty$Days.PM2.5    
      mP_PM10 <- newCounty$Days.PM10
      
      df_num_days <- data.frame(MainPollutant = c( "CO" , "NO2" , "Ozone", "SO2", "PM2.5" ,"PM10"),
                                NumberOfDays = c(mPCO,mPNO2, mPOzone, mPSO2, mP_PM2.5,mP_PM10))
    
    },
    options = list(searching = FALSE, pageLength = 6, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  output$tableCO <- DT::renderDataTable(
    DT::datatable({ 

      newCounty <- justOneCountyReactive2()
      mPCO <- newCounty$Days.CO
      numAQIDays <- newCounty$Days.with.AQI
      per_mPCO = mPCO/numAQIDays
      
      df_num_days <- data.frame(year = newCounty$Year,
                                percent = c(per_mPCO))
    },
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'asec'))
    ), rownames = FALSE 
    )
  )
  output$tableNO2 <- DT::renderDataTable(
    DT::datatable({ 
      newCounty <- justOneCountyReactive2()
      mPNO2 <- newCounty$Days.NO2
      numAQIDays <- newCounty$Days.with.AQI
      per_mPNO2 = mPNO2/numAQIDays
      
      df_num_days <- data.frame(year = newCounty$Year,
                                percent = c(per_mPNO2))
    },
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'asec'))
    ), rownames = FALSE 
    )
  )
  
    output$tableOzone <- DT::renderDataTable(
    DT::datatable({ 
      newCounty <- justOneCountyReactive2()
      mPOzone <- newCounty$Days.Ozone
      numAQIDays <- newCounty$Days.with.AQI
      per_mPOzone = mPOzone/numAQIDays
      
      df_num_days <- data.frame(year = newCounty$Year,
                                percent = c(per_mPOzone))
    },
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'asec'))
    ), rownames = FALSE 
    )
  )
    output$tableSO2 <- DT::renderDataTable(
      DT::datatable({ 

        newCounty <- justOneCountyReactive2()
        mPSO2 <- newCounty$Days.SO2
        numAQIDays <- newCounty$Days.with.AQI
        per_mPSO2 = mPSO2/numAQIDays
        
        df_num_days <- data.frame(year = newCounty$Year,
                                  percent = c(per_mPSO2))
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'asec'))
      ), rownames = FALSE 
      )
    )
    output$tablePM2.5 <- DT::renderDataTable(
      DT::datatable({ 

        newCounty <- justOneCountyReactive2()
        mP_PM2.5 <- newCounty$Days.PM2.5
        numAQIDays <- newCounty$Days.with.AQI
        per_mP_PM2.5 = mP_PM2.5/numAQIDays
        df_num_days <- data.frame(year = newCounty$Year,
                                  percent = c(per_mP_PM2.5))
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'asec'))
      ), rownames = FALSE 
      )
    )
    output$tablePM10 <- DT::renderDataTable(
      DT::datatable({ 

        newCounty <- justOneCountyReactive2()
        mP_PM10 <- newCounty$Days.PM10
        numAQIDays <- newCounty$Days.with.AQI
        per_mP_PM10 = mP_PM10/numAQIDays
        
        df_num_days <- data.frame(year = newCounty$Year,
                                  percent = c(per_mP_PM10))
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'asec'))
      ), rownames = FALSE 
      )
    )
    
  output$tabset1Selected <- renderText({
    input$tabset1
  })
  output$tabset2Selected <- renderText({
    input$tabset2})
  output$tabset3Selected <- renderText({
      input$tabset3
      
      })
  output$line1 <- renderPlot({
    # Basic line plot
    newCounty <- justOneCountyReactive2()
    ggplot(data=newCounty, aes(x=Year))+
      geom_line(aes(y=newCounty$Median.AQI,color="Median AQI"))+
      geom_line(aes(y=newCounty$X90th.Percentile.AQI,color="90th.Percentile"))+
      geom_line(aes(y=newCounty$Max.AQI,color="Max AQI"))+
      scale_colour_manual(name="AQI value",values = c("Median AQI" = "red", "90th.Percentile" = "blue", "Max AQI" = "brown"))+ labs(y="AQI value")
    })
  output$line2 <- renderPlot({
    # Basic line plot
    newCounty <- justOneCountyReactive2()
    mPCO <- newCounty$Days.CO
    mPNO2 <- newCounty$Days.NO2
    mPOzone <- newCounty$Days.Ozone
    mPSO2 <- newCounty$Days.SO2
    mP_PM2.5<- newCounty$Days.PM2.5    
    mP_PM10 <- newCounty$Days.PM10
    
    numAQIDays <- newCounty$Days.with.AQI
    
    per_mPCO = mPCO/numAQIDays
    per_mPNO2 = mPNO2/numAQIDays
    per_mPOzone = mPOzone/numAQIDays
    per_mPSO2 = mPSO2/numAQIDays
    per_mP_PM2.5 = mP_PM2.5/numAQIDays
    per_mP_PM10 = mP_PM10/numAQIDays
    
    ggplot(data=newCounty, aes(x=Year))+
      geom_line(aes(y=per_mPCO,color="CO"))+
      geom_line(aes(y=per_mPNO2,color="NO2"))+
      geom_line(aes(y=per_mPOzone,color="Ozone"))+
      geom_line(aes(y=per_mPSO2,color="SO2"))+
      geom_line(aes(y=per_mP_PM2.5,color="PM2.5"))+
      geom_line(aes(y=per_mP_PM10,color="PM10"))+
      scale_colour_manual(name="Pollutant",values = c("CO" = "red", "NO2" = "blue", "Ozone" = "brown","SO2" = "orange", "PM2.5" = "green", "PM10" = "purple"))+ labs(y="Percent")
  })
  output$leaf <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    newcounty <- subset(aqs_sites, aqs_sites$County.Name== input$County & aqs_sites$State.Name==input$State)
    map <- setView(map, lng = newcounty[1,]$Longitude, lat = newcounty[1,]$Latitude, zoom = 18)
    map <- addMarkers(map, lng = newcounty[1,]$Longitude, lat = newcounty[1,]$Latitude, popup = input$County)
    map
  })
  
  
}

shinyApp(ui = ui, server = server)
