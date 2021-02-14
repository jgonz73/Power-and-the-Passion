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
library(lubridate)
library(DT)
library(grid)
library(dplyr)
library(usmap)


energy <- read.csv("annual_generation_state.csv")

# convert generation to numbers from strings, upper all of STATE, factor state, type, and source
energy$GENERATION..Megawatthours. <- as.numeric(factor(energy$GENERATION..Megawatthours.))
energy$STATE = toupper(energy$STATE)
energy$STATE <- as.factor(energy$STATE)
energy$TYPE.OF.PRODUCER <- as.factor(energy$TYPE.OF.PRODUCER)
energy$ENERGY.SOURCE <- as.factor(energy$ENERGY.SOURCE)

# shows missing identifiers
# subset(energy, energy$STATE == "  ")
#YEAR STATE              TYPE.OF.PRODUCER ENERGY.SOURCE GENERATION..Megawatthours.
#20577 2003       Total Electric Power Industry          Coal                          0
#20578 2003       Total Electric Power Industry   Natural Gas                          0
#20579 2003       Total Electric Power Industry     Petroleum                          0
# removes rows w/ missing identifiers
energy <- energy[-c(20577, 20578, 20579), ]

# removes rows w/ negative values for Generation 
energy <- energy[energy$GENERATION..Megawatthours. >= 0, ] 

# remove rows with certain energy sources 
energy <- subset(energy, energy$ENERGY.SOURCE != "Other" & 
                   energy$ENERGY.SOURCE != "Other Gases" & 
                   energy$ENERGY.SOURCE != "Other Biomass" & 
                   energy$ENERGY.SOURCE != "Pumped Storage")
energy$ENERGY.SOURCE <- droplevels(energy$ENERGY.SOURCE)

# shorten names
levels(energy$ENERGY.SOURCE)[levels(energy$ENERGY.SOURCE) == "Hydroelectric Conventional"] <- "Hydro"
levels(energy$ENERGY.SOURCE)[levels(energy$ENERGY.SOURCE) == "Wood and Wood Derived Fuels"] <- "Wood"
levels(energy$ENERGY.SOURCE)[levels(energy$ENERGY.SOURCE) == "Solar Thermal and Photovoltaic"] <- "Solar"

# calculations
eSource <- aggregate(GENERATION..Megawatthours. ~ YEAR + ENERGY.SOURCE, energy, sum)
energyYearly <- aggregate(GENERATION..Megawatthours. ~ YEAR, energy, sum)

# coal
coaldf <- subset(eSource, eSource$ENERGY.SOURCE == "Coal")
coaldf <- merge(coaldf, energyYearly, by="YEAR")
coaldf <- coaldf %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

# geothermal
geodf <- subset(eSource, eSource$ENERGY.SOURCE == "Geothermal")
geodf <- merge(geodf, energyYearly, by="YEAR")
geodf <- geodf %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

# Hydro
hydrodf <- subset(eSource, eSource$ENERGY.SOURCE == "Hydro")
hydrodf <- merge(hydrodf, energyYearly, by="YEAR")
hydrodf <- hydrodf %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

# Natural Gas
naturaldf <- subset(eSource, eSource$ENERGY.SOURCE == "Natural Gas")
naturaldf <- merge(naturaldf, energyYearly, by="YEAR")
naturaldf <- naturaldf %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

# Nuclear
nucleardf <- subset(eSource, eSource$ENERGY.SOURCE == "Nuclear")
nucleardf <- merge(nucleardf, energyYearly, by="YEAR")
nucleardf <- nucleardf %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

# Petroleum
petroleumdf <- subset(eSource, eSource$ENERGY.SOURCE == "Petroleum")
petroleumdf <- merge(petroleumdf, energyYearly, by="YEAR")
petroleumdf <- petroleumdf %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

# Solar
solardf <- subset(eSource, eSource$ENERGY.SOURCE == "Solar")
solardf <- merge(solardf, energyYearly, by="YEAR")
solardf <- solardf %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

# Wind
winddf <- subset(eSource, eSource$ENERGY.SOURCE == "Wind")
winddf <- merge(winddf, energyYearly, by="YEAR")
winddf <- winddf %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

# Wood
wooddf <- subset(eSource, eSource$ENERGY.SOURCE=="Wood")
wooddf <- merge(wooddf, energyYearly, by="YEAR")
wooddf <- wooddf %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

# Calculate percents
percentages <- rbind(coaldf, geodf, hydrodf, naturaldf, nucleardf, petroleumdf, solardf, winddf, wooddf)
percentages <- percentages[order(percentages$YEAR), ]
names(percentages)[names(percentages)=="GENERATION..Megawatthours..y"] <- "Total Generated MWh"
names(percentages)[names(percentages)=="GENERATION..Megawatthours..x"] <- "Generation MWh"
drops <- c("Total Generated MWh", "PERCENT")
ptable <- percentages[, !(names(percentages) %in% drops)]
drops <- c("Generation MWh", "Total Generated MWh")
ptable2 <- percentages[, !(names(percentages) %in% drops)]
ptable2$PERCENT <- lapply(ptable2$PERCENT, round, 2)

#=====================================================================================================================================

eByState <- aggregate(GENERATION..Megawatthours. ~ YEAR + ENERGY.SOURCE + STATE, energy, sum)
eByState <- eByState[order(eByState$YEAR),]

coalS <- subset(eByState, eByState$ENERGY.SOURCE == "Coal")
coalS <- merge(coalS, energyYearly, by="YEAR")
coalS <- coalS %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

geoS <- subset(eByState, eByState$ENERGY.SOURCE == "Geothermal")
geoS <- merge(geoS, energyYearly, by="YEAR")
geoS <- geoS %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

hydroS <- subset(eByState, eByState$ENERGY.SOURCE == "Hydro")
hydroS <- merge(hydroS, energyYearly, by="YEAR")
hydroS <- hydroS %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)


naturalS <- subset(eByState, eByState$ENERGY.SOURCE == "Natural Gas")
naturalS <- merge(naturalS, energyYearly, by="YEAR")
naturalS <- naturalS %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

nuclearS <- subset(eByState, eByState$ENERGY.SOURCE == "Nuclear")
nuclearS <- merge(nuclearS, energyYearly, by="YEAR")
nuclearS <- nuclearS %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

petroleumS <- subset(eByState, eByState$ENERGY.SOURCE == "Petroleum")
petroleumS <- merge(petroleumS, energyYearly, by="YEAR")
petroleumS <- petroleumS %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

solarS <- subset(eByState, eByState$ENERGY.SOURCE == "Solar")
solarS <- merge(solarS, energyYearly, by="YEAR")
solarS <- solarS %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

windS <- subset(eByState, eByState$ENERGY.SOURCE == "Wind")
windS <- merge(windS, energyYearly, by="YEAR")
windS <- windS %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

woodS <- subset(eByState, eByState$ENERGY.SOURCE == "Wood")
woodS <- merge(woodS, energyYearly, by="YEAR")
woodS <- woodS %>% group_by(YEAR) %>% mutate(PERCENT = GENERATION..Megawatthours..x / GENERATION..Megawatthours..y * 100)

percentS <- rbind(coalS, geoS, hydroS, naturalS, nuclearS, petroleumS, solarS, windS, woodS)
percentS <- percentS[order(percentS$YEAR),]
names(percentS)[names(percentS)=="GENERATION..Megawatthours..x"] <- "GENERATION.MWh"
names(percentS)[names(percentS)=="GENERATION..Megawatthours..y"] <- "Total Generated MWh"
drops1 <- c("Total Generated MWh", "PERCENT")
ptableS <- percentS[, !(names(percentS) %in% drops1)]
drops1 <- c("GENERATION.MWh", "Total Generated MWh")
ptableS2 <- percentS[, !(names(percentS) %in% drops1)]
ptableS2$PERCENT <- lapply(ptableS2$PERCENT, round, 2)


#=====================================================================================================================================
# Create menu items to select different years and different states
sources <- c(levels(energy$ENERGY.SOURCE), "All")
sources <- sources[sources != "Total"]
energy2 <- energy[energy$ENERGY.SOURCE != "Total", ]

# States
state <- state.name
state <- append(state, c("Washington DC", "Total US"))
state <- sort(state)

years <- unique(energy$YEAR)

#====================================================================
# Define UI for application 
ui <- dashboardPage(
  
  dashboardHeader(title = "CS424 Spring 2021 Project 1"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("About", tabName="About"),
                     menuItem("Dashboard", tabName="Dashboard", selected = T),
                     menuItem("By State", tabName="byState")),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL)
                   
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="About",
              h1("Project 1: Power and the Passion"),
              h3("Developed By: Joshua Gonzales"),
              h4("Project 1 in CS 424 (Data Analytics / Visualization) at the University of Illinois at Chicago Spring 2021"),
              
              h5("________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________"),
              
              h3(""),
              h3("This project contains data from 1990 to 2019 for all 50 states of the US plus the District of Columbia plus the total US"),
              h3("The data focuses on these Energy Sources: Coal, Geothermal, Hydrothermal, Natural Gas, Nuclear, Petroleum, Solar, Total, Wind, and Wood."),
              h3(""),
              
              h5("* Libraries Used: shiny, shinydashboard, ggplot2, lubridate, dplyr, grid, DT"),
              
              h5("* U.S. Energy Information Administration -> https://www.eia.gov/electricity/data/state/"),
              
              h5("* Created using R, RStudio, Shiny ")
      ),
      tabItem(tabName="Dashboard",
        fluidRow(
          column(3,
                 fluidRow(
                   box(title = "Amount of Each Energy Source/Year", solidHeader=TRUE, status="primary", width=12, 
                       plotOutput("stacked1", height = 270))    
                 ),
                 fluidRow(
                   box(title = "% of Total Production For Each Energy Source/Year", solidHeader=TRUE, status="primary", width=12,
                       plotOutput("stacked2", height=270))
                 ) 
          ),
          column(5, 
                 fluidRow(
                   box(title="Amount of Each Energy Source/Year", solidHeader=TRUE, status="primary", width=12,    
                       plotOutput("line1", height=270))
                 ),
                 fluidRow(
                   box(title="% of Total Production For Each Energy Source/Year", solidHeader=TRUE, status="primary", width=12,
                       plotOutput("line2", height=270))
                 ),
                 fluidRow(
                   box(title="Checkboxes For Line Graphs", solidHeader=FALSE, status="primary", width=6,
                       checkboxGroupInput("icons", "Choose Energy Sources:",
                                          choiceNames = sources,
                                          choiceValues = sources,
                                          selected = "All",
                                          inline=TRUE)
                   )
                 )
          ),
          column(4,
                 fluidRow(
                   box(title="Table of Amount of Energy Source/Year", solidHeader=TRUE, status="primary", width=12,
                       DT::dataTableOutput("tab1"))
                 ),
                 fluidRow(
                   box(title="Table of % of Total Production of Each Energy Source/Year", solidHeader=TRUE, status="primary", width=12,
                       DT::dataTableOutput("tab2"))
                 )
          )
        )
      ),
      tabItem(tabName="byState",
        fluidRow(
          column(6,
             selectInput("state", "Select the state to visualize", state, selected = "Total US"),
             selectInput("year", "Select the year to visualize", years, selected = 2019),
             fluidRow(
               column(3,
                  fluidRow(
                    box(title = "Amount of Each Energy Source/Year", status="primary", width=12, 
                        plotOutput("stacked1S", height = 270))    
                  ),
                  fluidRow(
                    box(title = "% of Total Production For Each Energy Source/Year", status="primary", width=12,
                        plotOutput("stacked2S", height=270))
                  ) 
               ),
               column(4, 
                  fluidRow(
                    box(title="Amount of Each Energy Source/Year", status="primary", width=14,    
                        plotOutput("line1S", height=270))
                  ),
                  fluidRow(
                    box(title="% of Total Production For Each Energy Source/Year", status="primary", width=14,
                        plotOutput("line2S", height=270))
                  ),
                  fluidRow(
                    box(title="Checkboxes For Line Graphs", status="primary", width=6,
                        checkboxGroupInput("iconsS", "Choose Energy Sources:",
                                           choiceNames = sources,
                                           choiceValues = sources,
                                           selected = "All",
                                           inline=TRUE)
                    )
                  )
               ),
               column(5,
                  fluidRow(
                    box(title="Table of Amount of Energy Source/Year", status="primary", width=11,
                        div(DT::dataTableOutput("tab1S"), style="font-size:75%; width:40%"))
                  ),
                  fluidRow(
                    box(title="Table of % of Total Production of Each Energy Source/Year", status="primary", width=11,
                        div(DT::dataTableOutput("tab2S"), style="font-size:80%; width:40%"))
                  )
               )
             )
                 
          ),
          column(6,
             selectInput("state2", "Select the state to visualize", state, selected = "Illinois"),
             selectInput("year2", "Select the year to visualize", years, selected = 2019),
             fluidRow(
               column(3,
                  fluidRow(
                    box(title = "Amount of Each Energy Source/Year", status="success", width=12, 
                        plotOutput("stacked1S2", height = 270))    
                  ),
                  fluidRow(
                    box(title = "% of Total Production For Each Energy Source/Year", status="success", width=12,
                        plotOutput("stacked2S2", height=270))
                  ) 
               ),
               column(4, 
                  fluidRow(
                    box(title="Amount of Each Energy Source/Year", status="success", width=12,    
                        plotOutput("line1S2", height=270))
                  ),
                  fluidRow(
                    box(title="% of Total Production For Each Energy Source/Year", status="success", width=12,
                        plotOutput("line2S2", height=270))
                  ),
                  fluidRow(
                    box(title="Checkboxes For Line Graphs", solidHeader=FALSE, status="success", width=6,
                        checkboxGroupInput("iconsS2", "Choose Energy Sources:",
                                           choiceNames = sources,
                                           choiceValues = sources,
                                           selected = "All",
                                           inline=TRUE)
                    )
                  )
               ),
               column(5,
                  fluidRow(
                    box(title="Table of Amount of Energy Source/Year", status="success", width=11,
                        div(DT::dataTableOutput("tab1S2"), style="font-size:75%; width:40%"))
                  ),
                  fluidRow(
                    box(title="Table of % of Total Production of Each Energy Source/Year", status="success", width=11,
                        div(DT::dataTableOutput("tab2S2"), style="font-size:80%; width:40%"))
                  )
               )
             )
          )
          
        )
      )
  
    )
))


#====================================================================
# Define server logic required to draw
server <- function(input, output) {
  
# reactive elements
# percent reactive
reactiveSourcesP <- reactive({
  return(percentages[percentages$ENERGY.SOURCE%in%input$icons,])
})

# total reactives
reactiveSources <- reactive({
  return(energy2[energy2$ENERGY.SOURCE%in%input$icons,])
})
  
#====================================================================
# in ByState tab, should show by State AND Energy Source AND Year

# reactive data for charts on left
reactiveStateL <- reactive({
  
  if (input$state == "Total US") {
    rS <- energy2[energy2$STATE%in%"US-TOTAL",]
    rS <- subset(rS, rS$YEAR==input$year) 
    if(input$iconsS != "All" && length(input$iconsS) > 0) {
      return(rS[rS$ENERGY.SOURCE%in%input$iconsS,])
    }
    return(rS)
  } 
  if (input$state == "Washington DC") {
    rS <- energy2[energy2$STATE%in%"DC",]
    rS <- subset(rS, rS$YEAR==input$year) 
    if(input$iconsS != "All" && length(input$iconsS) > 0) {
      return(rS[rS$ENERGY.SOURCE%in%input$iconsS,])
    }
    return(rS)
  }
  inp <- state.abb[which(state.name==input$state)]
  rS <- energy2[energy2$STATE%in%inp,]
  rS <- subset(rS, rS$YEAR==input$year) 
  if(input$iconsS != "All" && length(input$iconsS) > 0) {
    return(rS[rS$ENERGY.SOURCE%in%input$iconsS,])
  }
  return(rS)
})

# reactive data for charts on right
reactiveStateL2 <- reactive({
  if (input$state2 == "Total US") {
    rS <- energy2[energy2$STATE%in%"US-TOTAL",]
    rS <- subset(rS, rS$YEAR==input$year2) 
    if(input$iconsS2 != "All" && length(input$iconsS2) > 0) {
      return(rS[rS$ENERGY.SOURCE%in%input$iconsS2,])
    }
    return(rS)
  } 
  if (input$state2 == "Washington DC") {
    rS <- energy2[energy2$STATE%in%"DC",]
    rS <- subset(rS, rS$YEAR==input$year2) 
    if(input$iconsS2 != "All" && length(input$iconsS2) > 0) {
      return(rS[rS$ENERGY.SOURCE%in%input$iconsS2,])
    }
    return(rS)
  }
  inp <- state.abb[which(state.name==input$state2)]
  rS <- energy2[energy2$STATE%in%inp,]
  rS <- subset(rS, rS$YEAR==input$year2) 
  if(input$iconsS2 != "All" && length(input$iconsS2) > 0) {
    return(rS[rS$ENERGY.SOURCE%in%input$iconsS2,])
  }
  return(rS)
})

# reactive data for left side percent
reactiveStateLP <- reactive({
  if (input$state == "Total US") {
    rS <- percentS[percentS$STATE%in%"US-TOTAL",]
    rS <- subset(rS, rS$YEAR==input$year) 
    if(input$iconsS != "All" && length(input$iconsS) > 0) {
      return(rS[rS$ENERGY.SOURCE%in%input$iconsS,])
    }
    return(rS)
  } 
  if (input$state == "Washington DC") {
    rS <- percentS[percentS$STATE%in%"DC",]
    rS <- subset(rS, rS$YEAR==input$year) 
    if(input$iconsS != "All" && length(input$iconsS) > 0) {
      return(rS[rS$ENERGY.SOURCE%in%input$iconsS,])
    }
    return(rS)
  }
  inp <- state.abb[which(state.name==input$state)]
  rS <- percentS[percentS$STATE%in%inp,]
  rS <- subset(rS, rS$YEAR==input$year) 
  if(input$iconsS != "All" && length(input$iconsS) > 0) {
    return(rS[rS$ENERGY.SOURCE%in%input$iconsS,])
  }
  return(rS)
})

# reactive data for right side percent
reactiveStateLP2 <- reactive({
  if (input$state2 == "Total US") {
    rS <- percentS[percentS$STATE%in%"US-TOTAL",]
    rS <- subset(rS, rS$YEAR==input$year2) 
    if(input$iconsS != "All" && length(input$iconsS2) > 0) {
      return(rS[rS$ENERGY.SOURCE%in%input$iconsS2,])
    }
    return(rS)
  } 
  if (input$state2 == "Washington DC") {
    rS <- percentS[percentS$STATE%in%"DC",]
    rS <- subset(rS, rS$YEAR==input$year2) 
    if(input$iconsS != "All" && length(input$iconsS2) > 0) {
      return(rS[rS$ENERGY.SOURCE%in%input$iconsS2,])
    }
    return(rS)
  }
  inp <- state.abb[which(state.name==input$state2)]
  rS <- percentS[percentS$STATE%in%inp,]
  rS <- subset(rS, rS$YEAR==input$year2) 
  if(input$iconsS2 != "All" && length(input$iconsS2) > 0) {
    return(rS[rS$ENERGY.SOURCE%in%input$iconsS2,])
  }
  return(rS)
})

# reactive data for table AMOUNT on left
tbl1Reactive <- reactive({
  if (input$state == "Total US") {
    rT <- ptableS[ptableS$STATE%in%"US-TOTAL",]
    rT <- subset(rT, rT$YEAR==input$year2) 
    if(input$iconsS != "All" && length(input$iconsS2) > 0) {
      return(rT[rT$ENERGY.SOURCE%in%input$iconsS2,])
    }
    return(rT)
  } 
  if (input$state == "Washington DC") {
    rT <- ptableS[ptableS$STATE%in%"DC",]
    rT <- subset(rT, rT$YEAR==input$year2) 
    if(input$iconsS != "All" && length(input$iconsS2) > 0) {
      return(rT[rT$ENERGY.SOURCE%in%input$iconsS2,])
    }
    return(rT)
  }
  inp <- state.abb[which(state.name==input$state)]
  rT <- ptableS[ptableS$STATE%in%inp,]
  rT <- subset(rT, rT$YEAR==input$year2) 
  if(input$iconsS2 != "All" && length(input$iconsS2) > 0) {
    return(rT[rT$ENERGY.SOURCE%in%input$iconsS2,])
  }
  return(rT)
})

# reactive data for table AMOUNT on right
tbl2Reactive <- reactive({
  if (input$state2 == "Total US") {
    rT <- ptableS[ptableS$STATE%in%"US-TOTAL",]
    rT <- subset(rT, rT$YEAR==input$year2) 
    if(input$iconsS != "All" && length(input$iconsS2) > 0) {
      return(rT[rT$ENERGY.SOURCE%in%input$iconsS2,])
    }
    return(rT)
  } 
  if (input$state2 == "Washington DC") {
    rT <- ptableS[ptableS$STATE%in%"DC",]
    rT <- subset(rT, rT$YEAR==input$year2) 
    if(input$iconsS != "All" && length(input$iconsS2) > 0) {
      return(rT[rT$ENERGY.SOURCE%in%input$iconsS2,])
    }
    return(rT)
  }
  inp <- state.abb[which(state.name==input$state2)]
  rT <- ptableS[ptableS$STATE%in%inp,]
  rT <- subset(rT, rT$YEAR==input$year2) 
  if(input$iconsS2 != "All" && length(input$iconsS2) > 0) {
    return(rT[rT$ENERGY.SOURCE%in%input$iconsS2,])
  }
  return(rT)
  
})

# reactive data for table PERCENT on left
tbl1ReactiveP <- reactive({
  if (input$state == "Total US") {
    rT <- ptableS2[ptableS2$STATE%in%"US-TOTAL",]
    rT <- subset(rT, rT$YEAR==input$year2) 
    if(input$iconsS != "All" && length(input$iconsS2) > 0) {
      return(rT[rT$ENERGY.SOURCE%in%input$iconsS2,])
    }
    return(rT)
  } 
  if (input$state == "Washington DC") {
    rT <- ptableS2[ptableS2$STATE%in%"DC",]
    rT <- subset(rT, rT$YEAR==input$year2) 
    if(input$iconsS != "All" && length(input$iconsS2) > 0) {
      return(rT[rT$ENERGY.SOURCE%in%input$iconsS2,])
    }
    return(rT)
  }
  inp <- state.abb[which(state.name==input$state)]
  rT <- ptableS2[ptableS2$STATE%in%inp,]
  rT <- subset(rT, rT$YEAR==input$year2) 
  if(input$iconsS2 != "All" && length(input$iconsS2) > 0) {
    return(rT[rT$ENERGY.SOURCE%in%input$iconsS2,])
  }
  return(rT)
  
})

# reactive data for table PERCENT on right
tbl2ReactiveP <- reactive({
  if (input$state2 == "Total US") {
    rT <- ptableS2[ptableS2$STATE%in%"US-TOTAL",]
    rT <- subset(rT, rT$YEAR==input$year2) 
    if(input$iconsS != "All" && length(input$iconsS2) > 0) {
      return(rT[rT$ENERGY.SOURCE%in%input$iconsS2,])
    }
    return(rT)
  } 
  if (input$state2 == "Washington DC") {
    rT <- ptableS2[ptableS2$STATE%in%"DC",]
    rT <- subset(rT, rT$YEAR==input$year2) 
    if(input$iconsS != "All" && length(input$iconsS2) > 0) {
      return(rT[rT$ENERGY.SOURCE%in%input$iconsS2,])
    }
    return(rT)
  }
  inp <- state.abb[which(state.name==input$state2)]
  rT <- ptableS2[ptableS2$STATE%in%inp,]
  rT <- subset(rT, rT$YEAR==input$year2) 
  if(input$iconsS2 != "All" && length(input$iconsS2) > 0) {
    return(rT[rT$ENERGY.SOURCE%in%input$iconsS2,])
  }
  return(rT)
  
})

#====================================================================
# amount of each energy source per year from 1990-2019, in Dashboard
output$stacked1 <- renderPlot({
    
    ggplot(energy2, aes(fill=ENERGY.SOURCE, 
                       y=GENERATION..Megawatthours., x=YEAR)) +
      geom_bar(position="stack", stat="identity") + 
        labs(x="Year", y="Generation MWh") 
})    

output$stacked2 <- renderPlot({
  
  ggplot(energy2, aes(fill=ENERGY.SOURCE, x=YEAR, 
                      y=GENERATION..Megawatthours.)) +
    geom_bar(position = "fill", stat = "identity") + 
    scale_y_continuous(labels = scales::percent) +
    labs(x="Year", y="% Generation MWh") 
  
})

output$line1 <- renderPlot({
  if(input$icons == "All"  || !length(input$icons)) { 
    ggplot(energy2, aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE, 
                        y=GENERATION..Megawatthours., x=YEAR)) + 
      stat_summary(fun="sum", geom="line") + labs(x="Year", y="Generation MWh")
  } else {
    ggplot(reactiveSources(), aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE, 
                                  y=GENERATION..Megawatthours., x=YEAR)) + 
      stat_summary(fun="sum", geom="line") + labs(x="Year", y="Generation MWh")
  }
})

output$line2 <- renderPlot({
  if(input$icons == "All"  || !length(input$icons)) {
    ggplot(percentages, aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE,
                            y=PERCENT, x=YEAR)) +
      stat_summary(fun="sum", geom="point") +
      stat_summary(fun="sum", geom="line") +
      labs(x="Year", y="% Generation MWh") 
  } else {
    ggplot(reactiveSourcesP(), aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE,
                                   y=PERCENT, x=YEAR))+
      stat_summary(fun="sum", geom="point") +
      stat_summary(fun="sum", geom="line") +
      labs(x="Year", y="% Generation MWh") 
  }
})

output$tab1 <- DT::renderDataTable({
  DT::datatable(data=ptable, options = list(pageLength=5))
})

output$tab2 <- DT::renderDataTable({
  DT::datatable(data=ptable2, options = list(pageLength=5))
})

#====================================================================
# Show based on state, energy source, and year in ByState

# STACKED BAR BY GENERATION
output$stacked1S <- renderPlot({
  
  ggplot(reactiveStateL(), aes(fill=ENERGY.SOURCE, 
                      y=GENERATION..Megawatthours., x=YEAR)) +
    geom_bar(position="stack", stat="identity") + 
    labs(x="Year", y="Generation MWh") 
})    

output$stacked1S2 <- renderPlot({
  
  ggplot(reactiveStateL2(), aes(fill=ENERGY.SOURCE, 
                               y=GENERATION..Megawatthours., x=YEAR)) +
    geom_bar(position="stack", stat="identity") + 
    labs(x="Year", y="Generation MWh") 
})    

# STACKED BAR BY % GENERATION
output$stacked2S <- renderPlot({
  
  ggplot(reactiveStateLP(), aes(fill=ENERGY.SOURCE, x=YEAR, 
                      y=PERCENT)) +
    geom_bar(position = "fill", stat = "identity") + 
    scale_y_continuous(labels = scales::percent) +
    labs(x="Year", y="% Generation MWh") 
  
})

output$stacked2S2 <- renderPlot({
  
  ggplot(reactiveStateLP2(), aes(fill=ENERGY.SOURCE, x=YEAR, 
                                y=PERCENT)) +
    geom_bar(position = "fill", stat = "identity") + 
    scale_y_continuous(labels = scales::percent) +
    labs(x="Year", y="% Generation MWh") 
  
})

# LINE CHART BY GENERATION MWh
output$line1S <- renderPlot({
  if(input$iconsS == "All"  || !length(input$iconsS)) { 
    ggplot(reactiveStateL(), aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE, 
                                 y=GENERATION..Megawatthours., x=YEAR)) + 
      stat_summary(fun="sum", geom="point") + labs(x="Year", y="Generation MWh")
  } else {
    ggplot(reactiveStateL(), aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE, 
                                 y=GENERATION..Megawatthours., x=YEAR)) + 
      stat_summary(fun="sum", geom="point") + labs(x="Year", y="Generation MWh")
  }
})

output$line1S2 <- renderPlot({
  if(input$iconsS2 == "All"  || !length(input$iconsS2)) { 
    ggplot(reactiveStateL2(), aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE, 
                                  y=GENERATION..Megawatthours., x=YEAR)) + 
      stat_summary(fun="sum", geom="point") + labs(x="Year", y="Generation MWh")
  } else {
    ggplot(reactiveStateL2(), aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE, 
                                  y=GENERATION..Megawatthours., x=YEAR)) + 
      stat_summary(fun="sum", geom="point") + labs(x="Year", y="Generation MWh")
  }
})

# LINE CHART BY % GENERATION MWh
output$line2S <- renderPlot({
  if(input$iconsS == "All"  || !length(input$iconsS)) { 
    ggplot(reactiveStateLP(), aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE, 
                                 y=PERCENT, x=YEAR)) + 
      stat_summary(fun="sum", geom="point") + labs(x="Year", y="% Generation MWh")
  } else {
    ggplot(reactiveStateLP(), aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE, 
                                 y=PERCENT, x=YEAR)) + 
      stat_summary(fun="sum", geom="point") + labs(x="Year", y="% Generation MWh")
  }
})

output$line2S2 <- renderPlot({
  if(input$iconsS2 == "All"  || !length(input$iconsS2)) { 
    ggplot(reactiveStateLP2(), aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE, 
                                  y=PERCENT, x=YEAR)) + 
      stat_summary(fun="sum", geom="point") + labs(x="Year", y="% Generation MWh")
  } else {
    ggplot(reactiveStateLP2(), aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE, 
                                  y=PERCENT, x=YEAR)) + 
      stat_summary(fun="sum", geom="point") + labs(x="Year", y="% Generation MWh")
  }
})

# TABLE BY AMOUNT GENERATED
output$tab1S <- DT::renderDataTable({
  DT::datatable(data=tbl1Reactive(), options = list(pageLength=5))
})

output$tab1S2 <- DT::renderDataTable({
  DT::datatable(data=tbl2Reactive(), options = list(pageLength=5))
})

# TABLE BY PERCENT GENERATED
output$tab2S <- output$tab2S2 <- DT::renderDataTable({
  DT::datatable(data=tbl1ReactiveP(), options = list(pageLength=5))
})

output$tab2S2 <- DT::renderDataTable({
  DT::datatable(data=tbl2ReactiveP(), options = list(pageLength=5))
})

}

# Run the application 
shinyApp(ui = ui, server = server)
