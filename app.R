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
#aggregate(GENERATION..Megawatthours. ~ YEAR + STATE + TYPE.OF.PRODUCER + ENERGY.SOURCE, energy, sum)
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

percentages <- rbind(coaldf, geodf, hydrodf, naturaldf, nucleardf, petroleumdf, solardf, winddf, wooddf)
percentages <- percentages[order(percentages$YEAR), ]
names(percentages)[names(percentages)=="GENERATION..Megawatthours..y"] <- "Total Generated MWh"
names(percentages)[names(percentages)=="GENERATION..Megawatthours..x"] <- "Generation MWh"
drops <- c("Total Generated MWh", "PERCENT")
ptable <- percentages[, !(names(percentages) %in% drops)]
drops <- c("Generation MWh", "Total Generated MWh")
ptable2 <- percentages[, !(names(percentages) %in% drops)]
ptable2$PERCENT <- lapply(ptable2$PERCENT, round, 2)


# Create menu items to select different years and different states
listStates <- as.character(unique(unlist(energy$STATE)))
years<-c(1990:2019)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    dashboardHeader(title = "CS424 Spring 2021 Project 1"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
        sidebarMenu(
            menuItem("", tabName = "cheapBlankSpace", icon = NULL),
            menuItem("", tabName = "cheapBlankSpace", icon = NULL),
            menuItem("", tabName = "cheapBlankSpace", icon = NULL),
            menuItem("", tabName = "cheapBlankSpace", icon = NULL),
            menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
            menuItem("", tabName = "cheapBlankSpace", icon = NULL),
            
            selectInput("Year", "Select the year to visualize", years, selected = 2019)
            #selectInput("State", "Select the state to visualize", listStates, selected = "")
        ),
    dashboardBody(
  fluidRow(
    column(2,
        fluidRow(
          box(title = "Amount of each energy source/year", solidHeader=TRUE, status="primary", width=17, 
              plotOutput("stacked1", height = 270))    
        ),
        fluidRow(
          box(title = "% of Total Production For Each Energy Source/Year", solidHeader=TRUE, status="primary", width=17,
              plotOutput("stacked2", height=270))
        ) 
    ),
    column(2, 
        fluidRow(
          box(title="Amount of Each Energy Source/Year", solidHeader=TRUE, status="primary", width=17,    
              plotOutput("line1", height=270))
        ),
        fluidRow(
          box(title="% of Total Production For Each Energy Source/Year", solidHeader=TRUE, status="primary", width=17,
              plotOutput("line2", height=270))
        )
    ),
    column(2,
        fluidRow(
          box(title="Table of Amount of Energy Source Per Year", solidHeader=TRUE, status="primary", width=19,
              dataTableOutput("tab1"))
        ),
        fluidRow(
          box(title="Table of % of Total Production of Each Energy Source/Year", solidHeader=TRUE, status="primary", width=19,
              dataTableOutput("tab2"))
        )
    )
  )    
    
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
# calculations
  energy2 <- energy[energy$ENERGY.SOURCE != "Total", ]
  
# amount of each energy source per year from 1990-2019
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
  
    ggplot(energy2, aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE, 
                       y=GENERATION..Megawatthours., x=YEAR)) + 
      stat_summary(fun.y="sum", geom="line") + labs(x="Year", y="Generation MWh") 
})

output$line2 <- renderPlot({
    ggplot(percentages, aes(group=ENERGY.SOURCE, color=ENERGY.SOURCE,
                       y=PERCENT, x=YEAR)) +
    stat_summary(fun.y="sum", geom="point") +
    stat_summary(fun.y="sum", geom="line") +
    labs(x="Year", y="% Generation MWh") 
})

output$tab1 <- DT::renderDataTable({
    DT::datatable(data=ptable, options = list(pageLength=5))
})

output$tab2 <- DT::renderDataTable({
  DT::datatable(data=ptable2, options = list(pageLength=5))
})

}

# Run the application 
shinyApp(ui = ui, server = server)
