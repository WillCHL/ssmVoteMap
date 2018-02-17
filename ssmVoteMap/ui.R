
library(shiny)
library(leaflet)
library(shinycssloaders)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    tags$head(
        HTML("<head><title>Australian Marriage Law Postal Survey Map</title></head>")),
  
  # Application title
  titlePanel("2017 Australian Marriage Law Postal Survey - Results Map"),
    fluidRow(column(width=7,p("Site is best viewed with Firefox of Chrome, there seem to be some IE issues.",br(),
                               "This page is a work in progress so keep an eye out for additions..."), style="font-size: 13px"),
             column(width=5,fluidRow(column(2,actionButton("loadSuburb","Load")),
                                    column(10,em(p("Load suburb map layers (slow)", style="font-size: 12px")))))),
    
  fluidRow(
      column(width=8,
             withSpinner(leafletOutput("mymap", width = "100%", height = 850),5)
      ),
      column(width=4,
             p(strong("Click Map to update graphs for Electorate."),br(),
             em("(Rate and number of participants)"),br(), style="font-size: 13px"),
             fluidRow(
                 withSpinner(plotOutput("partPlot", width="100%", height=370),5,size=.5)
             
             ),
             fluidRow(column(width=5,strong("Census Data:")),
                      column(width=7,selectInput("dataset", NULL,choices =  c("Age Breakdown", "Religious Affiliation", "Marriage Status"),
                                  selected = "Age Breakdown"))),
             fluidRow(
                 withSpinner(plotOutput("censusPlot", width="100%", height=380),5,size=.5)
                 
             ))
      
  ),
  
  fluidRow(column(width=4,h4("Data Sources:"))),
  fluidRow(column(width=12,p("Vote data: ",a("https://marriagesurvey.abs.gov.au/results/downloads.html",
                                             href="https://marriagesurvey.abs.gov.au/results/downloads.html"),br(),
                             "Region boundries and names: ",a("http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202017",
                                                              href="http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202017"),br(),
                             "Parliment members: ",a("https://en.wikipedia.org/wiki/Members_of_the_Australian_House_of_Representatives,_2016%E2%80%932019",
                                                     href="https://en.wikipedia.org/wiki/Members_of_the_Australian_House_of_Representatives,_2016%E2%80%932019"),br(),
                             "Census data: ", a("https://datapacks.censusdata.abs.gov.au/datapacks/", 
                                                    href="https://datapacks.censusdata.abs.gov.au/datapacks/"),style="font-size: 13px")))
  
))
