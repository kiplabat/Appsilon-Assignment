library(shiny)
library(leaflet)
library(RColorBrewer)
library(data.table)
library(dplyr)
library(ggplot2)
library(rvest)
library(httr)
library(htmltools)
library(feather)
library(tidyverse)
library(RCurl)
library(RSelenium)
library(stringr)
library(qdapRegex)
library(htm2txt)
library(httr)
library(tidyr)
library(DT)
library(googlesheets4)
library(leaflet.esri)


# Read in the necessary files/ Considered as BASE FILES
species <- readRDS("www/species.rds")
df1 <- readRDS("www/Betula spec .rds")


# Function for reading in files.
choicer <- function(sciName){
    if(nchar(sciName)<1){
        df <- df1
    }else{
        df <- readRDS(paste("www/", gsub("\\ -.*",'', sciName), ".rds", sep = ""))
    }
    lnk1 <- paste("<a href=\"https://observation.org/observation/", df$ObservationURL,"\"", sep = "")
    lnk2 <- paste(" target=\"_blank\">", "Open Observation: ", df$ObservationURL, sep = "")
    df$ObservationID <- paste("https://observation.org/observation/", df$ObservationURL, sep = "")
    df$ObservationURL <- paste(lnk1, lnk2, "</a>", sep = "")
    
    return(df)
}

# Function for plotting data. Pass in data from reactive expression that depends on other functions.
pluto <- function(df){
    df$Date <- format(as.Date(df$Date, format = "%y-%mm-%dd"),"%Y")
    suppressWarnings(date.min <- min(as.numeric(df$Date))) ; suppressWarnings(date.max <- max(as.numeric(df$Date)))
    
    df <- as.character(df$Date) %>%
        table() %>%
        data.frame()
    
    colnames(df) <- c("Year", "Observations")
    df$Year <- as.numeric(as.character(df$Year))
    
    df1 <- seq(date.min, date.max, by=1) %>% 
        data.frame
    
    colnames(df1) <- "Year"
    df <- left_join(df1, df, by="Year")
    df$Year <- as.character(df$Year)

    df[is.na(df)] <- 0
    
    return(df)
}


ui <- fluidPage(id = "fluidPage1",

# Pass some data into HTML head, NOTE also the CSS style sheet (although not advanced type). Favicon and logo designed.
    tags$head(HTML("<title>App | Polish Biodiversity Observatory</title>",
                   '<meta name="Keywords" content = "Polish Biodiversity Observatory is an interactive web app for mapping various species sightings around Poland">',
                   '<meta name="viewport" content="width=device-width, initial-scale=1.0">'),
              tags$link(rel="shortcut icon", href='Logo.png'),
              tags$link(rel = "stylesheet", type = "text/css", href = "AppsilonStyles.css")),

# Create output sections stylized by CSS sheets    
    fluidRow(id = "fluidRow", align = 'center',
             column(2,
                    # Add a logo
                    titlePanel(title = span(img(src = "Logo2.png", height = 50, align = 'center'))),
             ),
             
             column(8, align = 'center',
                 tags$em("Search or select [Scientific Name - and/or- Vernacular Name] from the dropdown", align = 'center'),
                 # Add a server controlled selectInput dropdown
                 selectizeInput(width = 400,
                                'sciName',label = NULL,
                                choices = NULL,
                                multiple = FALSE),
             ),
             column(2, align = 'center',
                    # Add a section for displaying simple computations
                    tags$h6("Total Observations:"),tags$h4(strong(textOutput('totNum'))),
                    
             ),
             
    ),
    
    fluidRow(id = "fluidRow2",
        # Display for maps and a timeline plot
        column(6,
               leafletOutput("map"),
        ),
        column(6,
        fluidRow(id = "fluidRow2",
                 plotOutput("box"),
               ),
        ),
    ),
    fluidRow(id = "fluidRow3",
             # Display a simple table that has links to the observations and dates
             tags$h6(strong("Download Map Data"), align = "left"),
             dataTableOutput("mapInfo"),
             tags$h6(strong("Data: https://doi.org/10.15468/dl.8qs7ms?"), align = "center"),
             tags$h6(strong("Web scraping: https://observation.org/"), align = "center"),
    ),
    
)

server <- function(input, output, session) {
    # Output the total number of observations
    output$totNum <- renderText({paste("-",nrow(mapDat()),"-")})
    
    # Use observer to dynamically update select I/O
    observe({
        updateSelectizeInput(session,'sciName', choices = species$ScientificName, server = T)
    })
    
    # Create a reactive expression to communicate with data reading module
    mapDat <- reactive({
        choicer(input$sciName)
    })
    
    # Map with margins bounded
    output$map <- renderLeaflet({
        leaflet(df1) %>%
            setView(lng = 19.6, lat = 52.1, zoom = 6) %>%
            addEsriBasemapLayer(esriBasemapLayers$Topographic)
    })

    # Use observer to dynamically clear and update map
    observe({
        leafletProxy("map", data = mapDat()) %>%
            clearMarkers() %>%
            addMarkers(popup = ~ObservationURL, label = "Click Observation")
        })
    
    # Show a trend of observations with some styling
    output$box <- renderPlot({
        ggplot(tryCatch(mapDat() %>% pluto(),
                        error = function(e) {Year <- NA ; Observations <- 0 ; diff <- data.frame(Year, Observations)}), aes(x = Year, y = Observations)) + 
        geom_col(fill = 'royalblue4')+
            scale_y_continuous(expand = c(0, 0)) +
            geom_text(aes(label = Observations), angle = 90, vjust = 0.5, hjust = 1, color = "white") +
            theme(
                plot.background = element_rect(fill = "white"),
                plot.title = element_text( size = 13, margin = margin(0, 0, 0, 0), hjust = 0.5),
                axis.text.x = element_text(angle = 90, size = 10, margin = margin(0, 0, 0, 0)),
                axis.text.y = element_text(lineheight = 20)
                )
        
    })
    
    # Show a table of URL links. Remember the data pre-processing step that removed the link? Its back!

    output$mapInfo <- DT::renderDataTable(select(mapDat(), c("ObservationID", "Date", "Longitude", "Latitude")),
        extensions = c('Buttons', 'Scroller'),
        options = list(
            dom = 'Bfrtip',
            deferRender = TRUE,
            scrollY = 150,
            scroller = TRUE,
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
        )
}

shinyApp(ui, server)
