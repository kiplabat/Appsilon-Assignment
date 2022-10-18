#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(rvest)
library(tidyverse)
library(RCurl)
library(data.table)
library(dplyr)
library(RSelenium)
library(stringr)
library(qdapRegex)
library(htm2txt)
library(httr)
library(shiny)
library(tidyr)
library(DT)
library(googlesheets4)
library(leaflet.esri)

g_sheet <- function(file){
    dataKING <- file
    gs4_deauth()
    gs4_auth(cache = ".secrets", email = "labatt.comptexx@gmail.com")
    ss <- "https://docs.google.com/spreadsheets/d/1WPikfvmrovBY0Z7R6rzhkP_lY85K4NMUT3ojWd964jo/edit"
    sheet_append(ss, dataKING)
}

<<<<<<< HEAD
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
=======
fom_txt <- function(txt){
    if (nchar(txt) < 1){
        dataKING <- data.frame(matrix(ncol = 4, nrow = 0))
        colnames(dataKING) <- c("Website", "Email", "Company", "Services")
    }else{
>>>>>>> 418e969f57bcdf7b94fc500fe6b891fd78ea157b
        
        dataKING <- data.frame(strsplit(txt, split = " "))
        dataKING$Email <- c("")
        dataKING$Company <- c("")
        dataKING$Services <- c("")
        colnames(dataKING) <- c("Website", "Email", "Company", "Services")
        g_sheet(dataKING)
        if(nrow(dataKING) <= 5){
            dataKING <- dataKING
        }else{
            dataKING <- dataKING[1:5, ]   
        }    
    }
    
<<<<<<< HEAD
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
=======
    return(dataKING)
>>>>>>> 418e969f57bcdf7b94fc500fe6b891fd78ea157b
}


get_contact <- function(txt) {
    dataKING <- fom_txt(txt)
    if(nrow(dataKING) > 0){
    i <- 1
    while (i <= nrow(dataKING)) {
        
        url_look <- as.character(dataKING[i,1])
        url_look <- gsub("http://www.", '', url_look)
        url_look <- gsub("https://www.", '', url_look)
        url_look <- gsub("http://", '', url_look)
        url_look <- gsub("https://", '', url_look)
        url_look <- gsub("/",'@@',url_look)
        url_look <- gsub("\\@.*",'',url_look)
        
        url0 <- paste("http://", url_look, sep = '')
        tryCatch(website <- url0 %>% GET(., timeout(30)) %>% read_html(),
                 error = function(e) {website <<- NA })
        if(!is.na(paste(website)[[1]]) == FALSE){
            dataKING <- dataKING
        }else{
            tryCatch(dataKING[i,3] <- paste(website %>% html_nodes("head > title") %>% html_text(), collapse = " | "),
                     error = function(e) {dataKING[i,3] <<- NA })
            tryCatch(dataKING[i,4] <- paste(website %>% html_nodes(xpath = '//meta[@name="description"]') %>% html_attr("content"), collapse = " | "),
                     error = function(e) {dataKING[i,4] <<- NA })
            tryCatch(extract_email1 <- website %>% paste(sep='') %>% ex_email %>% c(),
                     error = function(e) {extract_email1 <<- c(NA) })
            tryCatch(links <- data.frame(url0 %>% GET(., timeout(30)) %>% read_html() %>% html_nodes("a") %>% html_attr("href")),
                     error = function(e) {links <<- data.frame("") })
            # if(nrow(links) < 1){
            #     links <- data.frame("")
            # }else{
            #     links <- links
            # }
            # 
            
            links[nrow(links) + 1,] <- paste(url0, "/contact", sep = "")
            links[nrow(links) + 1,] <- paste(url0, "/contact-us", sep = "")
            links[nrow(links) + 1,] <- paste(url0, "/contact.html", sep = "")
            links[nrow(links) + 1,] <- paste(url0, "/contact.htm", sep = "")
            links[nrow(links) + 1,] <- paste(url0, "/contact-us.html", sep = "")
            links[nrow(links) + 1,] <- paste(url0, "/contact-us.htm", sep = "")
            
            
            if(count(links)$n >= 1){
                colnames(links) <- "links"
                links <- filter(links, grepl('contact|touch', links))
                if(count(links)$n >= 1){
                    m <- 1
                    while(m <= nrow(links)){
                        urlx <- links[m,1]
                        tryCatch(email_extract <- urlx %>% GET(., timeout(30)) %>% read_html() %>% paste(sep='') %>% ex_email() %>% c(),
                                 error = function(e) {email_extract <<- c(NA) })
                        
                        if(!is.na(email_extract[[1]]) == TRUE){
                            extract_email1 <- c(extract_email1, email_extract)
                        }else{
                            extract_email1 <- c(extract_email1)
                        }
                        m <- m+1
                        dataKING[i,2] <- extract_email1 %>% unique() %>%  toString() %>% ex_email() %>% paste(collapse = " , ")
                    }
                }else{
                    i <- i
                }
            }else{
                i <- i
            }
        }
        i <- i + 1
    }
    g_sheet(dataKING)
    }else{
        dataKING <- dataKING
    }
    return(dataKING)
}



# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(tags$link(rel="shortcut icon", href='scraper_icon.png')),
    tags$head(HTML("<title>Comptexx | Web Scraping</title>")),
    tags$head(HTML('<meta name="Keywords" content = "Best email finder tool, web scraping services, machine learning services, how to find emails on website">')),
    tags$head(HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0">')),
    
    
    fluidRow(
        column(12,
               tags$hr(),
               fluidRow(align = "center",
                   tags$em("Scan multiple websites for emails and service descriptions", align = "center"),
                   ),
               tags$hr(),
               fluidRow(align = "center",
                       img(src='Company_logo.png', align = "center"),
                       textInput("text", label = "", width = '60%', placeholder = "Input up to 5 URLs here, separated by space *DO NOT MIND THE INITIAL ERROR"),
                       submitButton("Scan", icon("search")),
                       tags$hr()
                   ),
               fluidRow(
                   column(width = 10, offset = 1, align = "center",
                       dataTableOutput("contact"),
                       tags$hr(),
                       tags$em("Contact us if you need to check more than 5 websites at a time"),
                       br(),
                       tags$em("labatt.comptexx@gmail.com | +254700219997"),
                       #tags$em("Charges: 1-500 websites (5",HTML("&#162;")," per email(s) found and 1",HTML("&#162;")," per website visited), 501-2000 websites (4",HTML("&#162;")," per email(s) found and 1",HTML("&#162;")," per website visited), Above 2000 websites (3",HTML("&#162;")," per email(s) found and 1",HTML("&#162;")," per website visited)"),
                   ),
                   
               ),
               tags$hr(),
               fluidRow(align = "center",
                   tags$em(HTML("&#169;"),"2022 Comptexx Services"),
               )
               
               ),
               
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$contact <- DT::renderDataTable(get_contact(input$text),
                                          extensions = c('Buttons', 'Scroller'),
                                          options = list(
                                              dom = 'Bfrtip',
                                              deferRender = TRUE,
                                              scrollY = 220,
                                              scroller = TRUE,
                                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                        ))
}

shinyApp(ui = ui, server = server)
