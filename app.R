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

g_sheet <- function(file){
    dataKING <- file
    gs4_deauth()
    gs4_auth(cache = ".secrets", email = "labatt.comptexx@gmail.com")
    ss <- "https://docs.google.com/spreadsheets/d/1WPikfvmrovBY0Z7R6rzhkP_lY85K4NMUT3ojWd964jo/edit"
    sheet_append(ss, dataKING)
}

fom_txt <- function(txt){
    if (nchar(txt) < 1){
        dataKING <- data.frame(matrix(ncol = 4, nrow = 0))
        colnames(dataKING) <- c("Website", "Email", "Company", "Services")
    }else{
        
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
    
    return(dataKING)
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
