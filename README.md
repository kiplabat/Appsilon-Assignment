Appsilon - Dashboard for Polish Biodiversity Observation
================
Kiplabat Tarus
10/17/2022

## Abstract

This R Shiny app was passionately developed to visualize the
biodiversity that has been observed in Poland over the years. Thanks to
Appsilon for initiating this project as part of their technical skill
evaluation. This README will begin by highlighting the requirements of
the task and the other sections will be dedicated to how the identified
requirements were met. The final section will summarize the results of
the task and give insights and recommendations.

## Inroduction

This assignment was split into two tasks. The main task and extras.

1.  Main Task: To “Build a dashboard that main purpose is to visualize
    selected species observations on the map and how often it is
    observed”. The user can search either the scientific or the common
    name and the app will display its observation on a map and as a
    timeline. On the technical side, the app should also have shiny
    modules and should be tested for use cases. The app should also be
    deployed to <http://shinyapps.io> and the solution also be shared on
    github. Room for addition of features was also allowed.

2.  Extras: Optionally, the following skills could be integrated into
    the app. UIs (using CSS and Sass), Performance optimization,
    JavaScript, and Infrastructure know-how.

## Main Task

### Data and Preprocessing

The data shared at
<https://drive.google.com/file/d/1l1ymMg-K_xLriFv1b8MgddH851d6n2sU/view?usp=sharing>
was quite large and was technically unusable considering the specs of
the available computer.

The data hosted at
<https://www.gbif.org/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165>
already had one filter applied and was quite sizable as well. I applied
the country filter (Poland) and downloaded the data. A pre-processing
step was necessary right after unpacking the data. The code chunk below
was used to both clean the data and prepare it for usage in the app.

``` r
# The main aim of this code was to split the data into smaller chunks and rename them appropriately.
# This has obvious implications on memory allocation in the server in the case of many users.
# These packages were used.
library(data.table)
library(dplyr)
library(vroom)
library(stringr)
library(feather)
library(rvest)
library(htmltools)
library(tidyverse)
library(RCurl)
library(stringr)
library(qdapRegex)
library(htm2txt)
library(httr)
library(tidyr)
library(DT)

setwd("~/www")

df <-fread("0076887-220831081235567.csv")
head(df)


# This data clearly does not contain [vernacularName] column
# I used the scientificName to get the Vernacular (common) name on the <http://observation.org> website.

df <- select(df,
             c("verbatimScientificName", "scientificName", "occurrenceID",
               "decimalLongitude", "decimalLatitude", "eventDate"))

# We remove duplicates and rename the columns
df <- df[!duplicated(df),]
colnames(df) <- c("ScientificName", "vernacularName",
                  "ObservationURL", "Longitude", "Latitude", "Date")
```

Getting the vernacular names (WEB SCRAPING)

``` r
# Our vernacular name is not actually vernacular, so we have to find it from the web
# Set vernacular name to 0 or Null or anything really, we will update it, remove duplicates

df$vernacularName <- 0
dfv <- select(df, c("ScientificName", "vernacularName"))
dfv <- dfv[!duplicated(dfv),]
 
# This function extracts the vernacular name from the observations website
# It takes the scientific name and searches the observation.org website

getVernacular <- function(txt){
  name <- gsub(" ", "+", txt)
  name <- gsub("\\/.*",'', name)
  url0 <- paste0("https://observation.org/species/search/?q=", name, "&species_group=0")
  tryCatch(webpage <- url0 %>% GET(., timeout(30)) %>% read_html(),
           error = function(e) {webpage <<- NA })
  if(!is.na(paste(webpage)[[1]]) == FALSE){
    name <- name
  }else{
    name2 <- webpage %>%
      html_nodes("span.species-common-name") %>%
      html_text()
    if (length(name2) == 0){
      name <- name
    } else{
      name <- name2[[1]]
    }

  }
  return(name)
}

# Get all the vernacular names from all unique scientific name using a while loop and the function.
i <- 1

while (i <= nrow(dfv)) {
  dfv[i,2] <- getVernacular(dfv[i,1])
  i <- i+1
}
```

Now we have the vernacular name (common name)

``` r
# We next merge the data based on the scientific name
# and save the data because it may be lost. The loop takes a while. Comment it out immediately after use

fwrite(dfv, "sci-and-vernacular.csv", sep = ",", eol = "\r", row.names = F)

df <- df[,-2] #get rid of the now unnecessary initial vernacular column
df <- left_join(df, dfv, by = "ScientificName")

# we only need to search for either the vernacular or the scientific name
# and they should appear as a pair (PART OF MAIN TASK)
# Remove punctuation marks and merge the fields

df$ScientificName <- gsub("[[:punct:]]", " ", df$ScientificName)
df$VernacularName <- gsub("[[:punct:]]", " ", df$VernacularName)

df$ScientificName <- paste(df$ScientificName, "-", df$VernacularName)

# drop the vernacular name now that we have merged
df <- df[,-6]

# The observationURL contains repetitive information which only serves to
# take away precious memory and performance.
# Lets remove the "https://observation.org/observation/" from observationURL

df$ObservationURL <- gsub("https://observation.org/observation/", "", df$ObservationURL)

# Our app will need to be searched and we will need it to be fffast.
# We will extract the scientific and vernacular name combinations into a separate file

df2 <- select(df, "ScientificName")

# Remove the the many duplicates in the data
df2 <- df2[!duplicated(df2),] # Now we have a few thousand species to search from

# Replace these characters, the brought issues while deploying the app
df$ScientificName <- gsub("Ã", "ae", df$ScientificName)
df2$ScientificName <- gsub("Ã", "ae", df2$ScientificName)
```

We have two datasets which can be saved as RDS or CSV or TXT or FEATHER
or whatever format. We also want it to be read very ffffast and be
memory efficient. RDS formats have shown to be optimum in both reading
and writing files. Several data formats were tested. Feather formats DID
NOT Perform so well. fread and fwrite were tested and quite did not
perform. In this case, we are more interested in storage and
performance. Storage issues happen at scale at if say we were dealing
with the entire world, we would have a huge constraint. On performance
even if the files are a few kilobytes, the reading time factor is still
present. However, in human time, it may be unnoticeable if the files are
just a few kilobytes.

``` r
# save in RDS format
saveRDS(df, "Data.rds")
saveRDS(df2, "species.rds")

# Write out the individual species data in RDS format. Making sure also that the scienfic name is not transferred inside the file. The name of the file will ID the species and its data will be read in.
i <- 1
while (i<=nrow(df2)) {
  df4 <- df[df$ScientificName == as.character(df2[i,1]),]
  jinna <- paste(gsub("\\ -.*",'', df2[i,1]), ".rds", sep = "")
  saveRDS(select(df4, c("Longitude", "Latitude", "Date","ObservationURL")), file = jinna)
  i <- i+1
}

# Finished. Proceed to app section.
```

### App Development

The business requirements were considered. This is the technical side.

The app UI underwent several iterations and I settled for a traditional
Shiny app look. The fluiPage design is both easy for prototyping and is
mobile friendly.
