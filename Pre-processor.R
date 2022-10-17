# The main aim of this code was to split the data into smaller chunks and rename them appropriately.
# This has obvious implications on memory allocation in the server in the case of many users.
library(data.table)
library(dplyr)
library(vroom)
library(stringr)
library(feather)

setwd("E:/Google Drive/Jobs/Appsilon/Shiny_Appsilon/Appsilon_Assignment/www")

df <-fread("0076887-220831081235567.csv")
head(df)


# This data clearly does not contain [vernacularName] column, so we will use
# verbatimScientificName and scientificName to represent either the
# vernacularName and scientificName

df <- select(df,
             c("verbatimScientificName", "scientificName", "occurrenceID", "decimalLongitude", "decimalLatitude", "eventDate"))

# We and remove duplicates 
# We remove duplicates and rename the columns
df <- df[!duplicated(df),]
colnames(df) <- c("ScientificName", "vernacularName", "ObservationURL", "Longitude", "Latitude", "Date")

#-------------Critical step-----It takes time-----This code was run elsewhere
# Our vernacular name is not actually vernacular, so we have to find it from the web
# Set vernacular name to 0 or Null or anything really, we will update it, remove duplicates

df$vernacularName <- 0
# dfv <- select(df, c("ScientificName", "vernacularName"))
# dfv <- dfv[!duplicated(dfv),]
# 
# # This function extracts the vernacular name from the observations website
# # It takes the scientific name and searches the observation.org website
# 
# getVernacular <- function(txt){
#   name <- gsub(" ", "+", txt)
#   name <- gsub("\\/.*",'', name)
#   url0 <- paste0("https://observation.org/species/search/?q=", name, "&species_group=0")
#   tryCatch(webpage <- url0 %>% GET(., timeout(30)) %>% read_html(),
#            error = function(e) {webpage <<- NA })
#   if(!is.na(paste(webpage)[[1]]) == FALSE){
#     name <- name
#   }else{
#     name2 <- webpage %>%
#       html_nodes("span.species-common-name") %>%
#       html_text()
#     if (length(name2) == 0){
#       name <- name
#     } else{
#       name <- name2[[1]]
#     }
#     
#   }
#   return(name)
# }
# 
# # Get all the vernacular names from all unique scientific name
# i <- 254
# while (i <= nrow(dfv)) {
#   dfv[i,2] <- getVernacular(dfv[i,1])
#   i <- i+1
# }

#----------YOU NOW HAVE BOTH THE SCIENTIFIC AND THE VERNACULAR NAME-----------

# next merge the data based on the scientific name
# save the data because it may be needed standalone. Comment it out immediately after use
#-------fwrite(dfv, "sci-and-vernacular.csv", sep = ",", eol = "\r", row.names = F)

dfv <- fread("sci-and-vernacular.csv", sep = ",")

df <- df[,-2] #get rid of the unnecessary initial vernacular column
df <- left_join(df, dfv, by = "ScientificName")

# we only need to search for either and they will appear as a pair (understand this
# point because it simplifies development tasks downstream)
# Remove punctuation marks

df$ScientificName <- gsub("[[:punct:]]", " ", df$ScientificName)
df$VernacularName <- gsub("[[:punct:]]", " ", df$VernacularName)

# Next we merge the scientific Name and vernacular Name since
# continue as above
df$ScientificName <- paste(df$ScientificName, "-", df$VernacularName)


# Get the combined Names and remove duplicates
df3 <- select(df,
              "ScientificName")
df3 <- df3[!duplicated(df3),]


#-------------review from here
# drop the vernacular name
df <- df[,-6]

# The observationURL contains repetitive information which only serves to
# take away memory and performance.
# Lets remove the "https://observation.org/observation/" from observationURL

df$ObservationURL <- gsub("https://observation.org/observation/", "", df$ObservationURL)

# Our app will need to be searched and we will need it to be fast.
# We will extract the scientific and vernacular name combinations into a separate file

df2 <- select(df, "ScientificName")

# Remove the the many duplicates in the data
df2 <- df2[!duplicated(df2),]

# Add a row with no data so the map can load with no initial data to display
# Actually no need, our map will load real fast!
# df2[nrow(df2)+1, 1] <- c("None Selected")

# We can save these two datasets in RDS or CSV or TXT or FEATHER or whatever format since we want a lean app
# We also want it to be read very very ffffast
# RDS formats have shown to be optimum in both reading and writing files.
# ------->feather formats DID NOT Perform so well actually!!!!
# In our case, we are more interested in storage. Storage issues happen at scale
# if say we were dealing with the entire world, we would have a huge constraint (I explored and covered the issue in the readME!)
#

saveRDS(df, "Data.rds")
saveRDS(df2, "species.rds")


df <- readRDS("Data.rds")
df1 <-readRDS("species.rds")

df$ScientificName <- gsub("Ã", "ae", df$ScientificName)
df1$ScientificName <- gsub("Ã", "ae", df1$ScientificName)

i <- 1
while (i<=nrow(df1)) {
  df4 <- df[df$ScientificName == as.character(df1[i,1]),]
  jinna <- paste(gsub("\\ -.*",'', df1[i,1]), ".rds", sep = "")
  saveRDS(select(df4, c("Longitude", "Latitude", "Date","ObservationURL")), file = jinna)
  i <- i+1
}

# Finished. Proceed to app section.