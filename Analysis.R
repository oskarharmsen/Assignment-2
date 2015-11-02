library("rvest")
library("ggplot2")
library("lubridate")
library("dplyr")
library("stringr")
library("XML")
library("httr")


getwd()
setwd("/Users/oskarh/Documents/Assignment 2/Assignment-2/")

### Load data
  df <- read.csv("Assign2.csv", header = TRUE, sep = ";")
  names(df) <- c("date", "type", "town", "state", "views", "department", "currency", "paid", "title")
  
  #Correct classes of columns
  df$town <- str_trim(df$town)
  df$title <- str_trim(df$title)
  df$date <- as.Date(df$date)
  df$views <- as.numeric(df$views)
  df$paid <- as.numeric(df$paid)
  
  #Remove NAs and Currency
  df <- df %>% 
        select(-c(currency, title)) %>% 
        filter(!is.na(paid))

  
### Basic data
  
  #Group by topic
  type.table <- df %>%
    group_by(type) %>% 
    summarise(count = n(), mean = mean(paid)) %>% 
    arrange(desc(count))
  
  #Group by department
  department.table <- df %>%
    group_by(department) %>% 
    summarise(count = n(), mean = mean(paid)) %>% 
    arrange(desc(count))
  
  #Group by state
  state.table <- df %>%
    group_by(state) %>% 
    summarise(count = n(), mean = mean(paid)) %>% 
    arrange(desc(count))
  
  
  
# Download wikipedia table, mergable with own dataset
  
  url <- "https://en.wikipedia.org/wiki/States_and_union_territories_of_India"
  tabs <- GET(url, encoding = "UTF-8")
  tabs <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F, header = TRUE)
  library(plyr)  
  tabs <- ldply(tabs[1:3])
  tabs <- tabs[12:40,]
  tabs <- tabs[,-c(1:5)]
  
  #remove wikilinks
  names(tabs) <- c("state", "code", "formation_date", "population", "area", "langugages", "capital",
                   "largest_city_if_not_capital", "population_density", "literacy", "urban_population_share")
  
  tabs <- as.data.frame(lapply(tabs, function(y) gsub(pattern = "\\[.*\\]", replacement =  "", x = y)))
  tabs <- as.data.frame(lapply(tabs, function(y) gsub(pattern = "\\%", replacement =  "", x = y)))
  tabs <- as.data.frame(lapply(tabs, function(y) gsub(pattern = "N\\/A", replacement =  NA, x = y)))
  
  names.wiki <- factor(tabs$name) %>% 
                unique() %>% 
                sort()
  
  names.df <- factor(df$state) %>% 
                unique() %>%
                sort()
  
  
  
  