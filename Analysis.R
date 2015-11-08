library("rvest")
library("ggplot2")
library("lubridate")
library("plyr")
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
  df$state <- str_trim(df$state)
  
  
  
  #Remove NAs and Currency
  df <- df %>% 
        select(-c(currency, title)) %>% 
        filter(!is.na(paid)) 
        
    df <- df %>% filter(!is.na(state) & state!="")

  
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
  
  tabs <- ldply(tabs[1:3])
  tabs <- tabs[12:40,]
  tabs <- tabs[,-c(1:5)]
  
  #remove wikilinks
  names(tabs) <- c("state", "code", "formation_date", "population", "area", "langugages", "capital",
                   "largest_city_if_not_capital", "population_density", "literacy", "urban_population_share")
  
  tabs <- as.data.frame(lapply(tabs, function(y) gsub(pattern = "\\[.*\\]", replacement =  "", x = y)))
  tabs <- as.data.frame(lapply(tabs, function(y) gsub(pattern = "\\%", replacement =  "", x = y)))
  tabs <- as.data.frame(lapply(tabs, function(y) gsub(pattern = "N\\/A", replacement =  NA, x = y)))
  # tabs$state <- gsub(x = tabs$state, pattern = "Delhi", ) #Match data from Punjab( wiki ) into Delhi (df)
  tabs$state <- gsub(x = tabs$state, pattern = "Odisha", replacement = "Orissa")
  tabs$literacy <- as.numeric(as.character(tabs$literacy))
  tabs$population_density <- as.numeric(as.character(tabs$population_density))
  tabs$urban_population_share <- as.numeric(as.character(tabs$urban_population_share))
  
#     
#   names.wiki <- factor(tabs$name) %>% 
#                 unique() %>% 
#                 sort()
#   
#   names.df <- factor(df$state) %>% 
#                 unique() %>%
#                 sort()
#   
  tabs2 <- tabs %>% select(state, population, literacy, population_density, urban_population_share)
  
  tabs3 <- full_join(state.table, tabs2, by = "state")
  
  df <- left_join(df, tabs2, by = "state")
  
  
### Kandidater til analyse ###
  
  # (1) Historie i at "ikke" finde noget
  # (2) Gennemsnitsomkostninger for bribe-typer
  # (3) Sammenhæng mellem gennemsnitsomk. og {urban_population_share, pop_density, literacy}
  # (4) Samlede bribes per stat
  # (5) Fordelingen af bribes (evt. indenfor en kategori)
  # (6) Alt på kort
  # (7) Typer af bribes
  # (8) Krydsplot, kun for birth-certificate, x = {density, literacy, urb_pop_share}, y = amount 
  
  
  
  p <- ggplot(data= df, aes(x = urban_population_share, y = paid))+
       geom_point()
  p
  