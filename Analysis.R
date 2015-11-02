library("rvest")
library("ggplot2")
library("lubridate")
library("dplyr")
library("stringr")
library("plyr")



### Load data
  df <- read.csv("Assign2.csv", header = TRUE, sep = ";")
  names(df) <- c("date", "type", "town", "district", "views", "department", "currency", "paid", "title")
  
  #Correct classes of columns
  df$title <- str_trim(df$title)
  df$date <- as.Date(df$date)
  df$views <- as.numeric(df$views)
  df$paid <- as.numeric(df$paid)
  
  #Remove NAs and Currency
  df <- df %>% 
        select(-c(currency)) %>% 
        filter(!is.na(paid)) %>% 
  
  