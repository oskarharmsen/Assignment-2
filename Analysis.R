library("rvest")
library("ggplot2")
library("lubridate")
library("dplyr")
library("stringr")
library("plyr")



### Load data
  df <- read.csv("Assign2.csv", header = TRUE, sep = ";")
  names(df) <- c("date", "type", "town", "district", "views", "department", "currency", "paid", "title")