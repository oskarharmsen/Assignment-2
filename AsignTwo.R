
# settings

Sys.setlocale(category = "LC_ALL", locale = "UTF-8")


# loading packages: 

library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("rvest")

# **************** (1) data scraping *********************** #

# declare an empty global dataframe:

df <- data.frame()

# create an overall loop to collect the "underpages" at the webpage, and then start scraping. 100 pages = 1000 cases.

for (i in seq(0,990,10)){

# generate the address to scape data from:
t <- c(i)
t <- t[1]

link <- "http://www.ipaidabribe.com/reports/paid?page="
link <- paste(paste(link, t, sep = ""),"#gsc.tab=0", sep ="")

# scrape the data from the link created above:
link <- html(link) 

n_title <- link %>%
  html_nodes(".heading-3 a") %>%
  html_text()

n_paid <- link %>%
  html_nodes(".paid-amount span") %>%
  html_text()

n_department <- link %>%
  html_nodes(".name a") %>%
  html_text()

n_views <- link %>%
  html_nodes(".overview .views") %>%
  html_text()

n_city <- link %>%
  html_nodes(".location") %>%
  html_text()

n_trans <- link %>%
  html_nodes(".transaction a") %>%
  html_text()

n_date <- link %>%
  html_nodes(".date") %>%
  html_text() %>%
  strptime(format = "%B %d, %Y") %>%
  as.character()

# make a datasets containing the the scraped information and bring it together to the global dataset, df.

data <- data.frame(cbind(n_date, n_trans, n_city, n_views, n_department, n_paid, n_title))

df <- rbind(df,data)

# command to slow down the scrapng process

Sys.sleep(0.6)

}
  
# **************** (2) data cleaning *********************** #


# split up "n_paid" and n_city":
dm <- df %>%
  separate(n_paid, c("n_currency","n_paid"), 8) %>%
  separate(n_city, c("n_town", "n_district"),",") 
# remove "view" from the n_view variable and "Paid" from n_currency:
dm$n_views <-  gsub(pattern = "[views]", "", as.character(dm$n_views))
dm$n_currency  <-  gsub(pattern = "[Paid]","",as.character(dm$n_currency)) 
#  
dm$n_paid <- gsub( x = dm$n_paid, pattern = "?\\,[0-9]+\\,", replacement = NA )
dm$n_paid <- gsub( x = dm$n_paid, pattern = "\\,", replacement = "")
dm$n_paid <- as.numeric(dm$n_paid)
dm <- dm %>% filter(!is.na(dm$n_paid))

# export data as .csv:
setwd("/Users/pernillekofod/Dropbox/Assignment-2")
        write.table(x = dm, file = "assign2.csv", 
                    fileEncoding = "UTF-8", sep = ";", row.names = FALSE, dec = ",")
   




# **************** (3) data analysis *********************** #

ggplot(dm, aes(n_district, n_paid))+geom_histogram(stat="identity")

ggmap

# **************** (4) mapping **************************** #

library(rgeos)
library(maptools)
library(sp)
library(ggplot2)
library(gpclib)
library(viridis)
library(ggthemes)

#load spatial data and fortify to dataframe#
india <- readRDS("/Users/susannesundgaardhansen/Downloads/IND_adm1.rds")
india <- fortify(india, region="NAME_1")

dm <- df %>% filter(dm$n_paid!="NA" & !is.na(dm$n_trans))
df$n_paid=as.numeric(df$n_paid)
df$n_district <- substring(df$n_district, 2, nchar(df$n_district))

#check if names of states matches in the two datasets#
namesInData <- levels(factor(df$n_district))
namesInMap <- levels(factor(india$id))
namesInData[which(!namesInData %in% namesInMap)]

#correct errors and spelling#
df$n_district<- gsub("Hadoi", "Uttar Pradesh", df$n_district)
df$n_district<- gsub("Uttarakhand", "Uttaranchal", df$n_district)

#new dataframe that summarizes over districts and amounts paid#
df1 <- ddply(df, c("n_district"), summarise,
             paid=sum(n_paid))

#plotting the spatial data - CAUTION this takes a while! Approx. 10 minutes#
p <- ggplot() + geom_map(data=df1, aes(map_id = n_district, fill=paid),
                         map = india) + 
  expand_limits(x = india$long, y = india$lat)+ 
  coord_equal()+v
ggtitle("Reported bribes in India")+
  theme_tufte()+
  scale_fill_viridis(trans="log10", na.value ="grey50",
                     name="Amount paid \n(logs)")
p
