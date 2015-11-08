
# settings

Sys.setlocale(category = "LC_ALL", locale = "UTF-8")


# loading packages: 

library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("rvest")

# **************** (1) data scraping *********************** #

# declare empty global dataframe:

df <- data.frame()

# create an overall loop to collect the "underpages" at the webpage, and then start scraping. 100 pages = 1000 cases.

for (i in seq(0,990,10)){

# generate the links to scape data from:
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

n_timeago <- link %>%
  html_nodes(".time-span") %>%
  html_text()

# make a dataset containing the the scraped information and bring it together to the global dataframe, df.

data <- data.frame(cbind(n_date, n_trans, n_city, n_views, n_department, n_paid, n_title, n_timeago))

df <- rbind(df,data)

# command to slow down the scrapng process.

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

#  remove ,'s in n_paid (2 times)
dm$n_paid <- gsub( x = dm$n_paid, pattern = "?\\,[0-9]+\\,", replacement = NA )
dm$n_paid <- gsub( x = dm$n_paid, pattern = "\\,", replacement = "")
dm$n_paid <- as.numeric(dm$n_paid)


# export data as .csv:
#setwd("/Users/dennishansen/Dropbox/Assignment-2")
#        write.table(x = dm, file = "assign2.csv", 
#                    fileEncoding = "UTF-8", sep = ";", row.names = FALSE, dec = ",")
   



# **************** (3) data analysis *********************** #

# make an aggregate dataframe for birth certificate:

agg <- dm %>%
  filter(n_paid != "NA", n_district != " ", n_trans == "Birth Certificate") %>%
  group_by(n_district) %>%
  summarise( count = n(), mean_paid = mean(n_paid)) %>%
  arrange(-count)

# simple barplot showing the distribution of birth certificate reports by district:

p <- ggplot(agg, aes( x = reorder(n_district, -count), y = count))
p <- p + geom_bar(stat = "identity", fill = "darkred") + coord_flip() 
p <- p + labs( x = "Name of state", y = "# of reports", title = "Birth Certificate")
p


  
        


