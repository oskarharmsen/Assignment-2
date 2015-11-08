# settings

Sys.setlocale(category = "LC_ALL", locale = "UTF-8")

options(scipen=999)

# loading packages: 
library("plyr")
library("rvest")
library("ggplot2")
library("lubridate")
library("stringr")
library("XML")
library("httr")
library("readr")
library("tidyr")
library("rgeos")
library("maptools")
library("sp")
library("gpclib")
library("viridis")
library("ggthemes")
library("dplyr")

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

#Update names
names(df) <- c("date", "type", "town", "state", "views", "department", "currency", "paid", "title")

 
# ALTERNATIVE: Pick up latest version of dataset from GitHub
df <- read.csv(file = "https://raw.githubusercontent.com/oskarharmsen/Assignment-2/master/assign2.csv", sep = ";")

names(df) <- c("date", "type", "town", "state", "views", "department", "currency", "paid", "title")

# **************** (3) data analysis *********************** #

#Correct classes of columns
  df$town <- str_trim(df$town)
  df$title <- str_trim(df$title)
  df$date <- as.Date(df$date)
  df$views <- as.numeric(df$views)
  df$paid <- as.numeric(df$paid)
  df$state <- str_trim(df$state)

#Remove NAs and Currency
  df <- df %>% 
    select(-c(currency, title)) %>% #Remove irrelevant columns
    filter(!is.na(paid)) #Remove observations with NA in paid
  
  df <- df %>% filter(!is.na(state) & state!="") #Remove observations with NA or missing in state

### Basic tables
  
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
  
  # Download wikipedia table, mergeable with own dataset
  
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
  tabs$state <- gsub(x = tabs$state, pattern = "Odisha", replacement = "Orissa")
  tabs$literacy <- as.numeric(as.character(tabs$literacy))
  tabs$population_density <- as.numeric(as.character(tabs$population_density))
  tabs$urban_population_share <- as.numeric(as.character(tabs$urban_population_share))
  
  tabs <- tabs %>% select(state, population, literacy, population_density, urban_population_share)

  df <- left_join(df, tabs, by = "state")

# (i) distrubution plots

# make an aggregate dataframe for birth certificate:

agg <- df %>%
  filter(paid != "NA", state != " ", type == "Birth Certificate") %>%
  group_by(state) %>%
  summarise( count = n(), meapaid = mean(paid)) %>%
  arrange(-count)

# simple barplot showing the distribution of birth certificate reports by district:

p <- ggplot(agg, aes( x = reorder(state, -count), y = count))
p <- p + geom_bar(stat = "identity", fill = "darkred") + coord_flip() 
p <- p + labs( x = "Name of state", y = "# of reports", title = "Birth Certificate")
p

# (ii) mapping 

#load spatial data and fortify to dataframe#
download.file("https://raw.githubusercontent.com/oskarharmsen/Assignment-2/master/IND_adm1.rds", "India")
india <- readRDS("India")
india <- fortify(india, region="NAME_1")

#check if names of states matches in the two datasets#
namesInData <- levels(factor(df$state))
namesInMap <- levels(factor(india$id))
namesInData[which(!namesInData %in% namesInMap)]

#correct errors and spelling#
df$state <- gsub("Hardoi", "Uttar Pradesh", df$state)
df$state <- gsub("Uttarakhand", "Uttaranchal", df$state)

#new dataframes to use for plotting#
df1 <- df %>% 
  group_by(state) %>% 
  summarise(count = n(), aggpaid=sum(paid))

df2 <- df %>%
  group_by(state)%>%
  summarise(count=n())

df3 <- df %>%
  group_by(state)%>%
  summarise(lit=median(literacy))
df3$lit[25] <- 69.72

df4 <- df %>%
  group_by(state)%>%
  summarise(urban=median(urban_population_share))
df4$urban[25] <- 20.8

#plotting the spatial data - CAUTION this takes a while! Approx. 10 minutes#
p1 <- ggplot() + 
  geom_map(data=df1, aes(map_id = state, fill=aggpaid), 
           map = india) + 
  expand_limits(x = india$long, y = india$lat)+ 
  coord_equal()+
  ggtitle("Paid: Reported bribes in India")+
  theme_tufte(ticks = FALSE)+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  scale_fill_viridis(trans="log10", na.value ="grey50",
                     name="Amount paid \n(logs)")

p2 <- ggplot() + 
  geom_map(data=df2, aes(map_id = state, fill=count), 
           map = india) + 
  expand_limits(x = india$long, y = india$lat)+ 
  coord_equal()+
  ggtitle("Count: Reported bribes in India")+
  theme_tufte(ticks = FALSE)+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  scale_fill_viridis(na.value ="grey50",
                     name="Count")
p3 <- ggplot() + 
  geom_map(data=df3, aes(map_id = state, fill=lit), 
           map = india) + 
  expand_limits(x = india$long, y = india$lat)+ 
  coord_equal()+
  ggtitle("Literacy rates in India")+
  theme_tufte(ticks = FALSE)+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  scale_fill_viridis(na.value ="grey50",
                     name="Literacy")

p4 <- ggplot() + 
  geom_map(data=df4, aes(map_id = state, fill=urban), 
           map = india) + 
  expand_limits(x = india$long, y = india$lat)+ 
  coord_equal()+
  ggtitle("% of total population that is urban")+
  theme_tufte(ticks = FALSE)+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  scale_fill_viridis(na.value ="grey50",
                     name="Urban pop.")

grid.arrange(p1, p2, p3, p4, ncol=2)


        #Create summary dataframe on state level
        state.table <- df %>%
          group_by(state) %>% 
          summarise(
            count = n(),
            mean_bribe = mean(paid),
            median_bribe = median(paid), 
            urban_population_share = mean(urban_population_share),
            literacy_percent = mean(literacy),
            population_density_per_sqkm = mean(population_density)
            ) %>%   
          arrange(desc(count))
        
        #Plot
        plot.literacy <-  ggplot(data = state.table, aes(x = literacy_percent, y = mean_bribe)) +
              stat_smooth(method = "lm")+
              geom_point() + 
              theme_minimal()+
              ylab("")
        
        plot.urban <-  ggplot(data = state.table, aes(x = urban_population_share, y = mean_bribe)) +
              stat_smooth(method = "lm")+
              geom_point() + 
              theme_minimal()
        
        plot.density <-  ggplot(data = state.table, aes(x = population_density_per_sqkm, y = mean_bribe)) +
              stat_smooth(method = "lm")+
              geom_point() + 
              theme_minimal()+
              ylab("")
        
        library(gridExtra)
        grid.arrange(plot.urban, plot.density, plot.literacy, ncol = 3)
        
        #Checking the summary with literacy
        fit1 <- lm(mean_bribe ~ literacy_percent, data = state.table)
        summary(fit1)
        
        
        
    #Tabeller med top for hver af staterne
        top.table <- df %>% 
          group_by(state) %>% 
          mutate(
            count.state = n()
          ) %>% 
          ungroup() %>% 
          
          group_by(state, type) %>% 
          summarise(
            mean_bribe = mean(paid),
            count = n(), 
            count.state = mean(count.state),
            share = count / count.state
          ) %>% 
          
          top_n( n = 3, wt = share) %>% 
          ungroup() %>% 
          arrange(desc(count.state), desc(share)) %>% 
        
          filter(count.state > 33)
            
      top.table <- top.table[-7,]
      top.table$state <- as.factor(top.table$state)
      
      
      # FACET WRAP
           
      plot <- ggplot( data = top.table)+
              facet_wrap( ~ state, ncol = 3, scales = "free") +
              geom_bar(stat = "identity", aes(x = reorder(type, -share), y = share)) +
              # coord_flip() + 
              xlab("")+
              ylab("")+
              ylim(0, 0.95)+
              ggtitle("Top 3 bribe types in the top 6 states \n by number of reports, percent")
      plot
        
        




