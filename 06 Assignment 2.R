library("rvest")
library("ggplot2")
library("lubridate")
library("dplyr")
library("stringr")
library("plyr")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#     The method will proceed in three steps. 
#     Step 1: Scrape 1000 unique links to reported bribes
#     Step 2: Scrape data from each bribe observation
#     Step 3: Clean Data
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

##### Step 1:  Scrape 1000 unique links to reported bribes

  # Function to get links to observations when parsed an overview page link
    get.links = function(link){
        bribe <- read_html(link) %>% 
                html_nodes(css = ".heading-3 a") %>% 
                html_attr(name = 'href')
        return(bribe)
    }
    

    
  
  # Loop link-obtaining function for enough sites
  
    baselink <- "http://www.ipaidabribe.com/reports/paid?page="
    bribe.links <- 0
    n.links <- 1000
    
    for (i in 1:(n.links/10)){
      link <- paste( baselink, (i-1)*10, sep = "") # Link to overview page
      links.latest <- get.links(link = link)       # Scrape observation links from overview page
      bribe.links <- c(bribe.links, links.latest)  # Append list of links with newest obtained
      
      print( paste("Completed ", i, "of", (n.links/10), sep = " ")) # Track and report progress
    }
    
    bribe.links <- bribe.links[2:length(bribe.links)] #Remove the first '0'

    #Gem downloadet data    
      #     setwd("/Users/oskarh/Documents/Assignment 2/Assignment-2")
      #     write.table(x = bribe.links, file = "Links to bribe observations.csv", 
      #                 fileEncoding = "UTF-8", sep = ";", row.names = FALSE, dec = ",")
  
  
###### Step 2: Scrape data from each bribe observation

  #Function to get output from each bribe-link
  get.bribe = function (link) { #Pun intended
    temp.data <- read_html(link)
    
    title <-  temp.data %>% 
              html_nodes(css = ".heading-3 a") %>% 
              html_text() %>% 
              str_trim()
    
    amount <- temp.data %>% 
              html_nodes(css = ".details .paid-amount span") %>% 
              html_text() %>% 
              str_trim() %>% 
              gsub(pattern = "Paid ", x = ., replacement = "")
  
    department <- temp.data %>% 
              html_nodes(css = ".details .name a") %>% 
              html_text() %>% 
              str_trim()
    
    transaction <- temp.data %>% 
              html_nodes(css = ".details .transaction a") %>% 
              html_text() %>% 
              str_trim()
    
    views <- temp.data %>% 
              html_nodes(css = ".overview .views") %>% 
              html_text() %>% 
              gsub(pattern = " views", x = ., replacement = "") %>% 
              as.numeric()
    
    city <- temp.data %>% 
              html_nodes(css = ".location") %>% 
              html_text() %>% 
              gsub( pattern = "\r\n                      ", x = ., replacement = "")
    
    date <- temp.data %>% 
              html_nodes(css = ".date") %>% 
              html_text() %>% 
              strptime(format = "%B %d, %Y") %>% 
              as.character()
    
    
    return( c(title, amount, department, transaction, views, city, date))
    
  } #Outputs 

#Fill list with data 
    df <- list() #Create empty list
    
    for (i in 1:length(bribe.links)){ #Fill list
      df[[i]] <- get.bribe(bribe.links[i])
      print( paste( "Downloaded", i, "out of", length(bribe.links), "observations."))
    }

##### Step 3: Clean data
    
    #Convert to dataframe format
    df1 <- ldply(df)
    
    #Rename
    names(df1) <- c("title", "amount", "department", "topic", "city", "views",  "date")
    
    #Convert Class
    df1$date <- as.Date(df1$date)
    
#     #Gem downloadet data    
#         setwd("/Users/oskarh/Documents/Assignment 2/Assignment-2")
#         write.table(x = df1, file = "Bribes.csv", 
#                     fileEncoding = "UTF-8", sep = ";", row.names = FALSE, dec = ",")
    
    
