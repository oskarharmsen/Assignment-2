Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
library("plyr")
library("rvest")
library("dplyr")
library("ggplot2")



## Obtain list of links to particular candidates

## Step 1: Obtain links to valgsteder ##

valgsteder.page <- "http://www.dr.dk/nyheder/politik/valg2015/se-din-stemmeseddel#skift-valgsted"

valgsteder.link = read_html(valgsteder.page, encoding = "UTF-8") %>% 
  html_nodes(css = ".heading-xxsmall")  %>%
  html_attr(name = 'href')
valgsteder.link <- as.data.frame(valgsteder.link)
valgsteder.link$valgsteder.link <- paste("http://www.dr.dk", valgsteder.link$valgsteder.link, sep="")

## Step 2: Obtain links to candidates at each valgsted
  #Step 2.1: Create function
    valgsted.kand = function(valgsted) {
      data <- read_html(valgsted, encoding = "UTF-8")
      kand.link <-  data %>% 
                    html_nodes(css = ".collaps-items .heading-xsmall") %>% 
                    html_attr(name = 'href')
    return(kand.link)
    }
    
  #Step 2.2: Apply function for each valgsted
    
    #Get raw list
    kand.link <- list()
    for (i in 1:nrow(valgsteder.link)) {
      kand.link[[i]] <- valgsted.kand(valgsteder.link[i,]) 
      print(paste(i, "of", nrow(valgsteder.link)))
    }
    
    #clean duplicate entries
    kand.link <- unique(unlist(kand.link))
    kand.link <- paste("http://www.dr.dk", kand.link, sep = "")
    
#Step 3: Get info on each candidate
  
  #Build function to extract information
  kand.info = function(link){
    #Read link
    data2 <- read_html(link, encoding = "UTF-8")
    
    #Fetch name
    name <- data2 %>%
      html_nodes(".heading-large") %>% 
      html_text()
      
    #Fetch Party
    party <- data2 %>%
      html_nodes(".large") %>% 
      html_attrs() %>% 
      gsub(x = ., pattern = "letter-color party-", replacement = "") %>% 
      gsub(x = ., pattern = "large", replacement = "") %>% 
      str_trim()
    
    #Fetch Alder
    age <- data2 %>%
      html_nodes(".fact-item:nth-child(1) .fact-item-text") %>% 
      html_text() %>%
      gsub(x = ., pattern = "år", replacement = "") %>% 
      str_trim() %>% 
      as.numeric()

    #Fetch votes
    votes <- data2 %>%
      html_nodes(".votes > .pv-votes-count-candidate-page") %>% 
      html_text() %>%
      as.numeric()
    
    #Fetch responses
    responses <- data2 %>%
      html_nodes(".heading-xxxsmall") %>% 
      html_text() %>%
      unlist() %>% 
      rbind()
    
    #Collect output
      return(cbind(name, age, party, votes, responses))
    
  }
      
  
  
  #Loop over candidates
  loop.length <- length(kand.link)
  df = list()
  for (i in 1:loop.length){
    df[[i]] <- kand.info(kand.link[i])
    print(paste(i, "of", loop.length))
  }

  
#Step 4: Clean
  #Remove candidates without responses
  df2 = list()
  for (i in 1:loop.length) {
    if(length(df[[i]]==33)){
      df2[[i]] <- df[[i]]
      }else{
    }
  }
  
  
  #Order into dataframe
  df2 <- as.data.frame(matrix(df))
  df2[,1:34] <- str_trim(str_split_fixed(df2[,1], ",", 34)) #Split several options into different rows
  
  #Clean
  for(i in 1:34){
    df2[,i] <- gsub(x = df2[,i], pattern = "\"", replacement = "")  
  }        
  df2[,1] <- gsub(x = df2[,1], pattern = "+[c]\\(", replacement = "")  
  df2 <-  df2 %>% 
          filter(V5!="") %>%  #Remove observations without responses
          select(-(0:14*2+6))
  df2$age <- as.numeric(df2$age)
  df2$votes <- as.numeric(df2$votes)
  for(i in c(3, 5:19)){
    df2[,i] <- as.factor(df2[,i])
  }
  df2 <- filter(df2, df2[,19]!="")
  
  
  #Get questions downloaded
  data <- read_html(kand.link[2], encoding = "UFT-8")
  data2 <- data %>% html_nodes(".quest") %>% 
           html_text() %>% 
           str_trim() %>% 
           gsub(x = ., pattern = "       ", replacement = "") %>% 
           gsub(x = ., pattern = "[\r\n]", replacement = "_")
  names(df2)=c("name", "age", "party", "votes", data2)
  
  #Rename
  data <- df2
  
  write.table(x = data, file = "dk_ft15_politician_responses.csv", fileEncoding = "UTF-8", sep = ",", row.names = FALSE)