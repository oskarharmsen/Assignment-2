#### Do a short version ####

setwd("/Users/oskarh/Documents/Assignment 2/Group 8/Exam Project")


data1 <- read.csv(file = "dk_ft15_politician_responses.csv", header = TRUE) #Load the raw dataset
data2 <- read.csv(file = "ft15_final.csv", header = TRUE) #Load the raw dataset

names(data2)[4:18] <- names(data1)[5:19]

data2 <-  data2 %>% 
          select( -X) %>% 
          arrange(Alder) %>%
          rename(pers_stemmer = Stemmer)
          
data.head <- data2[1:10,]

write.table(x = data.head, file = "head_dk_ft15_politician_responses.csv", fileEncoding = "UTF-8", sep = ",", row.names = FALSE)