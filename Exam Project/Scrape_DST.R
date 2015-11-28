library("plyr")
library("rvest")
library("dplyr")
library("stringr")
library("XML")
library("httr")


url <- "https://www.dst.dk/valg/Valg1487635/kandstat/kandstat.htm"
tabs <- GET(url, encoding = "UTF-8")
tabs <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F, header = TRUE)

tabs <- tabs[2]
dst <- data.frame(matrix(unlist(tabs), nrow=962, ncol=7), stringsAsFactors = F)
dst <- dst[(4:962),]
names(dst) <- c("Navn", "Opstillet i kreds nr.", "Nomineret i kreds nr.", "Stemmer_Ialt", "Stemmer_pers", "Valgt_nr", "Stedfor_nr")

dst <- dst %>%
  filter(!is.na(dst$'Opstillet i kreds nr.'))

dst[636:724,]$`Nomineret i kreds nr.`<- str_extract(dst[636:724,]$Navn, "[0-9]{1,3}. ")

dst$`Nomineret i kreds nr.`<-gsub("\\.","",dst$`Nomineret i kreds nr.`)
dst$Stemmer_pers<-gsub("\\.","", dst$Stemmer_pers)
dst$Stemmer_Ialt<-gsub("\\.","", dst$Stemmer_Ialt)
dst$Valgt_nr<-gsub(" ","", dst$Valgt_nr)
dst$Navn<-gsub("[0-9]{1,3}. ","",dst$Navn)


dst$`Nomineret i kreds nr.`<-as.factor(as.character(dst$`Nomineret i kreds nr.`))
dst$Stemmer_Ialt<-as.numeric(as.character(dst$Stemmer_Ialt))
dst$Stemmer_pers<-as.numeric(as.character(dst$Stemmer_pers))
dst$Valgt_nr<-as.factor(as.character(dst$Valgt_nr))
dst$Stedfor_nr<-as.factor(as.character(dst$Stedfor_nr))

df <- left_join(data, dst, by=c("name"="Navn"))
df <- df[-708,]
df <- df[-616,]

df2 <- left_join(data, dst, by=c("votes"="Stemmer_pers"))
df2 <- df2 %>% filter (!duplicated(df2$name))

