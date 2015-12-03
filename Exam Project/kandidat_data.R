# settings
Sys.setlocale(category = "LC_ALL", locale = "UTF-8")

# loading packages
library(readr)
library(dplyr)
library(XML)
library(rvest)
library(stringr)

# First of all, we want to scrape every candidate name from http://www.folketingsvalg-2015.dk
# To do that, we have to loop through every pari name and then create links to scrape from:

partier <- data.frame(c("alternativet","venstre","socialdemokraterne","dansk folkeparti","radikale venstre",
                        "socialistisk folkeparti", "enhedslisten", "liberal alliance", 
                        "det konservative folkeparti", "kristendemokraterne", "uden for folketingsgrupperne")) 
names(partier) = c("parti")

    # Creating links to http://www.folketingsvalg-2015.dk for the parties:
partier = partier %>%
  mutate(link = str_replace_all(parti, "[[:space:]]", "-")) %>%
  mutate(link = paste0("http://www.folketingsvalg-2015.dk/parti/",link))

    # Now loop through the links just created and scrape candidate names:

kandidater.ft15 <- data.frame()

for( i in 1:nrow(partier)){
  
  ft   <- partier[i,2]
  link <- html(ft)
  
  kandidat <- link %>%
    html_nodes(".simplelist p a") %>%
    html_text() %>%
    data.frame() %>%
    mutate(parti = partier[i,1])
  
  kandidater.ft15 <- rbind(kandidater.ft15, kandidat)
  
  Sys.sleep(0.5)
}  

names(kandidater.ft15) = c("navn","parti")

# Next, we have to generate a link from the names to further scrape information about
# the individual candidates.

  # 1. making the links:

kandidater.ft15 <- kandidater.ft15 %>%
  mutate(link = str_replace_all(navn,"[[:space:]]","-")) %>%
  mutate(link = str_replace_all(link, "\\.","")) %>%
  mutate(link = paste0("http://www.folketingsvalg-2015.dk/",link)) %>%
  mutate(link = str_replace_all(link, "ø","o")) %>%
  mutate(link = str_replace_all(link, "æ","ae")) %>%
  mutate(link = str_replace_all(link, "Ø","o")) 
  

  # 2. scrape individual candidate data:

kandidat.data <- data.frame()

for(i in 1:nrow(kandidater.ft15)){

ft   <- kandidater.ft15[i,3]
link <- html(ft)

storkreds = link  %>%
  html_nodes(".center p:nth-child(1) a") %>%
  html_text()

lokalkreds = link %>%
  html_nodes(".center p:nth-child(2)")   %>%
  html_text()

alder = link %>% 
  html_nodes(".center p:nth-child(3)")   %>%
  html_text()

titel = link %>% 
  html_nodes(".center p:nth-child(4)")   %>%
  html_text()

bopæl = link %>% 
  html_nodes(".center p:nth-child(5)")   %>%
  html_text()

køn = link   %>% 
  html_nodes(".center p:nth-child(6)")   %>%
  html_text()

opstillet_sidste_valg = link %>% 
  html_nodes(".center p:nth-child(7)")   %>%
  html_text()

buffer = link %>%
  html_nodes("p:nth-child(8)")           %>%
  html_text

navn  <- as.character(kandidater.ft15[i,1])
parti <- as.character(kandidater.ft15[i,2])

k <- data.frame(cbind(navn, parti,storkreds, lokalkreds, alder, titel, bopæl, køn, opstillet_sidste_valg, buffer))

kandidat.data <- data.frame(rbind(kandidat.data, k))

}

 # 3. nogle gange er der forskydninger i det scrapede data, da enkelte kandidater har angivet placeringen
 # på valglisten (eks. række 153). Disse rækker identificeres og rettes:

kandidat.data   = kandidat.data %>%
  mutate(indi   = ifelse(grepl("Opstillet" , buffer) == TRUE , 1, 0))           %>%
  mutate(alder  = ifelse(indi == 1,  as.character(titel), as.character(alder))) %>%
  mutate(titel  = ifelse(indi == 1,  as.character(bopæl), as.character(titel))) %>%
  mutate(bopæl  = ifelse(indi == 1,  as.character(køn), as.character(bopæl)))   %>%
  mutate(køn    = ifelse(indi == 1,  as.character(opstillet_sidste_valg), as.character(køn)))   %>%
  mutate(opstillet_sidste_valg = ifelse(indi == 1,  as.character(buffer), as.character(opstillet_sidste_valg))) %>%
  select(-(buffer:indi))
  


  # 4. laver en generel datacleaning på sættet:

kandidat.data <- kandidat.data %>%
  mutate(storkreds             = str_replace_all(storkreds,"storkreds",""))     %>%
  mutate(lokalkreds            = str_replace_all(lokalkreds,"Lokalkredse:","")) %>%
  mutate(alder                 = str_replace_all(alder,"Alder:",""))            %>%
  mutate(titel                 = str_replace_all(titel,"Titel:",""))            %>%
  mutate(bopæl                 = str_replace_all(bopæl,"Bopæl:",""))            %>%
  mutate(køn                   = str_replace_all(køn,"Køn:",""))                %>%
  mutate(navn                  = str_replace_all(navn,"aa","å"))                %>%
  mutate(opstillet_sidste_valg = str_replace_all(opstillet_sidste_valg,"Opstillet ved sidste valg:","")) 
  
  # 5. sammenkobling med det orginale data

 raw <- read_csv("/users/dennishansen/dk_ft15_politician_responses.csv")
 
  # .. først rettes partinavn:      
raw = raw %>%
  mutate(party = ifelse(party == "aa", "alternativet", as.character(party)))                 %>%
  mutate(party = ifelse(party == "v" , "venstre", as.character(party)))                      %>%
  mutate(party = ifelse(party == "a" , "socialdemokraterne", as.character(party)))           %>%
  mutate(party = ifelse(party == "o" , "dansk folkeparti", as.character(party)))             %>%
  mutate(party = ifelse(party == "b" , "radikale venstre", as.character(party)))             %>%
  mutate(party = ifelse(party == "f" , "socialistisk folkeparti", as.character(party)))      %>%
  mutate(party = ifelse(party == "oe", "enhedslisten", as.character(party)))                 %>%
  mutate(party = ifelse(party == "i" , "liberal alliance", as.character(party)))             %>%
  mutate(party = ifelse(party == "c" , "det konservative folkeparti", as.character(party)))  %>%
  mutate(party = ifelse(party == "k" , "kristendemokraterne", as.character(party)))          %>%
  mutate(party = ifelse(party ==  1  , "uden for folketingsgrupperne", as.character(party))) %>%
  mutate(name = str_replace_all(name, "aa","å"))

  # .. der laves manuelle rettelser af navne. Disse er identificeret i loppet nedenfor, og kan 
  # ikke matches, da de alle var stavet forkert i det oprindelige data for kandidaterne. 

  kandidat.data$navn[361] = "Søren Burcharth"
  kandidat.data$navn[629] = "Anne-Marie Tørnes Hansen"
  kandidat.data$navn[378] = "Henriette Bødevadt"
  kandidat.data$navn[38]  = "Tanja Schjellerup"
  kandidat.data$navn[663] = "Anette Bolvig"
  kandidat.data$navn[363] = "Giajenthiran Velmurugan"
  
raw = rename(raw, navn = name)

# .. data for valgtesten samt kandidater merges (dem som altså har sammenlignelige navne):

merge_raw  = left_join(raw,kandidat.data, by = "navn") 

# .. der laves et kandidat ID i de to datasæt, således de i den senere kode kan identificeres:

id.k = seq(1,nrow(kandidat.data))
kandidat.data = mutate(kandidat.data, id.k = id.k)

id.r = seq(1,nrow(merge_raw))
merge_raw = mutate(merge_raw, id.r = id.r)

# .. da navnene i valgtesten og kandidatdatasættet ikke altid er ens, grundet brug af mellemnavne 
# mv., er det altså ikke alle som er merged ordenligt. Dertil identifiseres disse i første omgang:

skalrettes = filter(merge_raw, is.na(parti) == TRUE) 

# .. følgende kodestump gennemgår altså disse navne fra "skalrettes" og ser om de kan kobles sammen
# med navne fra kandidatdatasættet ved at kigge på om fornavnet og efternavnet passer, samt partiet 
# er det samme, ELLER, fornavn og mellemnavn det samme samt samme parti. Er dette gældende, indsættes
# data fra kandidat-datasættet i de manglende felter i det mergede data (merge_raw) ved hjælp af 
# kandidat ID'et skabt tidligere. 

fejl = c()
for(i in 1:nrow(skalrettes)){

    # først laves der et filter på det data som skal gennemsøges, således det kun er de personer
    # med det relevante parti:
    parti.filter = skalrettes$party[i]  
    data.filter = filter(kandidat.data, parti == parti.filter ) 
    
    # herefter noteres kandidatens navn og ID:
    opdel = skalrettes$navn[i]
    id.r  = skalrettes$id.r[i]

    # nu loopes der igennem for at se på de omtalte kriterier, og sende data til merge_raw hvis 
    # koden finder et match. Hvis ikke vil rækkenummeret blive identifiseret og smidt i vektoren
    # fejl. Denne skal altså gerne være tom, hvilket den slutteligt også er. 
    
    k = "ikke fundet"
    for(j in 1:nrow(data.filter)){

      tjek = str_split(data.filter$navn[j],"[[:space:]]") 
      tjek = c(do.call("cbind",tjek))
      n = length(tjek)
      
      if(grepl(tjek[1], opdel) == T & grepl(tjek[n], opdel) == T | grepl(tjek[1], opdel) == T & grepl(tjek[2], opdel) ){
         k = "fundet"
         merge_raw$parti[id.r] = data.filter$parti[j]
         merge_raw$storkreds[id.r] = data.filter$storkreds[j]  
         merge_raw$lokalkreds[id.r] = data.filter$lokal[j] 
         merge_raw$alder[id.r] = data.filter$alder[j]
         merge_raw$køn[id.r] = data.filter$køn[j] 
         merge_raw$bopæl[id.r] = data.filter$bopæl[j] 
         merge_raw$titel[id.r] = data.filter$titel[j] 
         merge_raw$id.k[id.r] = data.filter$id.k[j]
         merge_raw$opstillet_sidste_valg[id.r] = data.filter$opstillet_sidste_valg[j]
        # merge_raw$opstillet_sidste_valg[id.r] = data.filter$opstillet_sidste_valg[j] 
         
         } else if (j == nrow(data.filter) & k != "fundet") {
             fejl = append(fejl, i )
       }
   }
} 

# .. der er to dubletter som ikke kan identifisers med kode, men rettes manuelt:
merge_final = merge_raw[-611,]
merge_final = merge_final[-702,]

# .. variable som optræder flere gange slettes og der navngives:

merge_final = select(merge_final, -age, -party, -id.r, -id.k) ## OSKAR: Lad v�re med at fjerne age og party her. Age er bedre, og party fucker data analysis kodernede :)

names(merge_final) = c("Navn", "Stemmer","Uddannelse","Forebyggelse","Sundhed","Velfærd","Arbejdsmarked_1",
               "Arbejdsmarked_2", "Økonomi", "Trafik", "Ret", "Social", "Integration", "EU", 
               "Udvikling", "Miljø", "Kultur", "Parti", "Storkreds", "Lokalkreds", "Alder", "Titel", 
               "Bopæl", "Køn", "Valgt_ved_sidste_valg" ) 



##Scraper DST-data##

url <- "https://www.dst.dk/valg/Valg1487635/kandstat/kandstat.htm"
tabs <- GET(url, encoding = "UTF-8")
tabs <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F, header = TRUE)

tabs <- tabs[4]
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

merge_final <- read.csv(file="ft15_final.csv", header = T)

##Merger med merge_final
df <- left_join(merge_final, dst, by="Navn")
df1 <- df %>% 
  filter(is.na(df$Stemmer_pers))
df1 <- df1[,1:25]

df2 <- left_join(df1, dst, by=c("Stemmer"="Stemmer_pers"))
df2 <- df2 %>% filter (!duplicated(df2$Navn.x))

df <- df %>% filter (!is.na(df$Stemmer_pers))

df <- df[, -29]
df2 <- df2[, -26]
names(df2)[names(df2)=="Navn.x"] <- "Navn"

final <- rbind(df, df2)

final <- final[-590,]
final <- final[-516,]

final <- tolower(final)
names(final) <- tolower(names(final))

# .. afslutningsvist eksporteres det nye datasæt til en .csv fil kaldet ft15_final:

write.csv(final, "final.csv")


