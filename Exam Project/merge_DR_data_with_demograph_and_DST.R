# settings
Sys.setlocale(category = "LC_ALL", locale = "UTF-8")

#Set working directory
setwd(dir = "/Users/oskarh/Documents/Assignment 2/Group 8/Exam Project")  #  <-- IMPORTANT: insert link to GitHub synced path on your computer here.

# loading packages
library(readr)
library(dplyr)
library(XML)
library(rvest)
library(stringr)
library(httr)

##### Part 1: Scrape data form folketingsvalg-2015.dk ####### ---------------------

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
  
rm(partier, kandidat) #remove unused variables


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

print(paste(i, "of", nrow(kandidater.ft15), "completed.", sep = " ")) #Keep track of progress

}

  # 3. for some of the rows the variables os not in the correct column due to asymmetry where some candidates 
  # has a variable containing the position on the actual election list (for example row # 152). Those rows 
  # has to be identified and correctet:


kandidat.data   = kandidat.data %>%
  mutate(indi   = ifelse(grepl("Opstillet" , buffer) == TRUE , 1, 0))           %>%
  mutate(alder  = ifelse(indi == 1,  as.character(titel), as.character(alder))) %>%
  mutate(titel  = ifelse(indi == 1,  as.character(bopæl), as.character(titel))) %>%
  mutate(bopæl  = ifelse(indi == 1,  as.character(køn), as.character(bopæl)))   %>%
  mutate(køn    = ifelse(indi == 1,  as.character(opstillet_sidste_valg), as.character(køn)))   %>%
  mutate(opstillet_sidste_valg = ifelse(indi == 1,  as.character(buffer), as.character(opstillet_sidste_valg))) %>%
  select(-(buffer:indi))
  


  # 4. do a generel data cleaning of the data:

kandidat.data <- kandidat.data %>%
  mutate(storkreds             = str_replace_all(storkreds,"storkreds",""))     %>%
  mutate(lokalkreds            = str_replace_all(lokalkreds,"Lokalkredse:","")) %>%
  mutate(alder                 = str_replace_all(alder,"Alder:",""))            %>%
  mutate(titel                 = str_replace_all(titel,"Titel:",""))            %>%
  mutate(bopæl                 = str_replace_all(bopæl,"Bopæl:",""))            %>%
  mutate(køn                   = str_replace_all(køn,"Køn:",""))                %>%
  mutate(navn                  = str_replace_all(navn,"aa","å"))                %>%
  mutate(opstillet_sidste_valg = str_replace_all(opstillet_sidste_valg,"Opstillet ved sidste valg:","")) 
  
  # 5. merging with the original data:

 raw <- read_csv("dk_ft15_politician_responses_DR_DATA_ONLY.csv")
 
  # .. first, the names of the party should be full lenght instead of letter (for merging comparison reasons)    
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

  # .. manual corrections of some of the names. Those ones got identified in the next loop and could not 
  # be matched due to incorrect spelling in most cases. 

  kandidat.data$navn[361] = "Søren Burcharth"
  kandidat.data$navn[629] = "Anne-Marie Tørnes Hansen"
  kandidat.data$navn[378] = "Henriette Bødevadt"
  kandidat.data$navn[38]  = "Tanja Schjellerup"
  kandidat.data$navn[663] = "Anette Bolvig"
  kandidat.data$navn[363] = "Giajenthiran Velmurugan"
  
  #raw <- rename(raw, navn = name)
  names(raw)[1] <- "navn"

# .. merging of the VAA and candidate data (those observation that can compared in terms of name): 

merge_raw  = left_join(raw,kandidat.data, by = "navn") 

# .. introducing a new variable called ID for both datasets. This should establish i way to identify 
# the candidates in the later code.


id.k = seq(1,nrow(kandidat.data))
kandidat.data = mutate(kandidat.data, id.k = id.k)

id.r = seq(1,nrow(merge_raw))
merge_raw = mutate(merge_raw, id.r = id.r)

# .. because the names in the VAA and candidate data is not always the same (even though it's the same candidate,
# but different use of middle name ect.) not every rows is merged correctly. Fist, we identify those:

skalrettes = filter(merge_raw, is.na(parti) == TRUE) 

# .. the following code chunk will go through "skalrettes" just created and try to connect these names with the ones
# in the candidate data. The code will identify it as a match IF the first name, sir name and pary is the same in the two 
# dataset OR IF the firstname, middle name and party is the same. If there is a match, the variables from the candidate 
# data will be copied into the missing values in the merged data for the matched candidate (here we use the ID created before)

fejl = c()
for(i in 1:nrow(skalrettes)){
    
    # first, a filter is made on the data to be searched by the relevant party. In that way, we 
    # limit the size of the loop each time.

    parti.filter = skalrettes$party[i]  
    data.filter = filter(kandidat.data, parti == parti.filter ) 
    
    # thereafter we note the ID's
    opdel = skalrettes$navn[i]
    id.r  = skalrettes$id.r[i]
    
    # now we loop through the narrowed seach data and look for the criterias described above, 
    # and copy in the observation in merge_raw for the matched candidates. If the code can't find 
    # a match, it will output the rownumber to a vector. By looking at the vector we could identidy
    # the candidates that could not be matched due to spelling errors ect. (which is correctet above).
    
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

# .. two observations are accouring more than once, and since they can't be corrected be the code, we have the exclude them manually.  
merge_final = merge_raw[-611,]
merge_final = merge_final[-702,]

# .. selection of variables:

merge_final = select(merge_final, -id.r, -id.k, -alder, -buffer)

merge_final = merge_final %>% 
              select(
                name = navn,
                party = party,
                storkreds = storkreds,
                lokalkreds = lokalkreds,
                ran.last.election = opstillet_sidste_valg,
                age = age,
                sex = køn,
                title = titel,
                location = bopæl,
                pers.votes = votes, 
                `UDDANNELSE__Efter folkeskolereformen får eleverne for lange skoledage` = `UDDANNELSE__Efter folkeskolereformen får eleverne for lange skoledage`,
                `FOREBYGGELSE__Afgiften på cigaretter skal sættes op` = `FOREBYGGELSE__Afgiften på cigaretter skal sættes op`,
                `SUNDHED__Et besøg hos den praktiserende læge skal koste eksempelvis 100 kr.` = `SUNDHED__Et besøg hos den praktiserende læge skal koste eksempelvis 100 kr.`,
                `VELFÆRD__Mere af ældreplejen skal udliciteres til private virksomheder` = `VELFÆRD__Mere af ældreplejen skal udliciteres til private virksomheder`,
                `ARBEJDSMARKED__Man skal hurtigere kunne genoptjene retten til dagpenge` = `ARBEJDSMARKED__Man skal hurtigere kunne genoptjene retten til dagpenge`,
                `ARBEJDSMARKED__Virksomheder skal kunne drages til ansvar for, om deres udenlandske underleverandører i Danmark overholder danske regler om løn, moms og skat` = `ARBEJDSMARKED__Virksomheder skal kunne drages til ansvar for, om deres udenlandske underleverandører i Danmark overholder danske regler om løn, moms og skat`,
                `ØKONOMI__Vækst i den offentlige sektor er vigtigere end skattelettelser` = `ØKONOMI__Vækst i den offentlige sektor er vigtigere end skattelettelser`,
                `TRAFIK__Investeringer i kollektiv trafik skal prioriteres højere end investeringer til fordel for privatbilisme` = `TRAFIK__Investeringer i kollektiv trafik skal prioriteres højere end investeringer til fordel for privatbilisme`,
                `RET__Straffen for grov vold og voldtægt skal skærpes` = `RET__Straffen for grov vold og voldtægt skal skærpes`,
                `SOCIAL__Kontanthjælpen skal sænkes, så den økonomiske gevinst ved at arbejde bliver større` = `SOCIAL__Kontanthjælpen skal sænkes, så den økonomiske gevinst ved at arbejde bliver større`,
                `INTEGRATION__Offentlige institutioner i Danmark tager for mange hensyn til religiøse minoriteter` = `INTEGRATION__Offentlige institutioner i Danmark tager for mange hensyn til religiøse minoriteter`,
                `EU__EU bestemmer for meget i forhold til dansk lov` = `EU__EU bestemmer for meget i forhold til dansk lov`,
                `UDVIKLING__Ulandsbistanden skal sænkes` = `UDVIKLING__Ulandsbistanden skal sænkes`,
                `MILJØ__Indsatsen for at forbedre miljøet skal gå forud for økonomisk vækst` = `MILJØ__Indsatsen for at forbedre miljøet skal gå forud for økonomisk vækst`,
                `KULTUR__Den offentlige kulturstøtte skal sænkes` = `KULTUR__Den offentlige kulturstøtte skal sænkes`
            )


#Export work in progress data set
write.table(x = merge_final, file = "dk_ft15_politician_responses_withoutDST.csv", fileEncoding = "UTF-8", sep = ",", row.names = FALSE)





#### Part 2: Scraping and merging with data from Statistics Denmark #### -------------------

rm( list = ls() ) #Clear work space

url <- "https://www.dst.dk/valg/Valg1487635/kandstat/kandstat.htm"
tabs <- GET(url, encoding = "UTF-8")
tabs <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F, header = TRUE)

tabs <- tabs[4]
dst <- data.frame(matrix(unlist(tabs), nrow=962, ncol=7), stringsAsFactors = F)
dst <- dst[(4:962),]
names(dst) <- c("name", "opstillet.i.kreds.nr", "nomineret.i.kreds.nr", "stemmer.i.alt", "stemmer.pers", "valgt.nr", "stedfor.nr")
dst <- dst %>%
  filter(!is.na(dst$opstillet.i.kreds.nr))

dst[636:724,]$nomineret.i.kreds.nr <- str_extract(dst[636:724,]$name, "[0-9]{1,3}. ")

dst$nomineret.i.kreds.nr <-gsub("\\.","",dst$nomineret.i.kreds.nr)
dst$stemmer.pers<-gsub("\\.","", dst$stemmer.pers)
dst$stemmer.i.alt<-gsub("\\.","", dst$stemmer.i.alt)
dst$valgt.nr<-gsub(" ","", dst$valgt.nr)
dst$name<-gsub("[0-9]{1,3}. ","",dst$name)


dst$nomineret.i.kreds.nr <-as.factor(as.character(dst$nomineret.i.kreds.nr))
dst$stemmer.i.alt<-as.numeric(as.character(dst$stemmer.i.alt))
dst$stemmer.pers<-as.numeric(as.character(dst$stemmer.pers))
dst$valgt.nr<-as.factor(as.character(dst$valgt.nr))
dst$stedfor.nr<-as.factor(as.character(dst$stedfor.nr))


# Add binary election outcome variable

  dst = dst %>% 
        mutate( elected = ifelse(valgt.nr != "", yes = TRUE, no = FALSE)) %>% 
        rename( votes.all = stemmer.i.alt, votes.pers = stemmer.pers)



##Merge with merge_final

merge_final <- read.csv(file="dk_ft15_politician_responses_withoutDST.csv", header = TRUE)

df <- left_join(merge_final, dst, by="name")

df1 <- df %>% 
  filter(is.na(df$votes.pers))

df1 <- df1[,1:25]

df2 <- left_join(df1, dst, by=c("pers.votes"="votes.pers"))
df2 <- df2 %>% filter (!duplicated(df2$name.x))

df <- df %>% filter (!is.na(df$votes.pers))


# Ensure split can be made, by removing excess variables that are not present in both dataframes
  df2 <- df2 %>% 
         select( -name.y ) %>% 
         rename( name = name.x)
  
  df <- df %>% 
        select( -votes.pers)

# Join the two parts of the dataset
  final <- rbind(df, df2)
  
  

#  SUSANNE: Hvad foregår der her?
# final <- final[-590,]
# final <- final[-516,]

  
# Cleaning and ordering
  
  final <- final %>% 
              rename( 
                  is.male = sex,
                  votes.pers = pers.votes
                )

  #Switch to binary gender and last election run
  final <- final %>%             
              mutate(
                is.male = ifelse(is.male == " Kvinde ", yes = FALSE, no = TRUE),
                ran.last.election = ifelse(ran.last.election == " Ja ", yes = FALSE, no = TRUE)
              )
  
  # Switch to short-form party names
  final <-final %>%
    mutate(party = ifelse(party == "alternativet", "aa", as.character(party)),
           party = ifelse(party == "venstre" , "v", as.character(party)),
           party = ifelse(party == "socialdemokraterne" , "a", as.character(party)),
           party = ifelse(party == "dansk folkeparti" , "o", as.character(party)),
           party = ifelse(party == "radikale venstre" , "b", as.character(party)),
           party = ifelse(party == "socialistisk folkeparti" , "f", as.character(party)),
           party = ifelse(party == "enhedslisten", "oe", as.character(party)),
           party = ifelse(party == "liberal alliance" , "i", as.character(party)),
           party = ifelse(party == "det konservative folkeparti" , "c", as.character(party)),
           party = ifelse(party == "kristendemokraterne" , "k", as.character(party)),
           party = ifelse(party ==  "uden for folketingsgrupperne"  , 1, as.character(party))
           )
  
  # Final ordering of the data
  
  final <- final %>% 
           select(
            name = name,
            party = party,
            storkreds = storkreds,
            lokalkreds = lokalkreds,
            age = age,
            is.male = is.male,
            title = title,
            location = location,
            elected = elected,
            votes.pers = votes.pers,
            votes.all = votes.all,
            valgt.nr = valgt.nr,
            stedfor.nr = stedfor.nr,
            opstillet.i.kreds.nr = opstillet.i.kreds.nr,
            nomineret.i.kreds.nr = nomineret.i.kreds.nr,
            ran.last.election = ran.last.election,
            UDDANNELSE__Efter.folkeskolereformen.får.eleverne.for.lange.skoledage = UDDANNELSE__Efter.folkeskolereformen.får.eleverne.for.lange.skoledage,
            FOREBYGGELSE__Afgiften.på.cigaretter.skal.sættes.op = FOREBYGGELSE__Afgiften.på.cigaretter.skal.sættes.op,
            SUNDHED__Et.besøg.hos.den.praktiserende.læge.skal.koste.eksempelvis.100.kr. = SUNDHED__Et.besøg.hos.den.praktiserende.læge.skal.koste.eksempelvis.100.kr.,
            VELFÆRD__Mere.af.ældreplejen.skal.udliciteres.til.private.virksomheder = VELFÆRD__Mere.af.ældreplejen.skal.udliciteres.til.private.virksomheder,
            ARBEJDSMARKED__Man.skal.hurtigere.kunne.genoptjene.retten.til.dagpenge = ARBEJDSMARKED__Man.skal.hurtigere.kunne.genoptjene.retten.til.dagpenge,
            ARBEJDSMARKED__Virksomheder.skal.kunne.drages.til.ansvar.for..om.deres.udenlandske.underleverandører.i.Danmark.overholder.danske.regler.om.løn..moms.og.skat = ARBEJDSMARKED__Virksomheder.skal.kunne.drages.til.ansvar.for..om.deres.udenlandske.underleverandører.i.Danmark.overholder.danske.regler.om.løn..moms.og.skat,
            ØKONOMI__Vækst.i.den.offentlige.sektor.er.vigtigere.end.skattelettelser = ØKONOMI__Vækst.i.den.offentlige.sektor.er.vigtigere.end.skattelettelser,
            TRAFIK__Investeringer.i.kollektiv.trafik.skal.prioriteres.højere.end.investeringer.til.fordel.for.privatbilisme = TRAFIK__Investeringer.i.kollektiv.trafik.skal.prioriteres.højere.end.investeringer.til.fordel.for.privatbilisme,
            RET__Straffen.for.grov.vold.og.voldtægt.skal.skærpes = RET__Straffen.for.grov.vold.og.voldtægt.skal.skærpes,
            SOCIAL__Kontanthjælpen.skal.sænkes..så.den.økonomiske.gevinst.ved.at.arbejde.bliver.større = SOCIAL__Kontanthjælpen.skal.sænkes..så.den.økonomiske.gevinst.ved.at.arbejde.bliver.større,
            INTEGRATION__Offentlige.institutioner.i.Danmark.tager.for.mange.hensyn.til.religiøse.minoriteter = INTEGRATION__Offentlige.institutioner.i.Danmark.tager.for.mange.hensyn.til.religiøse.minoriteter,
            EU__EU.bestemmer.for.meget.i.forhold.til.dansk.lov = EU__EU.bestemmer.for.meget.i.forhold.til.dansk.lov,
            UDVIKLING__Ulandsbistanden.skal.sænkes = UDVIKLING__Ulandsbistanden.skal.sænkes,
            MILJØ__Indsatsen.for.at.forbedre.miljøet.skal.gå.forud.for.økonomisk.vækst = MILJØ__Indsatsen.for.at.forbedre.miljøet.skal.gå.forud.for.økonomisk.vækst,
            KULTUR__Den.offentlige.kulturstøtte.skal.sænkes = KULTUR__Den.offentlige.kulturstøtte.skal.sænkes
            
           )
   
  
  
# .. afslutningsvist eksporteres det nye datasæt til en .csv fil kaldet ft15_final:

  write.table(x = final, file = "dk_ft15_politician_responses.csv", fileEncoding = "UTF-8", sep = ",", row.names = FALSE)