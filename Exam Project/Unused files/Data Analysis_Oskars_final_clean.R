Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
library("plyr")
library("rvest")
library("dplyr")
library("ggplot2")


#### Load and format data ####

data.org <- read.csv(file = "dk_ft15_politician_responses.csv", header = TRUE) #Load the raw dataset

data <- unique(data.org) # Load a "working" dataset, while removing duplicate entries



## Map responses to Likert-scale-style numeric
for (i in 17:31){ 
  data[,i] <- data[,i] %>% 
    gsub(x = ., pattern = "Helt enig", replacement = 5) %>% 
    gsub(x = ., pattern = "Delvist enig", replacement = 4) %>% 
    gsub(x = ., pattern = "Hverken enig eller uenig", replacement = 3) %>% 
    gsub(x = ., pattern = "Delvist uenig", replacement = 2) %>% 
    gsub(x = ., pattern = "Helt uenig", replacement = 1) 
}

for (i in 17:31){
  data[,i] <- as.numeric(data[,i])    #define as numeric
}


## Create colormapping to use for later plotting
colormapping <- c(                    
  "red",
  "darkorchid4",
  "lightgreen",
  "hotpink",
  "cyan1"     ,
  "grey"       ,
  "yellow"      ,
  "darkblue"     ,
  "orange"     ,
  "darkolivegreen4",
  "lightgrey"
)
names(colormapping) <- unique(as.character(data$party)) # Naming the elements in the character vector,
                                                        # for ggplot2 to call later.

## Create partyname mapping to use for later plotting
namemapping <- c(                    
  "Socialdemokratiet",
  "Radikale",
  "Konservative",
  "SF",
  "Liberal Alliance"     ,
  "Kristendemokraterne"       ,
  "Dansk Folkeparti"      ,
  "Venstre"     ,
  "Enhedslisten"     ,
  "Alternativet",
  "Uden for partierne"
)
names(namemapping) <- unique(as.character(data$party)) # Naming the elements in the character vector,
                                                       # for ggplot2 to call later.




#### Data description ####

########## Mean responses 

## -- Add mean response for each party, for each question -- ##

party.means <- data %>% 
  filter(party != 1) %>% 
  group_by(party) %>%
  summarize_each(funs(mean), -c(name, party, storkreds, lokalkreds, age, is.male,
                                title, location, elected, votes.pers, votes.all, valgt.nr,
                                stedfor.nr, opstillet.i.kreds.nr, nomineret.i.kreds.nr,
                                ran.last.election))

## --- Plot average response to each question, by party --- #

    # Construct labels with question text to be plotted
      labels <- data.frame( 
        question = names(party.means[2:16]),
        position.y = 16:2+0.5, # position is based on the plot below
        position.x = rep(3, 15) # position is based on the plot below
      )
      
    
    # Build plot
     p <-  ggplot(data = party.means) #initiate plot
    
    #Loop over each question, and plot the party means
      for(i in 2:16){
        p <- p + 
          geom_point(aes_string( 
            y = 18-i,                                                   # Split questions by y-coordinates for each question
            x = paste("party.means$", names(party.means)[i], sep = ""), # Let party means be x-axis
            fill = "party"
          ), colour = "black", alpha=0.8, shape = 21, size = 10 ) 
      }
    #Add questions  
      p <- p + geom_text(data = labels,
                         aes( y = position.y, x = position.x, label = question),
                         size = 3)
    
    #Party colors  
      p <- p +  scale_fill_manual ( values = colormapping )
    
    #Titles and axis
        p <- p + 
          theme_minimal() +            
          theme(axis.title.y  = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                panel.grid.minor=element_blank(),
                legend.position="top") +
          scale_y_continuous(breaks=seq(1, 16, 1)) +
          scale_x_continuous(breaks=c(1,3,5),
                             labels=c("Highly disagree", "Neither agree nor disagree", "Highly agree"))+
          ggtitle("Mean response to survey \n questions, by party")
    
    #Print plot    
    p      





## --- How close are parties to the middle? -------

#Calculate 'centerness' NOTE: Requires above code to have been run already, to create party.means
party.middle <- party.means
party.middle[,2:16] <- abs(party.middle[,2:16]-3)  #Re-align around center (defining center = 0) and take absolutes
party.middle[,17] <- rowMeans(party.middle[,2:16]) #Compute averages

#simplify dataframe
party.middle <- party.middle %>% 
  select( party = party, mean.dist.from.center = V17) #Select only the two relevant variables 

#Plot
p <- ggplot(data = party.middle, aes( x = reorder(party, mean.dist.from.center),
                                      y = mean.dist.from.center, fill = party)) +
  geom_bar(stat = "identity", 
           color = "black"
  ) +
  scale_fill_manual( values = colormapping) +
  coord_flip() +
  theme_minimal() +
  ylab("Average distance from 'neither agree nor disagree',\n on 0-2 scale") +
  xlab("")+
  ggtitle("What parties have the most extreme opinions?")
p                         




### Principal Component Analysis ----
pc <- princomp(data[,17:31], cor=TRUE, scores=TRUE)
data.pc <- data
data.pc[32:36] <- pc$scores[,1:5]


# Plot of two political dimensions

data.pc = filter(data.pc, party!="1") #Filter away candidates outside the parties

p <- ggplot(data = data.pc, aes(x = data.pc[,32], y = data.pc[,33] )) +
  geom_point(aes(fill = party), colour = "black", alpha=0.8, shape = 21, size = 10) +
  scale_fill_manual(values = colormapping) +
  theme_minimal()
p

## Let's try and divide the questions into two groups of questions: 
#redistribution and value-based policy questions

#Splitting the dataset 
redist <- data %>% select (1:16,18:19,23:24,26,29,31)
value <- data %>% select (1:17, 20:22,25, 27:28,30)

##Do PCA analysis on both subsets and restore 5 first components
pc1 <- princomp(redist[,17:23], cor = T, scores = T)
redist[24:28] <- pc1$scores[,1:5]
pc2 <- princomp(value[,17:24], cor = T, scores = T)
value[25:29] <- pc2$scores[,1:5]

##Compute summary statistics on components
summary(princomp(redist[,17:23], loadings = T ))
summary(princomp(value[,17:24], loadings = T ))

##Add the first component from each subset to original data in order to plot in same plot
data.pc[37] <- pc1$scores[,1]
data.pc[38] <- pc2$scores[,1]

##The PCA - using first component from each subset analysis
p <- ggplot(data.pc, aes(x = data.pc[,37], y=data.pc[,38])) +
  geom_point(aes(fill = party), colour = "black", alpha=0.8, shape = 21, size = 10) +
  scale_fill_manual(values = colormapping) +
  theme_minimal()
p


#Faceted Party Plot #### Note: not in final report 
data.pc = filter(data.pc) #Filter away candidates outside the parties

p <- ggplot(data = data.pc, aes(x = data.pc[,32], y = data.pc[,33], size = sqrt(votes.pers/pi))) +
  geom_point(aes(fill = party), colour = "black",
             alpha=0.8, shape = 21) +
  scale_size_continuous( range = c(1,25) ) +
  scale_fill_manual(values = colormapping) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ party)
p




#### Decision tree analysis #### ###### DENNIS: HER MÅ DU HAVE NOGET NYERE?

library(rpart)
set.seed(1)

# separate into training and test data
train <- sample( x = 1:nrow(data), size = 2/3 * nrow(data), replace = FALSE)
data.train <- data[train, ]
data.test <- data[-train,]

# Fit decision tree
model = rpart(party ~ ., data = data.train[,c(2,17:31)], method = "class")

partychoice = predict(model, newdata = data.test[,c(2,17:31)], type = "class")
summary(model)

library("rpart.plot")
prp(model, type = 4, extra = 2, nn = TRUE)





#### Agreement between candidates, Altinget definition #### --------------------------------------------

### Construct matrix of agreement between each pair of candidates ###

# Import and transpose responses
df.distance <- t(data[,17:31])

#Create empty matrix
cand.distance <- matrix(nrow = ncol(df.distance), ncol = ncol(df.distance))

#Fill out matrix, using DRs definition of agreement
for (k in 1:nrow(cand.distance)){
  for (i in 1:ncol(cand.distance)) {
    cand.distance[k,i] <- sum((-abs(df.distance[,k] - df.distance[,i])+4) / 60) #Use Altingets definition of Agreement
  }
  print(k)
}  

rm(df.distance)

#### Agreement with other candidates, full melted data set  #### ---------------------------------------------------

### Goal: the dataset should end up looking like this #

# Name1             name2           party_1       lokalkreds_1  storkreds_1   agreement     name2           party_1       lokalkreds_1  storkreds_1  
# navn navnsen1     navn navnsen1   venstre       xxx           xxxxx         88 %          navn navnsen1   venstre       xxx           xxxxx        
# navn navnsen1     lars løkke      venstre       xxx           xxxxx         58 %          lars løkke      venstre       xxx           xxxxx        
# navn navnsen1     pia k           venstre       xxx           xxxxx         42 %          pia k           venstre       xxx           xxxxx        
# .....                                                                                                                                            
# .....                                                                                                                                            
# .....                                                                                                                                            
# navn navnsen2      navn navnsen1  xxxx          xxxx          xxxxx         88 %           navn navnsen1  xxxx          xxxx          xxxxx        
# navn navnsen2      lars løkke                                                              lars løkke                                              
# navn navnsen2      pia k                                                                   pia k                                                   
# navn navnsen2      ...                                                                     ...                                                     


# Step 1: Add names, party, lokalkreds and storkreds to the dataframe with full distances
# Step 2: Melt the dataframe
# Step 3: Compute the distance for each candidate to the wanted other candidates by filtering and mutate (party, kreds, etc.)
# Step 4: Add distance measures as a single variable to the original dataset


### Step 1: Add names, party, lokalkreds and storkreds to the dataframe with full distances

cand.distance <- cbind(data[,c(1,2,3,4)], cand.distance)


#Put names on columns as well      
names(cand.distance)[5:728] <- as.character(cand.distance[,1])


#Load libraries
library(reshape2)

#Melt dataframe to obtain a 'long' version of the above distance matrix, as depicted in goal
melted.distance <- melt(data = cand.distance,
                        id.vars = c(1,2,3,4),
                        value.name = "agreement")

#Add candidate info to both 'sides' of the list (such that info is attached to both names in every row)
cand.info <- cand.distance[,1:4]
melted.distance <- left_join(melted.distance, cand.info, by = c("variable" = "name"))
rm(cand.info)
#Goal reached!


###Create distance measures

#Average agreement with three nearest same party candidates within storkreds
distance.measure <- melted.distance %>% 
  filter(
    storkreds.x == storkreds.y &      # Look only within same storkreds (for those with unknown lokalkreds)
      party.x == party.y &          # Look only across parties
      name != variable) %>%      # Technical: remove agreement with oneself
  group_by(name) %>% 
  arrange(desc(agreement)) %>% 
  filter( 1:n() == 1 | 1:n() == 2 | 1:n() == 3) %>%  #Select top three, with ties removed (always takes three)
  summarize(
    agree.three.mean.party.storkreds = mean(agreement)
  )
agree.three.mean.party.storkreds <- distance.measure


#Average agreement with three nearest non-same party candidates within storkreds
distance.measure <- melted.distance %>% 
  filter(
    storkreds.x == storkreds.y &      # Look only within same storkreds (for those with unknown lokalkreds)
      party.x != party.y &          # Look only across parties
      name != variable) %>%      # Technical: remove agreement with oneself
  group_by(name) %>% 
  arrange(desc(agreement)) %>% 
  filter( 1:n() == 1 | 1:n() == 2 | 1:n() == 3) %>%  #Select top three, with ties removed (always takes three)
  summarize(
    agree.three.mean.oth.party.storkreds = mean(agreement)
  )
agree.three.mean.oth.party.storkreds <- distance.measure


### Add to original dataframe

#Add distance measures to principal component dataframe
data.pc <- left_join(data.pc, agree.three.mean.party.storkreds)
data.pc <- left_join(data.pc, agree.three.mean.oth.party.storkreds)


### Plot agree.mean and personal votes in PCA dimensions

# Plot of mean agreement with five nearest candidates

data.pc.plot <- filter(data.pc, party != "1")
p <- ggplot(data = data.pc.plot, aes(x = data.pc.plot[,32], y = data.pc.plot[,33], size = sqrt(votes.pers/pi))) +
  geom_point(aes(fill = agree.three.mean.party.storkreds), colour = "black", alpha=0.8, shape = 21) +
  scale_size_continuous( range = c(1,25), labels = c("4,000", "15,000"), breaks = c(50, 100), name =  "votes" ) +
  scale_fill_continuous(low = "green", high = "red", name = "agree.mean") +
  theme(legend.position = "none") +
  # facet_wrap(~ party) +
  xlab("First Component") + 
  ylab("Second Component") + 
  theme_minimal()
p  



##### Compute center response of each party -------------------------------------------------------

centers <- data %>% 
  select(party, 17:31) %>%  #Select all questions
  group_by(party) %>%       # group by party
  summarize_each( funs(mean) ) %>% #take means of each question
  
  #Add to each line in the dataset, the agreement with that persons party
  for (i in 1:nrow(data.pc)) {
    par <- data.pc$party[i] #Record party for candidate i
    data.pc$agree.party.mean[i] = sum((-abs(data.pc[i,17:31] - filter(centers, party == par)[,2:16])+4) / 60) #Compute agreement
    print(i) #Print algorithm progress
  }

#Compute average agreement by party, NOTE: Not in report
party.centers <- data.pc %>% 
  group_by(party) %>% 
  summarize(
  average.agreement = mean(agree.party.mean) * 100
     ) %>% 
  arrange(desc(average.agreement))
party.centers #Print in console


# Plot spread of each party, NOTE: Not in report
p <- ggplot( data = party.centers, aes( x = reorder(party, average.agreement), y = average.agreement, fill = party) ) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = colormapping) +
  coord_flip()+
  theme_minimal() +
  ylab("..") +
  # ylim(60, 100)+
  xlab("..")+
  ggtitle("...")
p



#### -------- Regression analysis -------------------

names(reg.data)

reg.data <- data.pc
reg.data <- filter(reg.data, party != "1")

# Kør for enkelte partier, notér estimat
# agree.three.mean, Signifikant for: a, b, k, (positiv alle)
# agree.three.oth.mean, signifikant for o (negativ), 

# The following regression code, can be altered to include different combinations of variables,
# by commenting and uncommenting the lines below


lm1 <- lm(formula = votes.pers ~ 
            agree.three.mean.party.storkreds + 
            # agree.three.mean.party.storkreds*party +
            # agree.three.mean.oth.party.storkreds + 
            # agree.party.mean +
            # agree.party.mean*party +
            # party +
              is.male +
              ran.last.election+
              age,
          data = reg.data, na.action = "na.omit")
summary(lm1) #Plot summary 
length(lm2$fitted.values) #Plot the number of observations (which is equal to the number of fitted values)

library(stargazer)

stargazer(lm1, lm2, lm3) #Print output to be used (after cleanup) in Report


### How many votes does it take to get elected? Plotting kernel density estimates of personal
### votes by binary election. NOTE: Not in report.

p <- ggplot(data = data.pc, aes( x = votes.pers, group = elected, fill = elected)) +
     geom_density(alpha = 0.6) +
     scale_x_log10( breaks =  c(10, 100, 500, 1000, 2000, 5000, 10000,50000 )) +
     scale_fill_discrete() +
     xlab("Personal votes received") +
     theme_minimal()
p

# Compute the number of elected and unelected candidate with fewer than 2000 votes (referenced in text)
av <- data.pc %>% 
      group_by(elected) %>%
      filter(votes.pers < 2000) %>% 
      summarize(av = n() ) 
av


#### Description of the agree.mean_{i} variable #### -----------

summary(data.pc$agree.three.mean.party.storkreds) #Mean and quartiles
sqrt(var(data.pc$agree.three.mean.party.storkreds, na.rm = TRUE)) #Standard deviation

#Density estimates with normal curve
p <- ggplot(data = data.pc, aes(x = agree.three.mean.party.storkreds))+
  stat_function(fun = dnorm, args = list(mean = 0.8586,
                                         sd = 0.07812928)) + # This is crap code, but it works. Sorry.
  geom_density(na.rm = T, fill = "darkgreen", alpha = 0.8) +
  theme_minimal()
p

#Counting the number with agree.mean_{i} = 1.00 (referenced in text)
data.pc <- data.pc %>% ungroup()
sum(data.pc[,42][data.pc[,42] == 1], na.rm = T)





#Explanation of agreement definition
# http://www.altinget.dk/kandidater/ft15/information.aspx#.VmNPf7xlmRs

#   Testens algoritme virker sådan, at der gives point på baggrund af forskellen mellem en kandidat
#   og en brugers besvarelse. Et ens svar giver 4 point (f.eks. helt enig og helt enig), et trin ved
#   siden af giver 3 point (f.eks. helt uenig og delvist uenig). Man får 0 point for svar i hver sin
#   ende i skalaen (f.eks. helt enig og helt uenig). Hvert spørgsmål har en 1/20 vægt, og antallet af
#   point bliver summeret til den endelig procentsats.