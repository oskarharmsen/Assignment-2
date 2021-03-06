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

#Removing the double Kristian Andersen

#   data <- data %>%         # A candidate, Kristian Andersen, has several entries, these are removed. NOTE: This removes one candidate
#           group_by(name) %>% 
#           filter(row_number() == 1 ) %>% # Method: data is grouped on name variable, and groups with >1 name are discarded
#           ungroup() 


## Create mapping of response variables,


#    Use this to copy into code: -c(name, party, storkreds, lokalkreds, age, is.male,
#                                   title, location, elected, votes.pers, votes.all, valgt.nr,
#                                   stedfor.nr, opstillet.i.kreds.nr, nomineret.i.kreds.nr,
#                                   ran.last.election)


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

p      





## --- How close are parties to the middle? -------

#Calculate 'centerness' NOTE: Requires above code to have been run already, to create party.means
party.middle <- party.means
party.middle[,2:16] <- abs(party.middle[,2:16]-3)  #Re-align around center (defining center = 0) and take absolutes
party.middle[,17] <- rowMeans(party.middle[,2:16]) #Compute averages

#simplify dataframe
party.middle <- party.middle %>% 
  select( party = party, mean.dist.from.center = V17) #Select only the two relevant variables 

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


#### Variance in responses -------------------------- NOTE: useless. Doesn't measure the right thing.

  data.var <- data %>% 
              group_by(party) %>% 
              select(party, 17:31) %>% 
              summarize_each(
                funs(mean)
              )

  #This calculates variance in responses by party (but it's a non-informative measure)              
  for (i in 1:nrow(data.var)){
    data.var$party.std[i] <- sqrt(sum((data.var[i,2:16] - rep(mean(as.numeric(data.var[i,2:16])), 15))^2)/ (15 - 1) )
  }

  p <- ggplot( data = data.var, aes( x = reorder(party, party.std), y = party.std, fill = party) ) +
       geom_bar(stat = "identity") + 
       scale_fill_manual(values = colormapping) +
       coord_flip()+
       theme_minimal() +
       ylab("..") +
       xlab("..")+
       ggtitle("...")
  p



  p <- ggplot(data = data.var, aes( x = reorder(data.var, party.std),
                                        y = party.std, fill = party)) +
    geom_bar(stat = "identity", 
             color = "black"  ) +
    scale_fill_manual( values = colormapping) +
    coord_flip() +
    theme_minimal() +
    ylab("..") +
    xlab("..")+
    ggtitle("...")
  p  
              



### Principal Component Analysis ----
pc <- princomp(data[,17:31], cor=TRUE, scores=TRUE)
data.pc <- data
data.pc[32:36] <- pc$scores[,1:5]


#Pretty Plot#
# data.pc = filter(data.pc, party!="1") #Filter away candidates outside the parties

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


#Faceted Party Plot#
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




#   library(ggfortify)
#   autoplot(prcomp(data[,17:31]), loadings = TRUE, loadings.colour = 'blue',
#            loadings.label = TRUE, loadings.label.size = 3)


#### Decision tree analysis ####

  
  library(rpart)
  set.seed(1)
  
  # separate into training and test data
  train <- sample( x = 1:nrow(data), size = 2/3 * nrow(data), replace = FALSE)
  data.train <- data[train, ]
  data.train <- data.train[,c(2,17:31)]
  names(data.train) = c("party","uddannelse","forebyggelse","sundhed","velfærd","arb1","arb2","økonomi","trafik","ret","social","integration","eu","udvikling","miljø","kultur")
  data.test <- data[-train,]
  data.test <- data.test[,c(2,17:31)]
  names(data.test) = c("party","uddannelse","forebyggelse","sundhed","velfærd","arb1","arb2","økonomi","trafik","ret","social","integration","eu","udvikling","miljø","kultur")
  
  # Fit decision tree
  model = rpart(party ~ ., data = data.train, method = "class")
  
  partychoice = predict(model, newdata = data.test, type = "class")

  # plot the model
  library("rpart.plot")
  prp(model,   box.col = "lightblue", border.col = "darkblue", shadow.col = "lightgrey", split.cex = 0.7,split.font = 4, split.col = "darkblue", split.border.col = 9, split.shadow.col = "lightgrey", nn.col = "darkred")
  
  
  # variable importance
  
  v.importance <- data.frame(model$variable.importance)

  # run the model on the whole dataset
  
  data.pred <- data[,c(2,17:31)]
  names(data.pred) <- c("party","uddannelse","forebyggelse","sundhed","velfærd","arb1","arb2","økonomi","trafik","ret","social","integration","eu","udvikling","miljø","kultur")

  pred = data.frame(predict(model, newdata = data.pred, type = "class"))
  
  data.pred <- cbind(data.pred, pred)
  
  data.pred$homogen = ifelse(data.pred$party == data.pred[,17], 1,0 )  
  
  data.pred = mutate(data.pred, votes = data$votes.pers)
  
  # how is the mean personal votes for "homogenious" candidates versus "non-homogenious"
  
  homogenious <- data.pred %>%
    group_by(homogen) %>%
    summarise(meanvotes = mean(votes))
  
#### Distances between points #### ---------------------------  

# Construct matrix of Euclidean distances between all candidates, in all dimensions

df.distance <- data[,17:31] 

#Select only questions
rownames(df.distance) <- 1:nrow(df.distance) #Set names of rows
names(df.distance)[1:15] <- 1:15 #Simplify variable names

#Compute distance matrix
dist.eucl <- dist(df.distance) %>% 
  as.matrix() %>% 
  as.data.frame()

#Make a smaller matrix, containing only the distance to 30 nearest candidates, for each candidate
cand.dist <- data.frame() 
for (i in 1:ncol(dist.eucl)) {
  cand.dist[1:30, i] <- sort(dist.eucl[,i])[1:30]
}
cand.dist.one <- t(cand.dist[2,])

#Average distance to five nearest candidates
nearest.five.mean <- rep(0, ncol(dist.eucl))
for (i in 1:ncol(dist.eucl)) {
  nearest.five.mean[i] <- mean(cand.dist[2:6,i])
}

#Add distance measures to principal component dataframe
data.pc$nearest.cand <- as.numeric(cand.dist.one )
data.pc$nearest.five.mean <- nearest.five.mean


#Test plot of nearest candidates (note that distance is measured in many more dimensions than those plotted)
p <- ggplot(data = data.pc, aes(x = data.pc[,32], y = data.pc[,33] )) +
  geom_point(aes(fill = nearest.cand), colour = "black", alpha=0.8, shape = 21, size = 10) +
  scale_fill_continuous(low = "darkred", high = "green") +
  theme_minimal()
p


#Test plot of mean distance to five nearest candidates (note that distance is measured in many more dimensions than those plotted)
p <- ggplot(data = data.pc, aes(x = data.pc[,32], y = data.pc[,33] )) +
  geom_point(aes(fill = nearest.five.mean), colour = "black", alpha=0.8, shape = 21, size = 10) +
  scale_fill_continuous(low = "darkred", high = "green") +
  theme_minimal()
p


# THE MILLION DOLLAR PLOT (if it worked, but it doesn't)
p <- ggplot(data = filter(data.pc, votes.pers > 10 & nearest.five.mean >0), aes(x = nearest.five.mean, y = votes.pers )) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method=lm, col = "red")+
  # scale_fill_continuous(low = "darkred", high = "green") +
  theme_minimal()
p




#### Agreement between candidates, Altinget definition #### --------------------------------------------

### Construct matrix of agreement between all candidates ###

# Import and transpose responses
df.distance <- t(data[,17:31])

#Create empty matrix
cand.distance <- matrix(nrow = ncol(df.distance), ncol = ncol(df.distance))

#Fill out matrix 
for (k in 1:nrow(cand.distance)){
  for (i in 1:ncol(cand.distance)) {
    cand.distance[k,i] <- sum((-abs(df.distance[,k] - df.distance[,i])+4) / 60) #Use Altingets definition of Agreement (see below)
  }
  print(k)
}  

rm(df.distance)

###Average agreement with five nearest candidates

#Create average 'agreement' with five closest candidate for each candidate

agree.five.mean <- data.frame() #Empty frame

for (i in 1:ncol(cand.distance)) {
  agree.five.mean[1, i] <- sort(cand.distance[,i], decreasing = TRUE)[2:6] %>%  #Choose top 5 of each candidates agreement 
    mean() # Take the mean
}

agree.five.mean <- t(agree.five.mean) #transpose before merging with original data frame



### Test results in PCA plot

#Add distance measures to principal component dataframe
data.pc$agree.five.mean <- as.numeric(agree.five.mean)

### Plot

# Plot of mean agreement with five nearest candidates
p <- ggplot(data = data.pc, aes(x = data.pc[,32], y = data.pc[,33] )) +
  geom_point(aes(fill = agree.five.mean), colour = "black", alpha=0.8, shape = 21, size = 10) +
  scale_fill_continuous(low = "green", high = "red") +
  theme(legend.position = "none") +
  facet_wrap(~ party) +
  theme_minimal()
p


# THE MILLION DOLLAR PLOT (if it worked, but it doesn't)
# - Regressing personal votes on average agreement with five nearest candidates
p <- ggplot(data = filter(data.pc, votes.pers > 10), aes(x = agree.five.mean, y = votes.pers )) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method=lm, col = "red")+
  theme_minimal()
p

##### Party center analysis -------------------------------------------------------


centers <- data %>% 
  select(party, 17:31) %>% 
  group_by(party) %>% 
  summarize_each( funs(mean) ) %>% 
  
  for (i in 1:nrow(data.pc)) {
    par <- data.pc$party[i]
    data.pc$agree.party.mean[i] = sum((-abs(data.pc[i,17:31] - filter(centers, party == par)[,2:16])+4) / 60)
    print(i) 
  }
  
party.centers <- data.pc %>% 
  group_by(party) %>% 
  summarize(
  average.agreement = mean(agree.party.mean) * 100
     ) %>% 
  arrange(desc(average.agreement))
party.centers

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




#### Agreement with other candidates, full melted data set  #### ---------------------------------------------------
<<<<<<< HEAD
    
      ### Goal: the dataset should look something like this #
      
        # Name1             name2           party        lokalkreds     storkreds     agreement   
        # navn navnsen      esben lunde     venstre       xxx           xxxxx         88 %
        # navn navnsen      lars l?kke      venstre       xxx           xxxxx         58 %
        # navn navnsen      pia K           venstre       xxx           xxxxx         42 %
        # .....
        # .....
        # .....
        # esben lunde      navn navnsen     o             xxx           xxxxx         88 %
        # esben lunde      ...
        # esben lunde      ...
        # esben lunde      ...
        
      
        # Step 1: Add names, party, lokalkreds and storkreds to the dataframe with full distances
        # Step 2: Melt the dataframe
        # Step 3: Compute the distance for each candidate to the wanted other candidates (party, kreds, etc.)
        # Step 4: Add distance measures as a single variable to the original dataset
        
      
    ### Step 1: Add names, party, lokalkreds and storkreds to the dataframe with full distances
      
        View(cand.distance)
      
        cand.distance <- cbind(data[,c(1,2,3,4)], cand.distance)
      
        # Work around the *Kristian Andersen* mistake: This should be checked, if Kristian Andersen is fixed.
        
        #Add names to rows
        cand.distance[,1]    <- as.character(cand.distance[,1])
        cand.distance[517,1] <- "Kristian Andersen_K1"
        cand.distance[518,1] <- "Kristian Andersen_K2"
        cand.distance[592,1] <- "Kristian Andersen_V1"
        cand.distance[593,1] <- "Kristian Andersen_V2"
        cand.distance[,1]    <- as.factor(cand.distance[,1])
        
        
        cand.distance2 <- cand.distance
        
        #Put names on columns as well      
        names(cand.distance)[5:728] <- as.character(cand.distance[,1])
        
        
        #Load libraries
        library(reshape2)
        
        #Melt dataframe to obtain a 'long' version of the above distance matrix
        melted.distance <- melt(data = cand.distance,
                                id.vars = c(1,2,3,4),
                                value.name = "agreement")
        
        #Add candidate info to both 'sides' of the list (such that info is attached to both names in every row)
        cand.info <- cand.distance[,1:4]
        melted.distance <- left_join(melted.distance, cand.info, by = c("variable" = "name"))
        rm(cand.info)
        

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
        
        ### Plot: DISTANCE TO OWN PARTY
        
        # Plot of mean agreement with five nearest candidates
        p <- ggplot(data = data.pc, aes(x = data.pc[,32], y = data.pc[,33] )) +
          geom_point(aes(fill = agree.three.mean.party.storkreds), colour = "black", alpha=0.8, shape = 21, size = 10) +
          scale_fill_continuous(low = "green", high = "red") +
          theme(legend.position = "none") +
          #facet_wrap(~ party) +
          theme_minimal()
        p
        
        
        # THE MILLION DOLLAR PLOT (if it worked, but it doesn't)
        # - Regressing personal votes on average agreement with five nearest candidates
        p <- ggplot(data = filter(data.pc, votes.pers > 10), aes(x = agree.three.mean.party.storkreds, y = votes.pers )) +
          geom_point() +
          scale_y_log10() +
          geom_smooth(method=lm, col = "red")+
          theme_minimal()
        p
        
        
        
        ### Plot: DISTANCE TO OTHER PARTY
        
        # Plot of mean agreement with five nearest candidates
        p <- ggplot(data = data.pc, aes(x = data.pc[,32], y = data.pc[,33] )) +
          geom_point(aes(fill = agree.three.mean.oth.party.storkreds), colour = "black", alpha=0.8, shape = 21, size = 10) +
          scale_fill_continuous(low = "green", high = "red") +
          theme(legend.position = "none") +
          #facet_wrap(~ party) +
          theme_minimal()
        p
        
        
        # THE MILLION DOLLAR PLOT (if it worked, but it doesn't)
        # - Regressing personal votes on average agreement with five nearest candidates
        p <- ggplot(data = filter(data.pc, votes.pers > 10), aes(x = agree.three.mean.oth.party.storkreds, y = votes.pers )) +
          geom_point() +
          scale_y_log10() +
          geom_smooth(method=lm, col = "red")+
          theme_minimal()
        p

        
#### -----------------------------------      
        
        
        
        
        
    
=======

### Goal: the dataset should look something like this #

# Name1             name2           party        lokalkreds     storkreds     agreement   
# navn navnsen      esben lunde     venstre       xxx           xxxxx         88 %
# navn navnsen      lars l?kke      venstre       xxx           xxxxx         58 %
# navn navnsen      pia K           venstre       xxx           xxxxx         42 %
# .....
# .....
# .....
# esben lunde      navn navnsen     o             xxx           xxxxx         88 %
# esben lunde      ...
# esben lunde      ...
# esben lunde      ...


# Step 1: Add names, party, lokalkreds and storkreds to the dataframe with full distances
# Step 2: Melt the dataframe
# Step 3: Compute the distance for each candidate to the wanted other candidates (party, kreds, etc.)
# Step 4: Add distance measures as a single variable to the original dataset


### Step 1: Add names, party, lokalkreds and storkreds to the dataframe with full distances

View(cand.distance)

cand.distance <- cbind(data[,c(1,2,3,4)], cand.distance)

# Work around the *Kristian Andersen* mistake: This should be checked, if Kristian Andersen is fixed.

#Add names to rows
cand.distance[,1]    <- as.character(cand.distance[,1])
cand.distance[517,1] <- "Kristian Andersen_K1"
cand.distance[518,1] <- "Kristian Andersen_K2"
cand.distance[592,1] <- "Kristian Andersen_V1"
cand.distance[593,1] <- "Kristian Andersen_V2"
cand.distance[,1]    <- as.factor(cand.distance[,1])


cand.distance2 <- cand.distance

#Put names on columns as well      
names(cand.distance)[5:728] <- as.character(cand.distance[,1])


#Load libraries
library(reshape2)

#Melt dataframe to obtain a 'long' version of the above distance matrix
melted.distance <- melt(data = cand.distance,
                        id.vars = c(1,2,3,4),
                        value.name = "agreement")

#Add candidate info to both 'sides' of the list (such that info is attached to both names in every row)
cand.info <- cand.distance[,1:4]
melted.distance <- left_join(melted.distance, cand.info, by = c("variable" = "name"))
rm(cand.info)


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

### Plot: DISTANCE TO OWN PARTY

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

p <- ggplot(data = data.pc, aes(x = data.pc[,32], y = data.pc[,33], size = sqrt(votes.pers/pi))) +
  geom_point(aes(fill = party), colour = "black",
             alpha=0.8, shape = 21) +
  scale_size_continuous( range = c(1,25) ) +
  
  
p


# THE MILLION DOLLAR PLOT (if it worked, but it doesn't)
# - Regressing personal votes on average agreement with five nearest candidates
p <- ggplot(data = filter(data.pc, votes.pers > 10), aes(x = agree.three.mean.party.storkreds, y = votes.pers )) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method=lm, col = "red")+
  theme_minimal()
p



### Plot: DISTANCE TO OTHER PARTY

# Plot of mean agreement with five nearest candidates
p <- ggplot(data = data.pc, aes(x = data.pc[,32], y = data.pc[,33] )) +
  geom_point(aes(fill = agree.three.mean.oth.party.storkreds), colour = "black", alpha=0.8, shape = 21, size = 10) +
  scale_fill_continuous(low = "green", high = "red") +
  theme(legend.position = "none") +
  #facet_wrap(~ party) +
  theme_minimal()
p


# THE MILLION DOLLAR PLOT (if it worked, but it doesn't)
# - Regressing personal votes on average agreement with five nearest candidates
p <- ggplot(data = filter(data.pc, votes.pers > 10), aes(x = agree.three.mean.oth.party.storkreds, y = votes.pers )) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method=lm, col = "red")+
  theme_minimal()
p


#### -------- Regression analysis -------------------

names(reg.data)

reg.data <- data.pc
reg.data <- filter(reg.data, party != "1")

# K?r for enkelte partier, not?r estimat
# agree.three.mean, Signifikant for: a, b, k, (positiv alle)
# agree.three.oth.mean, signifikant for o (negativ), 

lm2 <- lm(formula = log(votes.pers) ~ 
            # agree.three.mean.party.storkreds + 
            # agree.three.mean.oth.party.storkreds + 
            # agree.three.mean.party.storkreds*party +
            # nearest.cand +
            # nearest.five.mean +
            # agree.party.mean +
            # agree.party.mean*party +
            # party +
            # opstillet.i.kreds.nr +
              is.male +
              ran.last.election+
              age,
          data = reg.data, na.action = "na.omit")
summary(lm2)
length(lm2$fitted.values)



library(stargazer)

stargazer(lm1, lm2, lm3)

### How many votes does it take to get elected?

av <- data.pc %>% 
      group_by(elected) %>%
      filter(votes.pers < 2000) %>% 
      summarize(av = n() ) 
av

p <- ggplot(data = data.pc, aes( x = votes.pers, group = elected, fill = elected)) +
     geom_density(alpha = 0.6) +
     scale_x_log10( breaks =  c(10, 100, 500, 1000, 2000, 5000, 10000,50000 )) +
     scale_fill_discrete() +
     xlab("Personal votes received") +
     theme_minimal()
p



#### Description of the distance measure #### -----------

summary(data.pc$agree.three.mean.party.storkreds)
sqrt(var(data.pc$agree.three.mean.party.storkreds, na.rm = TRUE))

p <- ggplot(data = data.pc, aes(x = agree.three.mean.party.storkreds))+
  stat_function(fun = dnorm, args = list(mean = 0.8586,
                                         sd = 0.07812928)) + # This is crap code, but it works. Sorry.
  geom_density(na.rm = T, fill = "darkgreen", alpha = 0.8) +
  theme_minimal()
p

data.pc <- data.pc %>% ungroup()
sum(data.pc[,42][data.pc[,42] == 1], na.rm = T)



>>>>>>> origin/master
#### TO DO #####

#       - Build distance algorithm
#         - within parties
#         - within storkreds
#         - within lokalkreds
#       
#       - Match valgkredsdata wwith
#         - latitude, or
#         - median income
#         
#       - Fix 
#         - scales in facet wrapped plots: the horizontal axis is different for each plot
#     


#### TRASH #####  
<<<<<<< HEAD
  
  ## Variance in responses
  
  resp.var <- data[,17:31] %>% 
    var() %>% 
    diag() %>% 
    sqrt() %>% 
    t()
  
  rownames(resp.var) <- "Standard Deviation"
  
  
  #Explanation
  # http://www.altinget.dk/kandidater/ft15/information.aspx#.VmNPf7xlmRs
  
#   Testens algoritme virker s?dan, at der gives point p? baggrund af forskellen mellem en kandidat
=======

## Variance in responses

resp.var <- data[,17:31] %>% 
  var() %>% 
  diag() %>% 
  sqrt() %>% 
  t()

rownames(resp.var) <- "Standard Deviation"


#Explanation
# http://www.altinget.dk/kandidater/ft15/information.aspx#.VmNPf7xlmRs

#   Testens algoritme virker s?dan, at der gives point p? baggrund af forskellen mellem en kandidat
>>>>>>> origin/master
#   og en brugers besvarelse. Et ens svar giver 4 point (f.eks. helt enig og helt enig), et trin ved
#   siden af giver 3 point (f.eks. helt uenig og delvist uenig). Man f?r 0 point for svar i hver sin
#   ende i skalaen (f.eks. helt enig og helt uenig). Hvert sp?rgsm?l har en 1/20 v?gt, og antallet af
#   point bliver summeret til den endelig procentsats.



