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
        p <-  ggplot(data = party.means) #initatite plot
        
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

  
  
  
  
## --- How close are parties to the middle? --- ##  ----
  
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
  
  
  #Faceted Party Plot#
  data.pc = filter(data) #Filter away candidates outside the parties
  
  p <- ggplot(data = data.pc, aes(x = data.pc[,32], y = data.pc[,33], size = sqrt(votes.pers/pi))) +
    geom_point(aes(fill = party), colour = "black",
               alpha=0.8, shape = 21) +
    scale_size_continuous( range = c(1,25) ) +
    scale_fill_manual(values = colormapping) +
    theme_minimal() +
    theme(legend.position = "none") +
    facet_wrap(~ party)
  p
  
  

  
  library(ggfortify)
  autoplot(prcomp(data[,17:31]), loadings = TRUE, loadings.colour = 'blue',
           loadings.label = TRUE, loadings.label.size = 3)
  
  
  
#### Decision tree analysis ####
  
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



#### TRASH #####  
  
  ## Variance in responses
  
  resp.var <- data[,17:31] %>% 
    var() %>% 
    diag() %>% 
    sqrt() %>% 
    t()
  
  rownames(resp.var) <- "Standard Deviation"
  
  
  #Explanation
  # http://www.altinget.dk/kandidater/ft15/information.aspx#.VmNPf7xlmRs
  
#   Testens algoritme virker sådan, at der gives point på baggrund af forskellen mellem en kandidat
#   og en brugers besvarelse. Et ens svar giver 4 point (f.eks. helt enig og helt enig), et trin ved
#   siden af giver 3 point (f.eks. helt uenig og delvist uenig). Man får 0 point for svar i hver sin
#   ende i skalaen (f.eks. helt enig og helt uenig). Hvert spørgsmål har en 1/20 vægt, og antallet af
#   point bliver summeret til den endelig procentsats.
  
  