Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
library("plyr")
library("rvest")
library("dplyr")
library("ggplot2")


#### Load and format data ####
  
  data.org <- read.csv(file = "dk_ft15_politician_responses.csv", header = TRUE) #Load the raw dataset
  data <- data.org #Load a "working" dataset

  ## Map responses to Likert-scale-style numeric
    for (i in 5:19){ 
      data[,i] <- data[,i] %>% 
      gsub(x = ., pattern = "Helt enig", replacement = 5) %>% 
      gsub(x = ., pattern = "Delvist enig", replacement = 4) %>% 
      gsub(x = ., pattern = "Hverken enig eller uenig", replacement = 3) %>% 
      gsub(x = ., pattern = "Delvist uenig", replacement = 2) %>% 
      gsub(x = ., pattern = "Helt uenig", replacement = 1) 
    }
  for (i in 5:19){
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
            summarize_each(funs(mean), -c(name, age, votes))

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
  
  #Calculate 'centerness'
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
  pc <- princomp(data[,5:19], cor=TRUE, scores=TRUE)
  data[20:24] <- pc$scores[,1:5]
  
  
  #Pretty Plot#
  data2 = filter(data, party!="1") #Filter away candidates outside the parties
  
  p <- ggplot(data = data2, aes(x = data2[,20], y = data2[,21] )) +
    geom_point(aes(fill = party), colour = "black", alpha=0.8, shape = 21, size = 10) +
    scale_fill_manual(values = colormapping) +
    theme_minimal()
  p
  
  
  #Faceted Party Plot#
  data2 = filter(data) #Filter away candidates outside the parties
  
  p <- ggplot(data = data2, aes(x = data2[,20], y = data2[,21], size = sqrt(votes/pi))) +
    geom_point(aes(fill = party), colour = "black",
               alpha=0.8, shape = 21) +
    scale_size_continuous( range = c(1,25) ) +
    scale_fill_manual(values = colormapping) +
    theme_minimal() +
    theme(legend.position = "none") +
    facet_wrap(~ party)
  p
  
  

  
  library(ggfortify)
  autoplot(prcomp(data[,5:19]), loadings = TRUE, loadings.colour = 'blue',
           loadings.label = TRUE, loadings.label.size = 3)
  
  
  
#### Decision tree analysis ####
  
  library(rpart)
  set.seed(1)
  
  train <- sample( x = 1:nrow(data), size = 2/3 * nrow(data), replace = FALSE)
  
  data.train <- data[train, ]
  data.test <- data[-train,]
  
  
  model = rpart(party ~ ., data = data.train[,c(3,5:19)], method = "class")
  partychoice = predict(model, newdata = data.test[,c(3,5:19)], type = "class")
  summary(model)
  
  library("rpart.plot")
  prp(model, type = 4, extra = 2, nn = TRUE)

#### TRASH #####  
  
  ## Variance in responses
  
  resp.var <- data[,5:19] %>% 
    var() %>% 
    diag() %>% 
    sqrt() %>% 
    t()
  
  rownames(resp.var) <- "Standard Deviation"
  
  
  
  #Age plot
  p <- ggplot(data = data, aes(x = data[,20], y = data[,21] )) +
    geom_point(aes(color=age), size=10, alpha=0.5)+
    scale_color_gradient(low="green", high = "red")+
    theme_minimal()
  p
  
  