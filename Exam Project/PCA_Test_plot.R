Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
library("plyr")
library("rvest")
library("dplyr")
library("ggplot2")

data.org <- read.csv(file = "dk_ft15_politician_responses.csv", header = TRUE)


data <- data.org

  # Convert to numeric
    for (i in 5:19){
      data[,i] <- data[,i] %>% 
      gsub(x = ., pattern = "Helt enig", replacement = 5) %>% 
      gsub(x = ., pattern = "Delvist enig", replacement = 4) %>% 
      gsub(x = ., pattern = "Hverken enig eller uenig", replacement = 3) %>% 
      gsub(x = ., pattern = "Delvist uenig", replacement = 2) %>% 
      gsub(x = ., pattern = "Helt uenig", replacement = 1) 
    }

  for (i in 5:19){
    data[,i] <- as.numeric(data[,i]) 
  }


  #Principal Component Analysis
  pc <- princomp(data[,5:19], cor=TRUE, scores=TRUE)
  
  data[20:24] <- pc$scores[,1:5]
  

  #Pretty Plot#
  data2 = filter(data, party!=1)
  
  p <- ggplot(data = data2, aes(x = data2[,20], y = data2[,21] )) +
        geom_point(aes(fill=party), colour = "black", alpha=0.8, shape = 21, size = 10)+
        scale_fill_manual(values = c("red", "darkolivegreen4", "darkorchid4", "lightgreen", "hotpink", "cyan1", "grey", "yellow", "orange", "darkblue"))+
        theme_minimal()
  p
  
  
  #Age plot
  p <- ggplot(data = data, aes(x = data[,20], y = data[,21] )) +
        geom_point(aes(color=age), size=10, alpha=0.5)+
        scale_color_gradient(low="green", high = "red")+
        theme_minimal()
  p
  
  
  library(ggfortify)
  autoplot(prcomp(data[,5:19]), loadings = TRUE, loadings.colour = 'blue',
           loadings.label = TRUE, loadings.label.size = 3)
  

  View(data)
  
  #Variance in responses
  
  resp.var <- data[,5:19] %>% 
              var() %>% 
              diag() %>% 
              sqrt() %>% 
              t()
            
  rownames(resp.var) <- "Standard Deviation"

  
  ## -- Add mean response for each party, for each question -- ##
  
  party.means <- data %>% 
            filter(party != 1) %>% 
            group_by(party) %>%
            summarize_each(funs(mean), -c(name, age, votes))

  ## --- Plot average response to each question, by party --- #
  
  # Construct labels
    labels <- data.frame(
      question = names(party.means[2:16]),
      position.y = 16:2+0.5,
      position.x = rep(3, 15)
    )
  
  
  # Build plot
  p <-  ggplot(data = party.means)
        
        #Loop over each question, and plot the party means
        for(i in 2:16){
          p <- p + 
            #Plot party means
            geom_point(aes_string(
            y = 18-i, 
            x = paste("party.means$", names(party.means)[i], sep = ""),
            fill = "party"
            ),
            colour = "black", alpha=0.8, shape = 21, size = 10
            )
        }
  
        #Format plot
        p <- p +
        
        #Add questions  
        geom_text(data = labels,
                  aes( y = position.y, x = position.x, label = question),
                  size = 3) +
        
        #Party colors  
        scale_fill_manual(values = c("red",             # Socialdemokratiet
                                     "darkolivegreen4", # Alternativet
                                     "darkorchid4",     # Radikale
                                     "lightgreen",      # Konservative
                                     "hotpink",         # SF
                                     "cyan1",           # Liberal Alliance
                                     "grey",            # Kristendemokraterne
                                     "yellow",          # Dansk Folkeparti
                                     "orange",          # Enhedslisten 
                                     "darkblue"),     # Venstre    
                          labels = c("S",             # Socialdemokratiet
                                     "Å", # Alternativet
                                     "R",     # Radikale
                                     "C",      # Konservative
                                     "SF",         # SF
                                     "Y",           # Liberal Alliance
                                     "K",            # Kristendemokraterne
                                     "O",          # Dansk Folkeparti
                                     "Ø",          # Enhedslisten 
                                     "V")) +     # Venstre    
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
