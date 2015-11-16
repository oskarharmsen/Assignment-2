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
  
  p <- ggplot(data = data, aes(x = data[,20], y = data[,21] )) +
        geom_point(aes(color=party), size=10, alpha=0.8)+
        scale_colour_manual(values = c("black", "red", "darkolivegreen4", "darkorchid4", "lightgreen", "hotpink", "cyan1", "grey", "yellow", "orange", "darkblue"))+
        theme_minimal()
  p
  
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
  