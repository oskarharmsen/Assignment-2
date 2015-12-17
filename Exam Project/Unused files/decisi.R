
 # installing packages:

install.packages("rpart")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("party")
install.packages("partykit")
install.packages("caret")


# loading packages:
library("rpart")
library("rattle")
library("rpart.plot")
library("RColorBrewer")
library("party")
library("partykit")
library("caret")

library("dplyr")

# Class models: the probability of the fitted class.
prp(model, extra = 8,  box.col = "lightblue", border.col = "darkblue", shadow.col = "lightgrey", split.cex = 0.7,split.font = 4, split.col = "darkblue", split.border.col = 9, split.shadow.col = "lightgrey", nn.col = "darkred")

summary(model)


# make a list of votes

votes = data %>%
  group_by(party) %>%
  summarise( n = sum(votes.pers))
a = predict(model)

# dec 

set.seed(1)

# separate into training and test data
train <- sample( x = 1:nrow(data), size = 2/3 * nrow(data), replace = FALSE)
data.train <- data[train, ]
data.test <- data[-train,]

# Fit decision tree
model = rpart(party ~ ., data = data.train[,c(2,17:31)], method = "class")


partychoice = predict(model, newdata = data[,c(2,17:31)], type = "class")

data_pre = data.frame(data, partychoice)

data_pre <- mutate(data_pre, homogen = ifelse(party == partychoice, 1, 0))  


summmer = data_pre %>%
  group_by(party) %>%
  summarise(votes = mean(votes.pers), homogen = sum(homogen), candidates = n(), pre = sum(ran.last.election)) %>%
  mutate(pct_homo = (homogen / candidates * 100), pct_last = (pre / candidates * 100)) %>%
  filter(party != 1)
arrange(desc(votes))
summmer
library(ggplot2)
p = ggplot(summmer, aes(x = pct_homo, y = votes))
p = p + geom_bar(stat = "identity", aes(fill = party))
p