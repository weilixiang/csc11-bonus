options(warn=-1)
library(ggplot2)
library(nnet)
library(ROCR)
train <- read.csv("../input/train.csv", header=TRUE, stringsAsFactors = FALSE)
# Set Survived, Pclass, Sex, Embarked as factors
for(i in c(2,3,5, 12)){
  train[,i] <- as.factor(train[,i])
}


#Brute NA handling... Deletion of all rows with a NA
#It mainly concerns Age variable
for (i in 1:dim(train)[2])
{
  train <- train[!is.na(train[,i]),]
}
nn <- nnet(Survived ~ Age + Fare + Embarked+ SibSp  + Parch, train, size = 100)  
nn.prediction <- predict(nn, newdata = train)
pred <- prediction(nn.prediction, train$Survived)
tpr <- unlist(performance(pred, "tpr")@y.values)
fpr <- unlist(performance(pred, "fpr")@y.values)

plot(fpr, tpr, main="Auroc Curve", xlab="False Positive Rare", ylab="True Positive Rate", lt=1)
train$nn.error = (train$Survived != ifelse(nn.prediction >.4,1,0))
evels(train$Survived) <- c("Did not Survive", "Survived")

ggplot(train, aes(x=Fare, y=Pclass, colour=nn.error)) + 
  geom_point(shape=1, position = "jitter") + facet_grid(Sex~Survived) + 
  scale_colour_manual(values=c("green", "red")) + 
  ggtitle("Presentation of the performance of the model \n red dot=ERROR") + 
  ylab("Passenger Class") + 
  xlab("Fare") 
