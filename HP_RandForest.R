library(randomForest)


train <- read.csv("train.csv",header=TRUE,stringsAsFactors=FALSE)
test <- read.csv("test.csv",header=TRUE,stringsAsFactors=FALSE)
submission <- read.csv("sample_submission.csv",header=TRUE,stringsAsFactors=FALSE)

# Deal with missing values
variables <- names(train)
num_var<-names(data.frame(train[which(sapply(train,is.numeric))]))
cat_var<-names(data.frame(train[which(sapply(train,is.character))]))
num_var = num_var[num_var!='SalePrice']

for(i in num_var)
{
  if(any(is.na(train[[i]])))
  {
    train[[i]][is.na(train[[i]])] <- mean(train[[i]],na.rm=TRUE)
    }
  if(any(is.na(test[[i]]))){
      test[[i]][is.na(test[[i]])] <- mean(test[[i]],na.rm=TRUE)
    }
}


for(i in cat_var)
{
  if(any(is.na(test[[i]]))){
    test[[i]][is.na(test[[i]])] <- "MISSING_VALUE"  
  }
  if(any(is.na(train[[i]]))){
    train[[i]][is.na(train[[i]])] <- "MISSING_VALUE"
  }
  
}

# for category variable, us it as factor
for(i in cat_var)
{
    levels <- sort(unique(c(train[[i]],test[[i]])))
    train[[i]] <- factor(train[[i]],levels=levels)
    test[[i]] <- factor(test[[i]],levels=levels)
}

Randf <- randomForest(SalePrice~.,train)

submission$SalePrice <- predict(Randf,test)

write.csv(submission,file="submission.csv",row.names=FALSE)
