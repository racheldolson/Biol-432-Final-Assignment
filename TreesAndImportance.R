## Loading and checking data
  
Data <- read.csv("data.csv")
str(Data)
head(data)
dim(data)
library(BiocManager)
library(Biostrings)
library(reshape2)
library(rentrez)
library(annotate)
library(ape)
library(ggtree)
library(ggplot2)
library(dplyr)
library(tree)
library(randomForest)
library(gbm)
library(vegan)
library(rpart)
library(rpart.plot)
library(caret)

data<-Data %>% 
  mutate(diagnosis = as.factor(diagnosis)) #Changing the diagnosis column from a character to a factor

diagData<-data %>% 
  select(-c("id","X")) #Removing the id and X columns as they are not needed for our analysis

## Making regression tree

DiagTree<-tree(diagnosis ~ ., data=diagData, method = "class")
plot(DiagTree)
text(DiagTree, cex=0.6, adj=0)

DiagTreeRpart<-rpart(diagnosis ~ ., data=diagData, method = "class")
rpart.plot(DiagTreeRpart, type = 2, extra = 104, tweak = 1.2, fallen.leaves=TRUE)

predictions<-predict(DiagTreeRpart, diagData, type="class")
confusionMatrix(predictions, diagData$diagnosis)

## Variable importance

importance <- as.data.frame(DiagTreeRpart$variable.importance)
print(importance)

par(mar = c(7, 4, 2, 2))
plot(DiagTreeRpart$variable.importance, xlab=" ", 
     ylab="Importance", xaxt = "n", pch=20)
axis(1, at=1:16, labels=row.names(importance), las=2, cex.axis = 0.7)
