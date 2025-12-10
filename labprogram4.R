library(tidyverse)
library(titanic)
library(dplyr)
library(caret)
library(ggcorrplot)

data <- titanic::titanic_train

data$Age[is.na(data$Age)] <- median(data$Age,na.rm=TRUE)

mode_embarked <- as.character(names(sort(table(data$Embarked),decreasing=TRUE))[1])
data$Embarked[is.na(data$Embarked)]<-mode_embarked

numeric_columns <- sapply(data,is.numeric)
z_scores <- as.data.frame(scale(data[,numeric_columns]))
outlier_condition <- apply(z_scores,1,function(row)any(abs(row)>3))

data_clean <- data[!outlier_condition,]

summary_before <- summary(data)
summary_after <- data_clean

correlation_matrix <- cor(data_clean[,numeric_columns],use="complete.obs")
write.csv(data_clean,"cleaned.csv",row.names=FALSE)

ggcorrplot(correlation_matrix,method="circle",lab=TRUE,title="Correlation Matrix")