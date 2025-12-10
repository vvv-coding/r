library(MASS)
library(ggplot2)
library(caret)
library(car)
library(pROC)
library(dplyr)
library(corrplot)

data("Boston")
head(Boston)
sum(is.na(Boston))

summary(Boston)

boxplot(Boston$medv,main="Boxplot of Median Values")
Boston <- Boston%>%filter(medv<50)

corr_matrix <- cor(Boston)
corrplot(corr_matrix,method="circle")

simple_model <- lm(medv~lstat,data=Boston)
summary(simple_model)

multiple_model <- lm(medv~lstat*rm,data=Boston)
summary(multiple_model)

adjusted_R2 <- summary(multiple_model)$adj.v.squared
AIC_value <- AIC(multiple_model)
BIC_value <- BIC(multiple_model)
cat('Adjusted R2:',adjusted_R2,"\tAIC value:",AIC_value)
cat("AIC value:",AIC_value,"\tBIC value:",BIC_value)

plot(multiple_model,which=1,main="Residual vs Fitted Plot")
plot(multiple_model,which=2,main="Nomal Q-Q Plot")

set.seed(42)
train_control <- trainControl(method="cv",number=10)
cv_model <- train(medv~lstat*rm,data=Boston,method="lm",trControl=train_control)
print(cv_model)

Boston$medv_class <- ifelse(Boston$medv>=25,1,0)
logistic_model <- glm(medv_class~lstat*rm,data=Boston,family="binomial")
pred_prob <- predict(logistic_model,type="response")
roc_curve <- roc(Boston$medv_class,pred_prob)

plot(roc_curve,main="ROC Curve of Logistic Regression",col="blue")
abline(a=0,b=1,lty=2,col="red")
cat("AUC:",auc(roc_curve))