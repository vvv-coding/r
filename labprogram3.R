library(dplyr)
library(ggplot2)
library(moments)
library(palmerpenguins)

data(iris)
data(penguins)

calc_mode <- function(x){
  return(as.numeric(names(sort(table(x),decreasing=TRUE))[1]))
}

iris_mean <- sapply(iris[,1:4],mean,na.rm=TRUE)
iris_variance <- sapply(iris[,1:4],var,na.rm=TRUE)
iris_mode <- sapply(iris[,1:4],calc_mode)
iris_std <- sapply(iris[,1:4],sd,na.rm=TRUE)
iris_median <- sapply(iris[,1:4],median,na.rm=TRUE)
iris_skewness <- sapply(iris[,1:4],skewness,na.rm=TRUE)
iris_kurtosis <- sapply(iris[,1:4],kurtosis,na.rm=TRUE)

setosa <- subset(iris,Species == "setosa")$Sepal.Length
versicolor <- subset(iris,Species == "versicolor")$Sepal.Length
t_test <- t.test(setosa,versicolor)

ggplot(iris,aes(x=Species,y=Sepal.Length,fill=Species))+
  geom_boxplot()+
  ggtitle("Boxplot of Sepal length by Species")

ggplot(iris,aes(x=Sepal.Length))+
  geom_histogram(binwidth=0.3,fill="blue",color="black")+
  ggtitle("Histogram of Sepal Length by Species")

penguin_clean <- na.omit(penguins)
penguin_mean <- sapply(penguin_clean[,3:6],mean,na.rm=TRUE)
penguin_variance <- sapply(penguin_clean[,3:6],var,na.rm=TRUE)
penguin_mode <- sapply(penguin_clean[,3:6],calc_mode)
penguin_std <- sapply(penguin_clean[,3:6],sd,na.rm=TRUE)
penguin_median <- sapply(penguin_clean[,3:6],median,na.rm=TRUE)
penguin_skewness <- sapply(penguin_clean[,3:6],skewness,na.rm=TRUE)
penguin_kurtosis <- sapply(penguin_clean[,3:6],kurtosis,na.rm=TRUE)

adelie <- subset(penguin_clean,species == "Adelie")$flipper_length_mm
gentoo <- subset(penguin_clean,species == "Gentoo")$flipper_length_mm
t_test <- t.test(adelie,gentoo)

ggplot(penguin_clean,aes(x=species,y=flipper_length_mm),fill=Species)+
  geom_point()+
  ggtitle("Boxplot of Flipper length by Species")

ggplot(penguin_clean,aes(x=flipper_length_mm))+
  geom_histogram(binwidth=0.3,fill="blue",color="black")+
  ggtitle("Histogram of Flipper Length by Species")