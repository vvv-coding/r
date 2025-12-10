set.seed(42)
random_vector <- runif(20,min=1,max=100)
cat("Original vector:",random_vector)

sorted_vector <- sort(random_vector)
cat("Sorted vector:",sorted_vector)

search_value <- 50
is_value_present <- any(random_vector == search_value)
cat("Is",search_value,"present?",is_value_present)

values_greater_than_60 <- random_vector[random_vector>60]
cat("Values greater than 60",values_greater_than_60)

matrix_from_vector <- matrix(random_vector,nrow=4,ncol=5)
print(matrix_from_vector)

matrix_transpose <- t(matrix_from_vector)
matrix_multiplication <- matrix_from_vector %*% matrix_transpose
print(matrix_multiplication)

elementwise_multiplication <- matrix_from_vector*matrix_from_vector
print(elementwise_multiplication)

mylist <- list(numbers = random_vector,
               characters = c("A","B","C","D"),
               logical_values = c(TRUE,FALSE,TRUE),
               matrix = matrix_from_vector)
print(mylist)
subest_numeric <- mylist$numbers
subset_logical <- mylist$logical
mylist$character[2] <- "Z"
squared_numbers <- mylist$numbers^2

df <- data.frame(
  ID = 1:20,
  Age = sample(18:65,20,replace=TRUE),
  Score = runif(20,min=50,max=100),
  Passed = sample(c(TRUE,FALSE),20,replace=TRUE)
)

filtered_df <- subset(df,Age>30&Score<70)

mean_age <- mean(df$Age)
sum_age <- sum(df$Age)
var_age <- var(df$Age)

var_score <- mean(df$Score)
sum_score <- sum(df$Score)
var_score <- var(df$Score)

df$Score[sample(1:20,5)] <- NA
df$Score[is.na(df$Score)] <- mean(df$Score,na.rm=TRUE)

library(dplyr)
grouped_stats <- df%>%
  group_by(Passed)%>%
  summarise(
    mean_score = mean(Score,na.rm=TRUE),
    mean_age = mean(Age)
  )