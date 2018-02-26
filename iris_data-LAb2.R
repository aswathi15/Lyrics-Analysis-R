#Data set
iris1 <- iris
head(iris1)
tail(iris1)
str(iris1)
summary(iris1)

#Data preparation
#We only need Sepal width and Petal width as our features
#We need to drop the Sepal Length and Petal Length from our 

iris1$Sepal.Length <- NULL
iris1$Petal.Length <- NULL

#Even in the Species columns, we only want "Setosa" and "Versicolor"

iris_new <- iris1[iris1$Species == "setosa" |iris1$Species == "versicolor",]

#Change the column names
colnames(iris_new) <- c("Sepal","Petal","Label")
head(iris_new)

nrow(iris_new)
ncol(iris_new)

library(ggplot2)
#Scatter plot
a <- ggplot(data=iris_new,aes(x = Sepal,y = Petal))
a + geom_point(aes(color = Label))
#Box plot
b <- ggplot(data=iris_new,aes(x = Label,y=Petal))
b + geom_jitter() + geom_boxplot(aes(fill = Label),color = "Black",alpha = 0.5)
#Histogram
c <- ggplot(data=iris_new,aes(x = Sepal))
c + geom_histogram(binwidth=0.1,aes(fill = Label),color="Black")

iris_new[iris_new$Sepal > 3 & iris_new$Petal > 0.4,]

#---------------LAB 2
levels(iris_new$Label)[1] <- 0
levels(iris_new$Label)
levels(iris_new$Label)[2] <- 1
levels(iris_new$Label)

head(iris_new)

write.csv(iris_new,"iris_new.csv")

A <- read.csv("iris_new.csv")
B <- read.csv("iris_new.csv",header=TRUE, sep=",",)

A$X <- NULL
head(A)
nrow(A)
ncol(A)

?sample()

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
#set.seed(123)
split = sample.split(A$Label, SplitRatio = 0.8)
training_set = subset(A, split == TRUE)
test_set = subset(A, split == FALSE)

split
# Fitting Multiple Linear Regression to the dataset A
regressor = lm(formula = Label~ Sepal+Petal,data=A)
summary(regressor)

#Fitting Multiple Linear Regression to the Training set
regressor_1 = lm(formula = Label ~ Sepal+Petal,data=training_set)
summary(regressor_1)

#Linear equation model on the Training set
y_hat = 0.22105 - 0.44259*test_set$Sepal + 1.47443*test_set$Petal

# Predicting the Test set results, got the same result using the predict function
y_pred = predict(regressor_1,newdata = test_set)
print(y_pred)
#head(test_set)
#test_set$Label - y_pred
RSS <- sum((test_set$Label - y_pred) ^2)
print(RSS)

#Changing the intercept value to a random value 0.890
y_hat_1 = 0.890 - 0.47610*test_set$Sepal + 1.49323*test_set$Petal
RSS_1 <- sum((test_set$Label - y_hat_1) ^2)
print(RSS_1)

y_hat_2 = 0.0001 - 0.47610*test_set$Sepal + 1.49323*test_set$Petal
RSS_2 <- sum((test_set$Label - y_hat_2) ^2)
print(RSS_2)

y_pred_train = predict(regressor_1,newdata = training_set)
RSS_train <- sum((training_set$Label - y_pred_train) ^2)
print(RSS_train)

#-----------------Logistic Regression 

class(training_set$Label)
training_set$Label <- as.factor(training_set$Label)

classifier = glm(formula = Label~Petal+Sepal,data=training_set,family = binomial(link = logit))
summary(classifier)

test_set$Label

y_hat_logistic = -2.961 + 73.957*test_set$Petal -18.193*test_set$Sepal

prob_pred <- predict(classifier,type="response",newdata = test_set)
prob_pred
y_pred_logistic = ifelse(prob_pred > 0.5,1,-1)
y_pred_logistic
test_set$Label

prob_pred_train <- predict(classifier,type="response",newdata = training_set)
prob_pred_train
y_pred_logistic_train <- ifelse(prob_pred_train > 0.5,1,-1)
y_pred_logistic_train
