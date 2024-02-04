#Spring24, Fundamentals of Machine Learning,
#Aleksandra Wasik, assignment 1
#Data source: https://www.kaggle.com/datasets/saketk511/travel-dataset-guide-to-indias-must-see-places

#import data
data <- read.csv('C:/Users/ola/Documents/Spring24_MachineLearning/archive1/Top Indian Places to Visit.csv', header =TRUE)

#statistics for numeric data
selected <- data$time.needed.to.visit.in.hrs
srednia <- mean(selected)
srednia

min_time <- min(selected)
min_time

max_time <- max(selected)
max_time

#median rating
selected1 <- data$Google.review.rating
mediana <- median(selected1)
mediana

#statistics for categorical data
selected3 <- data$Type
factor_selected3 <- factor(selected3)
factor_selected3
summary(factor_selected3)

#transform at least one variable
selected1
transforsel1 <- selected1 *2
transforsel1

zone <- order(data$Zone)  
data[zone, ]

# plot for one variable (histogram)
a <- data$Entrance.Fee.in.INR
max_a <- max(a)
max_a
hist(a, xlim = c(0, max_a))

#scatterplot
x <- selected1
y <- selected
plot(x, y,  
     xlab="rating", ylab="time needed to visit in hrs",
     col = "black")


