
library(ISLR)
library(grDevices)

########################
#   CHAPTER 2
########################

########
# 2.3.2

# Contour plots
x <- seq(-pi, pi, length=50)
y <- x
f <- outer(x, y, function(x,y) cos(y)/(1 + x^2))
contour(x, y, f)

contour(x, y, f, nlevels=45, add=T)

fa <- (f - t(f))/2
contour(x, y, fa, nlevels=15)

# Heatmap
image(x, y, fa)

# 3-D plots
persp(x, y, fa)
persp(x, y, fa, theta=30)
persp(x, y, fa, theta=30, phi=20)
persp(x, y, fa, theta=30, phi=70)
persp(x, y, fa, theta=30, phi=40)

########
# 2.3.5
attach(Auto)
head(Auto)
plot(horsepower, mpg)
identify(horsepower, mpg, name)

###############
# 2.4 Exercises

####
# 1.

# (a) : Flexible will be better than not flexible
# (b) : Inflexible is better
# (c) : Flexible is better
# (d) : Inflexible is better

####
# 2.

# (a) : Regression, inference, n=500, p=3
# (b) : Classification, prediction, n=20, p=13
# (c) : Regression, prediction, n=52, p=3

####
# 3.

# (a) and (b)
k <- seq(1, 20, 0.1)
variance <- exp(0.1*k)
bias_squared <- 2 - log(0.005*k)
training_error <- 0.5 - log(0.01*k)
test_error <- variance + bias_squared - 5.5

plot(k, variance, type="l", col="red", lwd=2)
lines(k, bias_squared, lwd=2)
lines(k, training_error, lwd=2, col="blue")
lines(k, test_error, col="gray", lwd=2)

legend(8, 7.7, bty="n", c("Variance", "Bias squared", "Training error", "Test error"),
       col=c("red", "black", "blue", "gray"), lwd=c(2, 2, 2, 2))

####
# 4.

# (a) : Spam/not spam setting, predict if a new email is spam or not spam; some predictors
#       could be the IP address, the email sender, some words in the text (such as money,
#       urgent, exclamation mark, etc...)
#       

####
# 5.








####
# 6.



####
# 7.

# (a)
euclidean_distance <- function(point_a_coord, point_b_coord){
  distance <- sqrt(sum( (point_a_coord - point_b_coord)^2 ))
  return(distance)
}

# a <- c(1, 2, 3)
# b <- c(0, 0, 0)
# euclidean_distance(a, b)

matrix_observations <- matrix(c(0, 2, 0, 0, -1, 1, 3, 0, 1, 1, 0, 1, 0, 0, 3, 2, 1, 1), nrow=6)
matrix_observations

x <- c(0, 0, 0)

my_distances <- rep(NA, nrow(matrix_observations))
for(row in 1:nrow(matrix_observations)){
  my_distances[row] <- euclidean_distance(matrix_observations[row, ], x)
}

my_distances

# (b)
# For k=1 the prediction is green because the distance is smallest

# (c)
# For k=3 we have a prediction red

# (d)
# Small k will perform better

####
# 8.

# (a)
college <- College
head(college)

# (b)
# RUN INTO PROBLEMS WITH X11 and XQuartz !!!!

# (c)
# i.
summary(college)
# ii.
pairs(college[, 1:10])
# iii.
boxplot(college$Outstate ~ college$Private, xlab="Private", ylab="Outstate")
# iv.
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(college$Elite)
boxplot(college$Outstate ~ college$Elite, xlab="Elite", ylab="Outstate")
# v.
par(mfrow=c(2, 2))
hist(college$Apps)
hist(college$Accept)
hist(college$Enroll)
hist(college$Outstate, breaks=20)
# vi.


####
# 9.
auto <- na.omit(Auto)
summary(auto)

# (a) : all are quantitative with the exception of name, origin and cylinders
table(auto$mpg)
table(auto$cylinders)
auto$cylinders <- as.factor(auto$cylinders)
table(auto$displacement)
table(auto$origin)

# (b)
range(auto$mpg)

# (c)
mean(auto$mpg)
sd(auto$mpg)

# (d)
new_auto <- auto[-c(10:85), ]
range(new_auto$mpg)
mean(new_auto$mpg)
sd(new_auto$mpg)

# (e)
pairs(auto)

# (f) : displacement, horsepower and weight seems potential good predictors

#######
# 10.

library(MASS)
?Boston
boston <- Boston

# (a) : rows=506, columns=14

# (b)
pairs(boston)

# (c)
cor(boston)

# (d)
summary(boston)

# (e)
boston_chas <- boston[boston$chas == 1, ]
nrow(boston_chas)

# (f)
median(boston$ptratio)

# (g)
which.min(boston$medv)
boston[399, ]
range(boston$crim)
range(boston$dis)
range(boston$black)

# (h)
boston_above_7_rooms <- boston[boston$rm > 7,]
boston_above_8_rooms <- boston[boston$rm > 8,]
head(boston_above_8_rooms)

#############################################################

library(ISLR)

########################
#   CHAPTER 3
########################











########################
#   CHAPTER 4
########################

library(ISLR)

# 4.6 Lab : Logistic regression, LDA, QDA, and KNN

# 4.6.1 The Stock Market data

head(Smarket)
names(Smarket)
dim(Smarket)
summary(Smarket)

cor(Smarket[, -9])

attach(Smarket)
plot(Volume)

# 4.6.2 Logistic regression

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket,
                family="binomial")
summary(glm.fits)

coef(glm.fits)
summary(glm.fits)$coef

glm.probs <- predict(glm.fits, type="response")
glm.probs[1:10]

contrasts(Direction)

glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > 0.5] <- "Up"
glm.pred

table(glm.pred, Direction)

# held out sample for test accuracy
train <- Year < 2005
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

glm.fits <- glm(Direction ~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                data = Smarket, family=binomial, subset=train)

glm.probs <- predict(glm.fits, Smarket.2005, type='response')

glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.2005)

# test error
1 - mean(glm.pred == Direction.2005)

# Removing higher lags

glm.fits <- glm(Direction ~Lag1 + Lag2,
                data = Smarket, family=binomial, subset=train)

glm.probs <- predict(glm.fits, Smarket.2005, type='response')

glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.2005)

1 - mean(glm.pred == Direction.2005)

# Prediction using a model
predict(glm.fits, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)), type='response')


# 4.6.3 LDA

library(MASS)

lda.fit <- lda(Direction ~ Lag1 + Lag2, data=Smarket, subset = train)
lda.fit

plot(lda.fit)

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class

table(lda.class, Direction.2005)
























































