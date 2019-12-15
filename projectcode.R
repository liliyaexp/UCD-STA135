## STA135 Project - Liya Li
rm(list = ls())
library(devtools)
#remotes::install_github('vqv/ggbiplot', force = T)
library(ellipse)
library(scatterplot3d)
library(psych)
library(MASS)
library(ggbiplot)
library(ICSNP) # for Hotelling's T2
library(C50)

data(iris)
attach(iris)
head(iris) 

######## summarize dataset
str(iris) # there are 5 variables in this dataset, dimension 5*150
table(iris$Species)
levels(iris$Species) # levels of the class attribute
# multi-class classification

# visualize data
# pairs scatterplot
pairs(iris[,1:4], col=iris[,5], oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.9,0.2, as.vector(unique(iris$Species)), fill=c(1,2,3))
# plot(iris) won't show colors

# 3d scatterplot
par(mfrow = c(2, 2))
mar0 <- c(2.5, 3, 2.5, 3)
scatterplot3d(iris[,1], iris[,2], iris[,3], mar = mar0, color = c("black","red", "green")[iris$Species], pch = 19)
scatterplot3d(iris[,2], iris[,3], iris[,4], mar = mar0, color = c("black","red", "green")[iris$Species], pch = 19)
scatterplot3d(iris[,3], iris[,4], iris[,1], mar = mar0, color = c("black","red", "green")[iris$Species], pch = 19)
scatterplot3d(iris[,4], iris[,1], iris[,2], mar = mar0, color = c("black","red", "green")[iris$Species], pch = 19)

#ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, colour = Species)) + 
#  geom_point() +
#  ggtitle('Iris Species by Petal and Sepal Length')
# setosa seems very different than the others in terms of length
# versicolor and virginia seems more familier at some points but still hard to classify

#ggplot(iris, aes(x = Petal.Width, y = Sepal.Width, colour = Species)) + 
#  geom_point() +
#  ggtitle('Iris Species by Petal and Sepal Width')

# the summary statistics for iris dataset and boxplot for it
summary(iris)
par(mfrow = c(1, 1))
par(mar=c(7,4,1,1)) # more space for labeling
boxplot(iris[,-5],las=2) # rough estimate of the distribution of the values for each variable

corr <- cor(iris[,1:4]) # correlation
round(corr,3)
# eg: Petal width, length highly correlated because correlation coefficient is 0.963 -> 92.16% of the variation is related
# eg: Sepal width, length not so correlated, -0.118, "inverse correlation"


######## analyze data

Setosa <- iris[iris$Species == "setosa",-5]
Versicolor <- iris[iris$Species == "versicolor",-5]
Virginica <- iris[iris$Species == "virginica", -5]


### Two-sample Hotelling’s T2 test
### (partial code adopted from discussion_7.R and credited to Weiping Zhang(USTC))

# since versicolor and virginica are not easy to classify
# we are interested in testing whether they are significantly different

# start with testing whether their population mean flower measurements are the same
# that is, whether the average petal and sepal dimensions are the same
# assume they have the same variance matrix
alpha <- 0.05
HotellingsT2(Versicolor, Virginica)
# F-statistics: 86.148 (shown as T.2 because it's an F transformation of computed T2)
# pval < 2.2e-16 < 0.05: so reject null

# same test but look at test statistics
n <- c(nrow(Versicolor), nrow(Virginica))
p <- 4
versimean <- colMeans(Versicolor)
virgimean <- colMeans(Virginica)
d <- versimean - virgimean
S1 <- var(Versicolor)
S2 <- var(Virginica)
Sp <- ((n[1]-1)*S1+(n[2]-1)*S2)/(sum(n)-2)
t2 <- t(d)%*%solve(sum(1/n)*Sp)%*%d
t2   # 355.4721
cval <- (sum(n)-2)*p/(sum(n)-p-1)*qf(1-alpha,p,sum(n)-p-1)
cval # 10.18166
# T2 > critical value: so reject null

# Thus, the population mean flower measurements for versicolor and virginica are all not the same
# and we want to find the simultaneous C.I for them



### Simultaneous confidence intervals based on T2 and Bonferroni correction
### (partial code adopted from Discussion_7.R and credited to Weiping Zhang(USTC))

# simultaneous confidence intervals
wd <- sqrt(((n[1]+n[2]-2)*p/(n[1]+n[2]-p-1))*qf(1-alpha,p,n[1]+n[2]-p-1))*sqrt(diag(Sp)*sum(1/n))
Cis <- cbind(d-wd, d+wd)
cat("95% simultaneous confidence interval","\n")
Cis


#Bonferroni simultaneous confidence intervals
wd.b <- qt(1-alpha/(2*p),n[1]+n[2]-2) *sqrt(diag(Sp)*sum(1/n))
Cis.b <- cbind(d-wd.b, d+wd.b)
cat("95% Bonferroni simultaneous confidence interval","\n")
Cis.b



### Principal component analysis
### (partial code adopted from Discussion_9.R)
# then we apply PCA to reduce dimentionality of the orthogonal transformation on original data
# to convert the correlated variables into a set of values of linearly uncorrelated variables called PC. 

flowers <- iris[,1:4]
classid <- iris[,5]
flowers.pca<- princomp(~., data=flowers)

plot(1:(length(flowers.pca$sdev)),  (flowers.pca$sdev)^2, type='b',
     main="Scree Plot", xlab="Number of Components", ylab="Eigenvalue Size")

summary(flowers.pca, loadings=TRUE)   # coefficient of components

plot(flowers.pca$scores[,1], flowers.pca$scores[,2], xlab="PC1", ylab="PC2", pch=rep(1:3,each=50), col=classid, main="Iris data")
legend("topright", legend=levels(classid), pch=1:3, col=1:3, cex=0.7)

ggbiplot(flowers.pca, ellipse=TRUE, groups=classid)



### Linear discriminant analysis
### (code adopted from Discussion_10.R)
iris.lda <- lda(Species~.,data=iris)
iris.lda
iris.pred <- predict(iris.lda)
iris$PredictSpecies <- iris.pred$class
iris[iris$PredictSpecies!=iris$Species,] # predicted results that are diff to original

# Confusion matrix
table(iris$Species,iris.pred$class)

# A Stacked Histogram of the LDA Values
ldahist(data = iris.pred$x[,1], g=iris$Species)



### Another model
### Interesting step: create a decision tree to predict species 
model <- C5.0(flowers, classid, control = C5.0Control(noGlobalPruning = TRUE, minCases=1))
model
plot(model, main="Iris Decision Tree")
# but there might be overfitting using Decision Tree model 







