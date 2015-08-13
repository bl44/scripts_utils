---
title: "Template for Predictive Modeling"
author: "Baolei"
date: "August 12, 2015"
output: html_document
---

## Executive Summary


## The Original Questions
> cite the original question here
> can be multiple times


## Data Cleaning
```{r}
# good packages to have
# data maniputlation dplyr
library(dplyr)
# ggplot2 for data visualization 
library(ggplot2)


# for evaluation 

```

## Expltory Data Analysis
 will use dplyr for the data rangling
 ggplot2 for visualization
 

## Modeling

```{r}
# Load the library caTools
library(caTools)
# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(u_data$active, SplitRatio = 0.60)
# Split up the data using subset
train = subset(u_data, split==TRUE)
test = subset(u_data, split==FALSE)
# Logistic Regression Model
uberLog = glm(active ~ ., data = train, family=binomial)
summary(uberLog)
```

```{r}
# Predictions on the test set
predictTest = predict(uberLog, type="response", newdata=test)
# Confusion matrix with threshold of 0.5
table(test$active, predictTest > 0.5)
# Accuracy
(10650+3809)/(10650+3809+1828+3713)
# Baseline accuracy
(10650+1828)/(10650+3809+1828+3713)
# Test set AUC 
library(ROCR)
predROCR = prediction(predictTest, test$active)
as.numeric(performance(predROCR, "auc")@y.values)
#plot the ROC curve
perfROCR = performance(predROCR,'tpr','fpr')
plot(perfROCR,colorize=T,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.6))
```

### Tree Model

```{r}
library(rpart)
library(rpart.plot)
# CART model
uberTree = rpart(active ~ ., data = train, method="class", minbucket=20)
prp(uberTree)
# Make predictions
predictTree = predict(uberTree, newdata = test, type = "class")
table(test$active, predictTree)
# accuracy
(10827+4232)/(20000)
# ROC curve
treeROC = predict(uberTree, newdata = test)
predTree = prediction(treeROC[,2], test$active)
perfTree = performance(predTree, "tpr", "fpr")
plot(perfTree,colorize=T,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.6))
#AUC
as.numeric(performance(predTree, "auc")@y.values)
```

### Random Forests

```{r}
library(randomForest)
# Build random forest model
train$active = as.factor(train$active)
test$active = as.factor(test$active)
uberForest = randomForest(active ~ ., data=train, type='class', ntree=200, nodesize=25 )
# Make predictions
predictForest = predict(uberForest, newdata = test)
table(test$active, predictForest)
(10836+4855)/20000
## Model Evaluation

## Business Implications



-------
# Appendix

R commend Reference [R Commend cheatsheets](https://www.rstudio.com/resources/cheatsheets/)

## dplyr functionality

* Five basic verbs: `filter`, `select`, `arrange`, `mutate`, `summarise` (plus `group_by`)
* Can work with data stored in databases and [data tables](http://datatable.r-forge.r-project.org/)
* Joins: inner join, left join, semi-join, anti-join (not covered below)
* Window functions for calculating ranking, offsets, and more
* [Better than plyr](http://blog.rstudio.org/2014/01/17/introducing-dplyr/) if you're only working with data frames (though it doesn't yet duplicate all of the plyr functionality)
* Examples below are based upon the [latest release](https://github.com/hadley/dplyr/releases), version 0.2 (released May 2014)


## Loading dplyr and an example dataset

* dplyr will mask a few base functions
* If you also use plyr, load plyr first
* hflights is flights departing from two Houston airports in 2011

```{r}
# load packages
suppressMessages(library(dplyr))
library(hflights)

# explore data
data(hflights)
head(hflights)
```

* `tbl_df` creates a "local data frame"
* Local data frame is simply a wrapper for a data frame that prints nicely

```{r}
# convert to local data frame
flights <- tbl_df(hflights)

# printing only shows 10 rows and as many columns as can fit on your screen
flights
```

## ggplot2 example
```{r}
# ggplot(data=mpg, aes(x=cty, y=hwy)) + 
#     geom_point(aes(color=cyl),alpha=I(0.5), size=5) + 
#     geom_smooth(method='lm') + 
#     coord_cartesian() + 
#     scale_color_gradient() + 
#     theme_bw()
## stats graph
a<-ggplot(mpg,aes(hwy))
a+geom_area(stat='bin')
a+geom_density(kernel='gaussian')
a+geom_dotplot(size = 0.2)
a+geom_freqpoly()
a+geom_histogram(binwidth=3, aes(fill=fl),alpha=0.5)
b<-ggplot(mpg, aes(x=fl))
b + geom_bar(fill = 'blue', alpha=0.3) + title(main='this is the best plot', xlab ='flower') 
```

## continus x contiu y
```{r}
f<-ggplot(mpg, aes(x=cty,y=hwy))
f + geom_blank()
f + geom_jitter(aes(color = cyl))
f + geom_point(aes(color = cyl)) + geom_quantile()
f + geom_rug(sides='bl')
f + geom_smooth(method='lm')
f + geom_text(aes(label=cty))
```

## Discrete x, continuos Y
```{r}
g <- ggplot(mpg,aes(class,hwy))
g+geom_bar(stat='identity')
g+geom_boxplot()
#dotplot
g+geom_dotplot(binaxis = 'y', stackdir = 'center',aes(fill = class),alpha=I(0.4))
g+geom_violin(scale = 'area',fill=grey(0.5)) 
```

## stat plot

```{r}
g+stat_boxplot(coef = 1.5)
g+stat_ydensity(adjust = 1, kernel='gaussian', scale = 'area')
f+stat_ecdf(n=40)
f+geom_point() + stat_quantile(quantile = c(0.25,0.5,0.75), formula = y ~ log(x), method = 'rq')
```

## general purpose
```{r}
ggplot() + stat_function(aes(x=-3:3), fun=dnorm, n= 101, args = list(sd=0.5))
f+ stat_sum()
f+ stat_summary(fun.data='mean_cl_boot')
f+stat_unique()
f+stat_sum()
```

## other 
```{r}
library(maps)
library(ggmap)
chicago = get_map(location='chicago',zoom = 11)
ggmap(chicago)
ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x = Longitude, y=Latitude))
mvt = read.csv('/Users/bli/Downloads/mvt.csv')
ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x = Longitude, y=Latitude))
ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x = Longitude, y=Latitude))
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2),round(mvt$Latitude,2)))
str(LatLonCounts)
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long,y=Lat,color=Freq, size = Freq))
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long,y=Lat,color=Freq, size = Freq)) + scale_color_gradient(low='yellow', high='red')
ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x=Long,y=Lat,alpha=Freq), fill = 'red')
LatLonCounts2 = subset(LatLonCounts, Freq>0)
ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x=Long,y=Lat,alpha=Freq), fill = 'red') 
dim(LatLonCounts)
dim(LatLonCounts2)
murders = read.csv('/Users/bli/Downloads/murders.csv')
statesMap = map_data('state')
str(statesMap)
ggplot(statesMap,aes(x=long,y=lat,group=group))+geom_polygon(fill='white',color = 'red')
murders$region = tolower(murders$State)
murderMap = merge(statesMap, murders, by='region')
ggplot(murderMap,aes(x=long,y=lat, group=group, fill = Murders)) + geom_polygon(color='black') + scale_fill_gradient(low = 'black', high='red', guide = 'legend')
ggplot(murderMap,aes(x=long,y=lat, group=group, fill = Population)) + geom_polygon(color='black') + scale_fill_gradient(low = 'black', high='red', guide = 'legend')
murderMap$MurderRate = murderMap$Murders/murderMap$Population * 1000
ggplot(murderMap,aes(x=long,y=lat, group=group, fill = MurderRate)) + geom_polygon(color='black') + scale_fill_gradient(low = 'black', high='red', guide = 'legend')
ggplot(murderMap,aes(x=long,y=lat, group=group, fill = GunOwnership)) + geom_polygon(color='black') + scale_fill_gradient(low = 'black', high='red', guide = 'legend')
```