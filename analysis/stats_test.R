library(tidyverse)
library(ggplot2)
library(e1071)
library(grid)
library("gridExtra")
library(car)
library(repr)
library(FSA)
library(rcompanion)
# install.packages('rcompanion')
# input data
big_all <- read.csv(file = './combine_data/big_app_web.csv')
sma_all <- read.csv(file = './combine_data/small_app_web.csv')
long_all <- read.csv(file = './combine_data/long_app_web.csv')
g4_all <- read.csv(file = './combine_data/4g_app_web.csv')

# RQ1 test
library(MASS)
wilcox.test(big_all$web, big_all$app, paired=FALSE)
wilcox.test(sma_all$web, sma_all$app, paired=FALSE) 

kruskal.test(big_all$web, big_all$app)
kruskal.test(sma_all$web, sma_all$app)

# RQ2.1 test
## build needed data frame
### extract big_all (since these experiment were done near WiFi)
rq2sweb <- big_all$web
rq2sapp <- big_all$app
rq2sweb <- cbind(rq2sweb, platform='web', distance='close')
rq2sapp <- cbind(rq2sapp, platform='android', distance='close')
colnames(rq2sweb) <- c("value", "platform", "distance")
colnames(rq2sapp) <- c("value", "platform", "distance")
### extract long_all data, which represent long distance from WiFi
rq2dweb <- long_all$web
rq2dapp <- long_all$app
rq2dweb <- cbind(rq2dweb, platform='web', distance='far')
rq2dapp <- cbind(rq2dapp, platform='android', distance='far')
colnames(rq2dweb) <- c("value", "platform", "distance")
colnames(rq2dapp) <- c("value", "platform", "distance")
### create df for test
df <- data.frame(rbind(rq2sweb, rq2sapp, rq2dweb, rq2dapp))
### transform the data type in each col
df$value <- as.numeric(df$value)
df$platform <- as.factor(df$platform)
df$distance <- as.factor(df$distance)
### check data type
str(df)
head(df)
sapply(df,class)
### Scheirer–Ray–Hare Test
scheirerRayHare(value ~ platform + distance,
                data=df)


# RQ2.2
### extract big_all (since these experiment were done by using WiFi)
rq2wweb <- big_all$web
rq2wapp <- big_all$app
rq2wweb <- cbind(rq2wweb, platform='web', signal='wifi')
rq2wapp <- cbind(rq2wapp, platform='android', signal='wifi')
colnames(rq2wweb) <- c("value", "platform", "signal")
colnames(rq2wapp) <- c("value", "platform", "signal")
### extract g4_all data, which represent simulated 4G signal
rq2gweb <- g4_all$web
rq2gapp <- g4_all$app
rq2gweb <- cbind(rq2gweb, platform='web', signal='4g')
rq2gapp <- cbind(rq2gapp, platform='android', signal='4g')
colnames(rq2gweb) <- c("value", "platform", "signal")
colnames(rq2gapp) <- c("value", "platform", "signal")
### create df for test
df2_2 <- data.frame(rbind(rq2wweb, rq2wapp, rq2gweb, rq2gapp))
head(df2_2)
### transform the data type in each col
df2_2$value <- as.numeric(df2_2$value)
df2_2$platform <- as.factor(df2_2$platform)
df2_2$signal <- as.factor(df2_2$signal)
### check data type
str(df2_2)
head(df2_2)
sapply(df2_2,class)


scheirerRayHare(value ~ platform + signal,
                data=df2_2)
