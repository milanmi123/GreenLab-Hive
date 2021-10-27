library(tidyverse)
library(ggplot2)
library(e1071)
library(grid)
library("gridExtra")
library(car)
library(repr)
library(FSA)
library(rcompanion)
library(effsize)
#install.packages('effsize')
if(!require(ARTool)){install.packages("ARTool")}
if(!require(emmeans)){install.packages("emmeans")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(rcompanion)){install.packages("rcompanion ")}
if(!require(ggplot2)){install.packages("ggplot2")}
# input data
big_all <- read.csv(file = './combine_data/big_app_web.csv')
sma_all <- read.csv(file = './combine_data/small_app_web.csv')
long_all <- read.csv(file = './combine_data/long_app_web.csv')
g4_all <- read.csv(file = './combine_data/4g_app_web.csv')

sma_all_m <- read.csv(file = './combine_data/small_app_web_modify.csv')
# RQ1 test
library(MASS)
wilcox.test(big_all$web, big_all$app, paired=FALSE)
wilcox.test(sma_all_m$web, sma_all_m$app, paired=FALSE) 

kruskal.test(big_all$web, big_all$app)
kruskal.test(sma_all$web, sma_all$app)

sapply(big_all, class)
sapply(sma_all, class)
## try to do statistical test in total 
total_rq1 <- data.frame(rbind(big_all,sma_all))
sapply(total_rq1, class)
wilcox.test(total_rq1$web, total_rq1$app, paired = FALSE)
kruskal.test(total_rq1$web, total_rq1$app)

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
df$platform = factor(df$platform,
                       levels=c("android", "web"))
### check ART anova
library(ARTool)
model = art(value ~ platform + distance + platform:distance,
            data = df)
anova(model)
### post hoc comparison for interactions in a two-way model
library(emmeans)
model.int = artlm(model, "platform:distance")
marginal = emmeans(model.int, ~ platform:distance)
contrast(marginal, method="pairwise", adjust="none")

### interaction plot
with(df, interaction.plot(platform, distance, value, fun = mean,
                             main = "Interaction Plot"))
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

### Scheirer–Ray–Hare Test
scheirerRayHare(value ~ platform + signal,
                data=df2_2)
### check ART anova
library(ARTool)
model_2 = art(value ~ platform + signal + platform:signal,
            data = df2_2)
anova(model_2)
### post hoc comparison for interactions in a two-way model
model.int2 = artlm(model_2, "platform:signal")
marginal_2 = emmeans(model.int2, ~ platform:signal)
contrast(marginal_2, method="pairwise", adjust="none")

### interaction plot
with(df2_2, interaction.plot(platform, signal, value, fun = mean,
                          main = "Interaction Plot"))

# Effect Size using Cliff Delta
## RQ 1
### combine two block
cliff.delta(total_rq1$web, total_rq1$app)
### block 1 (big company)
cliff.delta(big_all$web, big_all$app)
### block 2 (small company)
cliff.delta(sma_all$web, sma_all$app)

# plot density
## block 1
tmp_web <- big_all$web
tmp_app <- big_all$app
tmp_web <- cbind(tmp_web, platform='Web')
tmp_app <- cbind(tmp_app, platform='Native')
colnames(tmp_web) <- c("value", "platform")
colnames(tmp_app) <- c("value", "platform")

df_big_label <- data.frame(rbind(tmp_web, tmp_app))
sapply(df_big_label,class)
df_big_label$value <- as.numeric(df_big_label$value)
df_big_label$platform <- as.factor(df_big_label$platform)

ggplot(df_big_label, aes(value, fill = platform) ) +
  geom_density(alpha = 0.4)   + xlim(0, 300)+ labs( x = "Energy Consumption (joules)", y = "Density") 
## block 2
tmp_web <- sma_all$web
tmp_app <- sma_all$app
tmp_web <- cbind(tmp_web, platform='Web')
tmp_app <- cbind(tmp_app, platform='Native')
colnames(tmp_web) <- c("value", "platform")
colnames(tmp_app) <- c("value", "platform")

df_sma_label <- data.frame(rbind(tmp_web, tmp_app))
sapply(df_sma_label,class)
df_sma_label$value <- as.numeric(df_sma_label$value)
df_sma_label$platform <- as.factor(df_sma_label$platform)

ggplot(df_sma_label, aes(value, fill = platform) ) +
  geom_density(alpha = 0.4)   + xlim(0, 300)+ labs( x = "Energy Consumption (joules)", y = "Density") 
