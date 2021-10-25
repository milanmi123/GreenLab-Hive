library(tidyverse)
library(ggplot2)
library(e1071)
library(grid)
library("gridExtra")
library(car)
library(repr)

# input data
csv_wa <- read.csv(file = './combine_data/all.csv')
csv_long <- read.csv(file = './combine_data/long_app_web.csv')
csv_4g <- read.csv(file="./combine_data/4g_app_web.csv")
head(csv_wa['web'])

hist_web <- qplot(csv_wa$web, geom="histogram", xlab = "Energy Consumption (joules)", ylab = "Density", col = I("White"))
hist_app <- qplot(csv_wa$app, geom="histogram", xlab = "Energy Consumption (joules)", ylab = "Density", col = I("White"))
hist_app

# Test for normality -- Fail
shapiro.test(csv_wa$web)
shapiro.test(csv_wa$app)
# Test for skewness
skewness(csv_wa$web)
skewness(csv_wa$app)

# data transformation (square, natural log, reciprocal)
web_sqr <- csv_wa$web ^ 2
web_ln <- log(csv_wa$web)
web_recip <- 1/ csv_wa$web

app_sqr <- csv_wa$app ^ 2
app_ln <- log(csv_wa$app)
app_recip <- 1/ csv_wa$app

web_4g_sqr <- csv_4g$web ^ 2
web_4g_ln <- log(csv_4g$web)
web_4g_recip <- 1/ csv_4g$web

app_4g_sqr <- csv_4g$app ^ 2
app_4g_ln <- log(csv_4g$app)
app_4g_recip <- 1/ csv_4g$app

web_long_sqr <- csv_long$web ^ 2
web_long_ln <- log(csv_long$web)
web_long_recip <- 1/ csv_long$web

app_long_sqr <- csv_long$app ^ 2
app_long_ln <- log(csv_long$app)
app_long_recip <- 1/ csv_long$app



# test for skewness after transformation
skewness(web_sqr)
skewness(web_ln)
skewness(web_recip)

skewness(app_sqr)
skewness(app_ln)
skewness(app_recip)

#function of visualizing transformation with qqplot
check_normaliy <- function(inputdata1, inputdata2, inputdata3, qq_ylab1, qq_ylab2, qq_ylab3) {
  qqplot_inputdata1 <- ggplot(data.frame(y= inputdata1), aes(sample = y)) +
    stat_qq() + stat_qq_line(col="blue", lty=5)+ ylab(paste("Energy Consumption Sample Quantile", qq_ylab1)) + 
    xlab("Normal Theoretical Quantile")

  qqplot_inputdata2 <- ggplot(data.frame(y= inputdata2), aes(sample = y)) +
    stat_qq() + stat_qq_line(col="blue", lty=5)+ ylab(paste("Energy Consumption Sample Quantile", qq_ylab2)) + 
    xlab("Normal Theoretical Quantile")

  qqplot_inputdata3 <- ggplot(data.frame(y= inputdata3), aes(sample = y)) +
    stat_qq() + stat_qq_line(col="blue", lty=5)+ ylab(paste("Energy Consumption Sample Quantile", qq_ylab3)) + 
    xlab("Normal Theoretical Quantile")
  
  grid.arrange(qqplot_inputdata1, qqplot_inputdata2, qqplot_inputdata3, ncol=3)
}


web_qq <- check_normaliy(web_sqr, web_ln, web_recip, " (joules ^ 2)", "Ln(joules)", "(1/ joules)")
app_qq <- check_normaliy(app_sqr, app_ln, web_recip, " (joules ^ 2)", "Ln(joules)", "(1/ joules)")

web_long_qq <- check_normaliy(web_long_sqr, web_long_ln, web_long_recip, " (joules ^ 2)", "Ln(joules)", "(1/ joules)")
app_long_qq <- check_normaliy(app_long_sqr, app_long_ln, app_long_recip, " (joules ^ 2)", "Ln(joules)", "(1/ joules)")

web_4g_qq <- check_normaliy(web_4g_sqr, web_4g_ln, web_4g_recip, " (joules ^ 2)", "Ln(joules)", "(1/ joules)")
app_4g_qq <- check_normaliy(app_4g_sqr, app_4g_ln, app_4g_recip, " (joules ^ 2)", "Ln(joules)", "(1/ joules)")


