# Preparing the environment and the data
rm(list = ls())
setwd("C:/Users/Asus/Documents/01. Mestrado/Thesis/Notebooks & Results") 
source("librec.R") # Library of recursive methods with RL algorithms

library(tidyverse)
library(stargazer)
library(ggplot2)
library(dplyr)
library(GGally)
library(grid)

dt <- read.csv("ptdata2.csv")
dt_tibble <- as_tibble(read.csv("ptdata.csv"))

# Add year and quarter variables
dt <- dt %>% 
  mutate(obs_year = as.numeric(substr(OBS, 1, 4)),
         obs_quarter = as.numeric(substring(OBS, 6, 6)),
         num_quarter = as.numeric(gsub("Q", "", OBS)))

#Outlier visualization - COVID lock downs (2020Q2 and 2021Q1)
# Plot with the four time series displayed yearly from 1977 to 2022
dt_year_agg <- dt %>% mutate(OBS = as.numeric(substr(OBS,1,4))) %>% 
  group_by(OBS)  %>%
  summarise(mean_GDP = mean(GDP),
            mean_PRIV_CONS = mean(PRIV_CONS),
            mean_INVEST = mean(INVEST),
            mean_EXPORT = mean(EXPORT), 
            n = n())

ggplot(dt_year_agg) + 
  geom_line(mapping = aes(x = OBS, y = mean_GDP, color = "GDP"), size=1.2) + 
  geom_line(mapping = aes(x = OBS, y = mean_PRIV_CONS, color = "Private Consumption"), size=1.2) + 
  geom_line(mapping = aes(x = OBS, y = mean_INVEST, color = "Investment"), size=1.2) + 
  geom_line(mapping = aes(x = OBS, y = mean_EXPORT, color = "Export"), size=1.2) +
  labs(x = 'Year', y = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.line = element_line(colour = "black", 
                                 arrow = arrow(length = unit(0.3, "cm"))),  # Add arrows to the axes
        axis.text = element_text(color = "black")) +
  scale_color_manual(values = c("GDP" = "blue", "Private Consumption" = "orange", "Investment" = "red", "Export" = "green")) +
  guides(color = guide_legend(title = NULL)) +
  geom_segment(x = 2020, xend = 2020, y = -Inf, yend = 98.08170, linetype = 2, col = 'grey20')

#Outlier visualization - COVID lock downs (2020Q2 and 2021Q1) - closer look
dt_quarter_agg <- dt %>%
  mutate(Year = as.numeric(substr(OBS, 1, 4)),
         Quarter = substr(OBS, 6, 7)) %>%
  group_by(Year, Quarter) %>%
  summarise(mean_GDP = mean(GDP),
            mean_PRIV_CONS = mean(PRIV_CONS),
            mean_INVEST = mean(INVEST),
            mean_EXPORT = mean(EXPORT),
            n = n(),
            .groups = "drop_last") %>%
  filter(Year >= 2019 & Year <= 2022)

dt_quarter_agg <- dt_quarter_agg %>%
  mutate(YearQuarter = as.numeric(paste0(Year,Quarter)))

ggplot(dt_quarter_agg) + 
  geom_line(mapping = aes(x = YearQuarter, y = mean_GDP, color = "GDP"), size=1.2) + 
  geom_line(mapping = aes(x = YearQuarter, y = mean_PRIV_CONS, color = "Private Consumption"), size=1.2) + 
  geom_line(mapping = aes(x = YearQuarter, y = mean_INVEST, color = "Investment"), size=1.2) + 
  geom_line(mapping = aes(x = YearQuarter, y = mean_EXPORT, color = "Export"), size=1.2) +
  labs(x = 'Year and Quarter', y = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.line = element_line(colour = "black", 
                                 arrow = arrow(length = unit(0.3, "cm"))),  # Add arrows to the axes
        axis.text = element_text(color = "black")) +
  scale_color_manual(values = c("GDP" = "blue", "Private Consumption" = "orange", "Investment" = "red", "Export" = "green")) +
  guides(color = guide_legend(title = NULL)) +
  geom_segment(x = 2020, xend = 2020, y = -Inf, yend = 98.08170, linetype = 2, col = 'grey20')

# Visualization of the distribution of the values before treatment
boxplot(dt$GDP ~ dt$obs_year,col='steelblue',xlab='GDP',ylab='') # Boxplot for GDP
boxplot(dt$PRIV_CONS ~ dt$obs_year,col='steelblue',xlab='Private Consumption',ylab='') # Boxplot for PRIV_CONS
boxplot(dt$INVEST ~ dt$obs_year,col='steelblue',xlab='INVEST',ylab='') # Boxplot for INVEST
boxplot(dt$EXPORT ~ dt$obs_year,col='steelblue',xlab='EXPORT',ylab='') # Boxplot for EXPORT

#######################################################################################
# Outlier treatment - COVID lock downs (2020Q2 and 2021Q1) ... because RL requires smooth series
dt[174,2] <- (dt[173,2] + dt[175,2])/2
dt[174,3] <- (dt[173,3] + dt[175,3])/2
dt[174,4] <- (dt[173,4] + dt[175,4])/2
dt[174,5] <- (dt[173,5] + dt[175,5])/2

dt[177,2] <- (dt[176,2] + dt[178,2])/2
dt[177,3] <- (dt[176,3] + dt[178,3])/2
dt[177,4] <- (dt[176,4] + dt[178,4])/2
dt[177,5] <- (dt[176,5] + dt[178,5])/2

# Pos-treatment visualization
dt_year_agg <- dt %>% mutate(OBS = as.numeric(substr(OBS,1,4))) %>% 
  group_by(OBS)  %>%
  summarise(mean_GDP = mean(GDP),
            mean_PRIV_CONS = mean(PRIV_CONS),
            mean_INVEST = mean(INVEST),
            mean_EXPORT = mean(EXPORT), 
            n = n())

ggplot(dt_year_agg) + 
  geom_line(mapping = aes(x = OBS, y = mean_GDP, color = "GDP"), size=1.2) + 
  geom_line(mapping = aes(x = OBS, y = mean_PRIV_CONS, color = "Private Consumption"), size=1.2) + 
  geom_line(mapping = aes(x = OBS, y = mean_INVEST, color = "Investment"), size=1.2) + 
  geom_line(mapping = aes(x = OBS, y = mean_EXPORT, color = "Export"), size=1.2) +
  labs(x = 'Year', y = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.line = element_line(colour = "black", 
                                 arrow = arrow(length = unit(0.3, "cm"))),  # Add arrows to the axes
        axis.text = element_text(color = "black")) +
  scale_color_manual(values = c("GDP" = "blue", "Private Consumption" = "orange", "Investment" = "red", "Export" = "green")) +
  guides(color = guide_legend(title = NULL)) +
  geom_segment(x = 2020, xend = 2020, y = -Inf, yend = 98.08170, linetype = 2, col = 'grey20')


# Visualization of the distribution after treatment
boxplot(dt$GDP ~ dt$obs_year,col='steelblue',xlab='GDP (treated)',ylab='') # Boxplot for GDP
boxplot(dt$PRIV_CONS ~ dt$obs_year,col='steelblue',xlab='Private Consumption (treated)',ylab='') # Boxplot for PRIV_CONS
boxplot(dt$INVEST ~ dt$obs_year,col='steelblue',xlab='INVEST (treated)',ylab='') # Boxplot for INVEST
boxplot(dt$EXPORT ~ dt$obs_year,col='steelblue',xlab='EXPORT (treated)',ylab='') # Boxplot for EXPORT

#########################################################################################
# Data Characteristics
# Scatter plot showing the relationship between all four variables
ggpairs(dt[, c("GDP", "PRIV_CONS", "INVEST", "EXPORT")])

ggpairs(dt[, c("GDP", "PRIV_CONS", "INVEST", "EXPORT")], 
        lower = list(continuous = "smooth"), 
        upper = list(continuous = "points"),
        diag = list(continuous = "blank"), 
        axisLabels = "none") +
  theme(panel.background = element_blank())

# Table with Descriptive Statistics
stargazer(as.data.frame(dt %>% select(GDP,PRIV_CONS,INVEST,EXPORT)), type = "text")

#########################################################################################
########### GDP ########### 
y_gdp <- dt[,2] # Response variable: GDP
y_gdp <- ts(y_gdp)
#Hodrick-Prescott filter model
result <- cv.hp(y_gdp, lambda = 1600,32)
filtered_series <- result$filtered_series

print("MEAN ABSOLUTE ERROR")
print(result$MAE)
print("MEAN SQUARED ERROR")
print(result$MSE)
print("ROOT MEAN SQUARED ERROR")
print(result$RMSE)

#Auto-Regressive model
ar_gdp<- cv.AR1(y_gdp, k=32)
print("MEAN ABSOLUTE ERROR")
print(ar_gdp$MAE)
print("MEAN SQUARED ERROR")
print(ar_gdp$MSE)
print("ROOT MEAN SQUARED ERROR")
print(ar_gdp$RMSE)

# RL models
k <- 32    # Number of folds
td0      <- cv.td0LFA(y_gdp,c=0.00000001,k=k)
tdLambda <- cv.tdLambdaLFA(y_gdp,c=0.000000002,lambda=0.1,k=k)
gtd2     <- cv.GTD2(y_gdp,c=0.00000002,k=k)

# Combining forecasts with the median
pred <- cbind(td0=td0$prediction, tdLambda=tdLambda$prediction, gtd2=gtd2$prediction)
med <- apply(pred,1,median)
pred <- cbind(pred, median=med)

# Mean Absolute Error
n <- length(y_gdp)
m1 <- n - k + 1
error_med <- y_gdp[m1:n] - med
mae_med <- sum(abs(error_med))/k
MAE <- cbind(td0=td0$MAE, tdLambda=tdLambda$MAE, gtd2=gtd2$MAE, median=mae_med)
print("MEAN ABSOLUTE ERROR")
print(MAE)

# Mean Squared Error
mse_med <- sum((error_med)^2)/k
MSE <- cbind(td0=td0$MSE, tdLambda=tdLambda$MSE, gtd2=gtd2$MSE, median=mse_med)
print("MEAN SQUARED ERROR")
print(MSE)

# Root Mean Squared Error
rmse_med <- sqrt(sum((error_med)^2)/k)
RMSE <- cbind(td0=sqrt(td0$MSE), tdLambda=sqrt(tdLambda$MSE), gtd2=sqrt(gtd2$MSE), median=rmse_med)
print("ROOT MEAN SQUARED ERROR")
print(RMSE)

#########################################################################################
########### PRIV CONS ########### 
y_priv_cons <- dt[,3]     # Response variable: PRIV CONS

#Hodrick-Prescott filter
result <- hpfilt(y_priv_cons, lambda = 1600)
filtered_series <- result$filtered_series

print("MEAN ABSOLUTE ERROR")
print(result$MAE)
print("MEAN SQUARED ERROR")
print(result$MSE)
print("ROOT MEAN SQUARED ERROR")
print(result$RMSE)

#Auto-Regressive model
ar_priv_cons<- cv.AR1(y_priv_cons, k=32)
print("-MEAN ABSOLUTE ERROR")
print(ar_priv_cons$MAE)
print("MEAN SQUARED ERROR")
print(ar_priv_cons$MSE)
print("ROOT MEAN SQUARED ERROR")
print(ar_priv_cons$RMSE)

# RL models
k <- 32    # Number of folds
td0      <- cv.td0LFA(y_priv_cons,c=0.0000000005,k=k)
tdLambda <- cv.tdLambdaLFA(y_priv_cons,c=0.000000005,lambda=0.1,k=k)
gtd2     <- cv.GTD2(y_priv_cons,c=0.00000004,k=k)

# Combining forecasts with the median
pred <- cbind(td0=td0$prediction, tdLambda=tdLambda$prediction, gtd2=gtd2$prediction)
med <- apply(pred,1,median)
pred <- cbind(pred, median=med)

# Mean Absolute Error
n <- length(y_priv_cons)
m1 <- n - k + 1
error_med <- y_priv_cons[m1:n] - med
mae_med <- sum(abs(error_med))/k
MAE <- cbind(td0=td0$MAE, tdLambda=tdLambda$MAE, gtd2=gtd2$MAE, median=mae_med)
print("MEAN ABSOLUTE ERROR")
print(MAE)

# Mean Squared Error
mse_med <- sum((error_med)^2)/k
MSE <- cbind(td0=td0$MSE, tdLambda=tdLambda$MSE, gtd2=gtd2$MSE, median=mse_med)
print("MEAN SQUARED ERROR")
print(MSE)

# Root Mean Squared Error
rmse_med <- sqrt(sum((error_med)^2)/k)
RMSE <- cbind(td0=sqrt(td0$MSE), tdLambda=sqrt(tdLambda$MSE), gtd2=sqrt(gtd2$MSE), median=rmse_med)
print("ROOT MEAN SQUARED ERROR")
print(RMSE)

#########################################################################################
########### INVEST ###########
y_inv<- dt[,4]     # Response variable: INVEST

#Hodrick-Prescott filter
result <- hpfilt(y_inv, lambda = 1600)
filtered_series <- result$filtered_series

print("MEAN ABSOLUTE ERROR")
print(result$MAE)
print("MEAN SQUARED ERROR")
print(result$MSE)
print("ROOT MEAN SQUARED ERROR")
print(result$RMSE)

#Auto-Regressive model
ar_inv<- cv.AR1(y_inv, k=32)
print("MEAN ABSOLUTE ERROR")
print(ar_inv$MAE)
print("MEAN SQUARED ERROR")
print(ar_inv$MSE)
print("ROOT MEAN SQUARED ERROR")
print(ar_inv$RMSE)

# RL models
k <- 32    # Number of folds
td0      <- cv.td0LFA(y_inv,c=0.000000003,k=k)
tdLambda <- cv.tdLambdaLFA(y_inv,c=0.000000001,lambda=0.1,k=k)
gtd2     <- cv.GTD2(y_inv,c=0.00000002,k=k)

# Combining forecasts with the median
pred <- cbind(td0=td0$prediction, tdLambda=tdLambda$prediction, gtd2=gtd2$prediction)
med <- apply(pred,1,median)
pred <- cbind(pred, median=med)

# Mean Absolute Error
n <- length(y_inv)
m1 <- n - k + 1
error_med <- y_inv[m1:n] - med
mae_med <- sum(abs(error_med))/k
MAE <- cbind(td0=td0$MAE, tdLambda=tdLambda$MAE, gtd2=gtd2$MAE, median=mae_med)
print("MEAN ABSOLUTE ERROR")
print(MAE)

# Mean Squared Error
mse_med <- sum((error_med)^2)/k
MSE <- cbind(td0=td0$MSE, tdLambda=tdLambda$MSE, gtd2=gtd2$MSE, median=mse_med)
print("MEAN SQUARED ERROR")
print(MSE)

# Root Mean Squared Error
rmse_med <- sqrt(sum((error_med)^2)/k)
RMSE <- cbind(td0=sqrt(td0$MSE), tdLambda=sqrt(tdLambda$MSE), gtd2=sqrt(gtd2$MSE), median=rmse_med)
print("ROOT MEAN SQUARED ERROR")
print(RMSE)

########### EXPORT ###########
y_exp<- dt[,5]     # Response variable: EXPORT

#Hodrick-Prescott filter
result <- hpfilt(y_exp, lambda = 1600)
filtered_series <- result$filtered_series

print("MEAN ABSOLUTE ERROR")
print(result$MAE)
print("MEAN SQUARED ERROR")
print(result$MSE)
print("ROOT MEAN SQUARED ERROR")
print(result$RMSE)

#Auto-Regressive model
ar_exp<- cv.AR1(y_exp, k=32)
print("MEAN ABSOLUTE ERROR")
print(ar_exp$MAE)
print("MEAN SQUARED ERROR")
print(ar_exp$MSE)
print("ROOT MEAN SQUARED ERROR")
print(ar_exp$RMSE)

# RL models
k <- 32    # Number of folds
td0      <- cv.td0LFA(y_exp,c=0.00000006,k=k)
tdLambda <- cv.tdLambdaLFA(y_exp,c=0.000000003,lambda=0.1,k=k)
gtd2     <- cv.GTD2(y_exp,c=0.000003,k=k)

# Combining forecasts with the median
pred <- cbind(td0=td0$prediction, tdLambda=tdLambda$prediction, gtd2=gtd2$prediction)
med <- apply(pred,1,median)
pred <- cbind(pred, median=med)

# Mean Absolute Error
n <- length(y_exp)
m1 <- n - k + 1
error_med <- y_exp[m1:n] - med
mae_med <- sum(abs(error_med))/k
MAE <- cbind(td0=td0$MAE, tdLambda=tdLambda$MAE, gtd2=gtd2$MAE, median=mae_med)
print("MEAN ABSOLUTE ERROR")
print(MAE)

# Mean Squared Error
mse_med <- sum((error_med)^2)/k
MSE <- cbind(td0=td0$MSE, tdLambda=tdLambda$MSE, gtd2=gtd2$MSE, median=mse_med)
print("MEAN SQUARED ERROR")
print(MSE)

# Root Mean Squared Error
rmse_med <- sqrt(sum((error_med)^2)/k)
RMSE <- cbind(td0=sqrt(td0$MSE), tdLambda=sqrt(tdLambda$MSE), gtd2=sqrt(gtd2$MSE), median=rmse_med)
print("ROOT MEAN SQUARED ERROR")
print(RMSE)

