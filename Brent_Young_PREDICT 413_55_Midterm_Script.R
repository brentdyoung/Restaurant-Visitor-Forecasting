#Midterm Project Code
#R Code:
#Load the libraries

library(fpp)

# Loading Data
library(data.table)

# Data Manipulation
library(dplyr)             # Data Manipulation Verbs
library(tibble)            # Advanced Dataframe
library(tidyr)             # Tidying Data
library(stringr)           # String Manipulation
library(lubridate)         # Date manipulation

# Data Visualization
library(ggplot2)           # Grammar-of-Graphics Verbs
library(cowplot)           # Multiple plots in a panel

library(date)
require(knitr)
require(stringi)
library(reshape2)
require(scales)
library(magrittr)
library(Hmisc)

library(rJava)
library(readr)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(xlsxjars)
library(xlsx)

# specific visualization
library('ggfortify') # visualization
library('ggrepel') # visualization
library('ggridges') # visualization
library('ggExtra') # visualization
library('ggforce') # visualization

library('scales') # visualization
library('grid') # visualization
library('gridExtra') # visualization
library('RColorBrewer') # visualization
library('corrplot') # visualization

library('broom') # data wrangling
library('imputeTS') # imputation
library(data.table)
library(lubridate)
library(dplyr)
library(date)
library(doMC)
library(xts)
library(timetk)
library('prophet') # time series analysis
library(zoo)
library(VIM)


#Multiplot Code
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Load the data
setwd("~/R/PREDICT 413/Midterm")
air_visits<- read.csv("air_visit_data.csv") #historical visit data for the air restaurants.
holidays <- read.csv('date_info.csv') #gives basic information about the calendar dates in the dataset

store_ids <- read.csv('store_id_relation.csv') #allows you to join select restaurants that have both the air #and hpg system

test <- read.csv('sample_submission.csv') #submission in the correct format, including the days for which you must forecast

#EDA
#Data Quality Check

#air_visits
dim(air_visits)
summary(air_visits)
glimpse(air_visits)
describe(air_visits)

air_visits %>% distinct(air_store_id) %>% nrow()
apply(air_visits, 2, function(x)length(unique(x)))

#Holiday Stats
filter(holidays, holiday_flg == '1')  %>% group_by(day_of_week)  %>% count(holiday_flg)

#Data Visualization
air_visits <- air_visits %>%
  mutate(visit_date = mdy(visit_date))

p1 <- air_visits %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
  geom_line(col = "light gray") +
  labs(y = "All visitors", x = "Date")

p2 <- air_visits %>%
  ggplot(aes(visitors)) +
  geom_vline(xintercept = 20, color = "orange") +
  geom_histogram(fill = "light gray", bins = 30) +
  scale_x_log10()

p3 <- air_visits %>%
  mutate(wday = wday(visit_date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(wday, visits, fill = wday)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(x = "Day of the week", y = "Median visitors")

p4 <- air_visits %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(month, visits, fill = month)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Month", y = "Median visitors")

layout <- matrix(c(1,1,1,1,2,3,4,4),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)

#Missing Values EDA
air_visits<- read.csv("air_visit_data.csv") #historical visit data for the air restaurants.

air_visits <- air_visits %>%
  mutate(visit_date = mdy(visit_date))

#Reshape dataset from long to wide format
train_wide <- dcast(
  air_visits, air_store_id ~ visit_date, value.var = "visitors")

#Check for missing values
str(train_wide)
describe(train_wide)
sum(is.na(train_wide))
sapply(train_wide, function(x) sum(is.na(x)))

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(train_wide,2,pMiss)

aggr_plot <- aggr(train_wide, col=c('navylight gray','red'), numbers=TRUE, sortVars=TRUE, labels=names(train_wide), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

####################################
#Load the data
setwd("~/R/PREDICT 413/Midterm")
air_visits<- read.csv("air_visit_data.csv") #historical visit data for the air restaurants.
test <- read.csv('sample_submission.csv') #submission in the correct format, including the days for which you must forecast

air_visits <- air_visits %>%
  mutate(visit_date = mdy(visit_date))

#Model Selection
#auto.arima Method
air_id = "air_04341b588bde96cd"

#Tests Prediction by Forecasting on Identical Time Frame of the 39 days from Test Set
pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()

#Predict for the last 39 days of our training sample.
max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

#Extract the time series for the specific air_store_id.  Fill NA's with 0. 
foo <- air_visits %>%
  filter(air_store_id == air_id)

visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  replace_na(list(visitors = 0)) %>%
  rownames_to_column()

#Split into train and test
visits_train <- visits %>% filter(visit_date <= split_date)
visits_valid <- visits %>% filter(visit_date > split_date)

#TsDisplay
tsdisplay(visits_train$visitors)

#Unit Root Tests
adf.test(visits_train$visitors, alternative = "stationary")
kpss.test(visits_train$visitors)

#Differencing 
kpss.test(diff(visits_train$visitors))
tsdisplay(diff(visits_train$visitors))

#Fit auto.arima. Create time series and remove outliers using tsclean and weekly frequency of 7.
arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, frequency = 7)),
                        stepwise = FALSE, approximation = FALSE)

summary(arima.fit)

tsdisplay(residuals(arima.fit))
Box.test(residuals(arima.fit), fitdf=3, lag=10, type="Ljung")
tsdiag(arima.fit)

#Training Set Accuracy - Summary
summary(arima.fit) # training set

#Training Set Accuracy - Goodness-of-fit
accuracy(arima.fit) # training set

#Forecast Plot 
#Add confidence levels
arima_visits <- arima.fit %>% forecast(h = pred_len, level = c(80,95))
arima_visits %>%
  autoplot +
  geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "light gray") +
  labs(x = "Weeks", y = "Visitors")
accuracy(arima.fit)

#Forecast on Test Set in Weeks
par(mfrow=c(1,1)) 
Auto.ARIMA <-forecast(arima.fit, h=pred_len)
plot(Auto.ARIMA, ylab="Visitors")
Auto.ARIMA
summary(Auto.ARIMA)
print(accuracy(Auto.ARIMA, pred_len))

#Diagnostics on Forecasts

#Box-Ljung test B
Box.test(Auto.ARIMA$residuals, lag=25, type = "Ljung-Box")

par(mfrow=c(2,2)) 
acf(Auto.ARIMA $residuals, lag.max=25)
plot(Auto.ARIMA $residuals, ylab = "Residuals")
abline(h = 0, col = "red")
hist(Auto.ARIMA $residuals, main = "", xlab = "Residuals")
par(mfrow=c(1,1)) 

#Compare different ARIMA functions
plot_auto_arima_air_id <- function(air_id){
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()
  
  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  foo <- air_visits %>%
    filter(air_store_id == air_id)
  
  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    replace_na(list(visitors = 0)) %>%
    rownames_to_column()
  
  visits_train <- visits %>% filter(visit_date <= split_date)
  visits_valid <- visits %>% filter(visit_date > split_date)
  
  arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, frequency = 7)),
                          stepwise = FALSE, approximation = FALSE)
  
  arima_visits <- arima.fit %>% forecast(h = pred_len, level = c(80,95))
  
  arima_visits %>%
    autoplot +
    geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "light gray") +
    labs(x = "Weeks", y = "Visitors")
}

#Plot arima
p1 <- plot_auto_arima_air_id("air_f3f9824b7d70c3cf")
p2 <- plot_auto_arima_air_id("air_8e4360a64dbd4c50")
p3 <- plot_auto_arima_air_id("air_1c0b150f9e696a5f")
p4 <- plot_auto_arima_air_id("air_04341b588bde96cd")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)


#ETS Method
air_id = "air_04341b588bde96cd"

#Tests Prediction by Forecasting on Identical Time Frame of the 39 days from Test Set
pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()

#Predict for the last 39 days of our training sample.
max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

#Extract the time series for the specific air_store_id.  Fill NA's with 0. 
foo <- air_visits %>%
  filter(air_store_id == air_id)

visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  replace_na(list(visitors = 0)) %>%
  rownames_to_column()

#Split into train and test
visits_train <- visits %>% filter(visit_date <= split_date)
visits_valid <- visits %>% filter(visit_date > split_date)

ets.fit <- ets(tsclean(ts(visits_train$visitors, frequency = 7)))

ets_visits <-  ets.fit %>% forecast(h = pred_len, level = c(80,95))

summary(ets_visits)

#Diagnostics
tsdisplay(residuals(ets.fit))
Box.test(residuals(ets.fit), fitdf=3, lag=10, type="Ljung")

#Training Set Accuracy - Summary
summary(ets_visits) # training set

#Training Set Accuracy - Goodness-of-fit
accuracy(ets_visits) # training set

#Forecast Plot 
#Add confidence levels
ets_visits <- ets.fit %>% forecast(h = pred_len, level = c(80,95))
ets_visits %>%
  autoplot +
  geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "light gray") +
  labs(x = "Weeks", y = "Visitors")

#Forecast on Test Set
ETS <-forecast(ets_visits, h=pred_len)
plot(ETS, ylab="Visitors")
ETS
print(accuracy(ETS, pred_len))


#Compare different ETS  functions
plot_ets_air_id <- function(air_id){
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()
  
  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  foo <- air_visits %>%
    filter(air_store_id == air_id)
  
  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    replace_na(list(visitors = 0)) %>%
    rownames_to_column()
  
  visits_train <- visits %>% filter(visit_date <= split_date)
  visits_valid <- visits %>% filter(visit_date > split_date)
  
  ets.fit <- ets(tsclean(ts(visits_train$visitors, frequency = 7)))
  
  ets_visits <- ets.fit %>% forecast(h = pred_len, level = c(80,95))
  
  ets_visits %>%
    autoplot +
    geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "light gray") +
    labs(x = "Weeks", y = "Visitors")
}

#Plot ETS
p5 <- plot_ets_air_id("air_f3f9824b7d70c3cf")
p6 <- plot_ets_air_id("air_8e4360a64dbd4c50")
p7 <- plot_ets_air_id("air_1c0b150f9e696a5f")
p8 <- plot_ets_air_id("air_04341b588bde96cd")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p5, p6, p7, p8, layout=layout)

#Mixed Model

forecast_df <- data.frame(arima_visits=as.matrix(arima_visits$mean),ets_visits =as.matrix(ets_visits$mean), actual=as.matrix(visits_valid))
forecast_df[,6] <- as.numeric(as.character(forecast_df[, 6]))
forecast_df$avg_forecast <-(forecast_df$arima_visits +forecast_df$ets_visits)/2
forecast_df[,7] <- as.numeric(as.character(forecast_df[, 7]))
str(forecast_df)
forecast_df$avg_diffs <-forecast_df$actual.visitors-forecast_df$avg_forecast
forecast_df
print(paste("ME", round(mean(forecast_df$avg_diffs),2)))
print(paste("RMSE", round(sqrt(mean(forecast_df$avg_diffs^2)),2)))
print(paste("MAE", round(mean(abs(forecast_df$avg_diffs)),2)))

####################################################
#Mixed Model of Auto.Arima & ETS

library(data.table)
library(lubridate)
library(dplyr)
library(date)
library(doMC)
library(fpp)

##Load data
setwd("~/R/PREDICT 413/Midterm")
air_visit<- read.csv("air_visit_data.csv") #historical visit data for the air restaurants.

#Change date format
air_visit <- air_visit %>%
  mutate(visit_date = mdy(visit_date))

#Reshape dataset from long to wide format
train_wide <- dcast(
  air_visit, air_store_id ~ visit_date, value.var = "visitors", fill = 0)

#Create timeseries on train data and forecast interval of 39 days
train_ts <- ts(train_wide[, 2:dim(train_wide)[2]], frequency = 7) 
fcst_interval = 39  ### 39 days of forecast horizon

#Create a matrix for the models
fcst_matrix <- matrix(NA,nrow=4*nrow(train_ts),ncol=fcst_interval)

#Register cores for parallel processing and create forecasts
registerDoMC(detectCores()-1)
fcst_matrix <- foreach(i=1:nrow(train_ts),.combine=rbind, .packages=c("forecast")) %dopar% { 
  fcst_ets <- forecast(ets(train_ts[i,]),h=fcst_interval)$mean
  fcst_arima <- forecast(auto.arima(train_ts[i,]),h=fcst_interval)$mean
  fcst_matrix <- rbind(fcst_ets,fcst_arima)
}

#Post-process the forecast table
fcst_matrix_mix <- aggregate(fcst_matrix,list(rep(1:(nrow(fcst_matrix)/2),each=2)),mean)[-1]
fcst_matrix_mix[fcst_matrix_mix < 0] <- 0
colnames(fcst_matrix_mix) <- as.character(
  seq(from = as.Date("2017-04-23"), to = as.Date("2017-05-31"), by = 'day'))
fcst_df <- as.data.frame(cbind(train_wide[, 1], fcst_matrix_mix)) 
colnames(fcst_df)[1] <- "air_store_id"

#Change the forecast data frame back from wide to long format for final submission
fcst_df_long <- melt(
  fcst_df, id = 'air_store_id', variable.name = "fcst_date", value.name = 'visitors')
fcst_df_long$air_store_id <- as.character(fcst_df_long$air_store_id)
fcst_df_long$fcst_date <- as.Date(parse_date_time(fcst_df_long$fcst_date,'%y-%m-%d'))
fcst_df_long$visitors <- as.numeric(fcst_df_long$visitors)

#Process the sample submission file
sample_submission <- read.csv('sample_submission.csv') 
sample_submission$visitors <- NULL
sample_submission$store_id <- substr(sample_submission$id, 1, 20)
sample_submission$visit_date <- substr(sample_submission$id, 22, 31)
sample_submission$visit_date <- as.Date(parse_date_time(sample_submission$visit_date,'%y-%m-%d'))

#Create final submission file
submission <- left_join(
  sample_submission, fcst_df_long, c("store_id" = "air_store_id", 'visit_date' = 'fcst_date'))
submission$visitors[is.na(submission$visitors)] <- 0
final_submission <- select(submission, c('id', 'visitors'))
write.csv(final_submission, "Submission Mixed Model.csv", row.names = FALSE)

