#      * * * CAPSTONE PROJECT - WALMART STORE SALES FORECASTING * * *      #  

# Author: Grifo Vincenzo

################################
# Downloading the datasets
################################

# In this section the datasets will be downloaded and imported to the Global Environment
# Note: this process could take a couple of minutes

# Loading the libraries that will be used
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

# The Walmart Recruiting - Store Sales Forecasting competition in Kaggle
# https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting

# As downloading directly from this database will require a Kaggle account and to accept terms and condition of the competition,
# the datasets have been preloaded in a GitHub repository
# 3 datasets will be downloaded: 1) features.csv, 2) stores.csv and 3) train.csv

# --- Importing the datasets ---
# 1) features.csv
url <- "https://raw.githubusercontent.com/vgrifo/walmart_store_sales_forecasting/master/Walmart_store_sales_forecasting_datasets/features.csv"
tmp_filename <- tempfile()
download.file(url, tmp_filename)
features <- read_csv(tmp_filename)

# 2) stores.csv
url <- "https://raw.githubusercontent.com/vgrifo/walmart_store_sales_forecasting/master/Walmart_store_sales_forecasting_datasets/stores.csv"
tmp_filename <- tempfile()
download.file(url, tmp_filename)
stores <- read_csv(tmp_filename)

# 3) train.csv
url <- "https://raw.githubusercontent.com/vgrifo/walmart_store_sales_forecasting/master/Walmart_store_sales_forecasting_datasets/train.csv"
tmp_filename <- tempfile()
download.file(url, tmp_filename)
train <- read_csv(tmp_filename)

# Removing temporary files from Global Environment
rm(tmp_filename, url)


# --- Splitting the edx dataset into TEST and TRAINING set ---

set.seed(17, sample.kind="Rounding")

# The test set will be 10% of the training set
test_index <- createDataPartition(y = train$Weekly_Sales, times = 1, p = 0.1, list = FALSE)

# Converting train from a tibble to a dataframe
train <- as.data.frame(train)

train_set <- train[-test_index,]
test_set <- train[test_index,]
# Removing temporary files from the working environment
rm(test_index)


################################
# DATA EXPLORATION
################################

# In this section visual exploration techniques will be used to uncover initial patterns, characteristics, 
# and features that can be used in the model


# ---- ANALYSING THE DATASETS' STRUCTURE ----

# --- Features dataset ---
# Visualising the structure of the dataset
str(features)
# ->  'Store' is stored as 'num', however it might be more appropriate to transform it into a factor, as it is a categorical variable

# Visualising the first (and last) few rows of the dataset
head(features)
tail(features)

# Checking if there are any NA data
sum(is.na(features))
# -> there qre some NA values because the MarkDown data is only available after Nov 2011, 
#    and these are not available for all stores all the time

# Counting NA values due to missing MarkData
sum(is.na(features[,c(5:9)]))
# -> we can see that this number does not make up for the total of NAs. 
# ->This is because there are some missing data within the 'CPI' and 'Unemployment' columns

# We can verify this with the following code
sum(is.na(features[,c(5:11)]))

# Identifying CPI NAs
na_cpi <-features[(is.na(features$CPI)),]
na_cpi %>% 
  group_by(Date) %>% 
  summarise(n())

# Identifying Unemployment NAs
na_unmp <- features[(is.na(features$Unemployment)),]
na_unmp %>% 
  group_by(Date) %>% 
  summarise(n())
# -> The dates that are have NAs values are the weeks from 03rd May 2013 to 26th July 2013

# Checking that CPI NAs and Unemployment NAs are in the exact same rows
sum(na_cpi[,c(1:2)]!=na_unmp[,c(1:2)])
# -> the dates where there are NAs in CPI also got NAs in Unemployment

# Removing temporary datasets
rm(na_cpi,na_unmp)

# --- Stores dataset ---
# Visualising the structure of the dataset
str(stores)
# -> both 'Store' is stored as 'num', while 'Type' is stored as 'Character', 
#    however it might be more appropriate to transform these into factors, as they are categorical variables

# Visualising the first (and last) few rows of the dataset
head(stores)
tail(stores)

# Checking if there are any NA data
sum(is.na(stores))

# Checking store type distibution
summary(stores$Type)
# -> most of the stores are either A or B type

# --- Train set ---
# Visualising the structure of the dataset
str(train_set)
# -> both 'Store' and 'Dept' are stored as 'num', 
#    however it might be more appropriate to transform these into factors, as they are categorical variables

# Visualising the first (and last) few rows of the dataset
head(train_set)
tail(train_set)

# Checking if there are any NA data
sum(is.na(train_set))

# -- Test set ---
# -> same structure of training set
# Checking if there are any NA data
sum(is.na(test_set))


# --- EXPLORING THE FEATURES ---

#  -- Temperature -- 
# Plotting the time series for temperature by stores
features %>% 
  ggplot(aes(x= Date, y=Temperature)) + 
  geom_line() + 
  facet_wrap(~Store) +
  ggtitle("Temperature per Store Time Series") +
  xlab("Date") +
  ylab("Temperature") 
# -> we can notice the seasonality due to the 4 seasons and 
# -> also temperature varies across stores as these are located in different regions

# -> Average temperature by store
features %>% 
  group_by(Store) %>% 
  summarise(avg_temperature = mean(Temperature))  
# Min & Max avg temperature by store
features %>% 
  group_by(Store) %>% 
  summarise(avg_temperature = mean(Temperature)) %>% 
  arrange(avg_temperature)  %>% 
  slice(c(1,45)) 
# -> Average temperature varies considerably across stores (hence regions)

# Determining how the Temperature influences the sales
train_set %>% 
  left_join(stores, by ="Store") %>%
  left_join(features, by = c("Store","Date")) %>% 
  filter(Store==1 & IsHoliday.x == FALSE) %>%
  mutate(round_temp = round(Temperature, digits=0)) %>%
  # rounding the temperature 
  group_by(Store, round_temp) %>%
  summarise(avg_sales = mean(Weekly_Sales)) %>%
  ggplot(aes(x=round_temp,y=avg_sales)) +
  geom_point() + 
  ggtitle("Average weekly Sales by Tempearature (Store 1)") +
  xlab("Temperature") +
  ylab("Average Weekly Sales") 
  
# -> it is hard to see a pattern, because this view aggregate too much information and does not isolate any effect
# -> (eg high temperature in cold season)

#  -- Unemployement --
# Plotting the time series for unemployement rate by stores.
features %>% 
  ggplot(aes(x= Date, y= Unemployment)) + 
  geom_line() + 
  facet_wrap(~Store) +
  ggtitle("Unemployment rate by Region(Store)") +
  xlab("Date") +
  ylab("Unemployement Rate") 
# -> The unemployement rate history varies considerably across areas/stores 
#    (some area are likely more wealthier than others)

# Average unemployement by store
features %>% 
  filter(Unemployment>=0) %>% 
  group_by(Store) %>% 
  summarise(avg_unemployement = mean(Unemployment)) 

# Min & Max avg unemployement by store
features %>% 
  filter(Unemployment>=0)  %>% 
  group_by(Store) %>% 
  summarise(avg_unemployement = mean(Unemployment)) %>% 
  arrange(avg_unemployement)  %>% 
  slice(c(1,45))
# -> Average unemloyement varies considerably across stores (hence regions)

# -- Store Type --
# Plotting Store Type vs Size to understand if the type of stores depends on size and how
stores %>% 
  ggplot(aes(x="Store", y=Size)) + 
  geom_point(aes(color=Type)) +
  ggtitle("Stores classification by Size") +
  xlab("") +
  ylab("Size") 

# A are the largest type of stores, B the medium, C the smallest. 
# A type could actually be split into 2 size - "Large - Size around 150000" and Extra Large - Size around 200000
# We can further classify the store type using the size and engineer a new feature Size type
stores <- stores %>% 
  mutate(size_type = ifelse(Size < 75000, "Small", 
                            ifelse(Size < 150000, "Medium",
                                   ifelse(Size < 175000, "Large", 
                                          "X-Large"))))

# Average weekly sales  (Stores 1:9)
train_set %>%
  filter(Store==c(1,2,3,4,5,6,7,8,9)) %>%
  group_by(Store,Date) %>% 
  mutate(avg_sales = mean(Weekly_Sales)) %>%
  ggplot(aes(x=Date, y=avg_sales)) +
  geom_line() +
  facet_wrap(~Store) + 
  ggtitle("Average Weekly Sales per Year") +
  xlab("Week") +
  ylab("Average Sales") 
# -> we can see that some stores have considerably higher avg sales than other 
# -> and that sales vary considerably through time, although there are signs of seasonality and periodicity

# Average weekly sales per year
train_set %>%
  mutate(Year = as.factor(year(Date)), Week=week(Date))%>%
  group_by(Date) %>% 
  mutate(avg_sales = mean(Weekly_Sales)) %>%
  ggplot(aes(x=Week, y=avg_sales, color=Year)) +
  geom_line() + 
  ggtitle("Average Weekly Sales per Year") +
  xlab("Week") +
  ylab("Average Sales") 
# -> Average weekly sales follow a similar pattern each year. 
#    Sales in 2010 seem to be higher than 2011, and similar to 2012

# Let's verify this assumption
train_set %>%
  mutate(Year = as.factor(year(Date)), Week=week(Date))%>%
  filter(Week<=45) %>% # data for Xmas 2012 are missing
  group_by(Date) %>% 
  mutate(avg_sales = mean(Weekly_Sales)) %>%
  group_by(Year)%>%
  summarise(average_weekly_sales =mean(avg_sales))

# Average weekly Sales per Store
train_set %>%
  left_join(stores, by = "Store") %>% 
  group_by(Store) %>%
  summarise(average_sales = mean(Weekly_Sales)) %>%
  ggplot(aes(x=as.factor(Store), y=average_sales)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Average Weekly Sales by Store") +
  xlab("Store") +
  ylab("Average Weekly Sales") +
  theme(axis.text.x=element_text(angle=90,hjust=1, size = 6))
# -> some Stores have on average higher weekly sales than others.
# -> let's verify if this is due to the 'type' of store

# Average sales by Type of store
train_set %>%
  left_join(stores, by ="Store") %>%
  group_by(Type,Date) %>% 
  mutate(avg_sales = mean(Weekly_Sales)) %>%
  ggplot(aes(x=Date, y=avg_sales)) +
  geom_line(aes(colour= Type)) + 
  ggtitle("Average Weekly Sales by Type of Store") + 
  xlab("Date") +
  ylab("Average Sales") 
# -> this chart provides us with 2 key insights:
#    1. Larger stores have more sales on average
#    2. Sales in larger stores seem to be more impacted by seasonalities

# -- Department --
# Average weekly sales by Department
train_set %>%
  left_join(stores, by = "Store") %>% 
  group_by(Dept) %>%
  summarise(average_sales = mean(Weekly_Sales)) %>%
  ggplot(aes(x=as.factor(Dept), y=average_sales)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Average Weekly Sales by Department") +
  xlab("Department") +
  ylab("Average Weekly Sales") +
  theme(axis.text.x=element_text(angle=90,hjust=1, size = 6))
# -> some Departments have on average higher weekly sales than others

# Average weekly sales by Department and Type of store
train_set %>%
  mutate(Year = as.factor(year(Date)), Week=week(Date))%>%
  left_join(stores, by = "Store") %>% 
  group_by(Dept, Type, Week) %>%
  summarise(average_sales = mean(Weekly_Sales)) %>%
  filter(Dept %in% seq(1,9)) %>%
  ggplot(aes(x=Week, y=average_sales)) + 
  geom_line(aes(color =Type)) + 
  facet_wrap(~Dept) + 
  ggtitle("Average Weekly Sales by Department and Type of Store") +
  xlab("Week") +
  ylab("Average Sales") 
# -> Analysing a small sample of departments (1-9), we can see how the average weekly sales depends strongly on the departments. 
# -> Sales in some deparments are not influenced by the type of store
# -> Sales in some departments are not as seasonal as others

# -- Holiday --
# Trying to understand the influence of holidays on sales

# Impact of holiday on sales in general
train_set %>%
  left_join(features, by= c("Store", "Date")) %>%
  group_by(IsHoliday.x) %>%
  summarise(avg_weekly_sales = mean(Weekly_Sales)) %>%
  ggplot(aes(x=IsHoliday.x, y=avg_weekly_sales))+
  geom_bar(stat = "identity") +
  xlab("No Holiday / Holiday") +
  ylab("Average Weekly Sales") +
  ggtitle("Average weekly sales Holiday vs No-Holiday") +
  theme(plot.title = element_text(size=10))
# -> The weekly sales are on average higher during holiday periods
    
# Verifing if this pattern is the same across store 'type'
    train_set %>%
    left_join(features, by= c("Store", "Date")) %>%
    left_join(stores, by=c("Store")) %>%
    group_by(IsHoliday.x, Type ) %>%  
    summarise(avg_weekly_sales = mean(Weekly_Sales)) %>%
    ggplot(aes(x=IsHoliday.x, y=avg_weekly_sales)) +
    geom_bar(stat = "identity") + 
    facet_wrap(~Type) +
    ylab("Average Weekly Sales") +
    xlab("No Holiday / Holiday")+
    ggtitle("Average weekly sales Holiday vs No-Holiday by Type of store") +
    theme(plot.title = element_text(size=8))
# -> Weekly sales in Type C stores (smallest) seem less impacted by the holiday and no-holiday periods factor

# Promotional markdown events prominent holidays (Super Bowl, Labor Day, Thanksgiving and Christmas).

# These four holidays fall within the following weeks in the dataset (not all holidays are in the data)
# - Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
# - Labor Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
# - Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
# - Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13

# Insert manually a flag that identify the specific holiday
features <- features %>% 
  mutate(Date= as.Date(Date, format="%B %d %Y"), 
         holiday_week = ifelse(Date %in% as.Date(c("2010-02-12", "2011-02-11", "2012-02-10", "2013-02-08")), "Super Bowl",
                            ifelse(Date %in% as.Date(c("2010-09-10", "2011-09-09","2012-09-07", "2013-09-06")), "Labor Day",
                                ifelse(Date %in% as.Date(c("2010-11-26", "2011-11-25", "2012-11-23", "2013-11-29")),"Thanksgiving",
                                    ifelse(Date %in% as.Date(c("2010-12-31", "2011-12-30", "2012-12-28", "2013-12-27")),"Christmas",
                                           "No-Holiday")))))

# Understanding impact of specific holidays on Sales
train_set %>%
  left_join(features, by= c("Store", "Date")) %>%
  group_by(holiday_week) %>%
  summarise(average_week_sales = mean(Weekly_Sales)) %>%
  arrange(average_week_sales) %>%
  ggplot(aes(x=holiday_week, y=average_week_sales))+
  geom_bar(stat = "identity") +
  ggtitle("Average Weekly Sales by Holiday Week") +
  xlab("Holiday Week") +
  ylab("Average Weekly Sales")
# -> Thanksgiving appears to be the holiday period that drive most sales, 
#   while the other festivities on average drive the same amount of sales

# Understanding if this pattern is present across the store types
train_set %>%
  left_join(features, by= c("Store", "Date")) %>%
  left_join(stores, by=c("Store")) %>%
  filter(Date > as.Date("2011-11-01"), IsHoliday.x ==TRUE) %>% #Considering only Holiday with Markdown
  group_by(holiday_week, Type) %>%
  summarise(average_week_sales = mean(Weekly_Sales)) %>%
  ggplot(aes(x=holiday_week, y=average_week_sales))+
  geom_bar(stat = "identity") +
  facet_wrap(~Type)+
  ggtitle("Average Weekly Sales by Holiday Week") +
  xlab("Holiday Week") +
  ylab("Average Weekly Sales") +
  theme(axis.text.x=element_text(angle=90,hjust=1, size = 6))

# -> as expected sales in smaller stores (type C) in holiday periods follow a different pattern than the one in larger stores
# -> Thanksgiving holiday seems to have a larger impact on stores type A and B 

# Exploring the markdown feature

# Average sales when markdown promo events are running (by type of store)
train_set %>%
  left_join(features, by= c("Store", "Date")) %>%
  left_join(stores, by=c("Store")) %>%
  mutate(tot_markdown = MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5,  
         markdown_flag = ifelse(is.na(tot_markdown)== TRUE,"FALSE","TRUE")) %>%
  group_by(Type, markdown_flag) %>%
  summarise(avg_weekly_sales= mean(Weekly_Sales)) %>%
  ggplot(aes(x=markdown_flag, y=avg_weekly_sales)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Type)+
  ggtitle("Average Weekly Sales by Promo vs No-Promo") +
  xlab("Markdown Activity") +
  ylab("Average Weekly Sales") +
  theme(axis.text.x=element_text(angle=90,hjust=1, size = 6), plot.title = element_text(size=8))
# -> The presence of Markdown activities seemed to be driving sales in larger stores rather than smaller ones

# Cannot explore more the markdown feature because these are anonimous data so it is unknow what they represents. 
# In addition, these data are not available for all the years included in the training dataset


################################
# DATA PRE-PROCESSING
################################

# In this section, data will be to include processed in order to include the new features that had been engineered during the data exploration,
# Furthermore, during this stage any missing values will be treated and the multiple datesets will be joined into one

# -- Dealing with NAs - CPI and Unemployment (features dataset) --

# Index of rows with NAs
na_index <- is.na(features$CPI)
# -> Same for CPI and Unemployment rate
# -> Dates with from 2013-05-03 to 2013-07-26 

# Plotting CPI (Store 1) 
features %>% 
  filter(Store %in% c(1:9)) %>% 
  group_by(Store) %>%
  ggplot(aes(x=Date, y=CPI)) +
  geom_line() +
  facet_wrap(~Store) +
  ggtitle("CPI time series before replacing NAs (Store 1-9)")

# In this case linear regression can be used to replace the NAs

# Inizialising dataset that will contain the data from features after the pre-processing
features_post_pp <- features[,]

# -- CPI --
# Initialising dataframe that will contain predictions/ NA replacements
predicted_cpi <- data.frame(matrix(ncol = 45, nrow = 13)) 

# Loop to estimate values that will replace NAs with predictions from linear regression
i=1
for(i in 1:45) {
  fit_cpi <- features[-na_index,c(1,2,10)] %>%
  filter(Store==i) %>%
  lm(CPI ~  Date, data=.) # excluding NAs
  
  # populating the dataframe that will contains the predictions
  predicted_cpi[,i] <- features[na_index,c(1,2,10)] %>% 
    filter(Store==i) %>%
    predict(fit_cpi, newdata=.)
  i=i+1
}

# Loop to replace NAs 
i=1
start =170
for(i in 1:45) {
  loop_index <- seq(start,start+12)
  features_post_pp$CPI[loop_index] <- predicted_cpi[,i]
  start = start + 12 + 170 
  i=i+1
}

# Checking if CPI NAs have been replaced
sum(is.na(features_post_pp[,10]))

# Plotting CPI (Store 1) 
features_post_pp %>% 
  filter(Store==1) %>% 
  ggplot(aes(x=Date, y=CPI)) +
  geom_line() +
  ggtitle("CPI time series after replacing NAs (Store 1)")
# -> the chosen imputation method appears to be correct

# -- Unemployment --

# Plotting Unemployment (Store 1) 
features %>% 
  filter(Store %in% c(1:9)) %>% 
  group_by(Store) %>%
  ggplot(aes(x=Date, y=Unemployment)) +
  geom_line() +
  facet_wrap(~Store) +
  ggtitle("Unemployment time series before replacing NAs (Store 1-9)")
# -> in this case replaceing NAs values using a linear regression model wouldn't be appropriate
# -> as the Unemployment rate by store seems to be described by a floor/ceiling function
#    we can assume that the Unemployment rate stays constant throughout the time interval with NAs
#    with value equal to the last value of the Unemployment that has been recorded for that store

# Initialising dataframe that will contain predictions/ NA replacements
predicted_unmp <- data.frame(matrix(ncol = 45, nrow = 13)) 

# Loop to estimate values that will replace NAs 
i=1
for(i in 1:45) {
  # Storing last recorded value of Unemployment for store i
  last_unm <- features %>%
    filter(Store==i, 
           is.na(Unemployment)==FALSE) %>% 
    select(Unemployment) %>%
    tail(1) %>%
    pull
  
  # Populate the dataframe with the replacement for the NAs
  predicted_unmp[,i] <- rep(last_unm,13)
  i=i+1
}

# Loop to replace NAs 
i=1
start =170
for(i in 1:45) {
  loop_index <- seq(start,start+12)
  features_post_pp$Unemployment[loop_index] <- predicted_unmp[,i]
  start = start + 12 + 170 
  i=i+1
}

# Checking if Unemployment NAs have been replaced
sum(is.na(features_post_pp[,11]))

# Plotting Unemployment (Store 1)
features_post_pp %>% 
  filter(Store==1) %>% 
  ggplot(aes(x=Date, y=Unemployment)) +
  geom_line() +
  ggtitle("Unemployment time series after replacing NAs (Store 1)")
# -> The results from the replacements of the NA values appears to be satisfactory 

# Removing temporary files
rm(i, last_unm, loop_index, start, predicted_cpi, predicted_unmp, fit_cpi )


# -- Joining datasets -- 
# Adding the information from the datasets 'stores' and 'features' to the 'train' and 'test' sets
# Train set
train_set <- train_set %>% 
  left_join(stores, by ="Store",
            suffix = c("", "_new")) %>%
  left_join(features, by = c("Store","Date"),
            suffix = c("", "_new"))

# Test set
test_set <- test_set %>% 
  left_join(stores, by ="Store",
            suffix = c("", "_new")) %>%
  left_join(features, by = c("Store","Date"),
            suffix = c("", "_new"))

# Adding the features engineered in the data exploration
# Week, Month and Year of the date

train_set <- train_set %>%
  mutate(week_date = week(Date),
         month_date = month(Date),
         year_date = year(Date)
         )

test_set <- test_set %>%
  mutate(week_date = week(Date),
         month_date = month(Date),
         year_date = year(Date)
  )

# -- Dealing with categorical variables --
# Transforming 'Store', 'Type', 'Dept', size_type, holiday_week into factors for a more appropriate modelling

# Train set
train_set <- train_set %>%
  mutate(Store = as.factor(Store) ,
         Dept = as.factor(Dept),
         Type = as.factor(Type),
         size_type = as.factor(size_type),
         holiday_week = as.factor(holiday_week),
        )

# Test set
test_set <- test_set %>%
  mutate(Store = as.factor(Store) ,
         Dept = as.factor(Dept),
         Type = as.factor(Type),
         size_type = as.factor(size_type),
         holiday_week = as.factor(holiday_week),
  )

# Features post data pre-processing 
features_post_pp <- features_post_pp %>% 
  mutate(Store = as.factor(Store),
         holiday_week = as.factor(holiday_week))



################################
#  MODELLING
################################

# In this section, five different machine learning models will be implemented and compared.


# Defining LOSS FUNCTIONS - 

# Weighted mean absolute error (WMAE)
WMAE <- function(flag, actuals, predictions) {
  w_i <- ifelse(flag ==TRUE, 5,1)
  (1/sum(w_i))*  sum(w_i* abs(actuals - predictions))
}

# Root mean squared error (RMSE)
RMSE <- function(actuals, predictions){
  sqrt(mean((actuals - predictions)^2))
}

# Creating the vector that will be used to calculate the weights that indicates which weeks are holidays 
holiday_flag <- test_set$IsHoliday


# --- AVERAGE MODEL ---  (1st Model)
# The predictions will be the average weekly sales across the years 

# Creating the dataset that will contain only the features considered by the model
train_df <- train_set %>%
  select(Store, Dept, Date, Weekly_Sales, Type, holiday_week, year_date, week_date, month_date)

test_df <- test_set %>%
  select(Store, Dept, Date, Weekly_Sales, Type, holiday_week, year_date, week_date, month_date)

# Creating the dataframe with the weekly sales averages (1st round of predictions - average weekly sales)
fit_avg_week <- train_df %>%
  group_by(Store, Dept, week_date) %>% 
  summarise(predicted_sales = mean(Weekly_Sales)) 

fit_avg_week <- as.data.frame(fit_avg_week)

# Creating the matrix with the predictions 
avg_predictions <- test_df %>%
  left_join(fit_avg_week, 
            by = c('Store','Dept','week_date'),
            suffix = c("", "_new")) %>%
  select(Store,Dept,Type, Date, year_date, month_date, week_date,holiday_week, predicted_sales )
  
# Checking NAs values
sum(is.na(avg_predictions$predicted_sales))
# Storing the index 
avg_na <- is.na(avg_predictions$predicted_sales)

# -> There a few weeks in the test set whose week number was never included in the training set 
# -> For those weeks we will assign the average monthly sales

# Creating the matrix with the predictions (2nd round of predictions - average monthly sales)
fit_avg_month <- train_df %>%
  group_by(Store, Dept, month_date) %>% 
  summarise(predicted_sales = mean(Weekly_Sales)) 

# Replacing NAs 
avg_predictions[avg_na, "predicted_sales"] <- avg_predictions[avg_na,] %>%
                                                  left_join(fit_avg_month, by = c('Store','Dept','month_date'),
                                                            suffix = c("", "_new")) %>%
                                                              select(predicted_sales_new) %>%
                                                              pull()

# Checking NAs values
sum(is.na(avg_predictions$predicted_sales))
# Storing the indexes
avg_na <- is.na(avg_predictions$predicted_sales)
# -> There are still some NAs left 
# -> For those weeks the predicted values will be the average yearly sales in that specific store-department

# Creating the matrix with the predictions (3rd round of predictions - average yearly sales )
fit_avg_year <- train_df %>%
  group_by(Store, Dept, year_date) %>% 
  summarise(predicted_sales = mean(Weekly_Sales)) 

# Replacing NAs 
avg_predictions[avg_na, "predicted_sales"] <- avg_predictions[avg_na,] %>%
  left_join(fit_avg_year, by = c('Store','Dept','year_date'),
            suffix = c("", "_new")) %>%
  select(predicted_sales_new) %>%
  pull()

# Checking NAs values
sum(is.na(avg_predictions$predicted_sales))
# Storing the indexes 
avg_na <- is.na(avg_predictions$predicted_sales)
# -> few NAs left
# -> These resilient values will be replaced with the average sales in that department store accross the whole train_set

# Creating the matrix with the predictions (4th round of predictions - average sales across the train_Set)
fit_avg_tot <- train_df %>%
  group_by(Store, Dept,) %>% 
  summarise(predicted_sales = mean(Weekly_Sales)) 

# Replacing NAs 
avg_predictions[avg_na, "predicted_sales"] <- avg_predictions[avg_na,] %>%
  left_join(fit_avg_tot, by = c('Store','Dept'),
            suffix = c("", "_new")) %>%
  select(predicted_sales_new) %>%
  pull()

# Checking NAs values
sum(is.na(avg_predictions$predicted_sales))
# -> No NA values left

# Calculating the error
avg_wmae <- WMAE(holiday_flag,test_set$Weekly_Sales,avg_predictions$predicted_sales)
avg_wmae

avg_rmse <- RMSE(test_set$Weekly_Sales, avg_predictions$predicted_sales)
avg_rmse

# Building a table where we are going to store the WMAEs of all the models that we will be created to facilitate a comparison between models
error_results <- tibble(Model = c("avg model"), 
                       WMAE = c(avg_wmae), 
                       RMSE = c(avg_rmse))
error_results

# Remove temporary files
rm(fit_avg_tot, avg_na, fit_avg_month, fit_avg_week)


# --- LINEAR MODEL --- (2nd model)

# Features in the model: Store, Dept, Weekly_Sales, Date, Fuel_Price, Unemployment, Temperature

# The caret package cannot really handle categorical variables (Stores and Departments) there is a way around.
# two 'for' loops can be used to fit regression models for every department d in store s

# Creating the dataframe that contains only the predictors and that will be the data source of the train function 
train_df <- train_set %>%
  select(Store, Dept, Weekly_Sales, Date, Fuel_Price, Unemployment, Temperature, CPI, year_date, week_date ) 

# Setting up the test test in the same way
test_df  <- test_set %>%
  select(Store, Dept, Weekly_Sales, Date, Fuel_Price, Unemployment, Temperature, CPI, year_date, week_date)

# Initialising the indexes for the loops
s=1 # Index for the stores
d=1 # Index for the department

# Initialising dataframe that will contain all predictions
predictions_outcome <- data.frame(matrix(nrow=0, ncol=5))
colnames(predictions_outcome) = c("Store", "Dept", "Date", "Weekly_Sales", "Predicted Sales")

# ncol = 5 because the columns will be "Store", "Dept", "Date", "Weekly_Sales", "Predicted Sales"
# nrow = nrow(test_df) because for each entry of test_df a prediction will be calculated

for (s in 1:45) {
  
  # Retrieving the list of deparments of store s
  dept <- train_df %>%
    filter(Store==s) %>%
    select(Dept) %>%
    distinct() %>%
    pull()
  
  # Loop to fit a linear regression model for each department d in store s
  for (d in dept) {
    set.seed(87, sample.kind="Rounding")
    
    fit_lm <- train_df %>%
      filter(Store==s, 
             Dept==d) %>%
      train(Weekly_Sales ~ year_date + week_date + Fuel_Price + Unemployment + Temperature + CPI, 
            data=.,
            method="lm")
    
    store_s_dept_d_df <- test_df %>%
      filter(Store==s, 
             Dept==d)
    
    predict_lm <- store_s_dept_d_df %>%
      predict(fit_lm, 
              newdata=.) 
    
    # Populating  dataset with predictions and features
    predictions_lm_store_s_dept_d <- cbind(store_s_dept_d_df[,c("Store", "Dept", "Date", "Weekly_Sales")], predict_lm)
    # -> we are including Weekly sales for the time being to facilitate the calculation of the error
    
    predictions_outcome <- rbind(predictions_outcome,predictions_lm_store_s_dept_d)
  }
}

# Calculating the errors

# To be able to calculate the wmae we first need to lookup the relevant holiday_flag
predictions_outcome_plus <- predictions_outcome %>% 
  left_join(features_post_pp, by = c("Store", "Date"))

# Checking NAs
sum(is.na(predictions_outcome_plus$isHoliday))
holiday_flag_lm <- predictions_outcome_plus$IsHoliday

lm_wmae <- WMAE(holiday_flag_lm,predictions_outcome_plus$Weekly_Sales,predictions_outcome_plus$predict_lm)
lm_wmae

lm_rmse <- RMSE(predictions_outcome_plus$Weekly_Sales,predictions_outcome_plus$predict_lm )
lm_rmse

# Adding this WMAE to the table where we are storing all WMAE previously calculated
error_results <- tibble(Model = c("Avg model", "Linear model"), 
                        WMAE = c(avg_wmae,lm_wmae),
                        RMSE = c(avg_rmse, lm_rmse))
error_results


# --- K- NEAREST NEIGHBORS (KNN) --- (3rd model)

# Features in the model: Store, Department, year_date, week_date

# Creating the dataframe that contains only the predictors that will be the data source of the train function 
train_df <- train_set %>%
  select(Store, Dept, Weekly_Sales, Date, Fuel_Price, 
         Unemployment, Temperature, CPI, year_date, week_date )

# Setting up the test test in the same way
test_df  <- test_set %>%
  select(Store, Dept, Weekly_Sales, Date, Fuel_Price, 
         Unemployment, Temperature, CPI, year_date, week_date ) 

# Initialising the indexes for the loops
s=1 # Index for the stores
d=1 # Index for the department

# Initialising dataframe that will contain all predictions
predictions_outcome <- data.frame(matrix(nrow=0, ncol=5))
colnames(predictions_outcome) = c("Store", "Dept", "Date", "Weekly_Sales", "Predicted Sales")

# ncol = 5 because the columns will be "Store", "Dept", "Date", "Weekly_Sales", "Predicted Sales"
# nrow = nrow(test_df) because for each entry of test_df a prediction will be calculated


# Initialising the indexes for the loops
s=1 # Index for the stores
d=1 # Index for the department

# Initialising dataframe that will contain all predictions
predictions_outcome <- data.frame(matrix(nrow=0, ncol=5))
colnames(predictions_outcome) = c("Store", "Dept", "Date", "Weekly_Sales", "Predicted Sales")

# ncol = 5 because the columns will be "Store", "Dept", "Date", "Weekly_Sales", "Predicted Sales"
# nrow = nrow(test_df) because for each entry of test_df a prediction will be calculated

for (s in 1:45) {
  
  # Retrieving the list of deparments of store s
  dept <- train_df %>%
    filter(Store==s) %>%
    select(Dept) %>%
    distinct() %>%
    pull()
  
  # Loop to fit a knn regression model for each department d in store s
  for (d in dept) {
    set.seed(87, sample.kind="Rounding")
    
    fit_knn_dept <- train_df %>%
      filter(Store==s, 
             Dept==d) %>%
      train(Weekly_Sales ~ year_date + week_date + Fuel_Price + Unemployment + Temperature + CPI, 
            data=.,
            method="knn",
            trControl = trainControl("cv", number = 10),
            preProcess = c("center","scale"),
            tuneLength = 10)
    
    store_s_dept_d_df <- test_df %>%
      filter(Store==s, 
             Dept==d)
    
    predict_knn_dept <- store_s_dept_d_df %>%
      predict(fit_knn_dept, 
              newdata=.) 
    
    # Populating  dataset with predictions and features
    predictions_store_s_dept_d <- cbind(store_s_dept_d_df[,c("Store", "Dept", "Date", "Weekly_Sales")], predict_knn_dept)
    # -> we are including Weekly sales for the time being to facilitate the calculation of the error
    
    predictions_outcome <- rbind(predictions_outcome,predictions_store_s_dept_d)
    
    # Updating the index so that the next prediction will be stored in the next available empty row
  }
}

# Calculating the errors

# To be able to calculate the wmae we first need to lookup the relevant holiday_flag
predictions_outcome_plus <- predictions_outcome %>% 
  left_join(features_post_pp, by = c("Store", "Date"))

# Checking NAs
sum(is.na(predictions_outcome_plus$isHoliday))
holiday_flag_knn <- predictions_outcome_plus$IsHoliday

knn_wmae <- WMAE(holiday_flag_knn,predictions_outcome_plus$Weekly_Sales,predictions_outcome_plus$predict_knn_dept)
knn_wmae

knn_rmse <- RMSE(predictions_outcome_plus$Weekly_Sales,predictions_outcome_plus$predict_knn_dept )
knn_rmse

# Adding this WMAE to the table where we are storing all WMAE previously calculated
error_results <- tibble(Model = c("Avg model", "Linear model", "KNN model"), 
                        WMAE = c(avg_wmae,lm_wmae, knn_wmae),
                        RMSE = c(avg_rmse, lm_rmse, knn_rmse))
error_results


# Improving KNN  by removing some features ---

# Initialising the indexes for the loops
s=1 # Index for the stores
d=1 # Index for the department

# Initialising dataframe that will contain all predictions
predictions_outcome <- data.frame(matrix(nrow=0, ncol=5))
colnames(predictions_outcome) = c("Store", "Dept", "Date", "Weekly_Sales", "Predicted Sales")

for (s in 1:45) {
  
  # Retrieving the list of deparments of store s
  dept <- train_df %>%
    filter(Store==s) %>%
    select(Dept) %>%
    distinct() %>%
    pull()
  
  # Loop to fit a knn regression model for each department d in store s
  for (d in dept) {
    set.seed(87, sample.kind="Rounding")
    
    fit_knn_dept <- train_df %>%
                    filter(Store==s, 
                           Dept==d) %>%
                    train(Weekly_Sales ~ year_date + week_date , 
                    data=.,
                    method="knn",
                    trControl = trainControl("cv", number = 10),
                    preProcess = c("center","scale"),
                    tuneLength = 20)
    
    store_s_dept_d_df <- test_df %>%
                          filter(Store==s, 
                          Dept==d)
    
    predict_knn_dept <- store_s_dept_d_df %>%
                        predict(fit_knn_dept, 
                                newdata=.) 
    
    # Populating  dataset with predictions and features
    predictions_store_s_dept_d <- cbind(store_s_dept_d_df[,c("Store", "Dept", "Date", "Weekly_Sales")], predict_knn_dept)
    # -> we are including Weekly sales for the time being to facilitate the calculation of the error
    
    predictions_outcome <- rbind(predictions_outcome,predictions_store_s_dept_d)
    
    # Updating the index so that the next prediction will be stored in the next available empty row
  }
}

# Calculating the errors

# To be able to calculate the wmae we first need to lookup the relevant holiday_flag
predictions_outcome_plus <- predictions_outcome %>% 
                            left_join(features_post_pp, by = c("Store", "Date"))
  
# Checking NAs
sum(is.na(predictions_outcome_plus$isHoliday))
holiday_flag_knn <- predictions_outcome_plus$IsHoliday

knn_wmae_imp <- WMAE(holiday_flag_knn,predictions_outcome_plus$Weekly_Sales,predictions_outcome_plus$predict_knn_dept)
knn_wmae_imp

knn_rmse_imp <- RMSE(predictions_outcome_plus$Weekly_Sales,predictions_outcome_plus$predict_knn_dept )
knn_rmse_imp

# Adding this WMAE to the table where we are storing all WMAE previously calculated
error_results <- tibble(Model = c("Avg model", "Linear model", "KNN model", "KNN improved model"), 
                        WMAE = c(avg_wmae,lm_wmae, knn_wmae, knn_wmae_imp),
                        RMSE = c(avg_rmse, lm_rmse, knn_rmse, knn_rmse_imp))
error_results


# --- ARIMA --- (5th model)

# Creating the dataframe that contains only the predictors and that will be the data source of the train function 
train_ts <- train %>%
  select(Store, Dept, Date, Weekly_Sales,IsHoliday) 

# Splitting into train and test set. Observations taken in 2012 will be used as the test set
test_df <- train_ts %>%
  filter(Date >= as.Date("2012-01-01"))

train_df <- train_ts %>%
  filter(Date < as.Date("2012-01-01"))

# Calculating test / train ratio
nrow(test_df) / (nrow(test_df)+ nrow(train_df))
# -> the test set is 30% of the train set

# Check that every combination department-store have the same amount of observations with no missing sales for any week
train_df %>% 
  group_by(Store,Dept) %>%
  summarise(n_row = n()) %>% 
  ggplot(aes(x=n_row)) + geom_histogram()

train_df %>% 
  group_by(Store,Dept) %>%
  summarise(n_row = n()) %>% 
  arrange(-desc(n_row))
# -> sales data for certain stores are only available for the smaller time periods

# Initialising the indexes needed for the loops
s=1
d=1
# Initialising the dataframe where the predictions will be stored
predictions_outcome_arima <- data.frame(matrix(nrow=0, ncol=6))

for (s in 1:45) {
  
  # Retrieving the list of deparments of store s
  dept <- train_df %>%
    filter(Store==s) %>%
    select(Dept) %>%
    distinct() %>%
    pull()
  
  # Loop to fit a Arima model for each department d in store s
  for (d in dept) {
    set.seed(87, sample.kind="Rounding")
    
    # Extracting real sales for this department d and store s (to use for calculating the error)
    test_s_d <- test_df  %>%
      filter(Store==s, Dept==d) %>%
      arrange(Date) %>%
      select(Store, Dept, Date, Weekly_Sales, IsHoliday) 
    
    # Counting number of weeks to predict
    n_weeks <- nrow(test_s_d)
    
    # Retrieving first available date (test set)
    min_date <- train_df %>%
      filter(Store==s, Dept==d) %>%
      select(Date) %>%
      summarise(max_date=min(Date)) %>%
      pull()
    
    # Extracting the weekly sales sorted by dates
    arima_rawdata <- train_df %>%
      filter(Store==s, 
             Dept==d) %>%
      arrange(Date) %>%
      select( Weekly_Sales)
    
    # Converting the data into time series data format 
    tsData <- ts(arima_rawdata, 
                 start = c(year(min_date),month(min_date), day(min_date)), 
                 frequency = 52) # weekly data
    
    # Fitting Arima model
    fit_arima <- auto.arima(tsData)
    
    # Making Predictions (only if the department d - store s combination is included within the test set)
    if (n_weeks>0) {
    sales_forecast_model <- forecast(fit_arima, h=n_weeks)
    
    predicted_sales <- as.numeric(sales_forecast_model$mean)
    
    # Populating  dataset with predictions and features
    predictions_store_s_dept_d <- cbind(test_s_d, as.data.frame(predicted_sales))
    # -> we are including Weekly sales for the time being to facilitate the calculation of the error
    
    # storing those predictions
    predictions_outcome_arima <- rbind(predictions_outcome_arima,predictions_store_s_dept_d)}
    
  }
}

# Checking NAs
sum(is.na(predictions_outcome_arima))

# Calculating the errors
arima_wmae <- WMAE(predictions_outcome_arima$IsHoliday,predictions_outcome_arima$Weekly_Sales,predictions_outcome_arima$predicted_sales)
arima_wmae

arima_rmse <- RMSE(predictions_outcome_arima$Weekly_Sales,predictions_outcome_arima$predicted_sales )
arima_rmse


# Revisiting Model 2 - linear regression - to allow a comparison with the ARIMA model

# Creating the dataframe that contains only the predictors and that will be the data source of the train function 
train_ts <- train %>%
  left_join(stores, by ="Store",
            suffix = c("", "_new")) %>%
  left_join(features, by = c("Store","Date"),
            suffix = c("", "_new"))  %>%
  # Adding the features engineered in the data exploration
  mutate(week_date = week(Date),
         month_date = month(Date),
         year_date = year(Date)) %>%
  mutate(Store = as.factor(Store) ,
         Dept = as.factor(Dept),
         Type = as.factor(Type),
         size_type = as.factor(size_type),
         holiday_week = as.factor(holiday_week)) %>%
  select(Store, Dept, Weekly_Sales, Date, Fuel_Price, Unemployment, Temperature, CPI, year_date, week_date ) 
  
# Splitting into train and test set. Observations taken in 2012 will be used as the test set
test_df <- train_ts %>%
  filter(Date >= as.Date("2012-01-01"))

train_df <- train_ts %>%
  filter(Date < as.Date("2012-01-01"))

# Initialising the indexes for the loops
s=1 # Index for the stores
d=1 # Index for the department

# Initialising dataframe that will contain all predictions
predictions_outcome <- data.frame(matrix(nrow=0, ncol=5))
colnames(predictions_outcome) = c("Store", "Dept", "Date", "Weekly_Sales", "Predicted Sales")

# ncol = 5 because the columns will be "Store", "Dept", "Date", "Weekly_Sales", "Predicted Sales"
# nrow = nrow(test_df) because for each entry of test_df a prediction will be calculated

for (s in 1:45) {
  
  # Retrieving the list of deparments of store s
  dept <- train_df %>%
    filter(Store==s) %>%
    select(Dept) %>%
    distinct() %>%
    pull()
  
  # Loop to fit a linear regression model for each department d in store s
  for (d in dept) {
    set.seed(87, sample.kind="Rounding")
    
    fit_lm <- train_df %>%
      filter(Store==s, 
             Dept==d) %>%
      train(Weekly_Sales ~ year_date + week_date + Fuel_Price + Unemployment + Temperature + CPI, 
            data=.,
            method="lm")
    
    store_s_dept_d_df <- test_df %>%
      filter(Store==s, 
             Dept==d)
    
    predict_lm <- store_s_dept_d_df %>%
      predict(fit_lm, 
              newdata=.) 
    
    # Populating  dataset with predictions and features
    predictions_lm_store_s_dept_d <- cbind(store_s_dept_d_df[,c("Store", "Dept", "Date", "Weekly_Sales")], predict_lm)
    # -> we are including Weekly sales for the time being to facilitate the calculation of the error
    
    predictions_outcome <- rbind(predictions_outcome,predictions_lm_store_s_dept_d)
  }
}

# Calculating the errors

# To be able to calculate the wmae we first need to lookup the relevant holiday_flag
predictions_outcome_plus <- predictions_outcome %>% 
  left_join(features_post_pp, by = c("Store", "Date"))

# Checking NAs
sum(is.na(predictions_outcome_plus$isHoliday))
holiday_flag_lm <- predictions_outcome_plus$IsHoliday

lm_v2_wmae <- WMAE(holiday_flag_lm,predictions_outcome_plus$Weekly_Sales,predictions_outcome_plus$predict_lm)
lm_v2_wmae

lm_v2_rmse <- RMSE(predictions_outcome_plus$Weekly_Sales,predictions_outcome_plus$predict_lm )
lm_v2_rmse

# Creating the tables that contains the errors of the models that used the second split test-train set
error_results_2 <- tibble(Model = c("ARIMA", "Linear model (revisited)"), 
                        WMAE = c(arima_wmae, lm_v2_wmae),
                        RMSE = c(arima_rmse, lm_v2_rmse))

error_results_2

##### ATTENTION ######
# The following code requires high computational resources, hence, time to run. Make sure to have a powerful workstation 
# before uncommenting and run the code below


# --- RANDOM FOREST --- (4th model)

# -> Features used as predictors: Store, Dept, Weekly_Sales, Type, holiday_week, year_date, week_date, _month_date
# -> With machine with high computational power, the features Temperature, CPI and Unemployment could also be included

# Creating the dataframes that contain only the predictors and that will be the source data of the train function
# train_df <- train_set %>%
#          select(Store, Dept, Weekly_Sales, Type, holiday_week, year_date, week_date, month_date )
# test_df  <- test_set %>%
#           select(Store, Dept, Weekly_Sales, Type, holiday_week, year_date, week_date, month_date )

# Defining the control (10-folds cross validation)
#trControl <- trainControl(method = "cv", number = 10, search ="grid")


# Random Forest Models ----

# RF Model 1 - Default Settings

# -- Training --
# set.seed(87, sample.kind="Rounding")
# train_rf_def <- train(Weekly_Sales ~ .,
#                       data=train_df,
#                       method = "rf",
#                       importance= TRUE,
#                       trControl = trControl
# )

# -- Testing --
# Making predictions
# predicted_sales <- predict(train_rf_def, test_df)

# Calculating the error
# WMAE(holiday_flag,test_set$Weekly_Sales,predicted_sales)


# RF Model 2 - Tuning best mtry

# tuneGrid <- expand.grid(.mtry = c(1: 10))
# The model will be tested for values of mtry from 1 to 10

# -- Training --
# set.seed(87, sample.kind="Rounding")
# train_rf_2 <- train(Weekly_Sales ~ .,
#                       data=train_df,
#                       method = "rf",
#                       importance= TRUE,
#                       trControl = trControl,
#                       tuneGrid = tuneGrid
# )

# Best value of mtry
# train_rf_2$bestTune$mtry

# Store this value of mtry to used it in future tuning
# best_mtry <- train_rf_2$bestTune$mtry


# -- Testing --
# Making predictions
# predicted_sales <- predict(train_rf_2, test_df)

# Calculating the error
# WMAE(holiday_flag,test_set$Weekly_Sales,predicted_sales)


# RF Model 3 - Tuning best maxnodes

# Initialising the list where the random forest models trained for each value of max node will be stored
# store_maxnode <- list()

# Setting the tuneGrid to use the best value of mtry previously found
# tuneGrid <- expand.grid(.mtry = best_mtry)

# -- Training --
# set.seed(87, sample.kind="Rounding")

# Creating the loop which will fit a random forest model for each value of maxnodes from 5 to 15

# for (maxnodes in c(5: 15)) {
#   train_rf_maxnode <- train(Weekly_Sales ~ .,
#                       data=train_df,
#                       method = "rf",
#                       importance= TRUE,
#                       trControl = trControl,
#                       tuneGrid = tuneGrid,
#                       maxnodes = maxnodes)
#   current_iteration <- toString(maxnodes)
#   store_maxnode[[current_iteration]] <- rf_maxnode
# }

# results_mtry <- resamples(store_maxnode) # Arranging the result of the model
# summary(results_mtry)

# From the summary the value of maxnodes that maximise the accuracy can be selected, stored in the varianble 'tuned_maxnodes' and use to make the predictions

# train_rf_maxnode  <- train(Weekly_Sales ~ .,
#                       data=train_df,
#                       method = "rf",
#                       importance= TRUE,
#                       trControl = trControl,
#                       tuneGrid = tuneGrid,
#                       maxnodes = tuned_maxnodes)

# -- Testing --
# Making predictions
# predicted_sales <- predict(train_rf_maxnode, test_df)

# Calculating the error
# WMAE(holiday_flag,test_set$Weekly_Sales,predicted_sales)


# RF Model 4 - Tuning best ntrees
# The tuning of the max number of ntrees follow the same procedure used to tune max nodes.

# Initialising the list where the random forest models trained for each value of max node will be stored
# store_maxtrees <- list()

# Setting the tuneGrid to use the best value of mtry previously found
# tuneGrid <- expand.grid(.mtry = best_mtry)

# -- Training --
# set.seed(87, sample.kind="Rounding")

# Creating the loop which will fit a random forest model for each value of ntree

# for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
#   train_rf_maxtrees <- train(Weekly_Sales ~ .,
#                       data=train_df,
#                       method = "rf",
#                       importance= TRUE,
#                       trControl = trControl,
#                       tuneGrid = tuneGrid,
#                        ntrees = ntree
#                       maxnodes = tuned_maxnodes)
#   current_iteration <- toString(ntrees)
#   store_ntrees[[current_iteration]] <- rf_maxtrees
# }

# results_tree <- resamples(store_maxtrees) # Arranging the result of the model
# summary(results_tree)

# From the summary the value of ntree that maximise the accuracy can be selected, stored in the varianble 'tuned_ntree' and use to make the predictions

# train_rf_ntree  <- train(Weekly_Sales ~ .,
#                       data=train_df,
#                       method = "rf",
#                       importance= TRUE,
#                       trControl = trControl,
#                       tuneGrid = tuneGrid,
#                       maxnodes = tuned_maxnodes
#                       ntree= tuned_ntree
# )

# -- Testing --
# Making predictions
# predicted_sales <- predict(train_rf_ntree, test_df)


# Calculating the error
# WMAE(holiday_flag,test_set$Weekly_Sales,predicted_sales)

