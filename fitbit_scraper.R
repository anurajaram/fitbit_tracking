
# Author - Anupama Rajaram
# Description - Program to extract data from Fitbit site.
#               Note, we are only downloading data that belongs to us,
#               by using authorized login credentials (username & password)
# Date - Dec 23, 2016


# load standard libraries 
source("workspace_prep.R")

# load other libraries
library(fitbitScraper)
library(lubridate)
library(plyr)
library(dplyr)


# we will authorize our program with Fitbit site credentials.
cookie = login("<myemail@gmail.com>", "<add_password>", rememberMe = TRUE)


# Extract sleepdata
x = get_sleep_data(cookie, start_date = "2016-11-01", end_date = "2016-12-27")
# the result is a list 
sleepdf = x$df
varofinterest = c("date", "sleepDuration", "awakeCount", "restlessCount",
                  "awakeDuration", "restlessDuration", "sleepBucketTextA")
sleep_data = sleepdf[, varofinterest]
sleep_data$datebase = as.Date(sleep_data$date, "%Y-%m-%d")


# Get heart-rate related data specific to certain variables.
yheart = get_daily_data(cookie, what = "getTimeInHeartRateZonesPerDay",
                        start_date = "2016-11-01", end_date = "2016-12-27")

yheart2 = get_daily_data(cookie, what = "getRestingHeartRateData", 
                        start_date = "2016-11-01", end_date = "2016-12-27")

yheart3 = get_daily_data(cookie, what = "minutesVery", 
                         start_date = "2016-11-01", end_date = "2016-12-27")
# the dataset looks like "minutes_very_active.jpg"


# This data is stored under the filename anu_sleepdata.csv
write.csv(sleep_data, "sleepdata.csv", row.names = FALSE)                           

