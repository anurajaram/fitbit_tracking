# Author - Anupama Rajaram
# Date - Dec 23, 2016
# Description - Analyze Fitbit Health Data
# Dataset - Bunch of files downloaded from Fitbit website (needs login)
#           Data includes Sleep tracking, and fitness activities data.
#           Files include data for only 2 months (Nov, Dec). 
#           Some data is normalized, while some are fictitious, done 
#           to preserve personal information.



# suppress scientific notation e.g.: 2.4e5 instead 2,400,367 
options(scipen = 999)

# decimal places - only 4
options(digits = 4)

# call source file with all required packages.
library("data.table")
library(httr)
library(jsonlite)
library(rjson)
library(RJSONIO)



# Data file path - since we may have multiple files in the folder.
# Please update the path to the appropriate folder in your computer.
foldernamearr = c("C:/anu/ja/jan2017/fitbit_project/datafiles/")
# the datafolder provided should have 4 files, 2 each for sleep and exercise.

# note, we downloaded monthly data from the fitbit site for activities (exercise)
# and sleep. Although you can download the data together, reading such a file is
# bit difficult.
# Exercise files are named in the format = fitbit_activities_dec2016.
# Sleepdata files are named in the format = fitbit_sleep_nov2016.
# The suffix indicates month/year. 


monthnamearr = c("jan", "feb", "mar", "apr", "may", "jun",
                 "jul", "aug", "sep", "oct", "nov", "dec")
yearstartarr = c(2016, 2017)
filesuffix2 = ".csv"

activityarr = "fitbit_activities_"
sleeparr = "fitbit_sleep_"


# creating an array for all the monthyear suffixes
mthyeararr = c()  # initialize
ptrval = 1        # initialize


# we combine data from all the files.
for(yr in 1:length(yearstartarr )){
  
  for(mth in 1:length(monthnamearr)){
    #print(paste("val of ptr =", ptrval))
    mthyeararr[ptrval] = paste(monthnamearr[mth], yearstartarr[yr], sep = "" )
    ptrval = ptrval + 1
  }
}

# remove unwanted variables.
rm(monthnamearr, yearstartarr, mth, yr, ptrval)



# store data from all sleep activity files
sleepdatadf = data.frame()

for(ptrval in 1:length(mthyeararr) ){
  
  filepatharr = paste(foldernamearr, sleeparr, mthyeararr[ptrval], filesuffix2,
                   sep = "")
  fileflagval = file.exists(filepatharr)
  
  if(fileflagval == TRUE){
    tempdf = data.frame(fread(filepatharr, stringsAsFactors = FALSE))
    sleepdatadf = rbind(sleepdatadf, tempdf)
  }
}
# sleepdatadf has observations for 57 days (nov/dec 2016)



# similarly, store data from all exercise activity files
# store data from all sleep activity files
exercisedatadf = data.frame()

for(ptrval in 1:length(mthyeararr) ){
  
  filepatharr = paste(foldernamearr, activityarr, mthyeararr[ptrval], filesuffix2,
                      sep = "")
  fileflagval = file.exists(filepatharr)
  
  if(fileflagval == TRUE){
    tempdf = data.frame(fread(filepatharr, stringsAsFactors = FALSE,
                              skip = 1))
    # the skip option in the above file will ensure that the first row is skipped, 
    # since it does not contain any meaningul data.
    
    exercisedatadf = rbind(exercisedatadf, tempdf)
  }
}
# exercisedatadf has observations for 57 days (nov/dec 2016)


# create a merged dataset with both exercise and sleep data
fitbitdf = merge(exercisedatadf, sleepdatadf, by = "Date", all.x = TRUE)




# ========================================================================
# Clean up data & Derived variables
# ========================================================================

# convert all character variables to numeric.
for( i in 2:ncol(fitbitdf)){

  tempdf = fitbitdf[,i]
  
  if(class(tempdf) == "character"){
    
    print(paste("updating column", colnames(fitbitdf[i])))
    tempdf = gsub(",", "", tempdf)
    tempdf = as.integer(tempdf)
    fitbitdf[, i] = tempdf
  }
}


# convert "date" field to date format. this will help us extract the
# month, year and dayof-week.
fitbitdf$datebase = as.Date(fitbitdf$Date, "%m/%d/%Y")
fitbitdf$mth = as.numeric(format(fitbitdf$datebase, "%m"))
fitbitdf$year = as.numeric(format(fitbitdf$datebase, "%Y"))   
fitbitdf$weekday = weekdays(fitbitdf$datebase)
fitbitdf$weekday2 = factor(fitbitdf$weekday, 
                              levels = c("Monday", "Tuesday", "Wednesday",
                                         "Thursday", "Friday", "Saturday",
                                         "Sunday"))

# this file is stored as "charge2_trackerdata.csv"
# write.csv(fitbitdf, "charge2_trackerdata.csv", row.names = FALSE)

fitbitdf$weeknum <- as.numeric( format(fitbitdf$datebase+3, "%U"))


# we will take the file with the sleeptracking data (restless counts, no. of
# times awake and merge with this file)
# the sleeptracking file was scraped from the fitbit site, as explained in
# file "fitbit_scraper.R"
sleep_data = data.frame(fread("sleepdata.csv"))

fitbitdf_final = merge(fitbitdf, sleep_data, by = "datebase",
                    all.x = TRUE)


# this datafile is stored as "fitbitfinaldata.csv"
write.csv(fitbitdf_final, "fitbitfinaldata.csv", row.names = FALSE)

