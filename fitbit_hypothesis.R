
# Author - Anupama Rajaram
# Description - Hypothesis testing from Fitbit Data
# Date - Dec 23, 2016

# load the files - this was created from the program fitbit_data.R
# this file is available under the name "fitbitfinaldata.csv" 
fitbit_data_final = data.frame(fread("fitbitfinaldata.csv"))




# ========================================================================
# Hypothesis testing & Data Visualization
# ========================================================================
summary(fitbit_data_final[, c(3:8)])



# 1) Relation between steps and numberof awakenings
chisq.test(fitbit_data_final$Steps, fitbit_data_final$Number.of.Awakenings)
# relation exists as pval = 4e-7

qplot(fitbit_data_final$Steps, fitbit_data_final$Number.of.Awakenings, 
      color = fitbit_data_final$weekday2,
      xlab = "number of steps/day",
      ylab = "Number of Times Awake in Night")
# image stored as "steps_awake.jpg"

qplot(fitbit_data_final$Steps, fitbit_data_final$Number.of.Awakenings, 
      color = fitbit_data_final$sleepBucketTextA, 
      xlab = "number of steps/day",
      ylab = "Number of Times Awake in Night",
      main = "Do daily steps affect sleep quality?")
# image stored as "steps_sleep_quality.jpg"


# 2) Relation between steps and restless counts
chisq.test(fitbit_data_final$Steps, fitbit_data_final$restlessCount)
# NO relation exists as pval = 0.9
qplot(fitbit_data_final$Steps, fitbit_data_final$restlessCount)
# image stored as "steps_restless.jpg"


# 2b) Relation between calories and restless counts
chisq.test(fitbit_data_final$Calories.Burned, fitbit_data_final$restlessCount)
# NO relation exists as pval = 0.7



# 3) Relation between steps and sleep quality
chisq.test(fitbit_data_final$Steps, fitbit_data_final$sleepBucketTextA)
# no relation so far = 0.8



# 4) Relation between weekday and sleep quality
chisq.test(fitbit_data_final$weekday2, fitbit_data_final$sleepBucketTextA)
# no relation so far = 0.3
# i had assumed weekends would show better sleep quality



# 5) Relation between weekday and number_of_steps
fit2 <- aov(restlessCount ~ weekday2 + Steps, data=fitbit_data_final)
fit2 
summary(fit2)
# no relation so far as p-vals >0.05

# plotting the result also shows that the data is randomly distributed.
plot(fit2)
# image for residuals stored as "anova.jpg"



# 6) Relation between restlessCount/night and sleep_quality
chisq.test(fitbit_data_final$restlessCount,
           fitbit_data_final$sleepBucketTextA)

# p = 0.1, no relationship but it is very close! 
# plotting the data gives us a better understanding.
qplot(fitbit_data_final$restlessCount, 
      fitbit_data_final$sleepBucketTextA,
      xlab = "number of movements during night",
      ylab = "Sleep Quality",
      main = "Does moving during sleep affect sleep quality?")
# image saved as "move_sleepq.jpg"
# it looks like I've actually had good sleep when my number of restless_counts
# exceeded 15 or more! all my days of "poor" sleep where days when my restless 
# count were 10 or less!
# This may seem counter-intuitive, but from personal experience (and assuming 
# Fitbit data classification to be accurate) I know that on the days when I am 
# stressed out, I sleep like a robot in one position throughout the night. 
# On other "normal" nights, moving during the night is very normal for me, even 
# with calm REM sleep.



# this plot tries to visually check if sleep quality has been degrading 
# over time, or if there were certain stretches of days with low sleep_quality
qplot(fitbit_data_final$datebase, fitbit_data_final$Steps, 
      color = fitbit_data_final$sleepBucketTextA)



# chisquare test for 3 variables
mytable <- xtabs(~sleepBucketTextA + Number.of.Awakenings + mth, 
                 data = fitbit_data_final)
mantelhaen.test(mytable)
# no relation as p>0.05



# 7) Relation between day-of-week and calories
chisq.test(fitbit_data_final$weekday2, fitbit_data_final$Calories.Burned)
# yes relation exists, pval = 0.01
qplot(fitbit_data_final$weekday2, fitbit_data_final$Calories.Burned)
# weekends definitely show higher calorie burns



# 8) Relation between day-of-week and steps
chisq.test(fitbit_data_final$weekday2, fitbit_data_final$Steps)
# no relation so far = 0.01


 
# 9) Relation between steps and number-of-awake
cor(fitbit_data_final$Steps, fitbit_data_final$Number.of.Awakenings)
# low correlation = 0.3469
plot(fitbit_data_final$Steps, fitbit_data_final$Number.of.Awakenings)




# ========================================================================
# Other Data Visualization
# ========================================================================
library(corrgram)
corrgram(fitbit_data_final, upper.panel = NULL )
# corrgram(fitbit_data_final, upper.panel = NULL , 
#          col = colorRampPalette(c("firebrick3", "white", "navy")))
corrgram(fitbit_data_final[,c(2:17)], order =TRUE, lower.panel = panel.shade, 
         upper.panel = panel.pie )
# chart is not really meaningful for our data, but it is a quick visual
# check to see if there are relationships that we have missed.
# image saved as "corrgram.jpg"



pairs(~Calories.Burned + Steps + Distance + Minutes.Sedentary +
        Minutes.Lightly.Active , data=fitbit_data_final, 
      main="Simple Scatterplot Matrix")
# image saved as "plot_relationships.jpg"












