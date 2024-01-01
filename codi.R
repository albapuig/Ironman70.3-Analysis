#FINAL ASSIGNMENT

#Alba Puig Font
#NIU: 1636034

# Set working directory and load required packages
setwd("C:/Users/albap/OneDrive/Escriptori/UNIVERSITAT/Anàlisi de Dades Complexes/FINAL ASSIGNMENT")
library("readxl")

# Read the data from CSV file
data <- read.csv("Half_Ironman_df6.csv")

# Remove variables
data <- subset(data, select = -c(Transition1Time, Transition2Time, CountryISO2))
summary(data)
head(data)
View(data)

#-------------------------------------------------------------------------------------
#FIT THE MODEL
#--------------------------------------------------------------------------------------
# Perform random sampling to reduce the dataset size
sampled_data <- data[sample(nrow(data), size = 10000, replace = FALSE),]

# View the summary of the sampled data
summary(sampled_data)

#Now, we can design a model to be analysed for the dataset:
model_full<-lm(FinishTime ~.,data=sampled_data)
summary(model_full)

# Fit the full model using BIGLM
library(biglm)
model_full_biglm <- biglm(formula = FinishTime ~ Gender + AgeGroup + AgeBand + Country + EventYear + EventLocation + SwimTime + BikeTime + RunTime, data = data)

#---------------------------------------------------------
# Descriptive statistics
#---------------------------------------------------------

#Maximum, minimum and Mean of the values

#SwimTime
min(data$SwimTime)/60
max(data$SwimTime)/60
mean(data$SwimTime)/60

#BikeTime
min(data$BikeTime)/60
max(data$BikeTime)/60
mean(data$BikeTime)/60

#RunTime
min(data$RunTime)/60
max(data$RunTime)/60
mean(data$RunTime)/60

#FinishTime
min(data$FinishTime)/60
max(data$FinishTime)/60
mean(data$FinishTime)/60

#---------------------------------------
#Frequency of countries

# Calculate the frequency of each country
country_freq <- table(data$Country)

# Sort the frequencies in descending order
sorted_freq <- sort(country_freq, decreasing = TRUE)

# Subset the sorted frequencies table to include only the top N countries
top_countries <- head(sorted_freq, n = 10)
top_countries
# Create a pie chart of the number of people per country for the top countries
pie(top_countries, main = "Number of People per Country", labels = names(top_countries))

#----------------------------------------------
#Frequency of AgeGroups

# Calculate the frequency of each country
agegroup_freq <- table(data$AgeGroup)

# Sort the frequencies in descending order
sorted_freq <- sort(agegroup_freq, decreasing = TRUE)

# Select only the top 5 countries
top10_agegroup <- sorted_freq[1:10]
top10_agegroup

# Create a bar plot of the number of Ironman competitions per country
pie(top10_agegroup, main = "Number of People per Age Group", labels = names(top10_agegroup))

#------------------------------------------------
#Number of participants each year

# Calculate the number of participants each year
participants_per_year <- table(data$EventYear)

# Create a bar plot of the number of participants per year
barplot(participants_per_year, main = "Number of Participants per Year", xlab = "Year", ylab = "Number of Participants",col = "lightgreen")

#-----------------------------------------------
#Histograms of partial and full race times

#FINISH TIME
male_finish_times<-(data$FinishTime[data$Gender == "M"])/60
female_finish_times<-data$FinishTime[data$Gender=="F"])/60

summary(male_finish_times)
summary(female_finish_times)

hm<-hist(male_finish_times)
hf<-hist(female_finish_times)

# Plot the male histogram
plot(hm, col = 'lightblue',xlab = "Time (minutes)", main = "Finish Time")

# Add the female histogram to the plot
plot(hf, col = 'pink', add = TRUE)
legend("topright", c("Male","Female"),fill = c("lightblue","pink"))

#SWIM TIME
male_swim_times <- (data$SwimTime[data$Gender == "M"])/60
female_swim_times <- (data$SwimTime[data$Gender == "F"])/60

summary(male_swim_times)
summary(female_swim_times)

hm<-hist(male_swim_times)
hf<-hist(female_swim_times)

# Plot the male histogram
plot(hm, col = 'lightblue',xlab = "Time (minutes)", main = "Swim time")

# Add the female histogram to the plot
plot(hf, col = 'pink', add = TRUE)
legend("topright",c("Male","Female"),fill=c("lightblue", "pink"))


#BIKE TIME
male_bike_times <- (data$BikeTime[data$Gender == "M"])/60
female_bike_times <- (data$BikeTime[data$Gender == "F"])/60

summary(male_bike_times)
summary(female_bike_times)

hm<-hist(male_bike_times)
hf<-hist(female_bike_times)

# Plot the male histogram
plot(hm, col = 'lightblue',xlab = "Time (minutes)", main = "Bike time")

# Add the female histogram to the plot
plot(hf, col = 'pink', add = TRUE)
legend("topright",c("Male","Female"),fill=c("lightblue", "pink"))


#RUN TIME
male_run_times <- (data$RunTime[data$Gender == "M"])/60
female_run_times <- (data$RunTime[data$Gender == "F"])/60

summary(male_run_times)
summary(female_run_times)

hm<-hist(male_run_times)
hf<-hist(female_run_times)

# Plot the male histogram
plot(hm, col = 'lightblue',xlab = "Time (minutes)", main = "Run time")

# Add the female histogram to the plot
plot(hf, col = 'pink', add = TRUE)
legend("topright",c("Male","Female"),fill=("lightblue", "pink"))

#-----------------------------------------------------
#Race performance by age group and gender

# Create a subset of data for males and females
male_data <- data[data$Gender == "M", ]
female_data <- data[data$Gender == "F", ]

# Create separate box plots for male and female finish times
par(mfrow = c(1, 2))  # Set the plotting layout to have two plots side by side

# Plot for males
boxplot(FinishTime/60 ~ AgeGroup, data = male_data,
        main = "Finish Time by Age Group - Male",
        xlab = "Age Group", ylab = "Finish Time(minutes)",
        col = "lightblue")

# Plot for females
boxplot(FinishTime/60 ~ AgeGroup, data = female_data,
        main = "Finish Time by Age Group - Female",
        xlab = "Age Group", ylab = "Finish Time (minutes)",
        col = "pink")

#------------------------------------------------------
#Race performance EXCEL DATA

# Subset the data for males and females
male_data <- data[data$Gender == "M", ]
female_data <- data[data$Gender == "F", ]

# Aggregate the minimum and mean time by year for males
male_min_time <- aggregate(FinishTime ~ EventYear, data = male_data, FUN = min)
male_mean_time <- aggregate(FinishTime ~ EventYear, data = male_data, FUN = mean)

# Aggregate the minimum and mean time by year for females
female_min_time <- aggregate(FinishTime ~ EventYear, data = female_data, FUN = min)
female_mean_time <- aggregate(FinishTime ~ EventYear, data = female_data, FUN = mean)

# Convert time values from seconds to minutes
male_min_time$FinishTime <- male_min_time$FinishTime / 60
male_mean_time$FinishTime <- male_mean_time$FinishTime / 60
female_min_time$FinishTime <- female_min_time$FinishTime / 60
female_mean_time$FinishTime <- female_mean_time$FinishTime / 60

# Combine the results into a single data frame
results <- data.frame(EventYear = male_min_time$EventYear,
                      MinTime_Male = male_min_time$FinishTime,
                      MeanTime_Male = male_mean_time$FinishTime,
                      MinTime_Female = female_min_time$FinishTime,
                      MeanTime_Female = female_mean_time$FinishTime)

# View the results
results




#---------------------------------------------------------
#TWO-WAY ANOVA TEST
#---------------------------------------------------------

# Fit the two-way ANOVA model
model <- aov(FinishTime ~ AgeGroup * Gender, data = data)

# Conduct the ANOVA test
anova_result <- anova(model)

# Print the ANOVA table
print(anova_result)

#-----------------------------------------------------------------------------------------------------
#BACKWARD AND FORWARD VARIABLE SELECTION
#-----------------------------------------------------------------------------------------------------

library(MASS)

model_backward<-stepAIC(model_full,trace=TRUE,direction="backward")


model_null <- lm(FinishTime ~ 1, data = data)

# Perform forward variable selection using stepAIC
model_forward <- stepAIC(model_null, scope = list(lower = ~1, upper = ~ Gender + AgeGroup + AgeBand + Country + EventYear + EventLocation + SwimTime + BikeTime + RunTime), direction = "forward")

#-------------------------------------------------------------------------------------------
#PARAMETRIC BOOTSTRAP
#-------------------------------------------------------------------------------------------
# Load required library
library(ggplot2)

#First we need to identify which distribution the variable FinishTime follows.

# Plot a histogram of FinishTime
ggplot(sampled_data, aes(x = FinishTime)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(x = "Finish Time", y = "Count", title = "Histogram of Finish Time")

# Plot a density plot of FinishTime
ggplot(sampled_data, aes(x = FinishTime)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(x = "Finish Time", y = "Density", title = "Density Plot of Finish Time")

#To perform the parametric bootstrap assuming that the "FinishTime" variable follows a normal distribution, we can follow these steps:

#Estimate the parameters of the normal distribution
mu <- mean(sampled_data$FinishTime)/60
mu
sigma<-sd(sampled_data$FinishTime)/60
sigma

#Generate a random sample of the same size as the original dataset from the assumed normal distribution.
boot_sample <- rnorm(n = nrow(sampled_data), mean=mu, sd=sigma)

#Perform the desired analysis or calculation using the bootstrap sample.
boot_mean <- mean(boot_sample)
boot_median <- median(boot_sample)

n_boot <- 1000  # Number of bootstrap iterations
boot_means <- numeric(n_boot)
boot_medians <- numeric(n_boot)
boot_sd <- numeric(n_boot)

for (i in 1:n_boot) {
  
  #Generate a random sample of the same size as the original dataset from the assumed normal distribution
  boot_sample = rnorm(1000, mu, sigma)
  
  boot_means[i] <- mean(boot_sample)
  boot_medians[i] <- median(boot_sample)
  boot_sd[i] <- sd(boot_sample)

}

#Analyze the distribution of the bootstrap statistics.
hist(boot_means, main = "Bootstrap Distribution of Mean", xlab = "Mean", col="yellow")
hist(boot_medians, main = "Bootstrap Distribution of Median", xlab = "Median",col="lightgreen")
hist(boot_sd, main = "Bootstrap Distribution of Standard Deviation", xlab = "Standard Deviation",col="orange")

boot_mean_ci <- quantile(boot_means, c(0.025, 0.975))
boot_mean_ci
boot_median_ci <- quantile(boot_medians, c(0.025, 0.975))
boot_median_ci
boot_sd_ci <- quantile(boot_sd, c(0.025, 0.975))
boot_sd_ci


#--------------------------------------------------------------------------------------------
#NON-PARAMETRIC BOOTSTRAP
#--------------------------------------------------------------------------------------------

num_boot <- 1000  # Number of bootstrap iterations
n <- nrow(sampled_data)  # Number of observations in the dataset

# Create empty vectors to store bootstrap statistics
bootstrap_mean <- numeric(num_boot)
bootstrap_median <- numeric(num_boot)
bootstrap_sd <- numeric(num_boot)

# Perform bootstrap iterations
for (i in 1:num_boot) {
  # Generate bootstrap sample by resampling with replacement
  bootstrap_sample <- sampled_data[sample(n, replace = TRUE), ]
  
  # Calculate statistics from the bootstrap sample
  bootstrap_mean[i] <- mean(bootstrap_sample$FinishTime)
  bootstrap_median[i] <- median(bootstrap_sample$FinishTime)
  bootstrap_sd[i] <- sd(bootstrap_sample$FinishTime)
}

#Analyze the distribution of the bootstrap statistics.
hist(bootstrap_mean, main = "Bootstrap Distribution of Mean", xlab = "Mean", col="yellow")
hist(bootstrap_median, main = "Bootstrap Distribution of Median", xlab = "Median",col="lightgreen")
hist(boot_sd, main = "Bootstrap Distribution of Standard Deviation", xlab = "Standard Deviation",col="orange")

# Calculate confidence intervals
confidence_interval_mean <- quantile(bootstrap_mean, c(0.025, 0.975))
confidence_interval_mean
confidence_interval_median <- quantile(bootstrap_median, c(0.025, 0.975))
confidence_interval_median
confidence_interval_sd <- quantile(bootstrap_sd, c(0.025, 0.975))
confidence_interval_sd

#--------------------------------------
# Obtain the original dataset and extract the "Gender" variable
gender_data <- sampled_data$Gender

# Determine the sample size of the dataset
n <- length(gender_data)

# Set the number of bootstrap iterations
n_boot <- 1000

# Create an empty matrix to store the bootstrap sample statistics
bootstrap_stats <- matrix(NA, n_boot, 2)  # 2 columns for Male and Female proportions

# Perform the bootstrap procedure
for (i in 1:n_boot) {
  bootstrap_sample <- sample(gender_data, replace = TRUE)
  bootstrap_prop <- prop.table(table(bootstrap_sample))  # Calculate the proportion of each gender category
  bootstrap_stats[i, ] <- bootstrap_prop
}

# Analyze the distribution of the bootstrap statistics
hist(bootstrap_stats[, 1], main = "Bootstrap Distribution of Male Proportion", xlab = "Proportion")
hist(bootstrap_stats[,2], main = "Bootstrap Distribution of Female Proportion", xlab = "Proportion")


#-------------------------------------------------------------------------------------------
#CORRELATION MATRIX
#-------------------------------------------------------------------------------------------
library(ggplot2)
library(ggcorrplot)
library(factoextra)

# Select the numeric variables of interest from the sampled_data
numeric_data <- sampled_data[, c("SwimTime", "BikeTime", "RunTime", "FinishTime")]

# Normalize the numeric data by scaling it
data_normalized <- scale(numeric_data)

# Calculate the correlation matrix
corr_matrix <- cor(data_normalized)
corr_matrix

# Visualize the correlation matrix using ggcorrplot
ggcorrplot(corr_matrix)

#The result of the correlation matrix can be interpreted as follow: 
#The higher the value, the most positively correlated the two variables are.
#The closer the value to -1, the most negatively correlated they are.

#-------------------------------------------------------------------------------------------------
#PREDICT FUNCTION
#-------------------------------------------------------------------------------------------------

# Fit linear regression models for SwimTime, BikeTime, and RunTime
model_1<-lm(SwimTime/60 ~ Gender+AgeBand,data=sampled_data)

model_2<-lm(BikeTime/60 ~ Gender+AgeBand,data=sampled_data)

model_3<-lm(RunTime/60 ~ Gender++AgeBand,data=sampled_data)

# Create a grid of values for Gender and AgeBand
gender <- c("M", "F")  
age_band <- c(20, 30, 40, 50, 60)  

# Create an empty data frame to store the predicted values
predictions <- data.frame(Gender = character(),
                          AgeBand = numeric(),
                          SwimTime = numeric(),
                          BikeTime = numeric(),
                          RunTime = numeric())

# Generate predictions for each combination of Gender and AgeBand
for (g in gender) {
  for (a in age_band) {
    new_data <- data.frame(Gender = g, AgeBand = a)
    
    # Predict SwimTime, BikeTime, and RunTime using the respective models
    swim_pred <- predict(model_1, newdata = new_data, type = "response")
    bike_pred <- predict(model_2, newdata = new_data, type = "response")
    run_pred <- predict(model_3, newdata = new_data, type = "response")
    
    # Append the predictions to the data frame
    predictions <- rbind(predictions, data.frame(Gender = g, AgeBand = a,
                                                 SwimTime = swim_pred,
                                                 BikeTime = bike_pred,
                                                 RunTime = run_pred))
  }
}

# Plot the predictions
library(ggplot2)

# SwimTime
swim_plot <- ggplot(predictions, aes(x = AgeBand, y = SwimTime, color = Gender)) +
  geom_line() +
  labs(title = "Predicted SwimTime by Gender and AgeBand",
       x = "AgeBand", y = "SwimTime (minutes)")


# BikeTime
bike_plot <- ggplot(predictions, aes(x = AgeBand, y = BikeTime, color = Gender)) +
  geom_line() +
  labs(title = "Predicted BikeTime by Gender and AgeBand",
       x = "AgeBand", y = "BikeTime (minutes)")

# RunTime
run_plot <- ggplot(predictions, aes(x = AgeBand, y = RunTime, color = Gender))+  geom_line() +
  labs(title = "Predicted RunTime by Gender and AgeBand",
       x = "AgeBand", y = "RunTime (minutes)")

# Display the plots side by side using grid.arrange
library(gridExtra)
grid.arrange(swim_plot, bike_plot,run_plot, nrow = 1)

#----------------------------------------------------
#Example prediction

newdata = data.frame(Gender="F", AgeBand = 20)

model1<-lm(SwimTime/60 ~ Gender+AgeBand,data=sampled_data)
predict(model1, newdata = newdata, type = "response")

model2<-lm(BikeTime/60 ~ Gender+AgeBand,data=sampled_data)
predict(model2, newdata = newdata, type = "response")

model3<-lm(RunTime/60 ~ Gender+AgeBand,data=sampled_data)
predict(model3, newdata = newdata, type = "response")

model4<-lm(FinishTime/60 ~ Gender+AgeBand,data=sampled_data)
predict(model4, newdata = newdata, type = "response")


#-----------------------------------------------------------------------------------------------
#PEARSON RESIDUALS
#----------------------------------------------------------------------------------------------

# Calculate Pearson residuals using the full model
res=residuals(model_full,"pearson")

plot(res, pch = 20, xlab = "Observation", ylab = "Pearson Residuals", 
     main = "Pearson Residuals of Ironman Dataset")

# Find the maximum Pearson residual
max(res)

# Find the index of the outlier with the highest Pearson residual
outlier<-which.max(res)
outlier

# Print the details of the outlier
sampled_data$FinishTime[outlier]/60 #lowest time
sampled_data$Gender[outlier]
sampled_data$AgeBand[outlier]
sampled_data$Country[outlier]
sampled_data$EventYear[outlier]
sampled_data$EventLocation[outlier]

newdata = data.frame(Gender="M", AgeBand = 30)

model1<-lm(FinishTime/60 ~ Gender+AgeBand,data=sampled_data)
predict(model1, newdata = newdata, type = "response")
