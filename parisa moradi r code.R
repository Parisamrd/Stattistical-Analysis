#read all the data from education dataset
education <- read.csv("education-data.csv", header= TRUE)

str(education)

#convert the character to numeric
columns_to_convert <- c("Fbachelor25", "Mbachelor25", "Fprimary25", "Mprimary25",
                        "Fmaster25", "Mmaster25", "Government_expenditure", "LaborForce",
                        "Unedu_work", "Funemployment_education", "Munemployment_education")
education[columns_to_convert] <- lapply(education[columns_to_convert], as.numeric)

#prepration data
#change .. to NA
any(sapply(education, function(x) any(x == "..")))
education[education == ".."] <- NA

# missing value
missing_values <- sapply(education, function(x) sum(is.na(x)))
print(missing_values)

#detect numeric columns
numeric_columns <- sapply(education, is.numeric)
numeric_col_names <- names(education)[numeric_columns]

#replace missing value with mean
for (col in numeric_col_names) {
  missing_indices <- which(is.na(education[[col]]))
  if (length(missing_indices) > 0) {
    education[missing_indices, col] <- mean(education[[col]], na.rm = TRUE)
  }
}

#double check for missing value
missing_values_after_fill <- sapply(education, function(x) sum(is.na(x)))
print(missing_values_after_fill)

#two category
American_data <- education[education$continent == "American", ]
european_data <- education[education$continent == "European", ]

#mean for American countries
numeric_data <- as.data.frame(lapply(American_data, as.numeric))

means <- colMeans(numeric_data, na.rm = TRUE)
print(means)

#mean for European countries
numeric_data <- as.data.frame(lapply(european_data, as.numeric))

means <- colMeans(numeric_data, na.rm = TRUE)
print(means)

#median for American
numeric_data <- as.data.frame(lapply(American_data, as.numeric))
medians <- sapply(numeric_data, median, na.rm = TRUE)
print(medians)

#median for European countries
numeric_data <- as.data.frame(lapply(european_data, as.numeric))
medians <- sapply(numeric_data, median, na.rm = TRUE)
print(medians)

#mode for American_data

calculate_mode <- function(American_data) {
  modes <- sapply(American_data, function(x) {
    freq_table <- table(x)
    mode_value <- as.numeric(names(freq_table)[freq_table == max(freq_table)])
    return(mode_value)
  })
  return(modes)
}
modes <- calculate_mode(American_data)
print(modes) 


#mode for Europian countries
calculate_mode <- function(european_data) {
  modes <- sapply(european_data, function(x) {
    freq_table <- table(x)
    mode_value <- as.numeric(names(freq_table)[freq_table == max(freq_table)])
    return(mode_value)
  })
  return(modes)
}
modes <- calculate_mode(european_data)
print(modes) 

#standard deviation American countries
std_devs <- sapply(American_data, sd, na.rm = TRUE)
print(std_devs)

#standard deviation for European countries
std_devs <- sapply(european_data, sd, na.rm = TRUE)
print(std_devs)

#skewness

install.packages("moments")
install.packages("e1071")
library(moments)
library(e1071)

#skewness American
numeric_columns <- sapply(American_data, is.numeric)
numeric_data <- American_data[, numeric_columns]  
skew <- sapply(numeric_data, skewness, na.rm = TRUE)  
result <- data.frame(Column = names(skew), Skewness = skew)
print(result)

#skewness European
numeric_columns <- sapply(european_data, is.numeric)
numeric_data <- european_data[, numeric_columns]  
skew <- sapply(numeric_data, skewness, na.rm = TRUE)  
result <- data.frame(Column = names(skew), Skewness = skew)
print(result)



install.packages("mosaic")
library(mosaic)
library(ggplot2)

#American bar plot
# Create a bar plot for government expenditure on education
ggplot(American_data, aes(x = Country_Name, y = Government_expenditure)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  labs(title = "Government Expenditure on Education by Country",
       x = "Country",
       y = "Government Expenditure") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#European barplot 
# Create a bar plot for government expenditure on education
ggplot(european_data, aes(x = Country_Name, y = Government_expenditure)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  labs(title = "Government Expenditure on Education by Country",
       x = "Country",
       y = "Government Expenditure") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


#pie chart American countries
American_data <- data.frame(
  country_column = c('United State', 'Cuba', 'Mexico', 'Bahamas', 'Panama'),
    Government_expenditure_on_education = c(6.129205704, 0, 5.257470131, 2.448431969, 2.816828012)
)

with(American_data, pie(Government_expenditure_on_education, labels = country_column, main = 'Government Expenditure on  in 2014'))


#pie chart european countries
european_data <- data.frame(
  country_column = c('France', 'Italy', 'Germany', 'Austria', 'Switzerland'),
    Government_expenditure_on_education = c(5.512063026, 4.061279774,4.921020031, 5.447619915, 4.930570126)
)

with(european_data, pie(Government_expenditure_on_education, labels = country_column, main = 'Government Expenditure on Education in 2014'))

#American box plot
American_data <- education[education$continent == "American", ]
American_data$LaborForce <- as.numeric(as.character(American_data$LaborForce))
laborforce_data <- American_data$LaborForce

boxplot(laborforce_data, 
        main = "Box Plot of American Labor Force", 
        xlab = "American Labor Force", 
        ylab = "Labor Force")


#European box plot
european_data <- education[education$continent == "European", ]
european_data$LaborForce <- as.numeric(as.character(european_data$LaborForce))
laborforce_data <- european_data$LaborForce

boxplot(european_data$LaborForce, 
        main = "Box Plot of European Labor Force", 
        xlab = "European Labor Force", 
        ylab = "Labor Force")


install.packages("datarium")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("rcompanion")

library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)

#correlation

American_data <- education[education$continent == "American", ]
european_data <- education[education$continent == "European", ]


American_data$Fmaster25 <- as.numeric(as.character(American_data$Fmaster25))
American_data$Mmaster25 <- as.numeric(as.character(American_data$Mmaster25))
American_data$Government_expenditure <- as.numeric(as.character(American_data$Government_expenditure))
American_data$LaborForce <- as.numeric(as.character(American_data$LaborForce))


correlation_matrix <- cor(American_data[c("Fmaster25", "Mmaster25", "Government_expenditure", "LaborForce")])
print(correlation_matrix)

library(corrplot)
corrplot(correlation_matrix, method = "color")

european_data$Fmaster25 <- as.numeric(as.character(european_data$Fmaster25))
european_data$Mmaster25 <- as.numeric(as.character(european_data$Mmaster25))
european_data$Government_expenditure <- as.numeric(as.character(european_data$Government_expenditure))
european_data$LaborForce <- as.numeric(as.character(european_data$LaborForce))


correlation_matrix <- cor(european_data[c("Fmaster25", "Mmaster25", "Government_expenditure", "LaborForce")])
print(correlation_matrix)

library(corrplot)
corrplot(correlation_matrix, method = "color")



install.packages("datarium")
install.packages("qqplotr")
install.packages("ggplot2")

library(ggplot2)
library(datarium)
library(qqplotr)


#T_Test  european countries says the mean 0f unemployed men with education degree is 4.4
european_data <- education[education$continent == "European", ]
head(european_data)
summary(european_data)

dim(european_data)


class(european_data$Munemployment_education)
european_data$Munemployment_education <- as.numeric(as.character(european_data$
                                                                   Munemployment_education))
class(european_data$Munemployment_education)

hist(european_data$Munemployment_education)

mean(european_data$Munemployment_education)

sd(european_data$Munemployment_education)

t.test(european_data$Munemployment_education, mean=4.4, alternative="greater")


#non parametric alternative test
#wilcoxon rank sum test


American_data <- education[education$continent == "American", ]
european_data <- education[education$continent == "European", ]


American_data$LaborForce <- as.numeric(as.character(American_data$LaborForce))
european_data$LaborForce <- as.numeric(as.character(european_data$LaborForce))



mwu_test_result <- wilcox.test(American_data$LaborForce, european_data$LaborForce)
print(mwu_test_result)



install.packages("car")
install.packages("corrplot")
install.packages("caret")

library(car)
library(corrplot)
library(caret)

columns_to_convert <- c("Fbachelor25", "Mbachelor25", "Fprimary25", "Mprimary25",
                        "Fmaster25", "Mmaster25", "Government_expenditure", "LaborForce",
                        "Unedu_work", "Funemployment_education", "Munemployment_education")
education[columns_to_convert] <- lapply(education[columns_to_convert], as.numeric)
American_data <- education[education$continent == "American", ]
european_data <- education[education$continent == "European", ]


#regression European
education_reduced <- european_data[ ,c("Fbachelor25", "Mbachelor25", "Fprimary25", "Mprimary25",
                        "Fmaster25", "Mmaster25", "Government_expenditure", "LaborForce",
                        "Unedu_work", "Funemployment_education", "Munemployment_education")]
corrplot(cor(education_reduced))

model_2 <-lm(Government_expenditure ~ Funemployment_education + Unedu_work, education_reduced)
summary.lm(model_2)

model_2 <-lm(Government_expenditure ~ Funemployment_education + Unedu_work + Munemployment_education , 
             education_reduced)
summary.lm(model_2)

data.frame(colnames(education_reduced))

pairs(education_reduced[,c(7,9,10,11)], lower.panel = NULL, pch = 19,cex = 0.2)

# regression American
education_reduced <- American_data[ ,c("Fbachelor25", "Mbachelor25", "Fprimary25", "Mprimary25",
                                       "Fmaster25", "Mmaster25", "Government_expenditure", "LaborForce",
                                       "Unedu_work", "Funemployment_education", "Munemployment_education")]
corrplot(cor(education_reduced))

model_2 <-lm(Government_expenditure ~ Funemployment_education + Unedu_work, education_reduced)
summary.lm(model_2)

model_2 <-lm(Government_expenditure ~ Funemployment_education + Unedu_work + Munemployment_education ,
             education_reduced)
summary.lm(model_2)

data.frame(colnames(education_reduced))


pairs(education_reduced[,c(7,9,10,11)], lower.panel = NULL, pch = 19,cex = 0.2)

#time series




# Define the file path to dataset
file_path <- "C:/Users/Applecenter/Desktop/data statistic/P_Data_Extract_From_World_Development_Indicators (1)/education-data.csv"

data <- read.csv(file_path)

any(sapply(data, function(x) any(x == "..")))
education[data == ".."] <- NA

missing_values <- sapply(data, function(x) sum(is.na(x)))
numeric_columns <- sapply(data, is.numeric)
numeric_col_names <- names(data)[numeric_columns]

#replace missing value with mean
for (col in numeric_col_names) {
  missing_indices <- which(is.na(data[[col]]))
  if (length(missing_indices) > 0) {
    data[missing_indices, col] <- mean(data[[col]], na.rm = TRUE)
  }
}

American_data <- data[education$continent == "American", ]
european_data <- data[education$continent == "European", ]

install.packages("forecast")  
library(forecast)
library(readr)
library(dplyr)

#American Time series
American_data$Government_expenditure <- as.numeric(American_data$Government_expenditure)
time_series_data <- ts(American_data$Government_expenditure, frequency = 1)
arima_model <- auto.arima(time_series_data)
summary(arima_model)
forecast_values <- forecast(arima_model, h = 10)  
print(forecast_values)
plot(forecast_values)

#European Time series
european_data$Government_expenditure <- as.numeric(european_data$Government_expenditure)
time_series_data <- ts(european_data$Government_expenditure, frequency = 1)
arima_model <- auto.arima(time_series_data)
summary(arima_model)
forecast_values <- forecast(arima_model, h = 10)  
print(forecast_values)
plot(forecast_values)

