library(dplyr)
# Load the Census data
MyData <- read.csv(file="Resource/Census income data.csv", header=TRUE, sep=",")
summary(MyData$mig_prev_sunbelt)
#View Structure of Data
#glimpse(MyData)
#summary(MyData)
#view histogram
#hist(MyData$age)
#view plot of two variables
#plot(MyData$age, MyData$class_worker)

cleaned_data  <- MyData
cleaned_data$mig_prev_sunbelt <- NULL
cleaned_data$mig_chg_msa <- NULL
cleaned_data$mig_chg_reg <- NULL
cleaned_data$mig_move_reg <- NULL
cleaned_data <- distinct(cleaned_data)

summary(cleaned_data)

cleaned_data$country_father = as.character(cleaned_data$country_father)
cleaned_data$country_mother = as.character(cleaned_data$country_mother)
cleaned_data$country_self = as.character(cleaned_data$country_self)

vector_of_missing_data <- c()
count = 1
for(i in 1:nrow(cleaned_data)){
  if(cleaned_data$country_father[i] == " ?" || 
     cleaned_data$country_mother[i] == " ?" ||
     cleaned_data$country_self[i] == " ?"){
    vector_of_missing_data[count] <- i
    count = count + 1
  }
}
cleaned_data <- cleaned_data[-vector_of_missing_data, ]

cleaned_data$country_father = as.factor(cleaned_data$country_father)
cleaned_data$country_mother = as.factor(cleaned_data$country_mother)
cleaned_data$country_self = as.factor(cleaned_data$country_self)

summary(cleaned_data)
