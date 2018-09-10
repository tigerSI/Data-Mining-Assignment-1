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

#delete column because there are too much missing data
cleaned_data  <- MyData
cleaned_data$mig_prev_sunbelt <- NULL
cleaned_data$mig_chg_msa <- NULL
cleaned_data$mig_chg_reg <- NULL
cleaned_data$mig_move_reg <- NULL
cleaned_data <- distinct(cleaned_data)

summary(cleaned_data)

#eleminate some missing data in those 3 column because there are aournd 8000 objects
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

# change NA to Do not know
cleaned_data$hisp_origin = as.character(cleaned_data$hisp_origin)
for(i in 1:nrow(cleaned_data)){
  if(cleaned_data$hisp_origin[i] == " NA"){
    cleaned_data$hisp_origin[i] <- " Do not know"
  }
}
cleaned_data$hisp_origin = as.factor(cleaned_data$hisp_origin)

# change ? to not in universe because not in universe has too much data and ? should be in this column without change the data pattern 
cleaned_data$state_prev_res = as.character(cleaned_data$state_prev_res)
for(i in 1:nrow(cleaned_data)){
  if(cleaned_data$state_prev_res[i] == " ?"){
    cleaned_data$state_prev_res[i] <- " Not in universe"
  }
}
cleaned_data$state_prev_res = as.factor(cleaned_data$state_prev_res)

summary(cleaned_data)

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mean(cleaned_data$det_hh_summ)
median(cleaned_data$capital_gains)
get_mode(cleaned_data$capital_losses)
sd(cleaned_data$capital_losses)
range(cleaned_data$capital_losses)

summary(cleaned_data$capital_losses)

cleaned_data$class_worker = as.character(cleaned_data$class_worker)
hist(cleaned_data$class_worker)
