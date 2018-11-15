library(dplyr)
# Load the Census data
MyData <- read.csv(file="Resource/Census income data.csv", header=TRUE, sep=",")

#Delete following columns because there are too much missing data
cleaned_data  <- MyData
cleaned_data$mig_prev_sunbelt <- NULL
cleaned_data$mig_chg_msa <- NULL
cleaned_data$mig_chg_reg <- NULL
cleaned_data$mig_move_reg <- NULL

#Eliminate duplicate data objects
cleaned_data <- distinct(cleaned_data)

#Eliminate the data objects which have the missing data in any three columns.
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

#Change "NA" value to "Do not know" value 
#Because there is a few number of missing data, we can assume it to unknown value
cleaned_data$hisp_origin = as.character(cleaned_data$hisp_origin)
for(i in 1:nrow(cleaned_data)){
  if(cleaned_data$hisp_origin[i] == " NA"){
    cleaned_data$hisp_origin[i] <- " Do not know"
  }
}
cleaned_data$hisp_origin = as.factor(cleaned_data$hisp_origin)

#Change "?" value to "not in universe" value
#Because there is a few number of missing data, we can assume it to unknown value
cleaned_data$state_prev_res = as.character(cleaned_data$state_prev_res)
for(i in 1:nrow(cleaned_data)){
  if(cleaned_data$state_prev_res[i] == " ?"){
    cleaned_data$state_prev_res[i] <- " Not in universe"
  }
}
cleaned_data$state_prev_res = as.factor(cleaned_data$state_prev_res)


#
#get_mode <- function(v) {
#  uniqv <- unique(v)
#  uniqv[which.max(tabulate(match(v, uniqv)))]
#}

#mean(cleaned_data$det_hh_summ)
#median(cleaned_data$capital_gains)
#get_mode(cleaned_data$capital_losses)
#sd(cleaned_data$capital_losses)
#range(cleaned_data$capital_losses)

#summary(cleaned_data$country_self)

#plot(cleaned_data$wage_per_hour,cleaned_data$full_or_part_emp, col = cleaned_data$major_occ_code,  main="Example 1", xlab = "wage", ylab = "occ")
#legend("topleft", legend=levels(cleaned_data$major_occ_code), pch=16, col=unique(cleaned_data$major_occ_code))

#pairs(~country_father+country_mother+country_self+citizenship,data=cleaned_data, main="Simple Scatterplot Matrix")
#pairs(~capital_gains+capital_losses+age,data=cleaned_data, main="Simple Scatterplot Matrix")

#plot(~country_father+country_mother+country_self,data=cleaned_data, main="Simple Scatterplot Matrix", col=citizenship)
#plot(~wage_per_hour+income_50k+weeks_worked,data=cleaned_data, main="Job with wage plot", col=major_occ_code)
#plot(~age+sex+education,data=cleaned_data, main="Relation with employment plot", col=full_or_part_emp)
#plot(~wage_per_hour, data=cleaned_data, main="plot", col=major_occ_code)

#plot(~det_hh_fam_stat,det_hh_summ,data=cleaned_data, main="Simple plot", col=marital_stat)

#plot(~income_50k+citizenship+own_or_self,data=cleaned_data, main="Relation with employment plot", col=education)

#plot(cleaned_data$ind_code, cleaned_data$occ_code)
#cor(cleaned_data$ind_code, cleaned_data$occ_code)

#plot(cleaned_data$weeks_worked, cleaned_data$stock_dividends, xlab = "Weeks worked", ylab = "Stock dividends")
#cor(cleaned_data$weeks_worked, cleaned_data$stock_dividends)

#ordering
#test <- cleaned_data
#test <- test[order(test$major_occ_code),]

