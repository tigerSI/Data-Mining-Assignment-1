library(dplyr)
library(mlbench)
library(Matrix)
library(arules)
library(arulesViz)

# ---------------------------------------------------------------------------------------------------
# Data pre-processing
# ---------------------------------------------------------------------------------------------------

# function which convert all attributes of the input data frame to factor.
convert_to_factor <- function(data_frame){
  for(col_name in names(data_frame)){
    data_frame[[col_name]] <- as.factor(data_frame[[col_name]])
  }
  data_frame
}

# preprocessing data from assignment 2 which is a classifier for income_50k.
preprocess_data <- select(Cleaned_data, age, class_worker, education, major_ind_code, major_occ_code, 
                          wage_per_hour, num_emp, own_or_self, weeks_worked, income_50k)

# Balance data objects by income_50k by sampling
# because the number of 50k- is greater than 50k+
a <- filter(preprocess_data, income_50k == "-50000")    # less than 50k
b <- filter(preprocess_data, income_50k == " 50000+.")  # more than 50k
sampling_a <- sample_n(a, nrow(b))
preprocess_data <- bind_rows(b, sampling_a)

preprocess_data <- convert_to_factor(preprocess_data)
summary(preprocess_data)

# ---------------------------------------------------------------------------------------------------
# Create association rules 
# ---------------------------------------------------------------------------------------------------

# create concensus_rule by using Apriori method
concensus_rule = apriori(preprocess_data, parameter = list(support = 0.35, confidence = 0.9)) 
# ordering concensus_rule by confidence value
concensus_rule <- sort(concensus_rule, by="lift", decreasing=TRUE) # 'high-confidence' rules.

# use inspect for look into all rules in concensus_rule
#inspect(concensus_rule)

concensus_rule

# write concensus_rule to csv file for easy reading
write(concensus_rule,
      file = "Resource/association_rules.csv",
      sep = ",",
      quote = TRUE,
      row.names = FALSE)

# get concensus_rule as data frame
concensus_rule_table = read.csv("Resource/association_rules.csv")

#plotting
plot(concensus_rule, jitter = 0)
#plot(concensus_rule, jitter = 0, shading="order")
#plot(concensus_rule, jitter = 0, method = "two-key plot")