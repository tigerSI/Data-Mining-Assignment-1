library(dplyr)
library(mlbench)
library(Matrix)
library(arules)
library(arulesViz)

#--------------------------------------------------------------------------------------------------
# Data cleaning
#--------------------------------------------------------------------------------------------------

# We use the cleaning data approach which we implemented in our assignment 2 to do in this assignment

# Load the Census data
MyData <- read.csv(file = "Resource/Census income data.csv", header = TRUE, sep = ",")
Cleaned_data  <- MyData

# Change ? in country_father to other
Cleaned_data$country_father = as.character(Cleaned_data$country_father)
for(i in 1:nrow(Cleaned_data)){
  if(Cleaned_data$country_father[i] == " ?"){
    Cleaned_data$country_father[i] <- " Other"
  }
}
Cleaned_data$country_father = as.factor(Cleaned_data$country_father)

# Change ? in country_mother to other
Cleaned_data$country_mother = as.character(Cleaned_data$country_mother)
for(i in 1:nrow(Cleaned_data)){
  if(Cleaned_data$country_mother[i] == " ?"){
    Cleaned_data$country_mother[i] <- " Other"
  }
}
Cleaned_data$country_mother = as.factor(Cleaned_data$country_mother)

# Change ? in country_self to other
Cleaned_data$country_self = as.character(Cleaned_data$country_self)
for(i in 1:nrow(Cleaned_data)){
  if(Cleaned_data$country_self[i] == " ?"){
    Cleaned_data$country_self[i] <- " Other"
  }
}
Cleaned_data$country_self = as.factor(Cleaned_data$country_self)

# Change ? in mig_prev_sunbelt to Not in universe
Cleaned_data$mig_prev_sunbelt = as.character(Cleaned_data$mig_prev_sunbelt)
for(i in 1:nrow(Cleaned_data)){
  if(Cleaned_data$mig_prev_sunbelt[i] == " ?"){
    Cleaned_data$mig_prev_sunbelt[i] <- " Not in universe"
  }
}
Cleaned_data$mig_prev_sunbelt = as.factor(Cleaned_data$mig_prev_sunbelt)

# Change ? in mig_move_reg to Not in universe
Cleaned_data$mig_move_reg = as.character(Cleaned_data$mig_move_reg)
for(i in 1:nrow(Cleaned_data)){
  if(Cleaned_data$mig_move_reg[i] == " ?"){
    Cleaned_data$mig_move_reg[i] <- " Not in universe"
  }
}
Cleaned_data$mig_move_reg = as.factor(Cleaned_data$mig_move_reg)

# Change ? in mig_chg_msa to Not in universe
Cleaned_data$mig_chg_msa = as.character(Cleaned_data$mig_chg_msa)
for(i in 1:nrow(Cleaned_data)){
  if(Cleaned_data$mig_chg_msa[i] == " ?"){
    Cleaned_data$mig_chg_msa[i] <- " Not in universe"
  }
}
Cleaned_data$mig_chg_msa = as.factor(Cleaned_data$mig_chg_msa)

# Change ? in mig_chg_reg to Not in universe
Cleaned_data$mig_chg_reg = as.character(Cleaned_data$mig_chg_reg)
for(i in 1:nrow(Cleaned_data)){
  if(Cleaned_data$mig_chg_reg[i] == " ?"){
    Cleaned_data$mig_chg_reg[i] <- " Not in universe"
  }
}
Cleaned_data$mig_chg_reg = as.factor(Cleaned_data$mig_chg_reg)

# change NA in hisp_origin to Do not know
Cleaned_data$hisp_origin = as.character(Cleaned_data$hisp_origin)
for(i in 1:nrow(Cleaned_data)){
  if(Cleaned_data$hisp_origin[i] == " NA"){
    Cleaned_data$hisp_origin[i] <- " Do not know"
  }
}
Cleaned_data$hisp_origin = as.factor(Cleaned_data$hisp_origin)

# change ? in state_prev_res to Not in universe
Cleaned_data$state_prev_res = as.character(Cleaned_data$state_prev_res)
for(i in 1:nrow(Cleaned_data)){
  if(Cleaned_data$state_prev_res[i] == " ?"){
    Cleaned_data$state_prev_res[i] <- " Not in universe"
  }
}
Cleaned_data$state_prev_res = as.factor(Cleaned_data$state_prev_res)

# Eliminate same data objects
Cleaned_data <- distinct(Cleaned_data)
summary(Cleaned_data)


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
# Association rules 
# ---------------------------------------------------------------------------------------------------

# create concensus_rule by using Apriori method
concensus_rule = apriori(preprocess_data, parameter = list(support = 0.3, confidence = 0.9, minlen = 2)) 
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

#---------------------------------------------------------------------------------------------------
# Association rule visualization
#---------------------------------------------------------------------------------------------------
# plotting
# Scatter Plot
#"order" means number of items
plot(concensus_rule, jitter = 0, shading  = "order") 

# Interactive Plot
# # Two-key plot: It is a scatterplot with shading = "order"
plot(concensus_rule, jitter = 0, method = "two-key plot")

# Grouped matrix plot:
# Antecedents (columns) : in the matrix are grouped using clustering technique.
# Groups are represented by the most interesting item 
# (highest ratio of support in the group to support in all rules) in the group.
# Balloons location : in the matrix indicates what consequent the antecedent are connected.
plot(concensus_rule, method="grouped matrix")

#Graph
plot(concensus_rule, method="graph")



