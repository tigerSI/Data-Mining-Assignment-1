library(mlbench)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)
library(caret)
library(e1071)

# Clean data
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

# Read the test set
Testset <- read.csv(file = "Resource/Census income test.csv", header = TRUE, sep = ",")

# 1. Predict if a person’s income is greater than 50,000.
# Data preprocessing for classification task 1
# Attributes: age, class_worker, education, major_ind_code, major_occ_code, 
#             wage_per_hour, num_emp, own_or_self, weeks_worked, income_50k
p_data_1 <- select(Cleaned_data, age, class_worker, education, major_ind_code, major_occ_code, 
                   wage_per_hour, num_emp, own_or_self, weeks_worked, income_50k)

# Balance data objects by income_50k by sampling
# because the number of 50k- is greater than 50k+
a <- filter(p_data_1, income_50k == "-50000")    # less than 50k
b <- filter(p_data_1, income_50k == " 50000+.")  # more than 50k
sampling_a <- sample_n(a, nrow(b))
preprocessed_data_for_task_1 <- bind_rows(b, sampling_a)

# convert categorical data to numeric data
preprocessed_data_for_task_1$education = 
  factor(preprocessed_data_for_task_1$education,
         levels = c(" Children"," Less than 1st grade", " 1st 2nd 3rd or 4th grade", " 5th or 6th grade",
                    " 7th and 8th grade", " 9th grade", " 10th grade", " 11th grade", " 12th grade no diploma",
                    " High school graduate", " Some college but no degree", " Associates degree-academic program",
                    " Associates degree-occup /vocational", " Bachelors degree(BA AB BS)", " Masters degree(MA MS MEng MEd MSW MBA)",
                    " Prof school degree (MD DDS DVM LLB JD)", " Doctorate degree(PhD EdD)"),
         labels = c(1:17))

preprocessed_data_for_task_1$class_worker = 
  factor(preprocessed_data_for_task_1$class_worker,
         levels = c(" Not in universe", " Private", " Self-employed-not incorporated", " Local government"," State government",
                    " Self-employed-incorporated", " Federal government", " Never worked", " Without pay"),
         labels = c(1:9))

preprocessed_data_for_task_1$major_ind_code = 
  factor(preprocessed_data_for_task_1$major_ind_code,
         levels = c(" Not in universe or children", " Retail trade", " Manufacturing-durable goods", " Education", " Manufacturing-nondurable goods", 
                    " Finance insurance and real estate", " Construction", " Business and repair services", " Medical except hospital", 
                    " Public administration", " Other professional services", " Transportation", " Hospital services", " Wholesale trade", 
                    " Agriculture", " Personal services except private HH", " Social services", " Entertainment", " Communications", 
                    " Utilities and sanitary services", " Private household services", " Mining", " Forestry and fisheries", " Armed Forces"),
         labels = c(1:24))

preprocessed_data_for_task_1$major_occ_code = 
  factor(preprocessed_data_for_task_1$major_occ_code,
         levels = c(" Not in universe", " Adm support including clerical", " Professional specialty", " Executive admin and managerial",
                    " Other service", " Sales", " Precision production craft & repair", " Machine operators assmblrs & inspctrs",
                    " Handlers equip cleaners etc ", " Transportation and material moving", " Farming forestry and fishing", " Technicians and related support",
                    " Protective services", " Private household services", " Armed Forces"), 
         labels = c(1:15))

preprocessed_data_for_task_1$income_50k = 
  factor(preprocessed_data_for_task_1$income_50k,
         levels = c("-50000", " 50000+."), 
         labels = c(1:2))

# Test set preprocessing for task 1
t_data_1 <- select(Testset, age, class_worker, education, major_ind_code, major_occ_code, 
                   wage_per_hour, num_emp, own_or_self, weeks_worked, income_50k)
t_data_1$education = 
  factor(t_data_1$education,
         levels = c(" Children"," Less than 1st grade", " 1st 2nd 3rd or 4th grade", " 5th or 6th grade",
                    " 7th and 8th grade", " 9th grade", " 10th grade", " 11th grade", " 12th grade no diploma",
                    " High school graduate", " Some college but no degree", " Associates degree-academic program",
                    " Associates degree-occup /vocational", " Bachelors degree(BA AB BS)", " Masters degree(MA MS MEng MEd MSW MBA)",
                    " Prof school degree (MD DDS DVM LLB JD)", " Doctorate degree(PhD EdD)"),
         labels = c(1:17))

t_data_1$class_worker = 
  factor(t_data_1$class_worker,
         levels = c(" Not in universe", " Private", " Self-employed-not incorporated", " Local government"," State government",
                    " Self-employed-incorporated", " Federal government", " Never worked", " Without pay"),
         labels = c(1:9))

t_data_1$major_ind_code = 
  factor(t_data_1$major_ind_code,
         levels = c(" Not in universe or children", " Retail trade", " Manufacturing-durable goods", " Education", " Manufacturing-nondurable goods", 
                    " Finance insurance and real estate", " Construction", " Business and repair services", " Medical except hospital", 
                    " Public administration", " Other professional services", " Transportation", " Hospital services", " Wholesale trade", 
                    " Agriculture", " Personal services except private HH", " Social services", " Entertainment", " Communications", 
                    " Utilities and sanitary services", " Private household services", " Mining", " Forestry and fisheries", " Armed Forces"),
         labels = c(1:24))

t_data_1$major_occ_code = 
  factor(t_data_1$major_occ_code,
         levels = c(" Not in universe", " Adm support including clerical", " Professional specialty", " Executive admin and managerial",
                    " Other service", " Sales", " Precision production craft & repair", " Machine operators assmblrs & inspctrs",
                    " Handlers equip cleaners etc ", " Transportation and material moving", " Farming forestry and fishing", " Technicians and related support",
                    " Protective services", " Private household services", " Armed Forces"), 
         labels = c(1:15))

t_data_1$income_50k = 
  factor(t_data_1$income_50k,
         levels = c("-50000", " 50000+."), 
         labels = c(1:2))

# 2. Predict if a person’s marital status is never married.
# Data preprocessing for classification task 2
# Attributes: age, education, marital_stat, sex, tax_filer_stat, det_hh_summ
p_data_2 <- select(Cleaned_data, age, education, marital_stat, sex, tax_filer_stat, det_hh_summ)
preprocessed_data_for_task_2 <- sample_n(p_data_2, 100000)  # Sampling 100k data

# convert categorical data to numeric data
preprocessed_data_for_task_2$education = 
  factor(preprocessed_data_for_task_2$education,
         levels = c(" Children"," Less than 1st grade", " 1st 2nd 3rd or 4th grade", " 5th or 6th grade",
                    " 7th and 8th grade", " 9th grade", " 10th grade", " 11th grade", " 12th grade no diploma",
                    " High school graduate", " Some college but no degree", " Associates degree-academic program",
                    " Associates degree-occup /vocational", " Bachelors degree(BA AB BS)", " Masters degree(MA MS MEng MEd MSW MBA)",
                    " Prof school degree (MD DDS DVM LLB JD)", " Doctorate degree(PhD EdD)"),
         labels = c(1:17))

preprocessed_data_for_task_2$marital_stat = 
  factor(preprocessed_data_for_task_2$marital_stat,
         levels = c(" Never married", " Married-civilian spouse present", " Divorced", " Widowed",
                    " Separated", " Married-spouse absent", " Married-A F spouse present"),
         labels = c(1, 2, 2, 2, 2 ,2, 2))

preprocessed_data_for_task_2$sex = 
  factor(preprocessed_data_for_task_2$sex,
         levels = c(" Female", " Male"),
         labels = c(1:2))

preprocessed_data_for_task_2$tax_filer_stat = 
  factor(preprocessed_data_for_task_2$tax_filer_stat,
         levels = c(" Nonfiler", " Joint both under 65", " Single", " Joint both 65+", " Head of household",
                    " Joint one under 65 & one 65+"),
         labels = c(1:6))

preprocessed_data_for_task_2$det_hh_summ = 
  factor(preprocessed_data_for_task_2$det_hh_summ,
         levels = c(" Householder", " Child under 18 never married", " Spouse of householder", " Child 18 or older",
                    " Other relative of householder", " Nonrelative of householder", " Group Quarters- Secondary individual",
                    " Child under 18 ever married"),
         labels = c(1:8))

# Test set preprocessing for task 2
t_data_2 <- select(Testset, age, education, marital_stat, sex, tax_filer_stat, det_hh_summ)

# convert categorical data to numeric data
t_data_2$education = 
  factor(t_data_2$education,
         levels = c(" Children"," Less than 1st grade", " 1st 2nd 3rd or 4th grade", " 5th or 6th grade",
                    " 7th and 8th grade", " 9th grade", " 10th grade", " 11th grade", " 12th grade no diploma",
                    " High school graduate", " Some college but no degree", " Associates degree-academic program",
                    " Associates degree-occup /vocational", " Bachelors degree(BA AB BS)", " Masters degree(MA MS MEng MEd MSW MBA)",
                    " Prof school degree (MD DDS DVM LLB JD)", " Doctorate degree(PhD EdD)"),
         labels = c(1:17))

t_data_2$marital_stat = 
  factor(t_data_2$marital_stat,
         levels = c(" Never married", " Married-civilian spouse present", " Divorced", " Widowed",
                    " Separated", " Married-spouse absent", " Married-A F spouse present"),
         labels = c(1, 2, 2, 2, 2 ,2, 2))

t_data_2$sex = 
  factor(t_data_2$sex,
         levels = c(" Female", " Male"),
         labels = c(1:2))

t_data_2$tax_filer_stat = 
  factor(t_data_2$tax_filer_stat,
         levels = c(" Nonfiler", " Joint both under 65", " Single", " Joint both 65+", " Head of household",
                    " Joint one under 65 & one 65+"),
         labels = c(1:6))

t_data_2$det_hh_summ = 
  factor(t_data_2$det_hh_summ,
         levels = c(" Householder", " Child under 18 never married", " Spouse of householder", " Child 18 or older",
                    " Other relative of householder", " Nonrelative of householder", " Group Quarters- Secondary individual",
                    " Child under 18 ever married"),
         labels = c(1:8))


# 3. Predict if a white person’s income is less than 50,000.
# Data preprocessing for classification task 3
# Attributes: age, class_worker, education, major_ind_code, major_occ_code, race, hisp_origin,
#             wage_per_hour, num_emp, own_or_self, weeks_worked, income_50k
p_data_3 <- select(Cleaned_data, age, class_worker, education, major_ind_code, major_occ_code, race, hisp_origin, 
                   wage_per_hour, num_emp, own_or_self, weeks_worked, income_50k)
preprocessed_data_for_task_3 <- sample_n(p_data_3, 100000)  # Sampling 100k data
# Function: combining white and income less than 50k
New_combined_attribute_race_income <- function(table){
  roww <- nrow(table)
  coll <- ncol(table)
  
  for( i in 1:roww){
    if(toString(table$race[i]) == " White" && table$income_50k[i] == "-50000" ){
      #race="White"&income_50k="-50000" = 1, != ->0
      table$WhiteIncomeLess50k[i] <- 1
    }else{
      table$WhiteIncomeLess50k[i] <- 0
    }
  }
  return(table)
}

preprocessed_data_for_task_3$education = 
  factor(preprocessed_data_for_task_3$education,
         levels = c(" Children"," Less than 1st grade", " 1st 2nd 3rd or 4th grade", " 5th or 6th grade",
                    " 7th and 8th grade", " 9th grade", " 10th grade", " 11th grade", " 12th grade no diploma",
                    " High school graduate", " Some college but no degree", " Associates degree-academic program",
                    " Associates degree-occup /vocational", " Bachelors degree(BA AB BS)", " Masters degree(MA MS MEng MEd MSW MBA)",
                    " Prof school degree (MD DDS DVM LLB JD)", " Doctorate degree(PhD EdD)"),
         labels = c(1:17))

preprocessed_data_for_task_3$class_worker = 
  factor(preprocessed_data_for_task_3$class_worker,
         levels = c(" Not in universe", " Private", " Self-employed-not incorporated", " Local government"," State government",
                    " Self-employed-incorporated", " Federal government", " Never worked", " Without pay"),
         labels = c(1:9))

preprocessed_data_for_task_3$major_ind_code = 
  factor(preprocessed_data_for_task_3$major_ind_code,
         levels = c(" Not in universe or children", " Retail trade", " Manufacturing-durable goods", " Education", " Manufacturing-nondurable goods", 
                    " Finance insurance and real estate", " Construction", " Business and repair services", " Medical except hospital", 
                    " Public administration", " Other professional services", " Transportation", " Hospital services", " Wholesale trade", 
                    " Agriculture", " Personal services except private HH", " Social services", " Entertainment", " Communications", 
                    " Utilities and sanitary services", " Private household services", " Mining", " Forestry and fisheries", " Armed Forces"),
         labels = c(1:24))

preprocessed_data_for_task_3$major_occ_code = 
  factor(preprocessed_data_for_task_3$major_occ_code,
         levels = c(" Not in universe", " Adm support including clerical", " Professional specialty", " Executive admin and managerial",
                    " Other service", " Sales", " Precision production craft & repair", " Machine operators assmblrs & inspctrs",
                    " Handlers equip cleaners etc ", " Transportation and material moving", " Farming forestry and fishing", " Technicians and related support",
                    " Protective services", " Private household services", " Armed Forces"), 
         labels = c(1:15))

preprocessed_data_for_task_3$hisp_origin = 
  factor(preprocessed_data_for_task_3$hisp_origin,
         levels = c(" All other", " Mexican-American", " Mexican (Mexicano)", " Central or South American", " Puerto Rican",
                    " Other Spanish", " Cuban", " Do not know", " Chicano"),
         labels = c(1:9))

preprocessed_data_for_task_3 <- New_combined_attribute_race_income(preprocessed_data_for_task_3)
preprocessed_data_for_task_3 <- select(preprocessed_data_for_task_3, age, class_worker, education, major_ind_code, major_occ_code, 
                                       hisp_origin, wage_per_hour, num_emp, own_or_self, weeks_worked, WhiteIncomeLess50k)

preprocessed_data_for_task_3$WhiteIncomeLess50k = factor(preprocessed_data_for_task_3$WhiteIncomeLess50k, levels = c(0, 1), labels = c(0, 1))

# Test set preprocessing for task 3
t_data_3 <- select(Testset, age, class_worker, education, major_ind_code, major_occ_code, race, hisp_origin, 
                   wage_per_hour, num_emp, own_or_self, weeks_worked, income_50k)

t_data_3$education = 
  factor(t_data_3$education,
         levels = c(" Children"," Less than 1st grade", " 1st 2nd 3rd or 4th grade", " 5th or 6th grade",
                    " 7th and 8th grade", " 9th grade", " 10th grade", " 11th grade", " 12th grade no diploma",
                    " High school graduate", " Some college but no degree", " Associates degree-academic program",
                    " Associates degree-occup /vocational", " Bachelors degree(BA AB BS)", " Masters degree(MA MS MEng MEd MSW MBA)",
                    " Prof school degree (MD DDS DVM LLB JD)", " Doctorate degree(PhD EdD)"),
         labels = c(1:17))

t_data_3$class_worker = 
  factor(t_data_3$class_worker,
         levels = c(" Not in universe", " Private", " Self-employed-not incorporated", " Local government"," State government",
                    " Self-employed-incorporated", " Federal government", " Never worked", " Without pay"),
         labels = c(1:9))

t_data_3$major_ind_code = 
  factor(t_data_3$major_ind_code,
         levels = c(" Not in universe or children", " Retail trade", " Manufacturing-durable goods", " Education", " Manufacturing-nondurable goods", 
                    " Finance insurance and real estate", " Construction", " Business and repair services", " Medical except hospital", 
                    " Public administration", " Other professional services", " Transportation", " Hospital services", " Wholesale trade", 
                    " Agriculture", " Personal services except private HH", " Social services", " Entertainment", " Communications", 
                    " Utilities and sanitary services", " Private household services", " Mining", " Forestry and fisheries", " Armed Forces"),
         labels = c(1:24))

t_data_3$major_occ_code = 
  factor(t_data_3$major_occ_code,
         levels = c(" Not in universe", " Adm support including clerical", " Professional specialty", " Executive admin and managerial",
                    " Other service", " Sales", " Precision production craft & repair", " Machine operators assmblrs & inspctrs",
                    " Handlers equip cleaners etc ", " Transportation and material moving", " Farming forestry and fishing", " Technicians and related support",
                    " Protective services", " Private household services", " Armed Forces"), 
         labels = c(1:15))

t_data_3$hisp_origin = 
  factor(t_data_3$hisp_origin,
         levels = c(" All other", " Mexican-American", " Mexican (Mexicano)", " Central or South American", " Puerto Rican",
                    " Other Spanish", " Cuban", " Do not know", " Chicano"),
         labels = c(1:9))

t_data_3 <- New_combined_attribute_race_income(t_data_3)
t_data_3 <- select(t_data_3, age, class_worker, education, major_ind_code, major_occ_code, 
                   hisp_origin, wage_per_hour, num_emp, own_or_self, weeks_worked, WhiteIncomeLess50k)

t_data_3$WhiteIncomeLess50k = factor(t_data_3$WhiteIncomeLess50k, levels = c(0, 1), labels = c(0, 1))


# Function: finding accuracy
accuracy <- function(truth, prediction){
  tbl <- table(truth, prediction)   # Create confusion matrix
  sum(diag(tbl))/sum(tbl)           # Calculate accuracy
}
# Decision tree
tree_1 <- rpart(income_50k ~ ., data = preprocessed_data_for_task_1)
tree_2 <- rpart(marital_stat ~ ., data = preprocessed_data_for_task_2)
tree_3 <- rpart(WhiteIncomeLess50k ~ ., data = preprocessed_data_for_task_3)

# CART
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
set.seed(7)
fit.cart_1 <- train(income_50k~., data=preprocessed_data_for_task_1, method="rpart", metric=metric, trControl=control)
fit.cart_2 <- train(marital_stat~., data=preprocessed_data_for_task_2, method="rpart", metric=metric, trControl=control)
fit.cart_3 <- train(WhiteIncomeLess50k~., data=preprocessed_data_for_task_3, method="rpart", metric=metric, trControl=control)

# Rule-base
fit.rule_1 <- C5.0(income_50k ~ ., data = preprocessed_data_for_task_1, rules = TRUE)
fit.rule_2 <- C5.0(marital_stat ~ ., data = preprocessed_data_for_task_2, rules = TRUE)
fit.rule_3 <- C5.0(WhiteIncomeLess50k ~ ., data = preprocessed_data_for_task_3, rules = TRUE)

predicted_1 <- predict(tree_1, t_data_1, type = "class")
predicted_2 <- predict(tree_2, t_data_2, type = "class")
predicted_3 <- predict(tree_3, t_data_3, type = "class")

predicted_cart_1 <- predict(fit.cart_1, t_data_1, type = "class")
predicted_cart_2 <- predict(fit.cart_2, t_data_2, type = "class")
predicted_cart_3 <- predict(fit.cart_3, t_data_3, type = "class")

predicted_rule_1 <- predict(fit.rule_1, t_data_1, type = "class")
predicted_rule_2 <- predict(fit.rule_2, t_data_2, type = "class")
predicted_rule_3 <- predict(fit.rule_3, t_data_3, type = "class")

accuracy(t_data_1$income_50k, predicted_1)
accuracy(t_data_2$marital_stat, predicted_2)
accuracy(t_data_3$WhiteIncomeLess50k, predicted_3)

accuracy(t_data_1$income_50k, predicted_cart_1)
accuracy(t_data_2$marital_stat, predicted_cart_2)
accuracy(t_data_3$WhiteIncomeLess50k, predicted_cart_3)

accuracy(t_data_1$income_50k, predicted_rule_1)
accuracy(t_data_2$marital_stat, predicted_rule_2)
accuracy(t_data_3$WhiteIncomeLess50k, predicted_rule_3)


