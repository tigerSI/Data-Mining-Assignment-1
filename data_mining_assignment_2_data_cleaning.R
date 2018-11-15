library(dplyr)
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
summary(Cleaned_data)