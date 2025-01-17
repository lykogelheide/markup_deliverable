#------------------------------ Initial Settings ------------------------------# 
if(!require(tidyverse)){install.packages('tidyverse')}
library(tidyverse)


############################ Simulate Covariates ##############################
sim_covariates <- function(X, seed) {
  ###Set seed for reproducability
  set.seed(seed)
  ###Create IDs based on population size
  ID <- 1:nrow(X)
  ###dgp = data generating process
  dgp <- function(x1, x2, x3, x4) { 
    pi * x1 + sin(x2) + exp(x3) + 20 * x4^2 
  }
  #Check if dgp works (number of covs)
  #Check whether X has 4 numeric columns. Otherwise, warning
  ###Simulate covariates
  x1 <- X[, 1]
  x2 <- X[, 2]
  x3 <- X[, 3]
  x4 <- X[, 4]
  
  ###Calculate continuous response for the day
  f_values <- dgp(x1, x2, x3, x4)
  ###Standardize f(x) to have mean = 0 and sd = 1
  f_values_scaled <- scale(f_values)
  ###Apply sigmoid-function to get probabilities - logistic transformation
  probabilities <- 1 / (1 + exp(-f_values_scaled))
  ###Create data frame with ID, covariates, and response probabilities
  covariates <- data.frame(ID, x1, x2, x3, x4, probabilities)
}

############################## Simulate One Week ##############################
sim_week <- function(covariates, seed, week) {
  #covariates = data for the covariates
  #week = number of week to simulate
  ###Population size based on covariate data
  N <- nrow(covariates)
  ###Create list-object to store each days data in
  daily_list <- list()
  ###For loop to simulate data for each day (7 days)
  for(day in 1:7) {
    set.seed(seed) #for reproducibility
    ###Generate binary response for the day based on probabilities
    y <- rbinom(N, 1, covariates$probabilities) #y = 1 with prop = probabilities, else 0
    ###Store response
    daily_list[[day]] <- y
    ###Update seed to make sure not every day has the same data
    seed <- seed + 1
    
  }
  ###Combine daily data into a matrix instead of a list
  y_matrix <- do.call(cbind, daily_list)
  ###Update variable names to WeekNumberDayNumber (Week number as argument "week" when calling the function)
  colnames(y_matrix) <- paste0("W", week, "Day", 1:7)
  ###Store responses and IDs in data frame
  simulated_data <- data.frame(covariates$ID, y_matrix)
  
  return(simulated_data)
}

###################### Draw a Sample From the Population ######################

draw_sample <- function(pop, sampsize) {
  #pop = poplation data
  #sampsize = sample size
  
  ###Draw a sample of IDs to include in this sample
  sample_indices <- sample(1:nrow(pop), sampsize, replace = FALSE)
  
  ###Create sample indicator variable: 1 if included, 0 otherwise
  pop$in_sample <- ifelse(1:nrow(pop) %in% sample_indices, 1, 0)
  
  ###Randomly shuffle the numbers 1:13 with length of the sample size
  weeks <- sample(rep(1:13, length.out = sampsize))
  
  ###Assign which week each unit in the sample is supposed to be
  pop$sampleweek <- ifelse(pop$in_sample == 1, weeks, NA)
  
  ###Reorder columns such that sampweek is before the first day
  first_day <- which(colnames(pop) == "W1Day1") # Find the index
  new_column_order <- c(colnames(pop)[1:(first_day - 1)], "in_sample", 
                        "sampleweek", colnames(pop)[first_day:(ncol(pop)-2)])
  pop <- pop[, new_column_order]
  
  return(pop)
}

############################ Nonresponse Indicator ############################


response_indicator <- function(pop_data, rr) {
  #pop_data = population data
  #rr = response rate
  
  
  ###Identifiy sampled units and assign them either respondent or nonrespondent
  samp_indices <- which(pop_data$in_sample == 1)
  
  ###Calculate response size based on response rate and sample size
  response_size <- ceiling(length(samp_indices) * rr)
  
  ###Create indicator variable for respondent status (1 = respondent, 0 = not)
  ###First, initialize the response status as NA
  pop_data$response_status <- NA
  
  ###Draw a sample of respondents from the sample
  response_indices <- sample(samp_indices, response_size, replace = FALSE)
  
  ###Set status to 1 for respondents
  pop_data$response_status[response_indices] <- 1
  
  ###Set status to 0 for nonrespondents (but in sample)
  pop_data$response_status[setdiff(samp_indices, response_indices)] <- 0
  
  ###Find index for first day of first week and last day of last week
  first_day <- which(colnames(pop_data) == "W1Day1") #Index for first day of first week
  last_day <- which(colnames(pop_data) == "W13Day7") #Index of last day of last week
  
  ###For units not in the sample, we already initialized their response status to NA
  
  ###Reorder colums so that "response_status" is placed before first day's data
  new_column_order <- c(colnames(pop_data)[1:(first_day-1)], "response_status", 
                        colnames(pop_data)[first_day:last_day])
  pop_data <- pop_data[, new_column_order]
  
  ###Order them based on the response status and their ID
  # pop_data <- pop_data[order(pop_data$response_status,
  #                            -pop_data$ID, decreasing = TRUE), ]
  #####No need to sort this right now
  
  
  return(pop_data)
  
}

############################# Extract Sample Data #############################
extract_sample <- function(pop_data) {
  
  first_day <- which(colnames(pop_data) == "W1Day1")
  
  ### Filter for units that are in the sample
  sampled_data <- pop_data[pop_data$in_sample == 1, ]
  
  ### Create an empty list to store the processed data for each unit
  extracted_data_list <- list()
  
  ### Loop through each sample unit to extract the assigned week data
  for(i in 1:nrow(sampled_data)) {
    ### Get the assigned week to this unit
    assigned_week <- sampled_data$sampleweek[i]
    
    week_columns <- paste0("W", assigned_week, "Day", 1:7)
    
    ### Ensure the week data is available
    if(all(week_columns %in% colnames(sampled_data))) {
      week_data <- sampled_data[i, week_columns, drop = FALSE]
      
      colnames(week_data) <- paste0("Day", 1:7)
      
      ### Get the auxiliary data up to the first day of the week
      aux_data <- sampled_data[i, 1:(first_day - 1), drop = FALSE]
      
      ### Combine auxiliary data with the week data
      combined_data <- cbind(aux_data, week_data)
      
      ### Append the combined data to the list
      extracted_data_list[[i]] <- combined_data
    } else {
      warning(paste("Missing week data for sample unit", i))
    }
  }
  
  ### Combine all extracted data into a single data frame
  sampled_data <- do.call(rbind, extracted_data_list)
  
  return(sampled_data)
}


########################### Introduce Underreporting ###########################
include_underreporting <- function(samp_data, underreporting_rate) {
  #samp_data = population data
  #underreporting_rate = rate of underreporting as decimal number
  
  ###Include error message if underreporting rate is not between 0 and 1:
  if (underreporting_rate < 0 || underreporting_rate > 1) {
    stop("Error: underreporting_rate must be between 0 and 1.")
  }
  
  
  #Identify columns that correspond to target variable
  target_columns <- c("Day1", "Day2", "Day3", "Day4", "Day5", "Day6", "Day7")
  
  ###For each unit in the sample, check whether there is underreporting in each day (cell)
  for (i in 1:nrow(samp_data)) {
    for (j in target_columns) {
      if (samp_data[i, j] == 1) {
        ###Apply underreporting with given probability
        if (runif(1) < underreporting_rate) {
          samp_data[i, j] <- 0 #Change underreporting 1 to 0
        }
      }
    }
  }
  
  return(samp_data)
}