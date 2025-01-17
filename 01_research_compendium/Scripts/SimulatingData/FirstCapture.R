###Load needed functions from "Functions.R" file.
source("Scripts/SimulatingData/Functions.R")
###load population data
population <- readRDS("data/basic_population.rds")


#----------------- Repeat Sampling and Estimation 1000 Times -----------------# 

###Set number of simulations:
num_simulations <- 1000

###Initialize a vector to store the estimated totals for each simulation
estimated_totals <- numeric(num_simulations)

###Initialize a list to store the sample data for each iteration
first_capture_list <- list()

###Extract the population size (needed later for weighting)
pop_size <- nrow(population)

###Loop over 1000 simulations
for (iteration in 1:num_simulations) {
  ###Set a new seed for each iteration 
  seed <- (1337 + iteration)
  set.seed(seed)
  
  ###Step 1: Draw sample of population
  pop <- draw_sample(pop = population,
                     sampsize = 100)
  
  ###Step 2: Include response indicator
  pop <- response_indicator(pop_data = pop, 
                            rr = 1)
  
  ###Step 3: Extract sample data
  sample_data <- extract_sample(pop_data = pop)
  
  ###Step 4: Copy data
  week_columns <- paste0("Day", 1:7)
  sample_data[paste0(week_columns, "_true")] <- sample_data[week_columns]
  
  ###Step 5: Set NA for nonrespondents
  sample_data[sample_data$response_status == 0,week_columns] <- NA
  
  ###Step 6: Apply underreporting
  sample_data <- include_underreporting(samp_data = sample_data, 
                                        underreporting_rate = 0)
  
  ###Step 7: Store current sample data in list
  first_capture_list[[iteration]] <- sample_data
  
}



# ###Look at the first sampled data 
# first_capture_data <- first_capture_list[[1]]
# 

# ###Save files
# saveRDS(first_capture_list, file = "data/first_captures_list.rds")