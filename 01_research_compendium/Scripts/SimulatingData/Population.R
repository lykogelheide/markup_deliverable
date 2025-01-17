###Load needed functions from "Functions.R" file.
source("Scripts/SimulatingData/Functions.R")

###simulate ID, covariates & response probabilities
N <- 10000
x1 <- rbinom(N, 1, 0.5)  #bernoulli distribution; e.g. private/legal entity, leasing status
x2 <- sample(c(rep(1, 1/3 * N), rep(2, 1/3 * N), rep(3, 1/3 * N)), N, replace = TRUE) #categorical distribution; e.g. age, size, type of vehicle.
x3 <- sample(c(rep(1, 1/8 * N), rep(2, 1/8 * N), rep(3, 1/8 * N), rep(4, 1/8 * N),
               rep(5, 1/8 * N), rep(6, 1/8 * N), rep(7, 1/8 * N), rep(8, 1/8 * N)), N, replace = TRUE) #categorical distribution; e.g. economic activity, fuel type
x4 <- rnorm(N, 5, 1.5)  # Normal distribution;  e.g. horsepower, loading capacity in tons.
X <- cbind(x1, x2, x3, x4) # Combine data into a matrix 


covs <- sim_covariates(X = X, seed = 1337) 

###create list to store each weeks data in:
weeks_list <- list() 

seed <- 321
for(i in 1:13) {
  set.seed(seed)
  tmp_week <- sim_week(covariates = covs, seed = seed, week = i)
  weeks_list[[i]] <- tmp_week
  seed <- seed + 1
}

###Right now, weeks_list is of type list. Need to store and combine the elements
#of the list as a data frame using Reduce()
y_matrix <- Reduce(left_join, weeks_list)
###Link covariate data and response data by ID variables
population <- left_join(covs, y_matrix, by = c("ID" = "covariates.ID"))

#View(population)
#Save the data:
#saveRDS(population, file = "data/basic_population.rds")
#write.csv(population, file = "data/population.csv")
