#------------------------------ Initial Settings ------------------------------# 
if(!require(tidyverse)){install.packages('tidyverse')}
library(tidyverse)
if(!require(purrr)){install.packages('purrr')}
library(purrr)

###Compute the population totals from a list of (sample) data frames
estimate_population_total <- function(data_list, pop_size) {
  est_pop_totals <- map(data_list, ~ {
    
    ###Calculate number of respondents
    num_respondents <- sum(.x$response_status == 1)
    
    ###Estimate the population
    .x %>%
      filter(response_status == 1) %>%
      select("Day1":"Day7") %>% 
      sum() * pop_size / num_respondents * 13
    
  })
  
  return(unlist(est_pop_totals))
  
}
