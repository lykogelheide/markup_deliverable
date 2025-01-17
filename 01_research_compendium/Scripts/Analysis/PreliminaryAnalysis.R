###Load needed functions from "Functions.R" file.
source("Scripts/Analysis/Functions.R")
###load population data
population <- readRDS("data/basic_population.rds")
sample_data_list <- readRDS("data/first_captures_list.rds")


#--------------- Compute Estimates from Sample Data Frame List ---------------#
pop_size <- nrow(population)

estimated_totals <- estimate_population_total(data_list = sample_data_list,
                                              pop_size = pop_size)
# head(estimated_totals, n = 25)

#----------------------- Compute True Population Total -----------------------#
###Extract index of first days data
dayOne <- which(colnames(population) == "W1Day1")
###Compute true population total
pop_total_true <- sum(population[,dayOne:ncol(population)])
pop_total_true

#-------------------------- Post-Simulation Analysis --------------------------#
###Mean Estimate
mean_estimate <- mean(estimated_totals)
mean_estimate

cat("The true population total is:", pop_total_true,
    "and the estimated population total is:",  mean_estimate,
    "The difference between them is:", pop_total_true - mean_estimate)

###Standard Deviation Estimate
sd_estimate <- sd(estimated_totals)
sd_estimate
###Histogram Estimates

histogram <- ggplot(data.frame(estimated_totals), aes(x = estimated_totals)) + 
  geom_histogram(binwidth = 1000, color = "black", fill = "steelblue4", alpha = 0.6) + 
  geom_vline(aes(xintercept = mean_estimate, color = "Mean Estimate"), 
             linetype = "dashed", size = 1.5) + 
  geom_vline(aes(xintercept = pop_total_true, color = "True Population Total"), 
             linetype = "dashed", size = 1.5) + 
  labs(title = "Distribution of Estimated Population Totals", 
       x = "Estimated Population Total", 
       y = "Frequency", 
       color = "Legend") +  # This will add the color legend title
  scale_color_manual(values = c("Mean Estimate" = "mediumorchid", 
                                "True Population Total" = "springgreen")) +
  theme_bw() +
  theme(legend.position = "inside", legend.position.inside = c(0.75, 0.9)) +
  annotate("text", x = mean_estimate, y = max(table(estimated_totals)), 
           label = paste("Mean: ", round(mean_estimate)), 
           color = "mediumorchid", vjust = -11.5, hjust = -1.95) + 
  annotate("text", x = pop_total_true, y = max(table(estimated_totals)), 
           label = paste("True Population: ", round(pop_total_true, 2)), 
           color = "springgreen", vjust = -9.5, hjust = -0.9) 

ggsave("Figures/histogram.png", plot = histogram)

###Boxplot
boxplot <- ggplot(data.frame(estimated_totals), aes(y = estimated_totals)) + 
  geom_boxplot(fill = "slategray3", color = "black", alpha = 0.7) + 
  labs(title = "Boxplot of Estimated Population Totals", 
       y = "Estimated Population Total") + 
  theme_bw()

ggsave("Figures/boxplot.png", plot = boxplot)
