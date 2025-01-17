# Markup languages and reproducible programming in statistics

Deliverable 1: Reproducible research compendium.

This is a part of my master thesis project. Here, I start a simulation study for Capture-Recapture estimation, where I simulate a population with probabilities for a binary target variable, 4 auxiliary variables (please note, the data generating mechanism is currently completely arbitrary. For example, I am well aware, that taking the sine of a categorical variable does not make any sense.), and the outcome for one quarter (7 days for 13 weeks). 

The data can be found in the folder: data

You can find the scripts to create these data in the folder: Scripts/SimulatingData. The Functions.R script creates all needed functions to create the population and draw the first list of samples. The Population.R script creates the population. The FirstCapture.R script creates the first list of samples drawn from the population with a given set of hyperparameters. Both the Population.R and FirstCapture.R scripts automatically call the Function.R scripts, so there is no need to run this script yourself in case you want to recreate the data. 

You can find the scripts to recreate the current state of analysis in the folder: Scripts/Analysis. The Functions.R script creates the function used to calculate the true and estimated population totals. The PreliminaryAnalysis.R contains the code to calculate the statistics and produce two plots - a histogram of the distribution including the mean of the estimated population totals and the true population total, and a boxplot of the estimated population totals.

You can find these plots in the folder: Figures

The session_info.txt contains information on the R session's configuration while I created these documents.

The renv folder contains information on package dependencies and the isolated environment for this project.