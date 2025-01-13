library(lattice)
library(xtable)

# Fix the random generator seed
set.seed(123)

# Create data
data <- rnorm(1000)

# Generate separate plots
# Histogram
pdf("histogram.pdf")
histogram(data)
dev.off()

# Density plot
pdf("densityplot.pdf")
densityplot(data^12 / data^10, xlab = expression(data^12/data^10))
dev.off()

# Stripplot
pdf("stripplot.pdf")
stripplot(data^2, xlab = expression(data^2))
dev.off()

# Boxplot
pdf("boxplot.pdf")
bwplot(exp(data))
dev.off()

# Create matrix with all data used
data.all <- cbind(
  data = round(data, 2), 
  squared1 = round(data^12 / data^10, 2),
  squared2 = round(data^2, 2),
  exponent = round(exp(data), 2)
)

data_table <- xtable(data.all[1:9, ], caption = "Sample Data Table (First 10 Rows)", label = "tab:data", digits = 4)
