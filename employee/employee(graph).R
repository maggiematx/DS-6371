# Fired group ages
fired <- c(34, 37, 37, 38, 41, 42, 43, 44, 44, 45, 45, 45, 46, 48, 49, 53, 53, 54, 54, 55, 56)

# Not fired group ages
not_fired <- c(27, 33, 36, 37, 38, 38, 39, 42, 42, 43, 43, 44, 44, 44, 45, 45, 45, 45, 46, 46, 47, 47, 48, 48, 49, 49, 51, 51, 52, 54)

# Histogram for "fired" group
hist(fired, main="Histogram of Fired Ages", xlab="Age", col="lightblue")

# Histogram for "not fired" group
hist(not_fired, main="Histogram of Not Fired Ages", xlab="Age", col="lightgreen")

# Q-Q plot for "fired" group
qqnorm(fired, main="Q-Q Plot of Fired Ages")
qqline(fired, col="red")

# Q-Q plot for "not fired" group
qqnorm(not_fired, main="Q-Q Plot of Not Fired Ages")
qqline(not_fired, col="red")

# Combine the data
fired <- c(34, 37, 37, 38, 41, 42, 43, 44, 44, 45, 45, 45, 46, 48, 49, 53, 53, 54, 54, 55, 56)
not_fired <- c(27, 33, 36, 37, 38, 38, 39, 42, 42, 43, 43, 44, 44, 44, 45, 45, 45, 45, 46, 46, 47, 47, 48, 48, 49, 49, 51, 51, 52, 54)

# Create a data frame
data <- data.frame(
  Age = c(fired, not_fired),
  Group = c(rep("Fired", length(fired)), rep("Not Fired", length(not_fired)))
)

# Load ggplot2 for visualization
library(ggplot2)

# Create the density plot
ggplot(data, aes(x = Age, color = Group, fill = Group)) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of Ages for Fired and Not Fired Groups") +
  labs(x = "Age", y = "Density") +
  theme_minimal()

boxplot(fired, not_fired, names = c("fired", "not fired"), main="Boxplots of Employee Age Groups")
