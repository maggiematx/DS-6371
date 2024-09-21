# Install the 'pwr' package if not already installed
# install.packages("pwr")

# Install the 'pwr' package if not already installed
# install.packages("pwr")

library(pwr)

# Parameters
n1 <- 24  # Sample size for intrinsic group
n2 <- 23  # Sample size for extrinsic group
delta <- 3  # True difference (effect size in points)
sigma <- 4.5  # Standard deviation for both groups
alpha <- 0.05  # Significance level

# Calculate Cohen's d (effect size)
effect_size <- delta / sigma

# Use pwr.t2n.test() to calculate the power for unequal sample sizes
power_result <- pwr.t2n.test(n1 = n1, n2 = n2, d = effect_size, 
                             sig.level = alpha, alternative = "greater")

# Print the calculated power
power_result$power

# Power Curve Code for Two Sample T Test
# Install the 'pwr' package if not already installed
# install.packages("pwr")

library(pwr)

# Define the effect sizes (3, 4, 5)
effectsizes <- c(3, 4, 5)

# Initialize an empty vector to store the power results
powerholder <- c()

# Parameters
n1 <- 24  # Sample size for group 1
n2 <- 23  # Sample size for group 2
sigma <- 4.5  # Standard deviation
alpha <- 0.05  # Significance level

# Loop over each effect size and calculate power
for (i in 1:length(effectsizes)) {
  # Calculate the effect size (Cohen's d)
  effect_size <- effectsizes[i] / sigma
  
  # Calculate power for the given effect size
  power_result <- pwr.t2n.test(n1 = n1, n2 = n2, d = effect_size, 
                               sig.level = alpha, alternative = "greater")
  
  # Store the power result
  powerholder[i] <- power_result$power
}

# Plot the power curve
plot(effectsizes, powerholder, type = "l", col = "blue", 
     main = "POWER CURVE", xlab = "Effect Size", ylab = "Power", lwd = 3)

abline(h = 0.91, col = "green", lwd = 3)
abline(v = 4, col = "green", lwd = 3)

abline(h = .80, col = "red", lwd = 3)
power.t.test(delta = 1.5, power = .91, sd = 3, sig.level = .05, type = "one.sample",alternative = "one.sided")$n
abline(v = 3.4, col = "red", lwd = 3)

#alternative
#Effect Size
powerholder = c()
effectsizes = seq(3,5,length = 20)

for(i in 1:20)
{
  powerholder[i] = power.t.test(n = 24,delta = effectsizes[i], sd = 4.5, sig.level = .05, type = "two.sample",alternative = "two.sided")$power
}

plot(effectsizes,powerholder,type = "l", col = "blue", main = "POWER CURVE", ylab = "POWER", lwd = 3)
