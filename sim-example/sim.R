# Simulation example from section 7.2 of ARM

# Read data
## The data is cleaned using authors' code
load("congress.RData")

# Code from the textbook
fit.88 <- lm(vote.88 ~ vote.86 + incumbency.88)
summary(fit.88)

# New data for predictions
incumbency.90 <- inc90
vote.88 <- v88

n.tilde <- length(vote.88) # n.tilde is the number of predictions
n.tilde

X.tilde <- cbind(rep(1, n.tilde), vote.88, incumbency.90)
# The first column is the intercepts, followed by variables for the predictions.
# vote.88 corresponds to vote.86 in the model; incumbency.90 corresponds to the 
# incumbency.88 in the model

head(X.tilde) # Check the X.tilde object.

n.sims <- 1000
sim.88 <- sim(fit.88, n.sims) # Make simulations

#sim.88 will give 1000 betas (similar to 1000 regression output) and 1000 sigmas.
head(sim.88@coef)
head(sim.88@sigma)

# Create an array to save the predicted results.
## This is equivalent to a 1000*435 matrix. There are 1000 simulations; for each 
## iteration of simulation, we have 435 cases to predict.
y.tilde <- array(NA, c(n.sims, n.tilde))

# Predict loop
for (s in 1:n.sims){
  y.tilde[s,] <- rnorm (n.tilde, 
                        X.tilde %*% sim.88@coef[s,], 
                        # multiply the new case matrix (X.tilde) by a matrix of simulated coefficients (sim.88@coef)
                        sim.88@sigma[s]
                        # extract the sigma for the predictions and use it as the standard deviation of rnorm()
                        )
}

dim(y.tilde)
head(y.tilde)


# Note: there is an interesting behavior of rnorm. If you feed it with (N, N means, N sds),
# it will give you N values from N random normal distributions. 
# Try the following toy example:
mu <- c(1, 20, 100)
std <- c(1, 5, 10)
rnorm(3, mu, std)
