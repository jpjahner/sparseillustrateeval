
# Run a few tests to check everything is working -------------------------------
# Note: must first load Andrew's sparse_sim() function to the environment
source("../simulations/sparse_sim.R")
simdat <- sparse_sim(n=100, p=50, clusters=1, sparsity=0.4, beta_scale=0.5)

test.blasso <- monomvn_function(simdat$X, simdat$y, 
                                method = "blasso", revjump = T,
                                training_perc = 0.8, 
                                beta_matrix = F, iter=500,
                                prior="hs", nchains=1)

test.lasso <- monomvn_function(simdat$X, simdat$y, 
                               method = "lasso",
                               training_perc = 0.8)

test.ridge <- monomvn_function(simdat$X, simdat$y, 
                               method = "ridge",
                               training_perc = 0.8)

# Check model outputs for similarity to inputs ---------------------------------

# Note: these plots assume nchains=1 and beta_matrix=F

par(mfrow=c(3,3), las=1, pty="s")

# First, let's check how closely the beta coefficients line up
plot(test.blasso$betas ~ simdat$beta, 
     ylab="Estimated", xlab="Expected", main="BLASSO")
plot(test.lasso$betas ~ simdat$beta, 
     ylab="Estimated", xlab="Expected", main="LASSO")
plot(test.ridge$betas ~ simdat$beta, 
     ylab="Estimated", xlab="Expected", main="ridge")

# Now the simulated observations and training data
plot(test.blasso$y.sim ~ simdat$y[test.blasso$train_indices], 
     ylab="Estimated", xlab="Expected", main="BLASSO")
plot(test.lasso$y.sim ~ simdat$beta[test.lasso$train_indices], 
     ylab="Estimated", xlab="Expected", main="LASSO")
plot(test.ridge$y.sim ~ simdat$beta[test.ridge$train_indices], 
     ylab="Estimated", xlab="Expected", main="ridge")

# Now the simulated observations and training data
plot(test.blasso$y.pred ~ simdat$y[!test.blasso$train_indices], 
     ylab="Estimated", xlab="Expected", main="BLASSO")
plot(test.lasso$y.pred ~ simdat$beta[!test.lasso$train_indices], 
     ylab="Estimated", xlab="Expected", main="LASSO")
plot(test.ridge$y.pred ~ simdat$beta[!test.ridge$train_indices], 
     ylab="Estimated", xlab="Expected", main="ridge")

# Other checks??? --------------------------------------------------------------

# placeholder for running all the other planned model metrics
