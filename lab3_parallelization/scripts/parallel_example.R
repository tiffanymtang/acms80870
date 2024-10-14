# load librariers
library(future)
library(furrr)

# to see how many cores are available on machine
availableCores()

# load in helper functions
source(here::here("R", "fit.R"))

# look at the fit_rf_loo (leave-one-out random forest fit) function
fit_rf_loo



# helper variables
n <- 30  # do leave-one-out for first 30 samples for illustration

#### first, let's try without parallelization ####
start_time <- Sys.time()
preds <- rep(NA, n) # vector of leave-one-out predictions
for (i in 1:n) {
  preds[i] <- fit_rf_loo(i, X, y)
}
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)

#### next, let's try with parallelization using future ####
plan(multisession, workers = 2)
start_time <- Sys.time()
futures <- list()
for (i in 1:2) {
  # a "future" is an abstraction for a value that may be available at some point in the future
  # If the value is queried while the future is still unresolved, the current process is blocked until the future is resolved.
  futures[[i]] <- future({
    fit_rf_loo(i, X, y)
  })
  # A future can be resolved or unresolved. Let's check whether the future we created has been resolved or unresolved
  print(resolved(futures))
}
print(Sys.time() - start_time)
preds <- lapply(futures, value)
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)

#### we can also use furrr to parallelize the for loop ####
plan(multisession, workers = 2)
start_time <- Sys.time()
preds <- furrr::future_map(
  1:n,
  function(i) {
    fit_rf_loo(i, X, y)
  }
)
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)
