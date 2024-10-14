# to submit job: qsub -N parallel_example submit_r_job.sh ../scripts/parallel_example
library(future)
library(furrr)

# load in helper functions
source(here::here("R", "fit.R"))

# set seed
set.seed(242)

# set up parallel processing
n_cores <- as.integer(Sys.getenv("NSLOTS"))
print(paste("Using the following number of cores:", n_cores))
plan(multicore, workers = n_cores)

# helper variables
n <- n_cores * 10  # do leave-one-out for this many number of samples
DATA_DIR <- here::here("data")
RESULTS_DIR <- here::here("results")

# load in TCGA breast cancer data
X <- data.table::fread(
  here::here(DATA_DIR, "X_tcga_cleaned.csv"),
  data.table = FALSE
)
y <- data.table::fread(
  here::here(DATA_DIR, "Y_tcga.csv"),
  data.table = FALSE
)[, 1] |>
  as.factor()

start_time <- Sys.time()
preds_furrr <- furrr::future_map(
  1:n,
  function(i) {
    fit_rf_loo(i, X, y)
  },
  .options = furrr::furrr_options(seed = TRUE)
)
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)

if (!dir.exists(RESULTS_DIR)) {
  dir.create(RESULTS_DIR)
}
saveRDS(
  preds_furrr, 
  file = here::here(RESULTS_DIR, "preds_furrr.rds")
)