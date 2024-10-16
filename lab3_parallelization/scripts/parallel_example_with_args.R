# to submit job: qsub -N parallel_example_with_args submit_r_array_job.sh scripts/parallel_example_with_args
library(future)
library(furrr)
library(optparse)

# define the list of command line options
option_list <- list(
  make_option(
    "--array_id", type = "integer", default = 1, 
    help = "array id", metavar = "integer"
  )
)

# parse the command line options
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# translate option into selected method name
method_names <- c("rf", "knn")
method_name <- method_names[opt$array_id]
print(method_name)

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

if (method_name == "rf") {
  fit_loo <- fit_rf_loo
} else if (method_name == "knn") {
  fit_loo <- fit_knn_loo
} else {
  stop("Invalid method name")
}

start_time <- Sys.time()
preds_furrr <- furrr::future_map(
  1:n,
  function(i) {
    fit_loo(i, X, y)
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
  file = here::here(RESULTS_DIR, sprintf("preds_%s.rds", method_name))
)