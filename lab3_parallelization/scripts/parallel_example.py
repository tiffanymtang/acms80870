import pandas as pd
import numpy as np
import pickle as pkl
import sys
import os
from os.path import join as oj
import time
from joblib import Parallel, delayed

sys.path.append(".")
from python.fit import fit_rf_loo

if __name__ == '__main__':

    # Helper variables
    DATA_PATH = oj("data")
    RESULTS_PATH = oj("results")
    n_cores = int(os.getenv("NSLOTS"))
    print("Number of cores available:", n_cores)
    n = n_cores * 10  # Do leave-one-out for the first n samples

    X = pd.read_csv(oj(DATA_PATH, "X_tcga_cleaned.csv")).values  # Convert to NumPy array
    y = pd.read_csv(oj(DATA_PATH, "Y_tcga.csv")).iloc[:, 0].values  # Convert to NumPy array

    start_time = time.time()
    preds = Parallel(n_jobs=n_cores)(delayed(fit_rf_loo)(i, X, y) for i in range(n))
    end_time = time.time()
    execution_time = end_time - start_time
    print("Execution time with joblib parallelization:", execution_time)

    os.makedirs(RESULTS_PATH, exist_ok=True)
    with open(oj(RESULTS_PATH, "preds_joblib.pkl"), "wb") as f:
        pkl.dump(preds, f)
