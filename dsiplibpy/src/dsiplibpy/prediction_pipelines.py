import copy
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.metrics import accuracy_score, roc_auc_score, average_precision_score


def run_binary_classification_pipeline(X_train, y_train, X_test, y_test,
                                       models, cv_params, nfolds=5, metrics="auto"):
    """
    Run a binary classification pipeline for a set of models and hyperparameters.

    Parameters
    ----------
    X_train : array-like
        Training features.
    y_train : array-like
        Training labels.
    X_test : array-like
        Test features.
    y_test : array-like
        Test labels.
    models : dict
        Dictionary of model objects.
    cv_params : dict
        Dictionary of hyperparameters to search over for each model.
    nfolds : int
        Number of cross-validation folds.
    metrics : dict or str
        Dictionary of metrics to evaluate the models. If "auto", default metrics
        are used.

    Returns
    -------
    errs : dict
        Dictionary of evaluation metrics for each model.
    preds : dict
        Dictionary of predictions for each model.
    prob_preds : dict
        Dictionary of predicted probabilities for each model.
    tuned_pipelines : dict
        Dictionary of tuned pipelines.

    Examples
    --------
    >>> from sklearn.datasets import make_classification
    >>> from sklearn.model_selection import train_test_split
    >>> from sklearn.ensemble import RandomForestClassifier
    >>> from sklearn.linear_model import LogisticRegression
    >>> from sklearn.svm import SVC
    >>> from dsiplibpy import run_binary_classification_pipeline
    >>> X, y = make_classification(n_samples=100, n_features=20, n_informative=5, random_state=0)
    >>> X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)
    >>> models = {
    ...     "rf": RandomForestClassifier(),
    ...     "lr": LogisticRegression(),
    ...     "svc": SVC()
    ... }
    >>> cv_params = {
    ...     "rf__n_estimators": [10, 50, 100],
    ...     "lr__C": [0.1, 1.0, 10.0],
    ...     "svc__C": [0.1, 1.0, 10.0]
    ... }
    >>> errs, preds, prob_preds, tuned_pipelines = run_binary_classification_pipeline(
    ...     X_train, y_train, X_test, y_test, models, cv_params, nfolds=5, metrics="auto"
    ... )
    """
    if metrics == "auto":
        metrics = {
            "accuracy": (accuracy_score, "predict"),
            "auroc": (roc_auc_score, "predict_proba"),
            "auprc": (average_precision_score, "predict_proba")
        }
    
    pipes = {}
    for model_name, model in models.items():
        pipe = Pipeline(steps=[(model_name, model)])
        pipes[model_name] = pipe

    errs = {}
    preds = {}
    prob_preds = {}
    tuned_pipelines = {}
    for pipe_name, pipe in pipes.items():
        print(pipe_name)
        # get relevant CV parameters given the steps of the pipeline
        cv_param_grid = {
            key: cv_params[key] for key in cv_params.keys() \
                if key.startswith(tuple(pipe.named_steps.keys()))
        }
        # run CV for pipeline
        pipe_search = GridSearchCV(pipe, cv_param_grid, cv=nfolds)
        pipe_search.fit(X_train, y_train)
        tuned_pipelines[pipe_name] = copy.deepcopy(pipe_search)

        if X_test is not None and y_test is not None:
            # make predictions
            preds[pipe_name] = pipe_search.predict(X_test)
            if hasattr(pipe_search, "predict_proba"):
                prob_preds[pipe_name] = pipe_search.predict_proba(X_test)
                if prob_preds[pipe_name].ndim == 2:
                    prob_preds[pipe_name] = prob_preds[pipe_name][:, 1]

            # evaluate predictions
            err_out = {}
            for metric_name, metric in metrics.items():
                metric_fun = metric[0]
                metric_type = metric[1]
                if metric_type == "predict":
                    err_out[metric_name] = metric_fun(y_test, preds[pipe_name])
                elif metric_type == "predict_proba":
                    if pipe_name in prob_preds.keys():
                        err_out[metric_name] = metric_fun(y_test, prob_preds[pipe_name])
            errs[pipe_name] = copy.deepcopy(err_out)

    return errs, preds, prob_preds, tuned_pipelines