from sklearn.ensemble import RandomForestClassifier
import numpy as np


def fit_rf_loo(i, x, y, **kwargs):
    # Remove the ith observation for leave-one-out
    x_train = np.delete(x, i, axis=0)
    y_train = np.delete(y, i)

    # Train the RandomForestClassifier
    rf = RandomForestClassifier(**kwargs)
    rf.fit(x_train, y_train)

    # Make a prediction on the left-out observation
    preds = rf.predict(x[i:(i+1), :])[0]
    return preds