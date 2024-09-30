import pandas as pd


def load_q_and_a_key(path = "../data"):
    """
    Load the question and answer key from the given path.
    """
    # Load the question and answer key
    q_and_a_key = pd.read_csv(f"{path}/q_and_a_key.csv")
    return q_and_a_key


def load_ling_data(path = "../data"):
    """
    Load the linguistic data from the given path.
    """
    # FILL IN: Load the linguistic data
    return


def load_zip_data(path = "../data"):
    """
    Load the zip locations data from the given path.
    """
    # Load the zip data
    zip_data = pd.read_csv(
        f"{path}/zip_locations.csv",
        dtype={"ZIP": str}
    )
    return zip_data