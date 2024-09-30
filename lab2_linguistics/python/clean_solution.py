import pandas as pd


def merge_ling_zip_data(ling_data, zip_data):
    """
    Merge the linguistic data and zip data.
    """
    # Merge the linguistic data and zip data
    merged_data = pd.merge(ling_data, zip_data, how="left", on="ZIP")
    return merged_data


def clean_ling_data(ling_data):
    """
    Clean the linguistic data.
    """
    # ADD IN ANY DESIRED CLEANING STEPS HERE
    return ling_data