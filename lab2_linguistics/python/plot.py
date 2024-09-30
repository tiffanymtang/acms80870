import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import seaborn as sns


def plot_ling_map(ling_data, qa_key, qid, point_size=0.5,
                  map_type="state", show_contiguous_us=True):
    """
    Plot linguistic survey results on US map

    Parameters
    ----------
    ling_data : pd.DataFrame
        The linguistic data
    qa_key : pd.DataFrame
        The question and answer key
    qid : int
        The question ID to plot
    """

    # Format question column based on qid
    if qid < 100:
        qcol = f"Q0{qid}"
    else:
        qcol = f"Q{qid}"
    
    # Merge on answer_num and qcol
    qid_key = qa_key[qa_key['qid'] == qid]
    plt_df = ling_data.merge(qid_key, left_on=qcol, right_on='answer_num', how='left')
    
    # Filter out non-contiguous states if needed
    if show_contiguous_us:
        plt_df = plt_df[~plt_df['ZIP_state_abb'].isin(['HI', 'AK'])]
    
    # Set up the map
    fig, ax = plt.subplots(figsize=(10, 8), subplot_kw={'projection': ccrs.PlateCarree()})
    ax.add_feature(cfeature.BORDERS, linestyle=':')
    ax.add_feature(cfeature.COASTLINE)
    ax.add_feature(cfeature.STATES, edgecolor='black')

    # Make colormap
    unique_answers = plt_df['answer'].unique()
    palette = sns.color_palette("viridis", len(unique_answers))
    color_map = dict(zip(unique_answers, palette))
    plt_df['color'] = plt_df['answer'].map(color_map)

    # Plot the data
    ax.scatter(plt_df['ZIP_long'], plt_df['ZIP_lat'], 
               c=plt_df['color'], s=point_size, transform=ccrs.PlateCarree())
    
    # Add legend
    for answer, color in color_map.items():
        plt.scatter([], [], c=[color], label=answer)
    plt.legend(title="Answer")

    # Add title and labels
    ax.set_title(f"{qcol}: {qid_key['question'].values[0]}")

    plt.show()
