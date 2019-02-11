import os
import sys
import pandas as pd


if len(sys.argv) < 2:
    print('summarize_data takes at least 1 command line argment (all args should be paths to gapminder CSV files.')
    sys.exit(1)

datafiles = sys.argv[1:]

output_dir = 'summaries'

# check if the output directory already exists; if not, create it
if not os.path.exists(output_dir):
    os.mkdir(output_dir)

for datafile in datafiles:
    data = pd.read_csv(datafile, index_col='country')
    region = datafile.strip('.csv').split('_')[-1]
    # remove extra column if it exists
    if 'continent' in data.columns:
        data.drop('continent', axis=1, inplace=True)
    summary = data.T.describe()
    summary.to_csv(os.path.join(output_dir, 'summary_' + region + '.csv'))


