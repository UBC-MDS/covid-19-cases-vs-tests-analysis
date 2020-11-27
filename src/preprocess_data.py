# author: Fatime Selimi, Neel Phaterpekar, Nicholas Wu, Tanmay Sharma
# date: 2020-11-25

"""Filters, cleans and transforms the OWID Covid-19 data (from (https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv)).
Writes the transformed and summarized data to separate csv files.
Usage: src/preprocess_data.py --input=<input> --out_dir=<out_dir>

Options:
--input=<input>       Path (including filename) to raw data (csv file)
--out_dir=<out_dir>   Path to directory where the processed data should be written
"""

import os
import pandas as pd
from docopt import docopt

opt = docopt(__doc__)

# create constants with relevant info we need from the raw csv data
ISO_CODES = {'CANADA': 'CAN', 'USA': 'USA'}
COL_NAMES = ["iso_code", "date", "new_cases", "new_tests"]
START_DATE = '2020-03-01'
END_DATE = '2020-10-31'


def main(input_file, out_dir):
    covid = pd.read_csv(input_file)

    # select only variables of interest (defined in the COL_NAMES constant) from the original dataset
    covid = covid[COL_NAMES]

    # define query that filters dataset to consider only Canada and USA
    select_can_usa_query = "iso_code == @ISO_CODES['CANADA'] or iso_code == @ISO_CODES['USA']"

    # define query that filters dataset to consider only new_tests and new_cases that have value greater than 0
    # and start_date and end_date is between the dates defined in the constants START_DATE/END_DATE
    filter_new_tests_cases_query = "new_tests > 0 and new_cases > 0 and date >= @START_DATE and date <= @END_DATE"

    covid_us_can = covid.query(select_can_usa_query).reset_index(drop=True)
    covid_us_can = covid_us_can.query(filter_new_tests_cases_query).reset_index(drop=True)

    # create a new column called "response_ratio" that computes the ratio between new_tests and new_cases
    covid_us_can["response_ratio"] = covid_us_can["new_tests"] / covid_us_can["new_cases"]

    # check if the output directory already exist; if yes - save the new processed csv in it
    try:
        covid_us_can.to_csv(f"{out_dir}/processed_data.csv", index=False)

    # if the output directory does not exist - create a new output directory first and then save the processed csv in it
    except:
        os.makedirs(os.path.dirname(out_dir), exist_ok=True)
        covid_us_can.to_csv(f"{out_dir}/processed_data.csv", index=False)


if __name__ == "__main__":
    main(opt["--input"], opt["--out_dir"])
