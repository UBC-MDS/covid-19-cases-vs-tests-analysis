# author: Fatime Selimi, Neel Phaterpekar, Nicholas Wu, Tanmay Sharma
# date: 2020-11-27

"""
Creates eda altair plots for the response ratio from the COVID-19 data (from https://github.com/owid/covid-19-data/tree/master/public/data).
Saves the plots as png files.
Usage: src/eda_covid19.py --input=<input> --out_dir=<out_dir>

Options:
--input=<input>       Path (including filename) to processed data (csv file)
--out_dir=<out_dir>   Path to directory where png files should be saved
"""

import calendar
import os

import pandas as pd
import altair as alt
from docopt import docopt
from altair_saver import save
from selenium import webdriver
from webdriver_manager.chrome import ChromeDriverManager

driver = webdriver.Chrome(ChromeDriverManager().install())

opt = docopt(__doc__)

# create constants with relevant info we need from the processed csv data
ISO_CODES = {'CANADA': 'CAN', 'USA': 'USA'}
START_DATE = '2020-03-01'
END_DATE = '2020-10-31'


def main(input_file, out_dir):
    covid = pd.read_csv(input_file)

    line_plot = create_line_plot(covid)
    ridgeline_plot = create_ridgeline_plot(covid)

    # check if the output directory already exist; if yes - save the plots in it
    try:
        save(line_plot, f"{out_dir}/line_plot.png", method='selenium', webdriver=driver)
        save(ridgeline_plot, f"{out_dir}/ridgeline_plot.png", method='selenium', webdriver=driver)

        # if the output directory does not exist - create a new output directory first and then save the plots in it
    except:
        os.makedirs(os.path.dirname(out_dir), exist_ok=True)
        save(line_plot, f"{out_dir}/line_plot.png", method='selenium', webdriver=driver)
        save(ridgeline_plot, f"{out_dir}/ridgeline_plot.png", method='selenium', webdriver=driver)


# helper function that creates a dict with month number and month name, e.g. {1: 'January', 2: 'February', ...}
def month_num_name_map():
    month_map = {}
    for i in range(1, 13):
        month_map[i] = calendar.month_name[i]
    return month_map


def create_line_plot(data):
    """A function that creates a line plot for covid_19 CAN & USA dataset.

    Parameters
    ----------
    data
        input data set from preprocessed csv.

    Returns
    -------
    altair object
        returns the plot as a altair object
    """
    data['month'] = pd.DatetimeIndex(data['date']).month
    months_lookup = month_num_name_map()
    data['month'] = data.apply(lambda row: months_lookup[row.month], axis=1)
    line_plt = (alt.Chart(data, title="COVID-19 Response Ratio - Canada vs USA").mark_line().encode(
        alt.X("month", sort=list(months_lookup.values()), title="Month(2020)"),
        alt.Y("mean(response_ratio)", title="Mean of Response Ratio"),
        color=alt.Color("iso_code", legend=alt.Legend(title="Country"))
    )).properties(height=350, width=650)

    return line_plt


def generate_ridgeline_plot(data, x_lab_country_name):
    """A function that generates a ridgeline plot for covid_19 CAN & USA dataset.

    Parameters
    ----------
    data
        input data set from preprocessed csv.
    x_lab_country_name
        name of the the country for which we want to generate the ridgeline plot

    Returns
    -------
    altair object
        returns the plot as a altair object
    """
    step = 40
    overlap = 1

    ridgeline_plt = alt.Chart(data, height=step).transform_timeunit(
        Month='month(date)'
    ).transform_joinaggregate(
        mean_response_ratio='mean(response_ratio)', groupby=['Month']
    ).transform_bin(
        ['bin_max', 'bin_min'], 'response_ratio'
    ).transform_aggregate(
        value='count()', groupby=['Month', 'mean_response_ratio', 'bin_min', 'bin_max']
    ).transform_impute(
        impute='value', groupby=['Month', 'mean_response_ratio'], key='bin_min', value=0
    ).mark_area(
        interpolate='monotone',
        fillOpacity=0.8,
        stroke='lightgray',
        strokeWidth=0.5
    ).encode(
        alt.X('bin_min:Q', bin='binned', title=f'Mean Response Ratio in {x_lab_country_name}'),
        alt.Y(
            'value:Q',
            scale=alt.Scale(range=[step, -step * overlap]),
            axis=None
        ),
        alt.Fill('mean_response_ratio:Q')
    ).facet(
        row=alt.Row(
            'Month:T',
            title=None,
            header=alt.Header(labelAngle=0, labelAlign='right', format='%B')
        )
    ).properties(
        title='',
        bounds='flush'
    )

    return ridgeline_plt


def create_ridgeline_plot(data):
    """A function that creates a ridgeline plot for covid_19 CAN & USA dataset.

    Parameters
    ----------
    data
        input data set from preprocessed csv.

    Returns
    -------
    altair object
        returns the plot as a altair object
    """
    usa = data.query(
        "iso_code == @ISO_CODES['USA'] and new_tests > 0 and date > @START_DATE and date <= @END_DATE").reset_index(
        drop=True)
    can = data.query(
        "iso_code == @ISO_CODES['CANADA'] and  new_tests > 0 and date >= @START_DATE and date <= @END_DATE").reset_index(
        drop=True)

    can_usa_plt = alt.hconcat(generate_ridgeline_plot(can, "Canada"), generate_ridgeline_plot(usa, "USA"),
                              title="COVID-19 Response Ratio - Canada vs USA").configure_facet(
        spacing=0
    ).configure_view(
        stroke=None
    ).configure_title(
        anchor='middle'
    )

    return can_usa_plt


if __name__ == "__main__":
    main(opt["--input"], opt["--out_dir"])
