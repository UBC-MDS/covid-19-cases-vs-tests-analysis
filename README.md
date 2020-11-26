# Covid-19 Cases vs Tests Analysis

-   Authors: Fatime Selimi, Neel Phaterpekar, Nicholas Wu, Tanmay Sharma

## Introduction

The data set used in this project comes from the Our World in Data
COVID-19 Database created by Hannah Richie et al. (Max Roser and Hasell
2020). This data set examines the impact of COVID-19 on countries all
over the world, where daily statistics pertaining to the pandemic from
over 200 countries have been recorded each day since December 31st 2019.
Each row in the data set represents a date in a country, where
measurements like total cases, new daily cases, hospital admission rates
etc. are recorded. Data has been collected in conjunction with the World
Health Organization (WHO), the European Center for Disease Prevention
and Control (ECDC) and is available on [Our World in
Data](https://ourworldindata.org/coronavirus) and raw data can be found
[here](https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv).

With this data, we ask whether there is a difference in the ratio of the 
new daily tests performed to the new daily cases between Canada and the 
United States. Both of these North American countries have been hit hard by the
pandemic, but have taken different approaches in response. This question
is of interest as it will provide insight into whether the number of
daily tests being performed is meeting the demand of new daily cases in
a given country. Moreover, the comparison between Canada and the United
States may suggest which country is responding better to the demands on
the pandemic. This analysis may implicate a certain country as having a
more robust response strategy to the pandemic, which could then be
implemented in the future.

We will be performing a two-tailed hypothesis test using a variant of the 2 sample
t-test to compare the difference in the mean ratio of the daily new
tests performed to the daily cases. We have coined this term as the
‘response ratio.’ Prior to hypothesis testing, we will perform extensive
exploratory data analysis to determine what the best measure of central
tendency is within our two groups (Canada and USA). We will create a new
response ratio column in the data set by dividing the daily new tests by
the number of cases on that date. We will explore the distribution of
the data to check our samples are normally distributed and to ensure
that we have sufficient observations to obtain a strong sample size. Our
null hypothesis will be that there is no difference in the mean response
ratio between Canada and USA (ie the ratios are equal). Our alternative
hypothesis will be that the ratios are not equal. Furthermore, we will
construct a 95% confidence interval to indicate the range that we are
95% confident that the true population parameter falls under.

We will create a summary table that explores the mean response ratio
between the two groups that will be used as our estimate. We can also
understand the spread of the data and well as if the data is skewed by
considering the standard deviation and median. A simple bar chart
comparing the group means would be an initial indicator of whether the
samples means are different. This analysis will give us information on
whether we satisfy all of the conditions that are necessary to perform a
hypothesis test.

We can create a violin plot that displays the distribution as well as
the 95% confidence interval that the estimate falls under. We can see if
the confidence intervals are overlapping to get an idea of whether the
point estimates are truly different. Another good table to display will
be the tidy version of of 95% two-tailed hypothesis test. This table
will contain a p-value which will indicate if any differences seen in
the groups is statistically significant.

## Usage

To replicate the analysis, clone this GitHub repository, install the
[dependencies](#dependencies) listed below, and run the following
commands at the command line/terminal from the root directory of this
project:

    python src/download_data.py --url=https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv --path=data/processed/covid19.csv
    Rscript src/download_data.R --url=https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv --path=data/processed/covid19.csv

## Dependencies

-   Python 3.8.5 and Python packages:
    -   docopt==0.6.2
    -   pandas==1.1.4
    -   numpy
    -   altair
-   R version 3.6.1 and R packages:
    -   knitr==1.29
    -   readr==1.3.1
    -   tidyverse==1.3.0
    -   broom==0.7.1
    -   infer==0.5.3
    -   cowplot==1.1.0
    -   ggplot2

## License

The materials on analysis about Covid-19 mean response ratio for Canada
and USA are licensed under the MIT License (Copyright (c) 2020 Master of
Data Science at the University of British Columbia). If you want to
re-use/re-mix the analysis and the materials used in this project,
please provide attribution and link to this repository.

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-owidcoronavirus" class="csl-entry">

Max Roser, Esteban Ortiz-Ospina, Hannah Ritchie, and Joe Hasell. 2020.
“Coronavirus Pandemic (COVID-19).” *Our World in Data*.

</div>

</div>
