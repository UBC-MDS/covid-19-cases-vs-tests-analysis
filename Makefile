# Covid 19 cases vs. tests in Canada and USA
# author: Fatime Selimi, Neep Phaterpekar, Nicholas Wu, Tanmay Sharma
# date: 2020-01-17

all: data/processed/covid_can_usa.csv

# download data
data/raw/owid-covid-data.csv : src/download_data.R
	Rscript src/download_data.R --url=https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv --path=data/raw/owid-covid-data.csv

# pre-process data
data/processed/covid_can_usa.csv : src/preprocess_data.R data/raw/owid-covid-data.csv
	Rscript src/preprocess_data.R --input=data/raw/owid-covid-data.csv --out_dir=data/processed

clean:
	rm -rf data/raw/owid-covid-data.csv
	rm -rf data/processed/covid_can_usa.csv

