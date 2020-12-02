# Covid 19 cases vs. tests in Canada and USA
# author: Fatime Selimi, Neep Phaterpekar, Nicholas Wu, Tanmay Sharma
# date: 2020-01-17

all: data/raw/owid-covid-data.csv

# download data
data/raw/owid-covid-data.csv : src/download_data.R
	Rscript src/download_data.R --url=https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv --path=data/raw/owid-covid-data.csv

clean:
	rm -rf data/raw/owid-covid-data.csv

