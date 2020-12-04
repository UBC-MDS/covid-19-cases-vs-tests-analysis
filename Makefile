# Covid 19 cases vs. tests in Canada and USA
# author: Fatime Selimi, Neel Phaterpekar, Nicholas Wu, Tanmay Sharma
# date: 2020-12-02

all: doc/covid-response.pdf

# download data
data/raw/owid-covid-data.csv : src/download_data.R
	Rscript src/download_data.R --url=https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv --path=data/raw/owid-covid-data.csv

# pre-process data
data/processed/covid_can_usa.csv : src/preprocess_data.R data/raw/owid-covid-data.csv
	Rscript src/preprocess_data.R --input=data/raw/owid-covid-data.csv --out_dir=data/processed

# run exploratory data analysis and create relevant figures
results/covid_can_usa_summary.csv results/summary_table.png results/histogram_plot.png results/violin_plot.png : src/eda_covid19.r data/processed/covid_can_usa.csv
	Rscript src/eda_covid19.r --input_path=data/processed/covid_can_usa.csv --out_dir=results

# run statistical analysis and create result figures
results/bartlett_test.png results/median_simulation.png results/simulation_pval.png : src/stat_analysis.R data/processed/covid_can_usa.csv results/covid_can_usa_summary.csv
	Rscript src/stat_analysis.R --data=data/processed/covid_can_usa.csv --sum_data=results/covid_can_usa_summary.csv --out_dir=results

# render report
doc/covid-response.pdf : doc/covid-response.Rmd doc/covid19.bib results/median_simulation.png results/simulation_pval.png results/histogram_plot.png
	Rscript -e "rmarkdown::render('doc/covid-response.Rmd', output_format = 'pdf_document')"

clean:
	rm -rf data/raw/*.*
	rm -rf data/processed/*.*
	rm -rf results/*.*
	rm -rf doc/covid-response.pdf doc/covid-response.tex doc/covid-response.pdf doc/covid-response.html doc/covid-response.md