# author: Fatime Selimi, Neel Phaterpekar, Nicholas Wu, Tanmay Sharma
# date: 2020-11-25

"Filters, cleans and transforms the OWID Covid-19 data (from (https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv)).
Writes the transformed and summarized data to separate csv files.
Usage: src/process_data.R --input=<input> --out_dir=<out_dir>

Options:
--input=<input>       Path (including filename) to raw data (csv file)
--out_dir=<out_dir>   Path to directory where the processed data should be written
" -> doc


library(tidyverse)
library(readr)
library(docopt)

opt <- docopt(doc)
main <- function(input, out_dir) {
  raw_data <- read_csv(input, guess_max = 5000)
  
  # filter dataset to consider only Canada and USA and select only variables of interest
  # then create a new column called response_ratio which holds the ratio between new_tests and new_cases
  covid_can_usa <- raw_data %>%
    filter(iso_code == "CAN" | iso_code == "USA",
           date < "2020-11-01",
           new_cases != 0 & new_tests != 0) %>%
    select("iso_code", "date", "new_tests", "new_cases") %>%
    drop_na(new_tests, new_cases) %>%
    mutate(response_ratio = new_tests / new_cases)
  
  print(covid_can_usa)
  
  # create a summary table for Canada and USA that contains summary statistic of our data
  covid_can_usa_summary <-  covid_can_usa %>%
    group_by(iso_code) %>%
    summarise(
      sample_size = n(),
      mean_response_ratio = mean(response_ratio),
      median_response_ratio = median(response_ratio),
      std = sd(response_ratio),
    )
  
  # create a directory for processed data if it does not exist
  try({
    dir.create(out_dir)
  })
  
  # write processed and summary data to csv files
  write_csv(covid_can_usa, paste0(out_dir, "/covid_can_usa.csv"))
  write_csv(covid_can_usa_summary,
            paste0(out_dir, "/covid_can_usa_summary.csv"))
}

main(opt$input, opt$out_dir)
