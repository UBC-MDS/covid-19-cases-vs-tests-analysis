"Performs statistical analysis on preprocessed data and save figures and tables 
into new file.

Usage: script4.R --data=<data> --out_dir=<out_dir> 
 
Options:
--data=<data>          Path to the file containing the processed data (in standard csv format)
--out_dir=<out_dir>   Path (including filename) of where to locally save the figures and tables
"-> doc

library(docopt)
library(readr)
library(tidyverse)

opt <- docopt(doc)

# main function to call 

main <- function(data, out_dir) {
  data <- read_csv(data)
  try({dir.create(out_dir)})
  summary_stats(data, out_dir)
  bartlett_test(data, out_dir)
  calculate_median(data)
  boot_dist(data)
  test_stat <- diff(calculate_median$median)
  ci_threshold <- quantile(null_dist$stat, c(0.0.25, 0.975))
  null_dist(boot_dist, ci_threshold, test_stat, out_dir)
}

# Function to create summary stats table 

summary_stats <- function(data, out_dir) {
  data %>%  
  group_by(location) %>% 
  summarise(n = n(),
            mu = mean(response_ratio),
            median = median(response_ratio),
            sd = sd (response_ratio),
            se = sd / (sqrt(n))
  ) %>% 
  saveRDS(file = paste0(out_dir, '/summary_stats.rds'))
  }

# Function to perform and save the results of a Bartlette test for equal variance

bartlett_test<- function(data, out_dir){
  bartlett.test(response_ratio ~ location, data = covid_data) %>% 
  tidy() %>% 
  saveRDS(file = paste0(out_dir, '/bartlett_test.rds'))
}

# Function to nest data and calculate the medians of each group 
calculate_median <- function(data) {
    data %>% 
    select(location, response_ratio) %>% 
    drop_na() %>% 
    group_by(location) %>% 
    nest() %>% 
    mutate(median = map_dbl(data, ~mean(.x$response_ratio)))
}

# Perform bootstrap generation of null distribution 

boot_dist <- function (data) {
  data %>% 
  specify(formula = response_ratio ~ location) %>% 
  hypothesize(null = 'independence') %>% 
  generate(reps = 1000, type = 'permute') %>% 
  calculate(stat = 'diff in medians', order = c('Canada', 'United States'))
}
  
# Calculate the true difference in sample medians as estimator 

test_stat <- diff(calculate_median$median)
ci_threshold <- quantile(null_dist$stat, c(0.0.25, 0.975))

# Plot null distribution, overlay with sample estimate and ci thresholds

null_dist <- function(boot_dist, ci_threshhold, test_stat, out_dir) {
    visualize(boot_dist) + 
      geom_vline(xintercept = c(ci_threshold[1], ci_threshold[2]),
      color = 'black',
      lty = 4) + 
      geom_vline(xintercept = test_stat, color = 'red') + 
      xlab('Difference in Medians') + 
      ylab('Count')
ggsave(filename = (paste0(out_dir,'/median_simulation.png'))
}



