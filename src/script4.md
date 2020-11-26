"Performs statistical analysis on preprocessed data and save figures and tables 
into new file.

Usage: download_data.R --data=<data> --output_dir=<output_dir> 
 
Options:
--data=<data>          Path to the file containing the processed data (in standard csv format)
--output_dir=<output_dir>   Path (including filename) of where to locally save the figures and tables
"-> doc

library(docopt)
library(readr)

opt <- docopt(doc)

data <- read_csv(data)

summary_table 

covid_bartlett <- bartlett.test(response_ratio ~ location, data = covid_data) %>% 
      tidy() 
saveRDS(covid_bartlett, file = paste0(output_dir, '/bartlett_test.rds'))

# Function to nest data and calculate the medians of each group 
calculate_median <- data %>% 
    select(location, response_ratio) %>% 
    drop_na() %>% 
    group_by(location) %>% 
    nest() %>% 
    mutate(median = map_dbl(data, ~mean(.x$response_ratio)))
    
# Perform bootstrap generation of null distribution 

boot_dist <- covid_CAN_USA %>% 
  specify(formula = response_ratio ~ location) %>% 
  hypothesize(null = 'independence') %>% 
  generate(reps = 1000, type = 'permute') %>% 
  calculate(stat = 'diff in medians', order = c('Canada', 'United States'))


# Calculate the true difference in sample medians as estimator 
test_stat <- diff(calculate_median$median)
95ci_threshold <- quantile(null_dist$stat, c(0.0.25, 0.975))

# Plot null distribution, overlay with sample estimate and ci thresholds

null_dist <- visualize(null_dist) + 
             geom_vline(xintercept = c(threshold[1], threshold[2]),
             color = 'black',
             lty = 4) + 
     geom_vline(xintercept = test_stat, color = 'red') + 
     xlab('Difference in Medians') + 
     ylab('Count')
ggsave(filename = 'median_simulation.png')


