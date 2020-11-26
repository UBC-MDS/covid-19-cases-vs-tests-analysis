"Performs statistical analysis on preprocessed data and save figures and tables 
into new file.

Usage: script4.R --data=<data> --sum_data=<sum_data> --out_dir=<out_dir>
 
Options:
--data=<data>          Path to the file containing the processed data (in standard csv format)
--sum_data=<sum_data>   Path to the file containing the summary data (in standard csv format)
--out_dir=<out_dir>   Path (including filename) of where to locally save the figures and tables
"-> doc

library(docopt)
library(readr)
library(tidyverse)
library(kableExtra)
library(broom)
library(infer)

opt <- docopt(doc)

# main function to call 

main <- function(data, sum_data, out_dir) {
  data <- read_csv(data)
  sum_data <- read_csv(sum_data)
  try({dir.create(out_dir)})
  
  summary_stats(sum_data, out_dir)
  bartlett_test(data, out_dir)
  boot_dist <- boot_dist(data)
  
  test_stat <- diff(sum_data$median_response_ratio)
  ci_threshold <- quantile(boot_dist$stat, c(0.025, 0.975))
  null_dist(boot_dist, ci_threshold, test_stat, out_dir)
}

# Function to create summary stats table 

summary_stats <- function(sum_data, out_dir) {
  kable(sum_data, caption = "Table 1. Summary Statistics of Response Ratio", 'html', digits = 2, padding = 20) %>%
  kable_material(c('striped', 'hover')) %>% 
  save_kable(paste0(out_dir, "/summary_table.png"))
}


# Function to perform and save the results of a Bartlette test for equal variance

bartlett_test<- function(data, out_dir) {
  bartlett.test(response_ratio ~ iso_code, data = data) %>% 
  tidy() %>% 
  kable(caption = "Table 1. Summary Statistics of Response Ratio", 'html', padding = 20, digits = 2) %>%
  kable_styling("striped") %>%
  save_kable(paste0(out_dir, "/bartlette_test.png"))
}

# Perform bootstrap generation of null distribution 

boot_dist <- function (data) {
  data %>% 
  specify(formula = response_ratio ~ iso_code) %>% 
  hypothesize(null = 'independence') %>% 
  generate(reps = 1000, type = 'permute') %>% 
  calculate(stat = 'diff in medians', order = c('CAN', 'USA'))
}
  
# Plot null distribution, overlay with sample estimate and ci thresholds

null_dist <- function(boot_dist, ci_threshold, test_stat, out_dir) {
    visualize(boot_dist) + 
    geom_vline(xintercept = c(ci_threshold[1], ci_threshold[2]),
               color = 'black',
               lty = 4) + 
    geom_vline(xintercept = test_stat, color = 'red') + 
    xlab('Difference in Medians') + 
    ylab('Count') + 
    ggtitle('Figure 1.')
    ggsave(filename = (paste0(out_dir,'/median_simulation.png')), width = 5, height = 3)
}
    
main(opt$data, opt$sum_data, opt$out_dir)

