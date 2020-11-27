# authors: Fatime Selimi, Nicholas Wu, Tanmay Sharma, Neel Phaterpekar
# date: 2019-12-28

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
  
  bartlett_test_save(data, out_dir)
  ratio_bootstraps <- ratio_bootstraps(data)
  test_stat <- diff(sum_data$median_response_ratio)
  ci_threshold <- quantile(ratio_bootstraps$stat, c(0.025, 0.975))
  null_dist_save(ratio_bootstraps, ci_threshold, test_stat, out_dir)
  p_val_save(ratio_bootstraps, test_stat, out_dir)
}


#' Performs a Bartlett Test to test for equal variances
#' and saves the output in the desired folder
#'    
#' @param data data frame to perform Bartlett test on
#' @param out_dir the folder to save the image to 
#' 
#' @examples
#' bartlette_test_save(data, '/bartlett_test.png')
bartlett_test_save<- function(data, out_dir) {
  bartlett.test(response_ratio ~ iso_code, data = data) %>% 
  tidy() %>% 
  kable(caption = "Table 1. Summary Statistics of Response Ratio", 'html', padding = 20, digits = 2) %>%
  kable_material(c('striped', 'hover')) %>%
  kable_styling("striped") %>%
  save_kable(paste0(out_dir, "/bartlett_test.png"))
}

#' Generates a bootstrap distribution and calculates difference in 
#' median response ratio between CAN and USA for each bootstrap
#'    
#' @param data data frame to bootstrap from
#' 
#' @return data frame of the bootstraps with calculated estimator
#' 
#' @examples
#' ratio_bootstraps(data)
ratio_bootstraps <- function(data) {
  data %>% 
  specify(formula = response_ratio ~ iso_code) %>% 
  hypothesize(null = 'independence') %>% 
  generate(reps = 1000, type = 'permute') %>% 
  calculate(stat = 'diff in medians', order = c('CAN', 'USA'))
}
  

#' Plots null distribution and outlines boundaries of the 95% 
#' confidence interval. This is saved as a png in the desired folder
#'    
#' @param ratio_bootstraps distribution of bootstrapped estimators
#' @param ci_threshold data frame containing the boundaries of the 95% confidence interval 
#' @param test_stat value of the sample estimate
#' @param out_dir the folder to save the image to 
#' 
#' @examples
#' bartlette_test_save(data, '/bartlett_test.png')
null_dist_save <- function(ratio_bootstraps, ci_threshold, test_stat, out_dir) {
    visualize(ratio_bootstraps) + 
    geom_vline(xintercept = c(ci_threshold[1], ci_threshold[2]),
               color = 'black',
               lty = 4) + 
    geom_vline(xintercept = test_stat, color = 'red') + 
    xlab('Difference in Medians') + 
    ylab('Count') + 
    ggtitle('Figure 1.')
    ggsave(filename = (paste0(out_dir,'/median_simulation.png')), width = 5, height = 3)
}

#' Calculates p-value of the simulated hypothesis test
#'    
#' @param ratio_bootstraps distribution of bootstrapped estimators
#' @param test_stat value of the sample estimate
#' @param out_dir the folder to save the image to 
#' 
#' @examples
#' p_val_save(ratio_bootstraps, test_stat, out_dir)     
p_val_save <- function(ratio_bootstraps, test_stat, out_dir){
  get_pvalue(ratio_bootstraps, obs_stat = test_stat, direction = 'both') %>% 
  kable(caption = "Table 3. P-value of Simulated Hypothesis Test", 'html', padding = 20) %>%
  kable_material(c('striped', 'hover')) %>%
  kable_styling("striped") %>%
  save_kable(paste0(out_dir, "/simulation_pval.png"))
}
  
main(opt$data, opt$sum_data, opt$out_dir)

