# author: Fatime Selimi, Neel Phaterpekar, Nicholas Wu, Tanmay Sharma
# date: 2020-11-25

"Creates eda table and plots for the response ratio from the COVID-19 data (from https://github.com/owid/covid-19-data/tree/master/public/data).
Saves the plots as a pdf and png file.
Usage: src/eda_covid19.r --input_path=<input_path> --out_dir=<out_dir>
  
Options:
--input_path=<input_path>     Path (including filename) to processed data in csv format
--out_dir=<out_dir>           Path to directory where the plots should be saved
" -> doc

library(tidyverse)
library(knitr)
library(infer)
library(cowplot)
library(docopt)
library(ggthemes)
library(kableExtra)
library(webshot)
library(magick)
webshot::install_phantomjs()

opt <- docopt(doc)

main <- function(input_path, out_dir) {
  
    #create a table summarizing the key statistics
    covid_can_usa <- read_csv(input_path)
    
    covid_can_usa_summary <-  covid_can_usa %>%
        group_by(iso_code) %>%
        summarise(sample_size = n(),
                  mean_response_ratio = mean(response_ratio),
                  median_response_ratio = median(response_ratio),
                  standard_dev = sd(response_ratio)
        )
    
    #create a directory for processed data if it does not exist
    try({
      dir.create(out_dir)
    })
    
    #save summary table as a .csv file for statistic analysis
    write_csv(covid_can_usa_summary, paste0(out_dir, "/covid_can_usa_summary.csv"))
    
    #save summary table as a .png file
    kable(covid_can_usa_summary,
          caption = "Table 1. Summary Statistics of Response Ratio",
          formst = 'html',
          digits = 2,
          padding = 20) %>%
      kable_material(c('striped', 'hover'), font_size = 30) %>%
      save_kable(paste0(out_dir, "/summary_table.png"))


    #create the histogram plot
    dist_can <- create_distribution_plot(covid_can_usa, "CAN", "Canada")

    dist_usa <- create_distribution_plot(covid_can_usa, "USA", "USA")

    histo_plot <- plot_grid(dist_usa, dist_can, ncol=1)

    #save histogram as a .png file
    ggsave(paste0(out_dir, "/histogram_plot.png"),
           histo_plot,
           width = 5,
           height = 3)
    
    #create a violin plot with mean point
    violin_plot <- covid_can_usa %>%
        mutate(across(iso_code, fct_relabel, .fun = str_wrap, width = 30)) %>%
        ggplot(aes(x = response_ratio, y = iso_code)) +
        geom_violin(fill = "yellow",
                    alpha = .1, adjust = .8, trim = FALSE) +
        stat_summary(fun = mean,
                     colour = "red",
                     geom = "point") +
        scale_x_continuous(labels = scales::label_number_si(), 
                           limits = c(0, 100)) +
        labs(title = "COVID-19 Response Ratio - Canada vs USA",
             subtitle = "Means shown in red",
             x = "Mean of Response Ratio in both countries",
             y = "Country")
    
    #save violin plot as a .png file
    ggsave(paste0(out_dir, "/violin_plot.png"),
           violin_plot,
           width = 8,
           height = 10)
    
}

#define a function for creating distribution plot


create_distribution_plot <- function(data, code, country){
  #' Create histogram plot via ggplot
  #'
  #' @param data the data containing the distribution 
  #' @param code iso_code
  #' @param country country name
  #'
  #' @return ggplot histogram 
  #'
  #' @examples dist_usa <- create_distribution_plot(covid_can_usa, "USA", "USA")
  data %>% 
    filter(iso_code == code) %>%
    ggplot(aes(response_ratio)) +
    geom_histogram(bins=50) +
    xlab(paste0("Response Ratio, ", country)) +
    ggtitle(paste0("Sample Distribution of ", country, "'s Response Ratio"))+
    theme(plot.title = element_text(size = 9)) +
    theme(axis.title = element_text(size = 9))
}

main(opt$input_path, opt$out_dir)

