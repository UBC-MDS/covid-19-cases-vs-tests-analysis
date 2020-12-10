# Docker file for the Covid 19 Cases vs. Tests
# author: Fatime Selimi, Neel Phaterpekar, Nicholas Wu, Tanmay Sharma
# date: 2020-12-10

# use rocker/tidyverse as the base image
FROM rocker/tidyverse

# install R packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  && install2.r --error \
    --deps TRUE \
    readr \
    knitr \
    ggthemes

RUN Rscript -e "install.packages('broom')" \
 "install.packages('kableExtra')" \
 "install.packages('magick')" \
 "install.packages('cowplot')" \
 "install.packages('docopt')" \
 "install.packages('infer')"

