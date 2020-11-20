"Downloads csv data from the web to a local filepath as a csv.
Usage: download_data.R --url=<url> --path=<path> 
 
Options:
<url>               URL from where to download the data (must be in standard csv format)
<path>          Path (including filename) of where to locally write the file
"-> doc

library(docopt)
library(readr)

opt <- docopt(doc)

main <- function(url, path) {
  data = read_csv(url)
  write_csv(data, path)
}

main(opt$url, opt$path)
