# Last Update: 2020-08-22 
# Install and load the readr gtrendsR & purrr packages 
installed.packages("readr","gtrendsR", "purrr") 
library(readr) 
library(gtrendsR) 
library(purrr) 

# Load your keywords list (.csv file) 
kwlist <- readLines("Your-keywords-list-path.csv")

# The function wrap all the arguments of the gtrendR::trends function and return only the interest_over_time (you can change that)
googleTrendsData <- function (keywords) { 
  
  # Set the geographic region, time span, Google product,... 
  # for more information read the official documentation https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf 
  country <- c('IT') 
  time <- ("2018-08-01 2018-08-27") 
  channel <- 'web' 
  
  trends <- gtrends(keywords, 
                    gprop = channel,
                    geo = country,
                    time = time ) 
  
  results <- trends$interest_over_time 
} 

# googleTrendsData function is executed over the kwlist
output <- map_dfr(.x = kwlist,
                  .f = googleTrendsData ) 

# Download the dataframe "output" as a .csv file 
write.csv(output, "download.csv")