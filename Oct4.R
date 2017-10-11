################################################################################
# October 4, 2017

# New dataset today -- New Haven Road Race
# http://www.newhavenroadrace.org/

# This is an annual Labor Day running race that celebrated its 40th anniversary
# this year. Results for each of the races can be found on the website -- 
# and it really goes back many, many years.

# We'll pause to look at the dataset. Content-wise, this is a lot more 
# straightforward compared to the Real Estate data. There are only a handful
# of columns, and they do appear pretty regular. An example of some of the 
# columns here:

# - Place (ranking)
# - Name
# - Div (division of ranking)
# - Time
# - Nettime 

# For today, you will be scraping data for ONE of the following events. 

# 1. 20K run
# 2. 5K run


# Once you've determined which of these to take on, 
# proceed with scraping at least the most recent 5 years of data. 
# Obviously, some columns are more important to get right than others. 


## example for 20k
Sys.setlocale('LC_ALL','C') 





res2017 <- readLines("http://www.newhavenroadrace.org/wp-content/uploads/2015/04/NHRR-2017-20k-results-update-3.txt")
cnames <- res2017[6]
equalsigns <- res2017[7]
widths <- c(1, gregexpr(" ", equalsigns)[[1]])
widths <- diff(widths)

res2017 <- read.fwf("http://www.newhavenroadrace.org/wp-content/uploads/2015/04/NHRR-2017-20k-results-update-3.txt", 
                    skip=7, widths=widths)
head(res2017)
tail(res2017)



### Due Monday:

# You should have scraped at least 5 years of the most 
# recent data corresponding to EITHER the 5k or 20k event.

# 
# Submit a single .csv containing the following columns:
# - Event ('5k' or '20k' for all rows)
# - Year (numeric)
# - Name ('Firstname Lastname' format)
# - Age (may be missing for recent years)
# - Sex ('M' or 'F')
# - Div (something like 'M20-29')
# - Nettime (numeric, instead of 59:05, convert this to decimal minutes
#            like 59.08)

# Also submit your .R script for generating the scrape.