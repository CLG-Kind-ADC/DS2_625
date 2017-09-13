### obtain a csv of addresses

setwd("/volumes/ssg ssd t3/ssd_statgrad/ds2_625")
folder <- "hw3_submit"
files <- dir(folder, pattern=".csv")


alldf <- list()
for (i in 1:length(files)) {
  netid <- gsub(".*_hw3_(.*).csv", "\\1", files[i])
  netid <- gsub("(.*)-[0-9]", "\\1", netid)
  alldf[[netid]] <- read.csv(paste0(folder, "/", files[i]), as.is=TRUE)
}

thiscol <- "location"

allsols <- sapply(alldf, function(df) df[,thiscol])
head(allsols)

v <- apply(allsols,
            1, function(y) length(unique(y)))
table(v)
which(v==2)

allsols[which(v==2),]

finaladdresses <- allsols[,1]

locations <- finaladdresses
pid <- 1:length(finaladdresses)
city <- "New Haven"
state <- "CT"

address_out <- data.frame(pid = pid,
                          location=locations,
                          city=city,
                          state=state,
                          zip="")
write.csv(address_out, "final_locations.csv", row.names=FALSE)




## use the Census API
library(httr)
url <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"

# the API can only handle up to 1000 requests
addresses <- address_out[1:30,]

# have to write out these 30 addresses into a .csv file
f <- tempfile(fileext = ".csv") # creates a temporary file connection somewhere
write.csv(addresses, f, row.names=FALSE, col.names = NULL)

# the API call
req <- POST(url, body=list(addressFile = upload_file(f),
                           benchmark = "Public_AR_Census2010",
                           vintage = "Census2010_Census2010"),
            encode = "multipart")

length(content(req, "text", encoding = "UTF-8"))

# easier to write the output into a .csv file and then read it in
outfile <- tempfile(fileext = ".csv")
writeLines(content(req, "text", encoding = "UTF-8"), outfile)


v <- read.csv(outfile, header=FALSE, as.is=TRUE)
head(v)
# I'm pulling in some column headers from the API document
colnames(v) <- c("pid", "address", "match1", "match2", "address_match",
                 "latlong", "lineid", "side", "state", "county", "tract", "block")
head(v)

table(v$match1)
table(v$match2)



### HW4 due Next Monday 11AM
# We now have 8 data frames for a master scrape. You should use all of these 
# to revise your team solution. You can feel free to use the 'location'
# column that I've pulled together from your data frames. I'll bet a lot of
# discrepancies between solutions are due to the `multibuilding = TRUE` 
# properties. For sanity's sake, let's just ignore these properties as 
# part of this final clean-up process. 

# Using this 'location' column, go ahead and use the `httr` package in 
# conjunction with the Census API to obtain latitudes/longitudes for each
# property. You will need to create multiple API calls since each API 
# call can only handle a total of 1000 addresses simultaneously. After 
# obtaining results from the Census API for all pid's, you should then merge
# in the following new columns to your master data frame:
#   - match1
#   - match2
#   - latitude (parsed from 'latlong' column)
#   - longitude (parsed from 'latlong' column)

# So, by 11AM Monday, I'm looking for a .csv file named `hw4_netid1_netid2_....csv`
# containing your revised master data frame which has (1) previous columns
# cleaned in light of the HW3 solutions being shared and (2) the new geocoding
# columns above. You should also submit the .R file used for code clean-ups
# and the geocoding (call this `hw4_netid1_netid2_....R`). 

