#############################
# September 20, 2017
#

# Agenda
# 1. Sale history scrape demo

# 2. Geocoding - still some work to do.

# 3. Your HW 4 - Some of you have got it, and others still have quite a bit to
#    do. Use the remainder of class time to submit improved versions of your
#    HW 4 csv.

# 4. Analysis: 
#    Work towards building a model that predicts `totval`. 
#    Nothing due on Monday, but be prepared to do a 5 minute presentation 
#    on Wednesday and submit a mini-report (3 pages max) using .Rmd by the
#    following Monday.

#  



###################
# First thing: sale history scrape
#
#
maxID <- 3
z <- as.data.frame(matrix(NA, maxID, 15))
names(z) <- paste(rep(c("saleowner", "saleprice", "saledate"), 5), 
                  rep(1:5, each=3), sep="")


for (i in maxID) {
  
  x <- readLines(paste0("samplefiles/", i, ".html"))
  
  m <- grep("Ownership History", x)[1]
  startEnd <- m+grep("<(/)*table", x[m:length(x)])[1:2]-1
  tableRows <- x[startEnd[1]:startEnd[2]]
  tableRows <- tableRows[grep("$", tableRows, fixed=TRUE)]
  tableRows <- strsplit(tableRows, "</td>")
  numCols <- length(tableRows[[1]])
  numOwners <- length(tableRows) # we're only tracking the latest 5, max
  tableRows <- gsub("<[^<>]*>|\t|\\$|,", "", unlist(tableRows))
  histdata <- matrix(tableRows, ncol=numCols, byrow=TRUE)
  histdata <- histdata[1:min(5, numOwners), c(1:2, numCols), drop=FALSE]
  
  z[i, 1:length(histdata)] <- as.vector(t(histdata))
}

######################
# Geocoding submissions
# 

folder <- "geocode_submits/"
files <- dir(folder, pattern=".csv")

geocode_ggmap <- data.frame(pid = 1:27307, 
                            latitude = NA,
                            longitude = NA)


for (i in 1:length(files)) {
  student <- gsub("_.*.csv", "", files[i])
  vals <- read.csv(paste0(folder, "/", files[i]), as.is=TRUE)
  cat(student, ": ", sum(!is.na(vals$lat)), " lats & ",
      sum(!is.na(vals$long)), " longs given \n")
  # if(student == "pringlecameron" | student == "jiangyuwei") browser()
  # if (!is.na(geocode_ggmap$lat[vals$pid[1]])) browser()
  geocode_ggmap[vals$pid, 2:3] <- cbind(vals$lat, vals$long)
}

head(geocode_ggmap)
w <- which(is.na(geocode_ggmap[,2]))
length(w)


## Susan will fill these in:
library(ggmap)
addresses <- read.csv("../Week03/final_locations.csv", as.is=TRUE)
head(addresses)

sum(is.na(addresses$location))

myaddresses <- data.frame(pid = w, latitude = NA, longitude = NA)
pb <- txtProgressBar(style=3)
for (i in 101:length(w)) { # iterating over the missing lat/longs
  if (!is.na(addresses$location[w[i]])) {
    v <- geocode(paste0(addresses$location[w[i]], ", New Haven, CT"))
    myaddresses$latitude[i] <- v$lat
    myaddresses$longitude[i] <- v$lon
    Sys.sleep(1) #sleep timer of a second between queries
  }
  setTxtProgressBar(pb, i/length(w))
}


o <- match(myaddresses$pid, geocode_ggmap$pid)
geocode_ggmap[o,2:3] <- myaddresses[,2:3]
sum(is.na(myaddresses$latitude))

write.csv(geocode_ggmap, "ggmap_latlongs.csv", row.names=FALSE)
