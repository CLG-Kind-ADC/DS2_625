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
url <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch?form"

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


# phoebe's notes:
# take the modal solution because reasons
# pctgood, acres, exval, occupancy
modal_table=NULL
modalpctgood = NULL
for (i in 1:27307){
  for (j in 1:length(alldf)){
    modal_table[j] = alldf[[j]]$pctgood[i]}
 modalpctgood[i] = names(table(modal_table,useNA="always")[
    order(table(modal_table,useNA="always"),decreasing=T)])[1]
}

modal_table=NULL
modalacres = NULL
for (i in 1:27307){
  for (j in 1:length(alldf)){
    modal_table[j] = alldf[[j]]$acres[i]}
  modalacres[i] = names(table(modal_table,useNA="always")[
    order(table(modal_table,useNA="always"),decreasing=T)])[1]
}

modal_table=NULL
modalexval = NULL
for (i in 1:27307){
  for (j in 1:length(alldf)){
    modal_table[j] = alldf[[j]]$exval[i]}
  modalexval[i] = names(table(modal_table,useNA="always")[
    order(table(modal_table,useNA="always"),decreasing=T)])[1]
}

modal_table=NULL
modaloccupancy = NULL
for (i in 1:27307){
  for (j in 1:length(alldf)){
    modal_table[j] = alldf[[j]]$occupancy[i]}
  modaloccupancy[i] = names(table(modal_table,useNA="always")[
    order(table(modal_table,useNA="always"),decreasing=T)])[1]
}

modal_table=NULL
modallocation = NULL
for (i in 1:27307){
  for (j in 1:length(alldf)){
    modal_table[j] = alldf[[j]]$location[i]}
  modallocation[i] = names(table(modal_table,useNA="always")[
    order(table(modal_table,useNA="always"),decreasing=T)])[1]
}

# append ", NEW HAVEN, CT" to everything

#modallocation = paste(modallocation, ", NEW HAVEN, CT", sep="")
five_Variables = data.frame(modalpctgood,modalacres,modalexval,modaloccupancy,
                            modallocation)
names(five_Variables)=c("pctgood","acres","exval","occupancy","location")
write.csv(five_Variables,
          "/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/five_variables.csv",
          row.names = F)

geodata0 = data.frame(pid=1:27307,
                     location=modallocation,
                     city=rep("New Haven",times=27307),
                     state=rep("CT",times = 27307),
                     zip="")

geodata = data.frame(pid=1:27307,
                     location=modallocation,
                     city=rep("New Haven",times=27307),
                     state=rep("CT",times = 27307),
                     zip=""
                     )

dim(geodata)

for (i in 1:27){
  write.csv(geodata[1:1000,],paste("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4/parsing",
            i,".csv",sep=""),row.names=F)
  geodata=geodata[1001:dim(geodata)[1],]
}
write.csv(geodata[1:dim(geodata)[1],],paste(
  "/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4/parsing",
                                            "28",".csv",sep=""),row.names=F)

rm(geodata)

url <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch?form"

for (i in 1:28){
  f=paste("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4/parsing",i,".csv",sep="")
req <- POST(url, body=list(addressFile = upload_file(f),
                           benchmark = "Public_AR_Census2010",
                           vintage = "Census2010_Census2010"),
            encode = "multipart")
content(req, "text", encoding = "UTF-8")

# easier to write the output into a .csv file and then read it in
outfile <- tempfile(fileext = ".csv")
writeLines(content(req, "text", encoding = "UTF-8"), outfile)

v <- read.csv(outfile, header=FALSE, as.is=TRUE)
v=v[-which(v$V1=="pid"),]
v=v[order(as.numeric(v$V1)),]
write.csv(v,paste("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4/v",i,".csv",sep=""),
          row.names=F)
}

govdata=NULL
for ( i in 1:28){
  ph=read.csv(paste("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4/v",i,".csv",sep=""))
  govdata=rbind(govdata,ph)
}
