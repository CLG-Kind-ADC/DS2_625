### Geocoding
# For today, let's not worry too much about the other columns in the dataset.
# We are going to focus exclusively on geocoding. 

# You probably saw that some of the addresses were not returning matching
# latitudes and longitudes. I bet there are still errors in the ones that
# did give a proper latitude/longitude too. Let's have a look.


x <- read.csv("HW4_submits/qiyan_58719_1481854_hw4_yq76_yj264_jl2966-1.csv", 
              as.is = TRUE)

names(x)
head(x[,42:45])

table(x$match1)
table(x$match2)

# Dylan mentioned that longs and lats are swapped
plot(longitude ~ latitude, data=x)

# For today, see if you can clean up the latitudes and longitudes. Some ideas 
# for missing/wrong lat/longs: 
# 1. the 'ggmap' package has its own geocoding function, using Google Maps API
#    which is at least a different source
# 2. perhaps there are multiple streets with the same name? see if the owner 
#    address can provide a zipcode that you can add to the API calls.

# to use ggmap
library(ggmap)

# Examples:
geocode("24 Hillhouse Ave, New Haven, CT")   # limit of 2500/day


## For Wednesday: Use ggmap to do geocoding for the latitude/longitude.
## Each person will scrape a maximum of 1051 addresses via ggmap

netids <- readLines("/Volumes/SSG SSD T3/ssd_statgrad/ds2_625/netids.txt")
your_net_id <- "ptc24" # update this
w <- which(netids == your_net_id)

start <- (w-1)*1051+1
end <- min(start+1050, 27307)
my_pids <- start:end

## by Wednesday 11AM, submit 3 columns:
# * pid
# * latitude
# * longitude

housing_data=read.csv("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4_latlongswitch.csv")
View(housing_data)
housing_data$location = as.character(housing_data$location)
head(my_pids)

llgeocode=NULL
for ( i in 1:length(my_pids)){
  x=geocode(paste(housing_data[which(housing_data$pid==my_pids[i]),]$location,", New Haven, CT",
                     sep=""))
  llgeocode[i]=paste(x[,2],",",x[,1],sep="")
}
nlll=strsplit(llgeocode,",")

lat1=NULL
long1=NULL
for (i in 1:length(nlll)){
  lat1[i]=nlll[[i]][1]
  long1[i]=nlll[[i]][2]
}
lat1=as.numeric(lat1)
long1=as.numeric(long1)

which(is.na(lat1))

my_pids2 = data.frame(my_pids, lat1, long1)
write.csv(my_pids2, "/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw5.csv",row.names=F)
