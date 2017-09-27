### Geocoding
# For today, let's not worry too much about the other columns in the dataset.
# We are going to focus exclusively on geocoding. 

# You probably saw that some of the addresses were not returning matching
# latitudes and longitudes. I bet there are still errors in the ones that
# did give a proper latitude/longitude too. Let's have a look.









# For today, see if you can clean up the latitudes and longitudes. Some ideas 
# for missing/wrong lat/longs: 
# 1. the 'ggmap' package has its own geocoding function, using Google Maps API
#    which is at least a different source
# 2. perhaps there are multiple streets with the same name? see if the owner 
#    address can provide a zipcode that you can add to the API calls.

# to use ggmap
library(ggmap)

# Examples:
#geocode("24 Hillhouse Ave, New Haven, CT")   # limit of 2500/day

housing_data = read.csv("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4_ptc24_ml2484_pt347.csv",
                        as.is=T)
View(housing_data)
names(housing_data)[names(housing_data)=="longitude"]="long"
names(housing_data)[names(housing_data)=="latitude"]="longitude"
names(housing_data)[names(housing_data)=="long"]="latitude"
write.csv(housing_data,
          "/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4_latlongswitch.csv",row.names=F)

housing_data=read.csv("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4_latlongswitch.csv",
                      as.is=T)
plot(latitude ~ longitude, data=housing_data)
high_latitude = housing_data[which(housing_data$latitude >= 41.347),]
dim(high_latitude)
points(latitude ~ longitude,data = high_latitude,col="red")


lower_corner = housing_data[which(housing_data$latitude <= 41.275 &
                                    housing_data$longitude <= -72.94),]
points(latitude~longitude,data=lower_corner,col="green")

View(high_latitude)

new_latlong_for_high_lat=NULL
for ( i in 1:dim(high_latitude)[1]){
  x=geocode(paste(high_latitude$location[i],", New Haven, CT",sep=""))
  new_latlong_for_high_lat[i]=paste(x[,2],",",x[,1],sep="")
}
nlll=strsplit(new_latlong_for_high_lat,",")

lat1=NULL
long1=NULL
for (i in 1:length(nlll)){
  lat1[i]=nlll[[i]][1]
  long1[i]=nlll[[i]][2]
}
lat1=as.numeric(lat1)
long1=as.numeric(long1)

identical(high_latitude$latitude,lat1)
which(high_latitude$latitude!=lat1)

points(lat1 ~ long1, col="purple")

######################
######################
lowercorner=NULL
for ( i in 1:dim(lower_corner)[1]){
  x=geocode(paste(lower_corner$location[i],", New Haven, CT",sep=""))
  lowercorner[i]=paste(x[,2],",",x[,1],sep="")
  Sys.sleep(2)
}
newlowercorner=strsplit(lowercorner,",")

lat2=NULL
long2=NULL
for (i in 1:length(newlowercorner)){
  lat2[i]=newlowercorner[[i]][1]
  long2[i]=newlowercorner[[i]][2]
}
lat2=as.numeric(lat2)
long2=as.numeric(long2)
points(x=long2,y=lat2,col="pink")

# replace the shit
lower_corner$latitude=lat2
lower_corner$longitude=long2
high_latitude$latitude=lat1
high_latitude$longitude=long1

# filter by PID to put them back in the housing_data dataframe

for ( i in 1:length(lower_corner$pid)){
  housing_data[which(housing_data$pid == lower_corner$pid[i]),]$latitude <- 
    lower_corner$latitude[i]
  housing_data[which(housing_data$pid == lower_corner$pid[i]),]$longitude <- 
    lower_corner$longitude[i]
}

for ( i in 1:length(high_latitude$pid)){
  housing_data[which(housing_data$pid == high_latitude$pid[i]),]$latitude <-
    high_latitude$latitude[i]
  housing_data[which(housing_data$pid == high_latitude$pid[i]),]$longitude <-
    high_latitude$longitude[i]
  
}
plot(housing_data$longitude,housing_data$latitude)
# adjust the bounds - we have a few "BY FAR" outliers that drag out the
# rest that are very, very close - so that's good actually.
plot(housing_data$longitude,housing_data$latitude,xlim = c(-73,-72.85),ylim=c(41.2,41.4))

onlyspring = read.csv("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/NewOnlySpring.csv",
                      as.is = T)
for ( i in 1:length(onlyspring$pid)){
  housing_data[which(housing_data$pid == onlyspring$pid[i]),]$latitude <-
    onlyspring$latitude[i]
  housing_data[which(housing_data$pid == onlyspring$pid[i]),]$longitude <-
    onlyspring$longitude[i]
  
}

hd2=read.csv("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4_latlongswitch.csv",as.is=T)
housing_data[6773:6778,]$latitude = hd2[6773:6778,]$latitude
housing_data[6773:6778,]$longitude = hd2[6773:6778,]$longitude
rm(hd2)

write.csv(housing_data,"/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/replaced.csv",row.names=F)

plot(housing_data$longitude,housing_data$latitude,xlim = c(-73,-72.85),ylim=c(41.23,41.37))

housing_data_test = housing_data[which(housing_data$latitude<41.37 &
                                     housing_data$latitude>41.23),]
housing_data_test = housing_data[which(housing_data$longitude< -72.85 &
                                    housing_data$latitude> -73),]

#rightside_new = read.csv("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/rightside_new.csv",
#                         as.is=T)

#for ( i in 1:length(rightside_new$pid)){
#  housing_data[which(housing_data$pid == rightside_new$pid[i]),]$latitude <-
#    rightside_new$latitude[i]
#  housing_data[which(housing_data$pid == rightside_new$pid[i]),]$longitude <-
#    rightside_new$longitude[i]
#  
#}