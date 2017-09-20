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

