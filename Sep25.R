#######################################
# September 25
#######################################

# Today, we're going to hopefully make the transition from data cleaning
# to data analysis. Start off by finalizing your latitudes and longitudes.
# When you are done, make a scatterplot of latitudes/longitudes (hopefully
# with suspicious values removed), and include "n=XXXXX" as the title of your
# plot to prominently display how many reliable latitudes/longitudes you are
# displaying. Project this onto a TV monitor. 

# Feel free to walk around and inspect another group's plot -- if they got
# a greater number of valid lat/longs than you did, maybe it's worth asking
# why and how. If your plot looks drastically different from someone else's,
# that's worth figuring out too. 


##### Towards modeling:
# The goal of this week is to get a model for `totval` that you trust.
# One student brought up a good point of excluding variables that would make
# this a bit too easy for us. To that end, please exclude the following variables:
# 1. `exval`
# 2. `landval`
# 3. `replcost`

# Clearly, you'll want to make use of the spatial data in some way. The
# spatial information is currently stored in 2 ways:

# for example:
latlongs <- read.csv("../../../STAT625/Plans/Week04/HW4_submits/chowbrandon_61063_1481062_hw4_zl392_btc32_tb586_dpo22.csv", as.is=TRUE)
latlongs <- latlongs[,c("latitude", "longitude")]
x <- read.csv("../../../STAT625/Plans/Week04/finaldf.csv", as.is=TRUE)

table(x$neighborhood)

# You'll want to brainstorm how you might use either one or both of these 
# location-based variables. 

# One possible avenue of exploration
# http://ctdatahaven.org/data-resources/nhcity-neighborhood-profiles-2014-5y

# If you want to use these variables, you'll need to match the neighborhoods
tail(sort(table(x$neighborhood)), 20)


plot(latlongs[,1] ~ latlongs[,2])
v <- which(x$neighborhood == "1600")

points(latlongs[v,1] ~ latlongs[v, 2], col="red")
