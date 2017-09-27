#######################################
# September 27
#######################################

# Modeling 'totval'

# Let's simplify the problem further by restricting our focus to only properties
# with 'zone' in the set ('RM1', 'RM2', 'RS1', 'RS2'). These are one-family 
# residential, one-family suburban residential, low-density apartment residential,
# and medium density apartment residential zones. We'll only examine properties
# with 'model' in the set ("Multi-Fam 2-4", "Multi-Fam 5-12", "Res Condo", 
# "Single Family"). 

# The goal of today is to build a model for predicting 'totval' as best as we can.

# Here's an example of a (bad) model:

nh <- read.csv("/Volumes/SSG SSD T3/ssd_statgrad/ds2_625/MasterData2.csv", as.is=TRUE)

# selection criteria
these <- nh$zone %in% c('RM1', 'RM2', 'RS1', 'RS2') & 
  nh$model %in% c("Multi-Fam 2-4", "Multi-Fam 5-12", "Res Condo", 
                  "Single Family")

nh2 <- nh[these,]

#latlongs <- read.csv("../Week04/HW4_submits/hw4_phh26_jt767_jcs277.csv", 
#                     as.is=TRUE)
latlongs <- nh2
latlongs <- latlongs[, c("latitude", "longitude")]

m1 <- lm(totval ~ bedrooms + bathrooms + halfbaths + yearbuilt, data = nh2)
summary(m1)$r.squared


# Now... a plot
plot(latlongs[,2], latlongs[,1], col=ifelse(resid(m1) > 0, "green", "red"),
     pch=16, cex=0.5)


## Now, go ahead and build your fancy model. In addition to checking diagnostics
# like R-squared, you should also periodically check a plot like the one above.

# At 1:35, we'll go around the room and do quick (3-5 min) presentations on 
# your model. By ready and plugged in on a display BY 1:30. 
# No slides are needed. Just pull up your code (plus a plot, if you think that
# would be helpful). It's not a lot of time, so focus your time on what model
# you used, with which variables, and some discussion of how well your model
# worked. 




### Due next Monday:
# Submit a 3-5 page report (done using .Rmd) as well as the final .csv file with
# latitudes/longitudes. The report should document any additional data cleaning
# that was done prior to modeling, describe the methods used, and summarize
# the results of your analysis. 
