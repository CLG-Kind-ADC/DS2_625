#### Started by copying over Dylan's stuff
#### 10/11 In class plots script
require(ggplot2)
ourdf <- read.csv("/Volumes/ssg ssd t3/ssd_statgrad/ds2_625/hw8_phh26.csv", as.is=TRUE)
ourdf[ourdf$Sex=="?",] <- NA

ourdf[ourdf$Div == "M20-29" | ourdf$Div == "F20-29",]$Div

names.20.29 <- subset(ourdf, (Div=="M20-29" | Div=="F20-29") & 
                        (duplicated(ourdf$Name, fromLast=TRUE) & ! 
                           duplicated(ourdf$Name, fromLast=FALSE)))$Name

rep.20.29 <- subset(ourdf, Name %in% names.20.29)

names.60plus <- subset(ourdf, Age > 60 & 
                         (duplicated(ourdf$Name, fromLast=TRUE) & ! 
                            duplicated(ourdf$Name, fromLast=FALSE)))$Name
rep.60plus <- subset(ourdf, Name %in% names.60plus)

names.10.18 <- subset(ourdf, Age >= 10 & Age <= 18 &
                        (duplicated(ourdf$Name, fromLast=TRUE) & ! 
                           duplicated(ourdf$Name, fromLast=FALSE)))$Name
rep.10.18 <- subset(ourdf, Name %in% names.10.18)


ourdf[ourdf$Div=="OVRALM" | ourdf$Div=="OVRALF",]$Age
table(ourdf$Div)
ourdf[ourdf$Name=="Galen Rupp",]

#pdf(file="/Volumes/SSG SSD T3/ssd_statgrad/ds2_625/dpo22_phh26_ptc24_ks2252.pdf")  

(d0 <- ggplot(rep.20.29, aes(factor(Year), Nettime, colour=Sex)) + 
    geom_line(aes(group = Name)) + 
    geom_point() + ggtitle("20k, 20-29 Yr Olds", subtitle = "Among Repeat Runners") + 
    xlab("Year") + ylab("Net Time"))

(d1 <- ggplot(rep.60plus, aes(factor(Year), Nettime, colour=Sex)) + 
    geom_line(aes(group = Name)) + 
    geom_point() + ggtitle("20k, 60+ Yr Olds", subtitle = "Among Repeat Runners") + 
    xlab("Year") + ylab("Net Time"))

(d2 <- ggplot(rep.10.18, aes(factor(Year), Nettime, colour=Sex)) + 
    geom_line(aes(group = Name)) + 
    geom_point() + ggtitle("20k, 10-18 Yr Olds", subtitle = "Among Repeat Runners") + 
    xlab("Year") + ylab("Net Time"))

#dev.off()

###################################
###################################
#start phoebe's autonomous stuff

# repeat runners
allrepeatednames=ourdf[which(duplicated(ourdf$Name, fromLast=TRUE)),]$Name
# junked the NA(s?)
allrepeatednames = allrepeatednames[-which(is.na(allrepeatednames))]

# separate out by age at first run
allnon=NULL
# takes first year they ran; we will use their div from this year
for (i in 1:length(unique(allrepeatednames))){
  ph=ourdf[which(ourdf$Name == unique(allrepeatednames)[i]),]
  # in a few cases it looked like there was more than one entry in the earliest year...
  # I just chose the first one.
  ph=ph[which(ph$Year == min(ph$Year))[1],]
  allnon=rbind(allnon,ph)
}

#quick sanity checks
length(unique(allnon$Name))
dim(allnon) # matches the above: good.
levels(factor(allnon$Year)) # no 2017s -- good.
#

# add starting div cols and starting year cols to ourdf
ourdf$startyear=NULL
ourdf$startdiv=NULL
ourdf$startage = NULL
ourdf$repeatrunner=NULL
ourdf$firstnettime=NULL
for (i in 1:length(ourdf$Name)){
  if (ourdf$Name[i] %in% allnon$Name){
    ourdf$startyear[i]=allnon[which(allnon$Name == ourdf$Name[i]),]$Year
    ourdf$startdiv[i]=allnon[which(allnon$Name == ourdf$Name[i]),]$Div
    ourdf$startage[i]=allnon[which(allnon$Name == ourdf$Name[i]),]$Age
    ourdf$repeatrunner[i]="Y"
    ourdf$firstnettime[i] = allnon[which(allnon$Name == ourdf$Name[i]),]$Nettime
  } else {ourdf$startyear[i]=ourdf$Year[i]; ourdf$startdiv[i]=ourdf$Div[i];
  ourdf$startage[i] = ourdf$Age[i]; ourdf$repeatrunner[i]="N";ourdf$firstnettime[i]=ourdf$Nettime[i]}
}
ourdf[which(ourdf$startdiv=="" & (ourdf$startage==0 | is.na(ourdf$startage))),]$repeatrunner="N"

# creating repeats to scrape
reps=ourdf[ourdf$repeatrunner=="Y",]
length(which(is.na(reps$startdiv)))

#judge based on STARTING division...separate by sex?
f1319=reps[which(reps$startdiv=="F13-19"),]
f2029=reps[which(reps$startdiv=="F20-29"),]
f3039=reps[which(reps$startdiv=="F30-39"),]
f4044=reps[which(reps$startdiv=="F40-44"),]
f4549=reps[which(reps$startdiv=="F45-49"),]
f5054=reps[which(reps$startdiv=="F50-54"),]
f5559=reps[which(reps$startdiv=="F55-59"),]
f6064=reps[which(reps$startdiv=="F60-64"),]
f6569=reps[which(reps$startdiv=="F65-69"),]

m1319=reps[which(reps$startdiv=="M13-19"),]
m2029=reps[which(reps$startdiv=="M20-29"),]
m3039=reps[which(reps$startdiv=="M30-39"),]
m4044=reps[which(reps$startdiv=="M40-44"),]
m4549=reps[which(reps$startdiv=="M45-49"),]
m5054=reps[which(reps$startdiv=="M50-54"),]
m5559=reps[which(reps$startdiv=="M55-59"),]
m6064=reps[which(reps$startdiv=="M60-64"),]
m6569=reps[which(reps$startdiv=="M65-69"),]