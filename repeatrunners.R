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
###############################
f13192=data.frame()
for (i in 1:dim(f1319)[1]){
  if(f1319$Nettime[i]!=f1319$firstnettime[i]){
    f13192=rbind(f13192,f1319[i,])
  }
}
summary(lm(f13192$Nettime~f13192$firstnettime))
fteensum=summary(lm(f13192$Nettime~f13192$firstnettime))
plot(f13192$Nettime~f13192$firstnettime)
abline(fteensum$coeff[1],fteensum$coeff[2])
#############################
f20292=data.frame()
for (i in 1:dim(f2029)[1]){
  if(f2029$Nettime[i]!=f2029$firstnettime[i]){
    f20292=rbind(f20292,f2029[i,])
  }
}
#summary(lm(f20292$Nettime~f20292$firstnettime))
f20ssum=summary(lm(f20292$Nettime~f20292$firstnettime))
plot(f20292$Nettime~f20292$firstnettime)
abline(f20ssum$coeff[1],f20ssum$coeff[2])
#############################
f30392=data.frame()
for (i in 1:dim(f3039)[1]){
  if(f3039$Nettime[i]!=f3039$firstnettime[i]){
    f30392=rbind(f30392,f3039[i,])
  }
}
#summary(lm(f30392$Nettime~f30392$firstnettime))
f30ssum=summary(lm(f30392$Nettime~f30392$firstnettime))
plot(f30392$Nettime~f30392$firstnettime)
abline(f30ssum$coeff[1],f30ssum$coeff[2])
#############################
f40442=data.frame()
for (i in 1:dim(f4044)[1]){
  if(f4044$Nettime[i]!=f4044$firstnettime[i]){
    f40442=rbind(f40442,f4044[i,])
  }
}
#summary(lm(f40442$Nettime~f40442$firstnettime))
f40s1sum=summary(lm(f40442$Nettime~f40442$firstnettime))
plot(f40442$Nettime~f40442$firstnettime)
abline(f40s1sum$coeff[1],f40s1sum$coeff[2])
#############################
f45492=data.frame()
for (i in 1:dim(f4549)[1]){
  if(f4549$Nettime[i]!=f4549$firstnettime[i]){
    f45492=rbind(f45492,f4549[i,])
  }
}
#summary(lm(f45492$Nettime~f45492$firstnettime))
f40s2sum=summary(lm(f45492$Nettime~f45492$firstnettime))
plot(f45492$Nettime~f45492$firstnettime)
abline(f40s2sum$coeff[1],f40s2sum$coeff[2])
#############################
f50542=data.frame()
for (i in 1:dim(f5054)[1]){
  if(f5054$Nettime[i]!=f5054$firstnettime[i]){
    f50542=rbind(f50542,f5054[i,])
  }
}
#summary(lm(f50542$Nettime~f50542$firstnettime))
f50s1sum=summary(lm(f50542$Nettime~f50542$firstnettime))
plot(f50542$Nettime~f50542$firstnettime)
abline(f50s1sum$coeff[1],f50s1sum$coeff[2])
#############################
f55592=data.frame()
for (i in 1:dim(f5559)[1]){
  if(f5559$Nettime[i]!=f5559$firstnettime[i]){
    f55592=rbind(f55592,f5559[i,])
  }
}
#summary(lm(f55592$Nettime~f55592$firstnettime))
f50s2sum=summary(lm(f55592$Nettime~f55592$firstnettime))
plot(f55592$Nettime~f55592$firstnettime)
abline(f50s2sum$coeff[1],f50s2sum$coeff[2])
#############################
f60642=data.frame()
for (i in 1:dim(f6064)[1]){
  if(f6064$Nettime[i]!=f6064$firstnettime[i]){
    f60642=rbind(f60642,f6064[i,])
  }
}
#summary(lm(f60642$Nettime~f60642$firstnettime))
f60s1sum=summary(lm(f60642$Nettime~f60642$firstnettime))
plot(f60642$Nettime~f60642$firstnettime)
abline(f60s1sum$coeff[1],f60s1sum$coeff[2])
#############################
f65692=data.frame()
for (i in 1:dim(f6569)[1]){
  if(f6569$Nettime[i]!=f6569$firstnettime[i]){
    f65692=rbind(f65692,f6569[i,])
  }
}
#summary(lm(f65692$Nettime~f65692$firstnettime))
f60s2sum=summary(lm(f65692$Nettime~f65692$firstnettime))
plot(f65692$Nettime~f65692$firstnettime)
abline(f60s2sum$coeff[1],f60s2sum$coeff[2])

########################################
######### Males ########################
########################################
m13192=data.frame()
for (i in 1:dim(m1319)[1]){
  if(m1319$Nettime[i]!=m1319$firstnettime[i]){
    m13192=rbind(m13192,m1319[i,])
  }
}
summary(lm(m13192$Nettime~m13192$firstnettime))
mteensum=summary(lm(m13192$Nettime~m13192$firstnettime))
plot(m13192$Nettime~m13192$firstnettime)
abline(mteensum$coeff[1],mteensum$coeff[2])
#############################
m20292=data.frame()
for (i in 1:dim(m2029)[1]){
  if(m2029$Nettime[i]!=m2029$firstnettime[i]){
    m20292=rbind(m20292,m2029[i,])
  }
}
#summary(lm(m20292$Nettime~m20292$firstnettime))
m20ssum=summary(lm(m20292$Nettime~m20292$firstnettime))
plot(m20292$Nettime~m20292$firstnettime)
abline(m20ssum$coeff[1],m20ssum$coeff[2])
#############################
m30392=data.frame()
for (i in 1:dim(m3039)[1]){
  if(m3039$Nettime[i]!=m3039$firstnettime[i]){
    m30392=rbind(m30392,m3039[i,])
  }
}
#summary(lm(m30392$Nettime~m30392$firstnettime))
m30ssum=summary(lm(m30392$Nettime~m30392$firstnettime))
plot(m30392$Nettime~m30392$firstnettime)
abline(m30ssum$coeff[1],m30ssum$coeff[2])
#############################
m40442=data.frame()
for (i in 1:dim(m4044)[1]){
  if(m4044$Nettime[i]!=m4044$firstnettime[i]){
    m40442=rbind(m40442,m4044[i,])
  }
}
#summary(lm(m40442$Nettime~m40442$firstnettime))
m40s1sum=summary(lm(m40442$Nettime~m40442$firstnettime))
plot(m40442$Nettime~m40442$firstnettime)
abline(m40s1sum$coeff[1],m40s1sum$coeff[2])
#############################
m45492=data.frame()
for (i in 1:dim(m4549)[1]){
  if(m4549$Nettime[i]!=m4549$firstnettime[i]){
    m45492=rbind(m45492,m4549[i,])
  }
}
#summary(lm(m45492$Nettime~m45492$firstnettime))
m40s2sum=summary(lm(m45492$Nettime~m45492$firstnettime))
plot(m45492$Nettime~m45492$firstnettime)
abline(m40s2sum$coeff[1],m40s2sum$coeff[2])
#############################
m50542=data.frame()
for (i in 1:dim(m5054)[1]){
  if(m5054$Nettime[i]!=m5054$firstnettime[i]){
    m50542=rbind(m50542,m5054[i,])
  }
}
#summary(lm(m50542$Nettime~m50542$firstnettime))
m50s1sum=summary(lm(m50542$Nettime~m50542$firstnettime))
plot(m50542$Nettime~m50542$firstnettime)
abline(m50s1sum$coeff[1],m50s1sum$coeff[2])
#############################
m55592=data.frame()
for (i in 1:dim(m5559)[1]){
  if(m5559$Nettime[i]!=m5559$firstnettime[i]){
    m55592=rbind(m55592,m5559[i,])
  }
}
#summary(lm(m55592$Nettime~m55592$firstnettime))
m50s2sum=summary(lm(m55592$Nettime~m55592$firstnettime))
plot(m55592$Nettime~m55592$firstnettime)
abline(m50s2sum$coeff[1],m50s2sum$coeff[2])
#############################
m60642=data.frame()
for (i in 1:dim(m6064)[1]){
  if(m6064$Nettime[i]!=m6064$firstnettime[i]){
    m60642=rbind(m60642,m6064[i,])
  }
}
#summary(lm(m60642$Nettime~m60642$firstnettime))
m60s1sum=summary(lm(m60642$Nettime~m60642$firstnettime))
plot(m60642$Nettime~m60642$firstnettime)
abline(m60s1sum$coeff[1],m60s1sum$coeff[2])
#############################
m65692=data.frame()
for (i in 1:dim(m6569)[1]){
  if(m6569$Nettime[i]!=m6569$firstnettime[i]){
    m65692=rbind(m65692,m6569[i,])
  }
}
#summary(lm(m65692$Nettime~m65692$firstnettime))
m60s2sum=summary(lm(m65692$Nettime~m65692$firstnettime))
plot(m65692$Nettime~m65692$firstnettime)
abline(m60s2sum$coeff[1],m60s2sum$coeff[2])

fteensum; mteensum
f20ssum; m20ssum
f30ssum; m30ssum
f40s1sum; m40s1sum
f40s2sum; m40s2sum
f50s1sum; m50s1sum
f50s2sum; m50s2sum
f60s1sum; m60s1sum
f60s2sum; m60s2sum
