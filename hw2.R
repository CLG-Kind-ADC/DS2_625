# hw2
#worked with : brandon chow, margaret luo, sara venkatraman, jon seyhun
theseries <- 1:27307     # Full set of queries (do not run!) 1:27309

placeholder=NULL

placeholder2=list()
for (i in theseries){
  x=try(scan(paste("/Volumes/SSG SSD T3/VisionAppraisal/newdata2017/",i,".html",sep=""),what="",sep="\n"))
  # change to this for my Windows pc
  # x=try(scan(paste("D://VisionAppraisal/newdata2017/",i,".html",sep=""),what="",sep="\n"))
  placeholder[i]=i
  if (class(x) != "try-error"){
    placeholder2[[i]]=x}
  else{
    placeholder2[[i]]="NA"
  }
  rm(x)
}

placeholder3=NULL
for (i in 1:length(placeholder2)){
  x=grepl("MainContent_ctl01_lblYearBuilt\\\">",placeholder2[[i]])
  if (sum(x)>=1){
    placeholder3[i]=placeholder2[[i]][grep("MainContent_ctl01_lblYearBuilt\\\">",placeholder2[[i]])]
  }
  else{placeholder3[i]=NA}
}
# quick check to make sure all lines, except those who have
# parcel id errors (5), have a YearBuilt
which(is.na(placeholder3))
# thumbsup


for (i in 1:length(placeholder3)){
  if(!is.na(placeholder3[i])){
    placeholder3[i]=sub(".*MainContent_ctl01_lblYearBuilt\\\">","",placeholder3[i])
    placeholder3[i]=sub("<.*","",placeholder3[i])}
  else{placeholder3[i]=NA}
}

placeholder4=NULL
for (i in 1:length(placeholder2)){
  x=grepl("MainContent_lblTab1Title\\\">",placeholder2[[i]])
  if (sum(x)>=1){
    placeholder4[i]=placeholder2[[i]][grep("MainContent_lblTab1Title\\\">",placeholder2[[i]])]
  }
  else{placeholder4[i]=NA}
}
which(is.na(placeholder4))

for (i in 1:length(placeholder4)){
  if (!is.na(placeholder4[i])){
    placeholder4[i]=sub(".*MainContent_lblTab1Title\\\">","",placeholder4[i])
    placeholder4[i]=sub("</span.*","",placeholder4[i])}
  else{placeholder4[i]=NA}
}

thing=data.frame(placeholder,placeholder4,placeholder3)
names(thing)=c("pid","location","yearbuilt")
thing$yearbuilt[thing$yearbuilt==""]<-NA
thing$yearbuilt=as.character(thing$yearbuilt)

bill=NULL
for (i in 1:length(placeholder2)){
  ee=grepl("MainContent_ctl0[1234567890]+_lblYearBuilt",placeholder2[[i]])
  if (sum(ee)==1) {
    bill[i]="YES"
  } else if (sum(ee)>1){bill[i]=as.character(sum(ee))}
  else {bill[i]="NO"}
}
multiyearbuilt=which(bill!="YES" & bill!="NO")

multiyblist=list()
for(i in 1:length(multiyearbuilt)){
  eli=placeholder2[[multiyearbuilt[i]]][grep(
    "_lblYearBuilt",placeholder2[[multiyearbuilt[i]]])]
  eli=sub(".*YearBuilt\\\">","",eli)
  eli=sub("</span>.*","",eli)
  eli = min(as.numeric(eli),na.rm = TRUE)
  
  multiyblist[[i]]=c(as.character(multiyearbuilt[i]),as.character(eli))
  rm(eli)
}

zha=thing$yearbuilt

for ( i in 1:length(multiyblist)){
  thing$yearbuilt[thing$pid==as.numeric(multiyblist[[i]][1])]=multiyblist[[i]][2]
}

View(thing)
#write.csv(thing, row.names=FALSE, file = "/Volumes/SSG SSD T3/VisionAppraisal/hw1_ptc24.csv")

# for hw2, now want totval (2016 total appraisal value)
# bedrooms
# bathrooms
# half bathrooms (halfbaths)

for (i in 1:length(placeholder2)){
  if (sum(grepl("MainContent_lblGenAppraisal\"",placeholder2[[i]]))==1){
    pwd=placeholder2[[i]][grep("MainContent_lblGenAppraisal\"",placeholder2[[i]])]
    pwd=sub(".*\\$","",pwd); pwd=sub("</span.*","",pwd); pwd=gsub(",","",pwd)
    thing$totval[i]=as.numeric(pwd)
    rm(pwd)
    # clean and scrape thing$totval
  }
}

#bedrooms
p=NULL
for (i in 1:length(placeholder2)){
  q=grepl("Total Bedrms|Total Bedrooms|Ttl Bedrms",placeholder2[[i]])
  if (sum(q)==1) {
    p[i]="YES"
  } else if (sum(q)>=1){p[i]=as.character(sum(q))}
  else {p[i]="NO"}
}

# start with the one "Field/Description"'s, like i did for the baths
onebed=(which(p=="YES"))
nonexist=which(p=="NO")
multibed=which(p!="YES" & p!="NO")
for (i in onebed){
  ae=placeholder2[[i]][grep("Total Bedrms|Total Bedrooms|Ttl Bedrms",placeholder2[[i]])]
  ae=sub(".*</td><td>","",ae)
  thing$bedrooms[i]=ae
  rm(ae)
}
table(thing$bedrooms)

thing$bedrooms=sub("</td>","",thing$bedrooms)
thing$bedrooms[grep("&nbsp;",thing$bedrooms)]=NA
thing$bedrooms[thing$bedrooms==""] <- NA
thing$bedrooms=sub("Bedrooms*","",thing$bedrooms)
for (i in 1:length(thing$bedrooms)){
  if(!grepl("[1234567890]",thing$bedrooms[i])){
    thing$bedrooms[i]=NA
  } else next
}
thing$bedrooms=sub(" ","",thing$bedrooms)
thing$bedrooms=sub("0+[^123456789]","0",thing$bedrooms)
for(i in 1:length(thing$bedrooms)){
  if (grepl("0[123456789]",thing$bedrooms[i])){
    thing$bedrooms[i]=sub("0","",thing$bedrooms[i])
  }
}

# need to handle the multi-listed differently...
# may also move some of the regex/data cleaning after this part.
multibedlist=list()
for(i in 1:length(multibed)){
  eli=placeholder2[[multibed[i]]][grep(
    "Total Bedrms|Total Bedrooms|Ttl Bedrms",placeholder2[[multibed[i]]])]
  eli=sub(".*</td><td>","",eli)
  eli=sub("</td>$","",eli)
  eli=sub("Bedrooms*","",eli)
  eli[eli=="&nbsp;"]=NA
  
  for (j in 1:length(eli)){
    if(!grepl("[1234567890]",eli[j])){
      eli[j]=NA
    } else next
  }
  eli=sub(" ","",eli)
  eli=sub("0+[^123456789]","0",eli)
  for(h in 1:length(eli)){
    if (grepl("0[123456789]",eli[h])){
      eli[h]=sub("0","",eli[h])
    }
  }
  
  # original code: just counted everything as NA if even one NA
  # after discussion w/peers, I changed it to sum everything
  #  if (sum(is.na(eli))>=1){
  #   eli = NA
  #  } else if (sum(grepl("9\\+",eli))>=1){
  #    eli = "9+"
  #  } else {eli = sum(as.numeric(eli))}
  
  if (sum(grepl("9\\+",eli))>=1){
    eli = "9+"
  } else {eli = sum(as.numeric(eli),na.rm=TRUE)}
  
  multibedlist[[i]]=c(multibed[i],eli)
  rm(eli)
}
# if there's even one NA, consider it an unknown and make everything NA. Else, add...
# then there's the 9+ case DO THE SAME for 9+ - because 9+ and (1+2+3+9)+ is still 9+
# ???
zzz=thing$bedrooms
# zzz for the quick check
for ( i in 1:length(multibedlist)){
  thing$bedrooms[thing$pid==as.numeric(multibedlist[[i]][1])]=multibedlist[[i]][2]
}
# thing$bedroomcheck = zzz; View(thing[multibed,]);
# if the above code shows differences between the two, you're golden

#########################
#########################
#########################

# basically repeat for baths
#checking for the tag of baths
x=NULL
for (i in 1:length(placeholder2)){
  y=grepl("Total Bthrms|Total Baths|Ttl Bathrms",placeholder2[[i]])
  if (sum(y)==1) {
    x[i]="YES"
  } else if (sum(y)>1){x[i]=as.character(sum(y))}
  else {x[i]="NO"}
}
identical(which(p!="YES" & p!="NO"),which(x!="YES" & x!="NO"))
#because I see that with the bathrooms there are 577 ">1"'s as well,
# i assume there are 577 with at least a double "Field/Description" chart.
# how fascinating


length(which(x=="YES")) # which(x=="YES")
onebathroom=which(x=="YES") # which(x=="YES")
# this is easiest for me

length(which(x!="YES" & x!="NO"))
multiplepartsbath=which(x!="YES" & x!="NO")
# this implies multiple parts of a building, well fuck

length(which(x=="NO")) # which(x=="NO")
# for these pid's i'll set the data frame cell to NA
nonexist=which(x=="NO")
# the length of x=="NO" is 827 which is the number of files we don't have.
# so that works out, and it's a good check

# like with bedrooms, we'll start out with the ones that exist

for (i in onebathroom){
  ae=placeholder2[[i]][grep("Total Bthrms|Total Baths|Ttl Bathrms",placeholder2[[i]])]
  ae=sub(".*</td><td>","",ae)
  thing$bathrooms[i]=ae
  rm(ae)
}
table(thing$bathrooms)

thing$bathrooms=sub("</td>","",thing$bathrooms)
thing$bathrooms[grep("&nbsp;",thing$bathrooms)]=NA
thing$bathrooms[thing$bathrooms==""] <- NA
for (i in 1:length(thing$bathrooms)){
  if(!grepl("[1234567890]",thing$bathrooms[i])){
    thing$bathrooms[i]=NA
  } else next
}
thing$bathrooms=sub("0+[^123456789]","0",thing$bathrooms)
for(i in 1:length(thing$bathrooms)){
  if (grepl("0[123456789]",thing$bathrooms[i])){
    thing$bathrooms[i]=sub("0","",thing$bathrooms[i])
  }
}
thing$bathrooms[which(thing$bathrooms=="1/2" | thing$bathrooms=="1 Half" | thing$bathrooms==".5")]=".5"
thing$bathrooms=sub(" Full$","",thing$bathrooms)

###################
###################
#thing$bathrooms=sub(" ","",thing$bathrooms)

# now move onto the multiple Field/Description charts
multibathlist=list()
for(i in 1:length(multiplepartsbath)){
  eli=placeholder2[[multiplepartsbath[i]]][grep(
    "Total Bthrms|Total Baths|Ttl Bathrms",placeholder2[[multiplepartsbath[i]]])]
  eli=sub(".*</td><td>","",eli)
  eli=sub("</td>$","",eli)
  eli=sub("[Bb]bathrooms*","",eli)
  eli[eli=="&nbsp;"]=NA
  
  
  for (j in 1:length(eli)){
    if(!grepl("[1234567890]",eli[j])){
      eli[j]=NA
    } else next
  }
  eli=sub(" ","",eli)
  eli=sub("0+[^123456789]","0",eli)
  for(h in 1:length(eli)){
    if (grepl("0[123456789]",eli[h])){
      eli[h]=sub("0","",eli[h])
    }
  }
  
  if (sum(grepl("9\\+",eli))>=1){
    eli = "9+"
  } else {eli = sum(as.numeric(eli),na.rm = TRUE)}
  
  multibathlist[[i]]=c(multiplepartsbath[i],eli)
  rm(eli)
}

zh=thing$bathrooms

for ( i in 1:length(multibathlist)){
  thing$bathrooms[thing$pid==as.numeric(multibathlist[[i]][1])]=multibathlist[[i]][2]
}
thing$bathrooms[which(thing$bathrooms==".5")]="0.5"
# thing$bathroomcheck = zh; View(thing[multiplepartsbath,]);
# if the above code shows differences between the two
# (namely, that `bathrooms` has fewer NAs than `bathroomcheck`), you're golden

# take note of these bathrooms with half bathrooms in them
thing[grep("\\.5",thing$bathrooms),]

# Look into half bathrooms now

d=NULL
for (i in 1:length(placeholder2)){
  naught=grepl("Total [Hh]alf Baths|Ttl Half Bths|lf [Bb]a?thro*ms",placeholder2[[i]])
  if (sum(naught)==1){
    d[i]="YES"
  } else if (sum(naught)>1){d[i]=as.character(sum(naught))}
  else {d[i]="NO"}
  
}
# OK - that's every combination I can think of right now.
# if that doesn't cover everything, I did my best...

onehalfbath=which(d=="YES")
nohalfbaths=which(d=="NO")
multihalf=which(d!="YES" & d!="NO")

# let's deal with those pesky halfbaths from earlier
halfbathpid=thing[grepl("\\.5",thing$bathrooms),]$pid
which(halfbathpid %in% onehalfbath) # 2 7
which(halfbathpid %in% multihalf) #none
which(halfbathpid %in% nohalfbaths) # everything else 1-11
# we'll add them back in once I get the code filled out.

for (i in onehalfbath){
  shae=placeholder2[[i]][grep("Total [Hh]alf Baths|Ttl Half Bths|lf [Bb]a?thro*ms",placeholder2[[i]])]
  shae=sub(".*</td><td>","",shae)
  thing$halfbaths[i]=shae
  rm(shae)
}
table(thing$halfbaths)

thing$halfbaths=sub("</td>","",thing$halfbaths)
thing$halfbaths[grep("&nbsp;",thing$halfbaths)]=NA
thing$halfbaths[thing$halfbaths==""] <- NA
for (i in 1:length(thing$halfbaths)){
  if(!grepl("[1234567890]",thing$halfbaths[i])){
    thing$halfbaths[i]=NA
  } else next
}


###################
###################
#thing$halfbaths=sub(" ","",thing$halfbaths)

# now move onto the multiple Field/Description charts
multihalfbathlist=list()
for(i in 1:length(multihalf)){
  eli=placeholder2[[multihalf[i]]][grep(
    "Total [Hh]alf Baths|Ttl Half Bths|lf [Bb]a?thro*ms",placeholder2[[multihalf[i]]])]
  eli=sub(".*</td><td>","",eli)
  eli=sub("</td>$","",eli)
  eli=sub("[Bb]bathrooms*","",eli)
  eli[eli=="&nbsp;"]=NA
  
  
  for (j in 1:length(eli)){
    if(!grepl("[1234567890]",eli[j])){
      eli[j]=NA
    } else next
  }
  eli=sub(" ","",eli)
  eli=sub("0+[^123456789]","0",eli)
  for(h in 1:length(eli)){
    if (grepl("0[123456789]",eli[h])){
      eli[h]=sub("0","",eli[h])
    }
  }
  
  if (sum(grepl("9\\+",eli))>=1){
    eli = "9+"
  } else {eli = sum(as.numeric(eli),na.rm=TRUE)}
  
  multihalfbathlist[[i]]=c(multihalf[i],eli)
  rm(eli)
}

zho=thing$halfbaths

for ( i in 1:length(multihalfbathlist)){
  thing$halfbaths[thing$pid==as.numeric(multihalfbathlist[[i]][1])]=multihalfbathlist[[i]][2]
}

# and now to deal with the "0.5" baths

for (i in halfbathpid){
  thing$bathrooms[thing$pid==i]=sub("\\.5","",thing$bathrooms[thing$pid==i])
  if(!is.na(thing$halfbaths[thing$pid==i])){
    thing$halfbaths[thing$pid==i]=as.numeric(thing$halfbaths[thing$pid==i])+1}
  else {thing$halfbaths[thing$pid==i]=1}
}
#quick check
thing[halfbathpid,]

# quick edit to the yearbuilt 0
thing$yearbuilt[thing$yearbuilt==0 | thing$yearbuilt=="0"]=NA
write.csv(file="/Volumes/SSG SSD T3/VisionAppraisal/hw2_ptc24.csv",x=thing,row.names=F)
