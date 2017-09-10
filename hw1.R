# HW 1
# worked with Jon Seyhun

#baseurl <- 'http://gis.vgsi.com/newhavenct/Parcel.aspx?pid=XXX'

these <- 1:1000     # Full set of queries (do not run!) 1:27309

theseries = these
placeholder=NULL

placeholder2=list()
for (i in theseries){
  x=try(scan(paste("/Volumes/SSG SSD T3/VisionAppraisal/newdata2017/",i,".html",sep=""),what="",sep="\n"))
  if (class(x) != "try-error"){
  placeholder[i]=i
  placeholder2[[i]]=x}
  else{
    placeholder[i]=i
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
View(thing)
write.csv(thing, row.names=FALSE, file = "/Volumes/SSG SSD T3/VisionAppraisal/hw1_ptc24.csv")
