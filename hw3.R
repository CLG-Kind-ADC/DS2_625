# hw3
source("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw2.R")

# i should really make this a 
for (i in 1:length(thing$pid)){
  ph = grepl("lblPctGood",placeholder2[[i]])
  if (sum(ph) == 1){
    scrapeit=placeholder2[[i]][grepl("lblPctGood",placeholder2[[i]])]
    scrapeit=sub(".*lblPctGood\">", "", scrapeit)
    scrapeit = sub("</span>.*","",scrapeit)
    scrapeit=as.numeric(scrapeit)
    thing$pctgood[i]=scrapeit
  }
    else {thing$pctgood[i]=NA}
  
  ph=grepl("<td>Style",placeholder2[[i]])
  if (sum(ph)==1){
    scrapeit=placeholder2[[i]][grepl("<td>Style",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$style[i]=scrapeit
  } else {thing$style[i]=NA}
  
  ph=grepl("<td>Model</td><td>",placeholder2[[i]])
  if (sum(ph)==1){
    scrapeit=placeholder2[[i]][grepl("<td>Model</td><td>",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$model[i]=scrapeit
  } else {thing$model[i]=NA}
  
  ph=grepl("<td>Grade:</td><td>",placeholder2[[i]])
  if (sum(ph)==1){
    scrapeit=placeholder2[[i]][grepl("<td>Grade:</td><td>",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$grade[i]=scrapeit
  } else {thing$grade[i]=NA}
  
  ph=grepl("<td>Occupancy</td><td>",placeholder2[[i]])
  if (sum(ph)==1){
    scrapeit=placeholder2[[i]][grepl("<td>Occupancy</td><td>",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$occupancy[i]=scrapeit
  } else {thing$occupancy[i]=NA}
  
  ph=grepl("AC Type:</td><td>",placeholder2[[i]])
  if (sum(ph)==1){
    scrapeit=placeholder2[[i]][grepl("AC Type:</td><td>",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$actype[i]=scrapeit
  } else {thing$actype[i]=NA}
  
  ph=grepl("Bath Style:</td><td>",placeholder2[[i]])
  if (sum(ph)==1){
    scrapeit=placeholder2[[i]][grepl("Bath Style:</td><td>",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$bathstyle[i]=scrapeit
  } else {thing$bathstyle[i]=NA}
  
  ph=grepl("Kitchen Style:</td><td>",placeholder2[[i]])
  if (sum(ph)==1){
    scrapeit=placeholder2[[i]][grepl("Kitchen Style:</td><td>",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$kstyle[i]=scrapeit
  } else {thing$kstyle[i]=NA}
  
  ph = grepl("MainContent_lblLndAcres\">",placeholder2[[i]])
  if (sum(ph)==1){
    scrapeit=placeholder2[[i]][grepl("MainContent_lblLndAcres\">",placeholder2[[i]])]
    scrapeit=sub(".*Acres\">", "", scrapeit)
    scrapeit = sub("</span.*","",scrapeit)
    thing$acres[i]=as.numeric(scrapeit)
  } else {thing$acres[i]=NA}
  
}

for (i in 1:dim(thing)[2]){
  if (sum(grepl("&nbsp;",thing[,i]))>=1){
    thing[,i][grep("&nbsp;",thing[,i])]=NA
  }
  if (sum(grepl("  .*",thing[,i]))>=1){
    thing[,i][grep("  .*",thing[,i])]=NA
  }
}

for (i in 1:length(placeholder2)){
  ph = grepl("Extra Features$",placeholder2[[i]])
  if (sum(ph)==1){
    scrapeit=placeholder2[[i]][(grepl("Extra Features$",placeholder2[[i]])+4)]
    if (sum(grepl("\\$",scrapeit))==1){
      scrapeit=sub(".*\\$", "", scrapeit)
      scrapeit = sub(",","",scrapeit)
      scrapeit = sub("</td>.*","",scrapeit)
      thing$exval[i]=as.numeric(scrapeit)
    } else {thing$exval[i]="No Data for Extra Features"}
  } else {thing$exval[i]=NA}
}

# "Bath Style:</td><td>" "Kitchen Style:</td><td>" (call it kstyle)
# acres: "MainContent_lblLndAcres\">"
