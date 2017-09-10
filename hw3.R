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
  
  ph1=grepl("<td>Style",placeholder2[[i]])
  if (sum(ph1)==1){
    scrapeit=placeholder2[[i]][grepl("<td>Style",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$style[i]=scrapeit
  } else {thing$style[i]=NA}
  
  ph2=grepl("<td>Model</td><td>",placeholder2[[i]])
  if (sum(ph2)==1){
    scrapeit=placeholder2[[i]][grepl("<td>Model</td><td>",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$model[i]=scrapeit
  } else {thing$model[i]=NA}
  
  ph3=grepl("<td>Grade:</td><td>",placeholder2[[i]])
  if (sum(ph3)==1){
    scrapeit=placeholder2[[i]][grepl("<td>Grade:</td><td>",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$grade[i]=scrapeit
  } else {thing$grade[i]=NA}
  
  ph4=grepl("<td>Occupancy</td><td>",placeholder2[[i]])
  if (sum(ph4)==1){
    scrapeit=placeholder2[[i]][grepl("<td>Occupancy</td><td>",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$occupancy[i]=scrapeit
  } else {thing$occupancy[i]=NA}
  
  ph5=grepl("AC Type:</td><td>",placeholder2[[i]])
  if (sum(ph5)==1){
    scrapeit=placeholder2[[i]][grepl("AC Type:</td><td>",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$actype[i]=scrapeit
  } else {thing$actype[i]=NA}
  
  ph6=grepl("Bath Style:</td><td>",placeholder2[[i]])
  if (sum(ph6)==1){
    scrapeit=placeholder2[[i]][grepl("Bath Style:</td><td>",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$bathstyle[i]=scrapeit
  } else {thing$bathstyle[i]=NA}
  
  ph7=grepl("Kitchen Style:</td><td>",placeholder2[[i]])
  if (sum(ph7)==1){
    scrapeit=placeholder2[[i]][grepl("Kitchen Style:</td><td>",placeholder2[[i]])]
    scrapeit=sub(".*</td><td>", "", scrapeit)
    scrapeit = sub("</td.*","",scrapeit)
    thing$kstyle[i]=scrapeit
  } else {thing$kstyle[i]=NA}

}

# "Bath Style:</td><td>" "Kitchen Style:</td><td>" (call it kstyle)
# acres: "MainContent_lblLndAcres\">"
