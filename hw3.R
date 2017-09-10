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
    scrapeit1=placeholder2[[i]][grepl("<td>Style",placeholder2[[i]])]
    scrapeit1=sub(".*</td><td>", "", scrapeit1)
    scrapeit1 = sub("</td.*","",scrapeit1)
    thing$style[i]=scrapeit1
  } else {thing$style[i]=NA}
  
  ph2=grepl("<td>Model</td><td>",placeholder2[[i]])
  if (sum(ph2)==1){
    scrapeit2=placeholder2[[i]][grepl("<td>Model</td><td>",placeholder2[[i]])]
    scrapeit2=sub(".*</td><td>", "", scrapeit2)
    scrapeit2 = sub("</td.*","",scrapeit2)
    thing$model[i]=scrapeit2
  } else {thing$model[i]=NA}
  
  ph3=grepl("<td>Grade:</td><td>",placeholder2[[i]])
  if (sum(ph3)==1){
    scrapeit3=placeholder2[[i]][grepl("<td>Grade:</td><td>",placeholder2[[i]])]
    scrapeit3=sub(".*</td><td>", "", scrapeit3)
    scrapeit3 = sub("</td.*","",scrapeit3)
    thing$grade[i]=scrapeit3
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

}

# "AC Type:</td><td>"
# "Bath Style:</td><td>" "Kitchen Style:</td><td>" (call it kstyle)
# acres: "MainContent_lblLndAcres\">"
