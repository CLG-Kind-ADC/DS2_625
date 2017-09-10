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
}