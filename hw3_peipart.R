

zone <- rep(NA, 27307)
neighborhood <- rep(NA, 27307)
landval <- rep(NA, 27307)
garage <- rep(NA, 27307)

for (ii in 1:27307){
  file <- paste(paste("newdata2017/",as.character(ii),sep = ""),".html",sep = "")
  
  if (file.exists(file)){
    x <- scan(file, what="", sep="\n")
    
    # zone
    zoneline <- grep("MainContent_lblZone", x)
    z <- x[zoneline]
    z <- gsub("<[^<>]*>", "", z)
    z <- gsub("\t", "", z)
    z <- gsub("Zone", "", z)
    zone[ii] <- z
    
    # neighborhood
    neighborhoodline <- grep("MainContent_lblNbhd", x)
    nh <- x[neighborhoodline]
    nh <- gsub("<[^<>]*>", "", nh)
    nh <- gsub("\t", "", nh)
    nh <- gsub("Neighborhood", "", nh)
    if (nh != ""){
      neighborhood[ii] <- nh
    }
    
    # landval
    valueline <- grep("MainContent_lblLndAppr", x)
    if (length(valueline) != 0){
      value <- x[valueline]
      value <- gsub("<[^<>]*>", "", value)
      value <- gsub("[^0-9]", "", value)
      landval[ii] <- as.integer(value)
    }
    
    # garagesqft
    garagesqft <- 0
    garageline1 <- grep("Garage", x)
    if (length(garageline1) != 0){
      garage1 <- x[garageline1 + 1]
      garage1 <- gsub("[^0-9]+", "", garage1)
      garagesqft <- sum(as.integer(garage1), na.rm = TRUE)
    }
    garageline2 <- grep("GARAGE", x)
    if (length(garageline2) != 0){
      garage2 <- x[garageline2]
      garage2 <- gsub("<[^<>]*>", "", garage2)
      garage2 <- gsub(".*;([0-9]+) S.F..*", "\\1", garage2)
      garagesqft <- garagesqft + sum(as.integer(garage2), na.rm = TRUE)
    }
    if (garagesqft != 0){
      garage[ii] <- garagesqft
    }
    
  }
}
