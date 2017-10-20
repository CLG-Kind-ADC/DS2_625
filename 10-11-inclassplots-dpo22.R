#### 10/11 In class plots script
require(ggplot2)
our_df <- read.csv("/Volumes/ssg ssd t3/ssd_statgrad/ds2_625/hw8_phh26.csv", as.is=TRUE)
our_df[our_df$Sex=="?",] <- NA

our_df[our_df$Div == "M20-29" | our_df$Div == "F20-29",]$Div

names.20.29 <- subset(our_df, (Div=="M20-29" | Div=="F20-29") & 
                        (duplicated(our_df$Name, fromLast=TRUE) & ! 
                           duplicated(our_df$Name, fromLast=FALSE)))$Name

rep.20.29 <- subset(our_df, Name %in% names.20.29)

names.60plus <- subset(our_df, Age > 60 & 
                         (duplicated(our_df$Name, fromLast=TRUE) & ! 
                            duplicated(our_df$Name, fromLast=FALSE)))$Name
rep.60plus <- subset(our_df, Name %in% names.60plus)

names.10.18 <- subset(our_df, Age >= 10 & Age <= 18 &
                        (duplicated(our_df$Name, fromLast=TRUE) & ! 
                           duplicated(our_df$Name, fromLast=FALSE)))$Name
rep.10.18 <- subset(our_df, Name %in% names.10.18)


our_df[our_df$Div=="OVRALM" | our_df$Div=="OVRALF",]$Age
table(our_df$Div)
our_df[our_df$Name=="Galen Rupp",]

pdf(file="dpo22_phh26_ptc24_ks2252.pdf")  

(d <- ggplot(rep.20.29, aes(factor(Year), Nettime, colour=Sex)) + 
    geom_line(aes(group = Name)) + 
    geom_point() + ggtitle("20k, 20-29 Yr Olds", subtitle = "Among Repeat Runners") + 
    xlab("Year") + ylab("Net Time"))

(d <- ggplot(rep.60plus, aes(factor(Year), Nettime, colour=Sex)) + 
    geom_line(aes(group = Name)) + 
    geom_point() + ggtitle("20k, 60+ Yr Olds", subtitle = "Among Repeat Runners") + 
    xlab("Year") + ylab("Net Time"))

(d <- ggplot(rep.10.18, aes(factor(Year), Nettime, colour=Sex)) + 
    geom_line(aes(group = Name)) + 
    geom_point() + ggtitle("20k, 10-18 Yr Olds", subtitle = "Among Repeat Runners") + 
    xlab("Year") + ylab("Net Time"))

dev.off()