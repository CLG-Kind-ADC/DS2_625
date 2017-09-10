# sink("hw1_csv_comparison_results.txt")

folder <- "hw2_submits"
files <- dir(folder, pattern=".csv")


alldf <- list()
for (i in 1:length(files)) {
  netid <- gsub(".*_hw2_(.*).csv", "\\1", files[i])
  netid <- gsub("(.*)-[0-9]", "\\1", netid)
  alldf[[netid]] <- read.csv(paste0(folder, "/", files[i]), as.is=TRUE)
}

cat("Dimension/colname check: \n")
scraped_cols <- c("pid", "location", "yearbuilt",
                  "totval", "bedrooms", "bathrooms",
                  "halfbaths")
print(sapply(alldf, dim))
sapply(alldf, function(x) identical(names(x), 
                                    scraped_cols))
print(sapply(alldf, names))


# some manual fixes to let us proceed
colnames(alldf$jt767)[4] <- "totval"
tail(alldf$yw435)
alldf$yw435 <- alldf$yw435[-27308,]

## YOUR TURN:

# Cycling through each of the columns that are scraped, identify what
# discrepancies exist in your solutions. Try to code cleanly, avoiding
# repetition of code where unnecessary.

# Here's something to get you started:

thiscol <- scraped_cols[5]

allsols <- sapply(alldf, function(df) df[,thiscol])

# thoughts?










# Now, organize into groups. Have one person "plug in" to a TV on their
# computer. Write a script to reconcile all of the columns and
# output a **cleaned up data frame** with all variables. Even if 
# you're not plugged in, you should be providing advice (or even coding
# in tandem on your computer). 

# You will likely stumble upon some differences that are due to 
# choices made in coding the data. There's not necessarily a single
# correct way to code the data, but we should settle on a collective
# choice for each of these discrepancies. If you have identified one 
# such coding discrepancy, bring it to my attention, and let's discuss
# as a class. 

# We'll use our pinned note on Piazza to keep track of all such 
# coding choices.
