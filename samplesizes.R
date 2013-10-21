# find number of adolescents in neighborhood.
library(plyr)
clustering <- ddply(new.prpdat.incl[[1]], .(Id2), summarise, 
               sum = length(SampleID)
               )
summary(clustering)

#find how many parents involved
parents<-read.table("psaqshort.csv", header=TRUE, sep=",")
parents1<-parents[!is.na(parents$final_weight_psaq_short),]
parents2<-merge(parents1, new.prpdat.incl[[1]], all=FALSE, by="final_weight")
nrow(parents2)
#[1] 8296
