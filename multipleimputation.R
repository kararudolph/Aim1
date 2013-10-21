setwd("/Users/kararudolph/Documents/PhD/NIMH/NCSA/urbanicity&disadvantage")
library(mitools)
library(mice)

load("forimp.Rdata")
pred<-quickpred(forimp)
pred[,"Id2"] <- 0
pred[,"SampleID"] <- 0
imp<-mice(forimp, pred=pred, maxit=10,m=100,seed=92385)
densityplot(imp)
#stripplot(imp)
save(imp, file="impdata100.Rdata")

load("impdata100.Rdata")
tmp<-list(rep(NA,100))

for(i in 1:100){
  tmp[[i]]<-complete(imp, action=i, include=FALSE)
}

dat<-list(rep(NA,100))

for(i in 1:100){
tmp[[i]]$tertscore<-ifelse(tmp[[i]]$score < (-2.293536), 1, 0)

#make income variable
tmp[[i]]$inc<-ifelse(tmp[[i]]$lninc==0, 0, 1)
tmp[[i]]$inccomp<-tmp[[i]]$inc*tmp[[i]]$lninc
#mean maternal age at birth is 26. 
tmp[[i]]$cmage<-tmp[[i]]$mage-26
tmp[[i]]$cmage2<-tmp[[i]]$cmage^2
#mean log income = mean(dat[[1]]$lninc[which(dat[[1]]$lninc!=0)],) = 11.16733
tmp[[i]]$inc2<-ifelse(tmp[[i]]$lninc==0, 11.16733, tmp[[i]]$lninc)
tmp[[i]]$cinc<-tmp[[i]]$inc2 - 11.16733
tmp[[i]]$nonzeroinc<-ifelse(tmp[[i]]$lninc==0, 0, 1)
tmp[[i]]$meducat<-tmp[[i]]$meducat - 1
tmp[[i]]$racecat<-tmp[[i]]$racecat - 1
  }

keep<-c("SampleID", "meducat", "moth", "fath", "Id2", 
  	"urbancat", "suburb", "age_cent", "CH33", "imgen", 
		"SEXF", "d_mdddys12_NIMH2", "pc_psych_minor", "d_anxiety12_NIMH2", "d_mood12_NIMH2","any", "internal", "pc_pa_minor", "pc_pa_severe", "pp_pa_minor", "pp_pa_severe",
        "cp_CdOddh12_NIMH2", "cinc", "nonzeroinc",
        "racecat",  "tertscore", "score", "str", "secu", "final_weight", "region", "cmage", "cmage2")
for(i in 1:100){
dat[[i]]<-tmp[[i]][keep]}


