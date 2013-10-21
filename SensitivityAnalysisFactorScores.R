setwd("/Users/krudolph/Documents/PhD/NIMH/Ncsa")
library(mitools)
library(survey)
library(mice)
library(MatchIt)
load("impdata100.Rdata")
tmp<-list(rep(NA,100))
tmpone<-list(rep(NA,100))

for(i in 1:100){
  tmp[[i]]<-complete(imp, action=i, include=FALSE)
}

load("nh1_fs.Rdata")
for(i in 1:100){
  tmpone[[i]]<-merge(tmp[[i]], nh1_fs, by.x=c("Id2", "SampleID"), by.y=c("Id2", "SampleID.y"), all=FALSE)
}

for(i in 1:100){
	tmp[[i]]<-tmpone[[i]]
}

multimp<-imputationList(tmp)

library(MatchIt)

dat<-list(rep(NA,100))
tmp.out.subclass<-list(rep(NA,100))
tmp.subcl<-list(rep(NA,100))
s.tmp.out<-list(rep(NA,100))
n<-list(rep(NA,100))

full.out.subclass<-list(rep(NA,100))
full.subcl<-list(rep(NA,100))
s.full.out<-list(rep(NA,100))
n.full<-list(rep(NA,100))

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
		"SEXF", "d_mdddys12_NIMH2", "pc_psych_minor", "d_anxiety12_NIMH2", "d_mood12_NIMH2","any", "internal", 
        "cp_CdOddh12_NIMH2", "cinc", "nonzeroinc",
        "racecat",  "tertscore", "score", "str", "secu", "final_weight", "region", "cmage", "cmage2", "disnh", "scale_fs")
for(i in 1:100){
dat[[i]]<-tmp[[i]][keep]}


###################################################################################################################
## Now, I'm making propensity score subclasses using the MatchIt program.
###################################################################################################################

#attach(dat[[i]])
#tmp.out.subclass[[i]]<-matchit(tertscore ~ SEXF + age_cent + meducat + moth + fath + urbancat + suburb + imgen + mage + factor(racecat) + factor(region) + inccomp, method="subclass", subclass=100, data=dat[[i]], sub.by="all", discard="both")
#detach(dat[[i]])
#print(summary(tmp.out.subclass[[i]], standardize=TRUE))
#s.tmp.out[[i]]<-summary(tmp.out.subclass[[i]], standardize=TRUE)
#tmp.subcl[[i]]<-match.data(tmp.out.subclass[[i]])
#n[[i]]<-dim(tmp.subcl[[i]])[1]
#}

#Sample sizes by subclasses:
#        Subclass 1 Subclass 2 Subclass 3 Subclass 4 Subclass 5
#Treated         23         69        139        194        270
#Control        840        938        868        814        737
#Total          863       10007       10007       10008       10007
#        Subclass 6 Subclass 7 Subclass 8 Subclass 9 Subclass 100
#Treated        400        495        575        662         839
#Control        607        513        432        345         165
#Total         10007       10008       10007       10007        10004


#few individuals in the first subclass so combine first and second. 

subclass8<-c(0.00,0.20, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00) 
for(i in 1:100){
attach(dat[[i]])
tmp.out.subclass[[i]]<-matchit(disnh ~ age_cent*SEXF + factor(meducat) + factor(meducat):SEXF + moth + moth:SEXF + fath + fath:SEXF + 
  urbancat + urbancat:SEXF + suburb + suburb:SEXF + factor(imgen) + factor(imgen):SEXF + poly(cmage, 2) + poly(cmage,2):SEXF + factor(racecat) +  factor(racecat):SEXF + factor(region) + factor(region):SEXF + cinc:nonzeroinc + cinc:nonzeroinc:SEXF, method="subclass", 
        subclass=subclass8, data=dat[[i]], sub.by="all", discard="both")
detach(dat[[i]])
#print(summary(tmp.out.subclass[[1]], standardize=TRUE))
s.tmp.out[[i]]<-summary(tmp.out.subclass[[i]], standardize=TRUE)
tmp.subcl[[i]]<-match.data(tmp.out.subclass[[i]])
n[[i]]<-dim(tmp.subcl[[i]])[1]
}


#this is a little tricky, because have folks who are in the dataset for some imputations but not for others. I
#want to limit the discards to discarding those who are outside the bounds on any the imputed datasets. 

incl<-subset(dat[[1]], !SampleID %in% tmp.subcl[[1]][,1]  |  !SampleID %in%tmp.subcl[[2]][,1] 
             |  !SampleID %in%tmp.subcl[[3]][,1]  |  !SampleID %in%tmp.subcl[[4]][,1]  |  !SampleID %in%tmp.subcl[[5]][,1]
              |  !SampleID %in%tmp.subcl[[6]][,1]  |  !SampleID %in%tmp.subcl[[7]][,1]  |  !SampleID %in%tmp.subcl[[8]][,1]
              |  !SampleID %in%tmp.subcl[[9]][,1]  |  !SampleID %in%tmp.subcl[[10]][,1]
             |  !SampleID %in% tmp.subcl[[11]][,1]  |  !SampleID %in%tmp.subcl[[12]][,1] 
             |  !SampleID %in%tmp.subcl[[13]][,1]  |  !SampleID %in%tmp.subcl[[14]][,1]  |  !SampleID %in%tmp.subcl[[15]][,1]
              |  !SampleID %in%tmp.subcl[[16]][,1]  |  !SampleID %in%tmp.subcl[[17]][,1]  |  !SampleID %in%tmp.subcl[[18]][,1]
              |  !SampleID %in%tmp.subcl[[19]][,1]  |  !SampleID %in%tmp.subcl[[20]][,1]
             |  !SampleID %in% tmp.subcl[[21]][,1]  |  !SampleID %in%tmp.subcl[[22]][,1] 
             |  !SampleID %in%tmp.subcl[[23]][,1]  |  !SampleID %in%tmp.subcl[[24]][,1]  |  !SampleID %in%tmp.subcl[[25]][,1]
              |  !SampleID %in%tmp.subcl[[26]][,1]  |  !SampleID %in%tmp.subcl[[27]][,1]  |  !SampleID %in%tmp.subcl[[28]][,1]
              |  !SampleID %in%tmp.subcl[[29]][,1]  |  !SampleID %in%tmp.subcl[[30]][,1]
             |  !SampleID %in% tmp.subcl[[31]][,1]  |  !SampleID %in%tmp.subcl[[32]][,1] 
             |  !SampleID %in%tmp.subcl[[33]][,1]  |  !SampleID %in%tmp.subcl[[34]][,1]  |  !SampleID %in%tmp.subcl[[35]][,1]
              |  !SampleID %in%tmp.subcl[[36]][,1]  |  !SampleID %in%tmp.subcl[[37]][,1]  |  !SampleID %in%tmp.subcl[[38]][,1]
              |  !SampleID %in%tmp.subcl[[39]][,1]  |  !SampleID %in%tmp.subcl[[40]][,1]
             | !SampleID %in% tmp.subcl[[41]][,1]  |  !SampleID %in%tmp.subcl[[42]][,1] 
             |  !SampleID %in%tmp.subcl[[43]][,1]  |  !SampleID %in%tmp.subcl[[44]][,1]  |  !SampleID %in%tmp.subcl[[45]][,1]
              |  !SampleID %in%tmp.subcl[[46]][,1]  |  !SampleID %in%tmp.subcl[[47]][,1]  |  !SampleID %in%tmp.subcl[[48]][,1]
              |  !SampleID %in%tmp.subcl[[49]][,1]  |  !SampleID %in%tmp.subcl[[50]][,1]
             |  !SampleID %in% tmp.subcl[[51]][,1]  |  !SampleID %in%tmp.subcl[[52]][,1] 
             |  !SampleID %in%tmp.subcl[[53]][,1]  |  !SampleID %in%tmp.subcl[[54]][,1]  |  !SampleID %in%tmp.subcl[[55]][,1]
              |  !SampleID %in%tmp.subcl[[56]][,1]  |  !SampleID %in%tmp.subcl[[57]][,1]  |  !SampleID %in%tmp.subcl[[58]][,1]
              |  !SampleID %in%tmp.subcl[[59]][,1]  |  !SampleID %in%tmp.subcl[[60]][,1]
             | !SampleID %in% tmp.subcl[[61]][,1]  |  !SampleID %in%tmp.subcl[[62]][,1] 
             |  !SampleID %in%tmp.subcl[[63]][,1]  |  !SampleID %in%tmp.subcl[[64]][,1]  |  !SampleID %in%tmp.subcl[[65]][,1]
              |  !SampleID %in%tmp.subcl[[66]][,1]  |  !SampleID %in%tmp.subcl[[67]][,1]  |  !SampleID %in%tmp.subcl[[68]][,1]
              |  !SampleID %in%tmp.subcl[[69]][,1]  |  !SampleID %in%tmp.subcl[[70]][,1]
             |  !SampleID %in% tmp.subcl[[71]][,1]  |  !SampleID %in%tmp.subcl[[72]][,1] 
             |  !SampleID %in%tmp.subcl[[73]][,1]  |  !SampleID %in%tmp.subcl[[74]][,1]  |  !SampleID %in%tmp.subcl[[75]][,1]
              |  !SampleID %in%tmp.subcl[[76]][,1]  |  !SampleID %in%tmp.subcl[[77]][,1]  |  !SampleID %in%tmp.subcl[[78]][,1]
              |  !SampleID %in%tmp.subcl[[79]][,1]  |  !SampleID %in%tmp.subcl[[80]][,1]
                    |  !SampleID %in% tmp.subcl[[81]][,1]  |  !SampleID %in%tmp.subcl[[82]][,1] 
             |  !SampleID %in%tmp.subcl[[83]][,1]  |  !SampleID %in%tmp.subcl[[84]][,1]  |  !SampleID %in%tmp.subcl[[85]][,1]
              |  !SampleID %in%tmp.subcl[[86]][,1]  |  !SampleID %in%tmp.subcl[[87]][,1]  |  !SampleID %in%tmp.subcl[[88]][,1]
              |  !SampleID %in%tmp.subcl[[89]][,1]  |  !SampleID %in%tmp.subcl[[90]][,1]
                    |  !SampleID %in% tmp.subcl[[91]][,1]  |  !SampleID %in%tmp.subcl[[92]][,1] 
             |  !SampleID %in%tmp.subcl[[93]][,1]  |  !SampleID %in%tmp.subcl[[94]][,1]  |  !SampleID %in%tmp.subcl[[95]][,1]
              |  !SampleID %in%tmp.subcl[[96]][,1]  |  !SampleID %in%tmp.subcl[[97]][,1]  |  !SampleID %in%tmp.subcl[[98]][,1]
              |  !SampleID %in%tmp.subcl[[99]][,1]  |  !SampleID %in%tmp.subcl[[100]][,1]
            )

new.prpdat.incl<-list(rep(NA,100))

for (i in 1:100){
  new.prpdat.incl[[i]]<-subset(tmp.subcl[[i]], !SampleID %in% incl[,1])
}

save(new.prpdat.incl, file="new.prpdat.incl.eightsub.fs.Rdata")

#Now, all of the imputed datasets have 10046 people. 12 people omitted.
load("new.prpdat.incl.Rdata")

for(i in 1:100){
  print(table(new.prpdat.incl[[i]]$subclass))
}
#   1    2    3    4    5    6    7    8    9 
#1555 1011 1012 1012 1013 1012 1012 1012  961 
# And now, the same number of people are in each subclass over the 100 imputations. 

#get graph of propensity score
#Figure 1
tmp.subcl[[1]]$disad<-ifelse(tmp.subcl[[1]]$disnh==1, "Disadvantaged", "Non-disadvantaged")
new.prpdat.incl[[1]]$disad<-ifelse(new.prpdat.incl[[1]]$disnh==1, "Disadvantaged", "Non-disadvantaged")
gr<-tmp.subcl[[1]]
gr$y[gr$SampleID %in% incl[,1] & gr$disnh==1]<-"Discarded, Disadvantaged"
gr$y[gr$SampleID %in% incl[,1] & gr$disnh==0]<-"Discarded, Non-disadvantaged"
gr$y[!gr$SampleID %in% incl[,1] & gr$disnh==0]<-"Non-disadvantaged"
gr$y[!gr$SampleID %in% incl[,1] & gr$disnh==1]<-"Disadvantaged"

library(gridExtra)
library(ggplot2)
pdf("Fig1_june2012.pdf")
p<-qplot(distance, y, data=gr, position="jitter", xlab="propensity score", ylab="")
p2<-p + theme_bw() + scale_y_discrete(limits=c("Discarded, Disadvantaged", "Disadvantaged", "Non-disadvantaged", "Discarded, Non-disadvantaged"))
q<-qplot(distance, data=new.prpdat.incl[[1]], geom="histogram", xlab="propensity score", ylab="")
q2<-q + theme_bw() + facet_wrap(~disad)
grid.arrange(p2, q2, nrow=2, ncol=1)
dev.off()

#Table 1
library(xtable)
multimp<-imputationList(new.prpdat.incl)
desimp<-svydesign(id=~secu, strata=~str, nest=TRUE, weight=~final_weight, data=multimp)

g<-with(desimp, svyby(~SEXF + urbancat + suburb + factor(imgen) +  cinc + age_cent + factor(meducat) + factor(region) + moth + fath + factor(racecat) + cmage + internal + cp_CdOddh12_NIMH2, ~disnh, svymean, keep.var=TRUE, na.rm=TRUE))

summary(MIcombine(g))
m<-as.data.frame(summary(MIcombine(g)))
n<-m[,c(-2,-5)]
odd<-seq(1,50,by=2)
even<-seq(2,50,by=2)
exp<-n[even,]
unexp<-n[odd,]
o<-cbind( exp,unexp)
print(xtable(n, type="latex"))

print(xtable(summaryBy(nhdis ~ subclass + SEXF, data=new.prpdat[[1]], FUN=c(length, sum))))


with(new.prpdat[[1]][new.prpdat[[1]]$SEXF==1,], table(subclass, sum(disnh)))

# Check cell sizes:
for(i in 1:100){
  new.prpdat.incl[[i]]$urbanicity[new.prpdat.incl[[i]]$urbancat==1]<-2
  new.prpdat.incl[[i]]$urbanicity[new.prpdat.incl[[i]]$suburb==1]<-1
  new.prpdat.incl[[i]]$urbanicity[new.prpdat.incl[[i]]$urbancat==0 & new.prpdat.incl[[i]]$suburb==0]<-0
}

library(doBy)
library(xtable)
x<-summaryBy(internal + cp_CdOddh12_NIMH2 ~ urbanicity + disnh + SEXF, data=new.prpdat.incl[[1]], FUN=c(mean, length, sum))
y<-summaryBy(internal + cp_CdOddh12_NIMH2 ~ urbanicity + disnh + SEXF + subclass, data=new.prpdat.incl[[1]], FUN=c(mean, length, sum))
print(xtable(x, type="html"))
#print(xtable(y, type="html"))

des<-list(rep(NA,100))
for(i in 1:100){
des[[i]]<-svydesign(id=~secu, strata=~str, nest=TRUE, weight=~final_weight, data=new.prpdat.incl[[i]])
}
#now, look at the distribution of propensity scores
for(i in 1:100){
  print(svyquantile(~distance, des[[i]], c(.25,.5,.75),ci=FALSE))
}

dsub1<-list(rep(NA,100))
dsub2<-list(rep(NA,100))
dsub3<-list(rep(NA,100))
dsub4<-list(rep(NA,100))
dsub5<-list(rep(NA,100))
dsub6<-list(rep(NA,100))
dsub7<-list(rep(NA,100))
dsub8<-list(rep(NA,100))
dsub9<-list(rep(NA,100))

for(i in 1:100){
  dsub1[[i]]<-subset(des[[i]],subclass==1)
  dsub2[[i]]<-subset(des[[i]],subclass==2)
  dsub3[[i]]<-subset(des[[i]],subclass==3)
  dsub4[[i]]<-subset(des[[i]],subclass==4)
  dsub5[[i]]<-subset(des[[i]],subclass==5)
  dsub6[[i]]<-subset(des[[i]],subclass==6)
  dsub7[[i]]<-subset(des[[i]],subclass==7)
  dsub8[[i]]<-subset(des[[i]],subclass==8)
  dsub9[[i]]<-subset(des[[i]],subclass==9)
}

for(i in 1:100){
  print(svyquantile(~distance, dsub1[[i]], c(.25,.5,.75),ci=FALSE))
}
for(i in 1:100){
  print(svyquantile(~distance, dsub2[[i]], c(.25,.5,.75),ci=FALSE))
}
for(i in 1:100){
  print(svyquantile(~distance, dsub3[[i]], c(.25,.5,.75),ci=FALSE))
}
for(i in 1:100){
  print(svyquantile(~distance, dsub4[[i]], c(.25,.5,.75),ci=FALSE))
}
for(i in 1:100){
  print(svyquantile(~distance, dsub5[[i]], c(.25,.5,.75),ci=FALSE))
}
for(i in 1:100){
  print(svyquantile(~distance, dsub6[[i]], c(.25,.5,.75),ci=FALSE))
}
for(i in 1:100){
  print(svyquantile(~distance, dsub7[[i]], c(.25,.5,.75),ci=FALSE))
}
for(i in 1:100){
  print(svyquantile(~distance, dsub8[[i]], c(.25,.5,.75),ci=FALSE))
}
for(i in 1:100){
  print(svyquantile(~distance, dsub9[[i]], c(.25,.5,.75),ci=FALSE))
}

#the distribution of propensity scores looks the same across imputations both (1) across subclasses and (2) within
#subclasses

pdf("FiguresJitterbothsex.pdf")
plot(tmp.out.subclass[[1]], type="jitter", interactive=TRUE)
dev.off()
pdf("FiguresStdBiasNearestbothsex.pdf")
plot(s.tmp.out[[1]], interactive=FALSE)
dev.off()
plot(tmp.out.subclass[[1]], type="hist", interactive=FALSE)
pdf("propensitybothsex.pdf")
par(mfrow=c(2,2))
hist(tmp.subcl[[1]]$distance[tmp.subcl[[1]]$disnh==1], main=("Histogram of propensity scores"), col="orange", breaks=100)
hist(tmp.subcl[[1]]$distance[tmp.subcl[[1]]$disnh==0], main=("Histogram of propensity scores"), col="orange", breaks=100)
dev.off()
#Sex is the only covariate whose balance gets worse

#check balance in male/female groups
plot(tmp.out.subclass[[1]], type = "QQ", interactive = FALSE, which.xs = "urbancat")



int1<-list(rep(NA,100))
int2<-list(rep(NA,100))
int3<-list(rep(NA,100))
int4<-list(rep(NA,100))
int5<-list(rep(NA,100))
int6<-list(rep(NA,100))
int7<-list(rep(NA,100))
int8<-list(rep(NA,100))
int9<-list(rep(NA,100))


ext1<-list(rep(NA,100))
ext2<-list(rep(NA,100))
ext3<-list(rep(NA,100))
ext4<-list(rep(NA,100))
ext5<-list(rep(NA,100))
ext6<-list(rep(NA,100))
ext7<-list(rep(NA,100))
ext8<-list(rep(NA,100))
ext9<-list(rep(NA,100))

vcov.int1<-list(rep(NA,100))
vcov.int2<-list(rep(NA,100))
vcov.int3<-list(rep(NA,100))
vcov.int4<-list(rep(NA,100))
vcov.int5<-list(rep(NA,100))
vcov.int6<-list(rep(NA,100))
vcov.int7<-list(rep(NA,100))
vcov.int8<-list(rep(NA,100))
vcov.int9<-list(rep(NA,100))


vcov.ext1<-list(rep(NA,100))
vcov.ext2<-list(rep(NA,100))
vcov.ext3<-list(rep(NA,100))
vcov.ext4<-list(rep(NA,100))
vcov.ext5<-list(rep(NA,100))
vcov.ext6<-list(rep(NA,100))
vcov.ext7<-list(rep(NA,100))
vcov.ext8<-list(rep(NA,100))
vcov.ext9<-list(rep(NA,100))


###################################################################################################################
## Now, I'm performing design-based, survey-weighted regression analyses for each imputation. 
###################################################################################################################

library(survey)

#to try to get rid of warnings
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="average")

#Females first
for(i in 1:100){
int1[[i]]<-summary(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==1)))
int2[[i]]<-summary(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==2)))
int3[[i]]<-summary(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==3)))
int4[[i]]<-summary(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==4)))
int5[[i]]<-summary(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==5)))
int6[[i]]<-summary(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==6)))
int7[[i]]<-summary(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==7)))
int8[[i]]<-summary(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==8)))

ext1[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==1)))
ext2[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==2)))
ext3[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==3)))
ext4[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==4)))
ext5[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==5)))
ext6[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==6)))
ext7[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==7)))
ext8[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==8)))

vcov.int1[[i]]<-vcov(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==1)))
vcov.int2[[i]]<-vcov(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==2)))
vcov.int3[[i]]<-vcov(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==3)))
vcov.int4[[i]]<-vcov(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==4)))
vcov.int5[[i]]<-vcov(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==5)))
vcov.int6[[i]]<-vcov(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==6)))
vcov.int7[[i]]<-vcov(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==7)))
vcov.int8[[i]]<-vcov(svyglm(internal ~ disnh + disnh:urbancat + disnh:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==8)))


vcov.ext1[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==1)))
vcov.ext2[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==2)))
vcov.ext3[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==3)))
vcov.ext4[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==4)))
vcov.ext5[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==5)))
vcov.ext6[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==6)))
vcov.ext7[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==7)))
vcov.ext8[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ disnh + disnh:urbancat + disnh:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==8)))

}



#for each subclass, get ratio of number of individuals in subclass to number of individuals overall
N.s.1<-list(rep(NA,100))
N.s.2<-list(rep(NA,100))
N.s.3<-list(rep(NA,100))
N.s.4<-list(rep(NA,100))
N.s.5<-list(rep(NA,100))
N.s.6<-list(rep(NA,100))
N.s.7<-list(rep(NA,100))
N.s.8<-list(rep(NA,100))
N.s.9<-list(rep(NA,100))
N<-list(rep(NA, 100))
sub1Fem<-list(rep(NA,100))
sub2Fem<-list(rep(NA,100))
sub3Fem<-list(rep(NA,100))
sub4Fem<-list(rep(NA,100))
sub5Fem<-list(rep(NA,100))
sub6Fem<-list(rep(NA,100))
sub7Fem<-list(rep(NA,100))
sub8Fem<-list(rep(NA,100))
sub9Fem<-list(rep(NA,100))	

#calculating effects by averaging across subclass for each imputation.
#tertscore
effect<-list(rep(NA, 100))
var<-list(rep(NA, 100))
expon<-list(rep(NA, 100))
z<-list(rep(NA, 100))
p<-list(rep(NA, 100))

#for(i in 1:100){
#effect[[i]]<-((int1[[i]]$coef[2]*(N.s.1[[i]]/N[[i]])) + (int2[[i]]$coef[2]*(N.s.2[[i]]/N[[i]])) + (int3[[i]]$coef[2]*(N.s.3[[i]]/N[[i]])) + (int4[[i]]$coef[2]*(N.s.4[[i]]/N[[i]])) + (int5[[i]]$coef[2]*(N.s.5[[i]]/N[[i]])) + (int6[[i]]$coef[2]*(N.s.6[[i]]/N[[i]])) + (int7[[i]]$coef[2]*(N.s.7[[i]]/N[[i]])) + (int8[[i]]$coef[2]*(N.s.8[[i]]/N[[i]])) + (int9[[i]]$coef[2]*(N.s.9[[i]]/N[[i]])) ) 

#var[[i]]<-(((int1[[i]]$coef[2,2]^2)*(N.s.1[[i]]/N[[i]])^2) +((int2[[i]]$coef[2,2]^2)*(N.s.2[[i]]/N[[i]])^2) + ((int3[[i]]$coef[2,2]^2)*(N.s.3[[i]]/N[[i]])^2) + ((int4[[i]]$coef[2,2]^2)*(N.s.4[[i]]/N[[i]])^2)+ ((int5[[i]]$coef[2,2]^2)*(N.s.5[[i]]/N[[i]])^2)+ ((int6[[i]]$coef[2,2]^2)*(N.s.6[[i]]/N[[i]])^2)+ ((int7[[i]]$coef[2,2]^2)*(N.s.7[[i]]/N[[i]])^2)+ ((int8[[i]]$coef[2,2]^2)*(N.s.8[[i]]/N[[i]])^2)+ ((int9[[i]]$coef[2,2]^2)*(N.s.9[[i]]/N[[i]])^2))

#calculating the mantel-haentzel logs odds ratio
## Effect for rural subgroup
for(i in 1:100){
  effect[[i]]<- ( ( (int1[[i]]$coef[2]*(1/int1[[i]]$coef[2,2]^2)) + (int2[[i]]$coef[2]*(1/int2[[i]]$coef[2,2]^2)) + 
    (int3[[i]]$coef[2]*(1/int3[[i]]$coef[2,2]^2)) + (int4[[i]]$coef[2]*(1/int4[[i]]$coef[2,2]^2)) + (int5[[i]]$coef[2]*(1/int5[[i]]$coef[2,2]^2))
    + (int6[[i]]$coef[2]*(1/int6[[i]]$coef[2,2]^2)) + (int7[[i]]$coef[2]*(1/int7[[i]]$coef[2,2]^2)) +  (int8[[i]]$coef[2]*(1/int8[[i]]$coef[2,2]^2)) 
                  ) / ( (1/int1[[i]]$coef[2,2]^2) +
                    (1/int2[[i]]$coef[2,2]^2)  + (1/int3[[i]]$coef[2,2]^2) + (1/int4[[i]]$coef[2,2]^2) + (1/int5[[i]]$coef[2,2]^2)
                    + (1/int6[[i]]$coef[2,2]^2) +(1/int7[[i]]$coef[2,2]^2) +(1/int8[[i]]$coef[2,2]^2)  ) )

  var[[i]]<- 1 / ((1/int1[[i]]$coef[2,2]^2) + (1/int2[[i]]$coef[2,2]^2)  + (1/int3[[i]]$coef[2,2]^2) + (1/int4[[i]]$coef[2,2]^2) + (1/int5[[i]]$coef[2,2]^2)
                    + (1/int6[[i]]$coef[2,2]^2) +(1/int7[[i]]$coef[2,2]^2) +(1/int8[[i]]$coef[2,2]^2))

#gives exponentiated results
expon[[i]]<-exp(effect[[i]])
#gives 2-sided p values based on wald statistics
z[[i]]<-effect[[i]]/sqrt(var[[i]])
p[[i]]<-2*pnorm(abs(z[[i]]), lower.tail=FALSE)

}

scorecore.int<-list(rep(NA, 100))
var.scorecore.int<-list(rep(NA, 100))

for(i in 1:100){
  scorecore.int[[i]]<- ( ( (int1[[i]]$coef[4]*(1/int1[[i]]$coef[4,2]^2)) + (int2[[i]]$coef[4]*(1/int2[[i]]$coef[4,2]^2)) + 
    (int3[[i]]$coef[4]*(1/int3[[i]]$coef[4,2]^2)) + (int4[[i]]$coef[4]*(1/int4[[i]]$coef[4,2]^2)) + (int5[[i]]$coef[4]*(1/int5[[i]]$coef[4,2]^2))
    + (int6[[i]]$coef[4]*(1/int6[[i]]$coef[4,2]^2)) + (int7[[i]]$coef[4]*(1/int7[[i]]$coef[4,2]^2)) +  (int8[[i]]$coef[4]*(1/int8[[i]]$coef[4,2]^2)) 
                  ) / ( (1/int1[[i]]$coef[4,2]^2) +
                    (1/int2[[i]]$coef[4,2]^2)  + (1/int3[[i]]$coef[4,2]^2) + (1/int4[[i]]$coef[4,2]^2) + (1/int5[[i]]$coef[4,2]^2)
                    + (1/int6[[i]]$coef[4,2]^2) +(1/int7[[i]]$coef[4,2]^2) +(1/int8[[i]]$coef[4,2]^2) ) )

  var.scorecore.int[[i]]<- 1 / ((1/int1[[i]]$coef[4,2]^2) + (1/int2[[i]]$coef[4,2]^2)  + (1/int3[[i]]$coef[4,2]^2) + (1/int4[[i]]$coef[4,2]^2) + (1/int5[[i]]$coef[4,2]^2)
                    + (1/int6[[i]]$coef[4,2]^2) +(1/int7[[i]]$coef[4,2]^2) +(1/int8[[i]]$coef[4,2]^2) )
}

scorefringe.int<-list(rep(NA, 100))
var.scorefringe.int<-list(rep(NA, 100))

for(i in 1:100){
  scorefringe.int[[i]]<- ( ( (int1[[i]]$coef[5]*(1/int1[[i]]$coef[5,2]^2)) + (int2[[i]]$coef[5]*(1/int2[[i]]$coef[5,2]^2)) + 
    (int3[[i]]$coef[5]*(1/int3[[i]]$coef[5,2]^2)) + (int4[[i]]$coef[5]*(1/int4[[i]]$coef[5,2]^2)) + (int5[[i]]$coef[5]*(1/int5[[i]]$coef[5,2]^2))
    + (int6[[i]]$coef[5]*(1/int6[[i]]$coef[5,2]^2)) + (int7[[i]]$coef[5]*(1/int7[[i]]$coef[5,2]^2)) +  (int8[[i]]$coef[5]*(1/int8[[i]]$coef[5,2]^2)) 
                   ) / ( (1/int1[[i]]$coef[5,2]^2) +
                    (1/int2[[i]]$coef[5,2]^2)  + (1/int3[[i]]$coef[5,2]^2) + (1/int4[[i]]$coef[5,2]^2) + (1/int5[[i]]$coef[5,2]^2)
                    + (1/int6[[i]]$coef[5,2]^2) +(1/int7[[i]]$coef[5,2]^2) +(1/int8[[i]]$coef[5,2]^2)  ) )

  var.scorefringe.int[[i]]<- 1 / ((1/int1[[i]]$coef[5,2]^2) + (1/int2[[i]]$coef[5,2]^2)  + (1/int3[[i]]$coef[5,2]^2) + (1/int4[[i]]$coef[5,2]^2) + (1/int5[[i]]$coef[5,2]^2)
                    + (1/int6[[i]]$coef[5,2]^2) +(1/int7[[i]]$coef[5,2]^2) +(1/int8[[i]]$coef[5,2]^2) )

}

lg.urb.tert<-list(rep(NA, 100))
var.lg.urb.tert<-list(rep(NA, 100))
expon.lg.urb.tert<-list(rep(NA, 100))
z.lg.urb.tert<-list(rep(NA, 100))
p.lg.urb.tert<-list(rep(NA, 100))


## Effect for large urban subgroup
a <- c(0, 1, 0,1,0)
for(i in 1:100){
  lg.urb.tert[[i]]<- ((  
     ( (int1[[i]]$coef[2]+int1[[i]]$coef[4]) * (1/ (a %*% vcov.int1[[i]] %*% a)) ) + 
     ( (int2[[i]]$coef[2]+int2[[i]]$coef[4]) * (1/ (a %*% vcov.int2[[i]] %*% a)) ) +
     ( (int3[[i]]$coef[2]+int3[[i]]$coef[4]) * (1/ (a %*% vcov.int3[[i]] %*% a)) ) +
     ( (int4[[i]]$coef[2]+int4[[i]]$coef[4]) * (1/ (a %*% vcov.int4[[i]] %*% a)) ) +
     ( (int5[[i]]$coef[2]+int5[[i]]$coef[4]) * (1/ (a %*% vcov.int5[[i]] %*% a)) ) +
     ( (int6[[i]]$coef[2]+int6[[i]]$coef[4]) * (1/ (a %*% vcov.int6[[i]] %*% a)) ) +
     ( (int7[[i]]$coef[2]+int7[[i]]$coef[4]) * (1/ (a %*% vcov.int7[[i]] %*% a)) ) +
     ( (int8[[i]]$coef[2]+int8[[i]]$coef[4]) * (1/ (a %*% vcov.int8[[i]] %*% a)) ) 
     ) / (
       (1/ (a %*% vcov.int1[[i]] %*% a))  +(1/ (a %*% vcov.int2[[i]] %*% a))  +(1/ (a %*% vcov.int3[[i]] %*% a)) +
         (1/ (a %*% vcov.int4[[i]] %*% a)) +(1/ (a %*% vcov.int5[[i]] %*% a)) +(1/ (a %*% vcov.int6[[i]] %*% a)) +
         (1/ (a %*% vcov.int7[[i]] %*% a))  +(1/ (a %*% vcov.int8[[i]] %*% a)) 
))
       
  var.lg.urb.tert[[i]]<- 1 / ( (1/ (a %*% vcov.int1[[i]] %*% a))  +(1/ (a %*% vcov.int2[[i]] %*% a))+
    (1/ (a %*% vcov.int3[[i]] %*% a)) + (1/ (a %*% vcov.int4[[i]] %*% a)) +(1/ (a %*% vcov.int5[[i]] %*% a)) +
    (1/ (a %*% vcov.int6[[i]] %*% a)) + (1/ (a %*% vcov.int7[[i]] %*% a))  +(1/ (a %*% vcov.int8[[i]] %*% a))  )

#gives exponentiated results
expon.lg.urb.tert[[i]]<-exp(lg.urb.tert[[i]])
#gives 2-sided p values based on wald statistics
#z.urban[[i]]<-urbancat[[i]]/sqrt(var.urban[[i]])
#p.urban[[i]]<-2*pnorm(abs(z.urban[[i]]), lower.tail=FALSE)
}


frg.urb.tert<-list(rep(NA, 100))
var.frg.urb.tert<-list(rep(NA, 100))
expon.frg.urb.tert<-list(rep(NA, 100))

## Effect for urban fringe subgroup
a <- c(0, 1, 0,0,1)
for(i in 1:100){
  frg.urb.tert[[i]]<- ((  
     ( (int1[[i]]$coef[2]+int1[[i]]$coef[5]) * (1/ (a %*% vcov.int1[[i]] %*% a)) ) + 
     ( (int2[[i]]$coef[2]+int2[[i]]$coef[5]) * (1/ (a %*% vcov.int2[[i]] %*% a)) ) +
     ( (int3[[i]]$coef[2]+int3[[i]]$coef[5]) * (1/ (a %*% vcov.int3[[i]] %*% a)) ) +
     ( (int4[[i]]$coef[2]+int4[[i]]$coef[5]) * (1/ (a %*% vcov.int4[[i]] %*% a)) ) +
     ( (int5[[i]]$coef[2]+int5[[i]]$coef[5]) * (1/ (a %*% vcov.int5[[i]] %*% a)) ) +
     ( (int6[[i]]$coef[2]+int6[[i]]$coef[5]) * (1/ (a %*% vcov.int6[[i]] %*% a)) ) +
     ( (int7[[i]]$coef[2]+int7[[i]]$coef[5]) * (1/ (a %*% vcov.int7[[i]] %*% a)) ) +
     ( (int8[[i]]$coef[2]+int8[[i]]$coef[5]) * (1/ (a %*% vcov.int8[[i]] %*% a)) ) 
     ) / (
       (1/ (a %*% vcov.int1[[i]] %*% a))  +(1/ (a %*% vcov.int2[[i]] %*% a))  +(1/ (a %*% vcov.int3[[i]] %*% a)) +
         (1/ (a %*% vcov.int4[[i]] %*% a)) +(1/ (a %*% vcov.int5[[i]] %*% a)) +(1/ (a %*% vcov.int6[[i]] %*% a)) +
         (1/ (a %*% vcov.int7[[i]] %*% a))  +(1/ (a %*% vcov.int8[[i]] %*% a))
))
       
  var.frg.urb.tert[[i]]<- 1 / ( (1/ (a %*% vcov.int1[[i]] %*% a))  +(1/ (a %*% vcov.int2[[i]] %*% a))+
    (1/ (a %*% vcov.int3[[i]] %*% a)) + (1/ (a %*% vcov.int4[[i]] %*% a)) +(1/ (a %*% vcov.int5[[i]] %*% a)) +
    (1/ (a %*% vcov.int6[[i]] %*% a)) + (1/ (a %*% vcov.int7[[i]] %*% a))  +(1/ (a %*% vcov.int8[[i]] %*% a)) )

#gives exponentiated results
expon.frg.urb.tert[[i]]<-exp(frg.urb.tert[[i]])
}

#combining across datasets
library(mitools)

#rural
rural.int<-MIcombine(results=effect, variances=var) 

#large urban
lg.urb.int<-MIcombine(results=lg.urb.tert, variances=var.lg.urb.tert)

#fringe urban
frg.urb.int<-MIcombine(results=frg.urb.tert, variances=var.frg.urb.tert)

mult.scorecore.int<-MIcombine(results=scorecore.int, variances=var.scorecore.int)
mult.scorefringe.int<-MIcombine(results=scorefringe.int, variances=var.scorefringe.int)

#calculating effects by averaging across subclass for each imputation.
#tertscore
#rural
ext.effect<-list(rep(NA, 100))
ext.var<-list(rep(NA, 100))
ext.expon<-list(rep(NA, 100))
ext.z<-list(rep(NA, 100))
ext.p<-list(rep(NA, 100))

for(i in 1:100){
  ext.effect[[i]]<- ( ( (ext1[[i]]$coef[2]*(1/ext1[[i]]$coef[2,2]^2)) + (ext2[[i]]$coef[2]*(1/ext2[[i]]$coef[2,2]^2)) + 
    (ext3[[i]]$coef[2]*(1/ext3[[i]]$coef[2,2]^2)) + (ext4[[i]]$coef[2]*(1/ext4[[i]]$coef[2,2]^2)) + (ext5[[i]]$coef[2]*(1/ext5[[i]]$coef[2,2]^2))
    + (ext6[[i]]$coef[2]*(1/ext6[[i]]$coef[2,2]^2)) + (ext7[[i]]$coef[2]*(1/ext7[[i]]$coef[2,2]^2)) +  (ext8[[i]]$coef[2]*(1/ext8[[i]]$coef[2,2]^2)) 
                 ) / ( (1/ext1[[i]]$coef[2,2]^2) +
                    (1/ext2[[i]]$coef[2,2]^2)  + (1/ext3[[i]]$coef[2,2]^2) + (1/ext4[[i]]$coef[2,2]^2) + (1/ext5[[i]]$coef[2,2]^2)
                    + (1/ext6[[i]]$coef[2,2]^2) +(1/ext7[[i]]$coef[2,2]^2) +(1/ext8[[i]]$coef[2,2]^2) ) )

  ext.var[[i]]<- 1 / ((1/ext1[[i]]$coef[2,2]^2) + (1/ext2[[i]]$coef[2,2]^2)  + (1/ext3[[i]]$coef[2,2]^2) + (1/ext4[[i]]$coef[2,2]^2) + (1/ext5[[i]]$coef[2,2]^2)
                    + (1/ext6[[i]]$coef[2,2]^2) +(1/ext7[[i]]$coef[2,2]^2) +(1/ext8[[i]]$coef[2,2]^2) )

#gives exponentiated results
ext.expon[[i]]<-exp(ext.effect[[i]])
#gives 2-sided p values based on wald statistics
ext.z[[i]]<-ext.effect[[i]]/sqrt(ext.var[[i]])
ext.p[[i]]<-2*pnorm(abs(ext.z[[i]]), lower.tail=FALSE)

}

scorecore.ext<-list(rep(NA, 100))
var.scorecore.ext<-list(rep(NA, 100))

for(i in 1:100){
  scorecore.ext[[i]]<- ( ( (ext1[[i]]$coef[4]*(1/ext1[[i]]$coef[4,2]^2)) + (ext2[[i]]$coef[4]*(1/ext2[[i]]$coef[4,2]^2)) + 
    (ext3[[i]]$coef[4]*(1/ext3[[i]]$coef[4,2]^2)) + (ext4[[i]]$coef[4]*(1/ext4[[i]]$coef[4,2]^2)) + (ext5[[i]]$coef[4]*(1/ext5[[i]]$coef[4,2]^2))
    + (ext6[[i]]$coef[4]*(1/ext6[[i]]$coef[4,2]^2)) + (ext7[[i]]$coef[4]*(1/ext7[[i]]$coef[4,2]^2)) +  (ext8[[i]]$coef[4]*(1/ext8[[i]]$coef[4,2]^2)) 
                   ) / ( (1/ext1[[i]]$coef[4,2]^2) +
                    (1/ext2[[i]]$coef[4,2]^2)  + (1/ext3[[i]]$coef[4,2]^2) + (1/ext4[[i]]$coef[4,2]^2) + (1/ext5[[i]]$coef[4,2]^2)
                    + (1/ext6[[i]]$coef[4,2]^2) +(1/ext7[[i]]$coef[4,2]^2) +(1/ext8[[i]]$coef[4,2]^2)  ) )

  var.scorecore.ext[[i]]<- 1 / ((1/ext1[[i]]$coef[4,2]^2) + (1/ext2[[i]]$coef[4,2]^2)  + (1/ext3[[i]]$coef[4,2]^2) + (1/ext4[[i]]$coef[4,2]^2) + (1/ext5[[i]]$coef[4,2]^2)
                    + (1/ext6[[i]]$coef[4,2]^2) +(1/ext7[[i]]$coef[4,2]^2) +(1/ext8[[i]]$coef[4,2]^2) )
}

scorefringe.ext<-list(rep(NA, 100))
var.scorefringe.ext<-list(rep(NA, 100))

for(i in 1:100){
  scorefringe.ext[[i]]<- ( ( (ext1[[i]]$coef[5]*(1/ext1[[i]]$coef[5,2]^2)) + (ext2[[i]]$coef[5]*(1/ext2[[i]]$coef[5,2]^2)) + 
    (ext3[[i]]$coef[5]*(1/ext3[[i]]$coef[5,2]^2)) + (ext4[[i]]$coef[5]*(1/ext4[[i]]$coef[5,2]^2)) + (ext5[[i]]$coef[5]*(1/ext5[[i]]$coef[5,2]^2))
    + (ext6[[i]]$coef[5]*(1/ext6[[i]]$coef[5,2]^2)) + (ext7[[i]]$coef[5]*(1/ext7[[i]]$coef[5,2]^2)) +  (ext8[[i]]$coef[5]*(1/ext8[[i]]$coef[5,2]^2)) 
                 ) / ( (1/ext1[[i]]$coef[5,2]^2) +
                    (1/ext2[[i]]$coef[5,2]^2)  + (1/ext3[[i]]$coef[5,2]^2) + (1/ext4[[i]]$coef[5,2]^2) + (1/ext5[[i]]$coef[5,2]^2)
                    + (1/ext6[[i]]$coef[5,2]^2) +(1/ext7[[i]]$coef[5,2]^2) +(1/ext8[[i]]$coef[5,2]^2) ) )

  var.scorefringe.ext[[i]]<- 1 / ((1/ext1[[i]]$coef[5,2]^2) + (1/ext2[[i]]$coef[5,2]^2)  + (1/ext3[[i]]$coef[5,2]^2) + (1/ext4[[i]]$coef[5,2]^2) + (1/ext5[[i]]$coef[5,2]^2)
                    + (1/ext6[[i]]$coef[5,2]^2) +(1/ext7[[i]]$coef[5,2]^2) +(1/ext8[[i]]$coef[5,2]^2) )

}



## Effect for large urban subgroup
ext.lg.urb.tert<-list(rep(NA, 100))
var.ext.lg.urb.tert<-list(rep(NA, 100))
expon.ext.lg.urb.tert<-list(rep(NA, 100))

## Effect for large urban subgroup
a <- c(0, 1, 0,1,0)
for(i in 1:100){
  ext.lg.urb.tert[[i]]<- ((  
     ( (ext1[[i]]$coef[2]+ext1[[i]]$coef[4]) * (1/ (a %*% vcov.ext1[[i]] %*% a)) ) + 
     ( (ext2[[i]]$coef[2]+ext2[[i]]$coef[4]) * (1/ (a %*% vcov.ext2[[i]] %*% a)) ) +
     ( (ext3[[i]]$coef[2]+ext3[[i]]$coef[4]) * (1/ (a %*% vcov.ext3[[i]] %*% a)) ) +
     ( (ext4[[i]]$coef[2]+ext4[[i]]$coef[4]) * (1/ (a %*% vcov.ext4[[i]] %*% a)) ) +
     ( (ext5[[i]]$coef[2]+ext5[[i]]$coef[4]) * (1/ (a %*% vcov.ext5[[i]] %*% a)) ) +
     ( (ext6[[i]]$coef[2]+ext6[[i]]$coef[4]) * (1/ (a %*% vcov.ext6[[i]] %*% a)) ) +
     ( (ext7[[i]]$coef[2]+ext7[[i]]$coef[4]) * (1/ (a %*% vcov.ext7[[i]] %*% a)) ) +
     ( (ext8[[i]]$coef[2]+ext8[[i]]$coef[4]) * (1/ (a %*% vcov.ext8[[i]] %*% a)) ) 
     ) / (
       (1/ (a %*% vcov.ext1[[i]] %*% a))  +(1/ (a %*% vcov.ext2[[i]] %*% a))  +(1/ (a %*% vcov.ext3[[i]] %*% a)) +
         (1/ (a %*% vcov.ext4[[i]] %*% a)) +(1/ (a %*% vcov.ext5[[i]] %*% a)) +(1/ (a %*% vcov.ext6[[i]] %*% a)) +
         (1/ (a %*% vcov.ext7[[i]] %*% a))  +(1/ (a %*% vcov.ext8[[i]] %*% a)) ))
       
  var.ext.lg.urb.tert[[i]]<- 1 / ( (1/ (a %*% vcov.ext1[[i]] %*% a))  +(1/ (a %*% vcov.ext2[[i]] %*% a))+
    (1/ (a %*% vcov.ext3[[i]] %*% a)) + (1/ (a %*% vcov.ext4[[i]] %*% a)) +(1/ (a %*% vcov.ext5[[i]] %*% a)) +
    (1/ (a %*% vcov.ext6[[i]] %*% a)) + (1/ (a %*% vcov.ext7[[i]] %*% a))  +(1/ (a %*% vcov.ext8[[i]] %*% a))  )

#gives exponentiated results
expon.ext.lg.urb.tert[[i]]<-exp(ext.lg.urb.tert[[i]])
#gives 2-sided p values based on wald statistics
#z.urban[[i]]<-urbancat[[i]]/sqrt(var.urban[[i]])
#p.urban[[i]]<-2*pnorm(abs(z.urban[[i]]), lower.tail=FALSE)
}


ext.frg.urb.tert<-list(rep(NA, 100))
var.ext.frg.urb.tert<-list(rep(NA, 100))
expon.ext.frg.urb.tert<-list(rep(NA, 100))

## Effect for urban fringe subgroup
a <- c(0, 1, 0,0,1)
for(i in 1:100){
  ext.frg.urb.tert[[i]]<- ((  
     ( (ext1[[i]]$coef[2]+ext1[[i]]$coef[5]) * (1/ (a %*% vcov.ext1[[i]] %*% a)) ) + 
     ( (ext2[[i]]$coef[2]+ext2[[i]]$coef[5]) * (1/ (a %*% vcov.ext2[[i]] %*% a)) ) +
     ( (ext3[[i]]$coef[2]+ext3[[i]]$coef[5]) * (1/ (a %*% vcov.ext3[[i]] %*% a)) ) +
     ( (ext4[[i]]$coef[2]+ext4[[i]]$coef[5]) * (1/ (a %*% vcov.ext4[[i]] %*% a)) ) +
     ( (ext5[[i]]$coef[2]+ext5[[i]]$coef[5]) * (1/ (a %*% vcov.ext5[[i]] %*% a)) ) +
     ( (ext6[[i]]$coef[2]+ext6[[i]]$coef[5]) * (1/ (a %*% vcov.ext6[[i]] %*% a)) ) +
     ( (ext7[[i]]$coef[2]+ext7[[i]]$coef[5]) * (1/ (a %*% vcov.ext7[[i]] %*% a)) ) +
     ( (ext8[[i]]$coef[2]+ext8[[i]]$coef[5]) * (1/ (a %*% vcov.ext8[[i]] %*% a)) )
     ) / (
       (1/ (a %*% vcov.ext1[[i]] %*% a))  +(1/ (a %*% vcov.ext2[[i]] %*% a))  +(1/ (a %*% vcov.ext3[[i]] %*% a)) +
         (1/ (a %*% vcov.ext4[[i]] %*% a)) +(1/ (a %*% vcov.ext5[[i]] %*% a)) +(1/ (a %*% vcov.ext6[[i]] %*% a)) +
         (1/ (a %*% vcov.ext7[[i]] %*% a))  +(1/ (a %*% vcov.ext8[[i]] %*% a)) ))
       
  var.ext.frg.urb.tert[[i]]<- 1 / ( (1/ (a %*% vcov.ext1[[i]] %*% a))  +(1/ (a %*% vcov.ext2[[i]] %*% a))+
    (1/ (a %*% vcov.ext3[[i]] %*% a)) + (1/ (a %*% vcov.ext4[[i]] %*% a)) +(1/ (a %*% vcov.ext5[[i]] %*% a)) +
    (1/ (a %*% vcov.ext6[[i]] %*% a)) + (1/ (a %*% vcov.ext7[[i]] %*% a))  +(1/ (a %*% vcov.ext8[[i]] %*% a)) )

#gives exponentiated results
expon.ext.frg.urb.tert[[i]]<-exp(ext.frg.urb.tert[[i]])
}


rural.ext<-MIcombine(results=ext.effect, variances=ext.var)

#large urban
lg.urb.ext<-MIcombine(results=ext.lg.urb.tert, variances=var.ext.lg.urb.tert)

#fringe urban
frg.urb.ext<-MIcombine(results=ext.frg.urb.tert, variances=var.ext.frg.urb.tert)

mult.scorecore.ext<-MIcombine(results=scorecore.ext, variances=var.scorecore.ext)
mult.scorefringe.ext<-MIcombine(results=scorefringe.ext,  variances=var.scorefringe.ext)

rural.int.sum<-summary(rural.int)
lg.urb.int.sum<-summary(lg.urb.int)
frg.urb.int.sum<-summary(frg.urb.int)
rural.ext.sum<-summary(rural.ext)
lg.urb.ext.sum<-summary(lg.urb.ext)
frg.urb.ext.sum<-summary(frg.urb.ext)

mult.scorecore.int.sum<-summary(mult.scorecore.int)
mult.scorefringe.int.sum<-summary(mult.scorefringe.int)
mult.scorecore.ext.sum<-summary(mult.scorecore.ext)
mult.scorefringe.ext.sum<-summary(mult.scorefringe.ext)
mult.scorecore.int.sum
#   results        se    (lower   upper) missInfo
#1 1.020785 0.4284704 0.1731327 1.868437     87 %

mult.scorecore.ext.sum
mult.scorefringe.int.sum
mult.scorefringe.ext.sum

scoreOR<-list(rep(NA,6))
scoreupper<-list(rep(NA,6))
scorelower<-list(rep(NA,6))

scoreOR<-list( exp(rural.int.sum[[1]]), exp(frg.urb.int.sum[[1]]), exp(lg.urb.int.sum[[1]]), exp(rural.ext.sum[[1]]), exp(frg.urb.ext.sum[[1]]), exp(lg.urb.ext.sum[[1]]) )
scorelower<-list(exp(rural.int.sum[[3]]), exp(frg.urb.int.sum[[3]]),  exp(lg.urb.int.sum[[3]]), exp(rural.ext.sum[[3]]),
 exp(frg.urb.ext.sum[[3]]), exp(lg.urb.ext.sum[[3]]))
 scoreupper<-list(exp(rural.int.sum[[4]]), exp(frg.urb.int.sum[[4]]),  exp(lg.urb.int.sum[[4]]), exp(rural.ext.sum[[4]]),
 exp(frg.urb.ext.sum[[4]]), exp(lg.urb.ext.sum[[4]]))
 
tab3<-cbind(scoreOR, scorelower, scoreupper)
#> tab3
#     scoreOR   scorelower scoreupper
#[1,] 0.5634073 0.251009   1.264607  
#[2,] 1.009606  0.8097971  1.258716  
#[3,] 1.691866  1.344721   2.128626  
#[4,] 0.3217279 0.09361002 1.105745  
#[5,] 0.972466  0.6910846  1.368415  
#[6,] 1.141555  0.8453931  1.541468  

library(gplots)
#internalizing
postscript("figure1.eps", width = 30.0, height = 50.0, horizontal=FALSE)
pdf("figure1.june.measinv.pdf")
plotCI(x=c(-.1,.9, 1.9), y=c(rural.int.sum[[1]], frg.urb.int.sum[[1]], lg.urb.int.sum[[1]]), 
       ui=c(rural.int.sum[[4]], frg.urb.int.sum[[4]], lg.urb.int.sum[[4]]), 
       li=c(rural.int.sum[[3]], frg.urb.int.sum[[3]], lg.urb.int.sum[[3]]), lty=1, lwd=2, 
       err="y", col="blue", add=FALSE, xlim=c(-.5, 2.5), ylim=c(-6,1), xaxt="n", ylab="logOR", xlab="")
axis(1, at=c(-.5, .0, .5,1,1.5, 2, 2.5), labels=c("", "non-urban", "", "urban fringe", "", "urban center", ""), 
     lty=1,tck=0)
abline(h=0)


#externalizing
plotCI(x=c(.1,1.1, 2.1), y=c(rural.ext.sum[[1]], frg.urb.ext.sum[[1]], lg.urb.ext.sum[[1]]), 
       ui=c(rural.ext.sum[[4]], frg.urb.ext.sum[[4]], lg.urb.ext.sum[[4]]), 
       li=c(rural.ext.sum[[3]], frg.urb.ext.sum[[3]], lg.urb.ext.sum[[3]]), lty=1, lwd=2, 
       err="y", col="red", add=TRUE, xlim=c(-.5, 2.5), xaxt="n", ylab="logOR", xlab="")
legend("bottomright", bty="n",
     c("internalizing","externalizing"), fill=c("blue", "red"), horiz=FALSE)
dev.off()

#making love plot
szd <- function(covlist, g) { 
   covlist2 <- as.matrix(covlist) 
   g <- as.factor(g) 
   res <- NA 
   for(i in 1:ncol(covlist2)) { 
     cov <- as.numeric(covlist2[,i]) 
     num <- 100*diff(tapply(cov, g, mean, na.rm=TRUE)) 
     den <- sqrt(mean(tapply(cov, g, var, na.rm=TRUE))) 
     res[i] <- round(num/den,2) 
   } 
   names(res) <- names(covlist)    
   res 
 }

coefnam<-c("Distance", "Age", "Female", "Less than HS", "HS Grad", "Some College", "College Grad", "Lived w/Mother" ,"Lived w/Father", "Urb Core",  "Urb Fringe",
"2nd Gen Imm", "3rd Gen Imm", "Mat Age", "Mat Age^2", "Hispanic", 
"Black", "Other", "Midwest", "South", "West", 
"Log Income","Age*Fem","Less than HS*Fem", "HS Grad*Fem", "Some College*Fem", 
"Lived Moth*Fem" ,"Lived Fath*Fem", "Urb Core*Fem",  "Urb Fringe*Fem","2ndImm*Fem", "3rdImm*Fem", "Mat Age*Fem", "Mat Age^2*Fem", "Hispanic*Fem", "Black*Fem", "Other*Fem", "Midwest*Fem", "South*Fem", "West*Fem","Log Income*Fem")

smd.pre<-c(1.2791,-.0706, .0001,.1991,.2151,-.07, 
			-.6112, -.0994, -.4053, -.0921, .1458, 
			.1722, -.2506, -.4056, .2532, .6044, 
			-.0491, -1.0841, -.5571, .4808, -.0363,
			-.5517, -.0595, .1072, -.029, -.4211, 
			-.0511, -.2393, -.0484, .0830, .1127, 
			-.1126, -.3043, .1781, .3814, -.0328, 
			-.7267, -.386, .2873, 0.0272, -.3966)

smd.post<-c(.0083, .0223, .0581, .0278, .0259, .0665,
			.0884, .0510, .0170, .0570, .0396, 
			.0225, .0259, .0264, .0315, .0177, 
			.025, .0194, .0574, .0624, .0328
			.0166, .0589, .0298, .0173, .0988,
			.0655, .0478, .0415, .0333, .0182, 
			.0601, .0347, .0243, .0129, .0239, 
			.0869, .0523, .0329, .0356, .0302)

tab5<-data.frame(nam=coefnam, pre=smd.pre*100, post=smd.post*100)
tab.ord1<-tab5[order(tab5$pre, decreasing=TRUE),]

pdf("loveplot.pdf")

#postscript("loveplot.eps", width = 35.0, height = 24.0, horizontal=FALSE)
dotchart(tab.ord1$pre, pch="", labels=tab.ord1$nam, cex=0.75) 
mtext("Standardized Difference (%)", side=1, line=2)
points(tab.ord1$pre, seq(1:length(tab.ord1$pre)), 
pch=21, col="blue", cex=1.2) 
points(tab.ord1$post,seq(1:length(tab.ord1$post)), 
pch=16, col="red", cex=1.2) 
abline(v=0, lty=1) 
abline(v=10, lty=2, lwd=1) 
abline(v=-10, lty=2, lwd=1) 
legend("topright", legend = c("Pre-Subclassification", "Post-Subclassification"), col=c("blue", 
"red"), text.col=c("blue", "red")) 
dev.off()

 ##Unadjusted analysis
multimp<-imputationList(tmp)

library(survey)

desimp.ns<-svydesign(id=~secu, strata=~str, nest=TRUE, weight=~final_weight, data=multimp)

int.un<- with(desimp.ns, svyglm(internal ~ disnh*urbancat + suburb + disnh:suburb, family=quasibinomial()))
int.mult.un<-MIcombine(int.un)
int.mult.un.m<-as.matrix(summary(int.mult.un))

ext.un<- with(desimp.ns, svyglm(cp_CdOddh12_NIMH2 ~ disnh*urbancat + suburb + disnh:suburb, family=quasibinomial()))
ext.mult.un<-MIcombine(ext.un)
ext.mult.un.m<-as.matrix(summary(ext.mult.un))

int.ad<- with(desimp.ns, svyglm(internal ~ disnh*urbancat + suburb + disnh:suburb + age_cent + meducat + moth + fath +
  imgen + SEXF + (lninc:I(lninc>0)) + racecat + region + poly(mage,2), family=quasibinomial()))
int.mult.ad<-MIcombine(int.ad)
int.mult.ad.m<-as.matrix(summary(int.mult.ad))

ext.ad<- with(desimp.ns, svyglm(cp_CdOddh12_NIMH2 ~ disnh*urbancat + suburb + disnh:suburb + age_cent + meducat + moth + fath +
  imgen + SEXF + (lninc:I(lninc>0)) + racecat + region + poly(mage,2),  family=quasibinomial()))
ext.mult.ad<-MIcombine(ext.ad)
ext.mult.ad.m<-as.matrix(summary(ext.mult.ad))

a <- c(0, 1, 0,0,1,0)
int.lg.urb.tert.un <-(int.mult.un$coef[2]+int.mult.un$coef[5]) 
var.int.lg.urb.tert.un<- (a %*% int.mult.un$variance %*% a )

a <- c(0, 1, 0,0,0,1)
int.frg.urb.tert.un <-(int.mult.un$coef[2]+int.mult.un$coef[6]) 
var.int.frg.urb.tert.un<- (a %*% int.mult.un$variance %*% a )

a <- c(0, 1, 0,0,1,0)
ext.lg.urb.tert.un <-(ext.mult.un$coef[2]+ext.mult.un$coef[5]) 
var.ext.lg.urb.tert.un<- (a %*% ext.mult.un$variance %*% a )

a <- c(0, 1, 0,0,0,1)
ext.frg.urb.tert.un <-(ext.mult.un$coef[2]+ext.mult.un$coef[6]) 
var.ext.frg.urb.tert.un<- (a %*% ext.mult.un$variance %*% a )

a <- c(0, 1, 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
int.lg.urb.tert.ad <-(int.mult.ad$coef[2]+int.mult.ad$coef[15]) 
var.int.lg.urb.tert.ad<- (a %*% int.mult.ad$variance %*% a )

a <- c(0, 1, 0,0,0,0,0,0,0,0,0,0,0,0,0,1,0)
int.frg.urb.tert.ad <-(int.mult.ad$coef[2]+int.mult.ad$coef[16]) 
var.int.frg.urb.tert.ad<- (a %*% int.mult.ad$variance %*% a )

a <- c(0, 1, 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
ext.lg.urb.tert.ad <-(ext.mult.ad$coef[2]+ext.mult.ad$coef[15]) 
var.ext.lg.urb.tert.ad<- (a %*% ext.mult.ad$variance %*% a )

a <- c(0, 1, 0,0,0,0,0,0,0,0,0,0,0,0,0,1,0)
ext.frg.urb.tert.ad <-(ext.mult.ad$coef[2]+ext.mult.ad$coef[16]) 
var.ext.frg.urb.tert.ad<- (a %*% ext.mult.ad$variance %*% a )

un.or<-c(exp(int.mult.un$coef[2]), exp(int.frg.urb.tert.un), exp(int.lg.urb.tert.un), exp(ext.mult.un$coef[2]), 
         exp(ext.frg.urb.tert.un), exp(ext.lg.urb.tert.un) )
un.li<-c(exp(int.mult.un$coef[2] - 1.96*sqrt(int.mult.un$var[2,2])), exp(int.frg.urb.tert.un - 
  (1.96*sqrt(var.int.frg.urb.tert.un))), exp(int.lg.urb.tert.un - 1.96*sqrt(var.int.lg.urb.tert.un)),
         exp(ext.mult.un$coef[2] - 1.96*sqrt(ext.mult.un$var[2,2])), exp(ext.frg.urb.tert.un 
 -1.96*sqrt(var.ext.frg.urb.tert.un)), exp(ext.lg.urb.tert.un - 1.96*sqrt(var.ext.lg.urb.tert.un)) )
un.ui<-c(exp(int.mult.un$coef[2] + 1.96*sqrt(int.mult.un$var[2,2])), exp(int.frg.urb.tert.un + 
  (1.96*sqrt(var.int.frg.urb.tert.un))), exp(int.lg.urb.tert.un + 1.96*sqrt(var.int.lg.urb.tert.un)),
         exp(ext.mult.un$coef[2] + 1.96*sqrt(ext.mult.un$var[2,2])), exp(ext.frg.urb.tert.un 
 +1.96*sqrt(var.ext.frg.urb.tert.un)), exp(ext.lg.urb.tert.un + 1.96*sqrt(var.ext.lg.urb.tert.un)) )

ad.or<-c(exp(int.mult.ad$coef[2]), exp(int.frg.urb.tert.ad), exp(int.lg.urb.tert.ad), exp(ext.mult.ad$coef[2]), 
         exp(ext.frg.urb.tert.ad), exp(ext.lg.urb.tert.ad) )
ad.li<-c(exp(int.mult.ad$coef[2] - 1.96*sqrt(int.mult.ad$var[2,2])), exp(int.frg.urb.tert.ad - 
  (1.96*sqrt(var.int.frg.urb.tert.ad))), exp(int.lg.urb.tert.ad - 1.96*sqrt(var.int.lg.urb.tert.ad)),
         exp(ext.mult.ad$coef[2] - 1.96*sqrt(ext.mult.ad$var[2,2])), exp(ext.frg.urb.tert.ad
 -1.96*sqrt(var.ext.frg.urb.tert.ad)), exp(ext.lg.urb.tert.ad - 1.96*sqrt(var.ext.lg.urb.tert.ad)) )
un.ui<-c(exp(int.mult.ad$coef[2] + 1.96*sqrt(int.mult.ad$var[2,2])), exp(int.frg.urb.tert.ad + 
  (1.96*sqrt(var.int.frg.urb.tert.ad))), exp(int.lg.urb.tert.ad + 1.96*sqrt(var.int.lg.urb.tert.ad)),
         exp(ext.mult.ad$coef[2] + 1.96*sqrt(ext.mult.ad$var[2,2])), exp(ext.frg.urb.tert.ad 
 +1.96*sqrt(var.ext.frg.urb.tert.ad)), exp(ext.lg.urb.tert.ad + 1.96*sqrt(var.ext.lg.urb.tert.ad)) )

tab1<-cbind(un.or, un.li, un.ui)

tab2<-cbind(ad.or, ad.li, un.ui)
library(xtable)
print(xtable(tab1, type="latex"))

un.or<-c(exp(int.mult.un$coef[2]), exp(int.frg.urb.tert.un), exp(int.lg.urb.tert.un) )
##unadjusted, weighted results
#          un.or     un.li     un.ui
#disnh 0.9186385 0.7230648 0.9832942
#disnh 1.1200177 0.8142631 1.3535063
#disnh 1.6465015 1.2390819 1.7966553
#disnh 0.5413224 0.3387090 0.8831872
#disnh 0.9063146 0.6450112 1.2916701
#disnh 0.9081944 0.6057075 1.1509617

##adjusted, weighted results
#          ad.or     ad.li     un.ui
#disnh 0.7969825 0.6459726 0.9832942
#disnh 0.9711778 0.6968466 1.3535063
#disnh 1.4358269 1.1474649 1.7966553
#disnh 0.5018396 0.2851524 0.8831872
#disnh 0.8834688 0.6042697 1.2916701
#disnh 0.8106742 0.5709943 1.1509617

#the interaction between urban center*disnh is statistically significant for internalizing
#no significant interactions for externalizing

## Unadjusted and unweighted analysis
multimp<-imputationList(tmp)
int.unun<- with(multimp, glm(internal ~ disnh*urbancat + suburb + disnh:suburb, family=binomial))
int.mult.unun<-MIcombine(int.unun)
int.mult.unun.m<-as.data.frame(summary(int.mult.unun))

ext.unun<- with(multimp, glm(cp_CdOddh12_NIMH2 ~ disnh*urbancat + suburb + disnh:suburb, family=binomial))
ext.mult.unun<-MIcombine(ext.unun)
ext.mult.unun.m<-as.data.frame(summary(ext.mult.unun))

a <- c(0, 1, 0,0,1,0)
int.lg.urb.tert.unun <-(int.mult.unun$coef[2]+int.mult.unun$coef[5]) 
var.int.lg.urb.tert.unun<- (a %*% int.mult.unun$variance %*% a )

a <- c(0, 1, 0,0,0,1)
int.frg.urb.tert.unun <-(int.mult.unun$coef[2]+int.mult.unun$coef[6]) 
var.int.frg.urb.tert.unun<- (a %*% int.mult.unun$variance %*% a )

a <- c(0, 1, 0,0,1,0)
ext.lg.urb.tert.unun <-(ext.mult.unun$coef[2]+ext.mult.unun$coef[5]) 
var.ext.lg.urb.tert.unun<- (a %*% ext.mult.unun$variance %*% a )

a <- c(0, 1, 0,0,0,1)
ext.frg.urb.tert.unun <-(ext.mult.unun$coef[2]+ext.mult.unun$coef[6]) 
var.ext.frg.urb.tert.unun<- (a %*% ext.mult.unun$variance %*% a )

unun.or<-c(exp(int.mult.unun$coef[2]), exp(int.frg.urb.tert.unun), exp(int.lg.urb.tert.unun), exp(ext.mult.unun$coef[2]), 
         exp(ext.frg.urb.tert.unun), exp(ext.lg.urb.tert.unun) )
unun.li<-c(exp(int.mult.unun$coef[2] - 1.96*sqrt(int.mult.unun$var[2,2])), exp(int.frg.urb.tert.unun - 
  (1.96*sqrt(var.int.frg.urb.tert.unun))), exp(int.lg.urb.tert.unun - 1.96*sqrt(var.int.lg.urb.tert.unun)),
         exp(ext.mult.unun$coef[2] - 1.96*sqrt(ext.mult.unun$var[2,2])), exp(ext.frg.urb.tert.unun 
 -1.96*sqrt(var.ext.frg.urb.tert.unun)), exp(ext.lg.urb.tert.unun - 1.96*sqrt(var.ext.lg.urb.tert.unun)) )
unun.ui<-c(exp(int.mult.unun$coef[2] + 1.96*sqrt(int.mult.unun$var[2,2])), exp(int.frg.urb.tert.unun + 
  (1.96*sqrt(var.int.frg.urb.tert.unun))), exp(int.lg.urb.tert.unun + 1.96*sqrt(var.int.lg.urb.tert.unun)),
         exp(ext.mult.unun$coef[2] + 1.96*sqrt(ext.mult.unun$var[2,2])), exp(ext.frg.urb.tert.unun 
 +1.96*sqrt(var.ext.frg.urb.tert.unun)), exp(ext.lg.urb.tert.unun + 1.96*sqrt(var.ext.lg.urb.tert.unun)) )

tab1<-cbind(unun.or, unun.li, unun.ui)

#unadjusted, unweighted results
#            unun.or   unun.li  unun.ui
#tertscore 0.9007361 0.7364597 1.101656
#tertscore 1.1820858 1.0071459 1.387413
#tertscore 1.3920068 1.2046828 1.608459
#tertscore 0.7322791 0.5239396 1.023463
#tertscore 0.8855570 0.6841650 1.146231
#11tertscore 0.8367050 0.6385238 1.096396
