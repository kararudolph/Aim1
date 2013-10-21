setwd("/Users/krudolph/Documents/PhD/NIMH/Ncsa")
nhood<-read.table("nhood.csv", header=TRUE, sep=",")
names(nhood)
temp1<-nhood
dat<-read.table("formice.csv", header=TRUE, sep=",")
keep<-c("Id2", "SampleID", "final_weight", "urbancat", "suburb")
temp2<-dat[keep]
temp2$urbanicity[temp2$urbancat==1]<-2
temp2$urbanicity[temp2$suburb==1]<-1
temp2$urbanicity[temp2$urbancat==0 & temp2$suburb==0]<-0

tmp<-merge(temp1, temp2, by.x="Id2",by.y="Id2", all.x=FALSE, all.y=FALSE)
#3367 census tracts in weighted sample
dattmp<-tmp[ which(tmp$medval>0),] 
dattmp2<-dattmp[!is.na(tmp$medinc),]
#3349 in final sample
x<-dattmp2$Id2
y<-nhoodtmp2$Id2
setdiff(x,y)
summary(nhood$medval)
summary(nhood$medinc)
nhoodtmp<-nhood[ which(nhood$medval>0),] 
nhoodtmp2<-nhoodtmp[!is.na(nhoodtmp$medinc),]
nrow(nhood)
#[1] 3372
nrow(nhoodtmp2)
#[1] 3353
drp2<-nhoodtmp2[which(nhoodtmp2$Id2!=12105014502 & nhoodtmp2$Id2!= 13297110501 & nhoodtmp2$Id2!=17031251400 & nhoodtmp2$Id2!= 17031803400 & nhoodtmp2$Id2!=18003002300 & nhoodtmp2$Id2!=26163582100),]
nrow(drp2)
#[1] 3347 final Census tract sample in full sample
finsamp<-merge(drp2, temp2, by="Id2", all.x=TRUE, all.y=FALSE)
nrow(finsamp)
#10074
keep<-c("Id2", "SampleID.y", "final_weight", "urbanicity")
a<-finsamp[keep]
nh2<-merge(a, drp2, by.x=c("SampleID.y", "Id2"), by.y=c("SampleID", "Id2"), all=FALSE)
#3340 final Census tract sample in unweighted sample
data<-merge(finsamp, dat, by.x="SampleID.y", by.y="SampleID", all.x=TRUE, all.y=FALSE)

nhoodtmp<-nh2
nhoodtmp2<-nhoodtmp[!is.na(nhoodtmp$medinc),]
nhoodtmp2$logval<-log(nhoodtmp2$medval)
summary(nhoodtmp2$logval)
nhoodtmp2$loginc<-log(nhoodtmp2$medinc)
summary(nhoodtmp2$loginc)
summary(nhoodtmp2$perhs)
summary(nhoodtmp2$percoll)
summary(nhoodtmp2$perocc)
summary(nhoodtmp2$perfaninc)

nhoodtmp2$zmedinc<-scale(nhoodtmp2$loginc)
nhoodtmp2$zmedval<-scale(nhoodtmp2$logval)
nhoodtmp2$zperocc<-scale(nhoodtmp2$perocc)
nhoodtmp2$zperhs<-scale(nhoodtmp2$perhs)
nhoodtmp2$zpercoll<-scale(nhoodtmp2$percoll)
nhoodtmp2$zperfanic<-scale(nhoodtmp2$perfaninc)


library(MplusAutomation)
n3<-nhoodtmp2
n3$zmedinc<-as.vector(n3$zmedinc)
n3$zmedval<-as.vector(n3$zmedval)
n3$zperocc<-as.vector(n3$zperocc)
n3$zperhs<-as.vector(n3$zperhs)
n3$zpercoll<-as.vector(n3$zpercoll)
n3$zperfanic<-as.vector(n3$zperfanic)
keep<-c("urbanicity", "zmedinc", "zmedval", "zperocc", "zperhs", "zpercoll", "zperfanic", "Id2")
n4<-n3[keep]
prepareMplusData(n4, "n4.dat")
#analysis of measurement invariance was done in mplus. there was measurement variance across urbanicity levels. the only 
# way to get statistical measurement invariance was to fit a completely unconstrained model.

n3$scale[n3$urbanicity==0]<- ((0.434*n3$zmedinc[n3$urbanicity==0]) + (0.503*n3$zmedval[n3$urbanicity==0]) + (0.353*n3$zperocc[n3$urbanicity==0]) 
                              + (0.590*n3$zperhs[n3$urbanicity==0]) + (0.35*n3$zpercoll[n3$urbanicity==0]) +  (0.59*n3$zperfanic[n3$urbanicity==0]))

n3$scale[n3$urbanicity==1]<- ((0.766*n3$zmedinc[n3$urbanicity==1]) + (0.672*n3$zmedval[n3$urbanicity==1]) + (0.838*n3$zperocc[n3$urbanicity==1]) 
                              + (0.726*n3$zperhs[n3$urbanicity==1]) + (0.819*n3$zpercoll[n3$urbanicity==1]) +  (0.820*n3$zperfanic[n3$urbanicity==1]))

n3$scale[n3$urbanicity==2]<- ((0.832*n3$zmedinc[n3$urbanicity==2]) + (0.643*n3$zmedval[n3$urbanicity==2]) + (1.056*n3$zperocc[n3$urbanicity==2]) 
                              + (0.884*n3$zperhs[n3$urbanicity==2]) + (1.029*n3$zpercoll[n3$urbanicity==2]) +  (0.916*n3$zperfanic[n3$urbanicity==2]))

n3$scalet<-cut(n3$scale, breaks=quantile(n3$scale, c(0,.33,.67,1)))
n3$urb.nhdef<-n3$urbanicity
keep<-c("Id2", "urb.nhdef", "scale")
n4<-n3[keep]
nh<-merge(n4, finsamp, by="Id2", all=FALSE)
#final sample size is 10,058
save(nh, file="nh.Rdata")

nh$disnh<-ifelse(nh$scale< (-2.07),1,0)
keep<-c("Id2", "urb.nhdef", "scale", "disnh", "SampleID.y")
nh1<-nh[keep]
save(nh1, file="nh1.Rdata")
ex<-merge(imp1, nh1, by.x=c("SampleID", "Id2"), by.y=c("SampleID.y", "Id2"), all=FALSE)
agree<-ifelse(ex$tertscore==ex$disnh, 1, 0)

agree
#   0    1 
# 590 9468 



n3$scale_fs[n3$urbanicity==0]<- ((0.199*n3$zmedinc[n3$urbanicity==0]) + (0.355*n3$zmedval[n3$urbanicity==0]) + (0.156*n3$zperocc[n3$urbanicity==0]) 
                              + (0.553*n3$zperhs[n3$urbanicity==0]) + (0.143*n3$zpercoll[n3$urbanicity==0]) +  (0.385*n3$zperfanic[n3$urbanicity==0]))

n3$scale_fs[n3$urbanicity==1]<- ((0.049*n3$zmedinc[n3$urbanicity==1]) + (0.025*n3$zmedval[n3$urbanicity==1]) + (0.504*n3$zperocc[n3$urbanicity==1]) 
                              + (0.140*n3$zperhs[n3$urbanicity==1]) + (0.238*n3$zpercoll[n3$urbanicity==1]) +  (0.217*n3$zperfanic[n3$urbanicity==1]))

n3$scale_fs[n3$urbanicity==2]<- ((0.832*n3$zmedinc[n3$urbanicity==2]) + (0.643*n3$zmedval[n3$urbanicity==2]) + (1.056*n3$zperocc[n3$urbanicity==2]) 
                              + (0.050*n3$zperhs[n3$urbanicity==2]) + (0.274*n3$zpercoll[n3$urbanicity==2]) +  (0.067*n3$zperfanic[n3$urbanicity==2]))

n3$scalet_fs<-cut(n3$scale_fs, breaks=quantile(n3$scale_fs, c(0,.33,.67,1)))
n3$urb.nhdef<-n3$urbanicity
keep<-c("Id2", "urb.nhdef", "scale_fs")
n4<-n3[keep]
nh_fs<-merge(n4, finsamp, by="Id2", all=FALSE)
#final sample size is 10,058
save(nh_fs, file="nh_fs.Rdata")

nh_fs$disnh<-ifelse(nh_fs$scale_fs< (-0.78),1,0)
keep<-c("Id2", "urb.nhdef", "scale_fs", "disnh", "SampleID.y")
nh1_fs<-nh_fs[keep]
save(nh1_fs, file="nh1_fs.Rdata")

imp1$tertscore<-ifelse(imp1$score < (-2.293536), 1, 0)

ex_fs<-merge(imp1, nh1_fs, by.x=c("SampleID", "Id2"), by.y=c("SampleID.y", "Id2"), all=FALSE)
agree<-ifelse(ex_fs$tertscore==ex_fs$disnh, 1, 0)

agree
#   0    1 
# 718 9340 

library(foreign)
dat<- read.table("fscores_2.csv", sep="")

dat$scalet<-cut(dat$V8, breaks=quantile(dat$V8, c(0,.33,.67,1)))
dat$urb.nhdef<-dat$V9
dat$Id2<-dat$V7
dat$scale_fs<-dat$V8
keep<-c("Id2", "urb.nhdef", "scale_fs")
n4<-dat[keep]
nh_fs<-merge(n4, finsamp, by="Id2", all=FALSE)
#final sample size is 10,058
save(nh_fs, file="nh_fs_mplussc.Rdata")

nh_fs$disnh<-ifelse(nh_fs$scale_fs< (0.606),1,0)
keep<-c("Id2", "urb.nhdef", "scale_fs", "disnh", "SampleID.y")
nh1_fs<-nh_fs[keep]
save(nh1_fs, file="nh1_fs.Rdata")

imp1$tertscore<-ifelse(imp1$score < (-2.293536), 1, 0)

ex_fs<-merge(imp1, nh1_fs, by.x=c("SampleID", "Id2"), by.y=c("SampleID.y", "Id2"), all=FALSE)
agree<-ifelse(ex_fs$tertscore==ex_fs$disnh, 1, 0)

agree
#   0    1 
# 648 9410 


v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors=3) # varimax is the default
factanal(m1, factors=3, rotation="promax")
# The following shows the g factor as PC1

f1<-factanal(~zmedinc + zmedval + zperocc + zperhs + zpercoll + zperfanic, factors = 1, data=n4, 
         scores = c("regression"))

library("psych")
describe.by(nhoodtmp2$medval, nhoodtmp2$scoreq)
describe.by(nhoodtmp2$medinc, nhoodtmp2$scoreq)
describe.by(nhoodtmp2$perfaninc, nhoodtmp2$scoreq)
describe.by(nhoodtmp2$perpov, nhoodtmp2$scoreq)
describe.by(nhoodtmp2$perhs, nhoodtmp2$scoreq)
describe.by(nhoodtmp2$percoll, nhoodtmp2$scoreq)
describe.by(nhoodtmp2$perunempl, nhoodtmp2$scoreq)
describe.by(nhoodtmp2$perocc, nhoodtmp2$scoreq)
describe.by(nhoodtmp2$perlabforc, nhoodtmp2$scoreq)
describe.by(nhoodtmp2$pervac, nhoodtmp2$scoreq)

summary(nhoodtmp2$score)
sd(nhoodtmp2$score)

pdf("Neighborhood SES Index")
hist(nhoodtmp2$score)
dev.off()
names(nhoodtmp2)
myvars<-c("Id2", "score")

nscoremerge<-nhoodtmp2[myvars]
names(nscoremerge)

write.csv(nscoremerge, file="nscoremerge.csv", row.names=FALSE)

nhood6<-subset(nhoodtmp2, select=c(perfaninc, perhs, percoll, perocc, loginc, logval))
nhoodfact<-factanal(nhood6, factors=1)
nhoodfact
library("multilevel")
cronbach(nhood6)
pdf("PC Neighborhood SES Index")
plot(prcomp(nhood6))
dev.off()


