setwd("/Users/krudolph/Documents/PhD/NIMH/Ncsa")
nhood<-read.table("nhood.csv", header=TRUE, sep=",")
names(nhood)
temp1<-nhood
dat<-read.table("formice.csv", header=TRUE, sep=",")
keep<-c("Id2", "SampleID", "final_weight", "urbancat")
temp2<-dat[keep]
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
#[1] 3347 final Census tract sample
finsamp<-merge(drp2, temp2, by.x="Id2",by.y="Id2", all.x=TRUE, all.y=FALSE)
nrow(finsamp)
#10074
data<-merge(finsamp, dat, by.x="SampleID.y", by.y="SampleID", all.x=TRUE, all.y=FALSE)
nhoodtmp2<-drp2
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

nhoodtmp2$score<-(nhoodtmp2$zmedinc + nhoodtmp2$zmedval + nhoodtmp2$zperocc +
nhoodtmp2$zperhs + nhoodtmp2$zpercoll + nhoodtmp2$zperfanic)

summary(nhoodtmp2$score)

nhoodtmp2$scoreq<-cut(nhoodtmp2$score, breaks=quantile(nhoodtmp2$score, c(0, .2, .4, .6, .8, 1)))
summary(nhoodtmp2$scoreq)

nhoodtmp2$scoret<-cut(nhoodtmp2$score, breaks=quantile(nhoodtmp2$score, c(0, .33, .67, 1)))
summary(nhoodtmp2$scoret)

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



nhoodfact<-subset(nhood, select=c(perfaninc, loginc,logval,perocc, perhs, percoll, 
peruncrowd, pernonpov, peremp, pernonvac))
nhood1<-nhoodfact[is.finite(nhoodfact$loginc),]
nhood2<-nhood1[is.finite(nhood1$logval),]
is.finite(nhood1$loginc)
is.finite(nhood2$logval)
nhoodfacta<-factanal(nhood2, factors=1)
nhoodfacta
plot(prcomp(nhood2))
nhood3<-subset(nhood2, select=c(1:9))
names(nhood3)
nhoodfacta<-factanal(nhood3, factors=1)
nhood4<-subset(nhood2, select=c(perfaninc, perhs, percoll, perocc, loginc, logval))
nhoodfactb<-factanal(nhood4, factors=1)
nhoodfactb
plot(prcomp(nhood4))
nhood5<-subset(nhood2, select=c(perfaninc, perhs, percoll, perocc, loginc, logval, pernonpov,
peremp))
nhoodfactc<-factanal(nhood5, factors=1)
nhoodfactc
nhood$zmedinc<-scale(nhood$loginc)
nhood$zmedval<-scale(nhood$logval)
nhood2$zperocc<-scale(nhood2$perocc)
nhood2$zperhs<-scale(nhood2$perhs)
nhood2$zpercoll<-scale(nhood2$percoll)
nhood2$zperuncrowd<-scale(nhood2$peruncrowd)
nhood2$zpernonpov<-scale(nhood2$pernonpov)
nhood2$zperemp<-scale(nhood2$peremp)
nhood2$zpernonvac<-scale(nhood2$pernonvac)

nhood$inctemp1<-nhood$medinc[nhood$medinc!=0]
nhood$loginc<-log(nhood$inctemp1)
nhood$zloginc<-scale(nhood$loginc)
nhood$valtemp1<-nhood$medval[nhood$medval!=0]
nhood$logval<-log(nhood$valtemp1)
nhood$zlogval<-scale(nhood$logval)


loginc<-function(medinc){if (medinc!=0) {log(medinc)} else{}}
summary(loginc)

