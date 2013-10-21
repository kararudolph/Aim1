setwd("/Users/krudolph/Documents/PhD/NIMH/Ncsa")
library(mitools)
library(survey)
library(mice)
library(MatchIt)

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

int1[[i]]<-summary(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==1)))
int2[[i]]<-summary(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==2)))
int3[[i]]<-summary(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==3)))
int4[[i]]<-summary(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==4)))
int5[[i]]<-summary(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==5)))
int6[[i]]<-summary(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==6)))
int7[[i]]<-summary(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==7)))
int8[[i]]<-summary(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==8)))
int9[[i]]<-summary(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==9)))

ext1[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==1)))
ext2[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==2)))
ext3[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==3)))
ext4[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==4)))
ext5[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==5)))
ext6[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==6)))
ext7[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==7)))
ext8[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==8)))
ext9[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==9)))

vcov.int1[[i]]<-vcov(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==1)))
vcov.int2[[i]]<-vcov(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==2)))
vcov.int3[[i]]<-vcov(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==3)))
vcov.int4[[i]]<-vcov(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==4)))
vcov.int5[[i]]<-vcov(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==5)))
vcov.int6[[i]]<-vcov(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==6)))
vcov.int7[[i]]<-vcov(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==7)))
vcov.int8[[i]]<-vcov(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==8)))
vcov.int9[[i]]<-vcov(svyglm(internal ~ tertscore +  distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==9)))


vcov.ext1[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==1)))
vcov.ext2[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==2)))
vcov.ext3[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==3)))
vcov.ext4[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==4)))
vcov.ext5[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==5)))
vcov.ext6[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==6)))
vcov.ext7[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==7)))
vcov.ext8[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==8)))
vcov.ext9[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore +  distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==9)))

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


#combining across datasets
library(mitools)

int<-MIcombine(results=effect, 
                            variances=var)

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
                  + (ext9[[i]]$coef[2]*(1/ext9[[i]]$coef[2,2]^2)) ) / ( (1/ext1[[i]]$coef[2,2]^2) +
                    (1/ext2[[i]]$coef[2,2]^2)  + (1/ext3[[i]]$coef[2,2]^2) + (1/ext4[[i]]$coef[2,2]^2) + (1/ext5[[i]]$coef[2,2]^2)
                    + (1/ext6[[i]]$coef[2,2]^2) +(1/ext7[[i]]$coef[2,2]^2) +(1/ext8[[i]]$coef[2,2]^2) + (1/ext9[[i]]$coef[2,2]^2) ) )

  ext.var[[i]]<- 1 / ((1/ext1[[i]]$coef[2,2]^2) + (1/ext2[[i]]$coef[2,2]^2)  + (1/ext3[[i]]$coef[2,2]^2) + (1/ext4[[i]]$coef[2,2]^2) + (1/ext5[[i]]$coef[2,2]^2)
                    + (1/ext6[[i]]$coef[2,2]^2) +(1/ext7[[i]]$coef[2,2]^2) +(1/ext8[[i]]$coef[2,2]^2) + (1/ext9[[i]]$coef[2,2]^2))

#gives exponentiated results
ext.expon[[i]]<-exp(ext.effect[[i]])
#gives 2-sided p values based on wald statistics
ext.z[[i]]<-ext.effect[[i]]/sqrt(ext.var[[i]])
ext.p[[i]]<-2*pnorm(abs(ext.z[[i]]), lower.tail=FALSE)

}

ext<-MIcombine(results=ext.effect, variances=ext.var) 

int.sum<-summary(int)
#    results         se      (lower    upper) missInfo
#1 0.1630248 0.09036655 -0.01409859 0.3401482      6 %
ext.sum<-summary(ext)
#     results        se     (lower    upper) missInfo
#1 -0.04202432 0.1433374 -0.3231071 0.2390584     21 %


## effect urbanicity on disorder adjusting for tertscore
## neither are statistically significant
## Adjusted and weighted analysis
load("impdata100.Rdata")
tmp<-list(rep(NA,100))

for(i in 1:100){
  tmp[[i]]<-complete(imp, action=i, include=FALSE)
}
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

load("new.prpdat.incl.Rdata")

folks<-new.prpdat.incl[[1]]["SampleID"]
aos<-list(rep(NA,100))
for(i in 1:100){
	aos[[i]]<-merge(tmp[[i]], folks, all=FALSE, by="SampleID")
}
multimp<-imputationList(aos)
desimp.ns<-svydesign(id=~secu, strata=~str, nest=TRUE, weight=~final_weight, data=multimp)

int.ad<- with(desimp.ns, svyglm(internal ~  tertscore+ urbancat + suburb + age_cent + SEXF + factor(meducat) + 
   moth +  fath + factor(imgen) + poly(cmage, 2) + factor(racecat) +  factor(region) + cinc:nonzeroinc, family=quasibinomial()))
int.mult.ad<-MIcombine(int.ad)
int.mult.ad.m<-as.matrix(summary(int.mult.ad))
#                     results         se        (lower      upper)
#(Intercept)      -1.42402861 0.29976680  -2.011582066 -0.83647516
#tertscore         0.18284765 0.09567721  -0.004676976  0.37037227
#urbancat          0.21994763 0.13670247  -0.047984287  0.48787954
#suburb            0.18551822 0.14280232  -0.094369186  0.46540563
#age_cent          0.04165359 0.02552965  -0.008383595  0.09169078
#SEXF              0.69370864 0.07801989   0.540792464  0.84662482
#factor(meducat)1 -0.06253778 0.16284997  -0.382107282  0.25703172
#factor(meducat)2 -0.06468627 0.17927336  -0.416488407  0.28711586
#factor(meducat)3 -0.05597547 0.18422027  -0.417430963  0.30548003
#moth             -0.35945970 0.10618276  -0.567574692 -0.15134471
#fath             -0.27231216 0.07678373  -0.422805537 -0.12181879
#factor(imgen)2    0.27098947 0.25244132  -0.223786559  0.76576550
#factor(imgen)3    0.34392186 0.25104258  -0.148112761  0.83595648
#poly(cmage, 2)1  -8.73048392 4.71057213 -17.963085173  0.50211733
#poly(cmage, 2)2  10.73534999 3.21130296   4.441220114 17.02947987
#factor(racecat)1 -0.11716461 0.13435409  -0.380493824  0.14616460
#factor(racecat)2 -0.15818683 0.08674004  -0.328194310  0.01182065
#factor(racecat)3 -0.15704027 0.10811281  -0.368937588  0.05485705
#factor(region)2  -0.05925451 0.10053142  -0.256292471  0.13778346
#factor(region)3  -0.18714261 0.11030046  -0.403327541  0.02904233
#factor(region)4   0.13524515 0.12063247  -0.101190151  0.37168044
#cinc:nonzeroinc   0.02184782 0.04015194  -0.056848558  0.10054419

ext.ad<- with(desimp.ns, svyglm(cp_CdOddh12_NIMH2 ~  tertscore+ urbancat + suburb + age_cent + SEXF + factor(meducat) + 
   moth +  fath + factor(imgen) + poly(cmage, 2) + factor(racecat) +  factor(region) + cinc:nonzeroinc , family=quasibinomial()))
ext.mult.ad<-MIcombine(ext.ad)
ext.mult.ad.m<-as.matrix(summary(ext.mult.ad))
#                       results         se       (lower      upper)
#(Intercept)       -1.641579310 0.47517779  -2.57291385 -0.71024477
#tertscore         -0.190228904 0.13618347  -0.45714443  0.07668662
#urbancat           0.073772446 0.17125666  -0.26188460  0.40942950
#suburb             0.240888480 0.16075709  -0.07418972  0.55596668
#age_cent          -0.006162585 0.03860834  -0.08183359  0.06950842
#SEXF              -0.273463935 0.11341764  -0.49575848 -0.05116939
#factor(meducat)1  -0.948726252 0.22721262  -1.39413063 -0.50332188
#factor(meducat)2  -0.146206308 0.23176437  -0.60053041  0.30811780
#factor(meducat)3  -0.509532286 0.24216199  -0.98422387 -0.03484070
#moth              -0.661709907 0.16034527  -0.97598138 -0.34743843
#fath              -0.774087125 0.13832916  -1.04520742 -0.50296683
#factor(imgen)2     0.461308643 0.41801188  -0.35797989  1.28059718
#factor(imgen)3     0.698055883 0.43654702  -0.15756070  1.55367247
#poly(cmage, 2)1  -10.826293582 5.90722779 -22.40454860  0.75196143
#poly(cmage, 2)2    6.944297767 4.95953674  -2.77662294 16.66521847
#factor(racecat)1  -0.405611127 0.26414243  -0.92332097  0.11209872
#factor(racecat)2  -0.340457842 0.27620577  -0.88181148  0.20089579
#factor(racecat)3   0.154526207 0.25495990  -0.34518620  0.65423861
#factor(region)2    0.127263088 0.17930324  -0.22416493  0.47869110
#factor(region)3   -0.478844356 0.19456110  -0.86017721 -0.09751151
#factor(region)4    0.006599563 0.18061070  -0.34739105  0.36059017
#cinc:nonzeroinc   -0.085436778 0.06448551  -0.21182615  0.04095260
