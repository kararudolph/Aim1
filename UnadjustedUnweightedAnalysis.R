 ##Unadjusted analysis
load("impdata100.Rdata")
tmp<-list(rep(NA,100))

for(i in 1:100){
  tmp[[i]]<-complete(imp, action=i, include=FALSE)
}

for(i in 1:100){
tmp[[i]]$tertscore<-ifelse(tmp[[i]]$score < (-2.293536), 1, 0)}
tmp[[i]]$nonzero<-ifelse(tmp[[i]]$lninc > 0, 1, 0)
}

multimp<-imputationList(tmp)

library(survey)

desimp.ns<-svydesign(id=~secu, strata=~str, nest=TRUE, weight=~final_weight, data=multimp)

int.un<- with(desimp.ns, svyglm(internal ~ tertscore*urbancat + suburb + tertscore:suburb, family=quasibinomial()))
int.mult.un<-MIcombine(int.un)
int.mult.un.m<-as.matrix(summary(int.mult.un))

ext.un<- with(desimp.ns, svyglm(cp_CdOddh12_NIMH2 ~ tertscore*urbancat + suburb + tertscore:suburb, family=quasibinomial()))
ext.mult.un<-MIcombine(ext.un)
ext.mult.un.m<-as.matrix(summary(ext.mult.un))

int.ad<- with(desimp.ns, svyglm(internal ~ tertscore*urbancat + suburb + tertscore:suburb + age_cent + meducat + moth + fath +
  imgen + SEXF + (lninc:I(lninc>0)) + racecat + region + poly(mage,2), family=quasibinomial()))
int.mult.ad<-MIcombine(int.ad)
int.mult.ad.m<-as.matrix(summary(int.mult.ad))

ext.ad<- with(desimp.ns, svyglm(cp_CdOddh12_NIMH2 ~ tertscore*urbancat + suburb + tertscore:suburb + age_cent + meducat + moth + fath +
  imgen + SEXF + (lninc:I(lninc>0)) + racecat + region + poly(mage,2),  family=quasibinomial()))
ext.mult.ad<-MIcombine(ext.ad)
ext.mult.ad.m<-as.matrix(summary(ext.mult.ad))

a <- c(0, 1, 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
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

print(xtable(tab1, type="latex"))

un.or<-c(exp(int.mult.un$coef[2]), exp(int.frg.urb.tert.un), exp(int.lg.urb.tert.un) )

## Unadjusted and unweighted analysis
multimp<-imputationList(tmp)
int.unun<- with(multimp, glm(internal ~ tertscore*urbancat + suburb + tertscore:suburb, family=binomial))
int.mult.unun<-MIcombine(int.unun)
int.mult.unun.m<-as.data.frame(summary(int.mult.unun))

ext.unun<- with(multimp, glm(cp_CdOddh12_NIMH2 ~ tertscore*urbancat + suburb + tertscore:suburb, family=binomial))
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

desimp.ns<-svydesign(id=~secu, strata=~str, nest=TRUE, weight=~final_weight, data=multimp)

## Unadjusted and weighted analysis
int.un.wt<- with(desimp.ns, svyglm(internal ~ tertscore*urbancat + suburb + tertscore:suburb, family=quasibinomial()))
int.mult.un.wt<-MIcombine(int.un.wt)
int.mult.un.wt.m<-as.data.frame(summary(int.mult.un.wt))

ext.un.wt<- with(desimp.ns, svyglm(cp_CdOddh12_NIMH2 ~ tertscore*urbancat + suburb + tertscore:suburb, family=quasibinomial()))
ext.mult.un.wt<-MIcombine(ext.un.wt)
ext.mult.un.wt.m<-as.data.frame(summary(ext.mult.un.wt))

a <- c(0, 1, 0,0,1,0)
int.lg.urb.tert.un.wt <-(int.mult.un.wt$coef[2]+int.mult.un.wt$coef[5]) 
var.int.lg.urb.tert.un.wt<- (a %*% int.mult.un.wt$variance %*% a )

a <- c(0, 1, 0,0,0,1)
int.frg.urb.tert.un.wt <-(int.mult.un.wt$coef[2]+int.mult.un.wt$coef[6]) 
var.int.frg.urb.tert.un.wt<- (a %*% int.mult.un.wt$variance %*% a )

a <- c(0, 1, 0,0,1,0)
ext.lg.urb.tert.un.wt <-(ext.mult.un.wt$coef[2]+ext.mult.un.wt$coef[5]) 
var.ext.lg.urb.tert.un.wt<- (a %*% ext.mult.un.wt$variance %*% a )

a <- c(0, 1, 0,0,0,1)
ext.frg.urb.tert.un.wt <-(ext.mult.un.wt$coef[2]+ext.mult.un.wt$coef[6]) 
var.ext.frg.urb.tert.un.wt<- (a %*% ext.mult.un.wt$variance %*% a )

un.wt.or<-c(exp(int.mult.un.wt$coef[2]), exp(int.frg.urb.tert.un.wt), exp(int.lg.urb.tert.un.wt), exp(ext.mult.un.wt$coef[2]), 
         exp(ext.frg.urb.tert.un.wt), exp(ext.lg.urb.tert.un.wt) )
un.wt.li<-c(exp(int.mult.un.wt$coef[2] - 1.96*sqrt(int.mult.un.wt$var[2,2])), exp(int.frg.urb.tert.un.wt - 
  (1.96*sqrt(var.int.frg.urb.tert.un.wt))), exp(int.lg.urb.tert.un.wt - 1.96*sqrt(var.int.lg.urb.tert.un.wt)),
         exp(ext.mult.un.wt$coef[2] - 1.96*sqrt(ext.mult.un.wt$var[2,2])), exp(ext.frg.urb.tert.un.wt 
 -1.96*sqrt(var.ext.frg.urb.tert.un.wt)), exp(ext.lg.urb.tert.un.wt - 1.96*sqrt(var.ext.lg.urb.tert.un.wt)) )
un.wt.ui<-c(exp(int.mult.un.wt$coef[2] + 1.96*sqrt(int.mult.un.wt$var[2,2])), exp(int.frg.urb.tert.un.wt + 
  (1.96*sqrt(var.int.frg.urb.tert.un.wt))), exp(int.lg.urb.tert.un.wt + 1.96*sqrt(var.int.lg.urb.tert.un.wt)),
         exp(ext.mult.un.wt$coef[2] + 1.96*sqrt(ext.mult.un.wt$var[2,2])), exp(ext.frg.urb.tert.un.wt 
 +1.96*sqrt(var.ext.frg.urb.tert.un.wt)), exp(ext.lg.urb.tert.un.wt + 1.96*sqrt(var.ext.lg.urb.tert.un.wt)) )

tab1<-cbind(un.wt.or, un.wt.li, un.wt.ui)
#           un.wt.or  un.wt.li un.wt.ui
#tertscore 1.1020730 0.7842315 1.548733
#tertscore 1.1030982 0.8158292 1.491520
#tertscore 1.6970834 1.2331115 2.335630
#tertscore 0.6802144 0.4044661 1.143956
#tertscore 0.8549686 0.6125776 1.193271
#tertscore 0.8919872 0.6007630 1.324384

## Adjusted and weighted analysis
int.ad<- with(desimp.ns, svyglm(internal ~  tertscore*urbancat + suburb + tertscore:suburb + age_cent*SEXF + factor(meducat) + factor(meducat):SEXF + moth + moth:SEXF + fath + fath:SEXF + urbancat:SEXF + suburb:SEXF + factor(imgen) + factor(imgen):SEXF + poly(cmage, 2) + poly(cmage,2):SEXF + factor(racecat) +  factor(racecat):SEXF + factor(region) + factor(region):SEXF + cinc:nonzeroinc + cinc:nonzeroinc:SEXF , family=quasibinomial()))
int.mult.ad<-MIcombine(int.ad)
int.mult.ad.m<-as.matrix(summary(int.mult.ad))

ext.ad<- with(desimp.ns, svyglm(cp_CdOddh12_NIMH2 ~  tertscore*urbancat + suburb + tertscore:suburb + age_cent*SEXF + factor(meducat) + factor(meducat):SEXF + moth + moth:SEXF + fath + fath:SEXF + urbancat:SEXF + suburb:SEXF + factor(imgen) + factor(imgen):SEXF + poly(cmage, 2) + poly(cmage,2):SEXF + factor(racecat) +  factor(racecat):SEXF + factor(region) + factor(region):SEXF + cinc:nonzeroinc + cinc:nonzeroinc:SEXF , family=quasibinomial()))
ext.mult.ad<-MIcombine(ext.ad)
ext.mult.ad.m<-as.matrix(summary(ext.mult.ad))

a <- c(0,1,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,1,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,0,0)
int.lg.urb.tert.ad <-(int.mult.ad$coef[2]+int.mult.ad$coef[22]) 
var.int.lg.urb.tert.ad<- (a %*% int.mult.ad$variance %*% a )

a <- c(0,1,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,0,1,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,0,0)
int.frg.urb.tert.ad <-(int.mult.ad$coef[2]+int.mult.ad$coef[23]) 
var.int.frg.urb.tert.ad<- (a %*% int.mult.ad$variance %*% a )

a <- c(0,1,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,1,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,0,0)

ext.lg.urb.tert.ad <-(ext.mult.ad$coef[2]+ext.mult.ad$coef[22]) 
var.ext.lg.urb.tert.ad<- (a %*% ext.mult.ad$variance %*% a )

a <- c(0,1,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,0,1,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,0,0)
ext.frg.urb.tert.ad <-(ext.mult.ad$coef[2]+ext.mult.ad$coef[23]) 
var.ext.frg.urb.tert.ad<- (a %*% ext.mult.ad$variance %*% a )

ad.or<-c(exp(int.mult.ad$coef[2]), exp(int.frg.urb.tert.ad), exp(int.lg.urb.tert.ad), exp(ext.mult.ad$coef[2]), 
         exp(ext.frg.urb.tert.ad), exp(ext.lg.urb.tert.ad) )
ad.li<-c(exp(int.mult.ad$coef[2] - 1.96*sqrt(int.mult.ad$var[2,2])), exp(int.frg.urb.tert.ad - 
  (1.96*sqrt(var.int.frg.urb.tert.ad))), exp(int.lg.urb.tert.ad - 1.96*sqrt(var.int.lg.urb.tert.ad)),
         exp(ext.mult.ad$coef[2] - 1.96*sqrt(ext.mult.ad$var[2,2])), exp(ext.frg.urb.tert.ad 
 -1.96*sqrt(var.ext.frg.urb.tert.ad)), exp(ext.lg.urb.tert.ad - 1.96*sqrt(var.ext.lg.urb.tert.ad)) )
ad.ui<-c(exp(int.mult.ad$coef[2] + 1.96*sqrt(int.mult.ad$var[2,2])), exp(int.frg.urb.tert.ad + 
  (1.96*sqrt(var.int.frg.urb.tert.ad))), exp(int.lg.urb.tert.ad + 1.96*sqrt(var.int.lg.urb.tert.ad)),
         exp(ext.mult.ad$coef[2] + 1.96*sqrt(ext.mult.ad$var[2,2])), exp(ext.frg.urb.tert.ad 
 +1.96*sqrt(var.ext.frg.urb.tert.ad)), exp(ext.lg.urb.tert.ad + 1.96*sqrt(var.ext.lg.urb.tert.ad)) )

tab1<-cbind(ad.or, ad.li, ad.ui)

#tab1
#              ad.or     ad.li    ad.ui
#tertscore 1.0902296 0.7525559 1.579418
#tertscore 1.0055538 0.7495789 1.348942
#tertscore 1.4824157 1.1463877 1.916940
#tertscore 0.8770803 0.4806931 1.600335
#tertscore 0.8615603 0.5671617 1.308774
#tertscore 0.7467570 0.5379888 1.036538
#but the interaction terms are no longer significant.

## now restricting to those within the area of support
folks<-new.prpdat.incl[[1]]["SampleID"]
aos<-list(rep(NA,100))
for(i in 1:100){
	aos[[i]]<-merge(dat[[i]], folks, all=FALSE, by="SampleID")
}

## Unadjusted and unweighted analysis
multimp<-imputationList(aos)
int.unun<- with(multimp, glm(internal ~ tertscore*urbancat + suburb + tertscore:suburb, family=binomial))
int.mult.unun<-MIcombine(int.unun)
int.mult.unun.m<-as.data.frame(summary(int.mult.unun))

ext.unun<- with(multimp, glm(cp_CdOddh12_NIMH2 ~ tertscore*urbancat + suburb + tertscore:suburb, family=binomial))
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
#tertscore 0.8893954 0.7251225 1.090884
#tertscore 1.1749084 1.0003374 1.379944
#tertscore 1.3346291 1.1512816 1.547176
#tertscore 0.7523510 0.5372949 1.053485
#tertscore 0.8809892 0.6797179 1.141859
#tertscore 0.7857300 0.5965386 1.034923

desimp.ns<-svydesign(id=~secu, strata=~str, nest=TRUE, weight=~final_weight, data=multimp)

## Unadjusted and weighted analysis
int.un.wt<- with(desimp.ns, svyglm(internal ~ tertscore*urbancat + suburb + tertscore:suburb, family=quasibinomial()))
int.mult.un.wt<-MIcombine(int.un.wt)
int.mult.un.wt.m<-as.data.frame(summary(int.mult.un.wt))

ext.un.wt<- with(desimp.ns, svyglm(cp_CdOddh12_NIMH2 ~ tertscore*urbancat + suburb + tertscore:suburb, family=quasibinomial()))
ext.mult.un.wt<-MIcombine(ext.un.wt)
ext.mult.un.wt.m<-as.data.frame(summary(ext.mult.un.wt))

a <- c(0, 1, 0,0,1,0)
int.lg.urb.tert.un.wt <-(int.mult.un.wt$coef[2]+int.mult.un.wt$coef[5]) 
var.int.lg.urb.tert.un.wt<- (a %*% int.mult.un.wt$variance %*% a )

a <- c(0, 1, 0,0,0,1)
int.frg.urb.tert.un.wt <-(int.mult.un.wt$coef[2]+int.mult.un.wt$coef[6]) 
var.int.frg.urb.tert.un.wt<- (a %*% int.mult.un.wt$variance %*% a )

a <- c(0, 1, 0,0,1,0)
ext.lg.urb.tert.un.wt <-(ext.mult.un.wt$coef[2]+ext.mult.un.wt$coef[5]) 
var.ext.lg.urb.tert.un.wt<- (a %*% ext.mult.un.wt$variance %*% a )

a <- c(0, 1, 0,0,0,1)
ext.frg.urb.tert.un.wt <-(ext.mult.un.wt$coef[2]+ext.mult.un.wt$coef[6]) 
var.ext.frg.urb.tert.un.wt<- (a %*% ext.mult.un.wt$variance %*% a )

un.wt.or<-c(exp(int.mult.un.wt$coef[2]), exp(int.frg.urb.tert.un.wt), exp(int.lg.urb.tert.un.wt), exp(ext.mult.un.wt$coef[2]), 
         exp(ext.frg.urb.tert.un.wt), exp(ext.lg.urb.tert.un.wt) )
un.wt.li<-c(exp(int.mult.un.wt$coef[2] - 1.96*sqrt(int.mult.un.wt$var[2,2])), exp(int.frg.urb.tert.un.wt - 
  (1.96*sqrt(var.int.frg.urb.tert.un.wt))), exp(int.lg.urb.tert.un.wt - 1.96*sqrt(var.int.lg.urb.tert.un.wt)),
         exp(ext.mult.un.wt$coef[2] - 1.96*sqrt(ext.mult.un.wt$var[2,2])), exp(ext.frg.urb.tert.un.wt 
 -1.96*sqrt(var.ext.frg.urb.tert.un.wt)), exp(ext.lg.urb.tert.un.wt - 1.96*sqrt(var.ext.lg.urb.tert.un.wt)) )
un.wt.ui<-c(exp(int.mult.un.wt$coef[2] + 1.96*sqrt(int.mult.un.wt$var[2,2])), exp(int.frg.urb.tert.un.wt + 
  (1.96*sqrt(var.int.frg.urb.tert.un.wt))), exp(int.lg.urb.tert.un.wt + 1.96*sqrt(var.int.lg.urb.tert.un.wt)),
         exp(ext.mult.un.wt$coef[2] + 1.96*sqrt(ext.mult.un.wt$var[2,2])), exp(ext.frg.urb.tert.un.wt 
 +1.96*sqrt(var.ext.frg.urb.tert.un.wt)), exp(ext.lg.urb.tert.un.wt + 1.96*sqrt(var.ext.lg.urb.tert.un.wt)) )

tab1<-cbind(un.wt.or, un.wt.li, un.wt.ui)
#           un.wt.or  un.wt.li un.wt.ui
#tertscore 1.0814559 0.7671906 1.524454
#tertscore 1.1014126 0.8169836 1.484864
#tertscore 1.6371186 1.1891387 2.253864
#tertscore 0.7023771 0.4164407 1.184643
#tertscore 0.8594933 0.5980986 1.235129
#tertscore 0.8641816 0.5804027 1.286710


## Adjusted and weighted analysis
int.ad<- with(desimp.ns, svyglm(internal ~  tertscore*urbancat + suburb + tertscore:suburb + age_cent*SEXF + factor(meducat) + factor(meducat):SEXF + moth + moth:SEXF + fath + fath:SEXF + urbancat:SEXF + suburb:SEXF + factor(imgen) + factor(imgen):SEXF + poly(cmage, 2) + poly(cmage,2):SEXF + factor(racecat) +  factor(racecat):SEXF + factor(region) + factor(region):SEXF + cinc:nonzeroinc + cinc:nonzeroinc:SEXF , family=quasibinomial()))
int.mult.ad<-MIcombine(int.ad)
int.mult.ad.m<-as.matrix(summary(int.mult.ad))

ext.ad<- with(desimp.ns, svyglm(cp_CdOddh12_NIMH2 ~  tertscore*urbancat + suburb + tertscore:suburb + age_cent*SEXF + factor(meducat) + factor(meducat):SEXF + moth + moth:SEXF + fath + fath:SEXF + urbancat:SEXF + suburb:SEXF + factor(imgen) + factor(imgen):SEXF + poly(cmage, 2) + poly(cmage,2):SEXF + factor(racecat) +  factor(racecat):SEXF + factor(region) + factor(region):SEXF + cinc:nonzeroinc + cinc:nonzeroinc:SEXF , family=quasibinomial()))
ext.mult.ad<-MIcombine(ext.ad)
ext.mult.ad.m<-as.matrix(summary(ext.mult.ad))

a <- c(0,1,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,1,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,0,0)
int.lg.urb.tert.ad <-(int.mult.ad$coef[2]+int.mult.ad$coef[22]) 
var.int.lg.urb.tert.ad<- (a %*% int.mult.ad$variance %*% a )

a <- c(0,1,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,0,1,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,0,0)
int.frg.urb.tert.ad <-(int.mult.ad$coef[2]+int.mult.ad$coef[23]) 
var.int.frg.urb.tert.ad<- (a %*% int.mult.ad$variance %*% a )

a <- c(0,1,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,1,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,0,0)

ext.lg.urb.tert.ad <-(ext.mult.ad$coef[2]+ext.mult.ad$coef[22]) 
var.ext.lg.urb.tert.ad<- (a %*% ext.mult.ad$variance %*% a )

a <- c(0,1,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,0,1,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,
	   0,0,0)
ext.frg.urb.tert.ad <-(ext.mult.ad$coef[2]+ext.mult.ad$coef[23]) 
var.ext.frg.urb.tert.ad<- (a %*% ext.mult.ad$variance %*% a )

ad.or<-c(exp(int.mult.ad$coef[2]), exp(int.frg.urb.tert.ad), exp(int.lg.urb.tert.ad), exp(ext.mult.ad$coef[2]), 
         exp(ext.frg.urb.tert.ad), exp(ext.lg.urb.tert.ad) )
ad.li<-c(exp(int.mult.ad$coef[2] - 1.96*sqrt(int.mult.ad$var[2,2])), exp(int.frg.urb.tert.ad - 
  (1.96*sqrt(var.int.frg.urb.tert.ad))), exp(int.lg.urb.tert.ad - 1.96*sqrt(var.int.lg.urb.tert.ad)),
         exp(ext.mult.ad$coef[2] - 1.96*sqrt(ext.mult.ad$var[2,2])), exp(ext.frg.urb.tert.ad 
 -1.96*sqrt(var.ext.frg.urb.tert.ad)), exp(ext.lg.urb.tert.ad - 1.96*sqrt(var.ext.lg.urb.tert.ad)) )
ad.ui<-c(exp(int.mult.ad$coef[2] + 1.96*sqrt(int.mult.ad$var[2,2])), exp(int.frg.urb.tert.ad + 
  (1.96*sqrt(var.int.frg.urb.tert.ad))), exp(int.lg.urb.tert.ad + 1.96*sqrt(var.int.lg.urb.tert.ad)),
         exp(ext.mult.ad$coef[2] + 1.96*sqrt(ext.mult.ad$var[2,2])), exp(ext.frg.urb.tert.ad 
 +1.96*sqrt(var.ext.frg.urb.tert.ad)), exp(ext.lg.urb.tert.ad + 1.96*sqrt(var.ext.lg.urb.tert.ad)) )

tab1<-cbind(ad.or, ad.li, ad.ui)
#              ad.or     ad.li    ad.ui
#tertscore 1.0999929 0.7581437 1.595983
#tertscore 1.0118909 0.7569527 1.352691
#tertscore 1.4740342 1.1377125 1.909777
#tertscore 0.9106284 0.4980997 1.664816
#tertscore 0.8664478 0.5652820 1.328066
#tertscore 0.7287602 0.5239543 1.013622