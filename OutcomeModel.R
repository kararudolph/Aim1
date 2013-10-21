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

des<-list(rep(NA,100))
for(i in 1:100){
des[[i]]<-svydesign(id=~secu, strata=~str, nest=TRUE, weight=~final_weight, data=new.prpdat.incl[[i]])
}

#Females first
for(i in 1:100){

int1[[i]]<-summary(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==1)))
int2[[i]]<-summary(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==2)))
int3[[i]]<-summary(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==3)))
int4[[i]]<-summary(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==4)))
int5[[i]]<-summary(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==5)))
int6[[i]]<-summary(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==6)))
int7[[i]]<-summary(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==7)))
int8[[i]]<-summary(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==8)))
int9[[i]]<-summary(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==9)))

ext1[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==1)))
ext2[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==2)))
ext3[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==3)))
ext4[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==4)))
ext5[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==5)))
ext6[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==6)))
ext7[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==7)))
ext8[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==8)))
ext9[[i]]<-summary(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==9)))

vcov.int1[[i]]<-vcov(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==1)))
vcov.int2[[i]]<-vcov(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==2)))
vcov.int3[[i]]<-vcov(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==3)))
vcov.int4[[i]]<-vcov(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==4)))
vcov.int5[[i]]<-vcov(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==5)))
vcov.int6[[i]]<-vcov(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==6)))
vcov.int7[[i]]<-vcov(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==7)))
vcov.int8[[i]]<-vcov(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==8)))
vcov.int9[[i]]<-vcov(svyglm(internal ~ tertscore + tertscore:urbancat + tertscore:suburb + distance , family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==9)))


vcov.ext1[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==1)))
vcov.ext2[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==2)))
vcov.ext3[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==3)))
vcov.ext4[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==4)))
vcov.ext5[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==5)))
vcov.ext6[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==6)))
vcov.ext7[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==7)))
vcov.ext8[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==8)))
vcov.ext9[[i]]<-vcov(svyglm(cp_CdOddh12_NIMH2 ~ tertscore + tertscore:urbancat + tertscore:suburb + distance, family=quasibinomial(), subset(des[[i]],  new.prpdat.incl[[i]]$subclass==9)))

}

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
                  + (int9[[i]]$coef[2]*(1/int9[[i]]$coef[2,2]^2)) ) / ( (1/int1[[i]]$coef[2,2]^2) +
                    (1/int2[[i]]$coef[2,2]^2)  + (1/int3[[i]]$coef[2,2]^2) + (1/int4[[i]]$coef[2,2]^2) + (1/int5[[i]]$coef[2,2]^2)
                    + (1/int6[[i]]$coef[2,2]^2) +(1/int7[[i]]$coef[2,2]^2) +(1/int8[[i]]$coef[2,2]^2) + (1/int9[[i]]$coef[2,2]^2) ) )

  var[[i]]<- 1 / ((1/int1[[i]]$coef[2,2]^2) + (1/int2[[i]]$coef[2,2]^2)  + (1/int3[[i]]$coef[2,2]^2) + (1/int4[[i]]$coef[2,2]^2) + (1/int5[[i]]$coef[2,2]^2)
                    + (1/int6[[i]]$coef[2,2]^2) +(1/int7[[i]]$coef[2,2]^2) +(1/int8[[i]]$coef[2,2]^2) + (1/int9[[i]]$coef[2,2]^2))

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
                  + (int9[[i]]$coef[4]*(1/int9[[i]]$coef[4,2]^2)) ) / ( (1/int1[[i]]$coef[4,2]^2) +
                    (1/int2[[i]]$coef[4,2]^2)  + (1/int3[[i]]$coef[4,2]^2) + (1/int4[[i]]$coef[4,2]^2) + (1/int5[[i]]$coef[4,2]^2)
                    + (1/int6[[i]]$coef[4,2]^2) +(1/int7[[i]]$coef[4,2]^2) +(1/int8[[i]]$coef[4,2]^2) + (1/int9[[i]]$coef[4,2]^2) ) )

  var.scorecore.int[[i]]<- 1 / ((1/int1[[i]]$coef[4,2]^2) + (1/int2[[i]]$coef[4,2]^2)  + (1/int3[[i]]$coef[4,2]^2) + (1/int4[[i]]$coef[4,2]^2) + (1/int5[[i]]$coef[4,2]^2)
                    + (1/int6[[i]]$coef[4,2]^2) +(1/int7[[i]]$coef[4,2]^2) +(1/int8[[i]]$coef[4,2]^2) + (1/int9[[i]]$coef[4,2]^2))
}

scorefringe.int<-list(rep(NA, 100))
var.scorefringe.int<-list(rep(NA, 100))

for(i in 1:100){
  scorefringe.int[[i]]<- ( ( (int1[[i]]$coef[5]*(1/int1[[i]]$coef[5,2]^2)) + (int2[[i]]$coef[5]*(1/int2[[i]]$coef[5,2]^2)) + 
    (int3[[i]]$coef[5]*(1/int3[[i]]$coef[5,2]^2)) + (int4[[i]]$coef[5]*(1/int4[[i]]$coef[5,2]^2)) + (int5[[i]]$coef[5]*(1/int5[[i]]$coef[5,2]^2))
    + (int6[[i]]$coef[5]*(1/int6[[i]]$coef[5,2]^2)) + (int7[[i]]$coef[5]*(1/int7[[i]]$coef[5,2]^2)) +  (int8[[i]]$coef[5]*(1/int8[[i]]$coef[5,2]^2)) 
                  + (int9[[i]]$coef[5]*(1/int9[[i]]$coef[5,2]^2)) ) / ( (1/int1[[i]]$coef[5,2]^2) +
                    (1/int2[[i]]$coef[5,2]^2)  + (1/int3[[i]]$coef[5,2]^2) + (1/int4[[i]]$coef[5,2]^2) + (1/int5[[i]]$coef[5,2]^2)
                    + (1/int6[[i]]$coef[5,2]^2) +(1/int7[[i]]$coef[5,2]^2) +(1/int8[[i]]$coef[5,2]^2) + (1/int9[[i]]$coef[5,2]^2) ) )

  var.scorefringe.int[[i]]<- 1 / ((1/int1[[i]]$coef[5,2]^2) + (1/int2[[i]]$coef[5,2]^2)  + (1/int3[[i]]$coef[5,2]^2) + (1/int4[[i]]$coef[5,2]^2) + (1/int5[[i]]$coef[5,2]^2)
                    + (1/int6[[i]]$coef[5,2]^2) +(1/int7[[i]]$coef[5,2]^2) +(1/int8[[i]]$coef[5,2]^2) + (1/int9[[i]]$coef[5,2]^2))

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
     ( (int8[[i]]$coef[2]+int8[[i]]$coef[4]) * (1/ (a %*% vcov.int8[[i]] %*% a)) ) +
     ( (int9[[i]]$coef[2]+int9[[i]]$coef[4]) * (1/ (a %*% vcov.int9[[i]] %*% a)) ) 
     ) / (
       (1/ (a %*% vcov.int1[[i]] %*% a))  +(1/ (a %*% vcov.int2[[i]] %*% a))  +(1/ (a %*% vcov.int3[[i]] %*% a)) +
         (1/ (a %*% vcov.int4[[i]] %*% a)) +(1/ (a %*% vcov.int5[[i]] %*% a)) +(1/ (a %*% vcov.int6[[i]] %*% a)) +
         (1/ (a %*% vcov.int7[[i]] %*% a))  +(1/ (a %*% vcov.int8[[i]] %*% a)) +(1/ (a %*% vcov.int9[[i]] %*% a))
))
       
  var.lg.urb.tert[[i]]<- 1 / ( (1/ (a %*% vcov.int1[[i]] %*% a))  +(1/ (a %*% vcov.int2[[i]] %*% a))+
    (1/ (a %*% vcov.int3[[i]] %*% a)) + (1/ (a %*% vcov.int4[[i]] %*% a)) +(1/ (a %*% vcov.int5[[i]] %*% a)) +
    (1/ (a %*% vcov.int6[[i]] %*% a)) + (1/ (a %*% vcov.int7[[i]] %*% a))  +(1/ (a %*% vcov.int8[[i]] %*% a)) +
    (1/ (a %*% vcov.int9[[i]] %*% a)) )

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
     ( (int8[[i]]$coef[2]+int8[[i]]$coef[5]) * (1/ (a %*% vcov.int8[[i]] %*% a)) ) +
     ( (int9[[i]]$coef[2]+int9[[i]]$coef[5]) * (1/ (a %*% vcov.int9[[i]] %*% a)) ) 
     ) / (
       (1/ (a %*% vcov.int1[[i]] %*% a))  +(1/ (a %*% vcov.int2[[i]] %*% a))  +(1/ (a %*% vcov.int3[[i]] %*% a)) +
         (1/ (a %*% vcov.int4[[i]] %*% a)) +(1/ (a %*% vcov.int5[[i]] %*% a)) +(1/ (a %*% vcov.int6[[i]] %*% a)) +
         (1/ (a %*% vcov.int7[[i]] %*% a))  +(1/ (a %*% vcov.int8[[i]] %*% a)) +(1/ (a %*% vcov.int9[[i]] %*% a))
))
       
  var.frg.urb.tert[[i]]<- 1 / ( (1/ (a %*% vcov.int1[[i]] %*% a))  +(1/ (a %*% vcov.int2[[i]] %*% a))+
    (1/ (a %*% vcov.int3[[i]] %*% a)) + (1/ (a %*% vcov.int4[[i]] %*% a)) +(1/ (a %*% vcov.int5[[i]] %*% a)) +
    (1/ (a %*% vcov.int6[[i]] %*% a)) + (1/ (a %*% vcov.int7[[i]] %*% a))  +(1/ (a %*% vcov.int8[[i]] %*% a)) +
    (1/ (a %*% vcov.int9[[i]] %*% a)) )

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

scorecore.ext<-list(rep(NA, 100))
var.scorecore.ext<-list(rep(NA, 100))

for(i in 1:100){
  scorecore.ext[[i]]<- ( ( (ext1[[i]]$coef[4]*(1/ext1[[i]]$coef[4,2]^2)) + (ext2[[i]]$coef[4]*(1/ext2[[i]]$coef[4,2]^2)) + 
    (ext3[[i]]$coef[4]*(1/ext3[[i]]$coef[4,2]^2)) + (ext4[[i]]$coef[4]*(1/ext4[[i]]$coef[4,2]^2)) + (ext5[[i]]$coef[4]*(1/ext5[[i]]$coef[4,2]^2))
    + (ext6[[i]]$coef[4]*(1/ext6[[i]]$coef[4,2]^2)) + (ext7[[i]]$coef[4]*(1/ext7[[i]]$coef[4,2]^2)) +  (ext8[[i]]$coef[4]*(1/ext8[[i]]$coef[4,2]^2)) 
                  + (ext9[[i]]$coef[4]*(1/ext9[[i]]$coef[4,2]^2)) ) / ( (1/ext1[[i]]$coef[4,2]^2) +
                    (1/ext2[[i]]$coef[4,2]^2)  + (1/ext3[[i]]$coef[4,2]^2) + (1/ext4[[i]]$coef[4,2]^2) + (1/ext5[[i]]$coef[4,2]^2)
                    + (1/ext6[[i]]$coef[4,2]^2) +(1/ext7[[i]]$coef[4,2]^2) +(1/ext8[[i]]$coef[4,2]^2) + (1/ext9[[i]]$coef[4,2]^2) ) )

  var.scorecore.ext[[i]]<- 1 / ((1/ext1[[i]]$coef[4,2]^2) + (1/ext2[[i]]$coef[4,2]^2)  + (1/ext3[[i]]$coef[4,2]^2) + (1/ext4[[i]]$coef[4,2]^2) + (1/ext5[[i]]$coef[4,2]^2)
                    + (1/ext6[[i]]$coef[4,2]^2) +(1/ext7[[i]]$coef[4,2]^2) +(1/ext8[[i]]$coef[4,2]^2) + (1/ext9[[i]]$coef[4,2]^2))
}

scorefringe.ext<-list(rep(NA, 100))
var.scorefringe.ext<-list(rep(NA, 100))

for(i in 1:100){
  scorefringe.ext[[i]]<- ( ( (ext1[[i]]$coef[5]*(1/ext1[[i]]$coef[5,2]^2)) + (ext2[[i]]$coef[5]*(1/ext2[[i]]$coef[5,2]^2)) + 
    (ext3[[i]]$coef[5]*(1/ext3[[i]]$coef[5,2]^2)) + (ext4[[i]]$coef[5]*(1/ext4[[i]]$coef[5,2]^2)) + (ext5[[i]]$coef[5]*(1/ext5[[i]]$coef[5,2]^2))
    + (ext6[[i]]$coef[5]*(1/ext6[[i]]$coef[5,2]^2)) + (ext7[[i]]$coef[5]*(1/ext7[[i]]$coef[5,2]^2)) +  (ext8[[i]]$coef[5]*(1/ext8[[i]]$coef[5,2]^2)) 
                  + (ext9[[i]]$coef[5]*(1/ext9[[i]]$coef[5,2]^2)) ) / ( (1/ext1[[i]]$coef[5,2]^2) +
                    (1/ext2[[i]]$coef[5,2]^2)  + (1/ext3[[i]]$coef[5,2]^2) + (1/ext4[[i]]$coef[5,2]^2) + (1/ext5[[i]]$coef[5,2]^2)
                    + (1/ext6[[i]]$coef[5,2]^2) +(1/ext7[[i]]$coef[5,2]^2) +(1/ext8[[i]]$coef[5,2]^2) + (1/ext9[[i]]$coef[5,2]^2) ) )

  var.scorefringe.ext[[i]]<- 1 / ((1/ext1[[i]]$coef[5,2]^2) + (1/ext2[[i]]$coef[5,2]^2)  + (1/ext3[[i]]$coef[5,2]^2) + (1/ext4[[i]]$coef[5,2]^2) + (1/ext5[[i]]$coef[5,2]^2)
                    + (1/ext6[[i]]$coef[5,2]^2) +(1/ext7[[i]]$coef[5,2]^2) +(1/ext8[[i]]$coef[5,2]^2) + (1/ext9[[i]]$coef[5,2]^2))

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
     ( (ext8[[i]]$coef[2]+ext8[[i]]$coef[4]) * (1/ (a %*% vcov.ext8[[i]] %*% a)) ) +
     ( (ext9[[i]]$coef[2]+ext9[[i]]$coef[4]) * (1/ (a %*% vcov.ext9[[i]] %*% a)) ) 
     ) / (
       (1/ (a %*% vcov.ext1[[i]] %*% a))  +(1/ (a %*% vcov.ext2[[i]] %*% a))  +(1/ (a %*% vcov.ext3[[i]] %*% a)) +
         (1/ (a %*% vcov.ext4[[i]] %*% a)) +(1/ (a %*% vcov.ext5[[i]] %*% a)) +(1/ (a %*% vcov.ext6[[i]] %*% a)) +
         (1/ (a %*% vcov.ext7[[i]] %*% a))  +(1/ (a %*% vcov.ext8[[i]] %*% a)) +(1/ (a %*% vcov.ext9[[i]] %*% a))
))
       
  var.ext.lg.urb.tert[[i]]<- 1 / ( (1/ (a %*% vcov.ext1[[i]] %*% a))  +(1/ (a %*% vcov.ext2[[i]] %*% a))+
    (1/ (a %*% vcov.ext3[[i]] %*% a)) + (1/ (a %*% vcov.ext4[[i]] %*% a)) +(1/ (a %*% vcov.ext5[[i]] %*% a)) +
    (1/ (a %*% vcov.ext6[[i]] %*% a)) + (1/ (a %*% vcov.ext7[[i]] %*% a))  +(1/ (a %*% vcov.ext8[[i]] %*% a)) +
    (1/ (a %*% vcov.ext9[[i]] %*% a)) )

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
     ( (ext8[[i]]$coef[2]+ext8[[i]]$coef[5]) * (1/ (a %*% vcov.ext8[[i]] %*% a)) ) +
     ( (ext9[[i]]$coef[2]+ext9[[i]]$coef[5]) * (1/ (a %*% vcov.ext9[[i]] %*% a)) ) 
     ) / (
       (1/ (a %*% vcov.ext1[[i]] %*% a))  +(1/ (a %*% vcov.ext2[[i]] %*% a))  +(1/ (a %*% vcov.ext3[[i]] %*% a)) +
         (1/ (a %*% vcov.ext4[[i]] %*% a)) +(1/ (a %*% vcov.ext5[[i]] %*% a)) +(1/ (a %*% vcov.ext6[[i]] %*% a)) +
         (1/ (a %*% vcov.ext7[[i]] %*% a))  +(1/ (a %*% vcov.ext8[[i]] %*% a)) +(1/ (a %*% vcov.ext9[[i]] %*% a))
))
       
  var.ext.frg.urb.tert[[i]]<- 1 / ( (1/ (a %*% vcov.ext1[[i]] %*% a))  +(1/ (a %*% vcov.ext2[[i]] %*% a))+
    (1/ (a %*% vcov.ext3[[i]] %*% a)) + (1/ (a %*% vcov.ext4[[i]] %*% a)) +(1/ (a %*% vcov.ext5[[i]] %*% a)) +
    (1/ (a %*% vcov.ext6[[i]] %*% a)) + (1/ (a %*% vcov.ext7[[i]] %*% a))  +(1/ (a %*% vcov.ext8[[i]] %*% a)) +
    (1/ (a %*% vcov.ext9[[i]] %*% a)) )

#gives exponentiated results
expon.ext.frg.urb.tert[[i]]<-exp(ext.frg.urb.tert[[i]])
}


rural.ext<-MIcombine(results=ext.effect, variances=ext.var) 

#large urban
lg.urb.ext<-MIcombine(results=ext.lg.urb.tert, variances=var.ext.lg.urb.tert)
#fringe urban
frg.urb.ext<-MIcombine(results=ext.frg.urb.tert, variances=var.ext.frg.urb.tert)

mult.scorecore.ext<-MIcombine(results=scorecore.ext, variances=var.scorecore.ext)

mult.scorefringe.ext<-MIcombine(results=scorefringe.ext, variances=var.scorefringe.ext)


rural.int.sum<-summary(rural.int)
lg.urb.int.sum<-summary(lg.urb.int)
frg.urb.int.sum<-summary(frg.urb.int)
rural.ext.sum<-summary(rural.ext)
lg.urb.ext.sum<-summary(lg.urb.ext)
frg.urb.ext.sum<-summary(frg.urb.ext)

save(rural.int.sum, file="rural.int.sum.Rdata")
save(lg.urb.int.sum, file="lg.urb.int.sum.Rdata")
save(frg.urb.int.sum, file="frg.urb.int.sum.Rdata")
save(rural.ext.sum, file="rural.ext.sum.Rdata")
save(lg.urb.ext.sum, file="lg.urb.ext.sum.Rdata")
save(frg.urb.ext.sum, file="frg.urb.ext.sum.Rdata")

load("rural.int.sum.Rdata")
load("lg.urb.int.sum.Rdata")
load("frg.urb.int.sum.Rdata")
load("rural.ext.sum.Rdata")
load("lg.urb.ext.sum.Rdata")
load("frg.urb.ext.sum.Rdata")

mult.scorecore.int.sum<-summary(mult.scorecore.int)
mult.scorefringe.int.sum<-summary(mult.scorefringe.int)
mult.scorecore.ext.sum<-summary(mult.scorecore.ext)
mult.scorefringe.ext.sum<-summary(mult.scorefringe.ext)

save(mult.scorecore.int, file="mult.scorecore.int.Rdata")
save(mult.scorefringe.int, file="mult.scorefringe.int.Rdata")
save(mult.scorecore.ext, file="mult.scorecore.ext.Rdata")
save(mult.scorefringe.ext, file="mult.scorefringe.ext.Rdata")

load("mult.scorecore.int.Rdata")
load("mult.scorefringe.int.Rdata")

#    results        se    (lower   upper) missInfo
#1 0.7720788 0.2448426 0.2920312 1.252126     17 %
mult.scorecore.int.sum
#    results        se    (lower   upper) missInfo
#1 0.7347286 0.2699015 0.2033983 1.266059     60 %
z.mult.scorecore.int.sum<-mult.scorecore.int.sum[[1]]/mult.scorecore.int.sum[[2]]
#2.72221
p.mult.scorecore.int.sum<-2*pnorm(abs(z.mult.scorecore.int.sum), lower.tail=FALSE)
#0.006484695

mult.scorecore.ext.sum
mult.scorefringe.int.sum
mult.scorefringe.ext.sum
z.mult.scorefringe.ext.sum<-mult.scorefringe.ext.sum[[1]]/mult.scorefringe.ext.sum[[2]]
#3.153368
p.mult.scorefringe.ext.sum<-2*pnorm(abs(z.mult.scorefringe.ext.sum), lower.tail=FALSE)
#0.001613983

scoreOR<-list(rep(NA,6))
scoreupper<-list(rep(NA,6))
scorelower<-list(rep(NA,6))

scoreOR<-list( exp(rural.int.sum[[1]]), exp(frg.urb.int.sum[[1]]), exp(lg.urb.int.sum[[1]]), exp(rural.ext.sum[[1]]), exp(frg.urb.ext.sum[[1]]), exp(lg.urb.ext.sum[[1]]) )
scorelower<-list(exp(rural.int.sum[[3]]), exp(frg.urb.int.sum[[3]]),  exp(lg.urb.int.sum[[3]]), exp(rural.ext.sum[[3]]),
 exp(frg.urb.ext.sum[[3]]), exp(lg.urb.ext.sum[[3]]))
 scoreupper<-list(exp(rural.int.sum[[4]]), exp(frg.urb.int.sum[[4]]),  exp(lg.urb.int.sum[[4]]), exp(rural.ext.sum[[4]]),
 exp(frg.urb.ext.sum[[4]]), exp(lg.urb.ext.sum[[4]]))
 
tab3<-cbind(scoreOR, scorelower, scoreupper)