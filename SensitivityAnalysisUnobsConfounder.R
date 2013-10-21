library(ggplot2)
unobsc<-function(d,g, u1a1){
  u1a0<-u1a1/g
  biasOR<- (1+(d-1)*u1a1) / (1+(d-1)*u1a0)

# Corrected lower confidence bound
clor<-1.25/biasOR
clor
}

gamma<-c(1,1.25, 1.5, 2, 3, 5)
delta<-seq(1,5,.01)
u1a1<-c(.1, .3, .5, .8)
ngam<-length(gamma)
ndelt<-length(delta)
nu1a1<-length(u1a1)
sam.1<-list(rep(NA, nu1a1))
lisa<-list(rep(NA, nu1a1))

for(k in 1:nu1a1){
sam.1[[k]]<-array(numeric(ndelt*ngam), dim=c(ndelt, ngam))
  for (i in 1:ndelt){
			for(j in 1:ngam){
				lisa<-unobsc(d=delta[i], g=gamma[j], u1a1=u1a1[k])
				sam.1[[k]][i,j]<-lisa
			}
	}
	}

dat <- data.frame (x=rep (delta, 24), y=c(sam.1[[1]][,1], sam.1[[1]][,2], sam.1[[1]][,3], sam.1[[1]][,4], sam.1[[1]][,5], sam.1[[1]][,6], sam.1[[2]][,1], sam.1[[2]][,2], sam.1[[2]][,3], sam.1[[2]][,4], sam.1[[2]][,5], sam.1[[2]][,6], sam.1[[3]][,1], sam.1[[3]][,2], sam.1[[3]][,3], sam.1[[3]][,4], sam.1[[3]][,5], sam.1[[3]][,6], sam.1[[4]][,1], sam.1[[4]][,2], sam.1[[4]][,3], sam.1[[4]][,4], sam.1[[4]][,5], sam.1[[4]][,6]),
        gamma=factor (rep (c(1,1.25, 1.5, 2, 3, 5), each=length(delta))),
        graph=factor (rep (c("P(u|a=1,x)=0.1", "P(u|a=1,x)=0.3", "P(u|a=1,x)=0.5", "P(u|a=1,x)=0.8"), each=length(delta)*length(gamma))))

ggplot(data = dat,aes(x = x, y = y)) + 
    facet_wrap(~graph,nrow = 2) + 
    geom_line(aes(colour = gamma, group = gamma)) + 
    labs(x = "gamma", y = "Corrected lower confidence bound", colour = "") + 
    theme_bw() + scale_colour_discrete(name ="delta") +
    opts(legend.position = "right", legend.direction = "vertical" )

    unobsc_notrare<-function(d,g,u1a1){
  RR<-1
ev.u1a1<-0.5
#delta is strength of association between u and outcome
ev.u0a1<-ev.u1a1/d
ev.u1a0<-ev.u1a1/RR
ev.u0a0<-ev.u1a0/d
#gamma is strength of association between u and exposure
u1a0<-u1a1/g
Pra1<-.3333
u1<-(u1a1*Pra1) + (u1a0*(1-Pra1))

biasOR<-(
((ev.u1a1*u1a1 + ev.u0a1*(1-u1a1)) 
/ (((1-ev.u1a1)*u1a1) + ((1-ev.u0a1)*(1-u1a1))) )
/ 
((ev.u1a0*u1a0 + ev.u0a0*(1-u1a0)) 
/ (((1-ev.u1a0)*u1a0) + ((1-ev.u0a0)*(1-u1a0))) )
) / (
((ev.u1a1*u1 + ev.u0a1*(1-u1)) 
/ (((1-ev.u1a1)*u1) + ((1-ev.u0a1)*(1-u1))) )
/ 
((ev.u1a0*u1 + ev.u0a0*(1-u1)) 
/ (((1-ev.u1a0)*u1) + ((1-ev.u0a0)*(1-u1))) )
) 

# Corrected lower confidence bound
clor<-1.25/biasOR
clor
}

gamma<-c(1,1.25, 1.5, 2, 3, 5)
delta<-seq(1,5,.01)
u1a1<-c(.1, .3, .5, .8)
ngam<-length(gamma)
ndelt<-length(delta)
nu1a1<-length(u1a1)
sam.1<-list(rep(NA, nu1a1))
lisa<-list(rep(NA, nu1a1))

for(k in 1:nu1a1){
sam.1[[k]]<-array(numeric(ndelt*ngam), dim=c(ndelt, ngam))
  for (i in 1:ndelt){
      for(j in 1:ngam){
        lisa<-unobsc_notrare(d=delta[i], g=gamma[j], u1a1=u1a1[k])
        sam.1[[k]][i,j]<-lisa
      }
  }
  }

dat <- data.frame (x=rep (delta, 24), y=c(sam.1[[1]][,1], sam.1[[1]][,2], sam.1[[1]][,3], sam.1[[1]][,4], sam.1[[1]][,5], sam.1[[1]][,6], sam.1[[2]][,1], sam.1[[2]][,2], sam.1[[2]][,3], sam.1[[2]][,4], sam.1[[2]][,5], sam.1[[2]][,6], sam.1[[3]][,1], sam.1[[3]][,2], sam.1[[3]][,3], sam.1[[3]][,4], sam.1[[3]][,5], sam.1[[3]][,6], sam.1[[4]][,1], sam.1[[4]][,2], sam.1[[4]][,3], sam.1[[4]][,4], sam.1[[4]][,5], sam.1[[4]][,6]),
        gamma=factor (rep (c(1,1.25, 1.5, 2, 3, 5), each=length(delta))),
        graph=factor (rep (c("P(u|a=1,x)=0.1", "P(u|a=1,x)=0.3", "P(u|a=1,x)=0.5", "P(u|a=1,x)=0.8"), each=length(delta)*length(gamma))))

ggplot(data = dat,aes(x = x, y = y)) + 
    facet_wrap(~graph,nrow = 2) + 
    geom_line(aes(colour = gamma, group = gamma)) + 
    labs(x = "gamma", y = "Corrected lower confidence bound", colour = "") + 
    theme_bw() + scale_colour_discrete(name ="delta") +
    opts(legend.position = "right", legend.direction = "vertical" )