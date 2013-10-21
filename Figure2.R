library(gplots)
#internalizing
postscript("Fig3Col.eps", paper="special", height=8, width=8, colormodel="rgb", horizontal=FALSE)
#pdf("figure2.pdf")
plotCI(x=c(0,1, 2), y=c(rural.int.sum[[1]], frg.urb.int.sum[[1]], lg.urb.int.sum[[1]]), 
       ui=c(rural.int.sum[[4]], frg.urb.int.sum[[4]], lg.urb.int.sum[[4]]), 
       li=c(rural.int.sum[[3]], frg.urb.int.sum[[3]], lg.urb.int.sum[[3]]), lty=1, lwd=2, 
       err="y", col="black", add=FALSE, xlim=c(-.5, 2.5), ylim=c(-1.75, .75), xaxt="n", ylab="logOR", xlab="")
axis(1, at=c(-.5, .0, .5,1,1.5, 2, 2.5), labels=c("", "non-urban", "", "urban fringe", "", "urban center", ""), 
     lty=1,tck=0)
abline(h=0)


#externalizing
plotCI(x=c(.1,1.1, 2.1), y=c(rural.ext.sum[[1]], frg.urb.ext.sum[[1]], lg.urb.ext.sum[[1]]), 
       ui=c(rural.ext.sum[[4]], frg.urb.ext.sum[[4]], lg.urb.ext.sum[[4]]), 
       li=c(rural.ext.sum[[3]], frg.urb.ext.sum[[3]], lg.urb.ext.sum[[3]]), lty=2, lwd=2, 
       err="y", col="red", add=TRUE, xlim=c(-.5, 2.5), xaxt="n", ylab="logOR", xlab="")
   #    dev.off()
legend("bottomright", bty="n", c("emotional","behavioral"), lty=c(1,2), text.col=c("blue", "red"), col=c("blue", "red"), horiz=FALSE)
dev.off()

postscript("Fig3BW.eps", paper="special", height=8, width=8, colormodel="cmyk", horizontal=FALSE)
#pdf("figure2.pdf")
plotCI(x=c(-.1,.9, 1.9), y=c(rural.int.sum[[1]], frg.urb.int.sum[[1]], lg.urb.int.sum[[1]]), 
       ui=c(rural.int.sum[[4]], frg.urb.int.sum[[4]], lg.urb.int.sum[[4]]), 
       li=c(rural.int.sum[[3]], frg.urb.int.sum[[3]], lg.urb.int.sum[[3]]), lty=1, lwd=2, 
       err="y", col="black", add=FALSE, xlim=c(-.5, 2.5), ylim=c(-1.75, .75), xaxt="n", ylab="logOR", xlab="")
axis(1, at=c(-.5, .0, .5,1,1.5, 2, 2.5), labels=c("", "non-urban", "", "urban fringe", "", "urban center", ""), 
     lty=1,tck=0)
abline(h=0)


#externalizing
plotCI(x=c(.1,1.1, 2.1), y=c(rural.ext.sum[[1]], frg.urb.ext.sum[[1]], lg.urb.ext.sum[[1]]), 
       ui=c(rural.ext.sum[[4]], frg.urb.ext.sum[[4]], lg.urb.ext.sum[[4]]), 
       li=c(rural.ext.sum[[3]], frg.urb.ext.sum[[3]], lg.urb.ext.sum[[3]]), lty=2, lwd=2, 
       err="y", col="black", add=TRUE, xlim=c(-.5, 2.5), xaxt="n", ylab="logOR", xlab="")
   #    dev.off()
legend("bottomright", bty="n", c("emotional","behavioral"), lty=c(1,2), text.col=c("black", "black"), col=c("black", "black"), horiz=FALSE)
dev.off()