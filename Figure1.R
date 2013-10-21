coefnam<-c("Propensity Score Distance", "Age", "Female", "Maternal Education: Less than High School", "Maternal Education: High School Graduate", "Maternal Education: Some College", "Maternal Education: College Graduate", "Lived w/Mother" ,"Lived w/Father", "Urban Core",  "Urban Fringe",
"2nd Gen Immigrant", "3rd Gen Immigrant", "Maternal Age at Birth", "Maternal Age at Birth^2", "Hispanic", 
"Black", "Other Race", "Midwestern Region", "Southern Region", "Western Region", 
"Log Income","Age*Female","Maternal Education: Less than HS*Female", "Maternal Education: HS Graduate*Female", "Maternal Education: Some College*Female", 
"Lived w/Mother*Female" ,"Lived w/Father*Female", "Urban Core*Female",  "Urban Fringe*Female","2nd Gen Immigrant*Female", "3rd Gen Immigrant*Female", "Maternal Age at Birth*Female", "Maternal Age at Birth^2*Female", "Hispanic*Female", "Black*Female", "Other Race*Female", "Midwestern Region*Female", "Southern Region*Female", "Western Region*Female","Log Income*Female")

smd.pre<-c(1.2523,-.0903, .0106,.3018,.1917,-.0551, 
			-.5630, -.0974, -.4015, -.3050, .0975, 
			.1047, -.1613, -.4175, .2437, .5629, 
			-.1032, -.8657, -.5429, .5801, -.0527,
			-.5594, -.0579, .1048, -.0163, -.3902, 
			-.0369, -.2348, -.1818, .0541, .0661, 
			-.0598, -.3237, .1657, .3676, -.0756, 
			-.5555, -.3643, .3453, -.0301, -.3969)

smd.post<-c(.0122, .0195, .024, .0204, .0174, .057,
			.0676, .0397, .0389, .0594, .0408, 
			.0275, .0386, .0408, .0283, .0334, 
			.0291, .0226, .0531, .0341, .0262,
			.0366, .0196, .0243, .0325, .0473,
			.036, .0326, .072, .037, .0208, 
			.029, .0309, .0191, .028, .0247, 
			.0242, .0530, .0276, .0163, .0329)

tab5<-data.frame(nam=coefnam, pre=smd.pre*100, post=smd.post*100)
tab.ord1<-tab5[order(tab5$pre, decreasing=TRUE),]

pdf("loveplot.pdf")

postscript("loveplot_col_rgb.eps", paper="special", height=8, width=8, horizontal=FALSE, colormodel="rgb")
#postscript("loveplotnoleg.eps",  paper="special", height=9, width=8,horizontal=FALSE, colormodel="cmyk")
dotchart(tab.ord1$pre, pch="", labels=tab.ord1$nam, cex=0.75) 
mtext("Standardized Difference (%)", side=1, line=2, cex=0.85)
points(tab.ord1$pre, seq(1:length(tab.ord1$pre)), 
pch=21, col="blue", cex=1) 
points(tab.ord1$post,seq(1:length(tab.ord1$post)), 
pch=16, col="red", cex=1) 
abline(v=0, lty=1) 
abline(v=10, lty=2, lwd=1) 
abline(v=-10, lty=2, lwd=1) 
#dev.off()

legend("topright", legend = c("Pre-Subclassification", "Post-Subclassification"), col=c("blue", "red"), text.col=c("blue", "red"), pch=c(21,16), cex=0.8) 
dev.off()