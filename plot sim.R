
#Code mofied from: http://psychologicalstatistics.blogspot.com/2010/02/interaction-plot-from-cell-means.html
#Modifications allow for a greater number of cells (vs. the origional 2*2)

plot.sim <- function(obj, group.names, legend = TRUE, leg.loc=NULL, 
                     factor.labels=c('Factor A', 'Factor B'), swap = FALSE, ylab= NULL, main = NULL){
  means<-c(obj[,1],obj[,2])
  group.means <- means
  max.g1<-length(group.means)/2
  min.g2<-(length(group.means)/2)+1
  max.g2<-length(group.means)
  if(missing(ylab)) ylab <- expression(italic(DV))
  if(swap==TRUE) {
    group.names <- list(group.names[[2]], group.names[[1]]) ; 
    group.means <- means 
    factor.labels <- c(factor.labels[2], factor.labels[1])
  }
  plot(group.means, pch=NA, ylim=c(min(group.means)*.95, max(group.means)*1.025), 
       xlim=c(min(0.8),sum(table(lev.names[1]))+.3), ylab=ylab, xaxt='n', xlab=factor.labels[1], main=main)
  points(group.means[1:max.g1], pch = 21)
  points(group.means[min.g2:max.g2], pch = 19)
  axis(side = 1, at = c(1:max.g1), labels = group.names[[1]])
  lines(group.means[1:max.g1], lwd = .6, lty = 2)
  lines(group.means[min.g2:max.g2], lwd = .6)
  if(missing(leg.loc)) leg.loc <- c(.9,max(group.means))
  if(legend ==TRUE) legend(leg.loc[1], leg.loc[2],legend = group.names[[2]],lty = c(3,1),cex=.7)
  #    legend(.9,leg.loc[2],legend=group.names[[2]],lty=c(1,3),cex=.7)
}



x<-rbind(res1[c(3:4),1],res17[c(3:4),1],res9[c(3:4),1],res9[c(3:4),1],res9[c(3:4),1])
y1<-c("Low", "Med","Strong","Stronger","Strongest")
y2<-c("RE Simple","RE Comp")
lev.names <- list(y1,y2)
plot.sim(x,lev.names,ylab="Beta Mean",legend=TRUE,factor.labels=c('LD Strength','Meta Type'))
