
library(vegan)
data(varespec)
vare.dist <- vegdist(varespec)
# Orlóci's Chord distance: range 0 .. sqrt(2)
vare.dist <- vegdist(decostand(varespec, "norm"), "euclidean")
clust.res<-hclust(vare.dist,method="average")

genSpecAccumPerRange  <- function (ebd_diversity)
{
  
  ebd_diversity <- dcast(ebd_all, RANGE  ~ English.India, value.var = "Submission.ID", fun.aggregate = length ) 
  ebd_diversity <- cbind (ebd_diversity[1:2], ebd_diversity[4:ncol(ebd_diversity)])
  
  
  ebd_diversity <- dcast(ebd_all, Submission.ID + RANGE   ~ English.India, value.var = "Submission.ID", fun.aggregate = length ) 
  
  
  plot(diversity(ebd_diversity[2:ncol(ebd_diversity)]), xlab="Ranges", ylab="Shannon Diversity", pch=18, cex=1.5, col="blue") # Shannon Diversity
  
  #specnumber(varespec) # Species Richness
  #plot(specaccum(varespec)) #Species Accumulation Curve
  #specpool(varespec) # Estimated number of species
  
  plot(specaccum(ebd_diversity [3:ncol(ebd_diversity)], method="random"))
  plot(specaccum(ebd_diversity [ebd_diversity$RANGE=="Vazhachal",] [3:ncol(ebd_diversity)], method="random"), add=T)
  plot(specaccum(ebd_diversity [ebd_diversity$RANGE=="Charpa",] [3:ncol(ebd_diversity)], method="random"), add=T)
  plot(specaccum(ebd_diversity [ebd_diversity$RANGE=="Sholayar",] [3:ncol(ebd_diversity)], method="random"), add=T)
  
  attach(mtcars)
  plot(wt, mpg, main="Milage vs. Car Weight", 
       xlab="Weight", ylab="Mileage", pch=18, col="blue")
  #text(wt, mpg, row.names(mtcars), cex=0.6, pos=4, col="red")
  
  
  
  
  # specify the data 
  x <- c(1:10); y <- x; z <- 10/x
  
  # create extra margin room on the right for an axis 
  par(mar=c(5, 4, 4, 8) + 0.1)
  
  # plot x vs. y 
  plot(x, y,type="b", pch=21, col="red", 
       yaxt="n", lty=3, xlab="", ylab="")
  
  # add x vs. 1/x 
  lines(x, z, type="b", pch=22, col="blue", lty=2)
  
  # draw an axis on the left 
  axis(2, at=x,labels=x, col.axis="red", las=2)
  
  axis(1, at=x,labels=x, col.axis="red", las=2)
  
  # draw an axis on the right, with smaller text and ticks 
  axis(4, at=z,labels=round(z,digits=2),
       col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
  
  # add a title for the right axis 
  mtext("y=1/x", side=4, line=3, cex.lab=1,las=2, col="blue")
  
  # add a main title and bottom and left axis labels 
  title("An Example of Creative Axes", xlab="X values",
        ylab="Y=X")
  
  plot(0,0, xlim =c(1, 1000),ylim = c(0,300), type='n')
  
  cl <- rainbow(5)
  lines(-10:10,runif(21,-10,10),col = cl[1],type = 'b')
  runif(21,-10,10)
  
  for (i in 1:5){
    lines(-10:10,runif(21,-10,10),col = cl[i],type = 'b')
  }
}

library(vegan)
data(BCI) 
df <- lapply(c(1,21,41,61,81),function(i)specaccum(BCI[,seq(i,i+19)], method="random"))
plot(df[[1]])
for (i in 2:5) plot(df[[i]],add=T, col=i)