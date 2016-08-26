SXTpcaplot<-function(sample=NULL,qc=NULL,info=NULL,tags=NULL,
                     #used data
                     width=7,height=7,QC=FALSE,text=FALSE,ellipse=FALSE,scalemethod="auto",
                     color=c("green","red","blue","yellow","black","cyan","gray48",
                             "chocolate4","darkmagenta","indianred1"),
                     shape=c(17,19,15,18,2,8,11,13,12,14),
                     cexlab=1.3,cexaxis=1.3,cexa=1.3,cextext=1
                     #parameter setting
) {
  # browser()
  if (is.null(sample))  stop("sample is NULL")
  if (!is.null(qc)) {if (ncol(sample)!=ncol(qc)) stop("the column number of sample and qc must same")}
  if (is.null(qc)&QC) stop("QC shoud be FALSE because qc is NULL")
  if (is.null(info)) stop("info must not be NULL")

  int<-sample

  #select the samples in info and need QC or not
  index<-NULL
  for (i in 1:length(info)) {
    index1<-as.character(info[[i]])
    index<-c(index,index1)
  }
  if (length(which(index==""))!=0)  {index<-index[-which(index=="")]}

  index<-index[!is.na(index)]
  index<-match(index,rownames(int))
  index<-index[!is.na(index)]
  int<-int[index,]


  ##discard the sample's name who is not in the sample data
  for (i in 1:length(info)) {
    idx <- as.character(info[[i]])
    idx <- match(idx,rownames(int))
    idx <- idx[!is.na(idx)]
    info[[i]] <- rownames(int)[idx]
  }

  ifelse(QC, int<-rbind(int,qc) ,int <- int)
  name <- rownames(int)
  #browser()
  q <- grep("QC",name)

  if (scalemethod=="auto") {int<-scale(int)}
  if (scalemethod=="pareto") {int<-apply(int,2,function(x) {(x-mean(x))/sqrt(sd(x))})}
  if (scalemethod=="no") {int<-int}
  if (scalemethod=="center") {int<-apply(int,2,function(x) {(x-mean(x))})}

  int[is.na(int)] <- 0
  int[is.infinite(int)] <- 0

  int.pca<-prcomp(data.frame(int),retx=TRUE,center=FALSE,scale = FALSE)
  loading<-summary(int.pca)$rotation
  rownames(loading)<-tags["name",]
  pov<-summary(int.pca)$importance[2,]
  sd<-summary(int.pca)$importance[1,]
  cp<-summary(int.pca)$importance[3,]
  pc<-int.pca$x

  pc1<-pov[1]
  pc2<-pov[2]
  if(ncol(sample) >=3) {pc3<-pov[3]}

  x<-pc[,1]
  y<-pc[,2]
  if(ncol(sample) >=3) {z<-pc[,3]}

  xmin<-1.2*min(x)
  xmax<-1.2*max(x)
  ymin<-1.2*min(y)
  ymax<-1.2*max(y)
  if(ncol(sample) >=3) {
  zmin<-1.2*min(z)
  zmax<-1.2*max(z)
}
  label<-list()
  for (i in 1:length(info)) {
    label[[i]]<-match(as.character(info[[i]]),name)
    label[[i]]<-label[[i]][!is.na(label[[i]])]
  }

  legend<-NULL
  for (i in 1:length(label)) {
    legend[label[[i]]] <- names(info)[i]
  }

  if (QC) {legend[q] <- "QC"}

  colour<-NULL
  if (length(color) < length(info)) stop("color list is not enough")

  colourlist<-color
  for (i in 1:length(label)) {
    colour[label[[i]]]<-colourlist[i]
  }
  if (QC) {colour[q]<-colourlist[length(info)+1]}

  pcha<-NULL
  if (length(shape)< length(info)) stop("shape list is not enough")
  pchalist <- shape
  for (i in 1:length(label)) {
    pcha[label[[i]]]<-pchalist[i]
  }
  if (QC) {pcha[q]<-pchalist[length(info)+1]}

  #laoding plot
  pdf("loading plot 1 vs 2.pdf",width=width,height=height)
  plot(loading[,1],loading[,2],pch=20,xlab="Component 1",ylab="Component 2",
       cex.lab=cexlab,cex.axis=cexaxis)
  abline(v=0,lty=2)
  abline(h=0,lty=2)
  dev.off()

  if(ncol(sample) >=3) {
  pdf("loading plot 2 vs 3.pdf",width=width,height=height)
  plot(loading[,2],loading[,3],pch=20,xlab="Component 2",ylab="Component 3",
       cex.lab=cexlab,cex.axis=cexaxis)
  abline(v=0,lty=2)
  abline(h=0,lty=2)
  dev.off()

  pdf("loading plot 1 vs 3.pdf",width=width,height=height)
  plot(loading[,1],loading[,3],pch=20,xlab="Component 1",ylab="Component 3",
       cex.lab=cexlab,cex.axis=cexaxis)
  abline(v=0,lty=2)
  abline(h=0,lty=2)
  dev.off()

  #loading plot 3d
  require(scatterplot3d)
  pdf("loading plot 3d.pdf",width=width,height=height)
  scatterplot3d(loading[,1],loading[,2],loading[,3],xlab="Component 1",ylab="Component 2",
                zlab="Component 3",angle=40,
                pch=20,box=FALSE,cex.symbol=1,cex.lab=1.3,cex.axis=0.8)
  dev.off()
}

  browser()

  #PCA 2D
  pdf("pcaplot 2d pc1 vs pc2.pdf",width=width,height=height)
  #t1 vs t2 plot
  par(mar=c(5,5,4,2))
  plot(x,y,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col=colour,pch=pcha,
       xlab=paste("PC1:",pc1,sep=""),ylab=paste("PC2:",pc2,sep=""),
       cex=cexa,cex.axis=cexaxis,cex.lab=cexlab)
  if (text)
  {text(x,y,name,pos=4,cex=cextext)}

  abline(v=0,lty=2)
  abline(h=0,lty=2)

  if (ellipse)
  { require(ellipse)
    lines(ellipse::ellipse(0,scale=c(sd(x),sd(y)),centre=c(mean(x),mean(y))),lty=2)}

  if (QC) {
    legend("topleft",c(names(info),"QC"),
           pch=pchalist[1:(length(info)+1)],col=colourlist[1:(length(info)+1)],bty="n",cex=1.1)
  }
  else{
    legend("topleft",names(info),
           pch=pchalist[1:length(info)],col=colourlist[1:length(info)],bty="n",cex=1.1)
  }
  dev.off()


  if(ncol(sample) >=3) {
  #t2 vs t3 plot
  pdf("pcaplot 2d pc2 vs pc3.pdf",width=width,height=height)
  par(mar=c(5,5,4,2))
  plot(y,z,xlim=c(ymin,ymax),ylim=c(zmin,zmax),col=colour,pch=pcha,
       xlab=paste("PC2:",pc2,sep=""),ylab=paste("PC3:",pc3,sep=""),
       cex=cexa,cex.axis=cexaxis,cex.lab=cexlab)
  if (text)
  {text(y,z,name,pos=4,cex=cextext)}
  abline(v=0,lty=2)
  abline(h=0,lty=2)
  if (ellipse)
  {  lines(ellipse::ellipse(0,scale=c(sd(y),sd(z)),centre=c(mean(y),mean(z))),lty=2)}


  if (QC) {
    legend("topleft",c(names(info),"QC"),
           pch=pchalist[1:(length(info)+1)],col=colourlist[1:(length(info)+1)],bty="n",cex=1.1)
  }
  else{
    legend("topleft",names(info),
           pch=pchalist[1:length(info)],col=colourlist[1:length(info)],bty="n",cex=1.1)
  }
  dev.off()
  #t1 vs t3 plot
  pdf("pcaplot 2d pc1 vs pc3.pdf",width=width,height=height)
  plot(x,z,xlim=c(xmin,xmax),ylim=c(zmin,zmax),col=colour,pch=pcha,
       xlab=paste("PC1:",pc1,sep=""),ylab=paste("PC3:",pc3,sep=""),
       cex=1.3,cex.axis=1.3,cex.lab=1.3)
  if (text)
  {text(x,z,name,pos=4,cex=cextext)}
  abline(v=0,lty=2)
  abline(h=0,lty=2)
  if (ellipse)
  {lines(ellipse::ellipse(0,scale=c(sd(x),sd(z)),centre=c(mean(x),mean(z))),lty=2)}


  if (QC) {
    legend("topleft",c(names(info),"QC"),
           pch=pchalist[1:(length(info)+1)],col=colourlist[1:(length(info)+1)],bty="n",cex=1.1)
  }
  else{
    legend("topleft",names(info),
           pch=pchalist[1:length(info)],col=colourlist[1:length(info)],bty="n",cex=1.1)
  }

  dev.off()
  #PLS 3D
  require(scatterplot3d)
  pdf("pcaplot 3d.pdf",width=width,height=height)
  scatterplot3d(x,y,z,color=colour,xlab=paste("PC1:",pc1,sep=""),ylab=paste("PC2:",pc2,sep=""),zlab=paste("PC3:",pc3,sep=""),angle=40,
                pch=pcha,box=FALSE,cex.symbol=1,cex.lab=1.3,cex.axis=1.3,
                xlim=c(xmin,xmax),ylim=c(ymin,ymax),zlim=c(zmin,zmax))

  if (QC) {
    legend("topleft",c(names(info),"QC"),
           pch=pchalist[1:(length(info)+1)],col=colourlist[1:(length(info)+1)],bty="n",cex=1.5)
  }
  else{
    legend("topleft",names(info),
           pch=pchalist[1:length(info)],col=colourlist[1:length(info)],bty="n",cex=1.5)
  }
  dev.off()
  }

  sample.pca<-int.pca
  save(sample.pca,file="sample.pca")
  cat("PCA plot is done\n")
}
