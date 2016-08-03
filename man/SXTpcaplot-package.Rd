\name{SXTpcaplot}
\alias{SXTpcaplot-package}
\alias{SXTpcaplot}
\docType{package}
\title{
SXTpcaplot is used to draw PCA plot

}
\description{
SXTpcaplot is used to draw PCA plot
}

\usage{
SXTpcaplot<-function(sample=NULL,qc=NULL,info=NULL,tags=NULL,
                     #used data
                     width=7,height=7,QC=FALSE,text=FALSE,ellipse=FALSE,
                     color=c("green","red","blue","yellow","black","cyan","gray48",
                             "chocolate4","darkmagenta","indianred1"),
                     shape=c(17,19,15,18,2,8,11,13,12,14),
                     cexlab=1.3,cexaxis=1.3,cexa=1.3,cextext=1
                     #parameter setting)
}

\arguments{\tabular{ll}{
sample: \tab a matrix whose column reorents peak and row represents sample\cr
qc: \tab a matrix whose column reorents peak and row represents qc\cr
info: \tab a matrix whose colum represent the classes of samples\cr
tags: \tab a matrix represent the information of peaks\cr
width&heigh: \tab the width and heigh of plot, default are 7\cr
QC: \tab the qc samples are drawn in the plot or not, default is FALSE\cr
text: \tab the samples' names are writen in the plot or not, default is FALSE\cr
ellipse: \tab the ellipse is drawn in the plot or not, default is FALSE\cr
color: \tab the colors for the different classes of samples,\cr
\tab default are c("green","red","blue","yellow","black","cyan","gray48")\cr
shape: \tab the shapes for the different classes of samples, default are c(17,19,15,18,2,8,11)\cr
scalemethod: \tab which scale method you want to use? auto, pareto and center\cr
}
}

\details{
\tabular{ll}{
Package: \tab SXTpcaplot\cr
Type: \tab Package\cr
Version: \tab 2.2\cr
Date: \tab 2015-08-03\cr
License: \tab What license is it under?\cr
pcaplot 2d t1 vs t2: \tab the PCA plot respect to PC1 and PC2\cr
pcaplot 2d t1 vs t3: \tab the PCA plot respect to PC1 and PC3\cr
pcaplot 2d t2 vs t3: \tab the PCA plot respect to PC2 and PC3\cr
pcaplot 3d: \tab the PCA plot respect to PC1, PC2 and PC3\cr
sample.pca: \tab the pca model of sample\cr
version2.2: \tab the name who is not in the sample is discarded from info\cr
}
}



\author{
Jasper Shen

Maintainer: Jasper Shen <shenxt1990@163.com>
}
\references{
nothing
}

\keyword{ package }
\seealso{
\code{\link[<pkg>:<pkg>-package]{<pkg>}}
}
\example{
nothing
}
