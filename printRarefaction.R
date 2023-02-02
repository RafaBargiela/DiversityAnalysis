# Function to plot rarefaction curves
  printRarefaction<-function(Curvesx,Curvesy,xlim=c(0,15000),xlim.labs=seq(0,xlim[2],by=7500),ylim=c(0,400),cex.axis=1.2,lwd=2,cols="black",tittle=""){
    max<-max(unlist(Curvesx))
    plot(NA,NA,las=1,xlab="",ylab="",xlim=xlim,ylim=ylim,axes=FALSE)
    axis(2,seq(ylim[1],ylim[2],by=100),labels=TRUE,las=1,tick=TRUE,lwd.ticks=2,cex.axis=cex.axis,font.axis=2,lwd=2)
    axis(1,xlim.labs,labels=TRUE,las=1,tick=TRUE,lwd.ticks=2,cex.axis=cex.axis,font.axis=2,lwd=2)
    arrows(0,seq(ylim[1],ylim[2],by=100),xlim[2],seq(ylim[1],ylim[2],by=100),lwd=1,lty=3,col="grey80",code=0,xpd=TRUE)
    text(xlim[1],ylim[2]-((ylim[2]*10)/100),labels=tittle,cex=1.2,font=2,adj=c(0,0.5),xpd=TRUE)
    box(lwd=2)
    for(curve in 1:length(Curvesy)){
      if(length(lwd)>1){
        width<-lwd[curve]
      }else{
        width<-2
      }
      points(Curvesx[[curve]],Curvesy[[curve]],type="l",lwd=width+1,xpd=TRUE,col="black")        
      points(Curvesx[[curve]],Curvesy[[curve]],type="l",lwd=width,xpd=TRUE,col=cols[curve],lty=1)
       if(Curvesx[[curve]][length(Curvesx[[curve]])] < max){

        dat<-data.frame(curve=Curvesy[[curve]],subsample=Curvesx[[curve]]) # creating a data.frame
        mod<-drm(curve~subsample,data=dat,fct=W1.4())      # Calculating model         
        newdata<-seq(dat$subsample[length(dat$subsample)],max,by=50) # data frame for new data
        pre<-as.data.frame(predict(mod,newdata=data.frame(subsample=newdata),interval="predict")) # Getting predictions and coefficients
        
        polygon(x=c(newdata,rev(newdata)),y=c(pre$Lower,rev(pre$Upper)),border=rgb(t(col2rgb(cols[curve])),alpha=100,maxColorValue=255),col=rgb(t(col2rgb(cols[curve])),alpha=100,maxColorValue=255),xpd=TRUE)
        points(newdata,pre$Prediction,type="l",lty=3,lwd=2,col=cols[curve],xpd=TRUE)
      }
    }
  }
