# Function to draw bars and error bars based on abundance
# It needs to call an initial plot first
  barPlot<-function(M,E=NA,horiz=TRUE,samsep=0.375,cols=colorRampPalette(c("grey95","black"),space="rgb")(ncol(M)),border.cols=rep("black",length.out=ncol(M)),ebars=FALSE,ebars.length=0.025,arrows=NULL,vertical.lab=NULL,ver.lab.cols="black",ver.lab.cex=1,ver.lab.sep=0.1,x.axis.lab.sep=0.1,y.axis.lab.sep=0.05,ver.let.sep=0.8, ... ){

    # DEFINITIONS #
    # ver.lab.sep: Separation rate between different vertical labels
    # x.axis.lab.sep: Separation of the whole vertical labs set from the right barplot boundary
    # y.axis.lab.sep: Added separation of the whole vertical labs set from the top of the barplot
    # ver.let.sep: Separation rate between vertical labs letters

    cs<-0.75/ncol(M) # Column separation

    for (r in 1:nrow(M)){
      y<-((nrow(M)+1)-r)
      
      if(horiz==FALSE){
        y<-y-samsep
        for(c in 1:ncol(M)){
          if(M[r,c]>0){
            rect(0,y,M[r,c],y+cs,border=border.cols[c],col=cols[c])
          }
          y<-y+cs
        }
      }
      if(horiz==TRUE){      # In case barplot with horizontal bars
        y<-y+samsep
        for(c in 1:ncol(M)){
          if(M[r,c]>0){
            rect(0,y,M[r,c],y-cs,border=border.cols[c],col=cols[c])
            if(ebars==TRUE){
                arrows(M[r,c]-E[r,c],y-(cs/2),M[r,c]+E[r,c],y-(cs/2),angle=90,code=2,length=ebars.length,lty=1)
            }
        }
          y<-y-cs
        }
      }
      
    }
    if(!is.null(arrows)){
      if(horiz==TRUE){
        arrows(arrows,0.5,arrows,nrow(M)+0.5,lwd=1,lty=3,col="grey60",code=0,xpd=TRUE)
      }else{

      }
    }

    if(!is.null(vertical.lab)){
          xt<-par("usr")[2]-(par("usr")[2]*x.axis.lab.sep)
          yt<-par("usr")[4]-(par("usr")[4]*y.axis.lab.sep)
          for(lab in 1:length(vertical.lab)){
            t<-unlist(strsplit(as.character(vertical.lab[lab]),split=""))
            for(l in 1:length(t)){
              shadowtext(xt,yt,labels=t[l],font=2,cex=ifelse(length(ver.lab.cex)>1,ver.lab.cex[l],ver.lab.cex),adj=c(0.5,0.5),xpd=TRUE,col=ifelse(length(ver.lab.cols)>1,ver.lab.cols[lab],ver.lab.cols),bg="white", ...)
              if(l==length(t)){
                xt<-par("usr")[2]-(par("usr")[2]*x.axis.lab.sep)
                yt<-par("usr")[4]-(par("usr")[4]*y.axis.lab.sep)
                if(horiz==TRUE){
                  xt<-xt-(ver.lab.sep*par("usr")[2])*lab
                }else{
                  yt<-yt-(ver.lab.sep*par("usr")[4])*lab
                }
              }else{
                if(horiz==TRUE){
                  yt<-yt-(ver.let.sep*ver.lab.cex)
                }else{
                  xt<-xt-(ver.let.sep*ver.lab.cex)                
                }

              }
            }

          }
    }

  }
