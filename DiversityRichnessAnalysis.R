# Returns an object with Shannon index, rarefaction curve data and subsample data
# Use it to plot rarefaction curves. It can use models to predict poor curves
  DiversityRichnessAnalysis<-function(M,prediction=FALSE){
    require(vegan)
    DRA<-{}
    M<-as.matrix(M)
    
    ### Diversity indexes ####
    ShannonI<-diversity(t(M),index="shannon") 
    ShannonI<-format(ShannonI,nsmall=2,digits=2)
    DRA$Shannon<-ShannonI
    # Rarefaction curves ####
    Curves<-vector("list")   # Loop were several rarefaction curves are calculated and store in a vector list
    Subsample<-vector("list")
    for(sample in 1:ncol(M)){
      v<-as.numeric(M[,sample])
      curve<-rarefy(v,sample=seq(0,sum(v),by=50))
      Curves[[colnames(M)[sample]]]<-curve[1,]
      Subsample[[colnames(M)[sample]]]<-attributes(curve)$Subsample  

    }
    DRA$rarefaction.curve<-Curves
    DRA$rarefaction.curve.subsample<-Subsample

    if(prediction==TRUE){
        require(drc)
        Predictions<-vector("list")
        max<-max(unlist(DRA$rarefaction.curve.subsample))
            for(sample in 1:length(Curves)){
                # For prediction of additional data on the curve
                if(Subsample[[sample]][length(Subsample[[sample]])] < max){
                      dat<-data.frame(curve=Curves[[sample]],subsample=Subsample[[sample]]) # creating a data.frame
                      mod<-drm(curve~subsample,data=dat,fct=W1.4())               
                      newdata<-seq(dat$subsample[length(dat$subsample)],max,by=50)
                      pre<-as.data.frame(predict(mod,newdata=data.frame(subsample=newdata),interval="predict"))   
                }else{
                  newdata<-NA
                  pre<-NA
                }
                prediction<-cbind(newdata,pre)
                Predictions[[colnames(M)[sample]]]<-prediction
              }
        DRA$rarefaction.curve.prediction<-Predictions
        message("\nCurve predictions have been assessed\n")
    }

    return(DRA)
  }
