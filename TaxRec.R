# Function to use when drawing heatmaps, barplots or similar with taxonomic distribution
# the function helps to draw lateral rectangles to showing the lineage levels
  TaxRec<-function(M,start=-30,end=-0.2,sepRate=0.01,levels=c(1:ncol(M)),levels.colors=c("royalblue3","indianred3","goldenrod3"),levels.labs.col="black",fix.lab=TRUE,lev.lab.corr.y=NULL,lev.lab.corr.x=NULL,lev.cex=1,lev.font=2,alpha=150, ... ){
        # DEFINITIONS #
        ## sepRate: numeric. Additional proportional separation to rectangles
        ## lev.cex: numeric. Vector with levels labels cex. Must be one value for all or a vector of length equal to number of levels

        # CHECKINGS #
          ## levels and levels colors must be of equal number
          if(is.character(levels.colors)==TRUE | is.list(levels.colors)==TRUE){
            ncols<-length(levels.colors)
          }else{
            if(is.data.frame(levels.colors)==TRUE){
              ncols<-ncol(levels.colors)
            }else{
              stop("levels.colors must be character vector, list or data.frame")
            }
          }
            if(ncols!=length(levels.colors)){
              stop("number of colors (length or number of columns) different to number of levels to represent")
            }
          ## sepRate must be of length 1 or equal to number of levels
            if(length(sepRate)>1 & length(sepRate)!=length(levels)){
              warning("length of sepRate differ from number of levels")
            }
          ## lev.cex must must be of length=1 or equal to levels  
            if(length(lev.cex)>1){
              if(is.list(lev.cex)==TRUE){
                if(length(lev.cex)!=length(levels.colors)){
                  stop("When lev.cex is list: length of lev.cex must be equal to levels")
                }
              }else{
                if(is.numeric(lev.cex)==FALSE){
                  stop("lev.cex must be numeric")
                }
              }
            }
          ## levels.labs.col must be a list of length = length(levels) or is by default set as black color
            if(is.list(levels.labs.col)==FALSE){
                if(is.na(levels.labs.col)==TRUE){
                  message("labels color set to black")
                  labcol<-"black"         
                }else{
                  labcol<-levels.labs.col
                  if(length(levels.labs.col)!=length(levels)){
                      warning("length of levels.labs.col different of number of levels")
                  }
                }
            }
            else{
              if(length(levels.labs.col)!=length(levels)){
                  warning("levels.labs.col must be a list of length=levels")
                  message("labels color set to black")
              }
              labcol<-levels.labs.col
            }
          ## lev.lab.corr.y and lev.lab.corr.x must be numeric if not null
          if(length(lev.lab.corr.y)>0){
            if(is.numeric(lev.lab.corr.y)==FALSE){
              warning("lev.lab.corr.y must be a numeric vector")
            }
          }
          if(length(lev.lab.corr.x)>0){
            if(is.numeric(lev.lab.corr.x)==FALSE){
              warning("lev.lab.corr.x must be a numeric vector")
            }
          }
        # END CHECKINGS #

        # CREATING THE RECTANGLES #
        ## Common values ##
          cex<-1 # Common cex factor for labels and separation
          sep<-0 # Common cex factor for labels and separation

        ## LEVEL BY LEVEL ##
        for(lev in 1:length(levels)){

          levGr<-unique(M[,levels[lev]]) # All group names in the level
          # X and Y labels values
          XL<-start+abs((start*0.01))
          YL<-vector("numeric",length(levGr))

          ## Making the Color palette for each level
            ### When levels.colors is a vector: a palelette is created
            ### with the selected color and a lighter version.
            if(is.character(levels.colors)==TRUE){
              basePal<-colorRampPalette(c(levels.colors[lev],"white"),space="rgb")(3)[1:2]
              Pal<-vector("list")
              for(c in 1:length(basePal)){
                Pal[[c]]<-rgb(t(col2rgb(colorRampPalette(c(basePal[c],"white"),space="rgb")(100))),alpha=alpha,maxColorValue=255)
              }
              PAL<-matrix(unlist(Pal),nc=2)
              seq<-rep(c(1,2),length.out=length(levGr))         
            }
            ### When levels.colors is list
            if(is.list(levels.colors)==TRUE){
                Pal<-vector("list")
                for(e in 1:length(levels.colors[[lev]])){
                  Pal[[e]]<-rgb(t(col2rgb(colorRampPalette(c(levels.colors[[lev]][e],"white"),space="rgb")(100))),alpha=alpha,maxColorValue=255)
                }
                PAL<-matrix(unlist(Pal),nc=length(levels.colors[[lev]]))
                seq<-rep(c(1:length(levels.colors[[lev]])),length.out=length(levGr))
            }
            ### When levels.colors is a data.frame
            if(is.data.frame(levels.colors)==TRUE){ 
                Pal<-vector("list")
                for(r in 1:nrow(levels.colors)){
                  Pal[[r]]<-rgb(t(col2rgb(colorRampPalette(c(levels.colors[r,lev],"white"),space="rgb")(100))),alpha=alpha,maxColorValue=255)
                }
                PAL<-matrix(unlist(Pal),nc=nrow(levels.colors))
                seq<-rep(c(1:nrow(levels.colors)),length.out=length(levGr))
            } 
          ## END making color palette for each level

          ## Checking size of each level group and drawing rectangles
          for (gr in 1:length(levGr)){

            if(grepl("^$",levGr[gr])==TRUE){
              YL[gr]<-0
            }else{
              G<-grep(levGr[gr],M[,levels[lev]])
              yi<-nrow(M)+1-min(G)+0.5 # to start from top
              yf<-nrow(M)+1-max(G)-0.5 # to end on the bottom
              YL[gr]<-(yi+yf)/2         # y values for labels below
              xi<-start
              w<-abs(xi-end)/nrow(PAL)
              rect(xi,yi,end,yf,border=NA,col="white",xpd=TRUE)
              for(c in 1:nrow(PAL)){
                rect(xi,yi,xi+w,yf,border=NA,col=PAL[c,seq[gr]],xpd=TRUE)
                xi<-xi+w      
              }
            }         
          }
          ## END drawing rectangles

          ## Writting rectangles labels
            for(gr in 1:length(levGr)){
              if(grepl("^$",levGr[gr])==TRUE){

              }else{
                if(is.list(levels.labs.col)==TRUE){
                  seqlabs<-rep(c(1:length(levels.labs.col[[lev]])),length.out=length(levGr))
                  labcol<-levels.labs.col[[lev]][seqlabs[gr]]
                }else{
                    labcol<-levels.labs.col
                }
                if(fix.lab==TRUE){
                  l<-levGr[gr]
                  if(grepl("Unc.",l)==TRUE){
                    n<-unlist(strsplit(l,split=" "))
                    l<-as.expression(bquote(bold("Unc.")~bolditalic(.(n[2]))))
                  }else{
                    if(grepl("-Proteobacteria",l)==TRUE){
                      n<-unlist(strsplit(l,split="-"))
                      l<-as.expression(bquote(bold(.(n[1]))))
                    }else{
                      if(grepl("Other Phyla",l)==TRUE){
                        l<-as.expression(bquote(bold(.(l))))
                      }else{
                        l<-as.expression(bquote(bolditalic(.(l))))
                      }
                    }
                  }
                }else{
                  l<-levGr[gr]
                }
                yc<-0
                xc<-0
                if(is.null(lev.lab.corr.y)==FALSE){
                  if((levGr[gr]%in%names(lev.lab.corr.y))==TRUE){
                    yc<-lev.lab.corr.y[levGr[gr]]
                  }else{
                    yc<-0
                  }
                } 
                if(is.null(lev.lab.corr.x)==FALSE){
                  if((levGr[gr]%in%names(lev.lab.corr.x))==TRUE){
                    xc<-lev.lab.corr.x[levGr[gr]]
                  }else{
                    xc<-0
                  }
                }
                if(is.list(lev.cex)==TRUE){
                  cex<-lev.cex[[lev]][gr]
                  print(cex)
                }else{
                  if(length(lev.cex)>1){
                      if((levGr[gr]%in%names(lev.cex))==TRUE){
                        cex<-lev.cex[levGr[gr]]
                      }else{
                        cex<-lev.cex[lev]
                      }
                  }else{
                    cex<-lev.cex
                  }
                }
                text(XL+xc,YL[gr]+yc,labels=l,cex=cex,font=2,col=labcol,xpd=TRUE,adj=c(0,0.5), ...)
              }
            }
          ## END writting labels

          ## Adding separation among each level
          if(lev<length(levels)){
            if(length(sepRate)>1){
                sr<-sepRate[lev]
            }else{
              sr<-sepRate
            }
            sep<-max(strwidth(M[,lev],units="user",cex=cex,font=lev.font))
            start<-start+sep*(1+sr) # sepRate is additional proportional separation
          }
        }          
  }
