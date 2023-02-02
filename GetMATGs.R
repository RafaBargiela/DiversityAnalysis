# Function to get Most Abundant Taxonomic Groups from taxonomy or ASV table  
  GetMATGs<-function(M,lineage=c(1:6),cutoff=2,min.samples=1,exceptions=NA,rel.ab.=TRUE,ellaborate=FALSE, ... ){
      # M : data.frame with the lineage data and data for RELATIVE ABUNDANCES
      # lineage: integer vector with the taxonomy levels of the lineage to
        # evaluate. They must match their corresponding column on the table, sorted
        # from highest level (eg. Domain) to lowest (eg. Genus). Therefore, by
        # default lineage is set as c(1:6), analysing all taxon levels from
        # "Domain" (1) to "Genus" (6).
      # exceptions: Character vector containing those groups which will be allways in the final MAGs table, regardless the cutoff.
      # rel.ab.: Logical. If TRUE, M already contains relative abundances. If FALSE, relative abundances are calculated.
      # ellaborate: Logical. If FALSE, only Most Abundant Groups (MAGs) table is returned. If TRUE, an ellaborated object with MAGs and M in relative abundaces as attributes is returned.
      # min.samples : minimum number of samples were the group must be present

      Ml<-as.data.frame(M[,lineage]) # Columns with lineages
      Ma<-as.data.frame(M[,-lineage]) # Samples relative abundances
      if(rel.ab.==TRUE){
        message("Get Most Abundant Taxonomic Groups:\nMake sure input Matrix or DF represents relative abundance in percentages (%)")
      }else{
        message("Get Most Abundant Taxonomic Groups:
          Calculating Relative abundances")
        Ma<-as.data.frame(apply(Ma,2,function(x){
          (x*100)/sum(x)
        }))
      }

      MAG<-data.frame()
      accRows<-vector("numeric")
      for(l in rev(lineage)){
        levelTax<-unique(Ml[,l])
        for(ll in 1:length(levelTax)){
          # Checking that tax name is not undefined, if not
          # All rows with the same taxonomic level name are save in G
          if(levelTax[ll]!="" && grepl("^uncultured|unidentified",levelTax[ll])==FALSE){
            G<-grep(levelTax[ll],Ml[,l],fixed=TRUE)
            if(length(accRows)>0){
              G<-G[! G %in% accRows]
            }
            # print(paste(lineage[l],levelTax[ll],G))
            # Checking accumulated abundance by row on G
            if((max(colSums(Ma[G,]))>=cutoff & sum(colSums(Ma[G,]))>=min.samples) || l==lineage[1] || (is.na(exceptions)==FALSE & grepl(paste(exceptions,sep="",collapse="|"),levelTax[ll],fixed=TRUE)==TRUE)){
                rowname<-paste(Ml[G[1],1:l],sep="",collapse="|")
                MAG<-rbind(MAG,colSums(Ma[G,]))
                rownames(MAG)[nrow(MAG)]<-rowname
                accRows<-append(accRows,G,length(accRows))
            }else{
              next
            }
          }else{
            next
          }
        }
      }
      colnames(MAG)<-colnames(Ma)
      if(ellaborate==TRUE){
        MAGs<-{}
        MAGs$MAG<-MAG
        MAGs$All.relative.abundances<-Ma
        return(MAGs)
      }else{
        return(MAG)       
      }
  }
