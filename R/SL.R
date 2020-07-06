#' @title Synthetic lethal analysis
#'
#' @description
#' @param mutation a vector for samples that are wildtype or mutation for a gene.which 1 represent mutation, 2 represent wildtype and 0 represent NA.
#' @param data a data frame,including 3 columns ('sample','expression','mutation') for one gene.
#' @param expression a vector of expression data. it is equal to the number of rows for "expres" data.
#' @param exp_type the expression data formula mode. including 'RPKM','FPKM','TPM'.

SL<-function(mutation="mutation",expression="expression",exp_type="rpkm",data){
  result=list()
   if(dim(data)[1]<4){
    stop("The number of samples not more tha 3 !")

  }else{
    mut=as.numeric(data[,mutation])
    mut_s=which(as.numeric(mut)==1)
    wild_s=which(as.numeric(mut)==2)
    NA_s=which(as.numeric(mut)==0)
    expression=as.numeric(data[,expression])
    se=dim(data)[1]-length(NA_s)
    if(se<4 | length(mut)<4 | length(wild_s)<4){
      stop("The number of samples not more tha 3 !")

    }else{
      if(exp_type=="rpkm" | exp_type=="fpkm"){
        test=t.test(log2(expression[mut_s]+1),log2(expression[wild_s]+1),var=TRUE)
        result$P=test$p.value
        result$FC=mean(log2(expression[mut_s]+1))-mean(log2(expression[wild_s]+1))
        result$FC_type="log2(Fold Change)"



      }else{
        test=wilcox.test(expression[mut_s],expression[wild_s],alternative="two.sided")
        result$P=test$p.value
        result$FC_type="two.sided"

      }


    }

    return(result)

  }



#
}
