Top=function(object,n){

FC=as.numeric(object$FC)
order=order(abs(FC),decreasing =T)
order=order[1:n]
object$FC=FC[order]
object$target=object$target[order]
object$reg=object$reg[order]
object$FDR=object$FDR[order]
object$output=object$output[order,]
return(object)
}
