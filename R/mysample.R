#' @title Sample Animation
#'
#' @param n the amount drawn from the sample size
#' @param iter the number of iterations to sample and the number of graphs to produce
#' @param time the amount of time before a new graph is made, aka how long between each frame
#'
#' @returns iter number of bar plots of each sample
#' @export
#'
#' @examples
#' mysample(n = 1000, iter = 30, time = 1)
#'
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
