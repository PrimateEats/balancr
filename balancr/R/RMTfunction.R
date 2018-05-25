#' Right-angle Mixture Triangles
#' 
#' This function generates a right-angle mixture triangle demonstrating the nutrient balance of foods. 
#' All you need to do is input 3 nutrients and a grouping variable, and voila!
#' @param data the dataframe holding your data
#' @param id your grouping variable or sample ID
#' @param x X axis nutrient (will be labelled on an energy (kCal) basis)
#' @param y Y axis nutrient (will be labelled on an energy (kCal) basis)
#' @param z Z/implicit axis nutrient (will be labelled on an energy (kCal) basis)
#' @return dataframe of proportional nutrient data, along with right-angle mixture triangle for data
#' @examples 
#' RMTfunction(gorillafood,age,carb,fat,protein)
#' @export

RMTfunction=function(data,id,x,y,z){
  idlabel=deparse(substitute(id))
  xlabel=deparse(substitute(x))
  ylabel=deparse(substitute(y))
  zlabel=deparse(substitute(z))
  arguments=as.list(match.call())
  id=eval(arguments$id,data)
  x=eval(arguments$x,data)
  y=eval(arguments$y,data)
  z=eval(arguments$z,data)
  df1=data.frame(id,x,y,z)
  df2=cbind(df1[1], prop.table(as.matrix(df1[-1]), margin = 1))
  palette=c("chartreuse3","orange2","cornflowerblue","grey47","mediumpurple3","black","deeppink","darkblue","forestgreen",
            "chocolate","grey65","yellow3","lavenderblush4","darkgoldenrod2","slateblue4","brown1","steelblue4","mediumvioletred","tomato4","royalblue1",
            "seagreen","tan2","lightcoral","red")
  par(pin=c(4.93,5))
  plot(df2[,3]~df2[,2],cex=1.4, xlab=paste("Proportion",xlabel,"energy (kCal)"),
       ylab=paste("Proportion",ylabel,"energy (kCal)"),col=palette[as.factor(df1[,1])],pch=c(15,16,17,18,19,20,3,4,8,13,21,22,23,24,25)[as.factor(df1[,1])],xlim=c(0,1),ylim=c(0,1))
  legend.cols = as.numeric(as.factor(levels(df1[,1])))
  par(xpd=FALSE)
  abline(1,-1,col="red",lwd=1.5)
  abline(0.8,-1,lty="dashed",col="darkgrey",lwd=0.5)
  abline(0.6,-1,lty="dashed",col="darkgrey",lwd=0.5)
  abline(0.4,-1,lty="dashed",col="darkgrey",lwd=0.5)
  abline(0.2,-1,lty="dashed",col="darkgrey",lwd=0.5)
  text(0.3,0.6,paste("Proportion",zlabel,"energy (kCal)"),srt=atan(-1)/(2*pi)*360,adj=c(0,-3))
  par(xpd=TRUE)
  legend('topright', inset=0.09, title=paste(idlabel),legend=unique(df1[,1]), 
         text.font=1, bg="lightgrey",box.lty = 0, cex = 0.8, 
         pch = c(15,16,17,18,19,20,3,4,8,13,21,22,23,24,25)[as.numeric(as.factor(unique(df1[,1])))], 
         col=palette[as.numeric(as.factor(unique(df1[,1])))])
}