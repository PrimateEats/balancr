#' Groupwise Rail Plot
#' 
#' This function generates a rail plot demonstrating the nutrient balance of foods. It then plots the nutrient rails (intake rails) for each group delineated in your data set 
#' All you need to do is input 2 nutrients and a grouping variable, and voila!
#' @param data the dataframe holding your data
#' @param id your grouping variable
#' @param x X axis nutrient (will be labelled on an energy (kCal) basis)
#' @param y Y axis nutrient (will be labelled on an energy (kCal) basis)
#' @return A complete plot demonstrating the intake rail for EACH GROUP (see Railfunction for total dataset). The groups intake ratio will be printed close to the cluster, and identified through the group ID. Bear with me, it's a work in progress!
#' @examples 
#' Railfunction(baboondailyintake,individual,carb,fat)
#' @export


Groupwiserail=function(data,id,x,y){
  idlabel=deparse(substitute(id))
  xlabel=deparse(substitute(x))
  ylabel=deparse(substitute(y))
  arguments=as.list(match.call())
  id=eval(arguments$id,data)
  x=eval(arguments$x,data)
  y=eval(arguments$y,data)
  df1=data.frame(id,x,y)
  originpoint=c(0)
  df2 <- cbind(df1[,2], df1[,3])
  df2 <- as.data.frame(df2)
  indexx <- which.min(df2$V2)
  indexy <- which.min(df2$V1)
  row.in.dfx <- df2[indexx,]
  row.in.dfy <- df2[indexy,]
  df3=rbind(originpoint,row.in.dfx)
  df4=rbind(originpoint,row.in.dfy)
  palette=c("chartreuse3","orange2","cornflowerblue","grey47","mediumpurple3","black","deeppink","darkblue","forestgreen",
            "chocolate","grey65","yellow3","lavenderblush4","darkgoldenrod2","slateblue4","brown1","steelblue4","mediumvioletred","tomato4","royalblue1",
            "seagreen","tan2","lightcoral","red")
  plot(df1[,3]~df1[,2],cex=1.4, xlab=paste(xlabel,"energy"),
       ylab=paste(ylabel,"energy"),col=palette[as.factor(df1[,1])],
       pch=c(15,16,17,18,19,20,3,4,8,13,21,22,23,24,25)[as.factor(df1[,1])],
       xlim=range(df1[,2],df1[,3]),ylim=range(df1[,2],df1[,3]))
  par(xpd=TRUE)
  legend.cols = as.numeric(as.factor(levels(df1[,1])))
  legend('topright', inset=c(-0.2,0.01), title=paste(idlabel),legend=unique(df1[,1]), 
         text.font=1, bg="lightgrey",box.lty = 0, cex = 0.8, 
         pch = c(15,16,17,18,19,20,3,4,8,13,21,22,23,24,25)[as.factor(unique(df1[,1]))], 
         col=palette[as.factor(unique(df1[,1]))]) 
  par(xpd=FALSE)
  abline(lm(df3$V2~df3$V1-1),col="black",lty=2,lwd=2)
  abline(lm(df4$V2~df4$V1-1),col="black",lty=2,lwd=2)
  df1$id2=as.numeric(df1$id)
  nid=max(df1$id2)
  for (i in 1:nid) { 
    group <- subset(df1, id2==i)
    abline(lm(group$y~group$x-1), col=palette[i], lwd=1.5)
  } 
  for (i in 1:nid){
    group <- subset(df1,id2==i)
    nutratio=paste("1",":",round(mean(group$y)/mean(group$x),digits=2))
    par(xpd=TRUE)
    text(group$x[1],group$y[1],paste(unique(group$id),"-",nutratio,xlabel,"to",ylabel,"energy"),cex=0.7,pos=4)
  }
}
