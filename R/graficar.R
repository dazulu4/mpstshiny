### FUNCION PARA GRAFICAR MULTIPLES PLOTS

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#
#
# require(ggplot2)
# diagPlot <- function(model){
#   p1<-ggplot(model, aes(model$fitted[,1], residuals(model)))+geom_point()
#   p1<-p1+stat_smooth(method="lm")+geom_hline(yintercept=0, col="red", linetype="dashed")
#   p1<-p1+xlab("Fitted values")+ylab("Residuals")
#   p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
#
#   p2<-ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
#   p2<-p2+geom_abline(aes(qqline(.stdresid)))+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
#   p2<-p2+ggtitle("Normal Q-Q")+theme_bw()
#
#   p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
#   p3<-p3+stat_smooth(method="lm", na.rm = TRUE)+xlab("Fitted Value")
#   p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
#   p3<-p3+ggtitle("Scale-Location")+theme_bw()
#
#   p4<-ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
#   p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
#   p4<-p4+ggtitle("Cook's distance")+theme_bw()
#
#   p5<-ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
#   p5<-p5+stat_smooth(method="lm", na.rm=TRUE)
#   p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
#   p5<-p5+ggtitle("Residual vs Leverage Plot")
#   p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
#   p5<-p5+theme_bw()+theme(legend.position="bottom")
#
#   p6<-ggplot(model, aes(.hat, .cooksd))+geom_point(na.rm=TRUE)+stat_smooth(method="lm", na.rm=TRUE)
#   p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
#   p6<-p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
#   p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
#   p6<-p6+theme_bw()
#
#   return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
# }
#
#
#
# funggcast <- function(dn, fcast){
#
#   en <- max(time(fcast$mean)) # Extract the max date used in the forecast
#
#   # Extract Source and Training Data
#   ds <- as.data.frame(window(dn, end = en))
#   names(ds) <- 'observed'
#   ds$date <- as.Date(time(window(dn, end = en)))
#
#   # Extract the Fitted Values (need to figure out how to grab confidence intervals)
#   dfit <- as.data.frame(fcast$fitted)
#   dfit$date <- as.Date(time(fcast$fitted))
#   names(dfit)[1] <- 'fitted'
#
#   ds <- merge(ds, dfit, all.x = T) # Merge fitted values with source and training data
#
#   # Extract the Forecast values and confidence intervals
#   dfcastn <- as.data.frame(fcast)
#   dfcastn$date <- as.Date(paste(row.names(dfcastn),"01","01",sep="-"))
#   names(dfcastn) <- c('forecast','lo80','hi80','lo95','hi95','date')
#
#   pd <- merge(ds, dfcastn,all= T) # final data.frame for use in ggplot
#   return(pd)
#
# }
