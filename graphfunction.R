
library(plotrix) #import library 
graphs<-function(data,x,y)# Here we passing parameter in the function
{ 
  setwd(y)# here we change the directory
  if(!is.data.frame(data))# checking it is data frame or not
    stop("The given object is not a data frame")# stop if it is dataframe or not
  for(i in 1:ncol(data))# iterate all over the column
  { 
    if(is.numeric(data[,i]))#checking numeric or not
      #check if the variable is in x or not
    {if(i %in% x){ # checking if the passing variable is in data set or not
      png(paste(names(data)[i],".png",sep="")) # saving the plot
      par(mfrow=c(2,1)) #divide  the frame
      boxplot(data[,i],main=paste("Boxplot of",names(data)[i]),
              ylab=names(data)[i],col="maroon",border="grey5",horizontal = T)#box plot for univarient
      hist(data[,i],main=paste("Boxplot of",names(data)[i]),
           xlab=names(data)[i],ylab="no of houses",col="lightgreen",border="grey5",horizontal = T)#hist for univarient
      dev.off()
      
    }
      else#if the variable pass is not in dataframe plot all
        png(paste(names(data)[i],".png",sep="")) # saving criteria
      par(mfrow=c(2,1))#divide the frame
      boxplot(data[,i],main=paste("Boxplot of",names(data)[i]),
              ylab=names(data)[i],col="maroon",border="grey5",horizontal = T)
      hist(data[,i],main=paste("Boxplot of",names(data)[i]),
           xlab=names(data)[i],ylab="no of houses",col="lightgreen",border="grey5",horizontal = T)
      dev.off()
    }
    else if(length(unique(data[,i]))<20){ #checking if the categorical variable unique value is less than 20 or not
      png(paste(names(data)[i],".png",sep=""))# save the graph
      par(mfrow=c(2,1))
      barplot(table(data[,i]),
              main = paste("Barplot of",names(data)[i]),
              xlab = names(data)[i],col="lightgreen",border="grey5",
              horizontal = T)
      pie3D(table(data[,i]),main = paste("piechart of",names(data)[i]),labels = unique(data[,i]))
      dev.off()
    }
  }
  #for bivarient numerical
  for(i in 1:ncol(data)){
    
    
    for(j in 1:ncol(data)){
      if(i!=j){
        if(is.numeric(data[,i])){ 
          x <- data.frame(data[i])
          x2 <- data.frame(x, data[j])
          png(paste(paste(names(data)[i]," ","vs"," "),names(data)[j],".png",sep=""))#save the graph
          plot(x2,main=paste(paste("scater plot of",names(data)[i]),"vs",names(data)[j]),
               col="Red",border="grey5")
          
          dev.off()
        }
        
        
      }
    }
    
  }

}
graphs(cars,c(15,17),"C:\\Users\\DURJAY\\Desktop\\class material\\ML\\gra")##funtion call
setwd("C:\\Users\\DURJAY\\Desktop\\class material\\ML\\car")
cars <- read.csv("cars.csv")
View(cars)
