makeunique <- function(data)#Function to make the dataset unique -> Max VEI
{
  u = unique(data)
  rowcount = 1
  x = u[order(u$Activity.ID, u$VEI),]
  y = data[1,]
  for (i in 1:(nrow(x)-1))
  {
    if (!is.na(x[i,4]) && !is.na(x[i+1,4]) && x[i,4]==x[i+1,4])#Col 4 is activity ID
    {
      if (x[i,'VEI']>x[i+1,'VEI'])
      {
        y[rowcount,]= x[i,]
        rowcount = rowcount+1
      }
    }
    else {
      y[rowcount,]=x[i,]
      rowcount = rowcount+1
    }
  }
  return(y)
}

#####cumDistPlot#####
#Plots the cumulative distribution of the start year.
#Requires data to have a column named 'Start.Year'
cumDistPlot <- function(data, by = 1, add = FALSE, col = "black", xlab = NULL, ylab = NULL, main = NULL ){
  duration = data$Start.Year
  breaks = seq(min(duration, na.rm = TRUE),max(duration,na.rm = TRUE), by = by)
  duration.cut = cut(duration, breaks, right = FALSE)
  duration.freq = table(duration.cut)
  cumfreq = c(0, cumsum(duration.freq))
  if (add == FALSE){
    plot(breaks, cumfreq, col = col, xlab = xlab, ylab = ylab, main = main)  
  }
  else {
    points(breaks, cumfreq, col = col, xlab = xlab, ylab = ylab, main = main)
  }
}

#####Cumulative events plot#####
cumEvents <- function(data, add = FALSE, col = "black", xlab = NULL, ylab = NULL, main = NULL){
  
  duration = data$Start.Year
  if (add == FALSE){
    #Window Settings
    #windows(1600,1200, xpinch = 1200, ypinch = 1200)
    par(cex=1.0, bty ="l", lwd = 1, mar = c(3.4,4,2,1) + 0.1, mgp = c(2.2, 1, 0), xaxp = c(1500, 2000, 500))#cex 2.5 #mar = c(3.4,3.4,1,1)
      #Plot
    plot(sort(duration), seq(1, length(sort(duration)), by = 1), col = col,  
         type = "p",  xlab = "Year", ylab = "Cumulative number of recorded eruptions", pch =20, yaxs = "r")  
  }
  else {
    #lines(sort(duration), seq(1, length(sort(duration)), by = 1), col = col)
    points(sort(duration), seq(1, length(sort(duration)), by = 1), col = col, pch =20)
  }
}