
#install.packages('robustHD')

library(robustHD)

#RobustHD package has a winserize function as well

custom_winsorize<-function (x, fraction=.01)
{
   if(length(fraction) != 1 || fraction < 0 ||
         fraction > 0.5) {
      stop("bad value for 'fraction'")
   }
   lim <- quantile(x, probs=c(fraction, 1-fraction))
   print (lim)

   x[ x < lim[1] ] <- lim[1]
   x[ x > lim[2] ] <- lim[2]
   x
}

