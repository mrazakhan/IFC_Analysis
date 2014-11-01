
featuresTtest <- function(data1,dataLabels){
  library(plyr)
  library(sm)
  combos <- combn(ncol(data1),1)
  #colnos <- seq(1,ncol(data),by=1)

  adply(combos, 2, function(x) {
    ttest <- t.test(data1[, x[1]]~dataLabels)
    descriptors1 <- summary(data1[dataLabels == '0', x[1]])
    descriptors2 <- summary(data1[dataLabels == '1', x[1]])

    out <- data.frame("feature" = colnames(data1)[x[1]]
                      , "min - 0" = descriptors1[1]
                      , "min - 1" = descriptors2[1]
                      #, "1st Qu." = descriptors[2]
                      , "median - 0" = descriptors1[3]
                      , "median - 1" = descriptors2[3]
                      , "mean - 0" = descriptors1[4]
                      , "mean - 1" = descriptors2[4]
                      , "diff of means" = (descriptors1[4] - descriptors2[4])
                      #, "3rd Qu." = descriptors[5]
                      , "max - 0" = descriptors1[6]
                      , "max - 1" = descriptors2[6]
                      , "t.value" = sprintf("%f", ttest$statistic)
                      ,  "df"= ttest$parameter
                      ,  "p.value" = sprintf("%f", ttest$p.value)
    )
    return(out)

  })
}

