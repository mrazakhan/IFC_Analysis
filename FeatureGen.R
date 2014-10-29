# Features Generation for Ghana Dataset
library(plyr)
library(reshape2)
library(sm)

setwd("/export/home/mraza/IFC_Analysis/IFC_Analysis")
source('FeatureGenHelper.R')


sdate<-'Oct29th-2014'

inputPath<-'./input_normal/'
outputPath<-'./features_normal/'

prefixes<-c('normal','mm','mmtop')
#prefixes<-c('mmtop')

for( i in prefixes){
  print (i)
  inputPath<-paste(paste('input_',i,sep=''),'/',sep='')
  print(paste('Processing Data for',i))
  fileName<-'dbo.data.txt_random.csv'
  dataFilePath<-paste(inputPath,fileName,sep='')
  dataSummary<-processDataFile(dataFilePath,"NormalDataSummary.csv")
  colnames(dataSummary)<-c('CallerId',
                           'TotalDataTransactions',
                           'TotalDataDistinctDays','TotalDataDistinctLocations')
  
  print(paste('Processing Reload for',i))
  fileName<-'dbo.reloads.txt_random.csv'
  dataFilePath<-paste(inputPath,fileName,sep='')
  lst<-processReloadFile(dataFilePath,"NormalReloadsSummary.csv")
  ReloadsSummary<-lst$ReloadsSummary
  colnames(ReloadsSummary)<-c('CallerId','TotalReloadTransactions','TotalReloadDays',
                              'TotalReloadDistinctLocations')
  
  
  ReloadsTypeSummary<-lst$ReloadsTypeSummary
  ReloadsTypevsDays<-dcast(ReloadsTypeSummary,CallerID~EventType, 
                           value.var='DistinctDays')
  colnames(ReloadsTypevsDays)<-c('CallerId','E-Pin-ReloadDays','EVC-ReloadDays',
                                 'MVOU-ReloadDays','TIGCASH-ReloadDays',
                                 'VOU-ReloadDays')

  #RemovingTIGCash - it is MM
  ReloadsTypevsDays<-ReloadTypevsDays[,!names(ReloadTypevsDays) %in% c('TIGCASH-ReloadDays')]

  ReloadsTypevsTransactions<-dcast(ReloadsTypeSummary,CallerID~EventType, 
                                   value.var='TotalTransactions')
  colnames(ReloadsTypevsTransactions)<-c('CallerId','E-Pin-ReloadTransactions','EVC-ReloadTransactions',
                                         'MVOU-ReloadTransaction',
                                         'TIGCASH-ReloadTransactions','VOU-ReloadTransactions')

  ReloadsTypevsTransactions<-ReloadTypevsTransactions[,!names(ReloadTypevsTransactions) %in% c('TIGCASH-ReloadTransactions')]

  print(paste('Processing Solution for',i))
  fileName<-'dbo.solution.txt_random.csv'
  dataFilePath<-paste(inputPath,fileName,sep='')
  soln<-processSolutionFile(dataFilePath,"NormalSolutionSummary.csv")
  SolutionsSummary<-soln$SolutionsSummary
  colnames(SolutionsSummary)<-c('CallerId','TotalSolutionTransactions',
                                'TotalSolutionDays','TotalSolutionUsed')
  head(SolutionsSummary)
  SolutionsTypeSummary<-soln$SolutionsTypeSummary
  head(SolutionsTypeSummary)
  SolutionsTypevsDays<-dcast(SolutionsTypeSummary,CallerID~EventType,
                             value.var='DistinctDays')
  head(SolutionsTypevsDays)
  colnames(SolutionsTypevsDays)<-c("CallerId","ALLNETSMS-SolutionDays",
                                   "BLACKBERRY-SolutionDays",
                                   "CRBTNewSource-SolutionDays",
                                   "OutgoingSMS-MT-SolutionDays",
                                   "SMART-TEXT-SOlutionDays",                    
                                   "TAG-TRIGGER-SolutionDays",
                                   "TigoNumber1-SolutionDays",
                                   "TigoSOS-SolutionDays",
                                   "TIGOXTRATIME_1MINUTEBORROW-SolutionDays")
  
  SolutionsTypevsTransactions<-dcast(SolutionsTypeSummary,CallerID~EventType,
                                     value.var='TotalTransactions')
  
  colnames(SolutionsTypevsTransactions)<-c("CallerId","ALLNETSMS-SolutionTransactions",
                                           "BLACKBERRY-SolutionTransactions",
                                           "CRBTNewSource-SolutionTransactions",
                                           "OutgoingSMS-MT-SolutionTransactions",
                                           "SMART-TEXT-SOlutionTransactions",                    
                                           "TAG-TRIGGER-SolutionTransactions",
                                           "TigoNumber1-SolutionTransactions",
                                           "TigoSOS-SolutionTransactions",
                                           "TIGOXTRATIME_1MINUTEBORROW-SolutionTransactions")
  head(SolutionsTypevsTransactions,2)
  print(paste('Processing sms for',i))
  fileName<-'dbo.sms.txt_random.csv'
  dataFilePath<-paste(inputPath,fileName,sep='')
  lst<-processSMSFile(dataFilePath,"NormalSMSSummary.csv")
  callerSMSSummary<-lst$callerSummary
  colnames(callerSMSSummary)<-c('CallerId','TotalSMSSent',
                                'TotalSMSSentDays',
                                'TotalSMSRecipients')
  head(callerSMSSummary)
  
  calleeSMSSummary<-lst$calleeSummary
  colnames(calleeSMSSummary)<-c('CallerId','TotalSMSReceived',
                                'TotalSMSReceivedDays',
                                'TotalSMSSenders')
  
  SMSSummary<-merge(x=callerSMSSummary,y=calleeSMSSummary,by="CallerId",all.x=TRUE)
  
  print(paste('Processing voice for',i))
  fileName<-'dbo.voice.txt_random.csv'
  dataFilePath<-paste(inputPath,fileName,sep='')
  lst<-processVoiceFile(dataFilePath,"NormalVoiceSummary.csv")
  callerVoiceSummary<-lst$callerSummary
  colnames(callerVoiceSummary)<-c('CallerId','TotalCallsSent',
                                  'TotalCallsSentDays',
                                  'TotalCallsRecipients',
                                  'TotalCallsSentDistinctCallerLocations',
                                  'TotalCallsSentDistinctCalleeLocations')
  calleeVoiceSummary<-lst$calleeSummary
  colnames(calleeVoiceSummary)<-c('CallerId','TotalCallsRecieved',
                                  'TotalCallsRecievedDays',
                                  'TotalCallsRecievedDistinctSenders',
                                  'TotalCallsRecievedDistinctCallerLocations',
                                  'TotalCallsRecievedDistinctCalleeLocations')
  
  VoiceSummary<-merge(x=callerVoiceSummary,y=calleeVoiceSummary,
                      by="CallerId",all.x=TRUE)
  
  print(paste('Processing Combined table for',i))
  combinedTables<-join_all(list(dataSummary,
                                ReloadsSummary,ReloadsTypevsDays,
                                ReloadsTypevsTransactions,
                                SolutionsSummary,
                                SolutionsTypevsDays,
                                SolutionsTypevsTransactions,
                                VoiceSummary,SMSSummary
  ), by = 'CallerId', type = 'full')
  combinedTables[is.na(combinedTables)]<-0
  
  combinedTables$UserType<-i
  
  print(paste('Saving CSV for',i))
  
  write.csv(combinedTables,paste(paste("CombinedTables_",i,sep=''),".csv",sep=""),quote=FALSE)
  
}

combinedTables<-NULL
combinedTablesNormal<-read.csv('CombinedTables_normal.csv')
combinedTablesMM<-read.csv('CombinedTables_mm.csv')

combinedTablesMMTop<-read.csv('CombinedTables_mmtop.csv')
combinedTablesMMTop<-combinedTablesMMTop[!apply(is.na(combinedTablesMMTop), 1, any), ]

combinedTablesNormal<-combinedTablesNormal[!apply(is.na(combinedTablesNormal), 1, any), ]
combinedTablesMM<-combinedTablesMM[!apply(is.na(combinedTablesMM), 1, any), ]

combinedTablesNormal<-combinedTablesNormal[combinedTablesNormal$TotalCallsSent!=0,]
combinedTablesMM<-combinedTablesMM[combinedTablesMM$TotalCallsSent!=0,]
combinedTablesMMTop<-combinedTablesMMTop[combinedTablesMMTop$TotalCallsSent!=0,]
#combinedTablesMM[apply(is.na(combinedTablesMM), 1, any), ]

#combinedTablesNormal<-combinedTablesNormal[!is.na(combinedTablesNormal),]
#combinedTablesMM<-combinedTablesMM[!is.na(combinedTablesMM),]


#data1<-combinedTablesNormal
data1<-rbind(combinedTablesNormal,combinedTablesMM)

cnames<-colnames(data1)

cnames<-cnames[!cnames %in% c("X","CallerId","UserType")] 
#cnames[c("X","CallerId","UserType")]<-NULL



for(each in cnames) {
  print (each)
  data1[,paste('sq',each,sep='-')]<-data1[,each]^2
  data1[,paste('sqrt',each,sep='-')]<-ifelse(data1[,each]>0,sqrt(data1[,each]),0)
  data1[,paste('log2',each,sep='-')]<-ifelse(data1[,each]>0,log2(data1[,each]),0)  
}

write.csv(data1,paste("DataNormalMM-Call1Threshold",".csv",sep=""),quote=FALSE)

#combinedTablesMM<-NULL


#data1<-data1[!is.na(data1),]

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

#replaceInf(data1,0)


data1$UserType <- gsub("normal", "0", data1$UserType)
data1$UserType <- gsub("mm", "1", data1$UserType)
dataLabels <- factor(data1$UserType)
data1 <- subset(data1, select = -c(UserType))



print('Running T test')
x=featuresTtest(data1,dataLabels)

write.csv(x,file=paste(sdate,'TTest-NormalvsMM.csv',sep='-'),quote=FALSE)
#combinedTablesMMTop<-NULL
data1<-rbind(combinedTablesNormal,combinedTablesMMTop)
cnames<-colnames(data1)

cnames<-cnames[!cnames %in% c("X","CallerId","UserType")] 
#cnames[c("X","CallerId","UserType")]<-NULL



for(each in cnames) {  print (each)
                       data1[,paste('sq',each,sep='-')]<-data1[,each]^2
                       data1[,paste('sqrt',each,sep='-')]<-ifelse(data1[,each]>0,sqrt(data1[,each]),0)
                       data1[,paste('log2',each,sep='-')]<-ifelse(data1[,each]>0,log2(data1[,each]),0)  
}

combinedTablesNormal<-NULL
combinedTablesMMTop<-NULL
gc()
write.csv(data1,paste("DataNormalMMTop-Call1Threshold",".csv",sep=""),quote=FALSE)

#data1<-read.csv('DataNormalMMTop-Call1Threshold.csv')
data1$UserType <- gsub("normal", "0", data1$UserType)
data1$UserType <- gsub("mmtop", "1", data1$UserType)
dataLabels <- factor(data1$UserType)
data1 <- subset(data1, select = -c(UserType))

sum(is.na(data1))

print('Running T test')
x=featuresTtest(data1,dataLabels)

write.csv(x,file=paste(sdate,'TTest-NormalvsMMTop.csv',sep='-'),quote=FALSE)
