trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)

replaceInf<-function(DT,val){
  do.call(data.frame,lapply(DT, function(x) replace(x, is.infinite(x),val)))
}




processTCDRFile<-function(filePath,outputFileName){
  #Adate=20140514,Type=buy,
  #ReferenceNo=199629033,Channel=SOAP,
  #Parent_reference=0,State=Completed,
  #Initiator_Agent=0271637596,
  #Debtor_Agent=0271637596,
  #Debtor_Amount=-.9,Creditor_Agent=eco_bank.airtime,
  #Creditor_Amount=0.9,
  #Recipient_Agent=0570523030,Orgion_Cell_id=0271637596:620;15;03;30002;36093,
  #Destination_Cell_ID=0271637596:620;15;03;30002;36093,
  #Date=14-MAY-14 07.46.47.626 AM,
  #Sender_Agent="",First_CashIn_YN=N
  
  
  dataFile<-data.frame(Adate=character(0),
                       Type=character(0),
                       ReferenceNo=integer(0),
                       Channel=character(0),
                       Parent_Reference=integer(0),
                       State=character(0),
                       CallerId=character(0),#initiator agent
                       DebtorAgent=character(0),
                       Debtor_Amount=numeric(0),
                       CreditorAgent=character(0),
                       Creditor_Amount=numeric(0),
                       RecipientId=character(0),
                       OrigCellId=character(0),
                       DestCellId=character(0),
                       Date=character(0),
                       SenderAgent=character(0),
                       FirstCashin=character(0))
  dataFile<-read.csv(filePath
                     ,colClasses=c("character",
                                   "character",
                                   "integer",
                                   "character",
                                   "integer",
                                   "character",
                                   "character",
                                   "character",
                                   "numeric",
                                   "character",
                                   "numeric",
                                   "character",
                                   "character",
                                   "character",
                                   "character",
                                   "character",
                                   "character"))
  
  colnames(dataFile)<-c('Adate','Type','ReferenceNo','Channel',
                        'Parent_Reference','State','CallerId',
                        'DebtorAgent','Debtor_Amount',
                        'Creditor_Agent','Creditor_Amount',
                        'RecipientId','OrigCellId','DestCellId','Date',
                        'SenderAgent','FirstCashin'
  )
  dataFile$ReferenceNo<-NULL
  dataFile$Parent_Reference<-NULL
  dataFile$State<-NULL
  dataFile$Channel<-NULL
  dataFile$DebtorAgent<-NULL
  dataFile$Debtor_Amount<-NULL
  dataFile$Creditor_Agent<-NULL
  dataFile$Date<-NULL # Will be using adate
  dataFile$SenderAgent<-NULL
  
  dataFile$CallerId<-substring(dataFile$CallerId,2,nchar(dataFile$CallerId))
  dataFile$ReceipientId<-substring(dataFile$RecipientId,2,nchar(dataFile$RecipientId))
  #dataFile$Date<-sapply(strsplit(dataFile$Date,' '), `[`, 1)# [ is the extraction op 1 is the index to extract
  dataFile[is.na(dataFile)]<-0
  dataUsageCallerSummary<-ddply(dataFile,.(CallerId),summarize,
                                TotalTransactions=length(Adate),
                                DistinctDays=length(unique(Adate)),
                                DistinctTypes=length(unique(Type)),
                                TotalCreditor_Amount=sum(Creditor_Amount),
                                UniqueOrigCell=length(unique(OrigCellId)),
                                UniqueDestCell=length(unique(DestCellId)),
                                uniqueRecievers=length(unique(RecipientId)))
  #write.csv(dataUsageSummary,pasteoutputFileName,quote=FALSE)
  dataUsageCallerSummaryvsType<-ddply(dataFile,.(CallerId,Type),summarize,
                                      TotalTransactions=length(Adate),
                                      DistinctDays=length(unique(Adate)),
                                      DistinctTypes=length(unique(Type)),
                                      TotalCreditor_Amount=sum(Creditor_Amount),
                                      UniqueOrigCell=length(unique(OrigCellId)),
                                      UniqueDestCell=length(unique(DestCellId)),
                                      uniqueRecievers=length(unique(RecipientId)))
  
  dataUsageCalleeSummary<-ddply(dataFile,.(RecipientId),summarize,
                                TotalTransactions=length(Adate),
                                DistinctDays=length(unique(Adate)),
                                DistinctTypes=length(unique(Type)),
                                TotalCreditor_Amount=sum(Creditor_Amount),
                                UniqueOrigCell=length(unique(OrigCellId)),
                                UniqueDestCell=length(unique(DestCellId)),
                                uniqueSenders=length(unique(CallerId)))
  #write.csv(dataUsageSummary,pasteoutputFileName,quote=FALSE)
  dataUsageCalleeSummaryvsType<-ddply(dataFile,.(RecipientId,Type),summarize,
                                      TotalTransactions=length(Adate),
                                      DistinctDays=length(unique(Adate)),
                                      DistinctTypes=length(unique(Type)),
                                      TotalCreditor_Amount=sum(Creditor_Amount),
                                      UniqueOrigCell=length(unique(OrigCellId)),
                                      UniqueDestCell=length(unique(DestCellId)),
                                      uniqueSenders=length(unique(CallerId)))
  #write.csv(dataUsageSummary,paste(outputFileName,"2",sep=""),quote=FALSE)
  
  return(list("CallerSummary"=dataUsageCallerSummary,
              "CallerTypeSummary"=dataUsageCallerSummaryvsType,
              "CalleeSummary"=dataUsageCalleeSummary,
              "CalleeTypeSummary"=dataUsageCalleeSummaryvsType))
}



processReloadFile<-function(filePath,outputFileName){
  
  dataFile<-data.frame(RecordId=integer(0),
                       CallerID=character(0),
                       Date=character(0),
                       Revenue=numeric(0),
                       EventType=character(0))
  dataFile<-read.csv(filePath
                     ,colClasses=c("integer","character",
                                   "character","numeric",
                                   "character"))
  
  colnames(dataFile)<-c('RecordId','CallerID','Date','Revenue',
                        'EventType')
  
  dataFile$Date<-sapply(strsplit(dataFile$Date,' '), `[`, 1)# [ is the extraction op 1 is the index to extract
  
  dataUsageSummary<-ddply(dataFile,.(CallerID),summarize,
                          TotalTransactions=length(Date),
                          DistinctDays=length(unique(Date)),
                          DistinctEvents=length(unique(EventType)))
  write.csv(dataUsageSummary,outputFileName,quote=FALSE)
  
  dataUsageSummary2<-ddply(dataFile,c("CallerID","EventType"),summarize,
                           TotalTransactions=length(Date),
                           DistinctDays=length(unique(Date))
  )
  write.csv(dataUsageSummary,paste(outputFileName,"2",sep=""),quote=FALSE)
  
  return(list("ReloadsSummary"=dataUsageSummary,"ReloadsTypeSummary"=dataUsageSummary2))
}


processSolutionFile<-function(filePath,outputFileName){
  
  dataFile<-data.frame(RecordId=integer(0),
                       CallerID=character(0),
                       Date=character(0),
                       Revenue=numeric(0),
                       EventType=character(0))
  dataFile<-read.csv(filePath
                     ,colClasses=c("integer","character",
                                   "character","numeric",
                                   "character"))
  
  colnames(dataFile)<-c('RecordId','CallerID','Date','Revenue',
                        'EventType')
  
  dataFile$Date<-sapply(strsplit(dataFile$Date,' '), `[`, 1)# [ is the extraction op 1 is the index to extract
  
  dataUsageSummary<-ddply(dataFile,.(CallerID),summarize,
                          TotalTransactions=length(Date),
                          DistinctDays=length(unique(Date)),
                          DistinctEvents=length(unique(EventType)))
  write.csv(dataUsageSummary,outputFileName,quote=FALSE)
  
  dataUsageSummary2<-ddply(dataFile,c("CallerID","EventType"),summarize,
                           TotalTransactions=length(Date),
                           DistinctDays=length(unique(Date))
  )
  write.csv(dataUsageSummary,paste(outputFileName,"2",sep=""),quote=FALSE)
  
  return(list("SolutionsSummary"=dataUsageSummary,"SolutionsTypeSummary"=dataUsageSummary2))
}

processDataFile<-function(filePath,outputFileName){
  dataFile<-data.frame(RecordId=integer(0),Adate=character(0),CallerId=character(0),
                       RecipientId=character(0),Date=character(0),
                       Duration=numeric(0),
                       Revenue=numeric(0),CallerCell=character(0),RecipientCell=character(0))
  dataFile<-read.csv(filePath
                     ,colClasses=c("integer","character",
                                   "character","character","character","numeric","numeric","character",
                                   "character"))
  
  colnames(dataFile)<-c('RecordId','Adate','CallerID','RecipientID','Date','Duration','Revenue',
                        'CallerCell','RecipientCell')
  
  dataFile$RecordId<-NULL
  dataFile$Adate<-NULL
  dataFile$Duration<-NULL
  dataFile$Revenue<-NULL
  dataFile$RecipientID<-NULL
  dataFile$RecipientCell<-NULL
  dataFile$Date<-sapply(strsplit(dataFile$Date,' '), `[`, 1)# [ is the extraction op 1 is the index to extract
  
  dataFile$CallerCell<-trim.leading(dataFile$CallerCell)
  dataFile$CallerCell<-trim.trailing(dataFile$CallerCell)
  
  
  dataUsageSummary<-ddply(dataFile,.(CallerID),summarize,
                          TotalTransactions=length(Date),
                          DistinctDays=length(unique(Date)),
                          DistinctCell=length(unique(CallerCell)))
  write.csv(dataUsageSummary,outputFileName,quote=FALSE)
  
  return (dataUsageSummary)
}


processVoiceFile<-function(filePath,outputFileName){
  dataFile<-data.frame(RecordId=integer(0),CallerId=character(0),
                       RecipientId=character(0),Date=character(0),
                       Duration=numeric(0),
                       Revenue=numeric(0),CallerCell=character(0),
                       RecipientCell=character(0))
  dataFile<-read.csv(filePath
                     ,colClasses=c("integer","character",
                                   "character","character",
                                   "numeric","numeric","character",
                                   "character"))
  
  colnames(dataFile)<-c('RecordId','CallerID',
                        'RecipientID','Date','Duration','Revenue',
                        'CallerCell','RecipientCell')
  
  dataFile$RecordId<-NULL
  
  dataFile$Duration<-NULL
  dataFile$Revenue<-NULL
  dataFile$Date<-sapply(strsplit(dataFile$Date,' '), `[`, 1)# [ is the extraction op 1 is the index to extract
  
  dataFile$CallerCell<-trim.leading(dataFile$CallerCell)
  dataFile$CallerCell<-trim.trailing(dataFile$CallerCell)
  
  callerSummary<-ddply(dataFile,.(CallerID),summarize,
                       TotalTransactions=length(Date),
                       DistinctDays=length(unique(Date)),
                       DistinctRecipients=length(unique(RecipientID)),
                       DistinctCallerCell=length(unique(CallerCell)),
                       DistinctRecipientCell=length(unique(RecipientCell))
  )
  write.csv(callerSummary,paste(outputFileName,"Caller",sep=""),quote=FALSE)
  calleeSummary<-ddply(dataFile,.(RecipientID),summarize,
                       TotalTransactions=length(Date),
                       DistinctDays=length(unique(Date)),
                       DistinctRecipients=length(unique(CallerID)),
                       DistinctCallerCell=length(unique(CallerCell)),
                       DistinctRecipientCell=length(unique(RecipientCell))
  )
  write.csv(calleeSummary,paste(outputFileName,"Callee",sep=""),quote=FALSE)
  
  return(list("callerSummary"=callerSummary,"calleeSummary"=calleeSummary))
}

processSMSFile<-function(filePath,outputFileName){
  dataFile<-data.frame(RecordId=integer(0),CallerId=character(0),
                       RecipientId=character(0),Date=character(0),
                       Duration=numeric(0),
                       Revenue=numeric(0),CallerCell=character(0),
                       RecipientCell=character(0))
  dataFile<-read.csv(filePath
                     ,colClasses=c("integer","character",
                                   "character","character",
                                   "numeric","numeric","character",
                                   "character"))
  
  colnames(dataFile)<-c('RecordId','CallerID',
                        'RecipientID','Date','Duration','Revenue',
                        'CallerCell','RecipientCell')
  
  dataFile$RecordId<-NULL
  dataFile$CallerCell<-NULL
  dataFile$RecipientCell<-NULL
  dataFile$Duration<-NULL
  dataFile$Revenue<-NULL
  
  dataFile$Date<-sapply(strsplit(dataFile$Date,' '), `[`, 1)# [ is the extraction op 1 is the index to extract
  
  
  callerSummary<-ddply(dataFile,.(CallerID),summarize,
                       TotalTransactions=length(Date),
                       DistinctDays=length(unique(Date)),
                       DistinctRecipients=length(unique(RecipientID))
  )
  write.csv(callerSummary,paste(outputFileName,"Caller",sep=""),quote=FALSE)
  calleeSummary<-ddply(dataFile,.(RecipientID),summarize,
                       TotalTransactions=length(Date),
                       DistinctDays=length(unique(Date)),
                       DistinctRecipients=length(unique(CallerID))
  )
  write.csv(calleeSummary,paste(outputFileName,"Callee",sep=""),quote=FALSE)
  
  return (list("callerSummary"=callerSummary,"calleeSummary"=calleeSummary))
}
