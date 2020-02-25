####Function and setup####
list.of.packages <- c("Hmisc","plyr","data.table","varhandle","ggplot2","reshape2","splitstackshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


data=fread("C:/Users/zachc/Documents/iaticolspilt.csv")

names(data)=makeNames(names(data))
data=data[,c("transaction.value","sector.code","sector.percentage","iati.identifier","transaction.type","recipient.country","reporting.org"),with=F]
data=cSplit(data,"sector.code", ";")
data=cSplit(data,"sector.percentage", ";")
data$percent.sum=rowSums(data[,58:109],na.rm=T)
percentages=names(data[,58:109])

for(percent in percentages){
  
}

data.long=reshape(data, varying=names(data)[4:ncol(data)],direction="long",sep="_")
data.long=subset(data.long, !is.na(sector.percentage))
data.long$sector.percentage=data.long$sector.percentage/100
data.long$amount=data.long$sector.percentage*data.long$transaction.value


fwrite(data.long, "C:/Users/zachc/Documents/iaticolspilt2.csv")
dat.tab=data.table(data.long)[
  ,.(amount=sum(amount,na.rm=T))
  ,by=.(sector.code)
]