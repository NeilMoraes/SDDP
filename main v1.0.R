repelem <- function(blockDefn){
  temp = list()
  for (i in 1:length(blockDefn)){
    temp[[i]] = rep(i, each=blockDefn[i])
  }
  blockKeyVector = do.call(rbind, lapply(temp, data.frame, stringsAsFactors=FALSE))
  colnames(blockKeyVector) = "BlockKey"
  return(blockKeyVector)
}

#This function converts a year of half-houlry demands to blocks
rm(list = setdiff(ls(), lsf.str()))

#blockDefine = c(2,4,20,18,18,22,35,28,28,28,28,28,8,45,24);

blockDefine = read.table("G:/Genesis/SDDP/Blocking Algorithm/blockDef.txt",header=FALSE)
blockDefine = data.matrix(blockDefine*2)
demandData = read.table("G:/Genesis/SDDP/Blocking Algorithm/DemandData.txt",header=TRUE)
#demandData = read.table("G:/Genesis/SDDP/Blocking Algorithm/Demand2.txt",header=TRUE)
demandData[,3] = demandData[,3]*2000


#Remove Last day of data (so data is exact number of weeks)
demandData = demandData[1:(52*7*48),]
#Change Period from daily (till 48) to weekly (till 336)
demandData[,2] = rep(1:336,52)

#Change Date to keep track of the week number
demandData[,1] =rep(1:52,each=336)

#Split Year Data into a list of Week Size chunks
weeklydemand_list = split(demandData, ceiling(seq_along(demandData[,2])/336))

plot(demandData[,3],type="l")

#Sort the demand in each week sized chunk in descending order
weeklydemand_list = lapply(weeklydemand_list, function(x) x[order(-x[,3]), ])

#Add the Block Key Vector to each week sized chunk
blockKeyVector = repelem(blockDefine)
weeklydemand_list = lapply(weeklydemand_list, function(x) cbind(x,blockKeyVector)) 

#Reorder the demand in each week sized chunk in by week-number and period
weeklydemand_list = lapply(weeklydemand_list, function(x) x[order(x[,1],x[,2]), ])

#Write Period-BlockKey Mapping to file
weeklydemand_vector = do.call("rbind",weeklydemand_list )
filename = "G:/Genesis/SDDP/Blocking Algorithm/block2hour.dat"
cat(paste("[FILES]","cmgdem.csv","gerter.csv","[MAP]",sep="\n"),file=filename,sep="\n",append=FALSE)
cat(paste("Stage","Hour-Min","Block",sep=","),file=filename,sep="\n",append=TRUE)
cat(paste(weeklydemand_vector[,1],weeklydemand_vector[,2],weeklydemand_vector[,4],sep=","),file=filename,sep="\n",append=TRUE,fill=FALSE)
#cat(paste(rep(53:104,each=336),weeklydemand_vector[,2],weeklydemand_vector[,4],sep=","),file=filename,sep="\n",append=TRUE,fill=FALSE)

#Collapse weeklydemand to get blockdemand - Get mean demand along BLockKey
blockDemand_list = lapply(weeklydemand_list, function(x) aggregate(GWh ~ BlockKey, FUN = mean,data=x))

#convert blockDemand list to single vector
blockDemand_vector = do.call("rbind",blockDemand_list )

#Write BLock Demand to file in GWh
filename = "G:/Genesis/SDDP/Blocking Algorithm/blockDemand.txt"
cat(paste(blockDemand_vector[,2]/1000,sep=""),file=filename,sep="\n",append=FALSE)

#Expand the block demand to HH data
blockDemandHH_list <- mapply(function(x,y) x[match(y[,4],x[,1]),2] ,x=blockDemand_list,y=weeklydemand_list,SIMPLIFY = FALSE)
blockDemandHH_vector1= unlist(blockDemandHH_list, recursive = FALSE)
plot(blockDemandHH_vector[1:336],type="l",ylim=c(0,7000))
blockDemandHH_vector = data.frame(Week=rep(1:52,each=336),HH=rep(1:336,52),Demand=blockDemandHH_vector1)

#Write HH demand data to file
filename = "G:/Genesis/SDDP/Blocking Algorithm/block2hour.txt"
cat(paste("Stage","Hour-Min","Demand",sep=","),file=filename,sep="\n",append=FALSE)
cat(paste(blockDemandHH_vector[,1],blockDemandHH_vector[,2],blockDemandHH_vector[,3],sep=","),file=filename,sep="\n",append=TRUE,fill=FALSE)
write.xlsx(blockDemandHH_vector,"G:/Genesis/SDDP/Blocking Algorithm/blockDemand.xlsx")


#Compare Means and SD of original HH data vs HH blocked data
fulldatamean = sapply(weeklydemand_list,function(x) mean(x[,3])/1000)
fulldataSD = sapply(weeklydemand_list,function(x) sd(x[,3]))
plot(fulldatamean)
fullblockdatamean = sapply(blockDemandHH_list,function(x) mean(x))
fullblockdataSD = sapply(blockDemandHH_list,function(x) sd(x))
lines(fullblockdatamean)

plot(fulldataSD)
lines(fullblockdataSD)

plot(fulldatamean-fullblockdatamean)
plot(fulldataSD-fullblockdataSD)

write.xlsx(weeklydemand_vector,"G:/Genesis/SDDP/Blocking Algorithm/Comparison.xlsx",sheetName="OriginalHourlyDemand")
write.xlsx(blockDemandHH_vector,"G:/Genesis/SDDP/Blocking Algorithm/Comparison.xlsx",sheetName="BlockHourlyDemand",append=TRUE)
write.xlsx(fulldatamean,"G:/Genesis/SDDP/Blocking Algorithm/Comparison.xlsx",sheetName="BlockDemandMean",append=TRUE)

#---------------------------------------------------

#Get block Price Data for a year
#blockPrice = rnorm(52*15,mean = 100, sd = 10)
#blockPrice = do.call("rbind",weeklydemand_list)[3]
blockPrice = blockDemand_vector[,2]
blockPrice = data.frame("Block"=rep(1:15,52),blockPrice)


#Read in Period-BLock Mapping
BlockKeyMapping = read.table("G:/Genesis/SDDP/Blocking Algorithm/block2hour.dat",header=TRUE,skip=4)

#Split BlockKeyMapping into list of week-size chunks
weeklyBlockKeyMapping = split(BlockKeyMapping, ceiling(seq_along(BlockKeyMapping[,2])/336)) 

#Split blockPrice into list of week-size chunks
weeklyBlockPrice =  split(blockPrice, ceiling(seq_along(blockPrice[,2])/15))  

#Match block to period
result <- mapply(function(x,y) x[match(y[,2],x[,1]),2] ,x=weeklyBlockPrice,y=weeklyBlockKeyMapping,SIMPLIFY = FALSE)
result1= unlist(result, recursive = FALSE)
BlockKeyMapping[,3] = result1



#Reorder data according to the period
weeklyBlockKeyMapping = split(BlockKeyMapping, ceiling(seq_along(BlockKeyMapping[,2])/336))  
weeklyBlockKeyMapping = lapply(weeklyBlockKeyMapping, function(x) x[with(x, order(Period)),])

priceHH = do.call("rbind",weeklyBlockKeyMapping)
plot(priceHH[,3],type="l")


#Reorder BlockKeyMapping to get HH-price
priceHH = BlockKeyMapping[with(BlockKeyMapping, order(Period)),]


#Reorder Data according to HHkey
plot(priceHH[,3],type="l")

lines(demandData[,3],type="l",col="red")







#--------------------------------------------
#Using Total Island Block Mapping, create the NI and SI mapping
islandDemand = read.table("G:/Genesis/SDDP/Blocking Algorithm/IslandDemand.txt",header=TRUE)
NIdemand = islandDemand[islandDemand[,3]=="TOTNILD",]
SIdemand = islandDemand[islandDemand[,3]=="TOTSILD",]
NIdemand = NIdemand[1:(52*7*48),]
SIdemand = SIdemand[1:(52*7*48),]
NIdemand[,2] = rep(1:336,52)
SIdemand[,2] =  rep(1:336,52)

BlockKeyMapping = read.table("G:/Genesis/SDDP/Blocking Algorithm/block2hour.dat",header=TRUE,sep=",",skip=4)

#Overwrite the period data for the island demand with the block-mapping from the full year run
NIdemand[,2] = BlockKeyMapping[,3]
SIdemand[,2] =BlockKeyMapping[,3]  
colnames(NIdemand)[2] = "BlockKey"
colnames(SIdemand)[2] = "BlockKey"


weeklyNIdemand_list = split(NIdemand, ceiling(seq_along(NIdemand[,2])/336)) 
weeklySIdemand_list = split(SIdemand, ceiling(seq_along(SIdemand[,2])/336)) 
weeklyBlockKeyMapping_list = split(BlockKeyMapping, ceiling(seq_along(BlockKeyMapping[,2])/336)) 

#Order the Island Demands according the BlockKey
#weeklyNIdemand_list <- mapply(function(x,y) x[match(x[,2],y[,2]),] ,x=weeklyNIdemand_list,y=weeklyBlockKeyMapping_list,SIMPLIFY = FALSE)


#Collapse weeklyislanddemand along blockKey to get blockdemand - Get mean demand along BLockKey
NIblockDemand_list = lapply(weeklyNIdemand_list, function(x) aggregate(x[,4] ~ x[,2], FUN = mean,data=x))
SIblockDemand_list = lapply(weeklySIdemand_list, function(x) aggregate(x[,4] ~ x[,2], FUN = mean,data=x))


NIblockDemand_vector = do.call("rbind",NIblockDemand_list )
SIblockDemand_vector = do.call("rbind",SIblockDemand_list)

NIblockDemand_matrix = matrix(,nrow=15,ncol=52)
SIblockDemand_matrix = matrix(,nrow=15,ncol=52)

for (j in 1:52){
  NIblockDemand_matrix[,j] = NIblockDemand_vector[(15*j-14):(15*j),2]
  SIblockDemand_matrix[,j] = SIblockDemand_vector[(15*j-14):(15*j),2]
}

years = matrix(2015,nrow=15,ncol=1)
block = matrix(c(1:15),nrow=15,ncol=1)
weeks = matrix(c(1:52),nrow=1,ncol=52)

library(xlsx)

outwb <- createWorkbook()
NIsheet <- createSheet(outwb, sheetName = "North Island MW")
SIsheet <- createSheet(outwb, sheetName = "South Island MW")

#Create NI Output Sheet
rows <- createRow(NIsheet,rowIndex=1)
value <- createCell(rows, colIndex=1)
setCellValue(value[[1,1]], "North Island MW")
value <- createCell(rows, colIndex=3)
setCellValue(value[[1,1]], "Week")
rows <- createRow(NIsheet,rowIndex=2)
value <- createCell(rows, colIndex=1)
setCellValue(value[[1,1]], "Years")
value <- createCell(rows, colIndex=2)
setCellValue(value[[1,1]], "Block")

addDataFrame(years, NIsheet, startRow=3, startColumn=1,row.names=FALSE,col.names=FALSE)
addDataFrame(block, NIsheet, startRow=3, startColumn=2,row.names=FALSE,col.names=FALSE)
addDataFrame(weeks, NIsheet, startRow=2, startColumn=3,row.names=FALSE,col.names=FALSE)
addDataFrame(NIblockDemand_matrix, NIsheet, startRow=3, startColumn=3,row.names=FALSE,col.names=FALSE)

#Create SI Output Sheet
rows <- createRow(SIsheet,rowIndex=1)
value <- createCell(rows, colIndex=1)
setCellValue(value[[1,1]], "North Island MW")
value <- createCell(rows, colIndex=3)
setCellValue(value[[1,1]], "Week")
rows <- createRow(SIsheet,rowIndex=2)
value <- createCell(rows, colIndex=1)
setCellValue(value[[1,1]], "Years")
value <- createCell(rows, colIndex=2)
setCellValue(value[[1,1]], "Block")

addDataFrame(years, SIsheet, startRow=3, startColumn=1,row.names=FALSE,col.names=FALSE)
addDataFrame(block, SIsheet, startRow=3, startColumn=2,row.names=FALSE,col.names=FALSE)
addDataFrame(weeks, SIsheet, startRow=2, startColumn=3,row.names=FALSE,col.names=FALSE)
addDataFrame(SIblockDemand_matrix, SIsheet, startRow=3, startColumn=3,row.names=FALSE,col.names=FALSE)

saveWorkbook(outwb, "G:/Genesis/SDDP/Blocking Algorithm/SDDPIslandDemand.xlsx")

weeklyNIdemand_list[[1]][1:20,]
weeklyBlockKeyMapping_list[[1]][1:20,]

NIblockDemand_list[[1]]




