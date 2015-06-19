library(TraMineR)
library(mtusRlocal)

load('/Users/giacomovagni/Documents/Data/MTUS/Aggregate/UK/MtusUK2000.RData')
load('/Users/giacomovagni/Documents/Data/MTUS/Episode/UK/MtusUK2000Episode.RData')

################################################
################################################

MD1 = subset(MtusUK2000Episode, diary == 1)
MD2 = subset(MtusUK2000Episode, diary == 2)

SeqActD1 = TimeMtusSeqAct(dataMtus = MD1, int = 10)
SeqActD2 = TimeMtusSeqAct(dataMtus = MD2, int = 10)

# SeqActD1 = MtusSeqAct(dataMtus = MD1, int = 10)
# SeqActD2 = MtusSeqAct(dataMtus = MD2, int = 10)

#save(SeqActD1, file = '/Users/giacomovagni/Dropbox/Working Paper/Working Paper Fev 2015/MtusScripts/DataMTUS_UK_2000/SeqActD1.RData')
#save(SeqActD2, file = '/Users/giacomovagni/Dropbox/Working Paper/Working Paper Fev 2015/MtusScripts/DataMTUS_UK_2000/SeqActD2.RData')

# save(SeqActD1, file = '/Users/giacomovagni/Documents/Data/MTUS/Sequences/UK2000/SeqActD1.RData')
# save(SeqActD2, file = '/Users/giacomovagni/Documents/Data/MTUS/Sequences/UK2000/SeqActD2.RData')

################################################
################################################

########
# the good way is to separate everything ! but it still misses the mact between the 2 diaries !!!
########

########
# Separate diary1 and diary 2 
MtusUK2000D1 = subset(MtusUK2000, diary == 1)
MtusUK2000D2 = subset(MtusUK2000, diary == 2)
########

MtusUK2000D1[1:5, 1:15]
MtusUK2000D2[1:5, 1:15]

# merge Aggregate and Sequences FILES 
########
dataCompleteD1 = TimeUsedataAggregateSequence(dataAggregate = MtusUK2000D1, dataSequenceActivity = SeqActD1)
########
dataCompleteD2 = TimeUsedataAggregateSequence(dataAggregate = MtusUK2000D2, dataSequenceActivity = SeqActD2)
########
diary1 = TimeUse.summary(data = dataCompleteD1, VarCols = 'av', activity = 11, intervals = 10)
diary2 = TimeUse.summary(data = dataCompleteD2, VarCols = 'av', activity = 11, intervals = 10)
########

diary1$activityMean
mean(diary1$AggregateFile$av11)

TimeUse.PlotDist(data = diary1, TypePlot = 'freQ', title = 'Diary1', ylim = c(0,01/10))
par(new = T)
TimeUse.PlotDist(data = diary2, TypePlot = 'freQ', title = 'Diary2', ylim = c(0,01/10))

diary2$activityMean
mean(diary2$AggregateFile$av11)

#########################
#########################
colnames(dataCompleteD1) <- c('idno', paste(colnames(dataCompleteD1 [,-c(1)] ), 'Diary1', sep = '') ) 
colnames(dataCompleteD2) <- c('idno', paste(colnames(dataCompleteD2 [,-c(1)] ), 'Diary2', sep = '') ) 

colnames(dataCompleteD1)
colnames(dataCompleteD2)

# fucking correct ! 
dataCompleteD1D2 = merge(dataCompleteD1, dataCompleteD2, by = 'idno')
nrow(dataCompleteD1D2)
dataCompleteD1D2[1:4, c(1:20, 597:620)]

# save(dataCompleteD1D2, file = '/Users/giacomovagni/Dropbox/Working Paper/Working Paper Fev 2015/MtusScripts/DataMTUS_UK_2000/dataCompleteD1D2.RData')
load('/Users/giacomovagni/Dropbox/Working Paper/Working Paper Fev 2015/MtusScripts/DataMTUS_UK_2000/dataCompleteD1D2.RData')

#########################
#########################

mean(dataCompleteD1D2$av11Diary1)
mean(dataCompleteD1D2$av11Diary1)
diary1$activityMean
diary2$activityMean

#########################
#########################

dataCompleteD1D2$hhtypeDiary1[1:4]
dataCompleteD1D2$hhtypeDiary2[1:4]

# Subset Couples 
# Couple alone = 2 ### Couple + others and other household types = 3#
dataCompleteD1D2.sub = subset(dataCompleteD1D2, hhtypeDiary1 != 1)
dataCompleteD1D2.sub = subset(dataCompleteD1D2.sub, hhtypeDiary1 != 4)

#
dataCompleteD1D2.subCouples = subset(dataCompleteD1D2, relrefpDiary1 == 1 | relrefpDiary1 == 2) 
dataCompleteD1D2.subCouples[1:20, ]

# tr??s int??ressant car ici ce sont les liens 
table(dataCompleteD1D2$relrefpDiary1) # RELREFP: Relation to household reference person - HAF only

# 1 ￼Person 1
# 2 Spouse/ Common-law partner
# 3 Child
# 4 Parent
# 5 Sibling
# 6 Son/Daughter-in-law
# 7 Father/Mother-in-law
# 8 Brother/Sister-in law
# 9 Other Relative
# 10 Not related

table(dataCompleteD1D2.subCouples$relrefpDiary1) # RELREFP: Relation to household reference person - HAF only

table(dataCompleteD1D2.subCouples$civstatDiary1) # is diarist in a couple ? 
table(dataCompleteD1D2.subCouples$civstatDiary2)
table(dataCompleteD1D2.subCouples$hhtype.x) # 3 Couple + others
table(dataCompleteD1D2.subCouples$hhldsize.x)
table(dataCompleteD1D2.subCouples$relrefp.x) # RELREFP: Relation to household reference person - HAF only

######
##### We need to select only When Both Partner 
######

HouseHold2 = table(dataCompleteD1D2.subCouples$hldidDiary1) 
good2 = which(HouseHold2 == 2)
length(dataCompleteD1D2.subCouples$hldid.x)
length(good2) * 2
HouseHold2Good = HouseHold2 [good2]
HouseHold2 = as.numeric(names(HouseHold2Good))

# select de 2 
nrow(dataCompleteD1D2.subCouples)
dataCompleteD1D2.subCouplesGood = dataCompleteD1D2.subCouples[ dataCompleteD1D2.subCouples$hldidDiary1 %in% HouseHold2, ]
nrow(dataCompleteD1D2.subCouplesGood) # 2 times each person == length(good2)

### ### 
### Persone 1 and 2
### ### 

dataCompleteD1D2.subCouplesGoodP1 = subset(dataCompleteD1D2.subCouplesGood, persidDiary1 == 1)
dataCompleteD1D2.subCouplesGoodP2 = subset(dataCompleteD1D2.subCouplesGood, persidDiary1 == 2)

colnames(dataCompleteD1D2.subCouplesGoodP1) <- c('idno', paste( colnames(dataCompleteD1D2.subCouplesGoodP1[,-1]), 'P1', sep = '') )
colnames(dataCompleteD1D2.subCouplesGoodP2) <- c('idno', paste( colnames(dataCompleteD1D2.subCouplesGoodP2[,-2]), 'P2', sep = '') )

### ### 
### Merge les rows By COUPLES ! 
### ### 

dataCompleteD1D2.subCouplesGoodP1$hldidDiary1P1 
dataCompleteD1D2.subCouplesGoodP2$hldidDiary1P2

#
dataCompleteD1D2.subCouplesGood_I = merge(x = dataCompleteD1D2.subCouplesGoodP1, 
                                          y = dataCompleteD1D2.subCouplesGoodP2, 
                                          by.x = 'hldidDiary1P1', 
                                          by.y = 'hldidDiary1P2')

library(Rage)
row.number(dataCompleteD1D2.subCouplesGood_I)
nrow(dataCompleteD1D2.subCouplesGood_I)

dataCompleteD1D2.subCouplesGood_I[300:310, c('hldidDiary1P1', 'idno.x', 'relrefpDiary1P1', 'famstatDiary1P1', 'dayDiary1P1', 'diaryDiary1P1', 'dayDiary1P1', 'diaryDiary1P1', 'main2Diary1P1', 'main2Diary1P2', 'main2Diary2P1', 'main2Diary2P2')]
dataCompleteD1D2.subCouplesGood_I[300:310, c('hldidDiary1P1', 'idno.y', 'relrefpDiary1P2', 'famstatDiary1P2', 'dayDiary1P2', 'diaryDiary1P2', 'dayDiary1P2', 'diaryDiary1P2', 'main2Diary1P1', 'main2Diary1P2', 'main2Diary2P1', 'main2Diary2P2')]

table(dataCompleteD1D2.subCouplesGood_I$relrefpDiary1P1)
table(dataCompleteD1D2.subCouplesGood_I$relrefpDiary1P2)

table(dataCompleteD1D2.subCouplesGood_I$famstatDiary1P1)
table(dataCompleteD1D2.subCouplesGood_I$famstatDiary1P2)

table(dataCompleteD1D2.subCouplesGood_I$hhtypeDiary1P1)
table(dataCompleteD1D2.subCouplesGood_I$hhtypeDiary1P2)

row.number(dataCompleteD1D2.subCouplesGood_I)
# save(dataCompleteD1D2.subCouplesGood_I, file = '/Users/giacomovagni/Documents/Data/MTUS/Sequences/UK2000/dataCompleteD1D2.subCouplesGood_I.RData')

dataCompleteD1D2.subCouplesGood_I.sub = dataCompleteD1D2.subCouplesGood_I[, c('hldidDiary1P1', 'idno.x', 'sexDiary1P1', 'relrefpDiary1P1', 'famstatDiary1P1', 'dayDiary1P1', 'hldidDiary1P1', 'idno.y', 'sexDiary1P2', 'relrefpDiary1P2', 'famstatDiary1P2', 'dayDiary1P2')]

library(plyr) 
dataCompleteD1D2.subCouplesGood_I = rename(x = dataCompleteD1D2.subCouplesGood_I, c('hldidDiary1P1' = 'hldid'))

write.csv(dataCompleteD1D2.subCouplesGood_I.sub, file = '/Users/giacomovagni/Documents/Data/MTUS/Sequences/dataCompleteD1D2.subCouplesGood_I.sub.csv')
write.csv(dataCompleteD1D2.subCouplesGood_I, file = '/Users/giacomovagni/Documents/Data/MTUS/Sequences/dataCompleteD1D2.subCouplesGood_I.csv')

# save(dataCompleteD1D2.subCouplesGood_I.sub, file = '/Users/giacomovagni/Documents/Data/MTUS/Sequences/dataCompleteD1D2.subCouplesGood_I.sub.RData')
# save(dataCompleteD1D2.subCouplesGood_I, file = '/Users/giacomovagni/Documents/Data/MTUS/Sequences/dataCompleteD1D2.subCouplesGood_I.RData')

# hldidDiary1P1 is common couple id ! 
# Person 1 
dataCompleteD1D2.subCouplesGood_I[100:105, c('hldid', 'idno.x', 'relrefpDiary1P1', 'dayDiary1P1', 'diaryDiary1P1', 'dayDiary1P1', 'diaryDiary1P1', 'main2Diary1P1', 'main2Diary1P2', 'main2Diary2P1', 'main2Diary2P2')]
# Person 2
dataCompleteD1D2.subCouplesGood_I[100:105, c('hldid', 'idno.y', 'relrefpDiary1P2', 'dayDiary1P2', 'diaryDiary1P2', 'dayDiary1P2', 'diaryDiary1P2', 'main2Diary1P1', 'main2Diary1P2', 'main2Diary2P2', 'main2Diary2P2')]

dataCompleteD1D2.subCouplesGood_I[1:3, c('hldid', 'idno.x', 'relrefpDiary1P1', 'dayDiary1P1', 'diaryDiary1P1', 'dayDiary1P1', 'diaryDiary1P1', 'main2Diary1P1', 'main2Diary1P2', 'main2Diary2P1', 'main2Diary2P1')]
dataCompleteD1D2.subCouplesGood_I[1:3, c('hldid', 'idno.y', 'relrefpDiary1P2', 'dayDiary1P2', 'diaryDiary1P2', 'dayDiary1P2', 'diaryDiary1P2', 'main2Diary1P2', 'main2Diary1P2', 'main2Diary2P2', 'main2Diary2P2')]

# Its easier to separate by person but keep the days together 
RowsP1 = which( grepl('Diary1P1|Diary2P1', x = row.number(dataCompleteD1D2.subCouplesGood_I)))
RowsP2 = which( grepl('Diary1P2|Diary2P2', x = row.number(dataCompleteD1D2.subCouplesGood_I)))

dataP1 = dataCompleteD1D2.subCouplesGood_I[,RowsP1]
dataP2 = dataCompleteD1D2.subCouplesGood_I[,RowsP2]

dataP1$dayDiary1P1[1:4]
dataP1$dayDiary2P1[1:4]

dataP2$dayDiary1P2[1:4]
dataP2$dayDiary2P2[1:4]

#######################################################
#######################################################
#######################################################
# Il faut que je mette toute la base ?? la suite pour avoir les m??mes couleurs !!!! 

Rows.av = which( grepl('^ac29$', x = names(dataCompleteD1D2.subCouplesGood_I) ) )

row.number(dataCompleteD1D2.subCouplesGood_I)

dataSeq = dataCompleteD1D2.subCouplesGood_I [,c(454:597,1050:1193,1646:1789,2242:2385)]
dataSeq[1, ]

row.number(dataSeq)
1:144 #P1D1
145:288 #P1D2
300:432
433:576


dataCompleteD1D2.subCouplesGood_I$relrefpDiary1P1
dataCompleteD1D2.subCouplesGood_I$relrefpDiary1P2


seqTimeUK = seqdef(data = dataSeq, cpal = rainbow(39))

par(mfrow = c(2,1))
seqdplot(seqTimeUK, border = NA, withlegend = F)
seqiplot(seqTimeUK, border = NA, withlegend = F)

seqdplot(seqTimeUK, border = NA, withlegend = F)



seqdplot(seqTimeUK [, 1:288], border = NA, dataCompleteD1D2.subCouplesGood_I$sexDiary1P1)
seqdplot(seqTimeUK [, 300:576], border = NA, dataCompleteD1D2.subCouplesGood_I$sexDiary1P1)

27 - 13

beginWeekEnd.Men = which(dataCompleteD1D2.subCouplesGood_I$dayDiary1P1 == 1 | 
                           dataCompleteD1D2.subCouplesGood_I$dayDiary1P1 == 7 & 
                           dataCompleteD1D2.subCouplesGood_I$sexDiary1P1 == 1
)

beginWeekEnd.Women = which(dataCompleteD1D2.subCouplesGood_I$dayDiary1P1 == 1 | 
                             dataCompleteD1D2.subCouplesGood_I$dayDiary1P1 == 7 & 
                             dataCompleteD1D2.subCouplesGood_I$sexDiary1P1 == 2
)

par(mfrow = c(2,1))
seqdplot(seqTimeUK [beginWeekEnd.Men, 1:288], border = NA, withlegend = F, title = 'Men')
seqdplot(seqTimeUK [beginWeekEnd.Women, 300:576], border = NA, withlegend = F, title = 'Women')



beginWeekDays.Men = which(dataCompleteD1D2.subCouplesGood_I$dayDiary1P1 != 1 | 
                            dataCompleteD1D2.subCouplesGood_I$dayDiary1P1 != 7 & 
                            dataCompleteD1D2.subCouplesGood_I$sexDiary1P1 == 1
)

beginWeekDays.Women = which(dataCompleteD1D2.subCouplesGood_I$dayDiary1P1 != 1 | 
                              dataCompleteD1D2.subCouplesGood_I$dayDiary1P1 != 7 & 
                              dataCompleteD1D2.subCouplesGood_I$sexDiary1P1 == 2
)

par(mfrow = c(2,1))
seqdplot(seqTimeUK [beginWeekDays.Men, 1:288], border = NA, withlegend = F, title = 'Men')
seqdplot(seqTimeUK [beginWeekDays.Women, 300:576], border = NA, withlegend = F, title = 'Women')



par(mfrow = c(1,1))
seqIplot(seqTimeUK, border = NA, withlegend = F)






seqdplot(seqTimeUK [, 300:432], border = NA)
seqdplot(seqTimeUK [, 433:576], border = NA)





# Same Couple Order of day
Data.P1.weekend = subset(dataP1, dayDiary1P1 == 1 | dayDiary1P1 == 7) 
Data.P2.weekend = subset(dataP2, dayDiary1P2 == 1 | dayDiary1P2 == 7) 

# Same Couple Order of day
Data.P1.weekdays = subset(dataP1, dayDiary1P1 =! 1 | dayDiary1P1 != 7) 
Data.P2.weekdays = subset(dataP2, dayDiary1P2 != 1 | dayDiary1P2 != 7) 


nrow(dataP1) - nrow(Data.P1.weekend)


row.number(Data.P1.weekend)

seqP1D1 = seqdef(data = Data.P1.weekend[,c(453:596)], cpal = rainbow(38))
seqP1D2 = seqdef(data = Data.P1.weekend[,c(1049:1192)], cpal = rainbow(39))

seqdplot(seqP1D1)
seqdplot(seqP1D2)





RowsD1P1 = which( grepl('Diary1P1', x = row.number(dataCompleteD1D2.subCouplesGood_I)))
RowsD2P1 = which(grepl('Diary2P1', x = row.number(dataCompleteD1D2.subCouplesGood_I)))
RowsD1P2 = which(grepl('Diary1P2', x = row.number(dataCompleteD1D2.subCouplesGood_I)))
RowsD2P2 = which(grepl('Diary2P2', x = row.number(dataCompleteD1D2.subCouplesGood_I)))

# complete Data ??? 
D1P1 = dataCompleteD1D2.subCouplesGood_I[,RowsD1P1]
D2P1 = dataCompleteD1D2.subCouplesGood_I[,RowsD2P1]
D1P2 = dataCompleteD1D2.subCouplesGood_I[,RowsD1P2]
D2P2 = dataCompleteD1D2.subCouplesGood_I[,RowsD2P2]

sleep = TimeUse.summary(data = D1P1, VarCols = 'av', activity = 11, intervals = 10)
sleep$activityMean
mean(D1P1$av11Diary1P1)

#######################################################
#######################################################
#######################################################

# Same Couple Order of day
D1P1.weekend = subset(D1P1, dayDiary1P1 == 1 | dayDiary1P1 == 7) 
D1P2.weekend = subset(D1P2, dayDiary1P2 == 1 | dayDiary1P2 == 7) 

D1P1.weekend$av11Diary1P1
D1P1.weekend$av11Diary1P1

D2P1.weekdays = subset(D2P1, dayDiary2P1 != 1 | dayDiary2P1 != 7) 
D2P2.weekdays = subset(D2P2, dayDiary2P2 != 1 | dayDiary2P2 != 7) 

nrow(D1P1.weekend)
nrow(D1P2.weekend)
nrow(D2P1.weekend)
nrow(D2P2.weekend)

################################################
################################################




################################################
# Some test 
################################################

data = MtusUK2000
data$idno = as.numeric(paste(data[,'hldid'], data[,'persid'], sep = '')) # code for waves + country #??make a unique Key 

TimePlyrTest = function(data, idno = 'idno', VarInterest = 'diary'){
  library(plyr)
  test = ddply(.data = data, summarize, sum=sum(sexDiary1P1), number=length(hldidDiary1P1) )
  table(test$sum == 3)
  table(test$sum == 2)
  table(test$sum == 1)
  table(test$sum == 0)
}

TimePlyrTest(data = dataCompleteD1D2.subCouplesGood_I.sub, idno = 'hldidDiary1P1', VarInterest = 'diary')
table(dataCompleteD1D2.subCouplesGood_I.sub$sexDiary1P1, dataCompleteD1D2.subCouplesGood_I.sub$sexDiary1P2)

dataCompleteD1D2.subCouplesGood_I.sub[1, ]

row.number(dataCompleteD1D2.subCouplesGood_I.sub)
p1 = dataCompleteD1D2.subCouplesGood_I.sub [,1:6]
p2 = dataCompleteD1D2.subCouplesGood_I.sub [,7:12]

colnames(p2) <- names(p1)

p1p2 = rbind(p1, p2)
p1p2[1:4, ]

TimePlyrTest(data = dataCompleteD1D2.subCouplesGood_I.sub, idno = 'hldidDiary1P1', VarInterest = 'sexDiary1P1')

table(dataCompleteD1D2.subCouplesGood_I.sub$sexDiary1P1)
table(dataCompleteD1D2.subCouplesGood_I.sub$sexDiary1P2)

table(dataCompleteD1D2.subCouplesGood_I.sub$sexDiary1P1, dataCompleteD1D2.subCouplesGood_I.sub$sexDiary1P2)

################################################



