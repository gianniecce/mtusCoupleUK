
table(dataCompleteD1D2.subCouplesGoodP1$sexDiary1P1)
table(dataCompleteD1D2.subCouplesGoodP1$sexDiary2P1)

library(TraMineR)
library(mtusRlocal)
library(Rage)

load('/Users/giacomovagni/Documents/Data/MTUS/Aggregate/UK/MtusUK2000.RData')
load('/Users/giacomovagni/Documents/Data/MTUS/Episode/UK/MtusUK2000Episode.RData')

# # # # # 

idlength = function(x) length(unique(x))

idlength(MtusUK2000Episode$hldid)
idno = paste(MtusUK2000Episode$hldid, MtusUK2000Episode$persid)
idlength(idno)

idlength(MtusUK2000$hldid)
idno = paste(MtusUK2000$hldid, MtusUK2000$persid)
idlength(idno)

# # # # # 

idlength(dataCompleteD1$idno)
idlength(dataCompleteD2$idno)
# both days 
idlength(dataCompleteD1D2$idno)
#

table(dataCompleteD1D2$sexDiary1)
table(dataCompleteD1D2$sexDiary2)

table(dataCompleteD1D2$hhtypeDiary) 
length(dataCompleteD1D2.subCouplesGood_I$hldid)

# Person 1 - Diary 1 and Diary 2 
table(dataCompleteD1D2.subCouplesGood_I$sexDiary1P1)
table(dataCompleteD1D2.subCouplesGood_I$sexDiary2P1)

# Person 2 - Diary 1 and Diary 2 
table(dataCompleteD1D2.subCouplesGood_I$sexDiary1P2)
table(dataCompleteD1D2.subCouplesGood_I$sexDiary2P2)

idlength(dataCompleteD1D2.subCouplesGood_I$hldid)
idlength(dataCompleteD1D2.subCouplesGood_I$idno.x)
idlength(dataCompleteD1D2.subCouplesGood_I$idno.y)

dataCompleteD1D2.subCouplesGood_I[1:2, c('hldid', 'idno.x', 'countryaDiary1P1', 'surveyDiary1P1', 'diaryDiary1P1', 'sexDiary1P1')]
dataCompleteD1D2.subCouplesGood_I[1:2, c('hldid', 'idno.x', 'countryaDiary1P1', 'surveyDiary1P2', 'diaryDiary1P2', 'sexDiary1P2')]

table(dataCompleteD1D2.subCouplesGood_I$diaryDiary1P1)
table(dataCompleteD1D2.subCouplesGood_I$diaryDiary1P2)

table(dataCompleteD1D2.subCouplesGood_I$sexDiary1P1)
table(dataCompleteD1D2.subCouplesGood_I$sexDiary1P2)

table(dataCompleteD1D2.subCouplesGood_I$sexDiary2P1)
table(dataCompleteD1D2.subCouplesGood_I$sexDiary2P2)

# Its easier to separate by person but keep the days together 
RowsP1 = which( grepl('Diary1P1|Diary2P1', x = row.number(dataCompleteD1D2.subCouplesGood_I)))
RowsP2 = which( grepl('Diary1P2|Diary2P2', x = row.number(dataCompleteD1D2.subCouplesGood_I)))

dataP1 = dataCompleteD1D2.subCouplesGood_I[,c(1:2, RowsP1)]
dataP2 = dataCompleteD1D2.subCouplesGood_I[,c(1:2, RowsP2)]
 
nrow(dataP1)
nrow(dataP2)

dataP1[1:4, 1:10]
dataP2[1:4, 1:10]

dataP1$h

dataP1$dayDiary1P1[1:4]
dataP1$dayDiary2P1[1:4]

dataP2$dayDiary1P2[1:4]
dataP2$dayDiary2P2[1:4]
