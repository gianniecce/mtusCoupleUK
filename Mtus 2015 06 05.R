library(Hmisc)
library(foreign)

library(plyr)
library(dplyr)
library(mtusRlocal)
library(magrittr)
library(reshape)
library(reshape2)
library(TraMineR)
library(mtusRlocal)

dtindividual = read.spss('/Users/giacomovagni/Documents/Data/TimeUse/UK/UKDA-4504-spss/spss/spss12/Individual_data_5.sav', to.data.frame = T, use.value.labels = T)
dtdiaryEpisode = spss.get('/Users/giacomovagni/Documents/Data/TimeUse/UK/UKDA-4504-spss/spss/spss12/diary_data_8_episode.sav')
dtdiary = spss.get('/Users/giacomovagni/Documents/Data/TimeUse/UK/UKDA-4504-spss/spss/spss12/diary_data_8.sav')
dthhld = spss.get('/Users/giacomovagni/Documents/Data/TimeUse/UK/UKDA-4504-spss/spss/spss12/hhld_data_6.sav')
dtworksheet = spss.get('/Users/giacomovagni/Documents/Data/TimeUse/UK/UKDA-4504-spss/spss/spss12/worksheet_data_3.sav')
dtweight = spss.get('/Users/giacomovagni/Documents/Data/TimeUse/UK/UKDA-4504-spss/spss/spss12/weight_diary_person.sav')

save(dtindividual, file = '/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dtindividual.RData')
save(dtdiaryEpisode, file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/dtdiaryEpisode.RData')
save(dtdiary, file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/dtdiary.RData')
save(dthhld, file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/dthhld.RData')
save(dtworksheet, file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/dtworksheet.RData')
save(dtweight, file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/dtweight.RData')

lab = label(dt3)
lab[which(grepl(pattern = 'work', lab))]
attributes(dataAct)$labels

dataAct = select(dt3, contains('act1'))
head(dataAct)

dataAct = seqdef(dataAct, cpal = rainbow(253))
seqdplot(dataAct, border = NA)
rev(attributes(dataAct)$labels)
seqlegend(sekk, ltext = c(rev(attributes(sekk)$labels)), fontsize = 0.5)

mseq = seqmeant(sekk)
round(mseq,2) * 10

SleepChannel = ifelse(dataAct == 'Sleep', 1, 0)
sekSleep = seqdef(SleepChannel, cpal = c('white', 'orange'))
seqdplot(sekSleep, border = NA) 

dt2_df = tbl_df(dt3)
dt2_dfs = select(dt2_df, SN2, SN3, DTYPE, DDAYOFWK, contains('evact'), contains('evst'))
