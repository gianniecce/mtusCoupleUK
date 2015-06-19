
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
load(file = '/Users/giacomovagni/Documents/Data/TimeUse/UK/DataUKarticle/dtc.RData')
load(file = '/Users/giacomovagni/Documents/Data/TimeUse/UK/DataUKarticle/dtcseq_r.RData')
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(Hmisc)
library(plyr)
library(dplyr)
library(mtusRlocal)
library(magrittr)
library(reshape)
library(reshape2)
library(TraMineR)
library(mtusRlocal)
library(Rage)
library(RColorBrewer)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

##################
# Where 
##################
seq_where = select(dtc, householdid.x, idno, contains('wher'))
seq_where_r = ifelse(seq_where == " Home ", yes = 'Home', no = ifelse(seq_where == " Other people's home ", "Other people's home", no = 'Other Location'))
seq_where_r[is.na(seq_where_r)] <- 'NA'

# Sequence Location 
seq_where_s = seqdef(seq_where_r[, -c(1,2)])
seqdplot(seq_where_s, border = NA, withlegend = F)
seqlegend(seq_where_s)


