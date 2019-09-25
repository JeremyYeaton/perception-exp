library(dplyr)
library(ggplot2)
library(reshape2)
library(neuropsychology)

## IMPORT DATA AND STIM LIST ####
stim <- read.table('stimSkeleton.csv',header=T,sep='\t')%>%
  mutate(cond = ordered(cond,levels=c('NC','DN','Not All','All not')))

users <- read.table('meta.csv',header=T,sep=',')%>%
  select('birthplace','currentlocation','growingUp','sex','userCode') %>%
  merge(read.table('users.csv',header=T,sep=','),by='userCode') %>%
  filter(.,!duplicated(userCode))
sum(users$sex == 'female')

data <- read.table('xp.csv',header=T,sep=',') %>%
  select('Item','response1','response2','response3','trialnumber','userCode')  %>%
  # merge(.,users,by='userCode') %>%
  mutate(response2 = as.character(response2),response1 = as.character(response1)) %>%
  mutate(response2 = replace(response2, which(response2=='Enregistrement 1'),1)) %>%
  mutate(response2 = replace(response2, which(response2=='Enregistrement 2'),2)) %>%
  mutate(response1 = replace(response1, which(response1=='VRAI'),T)) %>%
  mutate(response1 = replace(response1, which(response1=='FAUX'),F)) %>%
  mutate('rep1' = factor(response1),'rep2' = factor(response2))
  
## MERGE ####
df.all <- merge(data,stim,'Item') %>%
  filter(userCode %in% users$userCode) %>%
  mutate('interp' = rep1 == r1,
         'recording' = rep2 == r2,
         'confidence' = response3) %>%
  mutate('bothRight' = (interp == T & recording == T),
         'bothWrong' = (interp ==F & recording == F)) %>%
  mutate('match' = (bothRight | bothWrong)) %>%
  mutate(interp = replace(interp, which(interp == T),1),
         interp = replace(interp, which(interp == F),0),
         recording = replace(recording, which(recording == T),1),
         recording = replace(recording, which(recording == F),0)) %>%
  # filter(verb != 'faire' & verb != 'dire') %>%
  na.omit() %>%
  select(Item, userCode, cond, verb, interp, recording, bothRight, bothWrong, match, confidence)

## SUMMARISE ####

df.all <- df.all %>%
  mutate(cond2 = cond)
levels(df.all$cond2)[5] <- 'Filler'

df.all$cond2[df.all$cond2 == 'Not All' | df.all$cond2 == 'All not'] = 'Filler'

correctInt.summ <- df.all %>%
  filter(verb != 'faire' & verb != 'dire') %>%
  # filter(cond == 'NC' | cond == 'DN') %>%
  # filter(interp == 1) %>%
  group_by(userCode,cond2) %>%
  summarise(sumCorrect = mean(interp),
            sem = sd(interp)/sqrt(length(df.all$interp)))

# correctInt.barsum <- df.all %>%
#   filter(verb != 'faire' & verb != 'dire') %>%
#   # filter(cond == 'NC' | cond == 'DN') %>%
#   # filter(interp == 1) %>%
#   group_by(cond2) %>%
#   summarise(sumCorrect = mean(interp),
#             sem = sd(interp)/sqrt(length(df.all$interp)))

correctInt.barsum <- df.all %>%
  filter(verb != 'faire' & verb != 'dire') %>%
  # filter(cond == 'NC' | cond == 'DN') %>%
  # filter(interp == 1) %>%
   group_by(cond2) #%>%
  # summarise(sumCorrect = mean(interp),
  #           sem = sd(interp)/sqrt(length(df.all$interp)))

correctRec.summ <- df.all %>%
  # filter(verb != 'faire' & verb != 'dire') %>%
  filter(cond == 'NC' | cond == 'DN') %>%
  filter(interp == 1) %>%
  group_by(userCode,cond) %>%
  summarise(sumCorrect = mean(recording),
            sem = sd(recording)/sqrt(length(df.all$recording)))

correctRec.barsum <- df.all %>%
  # filter(verb != 'faire' & verb != 'dire') %>%
  filter(cond == 'NC' | cond == 'DN') %>%
  filter(interp == 1) %>%
  group_by(cond) %>%
  summarise(sumCorrect = mean(recording),
            sem = sd(recording)/sqrt(length(df.all$recording)))

dprime.summ <- df.all %>%
  mutate(hit = cond == 'DN' & recording == 1,
         miss = cond == 'DN' & recording == 0,
         fa = cond == 'NC' & recording == 0,
         cr = cond == 'NC' & recording ==1) %>%
  mutate(hit = replace(hit, which(hit==T),1),
         hit = replace(hit, which(hit==F),0),
         cr = replace(cr, which(cr==T),1),
         cr = replace(cr, which(cr==F),0),
         miss = replace(miss, which(miss==T),1),
         miss = replace(miss, which(miss==F),0),
         fa = replace(fa, which(fa==T),1),
         fa = replace(fa, which(fa==F),0)) %>%
  filter(interp == 1) %>%
  group_by(userCode) %>%
  summarise('aPrime' = dprime(sum(hit),sum(miss),sum(fa),sum(cr))$aprime)

corrInt.df <- df.all %>%
  filter(interp == 1) %>%
  # filter(match == T) %>%
  filter(cond == 'NC' | cond == 'DN') %>%
  group_by(userCode)

match.df <- df.all %>%
  # filter(match == T) %>%
  filter(cond == 'NC' | cond == 'DN') %>%
  mutate(newCond = cond) %>%
  # mutate(newCond = replace(newCond, which(cond == 'DN' & bothWrong == T),'NC'),
         # newCond = replace(newCond, which(cond == 'NC' & bothWrong == T),'DN')) %>%
  mutate(newCond = replace(newCond, which(cond == 'DN' & interp == 0),'NC'),
         newCond = replace(newCond, which(cond == 'NC' & interp == 0),'DN')) %>%
  group_by(userCode)

match.summ <- match.df %>%
  group_by(userCode,newCond) %>%
  summarise(meanMatch = mean(match),sem = sd(match)/sqrt(length(match.df$match)))
match.barsum <- match.df %>%
  group_by(newCond) %>%
  summarise(meanMatch = mean(match),sem = sd(match)/sqrt(length(match.df$match)))

dprimeMatch.summ <- match.df %>%
  mutate(hit = newCond == 'DN' & match == 1,
         miss = newCond == 'DN' & match == 0,
         fa = newCond == 'NC' & match == 0,
         cr = newCond == 'NC' & match ==1) %>%
  mutate(hit = replace(hit, which(hit==T),1),
         hit = replace(hit, which(hit==F),0),
         cr = replace(cr, which(cr==T),1),
         cr = replace(cr, which(cr==F),0),
         miss = replace(miss, which(miss==T),1),
         miss = replace(miss, which(miss==F),0),
         fa = replace(fa, which(fa==T),1),
         fa = replace(fa, which(fa==F),0)) %>%
  # filter(interp == 1) %>%
  group_by(userCode) %>%
  summarise('aPrime' = dprime(sum(hit),sum(miss),sum(fa),sum(cr))$aprime)
  

## STATISTICS WILCOXON ####
# Interpretation DN vs interpretation NC
wilcox.test(x = df.all$interp[df.all$cond == 'DN'],
            y = df.all$interp[df.all$cond == 'NC'])
interp.cm <- df.all %>%
  # filter(cond == 'DN' | cond == 'NC') %>%
  compare_means(interp ~ cond2,data = .)

# Difference in means of selecting DN and NC recording among correct interpretation
wilcox.test(x = corrInt.df$recording[corrInt.df$cond == 'NC'],
       y = corrInt.df$recording[corrInt.df$cond == 'DN'])
rec.cm <- corrInt.df %>%
  compare_means(recording ~ cond,data = .,group.by = 'userCode')

# Difference between NC and DN context-matched recording selection
wilcox.test(x = as.numeric(match.df$match[match.df$newCond == 'DN']), 
       y = as.numeric(match.df$match[match.df$newCond == 'NC']))

# Difference between DN rec & matched
wilcox.test(x = as.numeric(match.df$match[match.df$newCond == 'DN']), 
            y = corrInt.df$recording[corrInt.df$cond == 'DN'])

# Difference between NC rec & matched
wilcox.test(x = corrInt.df$recording[corrInt.df$cond == 'NC'], 
            y = as.numeric(match.df$match[match.df$newCond == 'NC']))

## One proportion z-test ####
samp = df.all$interp[df.all$cond == 'DN'] # yes
binom.test(x = sum(samp),
           n = length(samp),
           p = 0.5, alternative = "two.sided")

samp = df.all$interp[df.all$cond == 'NC'] # yes
binom.test(x = sum(samp),
           n = length(samp),
           p = 0.5, alternative = "two.sided")

samp = df.all$interp[df.all$cond == 'All not'] # yes
binom.test(x = sum(samp),
           n = length(samp),
           p = 0.5, alternative = "two.sided")

samp = df.all$interp[df.all$cond == 'Not All'] # yes
binom.test(x = sum(samp),
           n = length(samp),
           p = 0.5, alternative = "two.sided")

# Correct selection of DN recording among correct interpretation vs chance
samp = corrInt.df$recording[corrInt.df$cond == 'DN'] # yes
binom.test(x = sum(samp),
           n = length(samp),
           p = 0.5, alternative = "two.sided")
# Correct selection of NC recording among correct interpretation vs chance
samp = corrInt.df$recording[corrInt.df$cond == 'NC'] # no
binom.test(x = sum(samp),
           n = length(samp),
           p = 0.5, alternative = "two.sided")

# Selection of a recording that matched DN interpretation
samp = match.df$match[match.df$newCond == 'DN'] # yes but barely
binom.test(x = sum(samp),
           n = length(samp),
           p = 0.5, alternative = "two.sided")
# Selection of a recording that matched NC interpretation
samp = match.df$match[match.df$newCond == 'NC'] # no
binom.test(x = sum(samp),
           n = length(samp),
           p = 0.5, alternative = "two.sided")

# Number of items in interp data
sum(df.all$cond2 == 'DN')
sum(df.all$cond2 == 'NC')
sum(df.all$cond2 == 'Filler')

# Number of items in original data
sum(corrInt.df$cond2 == 'DN')
sum(corrInt.df$cond2 == 'NC')

# Number of items in re-coded data
sum(match.df$cond2 == 'DN' & match.df$newCond == 'DN')
sum(match.df$cond2 == 'NC' & match.df$newCond == 'NC')
sum(match.df$cond2 == 'DN' & match.df$newCond == 'NC')
sum(match.df$cond2 == 'NC' & match.df$newCond == 'DN')

# # Mean a-prime
# samp = dprime.summ$aPrime
# binom.test(x = samp,
#            n = length(samp),
#            p = 0.5, alternative = "two.sided")
# samp = dprimeMatch.summ$aPrime
# binom.test(x = sum(samp),
#            n = length(samp),
#            p = 0.5, alternative = "two.sided")


conf.summ <- match.df %>%
  # filter(interp == 1) %>%
  group_by(newCond,match) %>%
  summarise(mean(confidence))

participantConf <- match.df %>%
  group_by(userCode,newCond) %>%
  summarise(mean(confidence),mean(match))
  

######
dprime.sort <- dprime.summ[order(dprime.summ$aPrime),]
ggplot(data = dprime.sort) + 
  geom_point(aes(x = 1:length(dprime.sort$aPrime), y = aPrime)) +
  coord_cartesian(ylim = c(0,1))

