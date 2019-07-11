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
  na.omit() %>%
  select(Item, userCode, cond, verb, interp, recording, bothRight, bothWrong, match, confidence)

## SUMMARISE ####

correctInt.summ <- df.all %>%
  filter(verb != 'faire' & verb != 'dire') %>%
  # filter(cond == 'NC' | cond == 'DN') %>%
  # filter(interp == 1) %>%
  group_by(cond) %>%
  summarise(sumCorrect = mean(interp),
            sem = sd(interp)/sqrt(length(df.all$interp)))

correctRec.summ <- df.all %>%
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
  summarise('dPrime' = dprime(sum(hit),sum(miss),sum(fa),sum(cr))$aprime)

corrInt.df <- df.all %>%
  filter(interp == 1) %>%
  # filter(match == T) %>%
  filter(cond == 'NC' | cond == 'DN') %>%
  group_by(userCode)

## STATISTICS ####
mean(dprime.summ$dPrime)

t.test(x = corrInt.df$recording[corrInt.df$cond == 'DN']-.5)
t.test(x = corrInt.df$recording[corrInt.df$cond == 'NC'],y = corrInt.df$recording[corrInt.df$cond == 'DN'])
t.test(x = df.all$interp[df.all$cond == 'DN']-.5)#,y = df.all$interp[df.all$cond == 'DN'])
