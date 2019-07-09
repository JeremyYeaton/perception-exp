## From before ####
df <- df.all %>%
  filter(condition == 'NC' | condition == 'DN') %>%
  filter(verb != 'dire' & verb != 'faire') 

df.filt <- df %>%
  filter(c1 == TRUE)

summ1.df <- df %>%
  group_by(condition) %>%
  summarize('c1'=mean(c1),
            'c2'=mean(c2),
            'c3'=mean(c3)
            # ,'c4'=mean(c4),
            # 'confidence'=mean(response3)/5
  ) %>%
  ungroup()
summ1.df

cb.err <- df %>%
  group_by(condition) %>%
  summarize('c1.se'=sd(c1)/sqrt(length(df$c1)),
            'c2.se'=sd(c2)/sqrt(length(df$c2)),
            'c3.se'=sd(c3)/sqrt(length(df$c3))
            # ,'c4.se'=sd(c4)/sqrt(length(df$c4)),
            # 'conf.se'=sd(response3)/sqrt(length(df$response3))
  ) %>%
  melt(.,id.vars="condition") %>%
  mutate('condition' = ordered(condition,levels=c('NC','DN','Not All','All not')))
cb.err

cbFilt.err <- df.filt %>%
  group_by(condition) %>%
  summarize('c1.se'=sd(c1)/sqrt(length(df$c1)),
            'c2.se'=sd(c2)/sqrt(length(df$c2))
  ) %>%
  melt(.,id.vars="condition") %>%
  mutate('condition' = ordered(condition,levels=c('NC','DN','Not All','All not')))
cbFilt.err

int.summ <- df %>%
  filter(condition=='NC'|condition=='DN') %>%
  mutate('NC'=((condition=='NC' & c3==T)|(condition=='DN' & c4==T)),
         'DN'=((condition=='DN' & c3==T)|(condition=='NC' & c4==T))) %>%
  summarize('NC'=mean(NC),'DN'=mean(DN))
int.summ

combo <- df %>%
  group_by(condition) %>%
  summarize('context'=mean(c1),
            'audio'=mean(c2),
            'both'=mean(c3)
            # ,'neither'=mean(c4),
            # 'confidence'=mean(response3)/5
  ) %>%
  ungroup() %>%
  melt(.,id.vars="condition") %>%
  mutate('condition' = ordered(condition,levels=c('NC','DN','Not All','All not'))) %>%
  cbind('sem'=cb.err$value)
combo

combo.filt <- df.filt %>%
  group_by(condition) %>%
  summarize('context'=mean(c1),
            'context&audio'=mean(c3)
  ) %>%
  ungroup() %>%
  melt(.,id.vars="condition") %>%
  mutate('condition' = ordered(condition,levels=c('NC','DN','Not All','All not'))) %>%
  cbind('sem'=cbFilt.err$value)
combo.filt

participant.summ <- df %>%
  group_by(userCode,condition) %>%
  summarize('c1'=mean(c1),'c2'=mean(c2),'c3'=mean(c3),'confidence'=mean(response3),'records'=mean(records)) %>%
  ungroup()
participant.summ

sex.summ <- df %>%
  group_by(sex,condition) %>%
  summarize('c1'=mean(c1),'c2'=mean(c2),'c3'=mean(c3),'confidence'=mean(response3),'records'=mean(records)) %>%
  ungroup()
sex.summ

item.summ <- df %>%
  mutate('Item' = as.numeric(Item)) %>%
  group_by(Item) %>%
  summarize('c1'=mean(c1),'c2'=mean(c2),'c3'=mean(c3),'confidence'=mean(response3)) %>%
  merge(stim,by='Item') %>%
  ungroup()
item.summ

## PLOTS ####
summ1.plot <- ggplot(combo,aes(x=condition,y=value,ymin=value-sem,ymax=value+sem,fill=variable)) +
  geom_bar(stat='identity',position=position_dodge()) +
  geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  lims(y=c(0,1)) +
  labs(x='condition',y='proportion')
summ1.plot

summ2.plot <- ggplot(combo.filt,aes(x=condition,y=value,ymin=value-sem,ymax=value+sem,fill=variable)) +
  geom_bar(stat='identity',position=position_dodge()) +
  geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  lims(y=c(0,1)) +
  labs(x='condition',y='proportion')
summ2.plot

items.plot <- item.summ %>%
  group_by(condition) %>%
  ggplot(data=.,aes(x=Item,fill=condition)) +
  # geom_bar(aes(y=c1),stat='identity') +
  # geom_bar(aes(y=c2),stat='identity') +
  # geom_bar(aes(y=c1),stat='identity') +
  geom_bar(aes(y=c2),stat='identity') +
  lims(y=c(0,1))
items.plot

subj.plot <- ggplot(participant.summ,aes(x=userCode,fill=condition)) +
  # geom_bar(aes(y=c1/4),stat='identity') +
  # geom_bar(aes(y=c2/4),stat='identity') +
  # geom_bar(aes(y=confidence/20),stat='identity') +
  geom_bar(aes(y=c1/4),stat='identity') +
  lims(y=c(0,1)) +
  theme(axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(x='participant')
subj.plot

sex.plot <- sex.summ %>%
  ggplot(data=.,aes(x=sex,fill=condition)) +
  # geom_bar(aes(y=c1/4),stat='identity') +
  # geom_bar(aes(y=c2/4),stat='identity') +
  geom_bar(aes(y=confidence/20),stat='identity') +
  lims(y=c(0,1))
sex.plot

inter.plot <- int.summ %>%
  melt(.,id.vars="condition") %>%
  ggplot(data=.) %>%
  geom_bar(aes(x=sum(NC)))
inter.plot

users %>%
  group_by(sex)%>%
  summarize(length(sex))

t.test(df$c1[df$sex=='male'],df$c1[df$sex=='female'])
t.test(df$c2[df$sex=='male'],df$c2[df$sex=='female'])
t.test(df$response3[df$sex=='male'],df$response3[df$sex=='female'])
