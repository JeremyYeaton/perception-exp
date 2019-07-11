library(dplyr)
library(ggplot2)

## PLOT ####
correctInterp.plot <- ggplot(correctInt.summ,aes(x = cond, y = sumCorrect, 
                                                 fill = cond,ymin=sumCorrect-sem,ymax=sumCorrect+sem)) +
  geom_bar(stat='identity',position=position_dodge()) +
  geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  lims(y=c(0,1)) +
  labs(x='condition',y='proportion')
correctInterp.plot

correctRec.plot <- ggplot(correctRec.summ,aes(x = cond, y = sumCorrect, 
                                              fill = cond,ymin=sumCorrect-sem,ymax=sumCorrect+sem)) +
  geom_bar(stat='identity',position=position_dodge()) +
  geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  lims(y=c(0,1)) +
  labs(x='condition',y='proportion')
correctRec.plot
