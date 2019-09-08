library(dplyr)
library(ggplot2)
library(viridis)
library(ggpubr)

# Load in data and variables
source('check_results.R')
plot.w <- 18
plot.h <- 10

# Set colors
scale_colour_discrete <- function(...) {
  scale_colour_brewer(..., palette="Set1")
}

allColors <- plasma(3)
# opts <- options(ggplot2.continuous.colour="viridis")
dn_color <- allColors[2]
nc_color <- allColors[1]
mismatch <- c('#F8766D','red1','green1','#7CAE00')
mismatch2 <- c('red1','#F8766D','green1','#7CAE00')
controls <- c('darkorange2','magenta4')
conditions <- c('#F8766D','#7CAE00','#00BFC4','#C77CFF')
conditions2 <- c(dn_color,nc_color)
conditions3 <- c(nc_color,dn_color)

yLimits = c(0,1)
## BOXPLOTs ####
correctInterp.plot <- ggplot(correctInt.summ,aes(x = cond2, y = sumCorrect, 
                                                 ymin=sumCorrect-sem,
                                                 ymax=sumCorrect+sem, fill = cond2)) +
  # geom_bar(stat='identity',position=position_dodge()) +
  # geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  geom_boxplot(position=position_dodge()) +
  coord_cartesian(ylim=yLimits) +
  labs(x='Condition',y='Proportion correct') +
  theme(legend.position = 'none') +
  scale_fill_manual(values = c(nc_color,dn_color,allColors[3]))
correctInterp.plot
ggsave(plot=correctInterp.plot,'figures/correctInterp.pdf',width=plot.w/2,height=plot.h,units="cm")

correctRec.plot <- ggplot(correctRec.summ,aes(x = cond, y = sumCorrect, 
                                              fill = cond,ymin=sumCorrect-sem,ymax=sumCorrect+sem)) +
  # geom_bar(stat='identity',position=position_dodge()) +
  # geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  geom_boxplot(position=position_dodge()) +
  # stat_compare_means(comparisons = my_comparisons,method = 'wilcox.test') +
  coord_cartesian(ylim=yLimits) +
  labs(x='Condition',y='Proportion correct') +
  theme(legend.position = 'none') +
  scale_fill_manual(values = c(nc_color,dn_color))
  # stat_pvalue_manual(
  #   rec.cm, x = 'group1', y.position = 33,
  #   label = "p.signif",
  #   position = position_dodge(0.8)
  # )

correctRec.plot <- ggplot(corrInt.df,aes(x = cond, y = recording,fill = cond)) +
  geom_boxplot(position=position_dodge()) +
  # stat_compare_means(comparisons = my_comparisons,method = 'wilcox.test') +
  # coord_cartesian(ylim=yLimits) +
  labs(x='Condition',y='Proportion correct') +
  theme(legend.position = 'none') +
  scale_fill_manual(values = c(nc_color,dn_color))
  # stat_pvalue_manual(
  #   rec.cm, x = 'group1', y.position = 33,
  #   label = "p.signif",
  #   position = position_dodge(0.8)
  # )
correctRec.plot
ggsave(plot=correctRec.plot,'figures/correctRec.pdf',width=plot.w/2,height=plot.h,units="cm")

my_comparisons <- c("NC", "DN")

matchRec.plot <- ggplot(match.summ, aes(x = newCond, y = meanMatch, fill = newCond,
                                        ymin = meanMatch - sem, ymax = meanMatch + sem))+
  # geom_bar(stat='identity',position=position_dodge()) +
  # geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  geom_boxplot(position=position_dodge()) +
  coord_cartesian(ylim=yLimits) +
  labs(x='Condition',y='Proportion correct') +
  scale_fill_manual(values = c(nc_color,dn_color)) +
  theme(legend.position = 'none')
matchRec.plot
ggsave(plot=matchRec.plot,'figures/matchRec.pdf',width=plot.w/2,height=plot.h,units="cm")

match.df1 <- match.df %>%
  group_by(userCode, newCond)

match.df1summ <- match.df %>%
  group_by(userCode, newCond) %>%
  summarize(match = mean(match))

stat.test <- compare_means(match ~ newCond,data = match.df1,
                           method = 't.test',group_by = "userCode")
  
# match.df1$match[match.df1$match == T] <- 1
# match.df1$match[match.df1$match == F] <- 0

matchRec.plot <- ggplot(match.df1, aes(x = newCond, y = match)) +
  geom_boxplot(position=position_dodge()) +
  # geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  coord_cartesian(ylim=yLimits) +
  labs(x='Condition',y='Proportion correct') +
  # stat_compare_means(comparisons = my_comparisons, method = 'wilcox.test') +
  stat_pvalue_manual(
    stat.test, x = "group1", y.position = 1,
    label = "p.adj",
    position = position_dodge(0.8)
  ) +
  scale_fill_manual(values = c(nc_color,dn_color)) +
  theme(legend.position = 'none')
matchRec.plot


aprime.plot <- ggplot(dprimeMatch.summ, aes(y= aPrime)) +
  geom_boxplot(position=position_dodge()) +
  geom_point(aes(x= 0)) +
  geom_hline(aes(yintercept = .5),linetype = 'dashed') +
  coord_cartesian(ylim=yLimits) +
  labs(x='',y='A-prime') +
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
  
aprime.plot
ggsave(plot=aprime.plot,'figures/aprime.pdf',width=plot.w/2,height=plot.h,units="cm")

## BAR PLOTS ####
correctInterp.barplot <- ggplot(correctInt.barsum,aes(x = cond2, y = sumCorrect, 
                                                 ymin=sumCorrect-sem,
                                                 ymax=sumCorrect+sem, fill = cond2)) +
  geom_bar(stat='identity',position=position_dodge()) +
  geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  coord_cartesian(ylim=yLimits) +
  labs(x='Condition',y='Proportion correct') +
  theme(legend.position = 'none') +
  scale_fill_manual(values = c(nc_color,dn_color,allColors[3]))
correctInterp.barplot
ggsave(plot=correctInterp.barplot,'figures/correctInterpBar.pdf',width=plot.w/3,height=plot.h,units="cm")

correctRec.barplot <- ggplot(correctRec.barsum,aes(x = cond, y = sumCorrect, 
                                              fill = cond,ymin=sumCorrect-sem,ymax=sumCorrect+sem)) +
  geom_bar(stat='identity',position=position_dodge()) +
  geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  coord_cartesian(ylim=yLimits) +
  labs(x='Condition',y='Proportion correct') +
  theme(legend.position = 'none',axis.title.y=element_blank()) +
  scale_fill_manual(values = c(nc_color,dn_color))
correctRec.barplot
ggsave(plot=correctRec.barplot,'figures/correctRecBar.pdf',width=plot.w/3,height=plot.h,units="cm")


matchRec.barplot <- ggplot(match.barsum, aes(x = newCond, y = meanMatch, fill = newCond,
                                        ymin = meanMatch - sem, ymax = meanMatch + sem))+
  geom_bar(stat='identity',position=position_dodge()) +
  geom_errorbar(position =position_dodge(width=.9),width=0.25) +
  coord_cartesian(ylim=yLimits) +
  labs(x='Condition',y='Proportion correct') +
  scale_fill_manual(values = c(nc_color,dn_color)) +
  theme(legend.position = 'none',axis.title.y=element_blank())
matchRec.barplot
ggsave(plot=matchRec.barplot,'figures/matchRecBar.pdf',width=plot.w/3,height=plot.h,units="cm")
