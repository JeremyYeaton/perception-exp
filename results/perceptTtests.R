source('check_results.R')
## STATISTICS ####
# Interpretation DN vs chance
t.test(x = df.all$interp[df.all$cond == 'DN']-.5)
# Interpretation NC vs chance
t.test(x = df.all$interp[df.all$cond == 'NC']-.5)
# Controls
t.test(x = df.all$interp[df.all$cond == 'All not']-.5)
t.test(x = df.all$interp[df.all$cond == 'Not All']-.5)
# Interpretation DN vs interpretation NC
t.test(x = df.all$interp[df.all$cond == 'DN'],y = df.all$interp[df.all$cond == 'NC'])

# Mean a-prime
t.test(x = dprime.summ$aPrime-.5)
t.test(x = dprimeMatch.summ$aPrime-.5)

# Correct selection of DN recording among correct interpretation vs chance
t.test(x = corrInt.df$recording[corrInt.df$cond == 'DN']-.5)
# Correct selection of NC recording among correct interpretation vs chance
t.test(x = corrInt.df$recording[corrInt.df$cond == 'NC']-.5)
# Difference in means of selecting DN and NC recording among correct interpretation
t.test(x = corrInt.df$recording[corrInt.df$cond == 'NC'],
       y = corrInt.df$recording[corrInt.df$cond == 'DN'])

# Selection of a recording that matched DN interpretation
t.test(x = match.df$match[match.df$newCond == 'DN']-.5)
# Selection of a recording that matched NC interpretation
t.test(x = match.df$match[match.df$newCond == 'NC']-.5)
# Difference between NC and DN context-matchd recording selection
t.test(x = match.df$match[match.df$newCond == 'DN']-.5, 
       y = match.df$match[match.df$newCond == 'NC']-.5)

## Wilcoxon "against chance" tests ####
# Interpretation DN vs chance
wilcox.test(x = df.all$interp[df.all$cond == 'DN']-.5)
# Interpretation NC vs chance
wilcox.test(x = df.all$interp[df.all$cond == 'NC']-.5)
# Controls
wilcox.test(x = df.all$interp[df.all$cond == 'All not']-.5)
wilcox.test(x = df.all$interp[df.all$cond == 'Not All']-.5)

# Correct selection of DN recording among correct interpretation vs chance
wilcox.test(x = corrInt.df$recording[corrInt.df$cond == 'DN']-.5)
# Correct selection of NC recording among correct interpretation vs chance
wilcox.test(x = corrInt.df$recording[corrInt.df$cond == 'NC']-.5)

# Mean a-prime
wilcox.test(x = dprime.summ$aPrime-.5)
wilcox.test(x = dprimeMatch.summ$aPrime-.5)

# Selection of a recording that matched DN interpretation
wilcox.test(x = match.df$match[match.df$newCond == 'DN']-.5)
# Selection of a recording that matched NC interpretation
wilcox.test(x = match.df$match[match.df$newCond == 'NC']-.5)