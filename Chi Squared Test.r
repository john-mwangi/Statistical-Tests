library(FSelector)
library(mlbench)

data(HouseVotes84)

head(HouseVotes84)

#Chi squared statistics
weights <- chi.squared(formula = Class~., data = HouseVotes84)

weights

cutoff.k(attrs = weights, 5)
#Top 5 variables

cutoff.k.percent(weights, 0.75)
#Rank 75% of the variables

chisq.test(x = HouseVotes84$V10, y = HouseVotes84$Class)

summary(chisq.test(x = HouseVotes84$V10, y = HouseVotes84$Class))

typeof(chisq.test(x = HouseVotes84$V10, y = HouseVotes84$Class))

chisq.test(x = HouseVotes84$V10, y = HouseVotes84$Class)[3]

chisq.test(x = HouseVotes84$V10, y = HouseVotes84$Class)$p.value

#We want the above. To replicate it for all the variables, we use the following code...
lapply(HouseVotes84, function(x) chisq.test(x,HouseVotes84$Class))

chisq.test(x = HouseVotes84$V10, y = HouseVotes84$Class)[c('statistic','p.value')]

# Check the statistics & the p values
as.data.frame(lapply(HouseVotes84, function(x) chisq.test(x,HouseVotes84$Class)$p.value))
as.data.frame(lapply(HouseVotes84, function(x) chisq.test(x,HouseVotes84$Class)$statistic))

rbind(as.data.frame(lapply(HouseVotes84, function(x) chisq.test(x,HouseVotes84$Class)$p.value)),as.data.frame(lapply(HouseVotes84, function(x) chisq.test(x,HouseVotes84$Class)$statistic)))

#cbind = merge columns, rbind = merge rows
chi_stat_pval <- rbind(as.data.frame(lapply(HouseVotes84, function(x) chisq.test(x,HouseVotes84$Class)$p.value)),as.data.frame(lapply(HouseVotes84, function(x) chisq.test(x,HouseVotes84$Class)$statistic)))

# Drop Class column
chi_stat_pval$Class <- NULL

# Transpose
chi_stat_pval <- t(chi_stat_pval)

chi_stat_pval <- as.data.frame(chi_stat_pval)

chi_stat_pval

names(chi_stat_pval)[names(chi_stat_pval)=='1'] <- 'p_value'

names(chi_stat_pval)[names(chi_stat_pval)=='X-squared'] <- 'stat'

chi_stat_pval

chi_stat_pval$sig <- chi_stat_pval$p_value < 0.05

chi_stat_pval

# Sort by stat (desc) then by pvalue (asc)
chi_stat_pval <- chi_stat_pval[
  with(chi_stat_pval, order(-stat,p_value)),
]

chi_stat_pval

plot(chi_stat_pval$stat)
abline(v = 7,col='red')
# Seems like we can work with the top 7 features

# Top 7 features
row.names(chi_stat_pval[1:7,])


