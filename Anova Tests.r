Input = ("
Instructor       Student  Sodium
'Brendon Small'      a    1200
'Brendon Small'      b    1400
'Brendon Small'      c    1350
'Brendon Small'      d     950
'Brendon Small'      e    1400
'Brendon Small'      f    1150
'Brendon Small'      g    1300
'Brendon Small'      h    1325
'Brendon Small'      i    1425
'Brendon Small'      j    1500
'Brendon Small'      k    1250
'Brendon Small'      l    1150
'Brendon Small'      m     950
'Brendon Small'      n    1150
'Brendon Small'      o    1600
'Brendon Small'      p    1300
'Brendon Small'      q    1050
'Brendon Small'      r    1300
'Brendon Small'      s    1700
'Brendon Small'      t    1300
'Coach McGuirk'      u    1100
'Coach McGuirk'      v    1200
'Coach McGuirk'      w    1250
'Coach McGuirk'      x    1050
'Coach McGuirk'      y    1200
'Coach McGuirk'      z    1250
'Coach McGuirk'      aa   1350
'Coach McGuirk'      ab   1350
'Coach McGuirk'      ac   1325
'Coach McGuirk'      ad   1525
'Coach McGuirk'      ae   1225
'Coach McGuirk'      af   1125
'Coach McGuirk'      ag   1000
'Coach McGuirk'      ah   1125
'Coach McGuirk'      ai   1400
'Coach McGuirk'      aj   1200
'Coach McGuirk'      ak   1150
'Coach McGuirk'      al   1400
'Coach McGuirk'      am   1500
'Coach McGuirk'      an   1200
'Melissa Robins'     ao   900
'Melissa Robins'     ap   1100
'Melissa Robins'     aq   1150
'Melissa Robins'     ar   950
'Melissa Robins'     as   1100
'Melissa Robins'     at   1150
'Melissa Robins'     au   1250
'Melissa Robins'     av   1250
'Melissa Robins'     aw   1225
'Melissa Robins'     ax   1325
'Melissa Robins'     ay   1125
'Melissa Robins'     az   1025
'Melissa Robins'     ba    950
'Melissa Robins'     bc    925
'Melissa Robins'     bd   1200
'Melissa Robins'     be   1100
'Melissa Robins'     bf    950
'Melissa Robins'     bg   1300
'Melissa Robins'     bh   1400
'Melissa Robins'     bi   1100
")

Data = read.table(textConnection(Input),header=TRUE)

head(Data)

Data$Instructor

class(Data)

# Order factors by the order in data frame
# Otherwise, R will alphabetize them

Data$Instructor = factor(Data$Instructor, levels=unique(Data$Instructor))

Data$Instructor

#install.packages("FSA")

library(FSA)

Summarize(data = Data, object = Sodium ~ Instructor, digits = 3)

boxplot(Sodium ~ Instructor, data = Data)

library(tidyverse)

head(Data)

table(Data$Instructor)

#install.packages("rcompanion")

library(rcompanion)

groupwiseMean(formula = Sodium ~ Instructor, data = Data, conf = 0.95, traditional = T, percentile = T, digits = 7)

# How do we get z scores?
# Take z score for 95% confidence interval which we know if 1.96

# We don't do z(95%) directly. Rather, the starting point is alpha/2
# Then take that value and subtract it from 100%

conf_int = 0.95
conf_trans = 1-(1-conf_int)/2
conf_trans
qnorm(conf_trans)
qnorm(0.975)

# Trying to reconstruct this table manually
# Ref: http://www.stat.ucla.edu/~rgould/110as02/bsci

Sum <- Data %>% 
group_by(Instructor) %>%
summarise(n = n(),
         mean = mean(Sodium),
         sd = sd(Sodium),
         conf_int  = 0.95,
         alpha = 1-conf_int,
         conf_trans = 1-(alpha)/2,
         t_dist = qt(p = conf_trans, df = n-1),
         std_err_est = sd/sqrt(n),
         err_margin = std_err_est*t_dist,
         perc.low = mean-err_margin,
         perc.up = mean+err_margin)

Sum

# t_dist = If we knew the population std dev, we would have used 1.96*std_dev to determine the ci. Since that is 
# unknown, we use t distribution tables.
# std_err_est = estimated std error of the sample. If we knew the std_error of the population, it would not be
# called estimated. It's estimated because we use the sd of the sample as a proxy of the sd of the population.
# err_margin = error margin

#These tie to the previous table (Trad.lower & Trad.upper)

ggplot(data = Sum, aes(x = Instructor, y = mean, label = mean)) +
geom_point() +
geom_text(nudge_x = 0.2) +
geom_errorbar(aes(ymin = perc.low, ymax = perc.up, width = 0.1)) +
theme_bw() +
labs(y = "Mean sodium, mg", x = "Instructor name", title = "Sodium consumption")

str(Data)

stripchart(Sodium~Instructor, data=Data, vertical = T, jitter = 0.2, pch = 19, xlab = "Instructor", ylab = "Sodium,mg")

model_sodium <- lm(Sodium ~ Instructor, Data)

summary(model_sodium)

anova(model_sodium) %>% 
mutate(p_val = `Pr(>F)`<0.05)

car::Anova(model_sodium) %>% 
rownames_to_column('Var') %>% 
mutate(p_val = `Pr(>F)`<0.05)

plot(model_sodium)

#plot 1 = the variance is homegeneous for the fitted values across the 3 groups
#plot 2 = there is normality in the distribution of the residuals

TukeyHSD(aov(model_sodium))

#aov is another form of doing an analysis of variance

broom::tidy(TukeyHSD(aov(model_sodium))) %>% 
mutate(p_val = adj.p.value<0.05)

ggplot(data = Sum, aes(x = Instructor, y = mean, label = mean)) +
geom_point() +
geom_text(nudge_x = 0.2) +
geom_errorbar(aes(ymin = perc.low, ymax = perc.up, width = 0.1)) +
theme_bw() +
labs(y = "Mean sodium, mg", x = "Instructor name", title = "Sodium consumption")

Input = ("
Instructor        Town             Sodium
'Brendon Small'   Squiggleville    1200
'Brendon Small'   Squiggleville    1400
'Brendon Small'   Squiggleville    1350
'Brendon Small'   Metalocalypse     950
'Brendon Small'   Squiggleville    1400
'Brendon Small'   Squiggleville    1150
'Brendon Small'   Squiggleville    1300
'Brendon Small'   Metalocalypse    1325
'Brendon Small'   Metalocalypse    1425
'Brendon Small'   Squiggleville    1500
'Brendon Small'   Squiggleville    1250
'Brendon Small'   Metalocalypse    1150
'Brendon Small'   Metalocalypse     950
'Brendon Small'   Squiggleville    1150
'Brendon Small'   Metalocalypse    1600
'Brendon Small'   Metalocalypse    1300
'Brendon Small'   Metalocalypse    1050
'Brendon Small'   Metalocalypse    1300
'Brendon Small'   Squiggleville    1700
'Brendon Small'   Squiggleville    1300
'Coach McGuirk'   Squiggleville    1100
'Coach McGuirk'   Squiggleville    1200
'Coach McGuirk'   Squiggleville    1250
'Coach McGuirk'   Metalocalypse    1050
'Coach McGuirk'   Metalocalypse    1200
'Coach McGuirk'   Metalocalypse    1250
'Coach McGuirk'   Squiggleville    1350
'Coach McGuirk'   Squiggleville    1350
'Coach McGuirk'   Squiggleville    1325
'Coach McGuirk'   Squiggleville    1525
'Coach McGuirk'   Squiggleville    1225
'Coach McGuirk'   Squiggleville    1125
'Coach McGuirk'   Metalocalypse    1000
'Coach McGuirk'   Metalocalypse    1125
'Coach McGuirk'   Squiggleville    1400
'Coach McGuirk'   Metalocalypse    1200
'Coach McGuirk'   Squiggleville    1150
'Coach McGuirk'   Squiggleville    1400
'Coach McGuirk'   Squiggleville    1500
'Coach McGuirk'   Squiggleville    1200
'Melissa Robins'  Metalocalypse     900
'Melissa Robins'  Metalocalypse    1100
'Melissa Robins'  Metalocalypse    1150
'Melissa Robins'  Metalocalypse     950
'Melissa Robins'  Metalocalypse    1100
'Melissa Robins'  Metalocalypse    1150
'Melissa Robins'  Squiggleville    1250
'Melissa Robins'  Squiggleville    1250
'Melissa Robins'  Squiggleville    1225
'Melissa Robins'  Squiggleville    1325
'Melissa Robins'  Metalocalypse    1125
'Melissa Robins'  Metalocalypse    1025
'Melissa Robins'  Metalocalypse     950
'Melissa Robins'  Metalocalypse     925
'Melissa Robins'  Squiggleville    1200
'Melissa Robins'  Metalocalypse    1100
'Melissa Robins'  Metalocalypse     950
'Melissa Robins'  Metalocalypse    1300
'Melissa Robins'  Squiggleville    1400
'Melissa Robins'  Metalocalypse    1100
")

Data = read.table(textConnection(Input),header=TRUE)

###  Order factors by the order in data frame
###  Otherwise, R will alphabetize them

Data$Instructor = factor(Data$Instructor,
                         levels=unique(Data$Instructor))

Data$Town       = factor(Data$Town,
                         levels=unique(Data$Town))

model_sodium <- lm(Sodium~Instructor+Town, data = Data)

summary(model_sodium)

library(tidyverse)

anova(model_sodium)%>%
rownames_to_column('Var') %>% 
mutate(p_val = `Pr(>F)`<0.05)

car::Anova(model_sodium) %>% 
rownames_to_column('Var') %>% 
mutate(p_val = `Pr(>F)`<0.05)

table(Data$Instructor, Data$Town)

plot(model_sodium, which=1)

plot(model_sodium, which=2)

TukeyHSD(aov(model_sodium))

#install.packages('lsmeans')

marginal <- lsmeans::lsmeans(model_sodium, 'Instructor')

marginal

pairs(marginal, adjust="tukey")

broom::tidy(pairs(marginal, adjust="tukey")) %>% 
mutate(p_val = p.value<0.05)

1280-1155

broom::tidy(marginal)

ggplot(broom::tidy(marginal), aes(x = Instructor, y = estimate, label = round(estimate)))+
geom_point()+
theme_bw()+
geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0.1))+
geom_text(nudge_x = 0.15)+
labs(y = "Least Square Means", x="")

Input = ("
Instructor        Town             Sodium
'Brendon Small'   Squiggleville    1200
'Brendon Small'   Squiggleville    1400
'Brendon Small'   Squiggleville    1350
'Brendon Small'   Metalocalypse     950
'Brendon Small'   Squiggleville    1400
'Brendon Small'   Squiggleville    1150
'Brendon Small'   Squiggleville    1300
'Brendon Small'   Metalocalypse    1325
'Brendon Small'   Metalocalypse    1425
'Brendon Small'   Squiggleville    1500
'Brendon Small'   Squiggleville    1250
'Brendon Small'   Metalocalypse    1150
'Brendon Small'   Metalocalypse     950
'Brendon Small'   Squiggleville    1150
'Brendon Small'   Metalocalypse    1600
'Brendon Small'   Metalocalypse    1300
'Brendon Small'   Metalocalypse    1050
'Brendon Small'   Metalocalypse    1300
'Brendon Small'   Squiggleville    1700
'Brendon Small'   Squiggleville    1300
'Coach McGuirk'   Squiggleville    1100
'Coach McGuirk'   Squiggleville    1200
'Coach McGuirk'   Squiggleville    1250
'Coach McGuirk'   Metalocalypse    1050
'Coach McGuirk'   Metalocalypse    1200
'Coach McGuirk'   Metalocalypse    1250
'Coach McGuirk'   Squiggleville    1350
'Coach McGuirk'   Squiggleville    1350
'Coach McGuirk'   Squiggleville    1325
'Coach McGuirk'   Squiggleville    1525
'Coach McGuirk'   Squiggleville    1225
'Coach McGuirk'   Squiggleville    1125
'Coach McGuirk'   Metalocalypse    1000
'Coach McGuirk'   Metalocalypse    1125
'Coach McGuirk'   Squiggleville    1400
'Coach McGuirk'   Metalocalypse    1200
'Coach McGuirk'   Squiggleville    1150
'Coach McGuirk'   Squiggleville    1400
'Coach McGuirk'   Squiggleville    1500
'Coach McGuirk'   Squiggleville    1200
'Melissa Robins'  Metalocalypse     900
'Melissa Robins'  Metalocalypse    1100
'Melissa Robins'  Metalocalypse    1150
'Melissa Robins'  Metalocalypse     950
'Melissa Robins'  Metalocalypse    1100
'Melissa Robins'  Metalocalypse    1150
'Melissa Robins'  Squiggleville    1250
'Melissa Robins'  Squiggleville    1250
'Melissa Robins'  Squiggleville    1225
'Melissa Robins'  Squiggleville    1325
'Melissa Robins'  Metalocalypse    1125
'Melissa Robins'  Metalocalypse    1025
'Melissa Robins'  Metalocalypse     950
'Melissa Robins'  Metalocalypse     925
'Melissa Robins'  Squiggleville    1200
'Melissa Robins'  Metalocalypse    1100
'Melissa Robins'  Metalocalypse     950
'Melissa Robins'  Metalocalypse    1300
'Melissa Robins'  Squiggleville    1400
'Melissa Robins'  Metalocalypse    1100
")

Data = read.table(textConnection(Input),header=TRUE)


###  Order factors by the order in data frame
###  Otherwise, R will alphabetize them

Data$Instructor = factor(Data$Instructor,
                         levels=unique(Data$Instructor))

Data$Town       = factor(Data$Town,
                         levels=unique(Data$Town))

#library(lme4)

library(lmerTest)

model_fxd <- lmer(formula = Sodium ~ Instructor + (1|Town), data = Data)

summary(model_fxd)

anova(model_fxd)
#p values for the fixed effects

rand(model_fxd)
#p values for random effects

marginal <- lsmeans::lsmeans(model_fxd,"Instructor")

marginal

pairs(marginal, adjust="tukey")

boxplot(Data$Sodium~Data$Instructor)

Input = ("
Instructor        Supplement  Sodium
'Brendon Small'   A           1200
'Brendon Small'   A           1400
'Brendon Small'   A           1350
'Brendon Small'   A            950
'Brendon Small'   A           1400
'Brendon Small'   B           1150
'Brendon Small'   B           1300
'Brendon Small'   B           1325
'Brendon Small'   B           1425
'Brendon Small'   B           1500
'Brendon Small'   C           1250
'Brendon Small'   C           1150
'Brendon Small'   C            950
'Brendon Small'   C           1150
'Brendon Small'   C           1600
'Brendon Small'   D           1300
'Brendon Small'   D           1050
'Brendon Small'   D           1300
'Brendon Small'   D           1700
'Brendon Small'   D           1300
'Coach McGuirk'   A           1100
'Coach McGuirk'   A           1200
'Coach McGuirk'   A           1250
'Coach McGuirk'   A           1050
'Coach McGuirk'   A           1200
'Coach McGuirk'   B           1250
'Coach McGuirk'   B           1350
'Coach McGuirk'   B           1350
'Coach McGuirk'   B           1325
'Coach McGuirk'   B           1525
'Coach McGuirk'   C           1225
'Coach McGuirk'   C           1125
'Coach McGuirk'   C           1000
'Coach McGuirk'   C           1125
'Coach McGuirk'   C           1400
'Coach McGuirk'   D           1200
'Coach McGuirk'   D           1150
'Coach McGuirk'   D           1400
'Coach McGuirk'   D           1500
'Coach McGuirk'   D           1200
'Melissa Robins'  A            900
'Melissa Robins'  A           1100
'Melissa Robins'  A           1150
'Melissa Robins'  A            950
'Melissa Robins'  A           1100
'Melissa Robins'  B           1150
'Melissa Robins'  B           1250
'Melissa Robins'  B           1250
'Melissa Robins'  B           1225
'Melissa Robins'  B           1325
'Melissa Robins'  C           1125
'Melissa Robins'  C           1025
'Melissa Robins'  C            950
'Melissa Robins'  C            925
'Melissa Robins'  C           1200
'Melissa Robins'  D           1100
'Melissa Robins'  D            950
'Melissa Robins'  D           1300
'Melissa Robins'  D           1400
'Melissa Robins'  D           1100
")

Data = read.table(textConnection(Input),header=TRUE)

###  Order factors by the order in data frame
###  Otherwise, R will alphabetize them

Data$Instructor = factor(Data$Instructor,
                         levels=unique(Data$Instructor))

Data$Supplement       = factor(Data$Supplement,
                         levels=unique(Data$Supplement))

head(Data)

model_two <- lm(Sodium ~ Instructor + Supplement + Instructor*Supplement, Data)

summary(model_two)

car::Anova(model_two) %>% 
rownames_to_column('Var') %>% 
mutate(p_val = `Pr(>F)`<0.05)

plot(model_two)

marginal_ins <- lsmeans::lsmeans(model_two,"Instructor")

pairs(marginal_ins, adjust="tukey")

marginal_sup <- lsmeans::lsmeans(model_two,"Supplement")

pairs(marginal_sup, adjust="tukey")

boxplot(Sodium~Supplement,Data)

Input = ("
Instruction        Student  Month   Calories.per.day
'Curriculum A'     a        1       2000
'Curriculum A'     a        2       1978
'Curriculum A'     a        3       1962
'Curriculum A'     a        4       1873
'Curriculum A'     a        5       1782
'Curriculum A'     a        6       1737
'Curriculum A'     b        1       1900
'Curriculum A'     b        2       1826
'Curriculum A'     b        3       1782
'Curriculum A'     b        4       1718
'Curriculum A'     b        5       1639
'Curriculum A'     b        6       1644
'Curriculum A'     c        1       2100
'Curriculum A'     c        2       2067
'Curriculum A'     c        3       2065
'Curriculum A'     c        4       2015
'Curriculum A'     c        5       1994
'Curriculum A'     c        6       1919
'Curriculum A'     d        1       2000
'Curriculum A'     d        2       1981
'Curriculum A'     d        3       1987
'Curriculum A'     d        4       2016
'Curriculum A'     d        5       2010
'Curriculum A'     d        6       1946
'Curriculum B'     e        1       2100
'Curriculum B'     e        2       2004
'Curriculum B'     e        3       2027
'Curriculum B'     e        4       2109
'Curriculum B'     e        5       2197
'Curriculum B'     e        6       2294
'Curriculum B'     f        1       2000
'Curriculum B'     f        2       2011
'Curriculum B'     f        3       2089
'Curriculum B'     f        4       2124
'Curriculum B'     f        5       2199
'Curriculum B'     f        6       2234
'Curriculum B'     g        1       2000
'Curriculum B'     g        2       2074
'Curriculum B'     g        3       2141
'Curriculum B'     g        4       2199
'Curriculum B'     g        5       2265
'Curriculum B'     g        6       2254
'Curriculum B'     h        1       2000
'Curriculum B'     h        2       1970
'Curriculum B'     h        3       1951
'Curriculum B'     h        4       1981
'Curriculum B'     h        5       1987
'Curriculum B'     h        6       1969
'Curriculum C'     i        1       1950
'Curriculum C'     i        2       2007
'Curriculum C'     i        3       1978
'Curriculum C'     i        4       1965
'Curriculum C'     i        5       1984
'Curriculum C'     i        6       2020
'Curriculum C'     j        1       2000
'Curriculum C'     j        2       2029
'Curriculum C'     j        3       2033
'Curriculum C'     j        4       2050
'Curriculum C'     j        5       2001
'Curriculum C'     j        6       1988
'Curriculum C'     k        1       2000
'Curriculum C'     k        2       1976
'Curriculum C'     k        3       2025
'Curriculum C'     k        4       2047
'Curriculum C'     k        5       2033
'Curriculum C'     k        6       1984
'Curriculum C'     l        1       2000
'Curriculum C'     l        2       2020
'Curriculum C'     l        3       2009
'Curriculum C'     l        4       2017
'Curriculum C'     l        5       1989
'Curriculum C'     l        6       2020
")


Data = read.table(textConnection(Input),header=TRUE)

###  Order factors by the order in data frame
###  Otherwise, R will alphabetize them

Data$Instruction = factor(Data$Instruction,
                         levels=unique(Data$Instruction))

library(nlme)

# gls type of model allows correlation. In our case, there is going to be auto-correlation in the outcome var

model = lme(Calories.per.day ~ Instruction + Month + Instruction*Month,
            random = ~1|Student,
            correlation = corAR1(form = ~ Month | Student,
                                 value = 0.4287),
            data=Data,
            method="REML")

model.b = lme(Calories.per.day ~ Instruction + Month + Instruction*Month,
            random = ~1|Student,
            data=Data)

ACF(model.b)

library(car)
library(tidyverse)

Anova(model) %>% 
rownames_to_column() %>% 
mutate(p_val = `Pr(>Chisq)`<0.05)

model.fixed = gls(Calories.per.day ~ Instruction + Month + Instruction*Month,
                  data=Data,
                  method="REML")

anova(model,model.fixed)

marginal = lsmeans::lsmeans(model,"Instruction","Month")

pairs(marginal, adjust="tukey")


