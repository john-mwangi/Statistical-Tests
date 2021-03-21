dt <- read.csv("http://rfs.kvasaheim.com/data/crime.csv")

head(dt)

names(dt)

levels(dt$census4)

set.seed(123)

ts = numeric()

grp1 = dt$gspcap00[dt$census4=="Northeast"]
grp2 = dt$gspcap00[dt$census4=="Midwest"]

for (i in 1:1e4) {
    grp1mmt = sample(grp1, replace = TRUE)
    grp2mmt = sample(grp2, replace = TRUE)
    
    xbar1 = mean(grp1mmt)
    xbar2 = mean(grp2mmt)
    
    ts[i] = xbar1 - xbar2
}

hist(ts)

quantile(ts, c(0.025,0.975))

mean(dt$gspcap00[dt$census4=="Northeast"]) - mean(dt$gspcap00[dt$census4=="Midwest"])

set.seed(123)

ts = numeric()

for (i in 1:1e4){
    x = sample(dt$gspcap00, replace = TRUE)
    ts[i] = mean(x)
}

hist(ts)

quantile(ts, c(0.025,0.975))

set.seed(123)

ts = numeric()

inc = which(dt$census4=="Midwest" | dt$census4=="Northeast")

mmt = dt$gspcap00[inc]
grp = dt$census4[inc]

for (i in 1:1e4){
    grpX = sample(grp) #do not include replace=TRUE
    
    xbar1 = mean(mmt[grpX=="Northeast"])
    xbar2 = mean(mmt[grpX=="Midwest"])
    
    ts[i] = xbar1 - xbar2
}

hist(ts)

quantile(ts, c(0.025,0.975))

obs <- mean(mmt[grp=="Northeast"]) - mean(mmt[grp=="Midwest"])

mean(ts>=obs)*2 #Two sided p-value

obs

library(tidyverse)

library(lubridate)

brewing_materials_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv")

head(brewing_materials_raw)

brewing_materials <- brewing_materials_raw %>% 
mutate(date = paste0(year,"-",month,"-01"),
      date = ymd(date)) %>% 
select(date, type, month_current) %>% 
filter(type %in% c("Malt and malt products",
      "Sugar and syrups",
      "Hops (dry)")) %>% 
spread(key = type, value = month_current) %>% 
janitor::clean_names()

head(brewing_materials)

beer_fit <- lm(sugar_and_syrups ~ 0 + malt_and_malt_products, data = brewing_materials)

# This tells the model that there doesn't need to be sugar if there is no malt

summary(beer_fit)

broom::tidy(beer_fit, conf.int=T)

library(rsample)

set.seed(123)
beer_boot <- bootstraps(data = brewing_materials, times = 10000, apparent = TRUE)

head(beer_boot)

beer_models <- beer_boot %>% 
mutate(model = map(splits, ~lm(sugar_and_syrups ~ 0 + malt_and_malt_products, data = .)),
      coef_info = map(model, tidy))

head(beer_models)

beer_coefs <- beer_models %>% 
unnest(coef_info)

head(beer_coefs %>% select(term:p.value))

int_pctl(`.data` = beer_models, statistics = coef_info)


