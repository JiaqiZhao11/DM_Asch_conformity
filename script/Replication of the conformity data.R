library(readxl)
df <- read_excel("JiaqiZhao#1783/Asch social conformity/Asch conformity Data in general exl.XLSX", skip = 1)

library(tidyverse)

#df %>% ggplot(aes(x = as.factor(Group), y = `Conmformity Index`))+geom_boxplot()

#dfsum = df %>% group_by(Group) %>% summarize(n = n(), mean = mean(`Conmformity Index`), se = sd(`Conmformity Index`/sqrt(n)))

#dfsum %>% ggplot(aes(x = as.factor(Group), y = mean, fill = as.factor(Group)))+geom_bar(stat = 'identity')+geom_errorbar(aes(y = mean, ymin = mean-se, ymax = mean+se), width = 0.1)

# SelfEsteem
df %>% ggplot(aes(x = as.factor(Group), y = `SelfEsteem`))+geom_boxplot()
df[complete.cases(df),]
dfsum = df %>% group_by(Group) %>% summarize(n = n(), mean = mean(`SelfEsteem`), se = sd(`SelfEsteem`/sqrt(n)))

dfsum %>% ggplot(aes(x = as.factor(Group), y = mean, fill = as.factor(Group)))+geom_bar(stat = 'identity')+geom_errorbar(aes(y = mean, ymin = mean-se, ymax = mean+se), width = 0.1)


#df %>% ggplot(aes(x = as.factor(Group), y = `Conformity Error`))+geom_boxplot()

#dfsum = df %>% group_by(Group) %>% summarize(n = n(), mean = mean(`Conformity Error`), se = sd(`Conformity Error`/sqrt(n)))

#dfsum %>% ggplot(aes(x = as.factor(Group), y = mean, fill = as.factor(Group)))+geom_bar(stat = 'identity')+geom_errorbar(aes(y = mean, ymin = mean-se, ymax = mean+se), width = 0.1)
