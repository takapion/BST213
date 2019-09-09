t.test(lbw$bwt~lbw$smoke, alternative="two.sided",conf.level = .95, var.equal = TRUE)
t.test(lbw$bwt~lbw$smoke, alternative="two.sided",conf.level = .95, var.equal = FALSE)
t.test(lbw$bwt~lbw$smoke,conf.level = .95, var.equal = TRUE
summary(aov(lbw$wt~lbw$race))
lbw$race<-as.factor(lbw$race)
summary(aov(lbw$bwt~lbw$race))
t.test(lbw$bwt~lbw$ht, alternative="two.sided",conf.level = .95, var.equal = TRUE)
t.test(lbw$bwt~lbw$ui, alternative="two.sided",conf.level = .95, var.equal = TRUE)
lbw$race<-as.factor(lbw$race)
summary(aov(lbw$bwt~lbw$race))
cor.test(lbt$bwt~lbt$ftv)
cor.test(lbw$bwt,lbw$ftv)

# test for normality
library("ggpubr")
ggdensity(lbw$age, )

library(ggpubr)
ggqqplot(lbw$age)

library("ggpubr")
ggdensity(lbw$lwt)
ggqqplot(lbw$lwt)

# the Shapiro-Wilk test of normality for one variable (univariate)
shapiro.test(lbw$lwt)
