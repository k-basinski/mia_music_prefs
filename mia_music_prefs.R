library(tidyverse)
library(ez)
library(Hmisc)
library(cowplot)

# Check this to see the file path is correct
d <- read.csv("data_mia.csv")

# some custom funcitons for means and sd's
my_mean <- function(x) {
  mean(x, na.rm = T) %>% round(2)
}

my_sd <- function(x) {
  sd(x, na.rm = T) %>% round(2)
}


# Plot text
localeSet <- "EN" # EN albo PL

if(localeSet == "PL"){
  xdescr <- c("Pobudzenie", "Walencja", "Głębia", "Szum")
  xlabel <- "Warunek"
  ydescr.seconds <- "Sekundy"
  ydescr.nrs <- "NRS"
  factLabels <- c("Pobudzenie", "Walencja", "Głębia", "Szum")
  graphTitles.maxPain <- "Maksymalny ból"
  graphTitles.avgPain <- "Średni ból"
  graphTitles.controlability <- "Kontrolowalność bólu"
  graphTitles.painThresh <- "Próg bólu"
  graphTitles.painTolerance <- "Tolerancja bólu"
} else if(localeSet == "EN"){
  xdescr <- c("Arousal", "Valence", "Depth", "Noise")
  xlabel <- "Condition"
  ydescr.seconds <- "seconds"
  ydescr.nrs <- "NRS"
  factLabels <- c("Arousal", "Depth", "Noise", "Valence")
  graphTitles.maxPain <- "Maximal pain"
  graphTitles.avgPain <- "Average pain"
  graphTitles.controlability <- "Pain controlability"
  graphTitles.painThresh <- "Pain threshold"
  graphTitles.painTolerance <- "Pain tolerance"
  graphTitles.HR <- "Heart rate"
}



d.PainMax <- d[,1:5]
d.PainAvg <- d[,c(1, 6:9)]
d.PainContr <- d[,c(1, 10:13)]
d.PainThresh <- d[,c(1, 24:27)]
d.PainToler <- d[,c(1, 28:31)]

# Calculate systolic BP delta
d.BPsys <- select(d, ends_with("BPsys"), SubID) %>% 
  mutate(A_BPsysD = A_BPsys - CurrBPsys, 
         V_BPsysD = V_BPsys - CurrBPsys,
         D_BPsysD = D_BPsys - CurrBPsys,
         N_BPsysD = N_BPsys - CurrBPsys) %>% 
  select(A_BPsysD, V_BPsysD, D_BPsysD, N_BPsysD, SubID)

# Calculate diastolic BP delta
d.BPdiast <- select(d, ends_with("BPdiast"), CurrBPdias, SubID) %>% 
  mutate(A_BPdiastD = A_BPdiast - CurrBPdias, 
         V_BPdiastD = V_BPdiast - CurrBPdias,
         D_BPdiastD = D_BPdiast - CurrBPdias,
         N_BPdiastD = N_BPdiast - CurrBPdias) %>% 
  select(A_BPdiastD, V_BPdiastD, D_BPdiastD, N_BPdiastD, SubID)

# Calculate HR delta
d.HR <- select(d, ends_with("HR"), CurrHR, SubID) %>% 
  mutate(A_HR_D = A_HR - CurrHR,
         V_HR_D = V_HR - CurrHR,
         D_HR_D = D_HR - CurrHR,
         N_HR_D = N_HR - CurrHR) %>% 
  select(A_HR_D, V_HR_D, D_HR_D, N_HR_D, SubID)

# make tidy anova tables
dm.PainMax <- gather(d.PainMax, key="condition", value = "value", -SubID)
dm.PainAvg <- gather(d.PainAvg, key="condition", value = "value", -SubID)
dm.PainContr <- gather(d.PainContr, key="condition", value = "value", -SubID)
dm.PainThresh <- gather(d.PainThresh, key="condition", value = "value", -SubID)
dm.PainToler <- gather(d.PainToler, key="condition", value = "value", -SubID)
dm.BPsys <- gather(d.BPsys, key="condition", value = "value", -SubID)
dm.BPdiast <- gather(d.BPdiast, key="condition", value = "value", -SubID)
dm.HR <- gather(d.HR, key="condition", value = "value", -SubID)

# make factors
dm.PainMax$condition <- factor(dm.PainMax$condition, labels = factLabels)
dm.PainAvg$condition <- factor(dm.PainAvg$condition, labels = factLabels)
dm.PainContr$condition <- factor(dm.PainContr$condition, labels = factLabels)
dm.PainThresh$condition <- factor(dm.PainThresh$condition, labels = factLabels)
dm.PainToler$condition <- factor(dm.PainToler$condition, labels = factLabels)
dm.BPsys$condition <- factor(dm.BPsys$condition, labels = factLabels)
dm.BPdiast$condition <- factor(dm.BPdiast$condition, labels = factLabels)
dm.HR$condition <- factor(dm.HR$condition, labels = factLabels)

# Descriptives
# Pain max
summarise(dm.PainMax, mean = my_mean(value), sd = my_sd(value))


summarise(dm.PainAvg, mean = my_mean(value), sd = my_sd(value))
summarise(dm.PainContr, mean = my_mean(value), sd = my_sd(value))
summarise(dm.PainThresh, mean = my_mean(value), sd = my_sd(value))
summarise(dm.PainToler, mean = my_mean(value), sd = my_sd(value))


# ezANOVA
an.painMax <- ezANOVA(data = dm.PainMax, 
                      dv = value, 
                      wid = SubID, 
                      within = condition, 
                      detailed = T, 
                      type = 3)

an.painAvg <- ezANOVA(data = dm.PainAvg, 
                      dv = value, 
                      wid = SubID, 
                      within = condition, 
                      detailed = T, 
                      type = 3)

an.painContr <- ezANOVA(data = dm.PainContr, 
                        dv = value, 
                        wid = SubID, 
                        within = condition, 
                        detailed = T, 
                        type = 3)

an.painToler <- ezANOVA(data = dm.PainToler, 
                        dv = value, 
                        wid = SubID, 
                        within = condition, 
                        detailed = T, 
                        type = 3)

an.painThresh <- ezANOVA(data = dm.PainThresh, 
                         dv = value, 
                         wid = SubID, 
                         within = condition, 
                         detailed = T, 
                         type = 3)

an.BPsys <- ezANOVA(data = dm.BPsys, 
                         dv = value, 
                         wid = SubID, 
                         within = condition, 
                         detailed = T, 
                         type = 3)

an.BPdiast <- ezANOVA(data = dm.BPdiast, 
                    dv = value, 
                    wid = SubID, 
                    within = condition, 
                    detailed = T, 
                    type = 3)

an.HR <- ezANOVA(data = dm.HR, 
                      dv = value, 
                      wid = SubID, 
                      within = condition, 
                      detailed = T, 
                      type = 3)

# ANOVA PRINT
# Pain Max
an.painMax

# Pain Avg
an.painAvg
#post-hoc
pairwise.t.test(dm.PainAvg$value, dm.PainAvg$condition, paired = T, p.adjust.method = "bonferroni")
# descriptives
dm.PainAvg %>% 
  group_by(condition) %>% 
  summarise(M = my_mean(value), SD = my_sd(value))

an.painContr


#Pain thresh
an.painThresh

# Pain tolerance
an.painToler
#post-hoc
pairwise.t.test(dm.PainToler$value, dm.PainToler$condition, paired = T, p.adjust.method = "bonferroni")
# descriptives
dm.PainToler %>% 
  group_by(condition) %>% 
  summarise(M = my_mean(value), SD = my_sd(value))


# BP systolic
an.BPsys


# BP diast
an.BPdiast


# HR
an.HR

# BARPLOTS (ggplot2)

# Max pain
pMaxBar <- ggplot(dm.PainMax, aes(condition, value))
pMaxBar <- pMaxBar + 
  stat_summary(fun.y = mean, geom = "bar", width = .6) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width =  0.4) +
  scale_x_discrete(limits = xdescr) + 
  theme_gray(base_size = 12, base_family = "") +
  ggtitle(graphTitles.maxPain) + 
  xlab(xlabel) +
  ylab(paste(graphTitles.maxPain, " (", ydescr.nrs, ")", sep = ""))


# Average pain
pAvgBar <- ggplot(dm.PainAvg, aes(condition, value))
pAvgBar <- pAvgBar + stat_summary(fun.y = mean, geom = "bar", width = .6) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width =  0.4) +
  scale_x_discrete(limits = xdescr) + 
  theme_gray(base_size = 12, base_family = "") + 
  ggtitle(graphTitles.avgPain) + 
  xlab(xlabel) +
  ylab(paste(graphTitles.avgPain, " (", ydescr.nrs, ")", sep = ""))

# pAvgBar

# Pain Controlability
pContrBar <- ggplot(dm.PainContr, aes(condition, value))
pContrBar <- pContrBar + stat_summary(fun.y = mean, geom = "bar", width = .6) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width =  0.4) +
  scale_x_discrete(limits = xdescr) + 
  theme_gray(base_size = 12, base_family = "") + 
  ggtitle(graphTitles.controlability) + 
  xlab(xlabel) +
  ylab(paste(graphTitles.controlability, " (", ydescr.nrs, ")", sep = ""))


# Threshold
pThreshBar <- ggplot(dm.PainThresh, aes(condition, value))
pThreshBar <- pThreshBar + stat_summary(fun.y = mean, geom = "bar", width = .6) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width =  0.4) +
  scale_x_discrete(limits = xdescr) + 
  theme_gray(base_size = 12, base_family = "") + 
  ggtitle(graphTitles.painThresh) + 
  xlab(xlabel) +
  ylab(paste(graphTitles.painThresh, " (", ydescr.seconds, ")", sep = ""))

#Pain tolerance
pTolerBar <- ggplot(dm.PainToler, aes(condition, value))
pTolerBar <- pTolerBar + stat_summary(fun.y = mean, geom = "bar", width = .6) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width =  0.4) +
  scale_x_discrete(limits = xdescr) + 
  theme_gray(base_size = 12, base_family = "") + 
  ggtitle(graphTitles.painTolerance) + 
  xlab(xlabel) +
  ylab(paste(graphTitles.painTolerance, " (", ydescr.seconds, ")", sep = ""))





BPSysBar <- ggplot(dm.BPsys, aes(condition, value))
BPSysBar <- BPSysBar + stat_summary(fun.y = mean, geom = "bar", width = .6) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width =  0.4) +
  scale_x_discrete(limits = xdescr) + 
  theme_gray(base_size = 12, base_family = "") + 
  ggtitle("Systolic Blood Pressure") + 
  xlab(xlabel) +
  ylab("Systolic BP (change from baseline)")
#BPSysBar

BPDiastBar <- ggplot(dm.BPdiast, aes(condition, value))
BPDiastBar <- BPDiastBar + stat_summary(fun.y = mean, geom = "bar", width = .6) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width =  0.4) +
  scale_x_discrete(limits = xdescr) + 
  theme_gray(base_size = 12, base_family = "") + 
  ggtitle("Diastolic Blood Pressure") + 
  xlab(xlabel) +
  ylab("Diastolic BP (change from baseline)")
#BPDiastBar


HRBar <- ggplot(dm.HR, aes(condition, value))
HRBar <- HRBar + stat_summary(fun.y = mean, geom = "bar", width = .6) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width =  0.4) +
  scale_x_discrete(limits = xdescr) + 
  theme_gray(base_size = 12, base_family = "") + 
  ggtitle(graphTitles.HR) + 
  xlab(xlabel) +
  ylab("Heart rate (change from baseline)")
#HRBar  

# output plots
plot_grid(pThreshBar, pTolerBar, pMaxBar, pAvgBar, pContrBar, labels = "AUTO")
plot_grid(BPSysBar, BPDiastBar, HRBar, labels = "AUTO", ncol = 3)
