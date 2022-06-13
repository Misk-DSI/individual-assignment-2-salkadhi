# install.packages("tidyverse")
library(tidyverse)
getwd()
wine <- read_csv("data/winequality-red.csv")
# install.packages("janitor")
library(janitor)
wine
#add underscore to column names
wine <- clean_names(wine)
str(wine)
glimpse(wine)
summary(wine)
dim(wine)
length(wine)
names(wine)
wine$quality#change p_h to ph
colnames(wine)[colnames(wine) == 'p_h'] <- 'ph'
names(wine)
summary(wine$quality)
table(wine$quality)
#No missing data
sum(is.na(wine))
#install.packages("skimr")
library(skimr)
skim(wine)
#wine quality plot ####
hist(wine$quality)
barplot(table(wine$quality))
pie(table(wine$quality))
#wine zscore ####
wine_zscores = scale(wine)
view(wine_zscores)
library(DataExplorer)
#install.packages("dlookr")
library(dlookr)
describe(wine)
#install.packages("flextable")
library(flextable)
# Beautified tables ####
#one
describe(wine) %>% flextable()
#two
wine %>% 
  diagnose_numeric() %>% 
  flextable()
#Check skewness of all variables ####
wine %>% 
  plot_density()
#install.packages("moments")
#Skewness and kurtosis ####
library(moments)
sapply(wine, skewness)
sapply(wine, agostino.test)
sapply(wine, anscombe.test)
# QQ Plots ####
plot_qq(wine, by="quality")
#Normality test ####
#The null hypothesis of these tests is that “sample distribution is normal”. 
#If the test is significant, the distribution is non-normal.
normality(wine) %>%
  mutate_if(is.numeric, ~round(.,2)) %>% 
  flextable()

wine %>% 
  normality()

#Box plots of quality vs all variable ####
plot_boxplot(wine, by = "quality")
# install.packages("ggstatsplot")
library(ggstatsplot)
library(purrr)
#A bunch of plots ####
plot_list <- pmap(.l = list(data = list(wine), x="quality", y=list("chlorides", "ph")), .f=ggbetweenstats)
plot_list[[1]]
# Correlations ####
#2 graphs and beautified table
correlate(wine, quality)
library(dlookr)
plot_correlate(wine)
plot_correlate(wine, method = "kendall")
ggcorrmat(data = wine)
ggcorrmat(data = wine, type = "np", output = "dataframe") %>% mutate_if(is.numeric, ~round(.,2)) %>% 
  flextable()
#Drill into alcohol vs quality correlation ####
ggscatterstats(data = wine, x=alcohol, y=quality, type="np")
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
# Correlations. Another Method ####
#DON'T RERUN THIS METOD as is it will break r
#chart.Correlation(wine, method="kendall")

winecorr <- compare_numeric(wine)
winecorr$correlation
#yet another correlation table
winecorr$linear %>% mutate_if(is.numeric, ~round(.,2)) %>% flextable()
#More correlation plots ####
plot(winecorr)  

#Using ggplot ####
ggplot(wine, aes(alcohol, quality)) +
  geom_point()+
  geom_smooth()
#Density plot of alcohol of vs quality ####
#install.packages("sm")
library(sm)
sm.density.compare(wine$alcohol, wine$quality, xlab="Alcohol Content")
quality_legend = factor(wine$quality, labels=c("3","4","5","6","7","8"))
colfill <- c(2:(2+length(levels(quality_legend))))
legend("topright", levels(quality_legend), fill=colfill, title = "Quality")
scatter.smooth(wine$alcohol, wine$quality)
#install.packages("here")
library(here)
i_am("src/wine_EDA.R")
here()
#install.packages("flexdashboard")
# install.packages("gt")
# install.packages("DT")
library(flexdashboard)
library(gt)
library(DT)
