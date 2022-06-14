#Establish root dir ####
#install.packages("here")
library(tidyverse)
here::i_am("src/wine_EDA.R")
#Install packages ####
# install.packages("reactable")
# install.packages("tidyverse")
# install.packages("janitor")
#install.packages("skimr")
#install.packages("dlookr")
#install.packages("flextable")
# install.packages("ggstatsplot")
#install.packages("PerformanceAnalytics")
#install.packages("sm")
#install.packages("flexdashboard")
# install.packages("gt")
# install.packages("DT")
#install.packages("moments")
#library(here) #
#library(reactable)#y
#library(janitor)#y
library(skimr)
#library(DataExplorer) #y
library(dlookr) #y
#library(flextable) #y
library(moments)
#library(ggstatsplot)#y
library(purrr)
library(PerformanceAnalytics)
library(sm)
library(flexdashboard)
library(gt)
library(DT)

#Load and clean dataset
getwd()
wine <- read_csv("data/winequality-red.csv")
wine <- janitor::clean_names(wine) #remove spaces
colnames(wine)[colnames(wine) == 'p_h'] <- 'ph'#change p_h to ph



#Explore data ####
sum(is.na(wine))#No missing data
str(wine)#all variables are type double
#glimpse(wine)
#summary(wine)
#dim(wine)
#length(wine)
#names(wine)
wine$quality #wine quality seems to be whole numbers
#summary(wine$quality)
#table(wine$quality)
#skim(wine)

#FIRST PART static table ####
dlookr::describe(wine) %>% 
  flextable::flextable(
    col_keys =
      c("described_variables","mean", 
        "p00", "p50", "p100")) %>%
          flextable::colformat_double(digits = 2) %>% 
              flextable::bold(part="header") %>% 
                flextable::border_inner_h() 
#SECOND PART ONE interactive table ####
ggstatsplot::ggcorrmat(data = wine, type = "np", output = "dataframe", theme=) %>% 
  mutate_if(is.numeric, ~round(.,2)) %>% 
  reactable::reactable()
#another possible interactive table
winecorr <- compare_numeric(wine)
winecorr$correlation
winecorr$linear %>% mutate_if(is.numeric, ~round(.,2)) %>%
  flextable::flextable()  

#SECOND PART TWO data vis ####
wine %>% 
  DataExplorer::plot_density() #visualisation of skewness

#plot_correlate(wine, method = "kendall") #this is from dlookr will not work unless I load the whole package
#OR
ggstatsplot::ggcorrmat(data = wine) #includes significance
#Drill into alcohol vs quality correlation ####
#ggstatsplot::ggscatterstats(data = wine, x=alcohol, y=quality, type="np")##useless plot

#wine quality plot 
#hist(wine$quality) #bars are too spread out
barplot(table(wine$quality), xlab = "Wine Quality", ylab = "Frequency")
pie(table(wine$quality), radius = 1, main = "Pie Chart of Wine Quality")

#Drill into alcohol vs quality correlation 
#ggstatsplot::ggscatterstats(data = wine, x=alcohol, y=quality, type="np")#useless
#OR 
sm::sm.density.compare(wine$alcohol, wine$quality, xlab="Alcohol Content")
quality_legend = factor(wine$quality, labels=c("3","4","5","6","7","8"))
colfill <- c(2:(2+length(levels(quality_legend))))
legend("topright", levels(quality_legend), fill=colfill, title = "Quality")

#Skewness and kurtosis 
kableExtra::kable(wine) %>% sapply(skewness) %>% kableExtra::kable_styling()
sapply(wine, skewness) #quality is not skewed, alcohol is moderately skewed.  

wine %>% 
  normality()



