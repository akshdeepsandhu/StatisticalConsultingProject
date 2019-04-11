# StatisticalConsultingProject

---
title: "Analysis of Particulate Pollution in Lavington, BC"
author: "Ash Sandhu - 20665148"
date: '2019-03-04'
output:
  pdf_document:
    fig_caption: yes
    keep_tex: yes
header-includes: \usepackage{float}
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
#Load packages 
library(data.table)
library(dplyr)
library(ggplot2)
library(Amelia)
library(cowplot)
library(robust)
library(knitr)
library(broom)
```
#Summary 

The Pinnacle Renewable Energy pellet plant, located in Lavington BC opened in 2015. The local community raised concerns about the air quality following the opening of the plant due its proximity to a local elementary school. The Ministry of Environment (MOE) decided to set up  an air quality monitoring station (BAM) at Lavington Baptist Church, with a smaller monitor (EBAM) being installed for a short period of time later on. The MOE issued a report stating that the new pellet factory had no significant impact on the air quality in Lavington. Our clients contest this claim, citing video footage which shows plumes of smoke drifting across Lavington from the nearby factory. 

The goal of this project was to review the MOE report and conduct our own analysis using the provided sensor data. The MOE report stated that the two monitors were highly uncorrelated when looking at the hourly data, however the averaged daily data had a high correlation. They then used this aggregated data to conduct their analysis. Following a literature review we came across studies that showed that the BAM and EBAM behave quite erratically when the humidity is high (above 40%). Therefore we looked at the impact of humidity on the correlation between the two variables. We concluded that the correlation between the two monitors changes with humidity and averaging the data fails to take this relationship into account. The study of inversion conditions, an atmospheric event that results in air being trapped in the valley, found that there was no systematic increase in PM2.5 conditions over the years, but this may be due to several confounding factors. It is our recommendation to keep monitoring the air quality to see if the increase in PM2.5 during certain years was due to natural forces or the pellet factory. We also hope to look at how the PM2.5 levels changed with the level of production from the factory. Finally, the sensor data suggests that when there were NE winds the BAM readings were much higher than the EBAM. We cannot conclusively say that this due to the pellet factory, as meteorological conditions may have systematically effected the data. We suggest that a more nuanced model is used, that is able to incorporate the impact of humidity of the EBAM and BAM data. 

In summary, we cannot conclusively say the MOE report was incorrect, nor can we say that the pellet factory increased PM2.5 concentrations in Lavington. However, we do agree that the ministry failed to account for a lot of confounding factors and should continue monitoring the area. It is also suggested a better study design be used in the future, as there was no data on air quality before the factory was set up. This proved to be problematic as it was essentially impossible to compare the levels of PM2.5 before and after the factory was set up.  
